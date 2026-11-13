import uPlot from './uPlot.esm.js';
import {
    ControlNotImplementedError,
    disableMotors,
    fetchConfigValues,
    fetchStatusSchema,
    fetchStatusValues,
    homeAxes,
    jogAxis,
    setFeedrateScale,
    setFlowScale,
    setHeaterTarget
} from './api.js';
import { wsClient } from './ws.js';
import {
    getActiveLocale,
    onLocaleChange,
    t,
    translateStatusGroupLabel,
    translateStatusModuleLabel,
    translateStatusUnit,
    translateStatusValueLabel
} from './localization.js';

// The status page has three parts:
//  - an overview built from the default modules (machine state, motion, temperatures, fans, inputs and drivers),
//    using the configuration to pair each heater with its thermistor and to name the hotend, bed and chamber, with
//    controls for jogging, homing, heater targets and the feed and flow overrides (the control/ API in api.ts);
//  - charts of values the user has pinned, drawn from a history kept while the page is open;
//  - a table of every status value, which is where values are pinned.
// Values arrive about every 50 ms. Text is only rewritten when it changes, so it can be selected and copied, and the
// charts are fed from samples taken every SAMPLE_PERIOD_MS.

type ValueKind = 'Real' | 'Integer' | 'Boolean' | 'String';

interface ValueSchema {
    Kind: ValueKind;
    Unit?: string;
    Description?: string;
}

type StatusSchema = Record<string, Record<string, Record<string, ValueSchema>>>;

interface StatusPath {
    module: string;
    group: string;
    value: string;
}

interface Layout {
    hidden: string[];
    pins: string[];
    window: number;
    jogStep: number;
}

interface LineStyle {
    color: string;
    dash?: number[];
}

interface ChartSeries {
    key: string;
    style: LineStyle;
}

interface Chart {
    plot: uPlot;
    host: HTMLElement;
    series: ChartSeries[];
}

interface CellDef {
    id: string;
    title: () => string;
    available: () => boolean;
    build: (body: HTMLElement, head: HTMLElement) => void;
}

const LAYOUT_KEY = 'prunt-status-layout-v1';
const OLD_LAYOUT_KEYS = ['prunt-dashboard-layout-v4', 'prunt-dashboard-layout-v3', 'prunt-dashboard-layout-v2', 'prunt-dashboard-layout'];
const SAMPLE_PERIOD_MS = 500;
const HISTORY_SECONDS = 3600;
const WINDOWS = [60, 600, 3600];
const CHART_HEIGHT = 170;
const JOG_STEPS = [0.1, 1, 10, 100];
// Jog speeds in mm/s. The server limits them to the machine's configured maximums.
const JOG_FEEDRATES: Record<string, number> = { X_AXIS: 100, Y_AXIS: 100, Z_AXIS: 10, E_AXIS: 5 };
const DEFAULT_JOG_FEEDRATE = 10;
const OVERRIDE_STEP = 10;
const FEEDBACK_CLEAR_MS = 4000;
const PLOT_FONT = '11px "B612 Mono", ui-monospace, monospace';

const REPORTER = 'Internal Status Reporter';
const BLOCKING = 'Blocking Tracker';
const MACHINE_NAME = 'Machine Name';
const MOTION = 'Motion';
const HEATERS = 'Heaters';
const THERMISTORS = 'Thermistors';
const FANS = 'Fans';
const TACHOMETERS = 'Tachometers';
const SWITCHES = 'Input Switches';
const DRIVERS = 'TMC2240 Drivers';

const AXIS_ORDER = ['X_AXIS', 'Y_AXIS', 'Z_AXIS', 'E_AXIS'];
const HEATER_ROLES = ['Hotend', 'Bed', 'Chamber'];
const MODE_CODES: Record<string, string> = {
    MILLIMETER_UNITS_MODE: 'G21',
    INCH_UNITS_MODE: 'G20',
    ABSOLUTE_POSITIONING_MODE: 'G90',
    RELATIVE_POSITIONING_MODE: 'G91',
    ABSOLUTE_E_POSITIONING_MODE: 'M82',
    RELATIVE_E_POSITIONING_MODE: 'M83',
};
const UNIT_DECIMALS: Record<string, number> = { 'mm': 3, 'mm/s': 1, '°C': 1, 'RPM': 0, 'V': 2, 'A': 2 };
// Driver flags worth raising in the overview. Everything else, such as StealthChop being active, stays in the table.
const DRIVER_FAULT = /overtemperature$|error|undervoltage|short/i;
const DRIVER_CAUTION = /pre-warning|open load/i;

// Lines are told apart by colour and dash, as on a drawing. Colours are theme tokens read at draw time so charts
// follow theme changes.
const LINE_COLORS = ['--ink', '--rev', '--caution', '--ink-muted'];
const LINE_DASHES: (number[] | undefined)[] = [undefined, [6, 4], [10, 3, 2, 3], [2, 3]];
const TARGET_DASH = [6, 4];

let schema: StatusSchema = {};
let values: any = {};
let configValues: any = null;
let connected = false;
let editing = false;
let filterText = '';
let layout: Layout = loadLayout();

let bindings: (() => void)[] = [];
let charts: Chart[] = [];
const historyTimes: number[] = [];
const historyValues = new Map<string, number[]>();
let lastSampleMs = 0;

const chartResizeObserver = new ResizeObserver(entries => {
    for (const entry of entries) {
        const width = Math.floor(entry.contentRect.width);
        if (width <= 0) continue; // Hidden views are resized when they become visible again.
        const chart = charts.find(c => c.host === entry.target);
        if (chart && chart.plot.width !== width) chart.plot.setSize({ width, height: CHART_HEIGHT });
    }
});

export async function initStatusView() {
    new MutationObserver(() => charts.forEach(({ plot }) => plot.redraw(false)))
        .observe(document.documentElement, { attributes: true, attributeFilter: ['data-theme'] });

    document.getElementById('btn-status-edit')?.addEventListener('click', () => {
        editing = !editing;
        render();
    });
    document.getElementById('status-filter')?.addEventListener('input', event => {
        filterText = (event.target as HTMLInputElement).value.trim().toLowerCase();
        applyFilter();
    });

    wsClient.on('connected', () => {
        connected = true;
        refresh();
    });
    wsClient.on('disconnected', () => {
        connected = false;
        refresh();
    });
    onLocaleChange(() => render());

    try {
        [schema, values] = await Promise.all([fetchStatusSchema(), fetchStatusValues()]);
    } catch (e) {
        console.error('Failed to load the status schema', e);
        return;
    }
    try {
        const config = await fetchConfigValues();
        configValues = config.Values || config;
    } catch (e) {
        // The overview still works without the configuration; it just cannot pair heaters with thermistors.
        console.error('Failed to load configuration values for the status page', e);
    }

    render();
    wsClient.on('tick', (msg: any) => {
        if (!msg.Status_Values) return;
        values = msg.Status_Values;
        const now = Date.now();
        if (now - lastSampleMs >= SAMPLE_PERIOD_MS) {
            lastSampleMs = now;
            sampleHistory(now / 1000);
            if (isVisible()) updateCharts();
        }
        if (isVisible()) refresh();
    });
}

// ---------- Layout persistence ----------

function loadLayout(): Layout {
    const fallback: Layout = { hidden: [], pins: [], window: 600, jogStep: 10 };
    try {
        OLD_LAYOUT_KEYS.forEach(key => localStorage.removeItem(key));
        const saved = JSON.parse(localStorage.getItem(LAYOUT_KEY) || 'null');
        if (!saved) return fallback;
        return {
            hidden: Array.isArray(saved.hidden) ? saved.hidden.filter((x: unknown) => typeof x === 'string') : [],
            pins: Array.isArray(saved.pins) ? saved.pins.filter((x: unknown) => typeof x === 'string') : [],
            window: WINDOWS.includes(saved.window) ? saved.window : fallback.window,
            jogStep: JOG_STEPS.includes(saved.jogStep) ? saved.jogStep : fallback.jogStep,
        };
    } catch {
        return fallback;
    }
}

function saveLayout() {
    try {
        localStorage.setItem(LAYOUT_KEY, JSON.stringify(layout));
    } catch {
        // Storage can be unavailable (private windows); the layout then lasts for this page only.
    }
}

// ---------- Data access ----------

function pathKey(module: string, group: string, value: string): string {
    return JSON.stringify([module, group, value]);
}

function parseKey(key: string): StatusPath | null {
    try {
        const [module, group, value] = JSON.parse(key);
        return typeof module === 'string' && typeof group === 'string' && typeof value === 'string' ? { module, group, value } : null;
    } catch {
        return null;
    }
}

function valueAt(module: string, group: string, value: string): any {
    return values?.[module]?.[group]?.[value];
}

function schemaAt(p: StatusPath): ValueSchema | undefined {
    return schema[p.module]?.[p.group]?.[p.value];
}

function keysOf(module: string, group: string): string[] {
    return Object.keys(schema[module]?.[group] ?? {});
}

function hasGroup(module: string, group: string): boolean {
    return keysOf(module, group).length > 0;
}

function numberAt(module: string, group: string, value: string): number {
    const v = valueAt(module, group, value);
    return typeof v === 'number' ? v : NaN;
}

function stringAt(module: string, group: string, value: string): string {
    const v = valueAt(module, group, value);
    return typeof v === 'string' ? v : '';
}

function isNumeric(s: ValueSchema | undefined): boolean {
    return s?.Kind === 'Real' || s?.Kind === 'Integer';
}

function moduleConfig(module: string): any {
    return configValues?.Config?.[module]?.Config;
}

function enabledChildren(variant: any): any | null {
    return variant?.Kind?.Selected === 'Enabled' ? (variant.Kind.Children?.Enabled ?? {}) : null;
}

interface HeaterInfo {
    enabled: boolean;
    thermistor: string | null;
    band: number;
    role: string | null;
}

function heaterInfo(name: string): HeaterInfo {
    const heaters = moduleConfig(HEATERS);
    if (!heaters) return { enabled: true, thermistor: null, band: 3, role: null };
    const enabled = enabledChildren(heaters.Heaters?.[name]);
    const role = HEATER_ROLES.find(r => enabledChildren(heaters.Gcode_Defaults?.[r])?.Heater === name) ?? null;
    return {
        enabled: enabled !== null,
        thermistor: typeof enabled?.Thermistor === 'string' ? enabled.Thermistor : null,
        band: typeof enabled?.Check_Hysteresis === 'number' ? enabled.Check_Hysteresis : 3,
        role,
    };
}

function thermistorRange(name: string, readings: number[]): [number, number] {
    const config = moduleConfig(THERMISTORS)?.Thermistors?.[name];
    const lo = typeof config?.Minimum_Temperature === 'number' ? config.Minimum_Temperature : 0;
    const configuredHi = typeof config?.Maximum_Temperature === 'number' ? config.Maximum_Temperature : NaN;
    const hi = Number.isFinite(configuredHi) ? configuredHi : Math.max(300, ...readings.filter(Number.isFinite).map(r => r * 1.2));
    return hi > lo ? [lo, hi] : [0, 300];
}

// ---------- Formatting ----------

const numberFormats = new Map<string, Intl.NumberFormat>();

function formatNumber(n: number, decimals: number, grouping = false): string {
    if (!Number.isFinite(n)) return '—';
    if (Math.abs(n) < 0.5 * 10 ** -decimals) n = 0; // Never show -0.000.
    const key = `${getActiveLocale()}|${decimals}|${grouping}`;
    let format = numberFormats.get(key);
    if (!format) {
        const options = { minimumFractionDigits: decimals, maximumFractionDigits: decimals, useGrouping: grouping };
        try {
            format = new Intl.NumberFormat(getActiveLocale(), options);
        } catch {
            format = new Intl.NumberFormat(undefined, options);
        }
        numberFormats.set(key, format);
    }
    return format.format(n);
}

function formatPercent(scale: number): string {
    return Number.isFinite(scale) ? `${formatNumber(scale * 100, 0)} %` : '—';
}

function prettyString(s: string): string {
    if (s === '') return '—';
    const translated = t(`ui.status.enum.${s}`, '');
    if (translated) return translated;
    // Unknown enumeration images such as SOME_NEW_MODE read better in sentence case. Other strings, such as names
    // and file names, are shown exactly as reported.
    if (/^[A-Z][A-Z0-9]*(_[A-Z0-9]+)*_MODE$/.test(s)) {
        const words = s.replace(/_MODE$/, '').toLowerCase().replace(/_/g, ' ');
        return words.charAt(0).toUpperCase() + words.slice(1);
    }
    return s;
}

interface Display {
    text: string;
    unit: string;
}

function formatValue(p: StatusPath): Display {
    const s = schemaAt(p);
    const v = valueAt(p.module, p.group, p.value);
    if (v === undefined || v === null || !s) return { text: '—', unit: '' };
    const unit = s.Unit ? translateStatusUnit(p.module, p.group, p.value, s.Unit) : '';
    switch (s.Kind) {
        case 'Real':
            if (s.Unit === '×') return { text: formatNumber(Number(v) * 100, 0), unit: '%' };
            return { text: formatNumber(Number(v), UNIT_DECIMALS[s.Unit ?? ''] ?? 3), unit };
        case 'Integer':
            return { text: formatNumber(Number(v), 0, true), unit };
        case 'Boolean':
            return { text: v ? t('ui.status.yes', 'Yes') : t('ui.status.no', 'No'), unit: '' };
        default:
            return { text: prettyString(String(v)), unit };
    }
}

function axisLetter(axis: string): string {
    return axis.replace(/_AXIS$/, '');
}

function valueLabel(p: StatusPath): string {
    return translateStatusValueLabel(p.module, p.group, p.value, p.value);
}

// ---------- DOM helpers ----------

function el<K extends keyof HTMLElementTagNameMap>(tag: K, className = '', text?: string): HTMLElementTagNameMap[K] {
    const node = document.createElement(tag);
    if (className) node.className = className;
    if (text !== undefined) node.textContent = text;
    return node;
}

function setText(node: HTMLElement, text: string) {
    if (node.textContent !== text) node.textContent = text;
}

function setStyle(node: HTMLElement, property: 'width' | 'left', value: string) {
    if (node.style[property] !== value) node.style[property] = value;
}

function setTone(node: HTMLElement, tone: string) {
    if (node.dataset.tone !== tone) node.dataset.tone = tone;
}

function flag(text = ''): HTMLSpanElement {
    return el('span', 'st-flag', text);
}

// A short line in a series' colour and dash, used as a legend key.
function swatch(style: LineStyle): SVGSVGElement {
    const ns = 'http://www.w3.org/2000/svg';
    const svg = document.createElementNS(ns, 'svg');
    svg.setAttribute('class', 'st-swatch');
    svg.setAttribute('viewBox', '0 0 22 6');
    svg.setAttribute('aria-hidden', 'true');
    const line = document.createElementNS(ns, 'line');
    line.setAttribute('x1', '0');
    line.setAttribute('y1', '3');
    line.setAttribute('x2', '22');
    line.setAttribute('y2', '3');
    line.style.stroke = `var(${style.color})`;
    if (style.dash) line.setAttribute('stroke-dasharray', style.dash.join(' '));
    svg.appendChild(line);
    return svg;
}

function plotIcon(): SVGSVGElement {
    const ns = 'http://www.w3.org/2000/svg';
    const svg = document.createElementNS(ns, 'svg');
    svg.setAttribute('viewBox', '0 0 14 10');
    svg.setAttribute('aria-hidden', 'true');
    const line = document.createElementNS(ns, 'polyline');
    line.setAttribute('points', '1,8 4,3 7,6 10,1 13,4');
    svg.appendChild(line);
    return svg;
}

function themeColor(token: string): string {
    return getComputedStyle(document.documentElement).getPropertyValue(token).trim();
}

function isVisible(): boolean {
    return !!document.getElementById('status-view')?.classList.contains('active') && document.visibilityState === 'visible';
}

// ---------- Rendering ----------

function render() {
    const overview = document.getElementById('status-overview');
    const tables = document.getElementById('status-tables');
    if (!overview || !tables) return;

    charts.forEach(c => c.plot.destroy());
    charts = [];
    chartResizeObserver.disconnect();
    bindings = [];

    renderHeaderControls();
    renderOverview(overview);
    renderTables(tables);
    applyFilter();
    refresh();
    updateCharts();
}

function refresh() {
    document.getElementById('status-view')?.classList.toggle('st-stale', !connected);
    for (const binding of bindings) binding();
}

function renderHeaderControls() {
    const windowGroup = document.getElementById('status-window');
    if (windowGroup) {
        windowGroup.replaceChildren();
        windowGroup.setAttribute('aria-label', t('ui.status.history', 'History'));
        for (const seconds of WINDOWS) {
            const button = el('button', '', t(`ui.status.window.${seconds}`, seconds < 3600 ? `${seconds / 60} min` : `${seconds / 3600} h`));
            button.type = 'button';
            button.setAttribute('aria-pressed', String(layout.window === seconds));
            button.addEventListener('click', () => {
                layout.window = seconds;
                saveLayout();
                windowGroup.querySelectorAll('button').forEach(b => b.setAttribute('aria-pressed', String(b === button)));
                updateCharts();
            });
            windowGroup.appendChild(button);
        }
    }
    const edit = document.getElementById('btn-status-edit');
    if (edit) {
        edit.textContent = editing ? t('ui.status.doneEditing', 'Done') : t('ui.status.editLayout', 'Edit layout');
        edit.setAttribute('aria-pressed', String(editing));
    }
    document.getElementById('status-view')?.classList.toggle('st-editing', editing);
}

const CELLS: CellDef[][] = [
    [
        { id: 'machine', title: () => t('ui.status.cell.machine', 'Machine'), available: () => hasGroup(REPORTER, 'Print status') || hasGroup(BLOCKING, 'Blocking'), build: buildMachineCell },
        { id: 'position', title: () => t('ui.status.cell.motion', 'Motion'), available: () => hasGroup(REPORTER, 'Position'), build: buildMotionCell },
    ],
    [
        { id: 'temperatures', title: () => t('ui.status.cell.temperatures', 'Temperatures'), available: () => temperatureRows().length > 0, build: buildTemperatureCell },
    ],
    [
        { id: 'fans', title: () => t('ui.status.cell.fans', 'Fans'), available: () => hasGroup(FANS, 'Speed') || enabledTachometers().length > 0, build: buildFanCell },
        { id: 'inputs', title: () => t('ui.status.cell.inputs', 'Inputs and drivers'), available: () => hasGroup(SWITCHES, 'Triggered') || !!schema[DRIVERS], build: buildInputCell },
    ],
];

function renderOverview(sheet: HTMLElement) {
    sheet.replaceChildren();
    const hiddenAvailable: CellDef[] = [];

    for (const row of CELLS) {
        const shown = row.filter(def => {
            if (!def.available()) return false;
            if (layout.hidden.includes(def.id)) {
                hiddenAvailable.push(def);
                return false;
            }
            return true;
        });
        for (const def of shown) {
            const cell = el('section', 'st-cell');
            cell.dataset.cell = def.id;
            if (shown.length === 1) cell.classList.add('st-wide');
            const head = el('div', 'st-cell-head');
            const title = el('h3', 'st-label', def.title());
            head.appendChild(title);
            if (editing) {
                const hide = el('button', 'btn btn-sm', t('ui.status.hide', 'Hide'));
                hide.type = 'button';
                hide.addEventListener('click', () => {
                    layout.hidden = [...layout.hidden, def.id];
                    saveLayout();
                    render();
                });
                head.appendChild(hide);
            }
            cell.appendChild(head);
            def.build(cell, title);
            sheet.appendChild(cell);
        }
    }

    renderPinnedCharts(sheet);
    renderHiddenCells(hiddenAvailable);
}

function renderHiddenCells(hidden: CellDef[]) {
    const strip = document.getElementById('status-hidden');
    if (!strip) return;
    strip.replaceChildren();
    strip.hidden = !editing;
    if (!editing) return;
    strip.appendChild(el('p', 'st-hint', t('ui.status.editHint', 'Hide the parts of the overview you don\'t need. Chart any number with the plot buttons in All values.')));
    for (const def of hidden) {
        const show = el('button', 'btn btn-sm', t('ui.status.show', 'Show {cell}', { cell: def.title() }));
        show.type = 'button';
        show.addEventListener('click', () => {
            layout.hidden = layout.hidden.filter(id => id !== def.id);
            saveLayout();
            render();
        });
        strip.appendChild(show);
    }
}

// ----- Machine -----

function machineState(): { label: string; tone: string } {
    if (!connected) return { label: t('ui.status.state.disconnected', 'Disconnected'), tone: 'fault' };
    if (valueAt(REPORTER, 'Print status', 'Paused') === true) return { label: t('ui.status.state.paused', 'Paused'), tone: 'caution' };
    if (stringAt(REPORTER, 'Print status', 'File name')) return { label: t('ui.status.state.printing', 'Printing'), tone: 'rev' };
    return { label: t('ui.status.state.idle', 'Idle'), tone: 'idle' };
}

function buildMachineCell(cell: HTMLElement, title: HTMLElement) {
    const state = el('div', 'st-state-name');
    const file = el('div', 'st-state-file');
    const fileName = el('span', 'st-file-name');
    const line = el('span', 'st-file-line');
    file.append(fileName, line);
    const wait = el('div', 'st-wait');
    const waitText = el('span');
    wait.append(el('b', '', t('ui.status.waiting', 'Waiting')), waitText);
    cell.append(state, file, wait);

    bindings.push(() => {
        const name = stringAt(MACHINE_NAME, 'Identity', 'Machine name');
        setText(title, name ? t('ui.status.cell.machineNamed', 'Machine · {name}', { name }) : t('ui.status.cell.machine', 'Machine'));
        const s = machineState();
        setText(state, s.label);
        setTone(state, s.tone);
        const current = stringAt(REPORTER, 'Print status', 'File name');
        file.hidden = !current;
        setText(fileName, current.split('/').pop() ?? current);
        if (fileName.title !== current) fileName.title = current;
        const lineNumber = numberAt(REPORTER, 'Print status', 'Current line');
        setText(line, Number.isFinite(lineNumber) ? ` · ${t('ui.status.line', 'line')} ${formatNumber(lineNumber, 0, true)}` : '');
        const waiting = stringAt(BLOCKING, 'Blocking', 'Current');
        wait.hidden = !waiting;
        setText(waitText, waiting);
    });
}

// ----- Controls -----

function motionLockReason(): string {
    if (!connected) return t('ui.status.state.disconnected', 'Disconnected');
    if (stringAt(REPORTER, 'Print status', 'File name')) return t('ui.status.motionLocked', 'Motion controls are unavailable while a file is running.');
    return '';
}

function setDisabled(control: HTMLButtonElement | HTMLInputElement, disabled: boolean, reason = '') {
    if (control.disabled !== disabled) control.disabled = disabled;
    const title = disabled ? reason : control.dataset.title ?? '';
    if (control.title !== title) control.title = title;
}

function button(text: string, className = 'btn btn-sm', title = ''): HTMLButtonElement {
    const b = el('button', className, text);
    b.type = 'button';
    if (title) {
        b.title = title;
        b.dataset.title = title;
        b.setAttribute('aria-label', title);
    }
    return b;
}

// A line under a group of controls that reports what the last control did.
function feedbackLine(): HTMLElement {
    const line = el('p', 'st-feedback');
    line.hidden = true;
    line.setAttribute('role', 'status');
    return line;
}

const feedbackTimers = new WeakMap<HTMLElement, number>();

function showFeedback(line: HTMLElement, text: string, tone: string) {
    window.clearTimeout(feedbackTimers.get(line));
    line.textContent = text;
    line.dataset.tone = tone;
    line.hidden = false;
    if (tone === 'done') feedbackTimers.set(line, window.setTimeout(() => { line.hidden = true; }, FEEDBACK_CLEAR_MS));
}

async function runControl(line: HTMLElement, action: string, request: () => Promise<void>) {
    showFeedback(line, t('ui.status.control.pending', '{action}…', { action }), 'pending');
    try {
        await request();
        showFeedback(line, t('ui.status.control.done', '{action}: sent', { action }), 'done');
    } catch (error) {
        if (error instanceof ControlNotImplementedError) {
            showFeedback(line, t('ui.status.control.notImplemented', '{action}: the server does not support this yet.', { action }), 'caution');
        } else {
            const message = error instanceof Error && error.message ? error.message : t('ui.status.control.unknownError', 'Request failed.');
            showFeedback(line, t('ui.status.control.failed', '{action}: {message}', { action, message }), 'fault');
        }
    }
}

function signedStep(step: number, direction: number): string {
    return `${direction < 0 ? '−' : '+'}${formatNumber(step, step < 1 ? 1 : 0)}`;
}

// An override as a percentage: a number field between − and + buttons. The field follows the reported value except
// while it is being edited.
function overrideStepper(
    label: string,
    group: string,
    value: string,
    describe: (percent: string) => string,
    send: (scale: number) => Promise<void>,
    feedback: HTMLElement
): HTMLElement {
    const rate = el('span', 'st-rate st-override');
    const stepper = el('span', 'st-stepper');
    const input = el('input');
    input.type = 'number';
    input.min = '1';
    input.max = '999';
    input.step = '1';
    input.inputMode = 'numeric';
    input.setAttribute('aria-label', label);
    const down = button('−', 'st-step', t('ui.status.decrease', 'Decrease {name}', { name: label }));
    const up = button('+', 'st-step', t('ui.status.increase', 'Increase {name}', { name: label }));
    stepper.append(down, input, el('span', 'st-step-unit', '%'), up);
    rate.append(el('span', 'st-label', label), stepper);

    const current = () => Math.round(numberAt(MOTION, group, value) * 100);
    // After a change the field keeps the sent value for a moment, so repeated − and + build on it rather than on a
    // reported value that has not caught up yet.
    let heldUntil = 0;
    const commit = (percent: number) => {
        if (!Number.isFinite(percent)) return;
        const clamped = Math.min(999, Math.max(1, Math.round(percent)));
        input.value = String(clamped);
        heldUntil = Date.now() + 1500;
        void runControl(feedback, describe(String(clamped)), () => send(clamped / 100));
    };
    down.addEventListener('click', () => commit((Number(input.value) || current()) - OVERRIDE_STEP));
    up.addEventListener('click', () => commit((Number(input.value) || current()) + OVERRIDE_STEP));
    input.addEventListener('change', () => commit(Number(input.value)));
    input.addEventListener('keydown', event => {
        if (event.key === 'Enter') input.blur();
        if (event.key === 'Escape') {
            input.value = String(current());
            input.blur();
        }
    });

    bindings.push(() => {
        const percent = current();
        if (document.activeElement !== input && Date.now() > heldUntil) {
            const text = Number.isFinite(percent) ? String(percent) : '';
            if (input.value !== text) input.value = text;
        }
        rate.classList.toggle('changed', Number.isFinite(percent) && percent !== 100);
        const locked = !connected;
        for (const control of [down, input, up]) setDisabled(control, locked, t('ui.status.state.disconnected', 'Disconnected'));
    });
    return rate;
}

// ----- Motion -----

function buildMotionCell(cell: HTMLElement) {
    const axes = keysOf(REPORTER, 'Position').sort((a, b) => {
        const ia = AXIS_ORDER.indexOf(a), ib = AXIS_ORDER.indexOf(b);
        return (ia < 0 ? 99 : ia) - (ib < 0 ? 99 : ib) || a.localeCompare(b);
    });
    const unit = schema[REPORTER]?.Position?.[axes[0]]?.Unit;
    const feedback = feedbackLine();
    const motionControls: HTMLButtonElement[] = [];
    const jogButtons: { button: HTMLButtonElement; axis: string; direction: number }[] = [];

    const dro = el('div', 'st-dro');
    for (const axis of axes) {
        const letter = axisLetter(axis);
        const column = el('div', 'st-axis');
        const reading = el('div', 'st-axis-value');
        column.append(el('div', 'st-axis-name', letter), reading);
        const homed = axis !== 'E_AXIS' && keysOf(REPORTER, 'Homed').includes(axis) ? flag() : null;
        if (homed) column.appendChild(homed);
        const offset = keysOf(MOTION, 'G92 offset').includes(axis) ? el('div', 'st-axis-offset') : null;
        if (offset) column.appendChild(offset);

        const jog = el('div', 'st-jog');
        for (const direction of [-1, 1]) {
            const b = button('', 'btn btn-sm');
            b.addEventListener('click', () => {
                const distance = direction * layout.jogStep;
                const description = t('ui.status.action.jog', 'Move {axis} {distance} mm', { axis: letter, distance: signedStep(layout.jogStep, direction) });
                void runControl(feedback, description, () => jogAxis(axis, distance, JOG_FEEDRATES[axis] ?? DEFAULT_JOG_FEEDRATE));
            });
            jog.appendChild(b);
            jogButtons.push({ button: b, axis, direction });
            motionControls.push(b);
        }
        column.appendChild(jog);
        if (homed) {
            const home = button(t('ui.status.homeAxis', 'Home {axis}', { axis: letter }), 'btn btn-sm st-home');
            home.addEventListener('click', () => {
                void runControl(feedback, t('ui.status.action.home', 'Home {axes}', { axes: letter }), () => homeAxes([axis]));
            });
            column.appendChild(home);
            motionControls.push(home);
        }
        dro.appendChild(column);

        bindings.push(() => {
            setText(reading, formatNumber(numberAt(REPORTER, 'Position', axis), 3));
            if (homed) {
                const isHomed = valueAt(REPORTER, 'Homed', axis) === true;
                setText(homed, isHomed ? t('ui.status.homed', 'Homed') : t('ui.status.notHomed', 'Not homed'));
                homed.classList.toggle('on', isHomed);
                setTone(homed, isHomed ? 'ink' : 'caution');
            }
            if (offset) {
                const g92 = numberAt(MOTION, 'G92 offset', axis);
                offset.hidden = !Number.isFinite(g92) || Math.abs(g92) < 0.0005;
                setText(offset, `G92 ${g92 > 0 ? '+' : ''}${formatNumber(g92, 3)}`);
            }
        });
    }
    cell.appendChild(dro);
    if (unit) cell.querySelector('.st-label')?.append(` · ${translateStatusUnit(REPORTER, 'Position', axes[0], unit)}`);

    // Step size, home all and motors off.
    const controls = el('div', 'st-controls');
    const stepGroup = el('div', 'segmented');
    stepGroup.setAttribute('role', 'group');
    stepGroup.setAttribute('aria-label', t('ui.status.step', 'Step'));
    for (const step of JOG_STEPS) {
        const b = button(formatNumber(step, step < 1 ? 1 : 0));
        b.className = '';
        b.setAttribute('aria-pressed', String(layout.jogStep === step));
        b.addEventListener('click', () => {
            layout.jogStep = step;
            saveLayout();
            stepGroup.querySelectorAll('button').forEach(other => other.setAttribute('aria-pressed', String(other === b)));
            refresh();
        });
        stepGroup.appendChild(b);
    }
    const step = el('div', 'st-step-size');
    step.append(el('span', 'st-label', t('ui.status.step', 'Step')), stepGroup, el('span', 'st-label', 'mm'));
    const homeAll = button(t('ui.status.homeAll', 'Home all'));
    homeAll.addEventListener('click', () => {
        void runControl(feedback, t('ui.status.action.homeAll', 'Home all'), () => homeAxes([]));
    });
    const motorsOff = button(t('ui.status.motorsOff', 'Motors off'));
    motorsOff.addEventListener('click', () => {
        void runControl(feedback, t('ui.status.motorsOff', 'Motors off'), () => disableMotors());
    });
    motionControls.push(homeAll, motorsOff);
    const actions = el('div', 'st-control-actions');
    actions.append(homeAll, motorsOff);
    controls.append(step, actions);
    const lockNote = el('p', 'st-lock-note');
    cell.append(controls, lockNote);

    bindings.push(() => {
        const reason = motionLockReason();
        for (const control of motionControls) setDisabled(control, !!reason, reason);
        for (const { button: b, axis, direction } of jogButtons) {
            setText(b, signedStep(layout.jogStep, direction));
            const label = t('ui.status.action.jog', 'Move {axis} {distance} mm', { axis: axisLetter(axis), distance: signedStep(layout.jogStep, direction) });
            if (b.dataset.title !== label) {
                b.dataset.title = label;
                b.setAttribute('aria-label', label);
            }
        }
        lockNote.hidden = !reason || !connected;
        setText(lockNote, reason);
    });

    // Feedrate, overrides and modes.
    const rates = el('div', 'st-rates');
    const addRate = (label: string, update: (out: HTMLElement) => void) => {
        const out = el('b');
        const rate = el('span', 'st-rate');
        rate.append(el('span', 'st-label', label), out);
        rates.appendChild(rate);
        bindings.push(() => update(out));
    };
    if (keysOf(MOTION, 'Feedrate').includes('Effective feedrate')) {
        addRate(t('ui.status.feedrate', 'Feedrate'), out => {
            const d = formatValue({ module: MOTION, group: 'Feedrate', value: 'Effective feedrate' });
            setText(out, d.unit ? `${d.text} ${d.unit}` : d.text);
        });
    }
    if (keysOf(MOTION, 'Feedrate').includes('Feedrate scale')) {
        rates.appendChild(overrideStepper(
            t('ui.status.feed', 'Feed'), 'Feedrate', 'Feedrate scale',
            percent => t('ui.status.action.feed', 'Feed {percent} %', { percent }),
            setFeedrateScale, feedback));
    }
    if (keysOf(MOTION, 'Flow').includes('Flow scale')) {
        rates.appendChild(overrideStepper(
            t('ui.status.flow', 'Flow'), 'Flow', 'Flow scale',
            percent => t('ui.status.action.flow', 'Flow {percent} %', { percent }),
            setFlowScale, feedback));
    }
    if (hasGroup(MOTION, 'Modal state')) {
        addRate(t('ui.status.mode', 'Mode'), out => {
            const codes = ['Positioning', 'Units', 'E positioning']
                .map(key => MODE_CODES[stringAt(MOTION, 'Modal state', key)])
                .filter(Boolean);
            setText(out, codes.join(' · ') || '—');
        });
    }
    if (rates.childElementCount) cell.appendChild(rates);
    cell.appendChild(feedback);
}

// ----- Temperatures -----

interface TemperatureRow {
    name: string;
    title: string;
    subtitle: string;
    actual: StatusPath | null;
    target: StatusPath | null;
    band: number;
    sensor: string | null;
}

function temperatureRows(): TemperatureRow[] {
    const rows: TemperatureRow[] = [];
    const linked = new Set<string>();
    const thermistors = keysOf(THERMISTORS, 'Temperature');
    for (const heater of keysOf(HEATERS, 'Target')) {
        const info = heaterInfo(heater);
        if (!info.enabled) continue;
        const sensor = info.thermistor && thermistors.includes(info.thermistor) ? info.thermistor : null;
        if (sensor) linked.add(sensor);
        const label = translateStatusValueLabel(HEATERS, 'Target', heater, heater);
        rows.push({
            name: heater,
            title: info.role ? t(`ui.status.role.${info.role}`, info.role) : label,
            subtitle: info.role ? label : '',
            actual: sensor ? { module: THERMISTORS, group: 'Temperature', value: sensor } : null,
            target: { module: HEATERS, group: 'Target', value: heater },
            band: info.band,
            sensor,
        });
    }
    // Heaters with a role first, in the order hotend, bed, chamber.
    const rank = (row: TemperatureRow) => {
        const role = heaterInfo(row.name).role;
        return role ? HEATER_ROLES.indexOf(role) : HEATER_ROLES.length;
    };
    rows.sort((a, b) => rank(a) - rank(b));
    for (const sensor of thermistors) {
        if (linked.has(sensor)) continue;
        rows.push({
            name: sensor,
            title: translateStatusValueLabel(THERMISTORS, 'Temperature', sensor, sensor),
            subtitle: '',
            actual: { module: THERMISTORS, group: 'Temperature', value: sensor },
            target: null,
            band: 0,
            sensor,
        });
    }
    return rows;
}

function buildTemperatureCell(cell: HTMLElement) {
    const rows = temperatureRows();
    const layoutGrid = el('div', 'st-temps');
    const list = el('div', 'st-temp-list');
    const series: ChartSeries[] = [];
    const feedback = feedbackLine();

    rows.forEach((row, index) => {
        const color = LINE_COLORS[index % LINE_COLORS.length];
        if (row.actual) series.push({ key: pathKey(row.actual.module, row.actual.group, row.actual.value), style: { color } });
        if (row.target) series.push({ key: pathKey(row.target.module, row.target.group, row.target.value), style: { color, dash: TARGET_DASH } });

        const item = el('div', 'st-heater');
        const name = el('div', 'st-heater-name');
        name.append(swatch({ color }), el('span', '', row.title));
        if (row.subtitle) name.appendChild(el('small', '', row.subtitle));
        const reading = el('div', 'st-reading');
        const actual = el('b');
        const target = el('span', 'st-target');
        const tag = el('span', 'st-tag');
        reading.append(actual, target, tag);
        const gauge = el('div', 'st-gauge');
        const bandEl = el('span', 'st-gauge-band');
        const bar = el('span', 'st-gauge-bar');
        const tick = el('span', 'st-gauge-target');
        gauge.append(bandEl, bar, tick);
        item.append(name, reading, gauge);
        if (row.target) item.appendChild(heaterTargetControl(row, feedback));
        list.appendChild(item);

        bindings.push(() => {
            const a = row.actual ? numberAt(row.actual.module, row.actual.group, row.actual.value) : NaN;
            const tgt = row.target ? numberAt(row.target.module, row.target.group, row.target.value) : NaN;
            setText(actual, formatNumber(a, 1));
            const heating = Number.isFinite(tgt) && tgt > 0;
            if (!row.target) setText(target, t('ui.status.sensorOnly', 'sensor only'));
            else setText(target, heating ? `/ ${formatNumber(tgt, Number.isInteger(tgt) ? 0 : 1)}` : '');

            let state = '';
            let tone = 'muted';
            if (row.target && !row.actual) {
                state = t('ui.status.heater.noSensor', 'No sensor');
            } else if (row.target && !heating) {
                state = t('ui.status.heater.off', 'Off');
            } else if (row.target && Number.isFinite(a)) {
                if (Math.abs(a - tgt) <= row.band) [state, tone] = [t('ui.status.heater.atTarget', 'At target'), 'rev'];
                else if (a < tgt) [state, tone] = [t('ui.status.heater.heating', 'Heating'), 'caution'];
                else [state, tone] = [t('ui.status.heater.cooling', 'Cooling'), 'muted'];
            }
            setText(tag, state);
            tag.hidden = !state;
            setTone(tag, tone);

            const [lo, hi] = thermistorRange(row.sensor ?? '', [a, tgt]);
            const at = (x: number) => Math.min(1, Math.max(0, (x - lo) / (hi - lo)));
            setStyle(bar, 'width', `calc((100% - 4px) * ${Number.isFinite(a) ? at(a).toFixed(4) : 0})`);
            tick.hidden = !heating;
            bandEl.hidden = !heating || row.band <= 0;
            if (heating) {
                setStyle(tick, 'left', `calc(2px + (100% - 4px) * ${at(tgt).toFixed(4)})`);
                setStyle(bandEl, 'left', `calc(2px + (100% - 4px) * ${at(tgt - row.band).toFixed(4)})`);
                setStyle(bandEl, 'width', `calc((100% - 4px) * ${(at(tgt + row.band) - at(tgt - row.band)).toFixed(4)})`);
            }
        });
    });

    const chartBox = el('div', 'st-chart');
    const host = el('div', 'st-plot-host');
    chartBox.appendChild(host);
    if (rows.some(row => row.target)) {
        const key = el('div', 'st-key');
        key.append(
            el('span', '', t('ui.status.actual', 'actual')),
            el('span', 'dash', t('ui.status.target', 'target'))
        );
        chartBox.appendChild(key);
    }
    list.appendChild(feedback);
    layoutGrid.append(list, chartBox);
    cell.appendChild(layoutGrid);
    cell.querySelector('.st-label')?.append(' · °C');
    createChart(host, series);
}

// A new target for a heater. The field starts empty, with the current target as its placeholder, so live updates never
// overwrite what is being typed.
function heaterTargetControl(row: TemperatureRow, feedback: HTMLElement): HTMLElement {
    const control = el('div', 'st-heater-set');
    const input = el('input');
    input.type = 'number';
    input.min = '0';
    input.step = 'any';
    input.inputMode = 'decimal';
    input.setAttribute('aria-label', t('ui.status.targetFor', 'New target for {heater}', { heater: row.title }));
    const apply = button(t('ui.status.set', 'Set'));
    const off = button(t('ui.status.turnOff', 'Off'));
    control.append(el('span', 'st-label', t('ui.status.target', 'target')), input, el('span', 'st-step-unit', '°C'), apply, off);

    const send = (target: number) => {
        if (!Number.isFinite(target) || target < 0) {
            input.focus();
            return;
        }
        input.value = '';
        const action = target === 0
            ? t('ui.status.action.heaterOff', '{heater} off', { heater: row.title })
            : t('ui.status.action.heater', '{heater} to {target} °C', { heater: row.title, target: formatNumber(target, Number.isInteger(target) ? 0 : 1) });
        void runControl(feedback, action, () => setHeaterTarget(row.name, target));
    };
    apply.addEventListener('click', () => send(input.value === '' ? NaN : Number(input.value)));
    off.addEventListener('click', () => send(0));
    input.addEventListener('keydown', event => {
        if (event.key === 'Enter') send(input.value === '' ? NaN : Number(input.value));
        if (event.key === 'Escape') {
            input.value = '';
            input.blur();
        }
    });

    bindings.push(() => {
        const target = row.target ? numberAt(row.target.module, row.target.group, row.target.value) : NaN;
        const placeholder = Number.isFinite(target) ? formatNumber(target, Number.isInteger(target) ? 0 : 1) : '';
        if (input.placeholder !== placeholder) input.placeholder = placeholder;
        const reason = connected ? '' : t('ui.status.state.disconnected', 'Disconnected');
        for (const c of [input, apply, off]) setDisabled(c, !!reason, reason);
    });
    return control;
}

// ----- Fans -----

function enabledTachometers(): string[] {
    const config = moduleConfig(TACHOMETERS)?.Tachometers;
    return keysOf(TACHOMETERS, 'Speed').filter(name => !config || config[name]?.Enabled !== false);
}

function buildFanCell(cell: HTMLElement) {
    const defaultFan = moduleConfig(FANS)?.Gcode_Defaults?.Default_Fan;
    for (const fan of keysOf(FANS, 'Speed')) {
        const row = el('div', 'st-fan');
        const name = el('span', '', translateStatusValueLabel(FANS, 'Speed', fan, fan));
        if (fan === defaultFan) name.appendChild(el('small', '', ` · ${t('ui.status.defaultFan', 'M106')}`));
        const out = el('b');
        const bar = el('div', 'st-bar');
        const fill = el('span', 'st-bar-fill');
        bar.appendChild(fill);
        row.append(name, out, bar);
        cell.appendChild(row);
        bindings.push(() => {
            const speed = numberAt(FANS, 'Speed', fan);
            setText(out, formatPercent(speed));
            setStyle(fill, 'width', `calc((100% - 2px) * ${Number.isFinite(speed) ? Math.min(1, Math.max(0, speed)).toFixed(4) : 0})`);
        });
    }
    for (const tach of enabledTachometers()) {
        const row = el('div', 'st-fan');
        const out = el('b');
        row.append(el('span', '', translateStatusValueLabel(TACHOMETERS, 'Speed', tach, tach)), out);
        cell.appendChild(row);
        bindings.push(() => {
            const d = formatValue({ module: TACHOMETERS, group: 'Speed', value: tach });
            setText(out, d.unit ? `${d.text} ${d.unit}` : d.text);
        });
    }
}

// ----- Inputs and drivers -----

function buildInputCell(cell: HTMLElement) {
    if (hasGroup(SWITCHES, 'Triggered')) {
        const flags = el('div', 'st-flags');
        for (const input of keysOf(SWITCHES, 'Triggered')) {
            const f = flag(translateStatusValueLabel(SWITCHES, 'Triggered', input, input));
            flags.appendChild(f);
            bindings.push(() => {
                const on = valueAt(SWITCHES, 'Triggered', input) === true;
                f.classList.toggle('on', on);
                f.title = on ? t('ui.status.triggered', 'Triggered') : t('ui.status.notTriggered', 'Not triggered');
            });
        }
        cell.appendChild(flags);
    }
    if (schema[DRIVERS]) {
        const flags = el('div', 'st-flags st-driver-flags');
        cell.appendChild(flags);
        let shown = '';
        bindings.push(() => {
            const problems: { text: string; tone: string }[] = [];
            for (const [group, motors] of Object.entries(schema[DRIVERS])) {
                const tone = DRIVER_CAUTION.test(group) ? 'caution' : DRIVER_FAULT.test(group) ? 'fault' : '';
                if (!tone) continue;
                for (const motor of Object.keys(motors)) {
                    if (valueAt(DRIVERS, group, motor) === true) {
                        problems.push({
                            text: `${translateStatusValueLabel(DRIVERS, group, motor, motor)}: ${translateStatusGroupLabel(DRIVERS, group, group)}`,
                            tone,
                        });
                    }
                }
            }
            const signature = JSON.stringify(problems);
            if (signature === shown) return;
            shown = signature;
            flags.replaceChildren();
            if (!problems.length) {
                flags.appendChild(flag(t('ui.status.driversOk', 'Drivers OK')));
                return;
            }
            for (const problem of problems) {
                const f = flag(problem.text);
                f.classList.add('on');
                f.dataset.tone = problem.tone;
                flags.appendChild(f);
            }
        });
    }
}

// ----- Pinned charts -----

function pinnedPaths(): StatusPath[] {
    return layout.pins.map(parseKey).filter((p): p is StatusPath => !!p && isNumeric(schemaAt(p)));
}

function setPinned(p: StatusPath, pinned: boolean) {
    const key = pathKey(p.module, p.group, p.value);
    layout.pins = pinned ? [...layout.pins.filter(k => k !== key), key] : layout.pins.filter(k => k !== key);
    saveLayout();
    render();
}

function renderPinnedCharts(sheet: HTMLElement) {
    // Pins share a chart when they come from the same module and group and have the same unit.
    const groups = new Map<string, StatusPath[]>();
    for (const p of pinnedPaths()) {
        const id = JSON.stringify([p.module, p.group, schemaAt(p)?.Unit ?? '']);
        groups.set(id, [...(groups.get(id) ?? []), p]);
    }
    const cells: HTMLElement[] = [];
    for (const paths of groups.values()) {
        const first = paths[0];
        const s = schemaAt(first);
        const unit = s?.Unit === '×' ? '%' : s?.Unit ? translateStatusUnit(first.module, first.group, first.value, s.Unit) : '';
        const cell = el('section', 'st-cell st-chart-cell');
        const head = el('div', 'st-cell-head');
        const title = `${translateStatusModuleLabel(first.module, first.module)} · ${translateStatusGroupLabel(first.module, first.group, first.group)}${unit ? ` · ${unit}` : ''}`;
        head.appendChild(el('h3', 'st-label', title));
        const host = el('div', 'st-plot-host');
        const legend = el('div', 'st-legend');
        const series = paths.map((p, i) => ({ key: pathKey(p.module, p.group, p.value), style: lineStyle(i), path: p }));
        for (const { style, path } of series) {
            const row = el('div', 'st-legend-row');
            const out = el('b');
            const remove = el('button', 'st-unpin', '×');
            remove.type = 'button';
            remove.title = t('ui.status.unplot', 'Remove {value} from chart', { value: valueLabel(path) });
            remove.setAttribute('aria-label', remove.title);
            remove.addEventListener('click', () => setPinned(path, false));
            row.append(swatch(style), el('span', '', valueLabel(path)), out, remove);
            legend.appendChild(row);
            bindings.push(() => {
                const d = formatValue(path);
                setText(out, d.unit ? `${d.text} ${d.unit}` : d.text);
            });
        }
        cell.append(head, host, legend);
        cells.push(cell);
        sheet.appendChild(cell);
        createChart(host, series);
    }
    if (cells.length % 2 === 1) cells[cells.length - 1].classList.add('st-wide');
}

function lineStyle(index: number): LineStyle {
    return {
        color: LINE_COLORS[index % LINE_COLORS.length],
        dash: LINE_DASHES[Math.floor(index / LINE_COLORS.length) % LINE_DASHES.length],
    };
}

// ----- All values -----

function renderTables(container: HTMLElement) {
    container.replaceChildren();
    const pinned = new Set(layout.pins);
    for (const [module, groups] of Object.entries(schema)) {
        for (const [group, entries] of Object.entries(groups)) {
            const table = el('table', 'st-table');
            const caption = el('caption', 'st-label', `${translateStatusModuleLabel(module, module)} · ${translateStatusGroupLabel(module, group, group)}`);
            table.appendChild(caption);
            const body = el('tbody');
            for (const [key, valueSchema] of Object.entries(entries)) {
                const p = { module, group, value: key };
                const row = el('tr');
                row.dataset.search = `${caption.textContent} ${valueLabel(p)} ${module} ${group} ${key} ${valueSchema.Description ?? ''}`.toLowerCase();
                const label = el('td', '', valueLabel(p));
                if (valueSchema.Description) label.title = valueSchema.Description;
                const cell = el('td', isNumeric(valueSchema) ? 'num' : '');
                const plotCell = el('td', 'st-plot');
                row.append(label, cell, plotCell);
                body.appendChild(row);

                if (valueSchema.Kind === 'Boolean') {
                    const f = flag();
                    cell.appendChild(f);
                    bindings.push(() => {
                        const on = valueAt(module, group, key) === true;
                        setText(f, formatValue(p).text);
                        f.classList.toggle('on', on);
                    });
                    continue;
                }
                const text = el('span');
                const unit = el('span', 'unit');
                cell.append(text, unit);
                bindings.push(() => {
                    const d = formatValue(p);
                    setText(text, d.text);
                    setText(unit, d.unit ? ` ${d.unit}` : '');
                });
                if (isNumeric(valueSchema)) {
                    const isPinned = pinned.has(pathKey(module, group, key));
                    const button = el('button');
                    button.type = 'button';
                    button.appendChild(plotIcon());
                    button.setAttribute('aria-pressed', String(isPinned));
                    button.title = isPinned
                        ? t('ui.status.unplot', 'Remove {value} from chart', { value: valueLabel(p) })
                        : t('ui.status.plot', 'Plot {value}', { value: valueLabel(p) });
                    button.setAttribute('aria-label', button.title);
                    button.addEventListener('click', () => setPinned(p, !isPinned));
                    plotCell.appendChild(button);
                }
            }
            table.appendChild(body);
            container.appendChild(table);
        }
    }
}

function applyFilter() {
    let anyShown = false;
    document.querySelectorAll<HTMLTableElement>('#status-tables .st-table').forEach(table => {
        let shown = 0;
        table.querySelectorAll<HTMLTableRowElement>('tbody tr').forEach(row => {
            const match = !filterText || (row.dataset.search ?? '').includes(filterText);
            row.hidden = !match;
            if (match) shown++;
        });
        table.hidden = shown === 0;
        anyShown ||= shown > 0;
    });
    const empty = document.getElementById('status-filter-empty');
    if (empty) empty.hidden = anyShown;
}

// ---------- History and charts ----------

function sampleHistory(time: number) {
    historyTimes.push(time);
    for (const [module, groups] of Object.entries(schema)) {
        for (const [group, entries] of Object.entries(groups)) {
            for (const [key, valueSchema] of Object.entries(entries)) {
                if (!isNumeric(valueSchema)) continue;
                const id = pathKey(module, group, key);
                let series = historyValues.get(id);
                if (!series) {
                    series = new Array(historyTimes.length - 1).fill(NaN);
                    historyValues.set(id, series);
                }
                series.push(numberAt(module, group, key));
            }
        }
    }
    let expired = 0;
    while (expired < historyTimes.length && historyTimes[expired] < time - HISTORY_SECONDS) expired++;
    if (expired > 0) {
        historyTimes.splice(0, expired);
        historyValues.forEach(series => series.splice(0, expired));
    }
}

function latestTime(): number {
    return historyTimes.length ? historyTimes[historyTimes.length - 1] : Date.now() / 1000;
}

function agoLabel(secondsAgo: number): string {
    const seconds = Math.round(secondsAgo);
    if (seconds === 0) return t('ui.status.now', 'now');
    if (layout.window <= 60) return t('ui.status.secondsAgo', '−{n} s', { n: seconds });
    return t('ui.status.minutesAgo', '−{n} min', { n: formatNumber(seconds / 60, seconds % 60 === 0 ? 0 : 1) });
}

function createChart(host: HTMLElement, series: ChartSeries[]) {
    if (!series.length) return;
    const muted = () => themeColor('--ink-muted');
    const opts: uPlot.Options = {
        width: Math.max(1, Math.floor(host.clientWidth)), // The resize observer sizes the plot once it is in the page.
        height: CHART_HEIGHT,
        legend: { show: false },
        cursor: { drag: { x: false, y: false }, points: { show: false } },
        scales: {
            x: { time: false, range: () => [latestTime() - layout.window, latestTime()] },
        },
        series: [
            {},
            ...series.map(s => ({ stroke: () => themeColor(s.style.color), dash: s.style.dash, width: 1.5, points: { show: false } })),
        ],
        axes: [
            {
                font: PLOT_FONT,
                stroke: muted,
                size: 28,
                ticks: { stroke: () => themeColor('--ink'), width: 1, size: 5 },
                grid: { stroke: () => themeColor('--construction'), width: 0.8, dash: [3, 3] },
                splits: () => [4, 3, 2, 1, 0].map(i => latestTime() - (layout.window * i) / 4),
                values: (_u: uPlot, splits: number[]) => splits.map(s => agoLabel(latestTime() - s)),
            },
            {
                font: PLOT_FONT,
                stroke: muted,
                size: 52,
                ticks: { stroke: () => themeColor('--ink'), width: 1, size: 5 },
                grid: { stroke: () => themeColor('--construction'), width: 0.8, dash: [3, 3] },
            },
        ],
    };
    const plot = new uPlot(opts, [[], ...series.map(() => [])] as uPlot.AlignedData, host);
    charts.push({ plot, host, series });
    chartResizeObserver.observe(host);
}

function updateCharts() {
    if (!charts.length) return;
    const start = lowerBound(historyTimes, latestTime() - layout.window);
    const times = historyTimes.slice(start);
    for (const chart of charts) {
        const data = [times, ...chart.series.map(s => (historyValues.get(s.key) ?? []).slice(start))];
        chart.plot.setData(data as uPlot.AlignedData);
    }
}

function lowerBound(sorted: number[], value: number): number {
    let lo = 0, hi = sorted.length;
    while (lo < hi) {
        const mid = (lo + hi) >> 1;
        if (sorted[mid] < value) lo = mid + 1;
        else hi = mid;
    }
    return lo;
}
