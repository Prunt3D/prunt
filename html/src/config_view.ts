import { fetchConfigSchema, fetchConfigValues, patchConfigValues } from './api.js';
import { renderDescription } from './description_markup.js';
import { ConfigCategory, configCategoriesFromSchema, configCategoryForProperty, matchesConfigSearch } from './config_browser.js';
import { revealConfigElement } from './navigation.js';
import { wsClient } from './ws.js';
import {
    onLocaleChange,
    t,
    translateConfigDescription,
    translateConfigCategory,
    translateConfigLabel,
    translateConfigOption,
    translateConfigUnit
} from './localization.js';

let currentSchema: any = null;
// The form's working copy. Rendered inputs hold the newest values; this holds everything else, such as hidden fields.
let currentValues: any = null;
// The stored configuration the working copy was taken from, and its revision. See "Changes made elsewhere" below.
let baseValues: any = null;
let baseRevision = '';
let currentErrors: any[] = [];
let dynamicPresentationControllerPaths = new Set<string>();

let devMode = false;
let showExperimental = false;
let configCategories: ConfigCategory[] = [];
let selectedCategory: string | null = null;
let searchQuery = '';
let showHelp = true;
const selectedConfigTabs = new Map<string, string>();

function configInputInvalid(input: HTMLInputElement | HTMLSelectElement): boolean {
    return !input.checkValidity() || !!input.closest('.config-value-field')?.querySelector('.range-hint.has-error');
}

function groupContainsError(group: HTMLElement, path: string[]): boolean {
    return Array.from(group.children).some(child => {
        const encoded = (child as HTMLElement).dataset.path;
        if (!encoded) return false;
        const root: string[] = JSON.parse(encoded);
        return root.slice(0, Math.min(root.length, path.length)).every((segment, index) => segment === path[index]);
    });
}

export async function initConfigView() {
    const btnSave = document.getElementById('btn-save-config');
    btnSave?.addEventListener('click', () => void saveConfiguration());

    const updateRealtimeFrontendErrors = () => {
        const container = document.getElementById('config-form');
        if (!container) return;
        const inputs = Array.from(container.querySelectorAll('input, select')) as (HTMLInputElement | HTMLSelectElement)[];
        const frontendErrors: any[] = [];
        for (const input of inputs) {
            const invalid = configInputInvalid(input);
            if (invalid) input.setAttribute('aria-invalid', 'true');
            else input.removeAttribute('aria-invalid');
            if (invalid && input.dataset.path) {
                frontendErrors.push({
                    Path: JSON.parse(input.dataset.path),
                    Message: input.validationMessage || "Invalid value" // We won't show it but we need it for handleErrors structure
                });
            }
        }

        // Update has-error classes based on both server and frontend
        document.querySelectorAll('.config-group').forEach(el => {
            el.classList.remove('has-error', 'has-server-error');
        });
        document.querySelectorAll('.config-tab').forEach(el => {
            el.classList.remove('has-error', 'has-server-error');
        });

        const isPrefix = (prefix: string[], full: string[]) => {
            if (prefix.length > full.length) return false;
            for (let i = 0; i < prefix.length; i++) {
                if (prefix[i] !== full[i]) return false;
            }
            return true;
        };

        const applyErrorClass = (errors: any[], className: string) => {
            errors.forEach(err => {
                // Outlines
                document.querySelectorAll('.config-group').forEach(el => {
                    const elPathStr = (el as HTMLElement).dataset.groupPath;
                    if (elPathStr) {
                        const elPath = JSON.parse(elPathStr);
                        if (elPath.length === 0 || (isPrefix(elPath, err.Path) && groupContainsError(el as HTMLElement, err.Path))) {
                            el.classList.add(className);
                        }
                    }
                });
                document.querySelectorAll('.config-tab').forEach(el => {
                    const elPathStr = (el as HTMLElement).dataset.tabPath;
                    if (elPathStr && elPathStr !== 'null') {
                        const elPath = JSON.parse(elPathStr);
                        if (isPrefix(elPath, err.Path)) {
                            el.classList.add(className);
                        }
                    }
                });
            });
        };

        applyErrorClass(currentErrors, 'has-server-error');
        applyErrorClass(frontendErrors, 'has-error');
        updateCategoryErrorIndicators();

        // Update nav item
        const configNavItem = document.querySelector('.config-nav-item');
        configNavItem?.classList.remove('has-error', 'has-server-error');
        if (frontendErrors.length > 0) {
            configNavItem?.classList.add('has-error');
        } else if (currentErrors.length > 0) {
            configNavItem?.classList.add('has-server-error');
        }
    };

    const search = document.getElementById('config-search') as HTMLInputElement | null;
    search?.addEventListener('input', () => {
        searchQuery = search.value;
        updateConfigBrowser();
    });

    document.getElementById('config-help-toggle')?.addEventListener('click', () => {
        showHelp = !showHelp;
        updateConfigBrowser();
    });

    const experimentalToggle = document.getElementById('config-experimental-toggle') as HTMLInputElement;
    if (experimentalToggle) {
        showExperimental = experimentalToggle.checked;
        experimentalToggle.addEventListener('change', () => {
            const updatedValues = scrapeFormValues();
            if (updatedValues) currentValues = updatedValues;
            showExperimental = experimentalToggle.checked;
            renderConfigForm();
            handleErrors(currentErrors);
            document.getElementById('config-form')?.dispatchEvent(new Event('input', { bubbles: true }));
        });
    }

    const devToggle = document.getElementById('config-dev-mode-toggle') as HTMLInputElement;
    if (devToggle) {
        devMode = devToggle.checked;
        devToggle.addEventListener('change', (e) => {
            if (currentSchema && currentValues) {
                const updatedValues = scrapeFormValues();
                if (updatedValues) currentValues = updatedValues;
            }
            devMode = (e.target as HTMLInputElement).checked;
            renderConfigForm();
            if (currentErrors && currentErrors.length > 0) {
                handleErrors(currentErrors);
            }
        });
    }

    const form = document.getElementById('config-form');
    if (form) {
        form.addEventListener('input', () => {
            updateRealtimeFrontendErrors();
        });

        form.addEventListener('change', (e: Event) => {
            const target = e.target as HTMLInputElement | HTMLSelectElement;
            if (!target?.dataset.path) return;
            const targetPath = JSON.parse(target.dataset.path);
            if (!dynamicPresentationControllerPaths.has(JSON.stringify(targetPath))) {
                updateConfigBrowser();
                return;
            }

            const updatedValues = scrapeFormValues();
            if (updatedValues) currentValues = updatedValues;
            renderConfigForm();
            if (currentErrors.length > 0) handleErrors(currentErrors);
        });
    }

    onLocaleChange(() => {
        if (currentSchema && currentValues) {
            const updatedValues = scrapeFormValues();
            if (updatedValues) currentValues = updatedValues;
            renderConfigForm();
            if (currentErrors && currentErrors.length > 0) {
                handleErrors(currentErrors);
            }
            const currentForm = document.getElementById('config-form');
            if (currentForm) {
                currentForm.dispatchEvent(new Event('input', { bubbles: true }));
            }
        }
    });

    initStoredChangesUI();
    wsClient.on('tick', (message: any) => {
        if (typeof message?.Config_Save_Count === 'number') noteConfigSaveCount(message.Config_Save_Count);
    });

    try {
        currentSchema = await fetchConfigSchema();
        dynamicPresentationControllerPaths = collectDynamicPresentationControllerPaths(currentSchema);
        await runConfigRequest(async () => {
            const res = await fetchConfigValues();
            latestStored = { values: res.Values, revision: res.Revision, errors: res.Errors ?? [] };
            showStoredConfig(latestStored);
        });
    } catch (e) {
        console.error("Failed to init config", e);
    }
}

function collectDynamicPresentationControllerPaths(schema: any): Set<string> {
    const result = new Set<string>();

    const visit = (value: any) => {
        if (!value || typeof value !== 'object') return;
        const condition = value.Present_When;
        if (typeof condition?.Owner === 'string' && Array.isArray(condition.Path)) {
            result.add(JSON.stringify(['Config', condition.Owner, 'Config', ...condition.Path]));
        }
        for (const child of Object.values(value)) visit(child);
    };

    visit(schema);
    return result;
}

function valueAtConfigPath(owner: string, path: string[]): any {
    return valueAt(currentValues, ['Config', owner, 'Config', ...path]);
}

function isDynamicallyPresented(schema: any): boolean {
    if (schema?.Experimental && !showExperimental) return false;
    const condition = schema?.Present_When;
    if (!condition) return true;
    if (typeof condition.Owner !== 'string' || !Array.isArray(condition.Path) || !Array.isArray(condition.Values)) {
        return false;
    }
    return condition.Values.includes(valueAtConfigPath(condition.Owner, condition.Path));
}

function categoryLabel(id: string): string {
    const category = configCategories.find(item => item.id === id);
    if (!category) return '';
    return category.id ? translateConfigCategory(category.id, category.label) : t('ui.config.otherCategory', category.label);
}

function renderConfigForm() {
    const container = document.getElementById('config-form');
    const navigation = document.getElementById('config-categories');
    if (!container || !navigation || !currentSchema) return;
    navigation.setAttribute('aria-label', t('ui.config.categoriesLabel', 'Categories'));

    container.replaceChildren();
    navigation.replaceChildren();
    configCategories = configCategoriesFromSchema(currentSchema);
    for (const category of configCategories) {
        const panel = document.createElement('section');
        panel.className = 'config-category-panel';
        panel.dataset.category = category.id;
        panel.setAttribute('aria-label', categoryLabel(category.id));

        for (const [moduleName, moduleSchema] of Object.entries(currentSchema.Config || {})) {
            const properties = (moduleSchema as any).Config || {};
            const path = ['Config', moduleName, 'Config'];
            const values = currentValues?.Config?.[moduleName]?.Config || {};
            const group = document.createElement('div');
            group.className = 'config-group';
            group.dataset.groupPath = JSON.stringify(path);

            const title = document.createElement('h3');
            title.textContent = translateConfigLabel(path, moduleName);
            group.appendChild(title);
            for (const [name, schema] of Object.entries(properties)) {
                if (configCategoryForProperty(schema as any) !== category.id) continue;
                const field = createField(name, schema, values[name], [...path, name], {
                    showLabel: translateConfigLabel([...path, name], name) !== categoryLabel(category.id)
                });
                if (field) group.appendChild(field);
            }
            if (group.children.length > 1) panel.appendChild(group);
        }
        if (!panel.children.length) continue;
        // The category heading supplies the title when there is only one contributing module.
        if (panel.children.length === 1) panel.querySelector('h3')?.remove();
        container.appendChild(panel);

        const button = document.createElement('button');
        button.type = 'button';
        button.className = 'config-category-button';
        button.dataset.category = category.id;
        const label = document.createElement('span');
        label.textContent = categoryLabel(category.id);
        const indicator = document.createElement('span');
        indicator.className = 'config-category-error';
        indicator.setAttribute('aria-hidden', 'true');
        button.append(label, indicator);
        button.addEventListener('click', () => {
            selectedCategory = category.id;
            searchQuery = '';
            const search = document.getElementById('config-search') as HTMLInputElement | null;
            if (search) search.value = '';
            updateConfigBrowser();
            const content = document.querySelector<HTMLElement>('.content-area');
            if (content) content.scrollTop = 0;
        });
        navigation.appendChild(button);
    }
    updateConfigBrowser();
}

function updateConfigBrowser() {
    const container = document.getElementById('config-form');
    const results = document.getElementById('config-search-results');
    const heading = document.getElementById('config-category-title');
    if (!container || !results || !heading) return;
    const panels = Array.from(container.querySelectorAll<HTMLElement>('.config-category-panel'));
    if (!panels.some(panel => panel.dataset.category === selectedCategory)) {
        selectedCategory = panels[0]?.dataset.category || '';
    }
    const searching = searchQuery.trim().length > 0;
    for (const panel of panels) panel.hidden = panel.dataset.category !== selectedCategory;
    for (const button of Array.from(document.querySelectorAll<HTMLButtonElement>('.config-category-button'))) {
        button.setAttribute('aria-pressed', String(!searching && button.dataset.category === selectedCategory));
    }
    heading.textContent = searching ? t('ui.config.searchResults', 'Search results') : categoryLabel(selectedCategory ?? '');
    const help = document.getElementById('config-help-toggle');
    if (help) {
        help.hidden = searching;
        help.textContent = showHelp ? t('ui.config.hideHelp', 'Hide help') : t('ui.config.showHelp', 'Show help');
        help.setAttribute('aria-pressed', String(showHelp));
    }
    container.classList.toggle('config-hide-help', !showHelp);
    container.hidden = searching;
    results.hidden = !searching;
    results.replaceChildren();
    if (searching) {
        const matches = Array.from(container.querySelectorAll<HTMLElement>('.form-group[data-search-label]'))
            .filter(field => {
                const text = [field.dataset.searchLabel, field.dataset.searchDescription, field.dataset.searchBreadcrumb, field.dataset.path].join(' ');
                return matchesConfigSearch(text, searchQuery);
            });
        const count = document.createElement('p');
        count.className = 'config-search-count';
        count.setAttribute('role', 'status');
        count.textContent = matches.length
            ? t('ui.config.searchCount', 'Matching settings: {count}', { count: matches.length })
            : t('ui.config.noSearchResults', 'No matching settings.');
        results.appendChild(count);
        for (const field of matches) {
            const button = document.createElement('button');
            button.type = 'button';
            button.className = 'config-search-result';
            const label = document.createElement('span');
            label.textContent = field.dataset.searchLabel!;
            const breadcrumb = document.createElement('small');
            breadcrumb.textContent = field.dataset.searchBreadcrumb!;
            button.append(label, breadcrumb);
            button.addEventListener('click', () => revealConfigElement(field));
            results.appendChild(button);
        }
    }
    updateCategoryErrorIndicators();
}

function updateCategoryErrorIndicators() {
    for (const button of Array.from(document.querySelectorAll<HTMLButtonElement>('.config-category-button'))) {
        const panel = document.querySelector<HTMLElement>(`.config-category-panel[data-category='${CSS.escape(button.dataset.category!)}']`);
        const invalid = !!panel?.querySelector('.config-group.has-error');
        const serverError = !!panel?.querySelector('.config-group.has-server-error');
        button.classList.toggle('has-error', invalid);
        button.classList.toggle('has-server-error', serverError);
        const label = categoryLabel(button.dataset.category!);
        button.setAttribute('aria-label', invalid || serverError
            ? t('ui.config.categoryErrors', '{category}: contains errors', { category: label }) : label);
        const indicator = button.querySelector('.config-category-error');
        if (indicator) indicator.textContent = invalid || serverError ? '!' : '';
    }
}

function configBreadcrumb(path: string[]): string {
    return path
        .map((segment, index) => ({ segment, index }))
        .filter(({ segment, index }) => !(segment === 'Config' && (index === 0 || index === 2)) && segment !== 'Children' && segment !== 'Selected')
        .map(({ segment, index }) => translateConfigLabel(path.slice(0, index + 1), segment))
        .join(' › ');
}

type FieldRenderOptions = {
    showLabel?: boolean;
    redundantTitleContext?: string | null;
};

function getOnlyChildName(schema: any): string | null {
    if (!schema?.Children) return null;
    const childNames = Object.keys(schema.Children);
    return childNames.length === 1 ? childNames[0] : null;
}

// Recursively builds the form fields
function buildProperties(schemaMap: any, valuesMap: any, parentEl: HTMLElement, path: string[]): boolean {
    let hasAdded = false;
    for (const [propName, propSchema] of Object.entries(schemaMap)) {
        const val = valuesMap ? valuesMap[propName] : undefined;
        const fieldEl = createField(propName, propSchema, val, [...path, propName]);
        if (fieldEl) {
            parentEl.appendChild(fieldEl);
            hasAdded = true;
        }
    }
    return hasAdded;
}

function createField(
    name: string,
    schema: any,
    value: any,
    path: string[],
    options: FieldRenderOptions = {}
): HTMLElement | null {
    if (!isDynamicallyPresented(schema)) return null;

    const fieldDiv = document.createElement('div');
    fieldDiv.className = 'form-group';
    fieldDiv.dataset.path = JSON.stringify(path);
    let hasContent = false;
    if (schema.Kind !== 'Sequence') {
        fieldDiv.dataset.searchLabel = translateConfigLabel(path, name || path[path.length - 1]);
        fieldDiv.dataset.searchDescription = translateConfigDescription(path, schema.Description || '');
        fieldDiv.dataset.searchBreadcrumb = configBreadcrumb(path);
    }

    if (name && options.showLabel !== false) {
        const label = document.createElement('label');
        label.innerText = translateConfigLabel(path, name);
        fieldDiv.appendChild(label);
        hasContent = true;
    }

    if (schema.Experimental) {
        const badge = document.createElement('span');
        badge.className = 'description';
        badge.classList.add('config-experimental-badge');
        badge.innerText = t('ui.config.experimental', 'Experimental');
        fieldDiv.appendChild(badge);
    }

    const description = translateConfigDescription(path, schema.Description || '');
    if (description) {
        renderDescription(fieldDiv, description);
        hasContent = true;
    }

    if (devMode && path && path.length > 0) {
        const pathDesc = document.createElement('p');
        pathDesc.className = 'description dev-mode-path';
        pathDesc.innerText = JSON.stringify(path).replace(/,/g, ', ');
        fieldDiv.appendChild(pathDesc);
        hasContent = true;
    }

    const errorSpan = document.createElement('span');
    errorSpan.className = 'error-message';
    errorSpan.id = `err-${path.join('-')}`;
    errorSpan.classList.add('d-none');

    let inputArea: HTMLElement | null = null;
    let actualValue = value !== undefined ? value : schema.Default;

    switch (schema.Kind) {
        case 'Boolean':
            inputArea = createBooleanInput(path, actualValue);
            break;
        case 'String':
            inputArea = createStringInput(path, actualValue);
            break;
        case 'Integer':
        case 'Float':
            inputArea = createNumberInput(path, actualValue, schema);
            break;
        case 'Discrete':
            inputArea = createSelectInput(path, actualValue, schema.Options, opt => translateConfigOption(path, opt, opt));
            break;
        case 'Float_Ratio':
            inputArea = createRatioInput(path, actualValue || { Numerator: schema.Default_Numerator, Denominator: schema.Default_Denominator }, schema);
            break;
        case 'Variant':
            inputArea = createVariantInput(path, actualValue, schema);
            break;
        case 'Sequence':
            inputArea = createSequenceInput(path, actualValue, schema, options.redundantTitleContext ?? null);
            break;
        default:
            console.warn(`Unknown config kind: ${schema.Kind}`);
    }

    if (inputArea) {
        if (schema.Kind !== 'Sequence' && schema.Kind !== 'Variant') {
            fieldDiv.classList.add('config-value-field');
            const copy = document.createElement('div');
            copy.className = 'config-field-copy';
            copy.append(...Array.from(fieldDiv.childNodes));
            fieldDiv.appendChild(copy);
            inputArea.classList.add('config-field-control');
        }
        if (schema.Kind !== 'Sequence') {
            const control = inputArea.matches('input, select') ? inputArea : inputArea.querySelector('input, select');
            if (control) {
                control.id = `config-input-${encodeURIComponent(JSON.stringify(path))}`;
                const label = fieldDiv.querySelector<HTMLLabelElement>('label:not(.toggle-switch)');
                if (label) label.htmlFor = control.id;
            }
        }
        fieldDiv.appendChild(inputArea);
        fieldDiv.appendChild(errorSpan);
        return fieldDiv;
    }
    return hasContent ? fieldDiv : null;
}

function createBooleanInput(path: string[], value: boolean): HTMLElement {
    const wrap = document.createElement('label');
    wrap.className = 'toggle-switch';
    const input = document.createElement('input');
    input.type = 'checkbox';
    input.checked = !!value;
    input.dataset.path = JSON.stringify(path);
    input.className = 'config-input-bool';
    input.setAttribute('aria-label', translateConfigLabel(path, path[path.length - 1]));

    const slider = document.createElement('span');
    slider.className = 'slider';

    wrap.appendChild(input);
    wrap.appendChild(slider);
    return wrap;
}

function createStringInput(path: string[], value: string): HTMLElement {
    const input = document.createElement('input');
    input.type = 'text';
    input.required = true;
    input.value = value ?? '';
    input.dataset.path = JSON.stringify(path);
    input.className = 'config-input-string';
    input.setAttribute('aria-label', translateConfigLabel(path, path[path.length - 1]));
    return input;
}

function createNumberInput(path: string[], value: number, schema: any): HTMLElement {
    const container = document.createElement('div');

    const wrap = document.createElement('div');
    wrap.className = 'form-control-row';
    const input = document.createElement('input');
    input.type = 'number';
    input.required = true;
    input.value = value?.toString() || '0';
    if (schema.Kind === 'Float') input.step = 'any';
    if (schema.Min !== undefined) input.min = schema.Min;
    if (schema.Max !== undefined) input.max = schema.Max;
    input.dataset.path = JSON.stringify(path);
    input.className = schema.Kind === 'Integer' ? 'config-input-int' : 'config-input-float';
    input.setAttribute('aria-label', translateConfigLabel(path, path[path.length - 1]));
    wrap.appendChild(input);

    if (schema.Unit) {
        const unit = document.createElement('span');
        unit.innerText = translateConfigUnit(path, schema.Unit);
        wrap.appendChild(unit);
    }
    container.appendChild(wrap);

    if (schema.Min !== undefined || schema.Max !== undefined) {
        const text = [];
        if (schema.Min !== undefined) text.push(t('ui.config.min', 'Min: {value}', { value: schema.Min }));
        if (schema.Max !== undefined) text.push(t('ui.config.max', 'Max: {value}', { value: schema.Max }));

        const rangeSpan = document.createElement('div');
        rangeSpan.className = 'range-hint';
        rangeSpan.innerText = text.join(', ');
        container.appendChild(rangeSpan);

        const checkValidity = () => {
            let valid = true;
            if (input.value === '') {
                valid = false;
            } else {
                const numVal = parseFloat(input.value);
                if (schema.Min !== undefined && numVal < schema.Min) {
                    valid = false;
                }
                if (schema.Max !== undefined && numVal > schema.Max) {
                    valid = false;
                }
            }

            rangeSpan.classList.toggle('has-error', !valid);

            // Dispatch input event to form to trigger global client error check if we are simulating one
            if ((input as any)._isSelfFiring) return;
            (input as any)._isSelfFiring = true;
            input.dispatchEvent(new Event('input', { bubbles: true }));
            (input as any)._isSelfFiring = false;
        };
        input.addEventListener('input', checkValidity);
        setTimeout(checkValidity, 0); // Initial check
    }

    return container;
}

function createSelectInput(
    path: string[],
    value: string,
    options: string[],
    translateOption: (option: string) => string = option => translateConfigOption(path, option, option)
): HTMLElement {
    const select = document.createElement('select');
    select.dataset.path = JSON.stringify(path);
    select.className = 'config-input-discrete';
    select.setAttribute('aria-label', translateConfigLabel(path, path[path.length - 1]));
    options.forEach(opt => {
        const option = document.createElement('option');
        option.value = opt;
        option.innerText = translateOption(opt);
        if (opt === value) option.selected = true;
        select.appendChild(option);
    });
    return select;
}

function createRatioInput(path: string[], value: any, schema: any): HTMLElement {
    const container = document.createElement('div');

    const wrap = document.createElement('div');
    wrap.className = 'ratio-group';

    const num = document.createElement('input');
    num.type = 'number';
    num.step = 'any';
    num.required = true;
    num.value = value.Numerator?.toString() || '0';
    num.dataset.path = JSON.stringify([...path, 'Numerator']);
    num.className = 'config-input-ratio-num';
    num.setAttribute('aria-label', t('ui.config.ratioNumerator', '{label}: numerator', { label: translateConfigLabel(path, path[path.length - 1]) }));

    const sep = document.createElement('span');
    sep.innerText = ':';

    const den = document.createElement('input');
    den.type = 'number';
    den.step = 'any';
    den.required = true;
    den.value = value.Denominator?.toString() || '1';
    den.dataset.path = JSON.stringify([...path, 'Denominator']);
    den.className = 'config-input-ratio-den';
    den.setAttribute('aria-label', t('ui.config.ratioDenominator', '{label}: denominator', { label: translateConfigLabel(path, path[path.length - 1]) }));

    wrap.appendChild(num);
    wrap.appendChild(sep);
    wrap.appendChild(den);
    container.appendChild(wrap);

    if (schema && (schema.Min !== undefined || schema.Max !== undefined)) {
        const text = [];
        if (schema.Min !== undefined) text.push(t('ui.config.min', 'Min: {value}', { value: schema.Min }));
        if (schema.Max !== undefined) text.push(t('ui.config.max', 'Max: {value}', { value: schema.Max }));

        const rangeSpan = document.createElement('div');
        rangeSpan.className = 'range-hint';
        rangeSpan.innerText = text.join(', ');
        container.appendChild(rangeSpan);

        const checkValidity = () => {
            const n = parseFloat(num.value);
            const d = parseFloat(den.value);
            let valid = true;
            if (isNaN(n) || isNaN(d) || d === 0) {
                valid = false;
            } else {
                const ratio = n / d;
                if (schema.Min !== undefined && ratio < schema.Min) {
                    valid = false;
                }
                if (schema.Max !== undefined && ratio > schema.Max) {
                    valid = false;
                }
            }

            rangeSpan.classList.toggle('has-error', !valid);

            if (!(num as any)._isSelfFiring) {
                (num as any)._isSelfFiring = true;
                num.dispatchEvent(new Event('input', { bubbles: true }));
                (num as any)._isSelfFiring = false;
            }
        };

        num.addEventListener('input', checkValidity);
        den.addEventListener('input', checkValidity);
        setTimeout(checkValidity, 0); // Initial check
    }

    return container;
}

function createVariantInput(path: string[], value: any, schema: any): HTMLElement {
    const wrap = document.createElement('div');
    wrap.className = 'variant-group';

    const selectedName = value?.Selected || schema.Default;
    // Keep the current choice visible so hiding options can never silently change it.
    const selectStr = Object.keys(schema.Children).filter(option =>
        showExperimental || !schema.Children[option].Experimental || option === selectedName);

    const select = createSelectInput(
        [...path, 'Selected'],
        selectedName,
        selectStr,
        option => {
            const label = translateConfigLabel([...path, 'Children', option], option);
            return schema.Children[option].Experimental
                ? t('ui.config.experimentalOption', '{label} (Experimental)', { label })
                : label;
        }
    );
    select.className = 'config-input-variant';
    select.setAttribute('aria-label', translateConfigLabel(path, path[path.length - 1]));
    if (!showExperimental) {
        for (const option of Array.from((select as HTMLSelectElement).options)) {
            option.disabled = !!schema.Children[option.value].Experimental;
        }
    }
    wrap.appendChild(select);

    const childrenContainer = document.createElement('div');
    childrenContainer.className = 'variant-children';
    wrap.appendChild(childrenContainer);

    // Re-render children when selection changes
    const renderActiveChild = (isInit = false) => {
        if (!isInit) {
            const updated = scrapeFormValues();
            if (updated) {
                currentValues = updated;
                let cur = currentValues;
                for (let i = 0; i < path.length; i++) {
                    if (cur) cur = cur[path[i]];
                }
                value = cur;
            }
        }

        childrenContainer.innerHTML = '';
        const activeName = (select as HTMLSelectElement).value;
        const activeSchema = schema.Children[activeName];
        if (activeSchema) {
            if (activeSchema.Kind === "Sequence" && !activeSchema.Tabbed) {
                childrenContainer.className = 'variant-children variant-children-flush';
            } else {
                childrenContainer.className = 'variant-children variant-children-indented';
            }
            const mappedChildVal = value?.Children?.[activeName];
            const childEl = createField("", activeSchema, mappedChildVal, [...path, 'Children', activeName], {
                redundantTitleContext: activeName
            });
            if (childEl) childrenContainer.appendChild(childEl);
        }
        if (!isInit) {
            updateConfigBrowser();
            // Refresh validation after removing inputs from the previous variant.
            wrap.dispatchEvent(new Event('input', { bubbles: true }));
        }
    };

    select.addEventListener('change', () => renderActiveChild(false));
    renderActiveChild(true);

    return wrap;
}

function createSequenceInput(
    path: string[],
    value: any,
    schema: any,
    redundantTitleContext: string | null = null
): HTMLElement | null {
    if (!isDynamicallyPresented(schema)) return null;
    if (!schema.Children || Object.keys(schema.Children).length === 0) {
        return null;
    }

    const wrap = document.createElement('div');
    wrap.classList.add('mt-8');

    if (schema.Tabbed) {
        wrap.className = 'sequence-group tabbed';
        const tabContainer = document.createElement('div');
        tabContainer.className = 'config-tabs';
        tabContainer.setAttribute('role', 'tablist');
        tabContainer.setAttribute('aria-label', translateConfigLabel(path, path[path.length - 1]));
        const contentContainer = document.createElement('div');
        contentContainer.className = 'tab-content';

        let first = true;
        let hasAnyTab = false;
        for (const [childName, childSchema] of Object.entries(schema.Children)) {
            const childVal = value ? value[childName] : undefined;

            const pane = document.createElement('div');
            const f = createField(childName, childSchema, childVal, [...path, childName], {
                showLabel: false,
                redundantTitleContext: childName
            });
            if (f) {
                pane.appendChild(f);

                const tabBtn = document.createElement('button');
                tabBtn.type = 'button';
                tabBtn.setAttribute('role', 'tab');
                tabBtn.setAttribute('aria-selected', String(first));
                tabBtn.className = `config-tab ${first ? 'active' : ''}`;
                tabBtn.innerText = translateConfigLabel([...path, childName], childName);
                tabBtn.dataset.tabPath = JSON.stringify([...path, childName]);
                pane.className = `tab-pane ${first ? 'active' : ''}`;
                pane.dataset.tabPath = JSON.stringify([...path, childName]);
                pane.id = `config-pane-${encodeURIComponent(pane.dataset.tabPath)}`;
                tabBtn.id = `${pane.id}-tab`;
                tabBtn.setAttribute('aria-controls', pane.id);
                pane.setAttribute('role', 'tabpanel');
                pane.setAttribute('aria-labelledby', tabBtn.id);

                tabBtn.addEventListener('click', () => {
                    Array.from(tabContainer.children).forEach(c => {
                        c.classList.remove('active');
                        c.setAttribute('aria-selected', 'false');
                    });
                    Array.from(contentContainer.children).forEach((c: any) => c.classList.remove('active'));
                    tabBtn.classList.add('active');
                    tabBtn.setAttribute('aria-selected', 'true');
                    pane.classList.add('active');
                    selectedConfigTabs.set(JSON.stringify(path), childName);
                });
                tabBtn.addEventListener('keydown', event => {
                    const tabs = Array.from(tabContainer.querySelectorAll<HTMLButtonElement>('button'));
                    const index = tabs.indexOf(tabBtn);
                    const next = event.key === 'ArrowRight' ? tabs[(index + 1) % tabs.length]
                        : event.key === 'ArrowLeft' ? tabs[(index + tabs.length - 1) % tabs.length]
                        : event.key === 'Home' ? tabs[0] : event.key === 'End' ? tabs[tabs.length - 1] : null;
                    if (next) { event.preventDefault(); next.click(); next.focus(); }
                });

                tabContainer.appendChild(tabBtn);
                contentContainer.appendChild(pane);
                first = false;
                hasAnyTab = true;
            }
        }

        if (!hasAnyTab) return null;
        const remembered = selectedConfigTabs.get(JSON.stringify(path));
        if (remembered) {
            Array.from(tabContainer.querySelectorAll<HTMLButtonElement>('button'))
                .find(button => button.dataset.tabPath === JSON.stringify([...path, remembered]))?.click();
        }

        wrap.appendChild(tabContainer);
        wrap.appendChild(contentContainer);
    } else {
        wrap.className = 'sequence-group';
        const onlyChildName = getOnlyChildName(schema);
        let hasAdded = false;

        if (onlyChildName !== null && onlyChildName === redundantTitleContext) {
            const childSchema = schema.Children[onlyChildName];
            const childVal = value ? value[onlyChildName] : undefined;
            const childField = createField(onlyChildName, childSchema, childVal, [...path, onlyChildName], {
                showLabel: false,
                redundantTitleContext: onlyChildName
            });
            if (childField) {
                wrap.appendChild(childField);
                hasAdded = true;
            }
        } else {
            hasAdded = buildProperties(schema.Children, value || {}, wrap, path);
        }

        if (!hasAdded) return null;
    }

    return wrap;
}

// -------------------------------------------------------------
// Saving & Extracting data
// -------------------------------------------------------------
function scrapeFormValues(): any | null {
    if (!currentValues) return null;
    const patchPayload = JSON.parse(JSON.stringify(currentValues));

    const container = document.getElementById('config-form');
    if (!container) return patchPayload;

    const bools = Array.from(container.querySelectorAll('.config-input-bool')) as HTMLInputElement[];
    const strings = Array.from(container.querySelectorAll('.config-input-string')) as HTMLInputElement[];
    const ints = Array.from(container.querySelectorAll('.config-input-int')) as HTMLInputElement[];
    const floats = Array.from(container.querySelectorAll('.config-input-float')) as HTMLInputElement[];
    const discretes = Array.from(container.querySelectorAll('.config-input-discrete')) as HTMLSelectElement[];
    const variants = Array.from(container.querySelectorAll('.config-input-variant')) as HTMLSelectElement[];
    const ratioNums = Array.from(container.querySelectorAll('.config-input-ratio-num')) as HTMLInputElement[];
    const ratioDens = Array.from(container.querySelectorAll('.config-input-ratio-den')) as HTMLInputElement[];

    const applyValue = (pathStr: string, val: any) => {
        const path = JSON.parse(pathStr);
        let cur = patchPayload;
        for (let i = 0; i < path.length - 1; i++) {
            if (!cur[path[i]]) cur[path[i]] = {};
            cur = cur[path[i]];
        }
        cur[path[path.length - 1]] = val;
    };

    bools.forEach(el => applyValue(el.dataset.path!, el.checked));
    strings.forEach(el => applyValue(el.dataset.path!, el.value));
    ints.forEach(el => applyValue(el.dataset.path!, parseInt(el.value, 10)));
    floats.forEach(el => applyValue(el.dataset.path!, parseFloat(el.value)));
    discretes.forEach(el => applyValue(el.dataset.path!, el.value));
    variants.forEach(el => applyValue(el.dataset.path!, el.value));
    ratioNums.forEach(el => applyValue(el.dataset.path!, parseFloat(el.value)));
    ratioDens.forEach(el => applyValue(el.dataset.path!, parseFloat(el.value)));

    return patchPayload;
}

// -------------------------------------------------------------
// Changes made elsewhere
// -------------------------------------------------------------
// The form edits a copy of the stored configuration. baseValues and baseRevision record the stored configuration
// that copy was taken from. Saving sends only the settings that differ from baseValues, and the server applies them
// only while baseRevision is still the stored revision. A save therefore never overwrites a change made elsewhere,
// such as by M500 or another browser tab, without this page first showing it to the user.
//
// The server reports a save counter in every status message. When it changes, this page fetches the stored
// configuration: without unsaved edits it simply shows the new values, otherwise it asks the user to review them.
// The review dialog's buttons act on exactly the stored configuration it lists. If more changes arrive while it is
// open, it says so instead of redrawing, and a save from it is still based on the listed revision, so the server
// rejects it and the dialog reopens with only the changes the user has not seen.
//
// Every request to config/values goes through runConfigRequest, which runs them one at a time and handles each
// response before the next request starts. A response can then never be applied over a newer one.

type ConfigChange = { path: string[]; before: any; after: any };

// A stored configuration as the server returned it. Errors is null when the response did not include them.
type StoredConfig = { values: any; revision: string; errors: any[] | null };

type ChangesDialogMode = 'save' | 'review' | 'notice';

type ShownChanges = {
    mode: ChangesDialogMode;
    // The stored configuration the dialog's buttons act on. Null for a notice, which has nothing to act on.
    stored: StoredConfig | null;
    // The stored revision the listed changes lead to. Newer changes than this have not been shown.
    revision: string;
    // Identifies each listed change, so changes that are new when the list is updated can be marked.
    keys: Set<string>;
};

let configRequestChain: Promise<unknown> = Promise.resolve();
// The newest stored configuration the server has reported. Responses are handled in order, so it is never older than
// baseValues.
let latestStored: StoredConfig | null = null;
// A stored configuration newer than baseValues that the user has unsaved edits against.
let pendingStored: StoredConfig | null = null;
// When newer values were loaded automatically because there were no unsaved edits, the base from before they were
// loaded, so the notice can list every change since. Null once the user dismisses the notice.
let noticeFrom: any | null = null;
let lastConfigSaveCount: number | null = null;
let storedCheckQueued = false;
let saving = false;
// What the open changes dialog lists, or null when it is closed.
let shownChanges: ShownChanges | null = null;

function runConfigRequest<T>(request: () => Promise<T>): Promise<T> {
    const result = configRequestChain.then(request);
    configRequestChain = result.catch(() => undefined);
    return result;
}

function cloneJSON<T>(value: T): T {
    return value === undefined ? value : JSON.parse(JSON.stringify(value));
}

function isJSONObject(value: any): value is Record<string, any> {
    return value !== null && typeof value === 'object' && !Array.isArray(value);
}

function sameJSON(a: any, b: any): boolean {
    if (a === b) return true;
    if (Array.isArray(a) || Array.isArray(b)) {
        return Array.isArray(a) && Array.isArray(b) && a.length === b.length && a.every((item, i) => sameJSON(item, b[i]));
    }
    if (!isJSONObject(a) || !isJSONObject(b)) return false;
    const keys = Object.keys(a);
    return keys.length === Object.keys(b).length
        && keys.every(key => Object.prototype.hasOwnProperty.call(b, key) && sameJSON(a[key], b[key]));
}

function valueAt(root: any, path: string[]): any {
    let value = root;
    for (const segment of path) {
        if (!isJSONObject(value)) return undefined;
        value = value[segment];
    }
    return value;
}

/** The schema of the setting at a full configuration path, or null when the path is not a setting. */
function schemaAtPath(path: string[]): any | null {
    if (path.length < 4 || path[0] !== 'Config' || path[2] !== 'Config') return null;
    let schema = currentSchema?.Config?.[path[1]]?.Config?.[path[3]];
    for (let i = 4; schema && i < path.length; i++) {
        if (schema.Kind === 'Sequence') schema = schema.Children?.[path[i]];
        else if (schema.Kind === 'Variant' && path[i] === 'Children') schema = schema.Children?.[path[++i]];
        else return null;
    }
    return schema ?? null;
}

/** Every setting that differs between two copies of the configuration. A ratio counts as one setting. */
function configChanges(before: any, after: any, path: string[] = [], changes: ConfigChange[] = []): ConfigChange[] {
    if (sameJSON(before, after)) return changes;
    if (isJSONObject(before) && isJSONObject(after) && schemaAtPath(path)?.Kind !== 'Float_Ratio') {
        for (const key of new Set([...Object.keys(before), ...Object.keys(after)])) {
            configChanges(before[key], after[key], [...path, key], changes);
        }
    } else {
        changes.push({ path, before, after });
    }
    return changes;
}

/** The smallest object that the server merges into base to get edited. Ratios are always sent whole. */
function patchBetween(base: any, edited: any, path: string[]): any {
    if (!isJSONObject(base) || !isJSONObject(edited) || schemaAtPath(path)?.Kind === 'Float_Ratio') {
        return cloneJSON(edited);
    }
    const patch: Record<string, any> = {};
    for (const [key, value] of Object.entries(edited)) {
        if (!sameJSON(base[key], value)) patch[key] = patchBetween(base[key], value, [...path, key]);
    }
    return patch;
}

/** The module entries of a patch that turns base into edited. Modules without changes are left out. */
function modulePatches(base: any, edited: any): Record<string, any> {
    const patches: Record<string, any> = {};
    for (const [module, value] of Object.entries<any>(edited?.Config ?? {})) {
        const baseModule = base?.Config?.[module];
        if (sameJSON(baseModule?.Config, value?.Config)) continue;
        patches[module] = {
            Version: baseModule?.Version ?? value.Version,
            Config: patchBetween(baseModule?.Config, value.Config, ['Config', module, 'Config'])
        };
    }
    return patches;
}

/** Merge patch into target the same way the server merges a patch into the stored configuration. */
function mergePatch(target: Record<string, any>, patch: Record<string, any>) {
    for (const [key, value] of Object.entries(patch)) {
        if (isJSONObject(target[key]) && isJSONObject(value)) mergePatch(target[key], value);
        else target[key] = cloneJSON(value);
    }
}

/** The working copy as it would be saved. Edits under variant options that are not selected are not saved. */
function workingValues(): any | null {
    const values = scrapeFormValues();
    const container = document.getElementById('config-form');
    if (!values || !container) return values;
    for (const select of Array.from(container.querySelectorAll<HTMLSelectElement>('.config-input-variant'))) {
        if (!select.dataset.path) continue;
        const variantPath: string[] = JSON.parse(select.dataset.path).slice(0, -1);
        const children = valueAt(values, [...variantPath, 'Children']);
        if (!isJSONObject(children)) continue;
        const baseChildren = valueAt(baseValues, [...variantPath, 'Children']);
        for (const option of Object.keys(children)) {
            if (option === select.value) continue;
            if (isJSONObject(baseChildren) && option in baseChildren) children[option] = cloneJSON(baseChildren[option]);
            else delete children[option];
        }
    }
    return values;
}

function unsavedChanges(): ConfigChange[] {
    const working = workingValues();
    return working && baseValues ? configChanges(baseValues, working) : [];
}

function showErrors(errors: any[]) {
    currentErrors = errors;
    handleErrors(currentErrors);
    // handleErrors clears every outline, so let the realtime validation restore the outlines for invalid inputs.
    document.getElementById('config-form')?.dispatchEvent(new Event('input', { bubbles: true }));
}

/** Make stored both the base and the working copy, discarding any unsaved edits. */
function showStoredConfig(stored: StoredConfig) {
    baseValues = stored.values;
    baseRevision = stored.revision;
    pendingStored = null;
    currentValues = cloneJSON(stored.values);
    renderConfigForm();
    showErrors(stored.errors ?? currentErrors);
}

/** Apply the unsaved edits on top of stored and make stored the new base. */
function keepEditsOn(stored: StoredConfig) {
    const working = workingValues();
    const merged = cloneJSON(stored.values);
    if (working) {
        for (const [module, patch] of Object.entries<any>(modulePatches(baseValues, working))) {
            const target = merged?.Config?.[module]?.Config;
            if (isJSONObject(target)) mergePatch(target, patch.Config);
        }
    }
    baseValues = stored.values;
    baseRevision = stored.revision;
    pendingStored = null;
    currentValues = merged;
    renderConfigForm();
    showErrors(stored.errors ?? currentErrors);
}

function noteConfigSaveCount(count: number) {
    if (count === lastConfigSaveCount) return;
    lastConfigSaveCount = count;
    void checkStoredConfig();
}

async function checkStoredConfig() {
    // One queued check is enough: it fetches whatever is stored when it runs.
    if (storedCheckQueued) return;
    storedCheckQueued = true;
    try {
        await runConfigRequest(async () => {
            storedCheckQueued = false;
            if (!baseValues) return;
            const res = await fetchConfigValues();
            latestStored = { values: res.Values, revision: res.Revision, errors: res.Errors ?? [] };
            handleStoredConfig(latestStored);
        });
    } catch (e) {
        storedCheckQueued = false;
        console.error("Failed to check the stored config", e);
    }
}

function handleStoredConfig(stored: StoredConfig) {
    if (stored.revision === baseRevision) {
        pendingStored = null;
        if (stored.errors && !sameJSON(stored.errors, currentErrors)) showErrors(stored.errors);
    } else if (pendingStored && stored.revision === pendingStored.revision) {
        if (stored.errors) pendingStored.errors = stored.errors;
    } else if (configChanges(baseValues, stored.values).length === 0) {
        // The same values under another revision, so there is nothing to show.
        baseValues = stored.values;
        baseRevision = stored.revision;
        pendingStored = null;
        if (stored.errors) showErrors(stored.errors);
    } else if (unsavedChanges().length === 0) {
        const from = noticeFrom ?? baseValues;
        showStoredConfig(stored);
        noticeFrom = configChanges(from, stored.values).length > 0 ? from : null;
    } else {
        pendingStored = stored;
    }
    updateStoredChangesUI();
}

/** After the user acted on the stored configuration a dialog listed, catch up with anything newer. */
function catchUpAfterReview(acted: StoredConfig, fetchErrors: boolean) {
    if (latestStored && latestStored.revision !== acted.revision) handleStoredConfig(latestStored);
    // A conflict reply carries no errors, so fetch the errors for the configuration now shown.
    else if (fetchErrors && !acted.errors) void checkStoredConfig();
}

function initStoredChangesUI() {
    document.getElementById('btn-config-review-changes')?.addEventListener('click', () => {
        openChangesDialog(pendingStored ? 'review' : 'notice');
    });
    document.getElementById('btn-config-dismiss-changes')?.addEventListener('click', () => {
        noticeFrom = null;
        updateStoredChangesUI();
    });
    document.getElementById('btn-config-changes-update')?.addEventListener('click', () => {
        const previous = shownChanges;
        if (!previous) return;
        if (pendingStored) {
            renderChangesDialog(previous.mode === 'save' ? 'save' : 'review', { previousKeys: previous.keys });
        } else if (noticeFrom && previous.mode === 'notice') {
            renderChangesDialog('notice', { previousKeys: previous.keys });
        } else {
            // The stored configuration changed back to this page's copy, so nothing is left to review.
            (document.getElementById('config-changes-dialog') as HTMLDialogElement | null)?.close();
            return;
        }
        document.getElementById('config-changes-body')?.focus();
    });
    const dialog = document.getElementById('config-changes-dialog') as HTMLDialogElement | null;
    dialog?.addEventListener('close', () => {
        shownChanges = null;
        updateStoredChangesUI();
    });
    onLocaleChange(() => updateStoredChangesUI());
}

function updateStoredChangesUI() {
    const banner = document.getElementById('config-stored-changes');
    const message = document.getElementById('config-stored-changes-message');
    const review = document.getElementById('btn-config-review-changes');
    const dismiss = document.getElementById('btn-config-dismiss-changes');
    if (banner && message && review && dismiss) {
        if (pendingStored) noticeFrom = null;
        banner.hidden = !pendingStored && !noticeFrom;
        banner.classList.toggle('is-notice', !pendingStored);
        message.textContent = pendingStored
            ? t('ui.config.changes.pendingBanner', 'The stored configuration was changed elsewhere after this page loaded it. Your edits are not saved.')
            : t('ui.config.changes.loadedBanner', 'The stored configuration was changed elsewhere. You had no unsaved edits, so this page now shows the new values.');
        review.textContent = pendingStored
            ? t('ui.config.changes.review', 'Review changes')
            : t('ui.config.changes.show', 'Show changes');
        dismiss.hidden = !!pendingStored;
        dismiss.textContent = t('ui.config.changes.dismiss', 'Dismiss');
    }

    // An open dialog keeps listing what its buttons act on. It only says that newer changes exist.
    const update = document.getElementById('config-changes-update');
    const updateMessage = document.getElementById('config-changes-update-message');
    const updateButton = document.getElementById('btn-config-changes-update');
    if (update && updateMessage && updateButton) {
        const newest = latestStored?.revision ?? baseRevision;
        update.hidden = !shownChanges || newest === shownChanges.revision;
        updateMessage.textContent = shownChanges?.stored
            ? t('ui.config.changes.moreWhileOpen', 'More changes were saved elsewhere while this was open, and they are not in this list. If you save, they are shown to you before anything is saved.')
            : t('ui.config.changes.moreWhileOpenNotice', 'More changes were saved elsewhere while this was open, and they are not in this list.');
        updateButton.textContent = t('ui.config.changes.showNew', 'Show the new changes');
    }
}

function formatConfigValue(path: string[], value: any): string {
    if (value === undefined) return t('ui.config.changes.notSet', '(not set)');
    if (path[path.length - 1] === 'Selected' && schemaAtPath(path.slice(0, -1))?.Kind === 'Variant') {
        return translateConfigLabel([...path.slice(0, -1), 'Children', String(value)], String(value));
    }
    const schema = schemaAtPath(path);
    switch (schema?.Kind) {
        case 'Boolean':
            return value ? t('ui.config.changes.on', 'On') : t('ui.config.changes.off', 'Off');
        case 'Integer':
        case 'Float':
            return schema.Unit ? `${value} ${translateConfigUnit(path, schema.Unit)}` : String(value);
        case 'Discrete':
            return translateConfigOption(path, String(value), String(value));
        case 'Float_Ratio':
            return `${value?.Numerator} : ${value?.Denominator}`;
        case 'String':
            return value === '' ? t('ui.config.changes.empty', '(empty)') : String(value);
    }
    return isJSONObject(value) || Array.isArray(value) ? JSON.stringify(value) : String(value);
}

function pathsOverlap(a: string[], b: string[]): boolean {
    const length = Math.min(a.length, b.length);
    for (let i = 0; i < length; i++) if (a[i] !== b[i]) return false;
    return true;
}

type ChangeNote = { text: string; kind: 'conflict' | 'related' };

function changeKey(change: ConfigChange): string {
    return `${JSON.stringify(change.path)}=${JSON.stringify(change.after)}`;
}

function changeListItem(change: ConfigChange, note: ChangeNote | null, isNew = false): HTMLLIElement {
    const item = document.createElement('li');
    item.className = 'config-change';
    const setting = document.createElement('span');
    setting.className = 'config-change-setting';
    if (isNew) {
        item.classList.add('is-new');
        const tag = document.createElement('span');
        tag.className = 'config-change-tag';
        tag.textContent = t('ui.config.changes.newTag', 'New');
        setting.appendChild(tag);
    }
    setting.append(configBreadcrumb(change.path));
    const values = document.createElement('span');
    values.className = 'config-change-values';
    const before = document.createElement('span');
    before.className = 'config-change-before';
    before.textContent = formatConfigValue(change.path, change.before);
    const arrow = document.createElement('span');
    arrow.setAttribute('aria-hidden', 'true');
    arrow.textContent = ' → ';
    const spoken = document.createElement('span');
    spoken.className = 'sr-only';
    spoken.textContent = ` ${t('ui.config.changes.changedTo', 'changed to')} `;
    const after = document.createElement('span');
    after.className = 'config-change-after';
    after.textContent = formatConfigValue(change.path, change.after);
    values.append(before, arrow, spoken, after);
    item.append(setting, values);
    if (note) {
        item.classList.add(note.kind === 'conflict' ? 'is-conflict' : 'is-related');
        const noteElement = document.createElement('span');
        noteElement.className = 'config-change-note';
        noteElement.textContent = note.text;
        item.appendChild(noteElement);
    }
    return item;
}

function changeSection(title: string, items: HTMLLIElement[]): HTMLElement {
    const section = document.createElement('section');
    const heading = document.createElement('h4');
    heading.textContent = `${title} (${items.length})`;
    const list = document.createElement('ul');
    list.className = 'config-change-list';
    list.append(...items);
    section.append(heading, list);
    return section;
}

type ChangesDialogOptions = {
    // The save was made from this dialog and met still more changes.
    again?: boolean;
    // The changes listed before the list was updated, so the others can be marked as new.
    previousKeys?: Set<string>;
};

function openChangesDialog(mode: ChangesDialogMode, options: ChangesDialogOptions = {}) {
    const dialog = document.getElementById('config-changes-dialog') as HTMLDialogElement | null;
    if (!dialog) return;
    renderChangesDialog(mode, options);
    if (!dialog.open) dialog.showModal();
    // Start on the list rather than a button, so Enter cannot act on changes before they are read.
    document.getElementById('config-changes-body')?.focus();
}

function renderChangesDialog(mode: ChangesDialogMode, options: ChangesDialogOptions = {}) {
    const title = document.getElementById('config-changes-title');
    const body = document.getElementById('config-changes-body');
    const actions = document.getElementById('config-changes-actions');
    const dialog = document.getElementById('config-changes-dialog') as HTMLDialogElement | null;
    if (!title || !body || !actions || !dialog) return;

    const stored = mode === 'notice' ? null : pendingStored;
    if (mode === 'notice' ? !noticeFrom : !stored) return;

    const elsewhere = stored ? configChanges(baseValues, stored.values) : configChanges(noticeFrom, baseValues);
    const edits = stored ? unsavedChanges() : [];
    shownChanges = { mode, stored, revision: stored ? stored.revision : baseRevision, keys: new Set(elsewhere.map(changeKey)) };

    const intro = document.createElement('p');
    switch (mode) {
        case 'save':
            title.textContent = options.again
                ? t('ui.config.changes.saveAgainTitle', 'Not saved: more changes elsewhere')
                : t('ui.config.changes.saveTitle', 'Not saved: configuration changed elsewhere');
            intro.textContent = options.again
                ? t('ui.config.changes.saveAgainIntro',
                    'More changes were saved elsewhere while you were reviewing, so nothing was saved. Only the changes you have not seen yet are listed. Your edits already include the changes you kept.')
                : t('ui.config.changes.saveIntro',
                    'The stored configuration changed after this page loaded it, so nothing was saved. Settings are also saved by G-code such as M500 and by other browser tabs. Keep your edits on top of the changes below, or discard your edits.');
            break;
        case 'review':
            title.textContent = t('ui.config.changes.reviewTitle', 'Configuration changed elsewhere');
            intro.textContent = t('ui.config.changes.reviewIntro',
                'The stored configuration changed after this page loaded it, and your edits are not saved. Keep your edits on top of the changes below, or discard your edits.');
            break;
        case 'notice':
            title.textContent = t('ui.config.changes.noticeTitle', 'Configuration changed elsewhere');
            intro.textContent = t('ui.config.changes.noticeIntro',
                'The stored configuration changed after this page loaded it. You had no unsaved edits, so this page now shows these values.');
            break;
    }

    const editedModules = new Set(edits.map(edit => edit.path[1]));
    let overlapCount = 0;
    const elsewhereItems = elsewhere.map(change => {
        const isNew = !!options.previousKeys && !options.previousKeys.has(changeKey(change));
        const overlapping = edits.filter(edit => pathsOverlap(edit.path, change.path));
        if (overlapping.length > 0) {
            if (overlapping.every(edit => sameJSON(edit.after, valueAt(stored!.values, edit.path)))) {
                return changeListItem(change, { kind: 'related', text: t('ui.config.changes.sameEdit', 'You made the same edit.') }, isNew);
            }
            overlapCount++;
            return changeListItem(change, {
                kind: 'conflict',
                text: t('ui.config.changes.conflict', 'You also edited this, to {value}. Keeping your edits replaces the stored value.', {
                    value: overlapping.map(edit => formatConfigValue(edit.path, edit.after)).join(', ')
                })
            }, isNew);
        }
        if (change.path[0] === 'Config' && editedModules.has(change.path[1])) {
            return changeListItem(change, {
                kind: 'related',
                text: t('ui.config.changes.related', 'You edited other settings in {module}. Check that they still fit with this change.', {
                    module: translateConfigLabel(change.path.slice(0, 2), change.path[1])
                })
            }, isNew);
        }
        return changeListItem(change, null, isNew);
    });
    const editItems = edits.map(edit => {
        const overlapping = elsewhere.some(change => pathsOverlap(edit.path, change.path));
        const storedValue = valueAt(stored!.values, edit.path);
        return changeListItem(edit, overlapping && !sameJSON(edit.after, storedValue)
            ? { kind: 'conflict', text: t('ui.config.changes.conflictEdit', 'Also changed elsewhere, to {value}.', { value: formatConfigValue(edit.path, storedValue) }) }
            : null);
    });

    body.replaceChildren(intro);
    if (elsewhere.length > 0 && edits.length > 0) {
        // Settings that differ can still depend on each other in ways the schema does not show, so always warn.
        const warning = document.createElement('div');
        warning.className = 'config-changes-warning';
        const overlap = document.createElement('p');
        overlap.textContent = overlapCount > 0
            ? t('ui.config.changes.overlapCount', 'Settings edited both here and elsewhere: {count}. Keeping your edits replaces their stored values.', { count: overlapCount })
            : t('ui.config.changes.noOverlap', 'None of these settings were also edited here.');
        const interaction = document.createElement('p');
        interaction.textContent = t('ui.config.changes.interaction',
            "Changes to different settings can still conflict. Check that your edits still make sense with these changes. Keep my edits puts them on top of these changes in the form without saving, so you can check the result first.");
        warning.append(overlap, interaction);
        body.appendChild(warning);
    }
    body.appendChild(changeSection(t('ui.config.changes.elsewhere', 'Changed elsewhere'), elsewhereItems));
    if (stored) body.appendChild(changeSection(t('ui.config.changes.yourEdits', 'Your unsaved edits'), editItems));
    body.scrollTop = 0;

    const button = (label: string, className: string, onClick: () => void) => {
        const element = document.createElement('button');
        element.type = 'button';
        element.className = `btn ${className}`;
        element.textContent = label;
        element.addEventListener('click', onClick);
        return element;
    };
    const close = () => dialog.close();
    if (!stored) {
        actions.replaceChildren(button(t('ui.config.changes.close', 'Close'), 'btn-primary', () => {
            // Keep the notice if newer changes arrived that this list did not show.
            if ((latestStored?.revision ?? baseRevision) === shownChanges?.revision) noticeFrom = null;
            close();
        }));
    } else {
        const discard = button(t('ui.config.changes.discard', 'Discard my edits'), 'btn-danger', () => {
            showStoredConfig(stored);
            close();
            catchUpAfterReview(stored, true);
        });
        const keep = button(t('ui.config.changes.keep', 'Keep my edits'), mode === 'save' ? 'btn-secondary' : 'btn-primary', () => {
            keepEditsOn(stored);
            close();
            catchUpAfterReview(stored, true);
        });
        const cancel = button(mode === 'save' ? t('ui.config.changes.cancel', 'Cancel') : t('ui.config.changes.close', 'Close'), 'btn-secondary', close);
        if (mode === 'save') {
            actions.replaceChildren(cancel, discard, keep, button(t('ui.config.changes.keepAndSave', 'Keep my edits and save'), 'btn-primary', () => {
                keepEditsOn(stored);
                close();
                catchUpAfterReview(stored, false);
                // Based on the revision just reviewed, so the server refuses it if anything newer was stored.
                void saveConfiguration({ afterReview: true });
            }));
        } else {
            actions.replaceChildren(cancel, discard, keep);
        }
    }
    updateStoredChangesUI();
}

function setSaveBusy(busy: boolean) {
    const button = document.getElementById('btn-save-config') as HTMLButtonElement | null;
    if (!button) return;
    button.disabled = busy;
    button.setAttribute('aria-busy', String(busy));
}

async function saveConfiguration(options: { afterReview?: boolean } = {}) {
    if (!currentSchema || !baseValues || saving) return;

    const container = document.getElementById('config-form');
    if (container) {
        const inputs = Array.from(container.querySelectorAll('input, select')) as (HTMLInputElement | HTMLSelectElement)[];
        for (const input of inputs) {
            // Check native HTML5 validation
            if (configInputInvalid(input)) {
                revealConfigElement(input.closest<HTMLElement>('.form-group') || input);
                input.focus();
                input.reportValidity();
                alert(t('ui.config.validationFailed', 'Validation failed. Please correct the highlighted fields.'));
                return;
            }
        }
    }

    saving = true;
    setSaveBusy(true);
    try {
        await runConfigRequest(async () => {
            // Read the form inside the request queue, so the base it is compared with cannot change before sending.
            const working = workingValues();
            const patches = working ? modulePatches(baseValues, working) : {};
            if (!working || Object.keys(patches).length === 0) {
                alert(t('ui.config.noChanges', 'There are no unsaved edits.'));
                return;
            }

            const sentRevision = baseRevision;
            const response = await patchConfigValues({
                'Prunt config version': baseValues['Prunt config version'],
                'Base revision': sentRevision,
                Config: patches
            });

            if (response.Conflict) {
                latestStored = { values: response.Values, revision: response.Revision, errors: null };
                pendingStored = latestStored;
                noticeFrom = null;
                openChangesDialog('save', { again: options.afterReview });
                return;
            }

            // The response is the stored configuration immediately after this patch, so it becomes the base. Edits
            // made while the request was in flight stay in the form and remain unsaved.
            const applied = response.Revision !== sentRevision;
            latestStored = { values: response.Values, revision: response.Revision, errors: response.Errors ?? [] };
            baseValues = response.Values;
            baseRevision = response.Revision;
            pendingStored = null;
            noticeFrom = null;
            currentValues = working;
            showErrors(response.Errors ?? []);
            updateStoredChangesUI();

            if (!applied) {
                alert(t('ui.config.notSaved', 'The configuration was not saved. Please correct the highlighted errors.'));
            } else if (currentErrors.length > 0) {
                alert(t('ui.config.savedWithErrors', 'Saved with errors. Please check the notifications.'));
            } else {
                alert(t('ui.config.saved', 'Configuration saved successfully!'));
            }
        });
    } catch (e) {
        console.error("Save failed", e);
        alert(t('ui.config.saveFailed', 'Failed to save configuration. Network error.'));
    } finally {
        saving = false;
        setSaveBusy(false);
    }
}

function handleErrors(errors: any[]) {
    const globalErrors = document.getElementById('config-global-errors');
    globalErrors?.replaceChildren();
    globalErrors?.classList.remove('active', 'server-error');
    document.querySelectorAll<HTMLElement>('#config-form .error-message').forEach(span => {
        span.textContent = '';
        span.classList.add('d-none');
        span.classList.remove('d-block', 'server-error');
    });
    let hasGlobal = false;

    const configNavItem = document.querySelector('.config-nav-item');

    configNavItem?.classList.remove('has-error', 'has-server-error');
    if (errors && errors.length > 0) {
        configNavItem?.classList.add('has-server-error');
    }

    document.querySelectorAll('.config-group').forEach(el => {
        el.classList.remove('has-error', 'has-server-error');
    });
    document.querySelectorAll('.config-tab').forEach(el => {
        el.classList.remove('has-error', 'has-server-error');
    });

    const isPrefix = (prefix: string[], full: string[]) => {
        if (prefix.length > full.length) return false;
        for (let i = 0; i < prefix.length; i++) {
            if (prefix[i] !== full[i]) return false;
        }
        return true;
    };

    errors.forEach(err => {
        let currentPath = [...err.Path];
        let errSpan: HTMLElement | null = null;

        // Find the most specific error span available by walking up the path
        while (currentPath.length > 0) {
            errSpan = document.getElementById(`err-${currentPath.join('-')}`);
            if (errSpan) break;
            currentPath.pop();
        }

        if (errSpan) {
            errSpan.classList.add('server-error');
            const msgWithNote = `${err.Message} (${t('ui.config.serverErrorSuffix', 'Will only be rechecked after saving')})`;
            if (!errSpan.classList.contains('d-none')) {
                errSpan.innerText += '\n' + msgWithNote;
            } else {
                errSpan.innerText = msgWithNote;
                errSpan.classList.remove('d-none');
                errSpan.classList.add('d-block');
            }
        } else if (globalErrors) {
            hasGlobal = true;
            const p = document.createElement('p');
            p.innerText = `${err.Path.join(' -> ')}: ${err.Message} (${t('ui.config.serverErrorSuffix', 'Will only be rechecked after saving')})`;
            globalErrors.appendChild(p);
        }

        // Add outlines to groups and tabs
        document.querySelectorAll('.config-group').forEach(el => {
            const elPathStr = (el as HTMLElement).dataset.groupPath;
            if (elPathStr) {
                const elPath = JSON.parse(elPathStr);
                if (elPath.length === 0 || (isPrefix(elPath, err.Path) && groupContainsError(el as HTMLElement, err.Path))) {
                    el.classList.add('has-server-error');
                }
            }
        });
        document.querySelectorAll('.config-tab').forEach(el => {
            const elPathStr = (el as HTMLElement).dataset.tabPath;
            if (elPathStr && elPathStr !== 'null') {
                const elPath = JSON.parse(elPathStr);
                if (isPrefix(elPath, err.Path)) {
                    el.classList.add('has-server-error');
                }
            }
        });
    });

    if (hasGlobal && globalErrors) {
        globalErrors.classList.add('active', 'server-error');
    }
    updateCategoryErrorIndicators();
}
