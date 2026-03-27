import uPlot from './uPlot.esm.js';
import { fetchStatusSchema, fetchStatusValues } from './api.js';
import { wsClient } from './ws.js';
import {
    onLocaleChange,
    t,
    translateSchemaKind,
    translateStatusGroupLabel,
    translateStatusModuleLabel,
    translateStatusUnit,
    translateStatusValueLabel
} from './localization.js';

export interface StatusPath {
    module: string;
    group: string;
    value: string;
}

let statusSchema: any = null;
let currentStatus: any = null;

let dashboardLayout: any[] = [];
let plots: Record<string, { plot: uPlot, paths: StatusPath[], data: number[][] }> = {};

export async function initStatusView() {
    const btnAddWidget = document.getElementById('btn-add-widget');
    btnAddWidget?.addEventListener('click', openAddWidgetModal);

    const btnAddHeader = document.getElementById('btn-add-header');
    btnAddHeader?.addEventListener('click', () => {
        const title = prompt(
            t('ui.status.newHeaderPrompt', 'Enter text for new header:'),
            t('ui.status.newHeaderDefault', 'New Area')
        );
        if (title) {
            dashboardLayout.push({ type: 'header', value: title });
            saveDashboardLayout();
            renderDashboard();
        }
    });

    const btnReset = document.getElementById('btn-reset-widgets');
    btnReset?.addEventListener('click', () => resetWidgetsToDefault(true));

    const btnCloseModal = document.getElementById('btn-close-modal');
    btnCloseModal?.addEventListener('click', () => {
        const modal = document.getElementById('add-widget-modal') as HTMLDialogElement;
        modal?.close();
    });

    const btnApply = document.getElementById('btn-apply-widgets');
    btnApply?.addEventListener('click', applyWidgetSelection);

    setupDragAndDrop();
    onLocaleChange(() => {
        if (statusSchema) {
            renderDashboard();
        }
    });

    try {
        statusSchema = await fetchStatusSchema();
        currentStatus = await fetchStatusValues();

        loadDashboardLayout();

        wsClient.on('tick', (msg: any) => {
            if (msg.Status_Values) {
                currentStatus = msg.Status_Values;
                updateWidgets();
            }
        });

    } catch (e) {
        console.error("Failed to init status view", e);
    }
}

function openAddWidgetModal() {
    if (!statusSchema) return;

    const modal = document.getElementById('add-widget-modal') as HTMLDialogElement;
    const listContainer = document.getElementById('widget-selection-list');
    if (!modal || !listContainer) return;

    listContainer.innerHTML = '';

    const activePaths = new Set<string>();
    dashboardLayout.forEach(item => {
        if (item.type === 'widget-group' || item.type === 'widget') {
            item.paths.forEach((p: StatusPath) => activePaths.add(`${p.module}/${p.group}/${p.value}`));
        }
    });

    for (const [modName, modSchema] of Object.entries(statusSchema)) {
        const groupEl = document.createElement('div');
        groupEl.className = 'module-group';
        const title = document.createElement('h4');
        title.innerText = translateStatusModuleLabel(modName, modName);
        groupEl.appendChild(title);

        let hasItems = false;
        for (const [groupName, groupSchema] of Object.entries(modSchema as any)) {
            for (const [valName, valSchema] of Object.entries(groupSchema as any)) {
                hasItems = true;
                hasItems = true;
                const pathStr = `${modName}/${groupName}/${valName}`;

                const label = document.createElement('label');
                const checkbox = document.createElement('input');
                checkbox.type = 'checkbox';
                // Store serialized object in value so we can easily parse it back
                checkbox.value = JSON.stringify({ module: modName, group: groupName, value: valName });
                checkbox.checked = activePaths.has(pathStr);

                label.appendChild(checkbox);
                label.appendChild(document.createTextNode(
                    ` ${t('ui.status.widgetSelectionLabel', '{group} - {value} ({kind})', {
                        group: translateStatusGroupLabel(modName, groupName, groupName),
                        value: translateStatusValueLabel(modName, groupName, valName, valName),
                        kind: translateSchemaKind((valSchema as any).Kind)
                    })}`
                ));
                groupEl.appendChild(label);
            }
        }

        if (hasItems) listContainer.appendChild(groupEl);
    }

    modal.showModal();
}

function applyWidgetSelection() {
    const modal = document.getElementById('add-widget-modal') as HTMLDialogElement;
    const checkboxes = document.querySelectorAll('#widget-selection-list input[type="checkbox"]:checked');
    const selectedPathJsonVars = new Set(Array.from(checkboxes).map(cb => (cb as HTMLInputElement).value));

    const newLayout: any[] = [];

    // Filter existing widgets to only keep selected paths
    for (const item of dashboardLayout) {
        if (item.type === 'header') {
            newLayout.push(item);
        } else if (item.type === 'widget' || item.type === 'widget-group') {
            const keptPaths = item.paths.filter((p: StatusPath) => selectedPathJsonVars.has(JSON.stringify(p)));
            if (keptPaths.length > 0) {
                newLayout.push({ ...item, type: 'widget', paths: keptPaths });
            }
            // Remove handled paths from set
            keptPaths.forEach((p: StatusPath) => selectedPathJsonVars.delete(JSON.stringify(p)));
        }
    }

    // Any remaining selected paths mean they were newly checked
    // Add them as standalone widgets at the end
    for (const jsonStr of selectedPathJsonVars) {
        const p: StatusPath = JSON.parse(jsonStr);
        const { module: mod, group: grp, value: val } = p;
        const schema = statusSchema[mod]?.[grp]?.[val];
        if (schema) {
            let groupKey = `${mod}/${grp}`;
            if (schema.Kind === 'Real') {
                groupKey += `/Real/${schema.Unit || ''}`;
            } else {
                groupKey += `/Text`;
            }
            newLayout.push({ type: 'widget', key: groupKey, groupKey, paths: [p] });
        }
    }

    dashboardLayout = newLayout;
    saveDashboardLayout();
    renderDashboard();
    modal.close();
}

function resetWidgetsToDefault(askConfirm = false) {
    if (askConfirm && !confirm(t('ui.status.resetConfirm', 'Are you sure you want to group all status values into module headers? This will clear your custom layout.'))) return;

    dashboardLayout = [];
    if (!statusSchema) return;

    for (const [modName, modSchema] of Object.entries(statusSchema)) {
        const groups = new Map<string, StatusPath[]>();
        for (const [groupName, groupSchema] of Object.entries(modSchema as any)) {
            for (const [valName, valSchema] of Object.entries(groupSchema as any)) {
                const pathObj: StatusPath = { module: modName, group: groupName, value: valName };

                let key = `${modName}/${groupName}`;
                if ((valSchema as any).Kind === 'Real') {
                    key += `/Real/${(valSchema as any).Unit || ''}`;
                } else {
                    key += `/Text`;
                }

                if (!groups.has(key)) groups.set(key, []);
                groups.get(key)!.push(pathObj);
            }
        }

        if (groups.size === 0) continue;

        dashboardLayout.push({ type: 'header', value: modName, module: modName, translatable: true });
        for (const [key, paths] of groups.entries()) {
            dashboardLayout.push({ type: 'widget', groupKey: key, key, paths });
        }
    }

    saveDashboardLayout();
    renderDashboard();
}

function renderDashboard() {
    const grid = document.getElementById('dashboard-grid');
    if (!grid) return;

    Object.values(plots).forEach(p => p.plot.destroy());
    plots = {};
    grid.innerHTML = '';

    dashboardLayout.forEach((item, index) => {
        if (item.type === 'header') {
            const header = document.createElement('div');
            header.className = 'dashboard-group-header';
            header.dataset.index = String(index);
            header.draggable = true;
            header.addEventListener('dragstart', handleDragStart);
            header.addEventListener('dragend', handleDragEnd);

            header.classList.add('status-header-flex');

            const titleSpan = document.createElement('span');
            titleSpan.innerText = item.translatable && item.module
                ? translateStatusModuleLabel(item.module, item.value)
                : item.value;
            header.appendChild(titleSpan);

            const delBtn = document.createElement('button');
            delBtn.innerText = 'x';
            delBtn.className = 'btn btn-sm btn-secondary';
            delBtn.title = t('ui.status.removeHeader', 'Remove header');
            delBtn.onclick = () => {
                dashboardLayout.splice(index, 1);
                saveDashboardLayout();
                renderDashboard();
            };
            header.appendChild(delBtn);

            grid.appendChild(header);
        } else if (item.type === 'widget' || item.type === 'widget-group') {
            const actualGroupKey = item.groupKey || item.key;
            if (!actualGroupKey) return; // Should not happen

            const [mod, grp, kind, unit] = actualGroupKey.split('/');

            const card = document.createElement('div');
            card.className = 'card widget-card';
            card.dataset.index = String(index);
            card.dataset.groupkey = actualGroupKey;
            card.draggable = true;
            card.addEventListener('dragstart', handleDragStart);
            card.addEventListener('dragend', handleDragEnd);

            const header = document.createElement('h3');
            const firstPath = item.paths[0] as StatusPath | undefined;
            header.innerText = `${translateStatusModuleLabel(mod, mod)} - ${translateStatusGroupLabel(mod, grp, grp)}`;
            if (unit && firstPath) {
                header.innerText += ` (${translateStatusUnit(firstPath.module, firstPath.group, firstPath.value, unit)})`;
            }

            header.className = 'status-widget-header';

            const titleSpan = document.createElement('span');
            titleSpan.innerText = header.innerText;
            titleSpan.className = 'flex-1';

            header.innerHTML = '';
            header.appendChild(titleSpan);

            // Ungroup button if multiple paths
            if (item.paths.length > 1) {
                const ungroupBtn = document.createElement('button');
                ungroupBtn.innerText = t('ui.status.ungroup', 'Ungroup');
                ungroupBtn.className = 'btn btn-sm btn-secondary';
                ungroupBtn.title = t('ui.status.ungroupTitle', 'Split into individual widgets');
                ungroupBtn.onclick = () => {
                    const newItems = item.paths.map((p: StatusPath) => ({ type: 'widget', groupKey: actualGroupKey, paths: [p] }));
                    dashboardLayout.splice(index, 1, ...newItems);
                    saveDashboardLayout();
                    renderDashboard();
                };
                header.appendChild(ungroupBtn);
            }

            const delBtn = document.createElement('button');
            delBtn.innerText = 'x';
            delBtn.className = 'btn btn-sm btn-secondary';
            delBtn.title = t('ui.status.removeWidget', 'Remove widget');
            delBtn.onclick = () => {
                dashboardLayout.splice(index, 1);
                saveDashboardLayout();
                renderDashboard();
            };
            header.appendChild(delBtn);
            card.appendChild(header);

            const contentDiv = document.createElement('div');
            contentDiv.className = 'widget-content';
            card.appendChild(contentDiv);

            if (kind === 'Real') {
                const seriesOpts: any[] = [{}];
                const plotData: number[][] = [[]];
                const colors = ["#58a6ff", "#2ea043", "#d29922", "#f85149", "#a371f7", "#00bcd4"];

                const legendContainer = document.createElement('div');
                legendContainer.className = 'widget-legend mt-8';

                item.paths.forEach((path: StatusPath, i: number) => {
                    const c = colors[i % colors.length];
                    const valName = translateStatusValueLabel(path.module, path.group, path.value, path.value);
                    seriesOpts.push({
                        label: valName,
                        stroke: c,
                        width: 2
                    });
                    plotData.push([]);

                    const textVal = document.createElement('div');
                    textVal.className = 'widget-text-val';
                    textVal.dataset.path = JSON.stringify(path);
                    textVal.style.color = c;
                    textVal.classList.add('status-val-text');
                    legendContainer.appendChild(textVal);
                });

                const opts = {
                    width: 280,
                    height: 150,
                    series: seriesOpts,
                    axes: [
                        { show: false },
                        { stroke: "var(--text-main)", grid: { show: false } }
                    ]
                };
                const u = new uPlot(opts, plotData as any, contentDiv);
                const uKey = actualGroupKey + "_" + index; // Ensure unique plot instance keys if duplicate groups exist
                plots[uKey] = { plot: u, paths: item.paths, data: plotData };

                card.appendChild(legendContainer);
            } else {
                item.paths.forEach((path: StatusPath) => {
                    const textVal = document.createElement('div');
                    textVal.className = 'widget-text-val';
                    textVal.dataset.path = JSON.stringify(path);
                    textVal.classList.add('status-val-large');
                    contentDiv.appendChild(textVal);
                });
            }

            grid.appendChild(card);
        }
    });

    updateWidgets();
}

function updateWidgets() {
    if (!currentStatus) return;
    const now = Date.now() / 1000;

    for (const pd of Object.values(plots)) {
        pd.data[0].push(now);
        pd.paths.forEach((p, idx) => {
            const v = currentStatus[p.module]?.[p.group]?.[p.value];
            pd.data[idx + 1].push(v !== undefined ? Number(v) : NaN); // Or 0
        });

        if (pd.data[0].length > 100) {
            pd.data.forEach(arr => arr.shift());
        }
        pd.plot.setData(pd.data as any);
    }

    document.querySelectorAll('.widget-text-val').forEach(el => {
        const p: StatusPath = JSON.parse((el as HTMLElement).dataset.path!);
        const v = currentStatus[p.module]?.[p.group]?.[p.value];
        const schema = statusSchema[p.module]?.[p.group]?.[p.value];
        const valueLabel = translateStatusValueLabel(p.module, p.group, p.value, p.value);

        let displayStr = `${valueLabel}: ` + (v !== undefined ? String(v) : t('ui.common.notAvailable', 'N/A'));
        if (schema?.Unit && v !== undefined) {
            displayStr += ` ${translateStatusUnit(p.module, p.group, p.value, schema.Unit)}`;
        }

        el.innerHTML = displayStr;
    });
}

function setupDragAndDrop() {
    const grid = document.getElementById('dashboard-grid');
    if (!grid) return;

    grid.addEventListener('dragenter', (e) => {
        e.preventDefault();
    });

    grid.addEventListener('dragover', (e) => {
        e.preventDefault();
        const dragging = document.querySelector('.dragging') as HTMLElement;
        const target = (e.target as HTMLElement)?.closest('.widget-card, .dashboard-group-header') as HTMLElement;

        if (!dragging) return;

        if (!target || dragging === target) {
            document.querySelectorAll('.merge-target').forEach(el => el.classList.remove('merge-target'));
            return;
        }

        document.querySelectorAll('.merge-target').forEach(el => {
            if (el !== target) el.classList.remove('merge-target');
        });

        const rect = target.getBoundingClientRect();
        const relX = e.clientX - rect.left;

        let isCompatible = false;
        if (target.classList.contains('widget-card') && dragging.classList.contains('widget-card')) {
            const targetKey = target.dataset.groupkey;
            const dragKey = dragging.dataset.groupkey;
            if (targetKey && dragKey && targetKey !== 'undefined') {
                const [tMod, tGrp, tKind, tUnit] = targetKey.split('/');
                const [dMod, dGrp, dKind, dUnit] = dragKey.split('/');

                // They must be from the same Module & Group
                if (tMod === dMod && tGrp === dGrp) {
                    if (tKind === 'Real' && dKind === 'Real') {
                        // For plots, units must match exactly
                        isCompatible = (tUnit === dUnit);
                    } else if (tKind !== 'Real' && dKind !== 'Real') {
                        // For everything else, types don't matter
                        isCompatible = true;
                    }
                }
            }
        }

        if (isCompatible) {
            if (relX >= rect.width * 0.25 && relX <= rect.width * 0.75) {
                target.classList.add('merge-target');
                e.dataTransfer!.dropEffect = 'copy';
                return;
            }
        }

        target.classList.remove('merge-target');
        e.dataTransfer!.dropEffect = 'move';

        const all = Array.from(grid.children);
        const draggingIdx = all.indexOf(dragging);
        const targetIdx = all.indexOf(target);

        const sortThresholdRight = isCompatible ? 0.75 : 0.5;
        const sortThresholdLeft = isCompatible ? 0.25 : 0.5;

        const isTargetHeader = target.classList.contains('dashboard-group-header');
        const isDraggingHeader = dragging.classList.contains('dashboard-group-header');

        if (isDraggingHeader) {
            const relY = e.clientY - rect.top;
            if (isTargetHeader) {
                if (draggingIdx < targetIdx && relY > rect.height * 0.5) {
                    target.after(dragging);
                } else if (draggingIdx > targetIdx && relY < rect.height * 0.5) {
                    target.before(dragging);
                }
            } else {
                const isLastInGroup = targetIdx === all.length - 1 || all[targetIdx + 1].classList.contains('dashboard-group-header');
                const isFirstInGroup = targetIdx === 0 || all[targetIdx - 1].classList.contains('dashboard-group-header');

                if (isLastInGroup && draggingIdx < targetIdx && relY > rect.height * 0.5) {
                    target.after(dragging);
                } else if (isFirstInGroup && draggingIdx > targetIdx && relY < rect.height * 0.5) {
                    target.before(dragging);
                }
            }
        } else if (isTargetHeader) {
            const relY = e.clientY - rect.top;
            if (draggingIdx < targetIdx && relY > rect.height * 0.5) {
                target.after(dragging);
            } else if (draggingIdx > targetIdx && relY < rect.height * 0.5) {
                target.before(dragging);
            }
        } else {
            if (draggingIdx < targetIdx && relX > rect.width * sortThresholdRight) {
                target.after(dragging);
            } else if (draggingIdx > targetIdx && relX < rect.width * sortThresholdLeft) {
                target.before(dragging);
            }
        }
    });

    grid.addEventListener('drop', (e) => {
        e.preventDefault();
        const target = grid.querySelector('.merge-target') as HTMLElement;
        if (target) {
            target.dataset.mergedrop = 'true';
        }
    });
}

function handleDragStart(e: DragEvent) {
    const target = e.currentTarget as HTMLElement;
    target.classList.add('dragging');
    if (e.dataTransfer) e.dataTransfer.setData('text/plain', target.dataset.index!);
}

function handleDragEnd(e: DragEvent) {
    const target = e.currentTarget as HTMLElement;
    target.classList.remove('dragging');

    const grid = document.getElementById('dashboard-grid');
    if (!grid) return;

    const mergeTargetEl = grid.querySelector('.merge-target') as HTMLElement;
    const isMergeDrop = mergeTargetEl && mergeTargetEl.dataset.mergedrop === 'true';

    // Clear UI state
    document.querySelectorAll('.merge-target').forEach(el => {
        el.classList.remove('merge-target');
        delete (el as HTMLElement).dataset.mergedrop;
    });

    if (isMergeDrop) {
        const dragIdx = parseInt(target.dataset.index!);
        const targetIdx = parseInt(mergeTargetEl.dataset.index!);

        if (dragIdx !== targetIdx && !isNaN(dragIdx) && !isNaN(targetIdx)) {
            const dragItem = dashboardLayout[dragIdx];
            const targetItem = dashboardLayout[targetIdx];

            if (dragItem && targetItem) {
                const mergedPaths = new Set([...targetItem.paths, ...dragItem.paths]);
                targetItem.paths = Array.from(mergedPaths);
                dashboardLayout.splice(dragIdx, 1);

                saveDashboardLayout();
                renderDashboard();
                return;
            }
        }
    }

    const newLayout: any[] = [];
    Array.from(grid.children).forEach(child => {
        const idx = (child as HTMLElement).dataset.index;
        if (idx !== undefined) {
            newLayout.push(dashboardLayout[parseInt(idx, 10)]);
        }
    });

    if (newLayout.length === dashboardLayout.length) {
        dashboardLayout = newLayout;
        saveDashboardLayout();
    }
    renderDashboard();
}

function saveDashboardLayout() {
    localStorage.setItem('prunt-dashboard-layout-v4', JSON.stringify(dashboardLayout));
}

function loadDashboardLayout() {
    const saved = localStorage.getItem('prunt-dashboard-layout-v4');
    if (saved) {
        dashboardLayout = JSON.parse(saved);
        renderDashboard();
        return;
    }

    // Fallback cleanup
    localStorage.removeItem('prunt-dashboard-layout-v3');
    localStorage.removeItem('prunt-dashboard-layout-v2');
    localStorage.removeItem('prunt-dashboard-layout');

    resetWidgetsToDefault(false);
}
