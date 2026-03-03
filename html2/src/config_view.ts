import { fetchConfigSchema, fetchConfigValues, patchConfigValues } from './api';

let currentSchema: any = null;
let currentValues: any = null;
let currentErrors: any[] = [];

let groupByModule = false;

export async function initConfigView() {
    const btnSave = document.getElementById('btn-save-config');
    btnSave?.addEventListener('click', saveConfiguration);

    const updateRealtimeFrontendErrors = () => {
        const container = document.getElementById('config-form');
        if (!container) return;
        const inputs = Array.from(container.querySelectorAll('input, select')) as (HTMLInputElement | HTMLSelectElement)[];
        const frontendErrors: any[] = [];
        for (const input of inputs) {
            // Need to manually check valid state for our custom ranges since we aren't using setCustomValidity
            let isInvalid = !input.checkValidity();

            // For ratio, check range hint color which we use for invalid state
            const rangeHint = input.parentElement?.parentElement?.querySelector('.range-hint') as HTMLElement;
            if (rangeHint && rangeHint.style.color !== '') {
                isInvalid = true;
            }
            // For numbers
            const numRangeHint = (input.parentElement?.querySelector('.range-hint') || input.parentElement?.parentElement?.querySelector('.range-hint')) as HTMLElement;
            if (numRangeHint && numRangeHint.style.color !== '') {
                isInvalid = true;
            }

            if (isInvalid && input.dataset.path) {
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
                        if (elPath.length === 0 || isPrefix(elPath, err.Path)) {
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

        // Update nav item
        const configNavItem = document.querySelector('.config-nav-item');
        configNavItem?.classList.remove('has-error', 'has-server-error');
        if (frontendErrors.length > 0) {
            configNavItem?.classList.add('has-error');
        } else if (currentErrors.length > 0) {
            configNavItem?.classList.add('has-server-error');
        }
    };

    const toggle = document.getElementById('config-group-toggle') as HTMLInputElement;
    if (toggle) {
        groupByModule = toggle.checked;
        toggle.addEventListener('change', (e) => {
            if (currentSchema && currentValues) {
                const updatedValues = scrapeFormValues();
                if (updatedValues) currentValues = updatedValues;
            }
            groupByModule = (e.target as HTMLInputElement).checked;
            renderConfigForm();
            if (currentErrors && currentErrors.length > 0) {
                handleErrors(currentErrors);
            }
        });
    }

    const form = document.getElementById('config-form');
    if (form) {
        form.addEventListener('input', (e: Event) => {
            const target = e.target as HTMLInputElement;
            if (target && target.tagName === 'INPUT') {
                if (!target.checkValidity()) {
                    target.reportValidity();
                }
            }
            updateRealtimeFrontendErrors();
        });
    }

    try {
        currentSchema = await fetchConfigSchema();
        const res = await fetchConfigValues();
        currentValues = res.Values || res;
        renderConfigForm();
        if (res.Errors && res.Errors.length > 0) {
            currentErrors = res.Errors;
            handleErrors(res.Errors);
        }
    } catch (e) {
        console.error("Failed to init config", e);
    }
}

function renderConfigForm() {
    const container = document.getElementById('config-form');
    if (!container || !currentSchema) return;

    container.innerHTML = '';

    const modules = currentSchema.Config || {};

    if (groupByModule) {
        for (const [modName, modSchema] of Object.entries(modules)) {
            const modContainer = document.createElement('div');
            modContainer.className = 'config-group';
            modContainer.dataset.groupPath = JSON.stringify(["Config", modName, "Config"]);

            const title = document.createElement('h3');
            title.innerText = modName;
            modContainer.appendChild(title);

            const modConfig = (modSchema as any).Config || {};
            const modVals = currentValues.Config?.[modName]?.Config || {};

            const hasProperties = buildProperties(modConfig, modVals, modContainer, ["Config", modName, "Config"]);

            if (hasProperties) {
                container.appendChild(modContainer);
            }
        }
    } else {
        const combinedContainer = document.createElement('div');
        combinedContainer.className = 'config-group';
        combinedContainer.dataset.groupPath = JSON.stringify([]);

        // Merge schemas and values
        const combinedSchema: any = {};
        const combinedValues: any = {};
        const combinedPaths: any = {};

        for (const [modName, modSchema] of Object.entries(modules)) {
            const modConfig = (modSchema as any).Config || {};
            const modVals = currentValues.Config?.[modName]?.Config || {};
            mergeCombined(combinedSchema, combinedValues, combinedPaths, modConfig, modVals, ["Config", modName, "Config"]);
        }

        const hasProperties = buildPropertiesCombined(combinedSchema, combinedValues, combinedPaths, combinedContainer);
        if (hasProperties) {
            container.appendChild(combinedContainer);
        }
    }
}

function mergeCombined(outSchema: any, outValues: any, outPaths: any, inSchema: any, inValues: any, pathPrefix: string[]) {
    for (const [propName, propSchema] of Object.entries(inSchema)) {
        if (!outSchema[propName]) {
            outSchema[propName] = JSON.parse(JSON.stringify(propSchema));
            outValues[propName] = inValues && inValues[propName] !== undefined ? JSON.parse(JSON.stringify(inValues[propName])) : undefined;
            outPaths[propName] = [...pathPrefix, propName];
        } else {
            if (outSchema[propName].Kind === "Sequence" && (propSchema as any).Kind === "Sequence") {
                if (!outValues[propName]) outValues[propName] = {};
                // Upgrade string path to object tracking children
                if (Array.isArray(outPaths[propName])) {
                    const base = outPaths[propName];
                    const childrenPaths: any = {};
                    if (outSchema[propName].Children) {
                        for (const childKey of Object.keys(outSchema[propName].Children)) {
                            childrenPaths[childKey] = [...base, childKey];
                        }
                    }
                    outPaths[propName] = { Base: base, ChildrenPaths: childrenPaths };
                }

                mergeCombined(
                    outSchema[propName].Children,
                    outValues[propName],
                    outPaths[propName].ChildrenPaths,
                    (propSchema as any).Children,
                    inValues ? inValues[propName] : {},
                    [...pathPrefix, propName]
                );
            } else {
                console.warn(`Conflict in combined view for property: ${propName}`);
            }
        }
    }
}

function buildPropertiesCombined(schemaMap: any, valuesMap: any, pathsMap: any, parentEl: HTMLElement): boolean {
    let hasAdded = false;
    for (const [propName, propSchema] of Object.entries(schemaMap)) {
        const val = valuesMap ? valuesMap[propName] : undefined;
        let p = pathsMap[propName];
        if (p && !Array.isArray(p) && p.ChildrenPaths) {
            p = p.Base; // Extract base path if it was converted to an object
        }

        let fieldEl: HTMLElement | null = null;

        if ((propSchema as any).Kind === "Sequence") {
            fieldEl = createCombinedSequence(propName, propSchema, val, pathsMap[propName]);
        } else {
            fieldEl = createField(propName, propSchema, val, p);
        }

        if (fieldEl) {
            parentEl.appendChild(fieldEl);
            hasAdded = true;
        }
    }
    return hasAdded;
}

function createCombinedSequence(name: string, schema: any, value: any, pathsDef: any): HTMLElement | null {
    if (!schema.Children || Object.keys(schema.Children).length === 0) {
        return null;
    }

    const fieldDiv = document.createElement('div');
    fieldDiv.className = 'form-group';

    // Use the base path for dataset.path if possible, or omit it because children will have actual paths
    let basePath = Array.isArray(pathsDef) ? pathsDef : pathsDef.Base;
    if (basePath) fieldDiv.dataset.path = JSON.stringify(basePath);

    const label = document.createElement('label');
    label.innerText = name;
    fieldDiv.appendChild(label);

    if (schema.Description) {
        for (const pText of schema.Description.split('\n')) {
            if (pText.trim().length === 0) continue;
            const desc = document.createElement('p');
            desc.className = 'description';
            desc.innerText = pText;
            fieldDiv.appendChild(desc);
        }
    }


    const wrap = document.createElement('div');
    wrap.style.marginTop = '8px';

    if (schema.Tabbed) {
        wrap.className = 'sequence-group tabbed';
        const tabContainer = document.createElement('div');
        tabContainer.className = 'config-tabs';
        const contentContainer = document.createElement('div');
        contentContainer.className = 'tab-content';

        let first = true;
        let hasAnyTab = false;
        for (const [childName, childSchema] of Object.entries(schema.Children)) {
            const childVal = value ? value[childName] : undefined;
            
            const pane = document.createElement('div');

            const childPathsMap = pathsDef.ChildrenPaths || {};
            let cp = childPathsMap[childName];
            if (!cp && basePath) { // Fallback if this child didn't have a conflict
                cp = [...basePath, childName];
            }

            let f: HTMLElement | null = null;
            if ((childSchema as any).Kind === "Sequence") {
                f = createCombinedSequence(childName, childSchema, childVal, cp);
            } else {
                f = createField(childName, childSchema, childVal, Array.isArray(cp) ? cp : cp.Base);
            }

            if (f) {
                pane.appendChild(f);

                const tabBtn = document.createElement('div');
                tabBtn.className = `config-tab ${first ? 'active' : ''}`;
                tabBtn.innerText = childName;
                tabBtn.dataset.tabPath = JSON.stringify(Array.isArray(cp) ? cp : (cp ? cp.Base : null));
                pane.style.display = first ? 'block' : 'none';

                tabBtn.addEventListener('click', () => {
                    Array.from(tabContainer.children).forEach(c => c.classList.remove('active'));
                    Array.from(contentContainer.children).forEach((c: any) => c.style.display = 'none');
                    tabBtn.classList.add('active');
                    pane.style.display = 'block';
                });

                tabContainer.appendChild(tabBtn);
                contentContainer.appendChild(pane);
                first = false;
                hasAnyTab = true;
            }
        }

        if (!hasAnyTab) return null;

        wrap.appendChild(tabContainer);
        wrap.appendChild(contentContainer);
    } else {
        wrap.className = 'sequence-group';
        wrap.style.paddingLeft = '16px';
        wrap.style.borderLeft = '2px solid var(--border)';
        const childPathsMap = pathsDef.ChildrenPaths || {};
        let hasAnyChild = false;

        for (const [childName, childSchema] of Object.entries(schema.Children)) {
            const childVal = value ? value[childName] : undefined;
            let cp = childPathsMap[childName] || (basePath ? [...basePath, childName] : undefined);

            let f: HTMLElement | null = null;
            if ((childSchema as any).Kind === "Sequence") {
                f = createCombinedSequence(childName, childSchema, childVal, cp);
            } else {
                f = createField(childName, childSchema, childVal, Array.isArray(cp) ? cp : cp.Base);
            }
            if (f) {
                wrap.appendChild(f);
                hasAnyChild = true;
            }
        }

        if (!hasAnyChild) return null;
    }

    fieldDiv.appendChild(wrap);
    return fieldDiv;
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

function createField(name: string, schema: any, value: any, path: string[]): HTMLElement | null {
    const fieldDiv = document.createElement('div');
    fieldDiv.className = 'form-group';
    fieldDiv.dataset.path = JSON.stringify(path);

    if (name) {
        const label = document.createElement('label');
        label.innerText = name;
        fieldDiv.appendChild(label);
    }

    if (schema.Description) {
        for (const pText of schema.Description.split('\n')) {
            if (pText.trim().length === 0) continue;
            const desc = document.createElement('p');
            desc.className = 'description';
            desc.innerText = pText;
            fieldDiv.appendChild(desc);
        }
    }

    const errorSpan = document.createElement('span');
    errorSpan.className = 'error-message';
    errorSpan.id = `err-${path.join('-')}`;
    errorSpan.style.display = 'none';

    let inputArea: HTMLElement | null = null;
    let actualValue = value !== undefined ? value : schema.Default;

    switch (schema.Kind) {
        case 'Boolean':
            inputArea = createBooleanInput(path, actualValue);
            break;
        case 'Integer':
        case 'Float':
            inputArea = createNumberInput(path, actualValue, schema);
            break;
        case 'Discrete':
            inputArea = createSelectInput(path, actualValue, schema.Options);
            break;
        case 'Float_Ratio':
            inputArea = createRatioInput(path, actualValue || { Numerator: schema.Default_Numerator, Denominator: schema.Default_Denominator }, schema);
            break;
        case 'Variant':
            inputArea = createVariantInput(path, actualValue, schema);
            break;
        case 'Sequence':
            inputArea = createSequenceInput(path, actualValue, schema);
            break;
        default:
            console.warn(`Unknown config kind: ${schema.Kind}`);
    }

    if (inputArea) {
        fieldDiv.appendChild(inputArea);
        fieldDiv.appendChild(errorSpan);
        return fieldDiv;
    }
    return null;
}

function createBooleanInput(path: string[], value: boolean): HTMLElement {
    const wrap = document.createElement('label');
    wrap.className = 'toggle-switch';
    const input = document.createElement('input');
    input.type = 'checkbox';
    input.checked = !!value;
    input.dataset.path = JSON.stringify(path);
    input.className = 'config-input-bool';

    const slider = document.createElement('span');
    slider.className = 'slider';

    wrap.appendChild(input);
    wrap.appendChild(slider);
    return wrap;
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
    wrap.appendChild(input);

    if (schema.Unit) {
        const unit = document.createElement('span');
        unit.innerText = schema.Unit;
        wrap.appendChild(unit);
    }
    container.appendChild(wrap);

    if (schema.Min !== undefined || schema.Max !== undefined) {
        const text = [];
        if (schema.Min !== undefined) text.push(`Min: ${schema.Min}`);
        if (schema.Max !== undefined) text.push(`Max: ${schema.Max}`);

        const rangeSpan = document.createElement('div');
        rangeSpan.className = 'range-hint';
        rangeSpan.style.fontSize = '0.85em';
        rangeSpan.style.marginTop = '4px';
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

            if (!valid) {
                rangeSpan.style.color = 'var(--danger)';
            } else {
                rangeSpan.style.color = '';
            }

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

function createSelectInput(path: string[], value: string, options: string[]): HTMLElement {
    const select = document.createElement('select');
    select.dataset.path = JSON.stringify(path);
    select.className = 'config-input-discrete';
    options.forEach(opt => {
        const option = document.createElement('option');
        option.value = opt;
        option.innerText = opt;
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

    const sep = document.createElement('span');
    sep.innerText = ':';

    const den = document.createElement('input');
    den.type = 'number';
    den.step = 'any';
    den.required = true;
    den.value = value.Denominator?.toString() || '1';
    den.dataset.path = JSON.stringify([...path, 'Denominator']);
    den.className = 'config-input-ratio-den';

    wrap.appendChild(num);
    wrap.appendChild(sep);
    wrap.appendChild(den);
    container.appendChild(wrap);

    if (schema && (schema.Min !== undefined || schema.Max !== undefined)) {
        const text = [];
        if (schema.Min !== undefined) text.push(`Min: ${schema.Min}`);
        if (schema.Max !== undefined) text.push(`Max: ${schema.Max}`);

        const rangeSpan = document.createElement('div');
        rangeSpan.className = 'range-hint';
        rangeSpan.style.fontSize = '0.85em';
        rangeSpan.style.marginTop = '4px';
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

            if (!valid) {
                rangeSpan.style.color = 'var(--danger)';
            } else {
                rangeSpan.style.color = '';
            }

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
    const selectStr = Object.keys(schema.Children);

    const select = createSelectInput([...path, 'Selected'], selectedName, selectStr);
    select.className = 'config-input-variant';
    wrap.appendChild(select);

    const childrenContainer = document.createElement('div');
    childrenContainer.style.marginTop = '12px';
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
                childrenContainer.style.paddingLeft = '0';
                childrenContainer.style.borderLeft = 'none';
            } else {
                childrenContainer.style.paddingLeft = '16px';
                childrenContainer.style.borderLeft = '2px solid var(--border)';
            }
            const mappedChildVal = value?.Children?.[activeName];
            const childEl = createField("", activeSchema, mappedChildVal, [...path, 'Children', activeName]);
            if (childEl) childrenContainer.appendChild(childEl);
        }
    };

    select.addEventListener('change', () => renderActiveChild(false));
    renderActiveChild(true);

    return wrap;
}

function createSequenceInput(path: string[], value: any, schema: any): HTMLElement | null {
    if (!schema.Children || Object.keys(schema.Children).length === 0) {
        return null;
    }

    const wrap = document.createElement('div');
    wrap.style.marginTop = '8px';

    if (schema.Tabbed) {
        wrap.className = 'sequence-group tabbed';
        const tabContainer = document.createElement('div');
        tabContainer.className = 'config-tabs';
        const contentContainer = document.createElement('div');
        contentContainer.className = 'tab-content';

        let first = true;
        let hasAnyTab = false;
        for (const [childName, childSchema] of Object.entries(schema.Children)) {
            const childVal = value ? value[childName] : undefined;
            
            const pane = document.createElement('div');
            const f = createField(childName, childSchema, childVal, [...path, childName]);
            if (f) {
                pane.appendChild(f);

                const tabBtn = document.createElement('div');
                tabBtn.className = `config-tab ${first ? 'active' : ''}`;
                tabBtn.innerText = childName;
                tabBtn.dataset.tabPath = JSON.stringify([...path, childName]);
                pane.style.display = first ? 'block' : 'none';

                tabBtn.addEventListener('click', () => {
                    Array.from(tabContainer.children).forEach(c => c.classList.remove('active'));
                    Array.from(contentContainer.children).forEach((c: any) => c.style.display = 'none');
                    tabBtn.classList.add('active');
                    pane.style.display = 'block';
                });

                tabContainer.appendChild(tabBtn);
                contentContainer.appendChild(pane);
                first = false;
                hasAnyTab = true;
            }
        }

        if (!hasAnyTab) return null;

        wrap.appendChild(tabContainer);
        wrap.appendChild(contentContainer);
    } else {
        wrap.className = 'sequence-group';
        wrap.style.paddingLeft = '16px';
        wrap.style.borderLeft = '2px solid var(--border)';
        const hasAdded = buildProperties(schema.Children, value || {}, wrap, path);
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
    ints.forEach(el => applyValue(el.dataset.path!, parseInt(el.value, 10)));
    floats.forEach(el => applyValue(el.dataset.path!, parseFloat(el.value)));
    discretes.forEach(el => applyValue(el.dataset.path!, el.value));
    variants.forEach(el => applyValue(el.dataset.path!, el.value));
    ratioNums.forEach(el => applyValue(el.dataset.path!, parseFloat(el.value)));
    ratioDens.forEach(el => applyValue(el.dataset.path!, parseFloat(el.value)));

    return patchPayload;
}

function populateFormValues(newValues: any) {
    if (!newValues) return;

    const container = document.getElementById('config-form');
    if (!container) return;

    const bools = Array.from(container.querySelectorAll('.config-input-bool')) as HTMLInputElement[];
    const ints = Array.from(container.querySelectorAll('.config-input-int')) as HTMLInputElement[];
    const floats = Array.from(container.querySelectorAll('.config-input-float')) as HTMLInputElement[];
    const discretes = Array.from(container.querySelectorAll('.config-input-discrete')) as HTMLSelectElement[];
    const variants = Array.from(container.querySelectorAll('.config-input-variant')) as HTMLSelectElement[];
    const ratioNums = Array.from(container.querySelectorAll('.config-input-ratio-num')) as HTMLInputElement[];
    const ratioDens = Array.from(container.querySelectorAll('.config-input-ratio-den')) as HTMLInputElement[];

    const getValue = (pathStr: string) => {
        const path = JSON.parse(pathStr);
        let cur = newValues;
        for (let i = 0; i < path.length; i++) {
            if (cur === undefined || cur === null) return undefined;
            cur = cur[path[i]];
        }
        return cur;
    };

    bools.forEach(el => {
        const val = getValue(el.dataset.path!);
        if (val !== undefined) el.checked = !!val;
    });

    ints.forEach(el => {
        const val = getValue(el.dataset.path!);
        if (val !== undefined) el.value = val.toString();
    });

    floats.forEach(el => {
        const val = getValue(el.dataset.path!);
        if (val !== undefined) el.value = val.toString();
    });

    discretes.forEach(el => {
        const val = getValue(el.dataset.path!);
        if (val !== undefined) el.value = val;
    });

    ratioNums.forEach(el => {
        const val = getValue(el.dataset.path!);
        if (val !== undefined) el.value = val.toString();
    });

    ratioDens.forEach(el => {
        const val = getValue(el.dataset.path!);
        if (val !== undefined) el.value = val.toString();
    });

    variants.forEach(el => {
        const val = getValue(el.dataset.path!);
        if (val !== undefined && el.value !== val) {
            el.value = val;
            // Trigger change event to re-render variant children
            el.dispatchEvent(new Event('change'));
        }
    });
}

async function saveConfiguration() {
    if (!currentSchema || !currentValues) return;

    const container = document.getElementById('config-form');
    let validationFailed = false;
    if (container) {
        const inputs = Array.from(container.querySelectorAll('input, select')) as (HTMLInputElement | HTMLSelectElement)[];
        for (const input of inputs) {
            // Check native HTML5 validation
            if (!input.checkValidity()) {
                input.reportValidity();
                validationFailed = true;
                break;
            }
            // Check custom range visual marker
            const numRangeHint = (input.parentElement?.querySelector('.range-hint') || input.parentElement?.parentElement?.querySelector('.range-hint')) as HTMLElement;
            if (numRangeHint && numRangeHint.style.color !== '') {
                validationFailed = true;
                break;
            }
        }
        if (validationFailed) {
            alert("Validation failed. Please correct the highlighted fields.");
            return;
        }
    }

    const patchPayload = scrapeFormValues();
    if (!patchPayload) return;

    if (container) {
        const variants = Array.from(container.querySelectorAll('.config-input-variant')) as HTMLSelectElement[];
        variants.forEach(el => {
            if (!el.dataset.path) return;
            const path = JSON.parse(el.dataset.path);
            const prefix = path.slice(0, -1);
            let cur = patchPayload;
            for (const segment of prefix) {
                if (!cur) break;
                cur = cur[segment];
            }
            if (cur && cur.Children) {
                const selected = el.value;
                for (const key of Object.keys(cur.Children)) {
                    if (key !== selected) {
                        delete cur.Children[key];
                    }
                }
            }
        });
    }

    // Clear previous errors
    document.querySelectorAll('.error-message').forEach((el: any) => {
        el.style.display = 'none';
        el.innerText = '';
        el.classList.remove('server-error');
    });
    const globalErrors = document.getElementById('config-global-errors');
    if (globalErrors) {
        globalErrors.classList.remove('active', 'server-error');
        globalErrors.innerHTML = '';
    }

    try {
        const response = await patchConfigValues(patchPayload);
        if (response.Values) {
            currentValues = response.Values;
            populateFormValues(currentValues);
        }

        if (response.Errors && response.Errors.length > 0) {
            currentErrors = response.Errors;
            handleErrors(response.Errors);
            alert("Saved with errors. Please check the notifications.");
        } else {
            alert("Configuration saved successfully!");
            // Clear server errors from DOM since the save successfully passed
            currentErrors = [];
            handleErrors(currentErrors);

            // Refresh with accepted values
            const res = await fetchConfigValues();
            currentValues = res.Values || res;
            currentErrors = res.Errors || [];
            populateFormValues(currentValues);
            if (currentErrors.length > 0) handleErrors(currentErrors);
        }

        // Let the realtime frontend validation run again so frontend error borders are restored and checked
        if (container) container.dispatchEvent(new Event('input', { bubbles: true }));
    } catch (e) {
        console.error("Save failed", e);
        alert("Failed to save configuration. Network error.");
    }
}

function handleErrors(errors: any[]) {
    const globalErrors = document.getElementById('config-global-errors');
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
            const msgWithNote = err.Message + ' (Will only be rechecked after saving)';
            if (errSpan.style.display === 'block') {
                errSpan.innerText += '\n' + msgWithNote;
            } else {
                errSpan.innerText = msgWithNote;
                errSpan.style.display = 'block';
            }
        } else if (globalErrors) {
            hasGlobal = true;
            const p = document.createElement('p');
            p.innerText = `${err.Path.join(' -> ')}: ${err.Message} (Will only be rechecked after saving)`;
            globalErrors.appendChild(p);
        }

        // Add outlines to groups and tabs
        document.querySelectorAll('.config-group').forEach(el => {
            const elPathStr = (el as HTMLElement).dataset.groupPath;
            if (elPathStr) {
                const elPath = JSON.parse(elPathStr);
                if (elPath.length === 0 || isPrefix(elPath, err.Path)) {
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
}
