import { fetchConfigSchema, fetchConfigValues, patchConfigValues } from './api.js';
import { renderDescription } from './description_markup.js';
import {
    onLocaleChange,
    t,
    translateConfigDescription,
    translateConfigLabel,
    translateConfigOption,
    translateConfigUnit
} from './localization.js';

let currentSchema: any = null;
let currentValues: any = null;
let currentErrors: any[] = [];
let dynamicPresentationControllerPaths = new Set<string>();

let groupByModule = true;
let devMode = false;
let showExperimental = false;

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
                // Ignore missing definition of handleErrors since it is globally bound
                // @ts-ignore
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

        form.addEventListener('change', (e: Event) => {
            const target = e.target as HTMLInputElement | HTMLSelectElement;
            if (!target?.dataset.path) return;
            const targetPath = JSON.parse(target.dataset.path);
            if (!dynamicPresentationControllerPaths.has(JSON.stringify(targetPath))) return;

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

    try {
        currentSchema = await fetchConfigSchema();
        dynamicPresentationControllerPaths = collectDynamicPresentationControllerPaths(currentSchema);
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
    let value = currentValues?.Config?.[owner]?.Config;
    for (const segment of path) {
        if (value === undefined || value === null) return undefined;
        value = value[segment];
    }
    return value;
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
            title.innerText = translateConfigLabel(["Config", modName, "Config"], modName);
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
        if (!isDynamicallyPresented(propSchema)) continue;
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

type FieldRenderOptions = {
    showLabel?: boolean;
    redundantTitleContext?: string | null;
};

function getOnlyChildName(schema: any): string | null {
    if (!schema?.Children) return null;
    const childNames = Object.keys(schema.Children);
    return childNames.length === 1 ? childNames[0] : null;
}

function createCombinedSequence(
    name: string,
    schema: any,
    value: any,
    pathsDef: any,
    options: FieldRenderOptions = {}
): HTMLElement | null {
    if (!isDynamicallyPresented(schema)) return null;
    if (!schema.Children || Object.keys(schema.Children).length === 0) {
        return null;
    }

    const fieldDiv = document.createElement('div');
    fieldDiv.className = 'form-group';

    // Use the base path for dataset.path if possible, or omit it because children will have actual paths
    let basePath = Array.isArray(pathsDef) ? pathsDef : pathsDef.Base;
    if (basePath) fieldDiv.dataset.path = JSON.stringify(basePath);

    if (name && options.showLabel !== false) {
        const label = document.createElement('label');
        label.innerText = translateConfigLabel(basePath, name);
        fieldDiv.appendChild(label);
    }

    if (schema.Experimental) {
        const badge = document.createElement('span');
        badge.className = 'description';
        badge.innerText = t('ui.config.experimental', 'Experimental');
        fieldDiv.appendChild(badge);
    }

    const description = translateConfigDescription(basePath, schema.Description || '');
    if (description) {
        renderDescription(fieldDiv, description);
    }

    if (devMode && basePath && basePath.length > 0) {
        const pathDesc = document.createElement('p');
        pathDesc.className = 'description dev-mode-path';
        pathDesc.innerText = JSON.stringify(basePath).replace(/,/g, ', ');
        fieldDiv.appendChild(pathDesc);
    }


    const wrap = document.createElement('div');
    wrap.classList.add('mt-8');

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
                f = createCombinedSequence(childName, childSchema, childVal, cp, {
                    showLabel: false,
                    redundantTitleContext: childName
                });
            } else {
                f = createField(childName, childSchema, childVal, Array.isArray(cp) ? cp : cp.Base, {
                    showLabel: false,
                    redundantTitleContext: childName
                });
            }

            if (f) {
                pane.appendChild(f);

                const tabBtn = document.createElement('div');
                tabBtn.className = `config-tab ${first ? 'active' : ''}`;
                tabBtn.innerText = translateConfigLabel(Array.isArray(cp) ? cp : (cp ? cp.Base : []), childName);
                tabBtn.dataset.tabPath = JSON.stringify(Array.isArray(cp) ? cp : (cp ? cp.Base : null));
                pane.className = `tab-pane ${first ? 'active' : ''}`;
                pane.dataset.tabPath = JSON.stringify(Array.isArray(cp) ? cp : (cp ? cp.Base : null));

                tabBtn.addEventListener('click', () => {
                    Array.from(tabContainer.children).forEach(c => c.classList.remove('active'));
                    Array.from(contentContainer.children).forEach((c: any) => c.classList.remove('active'));
                    tabBtn.classList.add('active');
                    pane.classList.add('active');
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
        const childPathsMap = pathsDef.ChildrenPaths || {};
        let hasAnyChild = false;

        const onlyChildName = getOnlyChildName(schema);
        const shouldSuppressOnlyChildTitle = onlyChildName !== null && onlyChildName === options.redundantTitleContext;

        if (shouldSuppressOnlyChildTitle) {
            const childSchema = schema.Children[onlyChildName];
            const childVal = value ? value[onlyChildName] : undefined;
            const cp = childPathsMap[onlyChildName] || (basePath ? [...basePath, onlyChildName] : undefined);

            let f: HTMLElement | null = null;
            if ((childSchema as any).Kind === "Sequence") {
                f = createCombinedSequence(onlyChildName, childSchema, childVal, cp, {
                    showLabel: false,
                    redundantTitleContext: onlyChildName
                });
            } else {
                f = createField(onlyChildName, childSchema, childVal, Array.isArray(cp) ? cp : cp.Base, {
                    showLabel: false,
                    redundantTitleContext: onlyChildName
                });
            }

            if (f) {
                wrap.appendChild(f);
                hasAnyChild = true;
            }
        } else {
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

    if (name && options.showLabel !== false) {
        const label = document.createElement('label');
        label.innerText = translateConfigLabel(path, name);
        fieldDiv.appendChild(label);
        hasContent = true;
    }

    if (schema.Experimental) {
        const badge = document.createElement('span');
        badge.className = 'description';
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

                const tabBtn = document.createElement('div');
                tabBtn.className = `config-tab ${first ? 'active' : ''}`;
                tabBtn.innerText = translateConfigLabel([...path, childName], childName);
                tabBtn.dataset.tabPath = JSON.stringify([...path, childName]);
                pane.className = `tab-pane ${first ? 'active' : ''}`;
                pane.dataset.tabPath = JSON.stringify([...path, childName]);

                tabBtn.addEventListener('click', () => {
                    Array.from(tabContainer.children).forEach(c => c.classList.remove('active'));
                    Array.from(contentContainer.children).forEach((c: any) => c.classList.remove('active'));
                    tabBtn.classList.add('active');
                    pane.classList.add('active');
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

function populateFormValues(newValues: any) {
    if (!newValues) return;

    const container = document.getElementById('config-form');
    if (!container) return;

    const bools = Array.from(container.querySelectorAll('.config-input-bool')) as HTMLInputElement[];
    const strings = Array.from(container.querySelectorAll('.config-input-string')) as HTMLInputElement[];
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

    strings.forEach(el => {
        const val = getValue(el.dataset.path!);
        if (val !== undefined) el.value = val;
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
            alert(t('ui.config.validationFailed', 'Validation failed. Please correct the highlighted fields.'));
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
        el.classList.add('d-none');
        el.classList.remove('d-block');
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
            alert(t('ui.config.savedWithErrors', 'Saved with errors. Please check the notifications.'));
        } else {
            alert(t('ui.config.saved', 'Configuration saved successfully!'));
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
        alert(t('ui.config.saveFailed', 'Failed to save configuration. Network error.'));
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
