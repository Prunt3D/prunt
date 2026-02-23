import { fetchConfigSchema, fetchConfigValues, patchConfigValues } from './api';

let currentSchema: any = null;
let currentValues: any = null;
let currentErrors: any[] = [];

let groupByModule = true;

export async function initConfigView() {
    const btnSave = document.getElementById('btn-save-config');
    btnSave?.addEventListener('click', saveConfiguration);

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

    setupScrollHints();

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
            
            const title = document.createElement('h3');
            title.innerText = modName;
            modContainer.appendChild(title);

            const modConfig = (modSchema as any).Config || {};
            const modVals = currentValues.Config?.[modName]?.Config || {};
            
            buildProperties(modConfig, modVals, modContainer, ["Config", modName, "Config"]);
            
            container.appendChild(modContainer);
        }
    } else {
        const combinedContainer = document.createElement('div');
        combinedContainer.className = 'config-group';
        
        // Merge schemas and values
        const combinedSchema: any = {};
        const combinedValues: any = {};
        const combinedPaths: any = {};

        for (const [modName, modSchema] of Object.entries(modules)) {
            const modConfig = (modSchema as any).Config || {};
            const modVals = currentValues.Config?.[modName]?.Config || {};
            mergeCombined(combinedSchema, combinedValues, combinedPaths, modConfig, modVals, ["Config", modName, "Config"]);
        }

        buildPropertiesCombined(combinedSchema, combinedValues, combinedPaths, combinedContainer);
        container.appendChild(combinedContainer);
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
                    outPaths[propName] = { Base: outPaths[propName], ChildrenPaths: {} };
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

function buildPropertiesCombined(schemaMap: any, valuesMap: any, pathsMap: any, parentEl: HTMLElement) {
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
        }
    }
}

function createCombinedSequence(name: string, schema: any, value: any, pathsDef: any): HTMLElement | null {
    const fieldDiv = document.createElement('div');
    fieldDiv.className = 'form-group';
    
    // Use the base path for dataset.path if possible, or omit it because children will have actual paths
    let basePath = Array.isArray(pathsDef) ? pathsDef : pathsDef.Base;
    if (basePath) fieldDiv.dataset.path = JSON.stringify(basePath);

    const label = document.createElement('label');
    label.innerText = name;
    fieldDiv.appendChild(label);

    if (schema.Description) {
        const desc = document.createElement('p');
        desc.className = 'description';
        desc.innerText = schema.Description;
        fieldDiv.appendChild(desc);
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
        for (const [childName, childSchema] of Object.entries(schema.Children)) {
            const childVal = value ? value[childName] : undefined;
            const tabBtn = document.createElement('div');
            tabBtn.className = `config-tab ${first ? 'active' : ''}`;
            tabBtn.innerText = childName;
            
            const pane = document.createElement('div');
            pane.style.display = first ? 'block' : 'none';
            
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

            if (f) pane.appendChild(f);
            
            tabBtn.addEventListener('click', () => {
                Array.from(tabContainer.children).forEach(c => c.classList.remove('active'));
                Array.from(contentContainer.children).forEach((c: any) => c.style.display = 'none');
                tabBtn.classList.add('active');
                pane.style.display = 'block';
            });
            
            tabContainer.appendChild(tabBtn);
            contentContainer.appendChild(pane);
            first = false;
        }
        
        wrap.appendChild(tabContainer);
        wrap.appendChild(contentContainer);
    } else {
        wrap.className = 'sequence-group';
        wrap.style.paddingLeft = '16px';
        wrap.style.borderLeft = '2px solid var(--border)';
        const childPathsMap = pathsDef.ChildrenPaths || {};
        
        for (const [childName, childSchema] of Object.entries(schema.Children)) {
            const childVal = value ? value[childName] : undefined;
            let cp = childPathsMap[childName] || (basePath ? [...basePath, childName] : undefined);
            
            let f: HTMLElement | null = null;
            if ((childSchema as any).Kind === "Sequence") {
                f = createCombinedSequence(childName, childSchema, childVal, cp);
            } else {
                f = createField(childName, childSchema, childVal, Array.isArray(cp) ? cp : cp.Base);
            }
            if (f) wrap.appendChild(f);
        }
    }

    fieldDiv.appendChild(wrap);
    return fieldDiv;
}

// Recursively builds the form fields
function buildProperties(schemaMap: any, valuesMap: any, parentEl: HTMLElement, path: string[]) {
    for (const [propName, propSchema] of Object.entries(schemaMap)) {
        const val = valuesMap ? valuesMap[propName] : undefined;
        const fieldEl = createField(propName, propSchema, val, [...path, propName]);
        if (fieldEl) {
            parentEl.appendChild(fieldEl);
        }
    }
}

function createField(name: string, schema: any, value: any, path: string[]): HTMLElement | null {
    const fieldDiv = document.createElement('div');
    fieldDiv.className = 'form-group';
    fieldDiv.dataset.path = JSON.stringify(path);

    const label = document.createElement('label');
    label.innerText = name;
    fieldDiv.appendChild(label);

    if (schema.Description) {
        const desc = document.createElement('p');
        desc.className = 'description';
        desc.innerText = schema.Description;
        fieldDiv.appendChild(desc);
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
            inputArea = createRatioInput(path, actualValue || { Numerator: schema.Default_Numerator, Denominator: schema.Default_Denominator });
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
    const wrap = document.createElement('div');
    wrap.className = 'form-control-row';
    const input = document.createElement('input');
    input.type = 'number';
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
    return wrap;
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

function createRatioInput(path: string[], value: any): HTMLElement {
    const wrap = document.createElement('div');
    wrap.className = 'ratio-group';
    
    const num = document.createElement('input');
    num.type = 'number';
    num.step = 'any';
    num.value = value.Numerator?.toString() || '0';
    num.dataset.path = JSON.stringify([...path, 'Numerator']);
    num.className = 'config-input-ratio-num';
    
    const sep = document.createElement('span');
    sep.innerText = '/';
    
    const den = document.createElement('input');
    den.type = 'number';
    den.step = 'any';
    den.value = value.Denominator?.toString() || '1';
    den.dataset.path = JSON.stringify([...path, 'Denominator']);
    den.className = 'config-input-ratio-den';
    
    wrap.appendChild(num);
    wrap.appendChild(sep);
    wrap.appendChild(den);
    return wrap;
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
    childrenContainer.style.paddingLeft = '16px';
    childrenContainer.style.borderLeft = '2px solid var(--border)';
    wrap.appendChild(childrenContainer);

    // Re-render children when selection changes
    const renderActiveChild = () => {
        childrenContainer.innerHTML = '';
        const activeName = (select as HTMLSelectElement).value;
        const activeSchema = schema.Children[activeName];
        if (activeSchema) {
            const mappedChildVal = value?.Children?.[activeName];
            const childEl = createField(activeName, activeSchema, mappedChildVal, [...path, 'Children', activeName]);
            if (childEl) childrenContainer.appendChild(childEl);
        }
    };
    
    select.addEventListener('change', renderActiveChild);
    renderActiveChild();
    
    return wrap;
}

function createSequenceInput(path: string[], value: any, schema: any): HTMLElement {
    const wrap = document.createElement('div');
    wrap.style.marginTop = '8px';
    
    if (schema.Tabbed) {
        wrap.className = 'sequence-group tabbed';
        const tabContainer = document.createElement('div');
        tabContainer.className = 'config-tabs';
        const contentContainer = document.createElement('div');
        contentContainer.className = 'tab-content';
        
        let first = true;
        for (const [childName, childSchema] of Object.entries(schema.Children)) {
            const childVal = value ? value[childName] : undefined;
            const tabBtn = document.createElement('div');
            tabBtn.className = `config-tab ${first ? 'active' : ''}`;
            tabBtn.innerText = childName;
            
            const pane = document.createElement('div');
            pane.style.display = first ? 'block' : 'none';
            const f = createField(childName, childSchema, childVal, [...path, childName]);
            if (f) pane.appendChild(f);
            
            tabBtn.addEventListener('click', () => {
                Array.from(tabContainer.children).forEach(c => c.classList.remove('active'));
                Array.from(contentContainer.children).forEach((c: any) => c.style.display = 'none');
                tabBtn.classList.add('active');
                pane.style.display = 'block';
            });
            
            tabContainer.appendChild(tabBtn);
            contentContainer.appendChild(pane);
            first = false;
        }
        
        wrap.appendChild(tabContainer);
        wrap.appendChild(contentContainer);
    } else {
        wrap.className = 'sequence-group';
        wrap.style.paddingLeft = '16px';
        wrap.style.borderLeft = '2px solid var(--border)';
        buildProperties(schema.Children, value || {}, wrap, path);
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

async function saveConfiguration() {
    if (!currentSchema || !currentValues) return;

    const patchPayload = scrapeFormValues();
    if (!patchPayload) return;

    // Clear previous errors
    document.querySelectorAll('.error-message').forEach((el: any) => {
        el.style.display = 'none';
        el.innerText = '';
    });
    const globalErrors = document.getElementById('config-global-errors');
    if (globalErrors) {
        globalErrors.classList.remove('active');
        globalErrors.innerHTML = '';
    }

    try {
        const response = await patchConfigValues(patchPayload);
        if (response.Errors && response.Errors.length > 0) {
            currentErrors = response.Errors;
            handleErrors(response.Errors);
            alert("Saved with errors. Please check the notifications.");
        } else {
            alert("Configuration saved successfully!");
            // Refresh with accepted values
            const res = await fetchConfigValues();
            currentValues = res.Values || res;
            currentErrors = res.Errors || [];
            renderConfigForm();
            if (currentErrors.length > 0) handleErrors(currentErrors);
        }
    } catch (e) {
        console.error("Save failed", e);
        alert("Failed to save configuration. Network error.");
    }
}

let activeErrors: HTMLElement[] = [];
let scrollHintTop: HTMLElement | null = null;
let scrollHintBottom: HTMLElement | null = null;
let mainContentArea: HTMLElement | null = null;

function setupScrollHints() {
    scrollHintTop = document.getElementById('scroll-hint-top');
    scrollHintBottom = document.getElementById('scroll-hint-bottom');
    mainContentArea = document.querySelector('.content-area');

    if (mainContentArea) {
        mainContentArea.addEventListener('scroll', updateScrollHints);
    }
    
    if (scrollHintTop) {
        scrollHintTop.addEventListener('click', () => scrollToError('prev'));
    }
    if (scrollHintBottom) {
        scrollHintBottom.addEventListener('click', () => scrollToError('next'));
    }
}

function updateScrollHints() {
    if (!mainContentArea || activeErrors.length === 0) {
        scrollHintTop?.classList.remove('show');
        scrollHintBottom?.classList.remove('show');
        return;
    }

    const viewportTop = mainContentArea.scrollTop;
    const viewportBottom = viewportTop + mainContentArea.clientHeight;

    let hasErrorAbove = false;
    let hasErrorBelow = false;

    for (const errEl of activeErrors) {
        const errTop = errEl.parentElement?.offsetTop || errEl.offsetTop;
        if (errTop < viewportTop) {
            hasErrorAbove = true;
        } else if (errTop > viewportBottom) {
            hasErrorBelow = true;
        }
    }

    if (hasErrorAbove) scrollHintTop?.classList.add('show');
    else scrollHintTop?.classList.remove('show');

    if (hasErrorBelow) scrollHintBottom?.classList.add('show');
    else scrollHintBottom?.classList.remove('show');
}

function scrollToError(dir: 'next' | 'prev') {
    if (!mainContentArea || activeErrors.length === 0) return;
    
    const viewportTop = mainContentArea.scrollTop;
    let targetEl: HTMLElement | null = null;

    if (dir === 'next') {
        for (const errEl of activeErrors) {
            const errTop = errEl.parentElement?.offsetTop || errEl.offsetTop;
            if (errTop > viewportTop + 10) { // +10 fuzz
                targetEl = errEl.parentElement || errEl;
                break;
            }
        }
    } else {
        for (let i = activeErrors.length - 1; i >= 0; i--) {
            const errEl = activeErrors[i];
            const errTop = errEl.parentElement?.offsetTop || errEl.offsetTop;
            if (errTop < viewportTop - 10) {
                targetEl = errEl.parentElement || errEl;
                break;
            }
        }
    }

    if (targetEl) {
        targetEl.scrollIntoView({ behavior: 'smooth', block: 'center' });
    }
}

function handleErrors(errors: any[]) {
    const globalErrors = document.getElementById('config-global-errors');
    let hasGlobal = false;
    
    activeErrors = [];
    const configNavItem = document.querySelector('.config-nav-item');

    if (errors && errors.length > 0) {
        configNavItem?.classList.add('has-error');
    } else {
        configNavItem?.classList.remove('has-error');
    }

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
            if (errSpan.style.display === 'block') {
                errSpan.innerText += '\n' + err.Message;
            } else {
                errSpan.innerText = err.Message;
                errSpan.style.display = 'block';
            }
            // Add to tracked errors for scroll hints
            if (!activeErrors.includes(errSpan)) {
                activeErrors.push(errSpan);
            }
        } else if (globalErrors) {
            hasGlobal = true;
            const p = document.createElement('p');
            p.innerText = `${err.Path.join(' -> ')}: ${err.Message}`;
            globalErrors.appendChild(p);
        }
    });

    if (hasGlobal && globalErrors) {
         globalErrors.classList.add('active');
    }
    
    // Sort active errors by vertical position
    activeErrors.sort((a, b) => {
        const aTop = a.parentElement?.offsetTop || a.offsetTop;
        const bTop = b.parentElement?.offsetTop || b.offsetTop;
        return aTop - bTop;
    });

    updateScrollHints();
}
