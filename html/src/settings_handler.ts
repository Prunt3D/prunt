export { };

interface SettingsSchemaBase {
    Description: string;
}

interface IntegerSettingsSchema extends SettingsSchemaBase {
    Kind: "Integer";
    Min: number;
    Max: number;
    Unit: string;
    Default: number;
}

interface FloatSettingsSchema extends SettingsSchemaBase {
    Kind: "Float";
    Min: number;
    Max: number;
    Unit: string;
    Default: number;
}

interface FloatRatioSettingsSchema extends SettingsSchemaBase {
    Kind: "Float_Ratio";
    Min: number;
    Max: number;
    Default_Numerator: number;
    Default_Denominator: number;
}

interface SequenceSettingsSchema extends SettingsSchemaBase {
    Kind: "Sequence";
    Tabbed: boolean;
    Children: Record<string, SettingsSchemaEntry>;
}

interface VariantSettingsSchema extends SettingsSchemaBase {
    Kind: "Variant";
    Children: Record<string, SettingsSchemaEntry>;
    Default: string;
}

interface DiscreteSettingsSchema extends SettingsSchemaBase {
    Kind: "Discrete";
    Options: string[];
    Default: string;
}

interface BooleanSettingsSchema extends SettingsSchemaBase {
    Kind: "Boolean";
    Default: boolean;
}

type SettingsSchemaEntry =
    | IntegerSettingsSchema
    | FloatSettingsSchema
    | FloatRatioSettingsSchema
    | SequenceSettingsSchema
    | VariantSettingsSchema
    | DiscreteSettingsSchema
    | BooleanSettingsSchema;

interface ModuleConfig {
    Version: number;
    Config: Record<string, SettingsSchemaEntry>;
}

interface OuterConfig {
    "Prunt config version": number;
    Config: Record<string, ModuleConfig>;
}

const configElements = new Map<string, HTMLElement>();
const configErrorLabels = new Map<string, HTMLElement>();
const configOwnerTabs = new Map<string, HTMLElement>();
const fieldOwnerModules = new Map<string, string>();
let unifiedSchema: Record<string, SettingsSchemaEntry> = {};

const configFieldValidationReset = new Event("configFieldValidationReset");
const setVariant = new Event("setVariant");


function updateValidation(): number {
    let numRemoteErrors = 0;
    let numLocalErrors = 0;
    const localInvalidTabs = new Set<HTMLElement>();
    const remoteInvalidTabs = new Set<HTMLElement>();

    for (const [key, errorLabel] of configErrorLabels) {
        const element = configElements.get(key);
        if (
            (element instanceof HTMLInputElement && element.type === "number" && !element.checkValidity()) ||
            (errorLabel.textContent !== "")
        ) {
            let isLocalError =
                element instanceof HTMLInputElement && element.type === "number" && !element.checkValidity();
            let isRemoteError = errorLabel.textContent !== "";
            if (isLocalError) {
                numLocalErrors++;
            }

            if (isRemoteError) {
                numRemoteErrors++;
            }

            let key2 = key;
            do {
                if (configOwnerTabs.has(key2)) {
                    if (isLocalError) {
                        localInvalidTabs.add(configOwnerTabs.get(key2)!);
                    }
                    if (isRemoteError) {
                        remoteInvalidTabs.add(configOwnerTabs.get(key2)!);
                    }
                }
                key2 = key2.split("$").slice(0, -1).join("$");
            } while (key2 !== "");
        }
    }

    if (numLocalErrors !== 0) {
        localInvalidTabs.add(document.getElementById("configTab")!);
    }
    if (numRemoteErrors !== 0) {
        remoteInvalidTabs.add(document.getElementById("configTab")!);
    }

    document.querySelectorAll<HTMLElement>(".tab").forEach((tab) => {
        if (localInvalidTabs.has(tab)) {
            tab.classList.add("local-invalid");
            tab.classList.remove("remote-invalid");
        } else if (remoteInvalidTabs.has(tab)) {
            tab.classList.add("remote-invalid");
            tab.classList.remove("local-invalid");
        } else {
            tab.classList.remove("remote-invalid");
            tab.classList.remove("local-invalid");
        }
    });

    return numLocalErrors;
}

function buildField(
    schema: SettingsSchemaEntry,
    path: string,
    container: HTMLElement,
    ownerTab: HTMLElement
) {
    configOwnerTabs.set(path, ownerTab);

    const label = document.createElement("div");
    label.innerHTML = schema.Description;
    container.appendChild(label);

    const errorLabel = document.createElement("div");
    errorLabel.classList.add("config-error-label");
    errorLabel.textContent = "";
    container.appendChild(errorLabel);

    configErrorLabels.set(path, errorLabel);

    switch (schema.Kind) {
        case "Sequence":
            if (schema.Tabbed) {
                buildTabbedSequence(schema.Children, path, container);
            } else {
                if (!container.classList.contains("tab-content")) {
                    container.classList.add("config-sequence");
                }
                buildSequence(schema.Children, path, container, ownerTab);
            }
            break;
        case "Variant":
            label.textContent +=
                " The selected tab when the save button is pressed is the option that will be used.";
            buildVariant(schema.Children, path, container);
            break;
        case "Discrete":
            buildDiscrete(schema.Options, path, container);
            break;
        case "Boolean":
            buildBoolean(schema, path, container);
            break;
        case "Integer":
            if (schema.Unit !== "") {
                label.innerHTML += ` (Units: ${schema.Unit})`;
            }
            buildInteger(schema, path, container);
            break;
        case "Float":
            if (schema.Unit !== "") {
                label.innerHTML += ` (Units: ${schema.Unit})`;
            }
            buildFloat(schema, path, container);
            break;
        case "Float_Ratio":
            configErrorLabels.set(path + "$Numerator", errorLabel);
            configErrorLabels.set(path + "$Denominator", errorLabel);
            buildFloatRatio(schema, path, container);
            break;
        default:
            throw new Error("Unknown field type.");
    }
}

function sanitizeForId(str: string): string {
    return str.replace(/[^a-zA-Z0-9-_]/g, '-');
}

function buildTabbedSequence(
    schema: Record<string, SettingsSchemaEntry>,
    path: string,
    container: HTMLElement
) {
    const tabContainer = document.createElement("div");
    tabContainer.classList.add("tab-container");
    tabContainer.setAttribute("role", "tablist");
    tabContainer.setAttribute("data-kind", "Sequence");

    const contentContainer = document.createElement("div");
    contentContainer.classList.add("tab-content-container");

    for (const [name, data] of Object.entries(schema)) {
        if (data.Kind !== "Sequence" || !data.Tabbed || Object.keys(data.Children).length != 0) {
            const sanitizedPath = sanitizeForId(path);
            const sanitizedName = sanitizeForId(name);
            const uniquePrefix = `nested-${sanitizedPath ? sanitizedPath + '-' : ''}${sanitizedName}`;
            const tabId = `${uniquePrefix}-tab`;
            const panelId = `${uniquePrefix}-panel`;

            const tab = document.createElement("div");
            tab.classList.add("tab");
            tab.textContent = name;

            tab.setAttribute("role", "tab");
            tab.setAttribute("id", tabId);
            tab.setAttribute("aria-controls", panelId);
            tab.setAttribute("tabindex", "0");
            tab.setAttribute("aria-selected", "false");

            const tabContent = document.createElement("div");
            tabContent.classList.add("tab-content", "hidden");
            tabContent.setAttribute("role", "tabpanel");
            tabContent.setAttribute("id", panelId);
            tabContent.setAttribute("aria-labelledby", tabId);

            tab.addEventListener("click", () => {
                tabContainer.querySelectorAll<HTMLElement>(":scope > .tab").forEach(t => {
                    t.classList.remove("active");
                    t.setAttribute("aria-selected", "false");
                });
                contentContainer.querySelectorAll(":scope > .tab-content").forEach(c => c.classList.add("hidden"));

                tab.classList.add("active");
                tab.setAttribute("aria-selected", "true");
                tabContent.classList.remove("hidden");
            });

            tab.addEventListener("keydown", (event: KeyboardEvent) => {
                if (event.key === "Enter" || event.key === " ") {
                    event.preventDefault();
                    tab.click();
                }
            });

            console.log(data);
            buildField(data, `${path}${path === "" ? "" : "$"}${name}`, tabContent, tab);

            tabContainer.appendChild(tab);
            contentContainer.appendChild(tabContent);
        }
    }

    container.appendChild(tabContainer);
    container.appendChild(contentContainer);
    configElements.set(path, tabContainer); // Needed for Sequence "value" placeholder if path is not root
}

function buildSequence(
    schema: Record<string, SettingsSchemaEntry>,
    path: string,
    container: HTMLElement,
    ownerTab: HTMLElement
) {
    for (const [name, data] of Object.entries(schema)) {
        const row = document.createElement("div");
        row.classList.add("config-form-row");
        const title = document.createElement("div");
        title.classList.add("title");
        title.textContent = name;
        row.appendChild(title);

        buildField(data, `${path}${path === "" ? "" : "$"}${name}`, row, ownerTab);

        container.appendChild(row);
    }
}

function buildVariant(
    schema: Record<string, SettingsSchemaEntry>,
    path: string,
    container: HTMLElement
) {
    const tabContainer = document.createElement("div");
    tabContainer.classList.add("tab-container");
    tabContainer.setAttribute("role", "tablist");
    tabContainer.setAttribute("data-kind", "Variant");

    const contentContainer = document.createElement("div");
    contentContainer.classList.add("tab-content-container");

    const dialog = document.createElement("dialog");
    dialog.classList.add("tab-switch-dialog");
    const dialogText = document.createElement("p");
    dialog.appendChild(dialogText);

    const yesButton = document.createElement("button");
    yesButton.textContent = "Yes";
    dialog.appendChild(yesButton);

    const noButton = document.createElement("button");
    noButton.textContent = "No";
    dialog.appendChild(noButton);

    document.body.appendChild(dialog);

    for (const [name, data] of Object.entries(schema)) {
        const sanitizedPath = sanitizeForId(path);
        const sanitizedName = sanitizeForId(name);
        const uniquePrefix = `variant-${sanitizedPath ? sanitizedPath + '-' : ''}${sanitizedName}`;
        const tabId = `${uniquePrefix}-tab`;
        const panelId = `${uniquePrefix}-panel`;

        const tab = document.createElement("div");
        tab.classList.add("tab");
        tab.textContent = name;
        tab.setAttribute("role", "tab");
        tab.setAttribute("id", tabId);
        tab.setAttribute("aria-controls", panelId);
        tab.setAttribute("tabindex", "0");
        tab.setAttribute("aria-selected", "false");

        const tabContent = document.createElement("div");
        tabContent.classList.add("tab-content");
        tabContent.classList.add("hidden");
        tabContent.setAttribute("role", "tabpanel");
        tabContent.setAttribute("id", panelId);
        tabContent.setAttribute("aria-labelledby", tabId);

        tab.addEventListener("click", () => {
            if (tab.classList.contains("active")) {
                return;
            }

            dialogText.textContent = "Are you sure you want to switch to " + name + "?";
            dialog.showModal();

            yesButton.onclick = () => {
                dialog.close();

                tabContainer.querySelectorAll<HTMLElement>(":scope > .tab").forEach(t => {
                    t.classList.remove("active");
                    t.setAttribute("aria-selected", "false");
                });
                contentContainer.querySelectorAll(":scope > .tab-content").forEach(c => c.classList.add("hidden"));

                tab.classList.add("active");
                tab.setAttribute("aria-selected", "true");
                tabContent.classList.remove("hidden");
            };

            noButton.onclick = () => {
                dialog.close();
            };
        });

        tab.addEventListener("keydown", (event: KeyboardEvent) => {
            if (event.key === "Enter" || event.key === " ") {
                event.preventDefault();
                tab.click();
            }
        });

        tab.addEventListener("setVariant", () => {
            tabContainer.querySelectorAll<HTMLElement>(":scope > .tab").forEach(t => {
                t.classList.remove("active");
                t.setAttribute("aria-selected", "false");
            });
            contentContainer.querySelectorAll(":scope > .tab-content").forEach(c => c.classList.add("hidden"));

            tab.classList.add("active");
            tab.setAttribute("aria-selected", "true");
            tabContent.classList.remove("hidden");
        });

        buildField(data, `${path}${path === "" ? "" : "$"}${name}`, tabContent, tab);

        tabContainer.appendChild(tab);
        contentContainer.appendChild(tabContent);
    }

    configElements.set(path, tabContainer);

    container.appendChild(tabContainer);
    container.appendChild(contentContainer);
}

function buildDiscrete(options: string[], path: string, container: HTMLElement) {
    const select = document.createElement("select");

    for (const option of options) {
        const optionElement = document.createElement("option");
        optionElement.value = option;
        optionElement.textContent = option;
        select.appendChild(optionElement);
    }

    configElements.set(path, select);

    container.appendChild(select);
}

function buildBoolean(schema: SettingsSchemaBase, path: string, container: HTMLElement) {
    const input = document.createElement("input");
    input.type = "checkbox";

    configElements.set(path, input);

    container.appendChild(input);
}

function buildInteger(schema: IntegerSettingsSchema, path: string, container: HTMLElement) {
    const input = document.createElement("input");
    input.type = "number";
    input.min = schema.Min.toString();
    input.max = schema.Max.toString();
    input.step = "1";
    input.setAttribute("required", "");

    let oldValidity: boolean | null = null;
    input.addEventListener("input", function() {
        if (oldValidity !== input.checkValidity()) {
            oldValidity = input.checkValidity();
            updateValidation();
        }
    });
    input.addEventListener("configFieldValidationReset", function() {
        oldValidity = null;
    });

    const rangeMessage = document.createElement("span");
    rangeMessage.textContent = "Range: " + schema.Min + " to " + schema.Max + "(integers only)";
    rangeMessage.classList.add("input-validity-error-label");

    configElements.set(path, input);

    container.appendChild(input);
    container.appendChild(rangeMessage);
}

function buildFloat(schema: FloatSettingsSchema, path: string, container: HTMLElement) {
    const input = document.createElement("input");
    input.type = "number";
    input.min = schema.Min.toString();
    input.max = schema.Max.toString();
    input.step = "any";
    input.setAttribute("required", "");

    let oldValidity: boolean | null = null;
    input.addEventListener("input", function() {
        if (oldValidity !== input.checkValidity()) {
            oldValidity = input.checkValidity();
            updateValidation();
        }
    });
    input.addEventListener("configFieldValidationReset", function() {
        oldValidity = null;
    });

    const rangeMessage = document.createElement("span");
    rangeMessage.textContent = "Range: " + schema.Min + " to " + schema.Max;
    rangeMessage.classList.add("input-validity-error-label");

    configElements.set(path, input);

    container.appendChild(input);
    container.appendChild(rangeMessage);
}

function buildFloatRatio(schema: FloatRatioSettingsSchema, path: string, container: HTMLElement) {
    const inputA = document.createElement("input");
    inputA.type = "number";
    inputA.step = "any";
    inputA.setAttribute("required", "");

    const inputB = document.createElement("input");
    inputB.type = "number";
    inputB.step = "any";
    inputB.setAttribute("required", "");

    let oldValidityA: boolean | null = null;
    let oldValidityB: boolean | null = null;

    function validate() {
        inputA.setCustomValidity("");
        inputB.setCustomValidity("");

        const a = parseFloat(inputA.value);
        const b = parseFloat(inputB.value);

        if (!isNaN(a) && !isNaN(b)) {
            if (b == 0.0) {
                inputB.setCustomValidity("Denominator cannot be zero.");
            } else {
                const ratio = a / b;
                if (ratio < schema.Min || ratio > schema.Max) {
                    const errorMessage = `Ratio A/B for A:B must be between ${schema.Min} and ${schema.Max}. Denominator cannot be zero.`;
                    inputA.setCustomValidity(errorMessage);
                    inputB.setCustomValidity(errorMessage);
                }
            }
        }

        const newValidityA = inputA.checkValidity();
        const newValidityB = inputB.checkValidity();

        if (oldValidityA !== newValidityA || oldValidityB !== newValidityB) {
            oldValidityA = newValidityA;
            oldValidityB = newValidityB;
            updateValidation();
        }
    };

    inputA.addEventListener("input", validate);
    inputB.addEventListener("input", validate);

    function resetHandler() {
        oldValidityA = null;
        oldValidityB = null;
        validate();
    };
    inputA.addEventListener("configFieldValidationReset", resetHandler);
    inputB.addEventListener("configFieldValidationReset", resetHandler);

    configElements.set(path + "$Numerator", inputA);
    configElements.set(path + "$Denominator", inputB);

    const rangeMessage = document.createElement("span");
    rangeMessage.textContent = `Ratio A/B for A:B must be between ${schema.Min} and ${schema.Max}. Denominator cannot be zero.`;
    rangeMessage.classList.add("input-validity-error-label");

    const inputsWrapper = document.createElement("div");
    inputsWrapper.style.display = "flex";
    inputsWrapper.style.alignItems = "center";
    inputsWrapper.style.gap = "0.5em";
    inputsWrapper.appendChild(inputA);
    const div = document.createElement("div");
    div.textContent = ":";
    inputsWrapper.appendChild(div);
    inputsWrapper.appendChild(inputB);
    inputsWrapper.appendChild(rangeMessage);

    container.appendChild(inputsWrapper);
}

function mergeSchemas(
    base: Record<string, SettingsSchemaEntry>,
    moduleSchema: Record<string, SettingsSchemaEntry>,
    path: string,
    moduleName: string
) {
    for (const [key, value] of Object.entries(moduleSchema)) {
        if (key in base) {
            // Collision detection
            const baseValue = base[key];
            if (baseValue.Kind === "Sequence" && value.Kind === "Sequence") {
                // Merge sequences
                mergeSchemas(baseValue.Children, value.Children, path ? `${path}$${key}` : key, moduleName);
            } else {
                // Collision error
                throw new Error(`Schema collision in module '${moduleName}' at path '${path ? path + '$' : ''}${key}'. Field already exists and is not a mergeable Sequence.`);
            }
        } else {
            // New field
            base[key] = value;
            // Record ownership for this subtree (until overridden by recursive calls on next modules, but we are in a single module pass here)
            // We need to record ownership for all LEAF nodes. recursive helper?
            recordOwnership(value, path ? `${path}$${key}` : key, moduleName);
        }
    }
}

function recordOwnership(schema: SettingsSchemaEntry, path: string, moduleName: string) {
    if (schema.Kind === "Sequence" || schema.Kind === "Variant") { // Variant children are local to variant, but we want the variant itself.
        if (schema.Kind === "Variant") {
             fieldOwnerModules.set(path, moduleName);
        }
        // Recurse
        for (const [key, child] of Object.entries(schema.Children)) {
            recordOwnership(child, `${path}$${key}`, moduleName);
        }
    } else {
        fieldOwnerModules.set(path, moduleName);
        // Float_Ratio special handling handled by checking schema type at path during save
    }
}

async function saveConfig(): Promise<void> {

    if (updateValidation() !== 0) {
        alert("There are out of range numbers in the config. Fix them before saving.");
        return;
    }

    const outerConfig: OuterConfig = {
        "Prunt config version": 1,
        Config: {}
    };

    // Helper to set deep value
    function setDeep(curr: any, parts: string[], value: any) {
        for (let i = 0; i < parts.length - 1; i++) {
            if (!curr[parts[i]]) {
                curr[parts[i]] = {};
            }
            curr = curr[parts[i]];
        }
        curr[parts[parts.length - 1]] = value;
    }

    for (const [key, element] of configElements) {
        let value: any = undefined;

        if (element.classList.contains("tab-container")) {
            // Only convert Variant tab state to value. Sequence tabs are just UI.
            const kind = element.getAttribute("data-kind");
            if (kind === "Variant") {
                const activeTab = Array.from(element.children).find(
                    (child) => child.classList.contains("active")
                );
                 value = activeTab ? activeTab.textContent : "";
            } else {
                continue; // Skip Sequence containers
            }
        } else if (element instanceof HTMLSelectElement) {
            value = element.value;
        } else if (element instanceof HTMLInputElement && element.type === "checkbox") {
            value = element.checked;
        } else if (element instanceof HTMLInputElement && element.type === "number" && element.step === "any") {
            value = parseFloat(element.value);
        } else if (element instanceof HTMLInputElement && element.type === "number") {
             value = parseInt(element.value, 10);
        } else {
            console.log(key);
            console.log(element);
            throw new Error("Unhandled field type.");
        }

        // Now reconstruct logic
        // key is Path like "A$B$C"
        // Find owner:
        // For Float_Ratio path is "Path$Numerator". Logic: strip last part if schema at path is missing?
        // Or check schema for Path. If missing, check parent.
        let pathForSchema = key;
        let isNumerator = false;
        let isDenominator = false;

        if (key.endsWith("$Numerator")) {
             pathForSchema = key.substring(0, key.length - 10);
             isNumerator = true;
        } else if (key.endsWith("$Denominator")) {
             pathForSchema = key.substring(0, key.length - 12);
             isDenominator = true;
        }

        const ownerModule = fieldOwnerModules.get(pathForSchema);
        if (!ownerModule) {
            console.warn(`No owner module found for path ${key}. Skipping.`);
            continue;
        }

        if (!outerConfig.Config[ownerModule]) {
            outerConfig.Config[ownerModule] = { Version: 1, Config: {} }; // Assuming version 1 for now, or fetch from loaded?
            // To properly preserve Version, we should probably have stored it during load.
            // For now, defaulting to 1 is likely safe if versioning isn't strict yet or if we assume we just fetched latest.
            // Better: Store loaded module versions.
        }

        const pathParts = key.split("$"); // This is the path in the flat schema
        // The path in "Config" object needs to be constructed.
        // Special types handling:
        // Variant: Path "Var$Child" -> "Var", "Children", "Child"
        // Sequence: Path "Seq$Child" -> "Seq", "Child"
        // Float_Ratio: Path "Val$Numerator" -> "Val", "Numerator"

        // We need to traverse the Unified Schema to map Path Parts to JSON Structure Parts
        let currentSchema = unifiedSchema;
        const jsonPathParts: string[] = [];
        
        // Handle Float_Ratio suffix first
        let partsToProcess = key.split("$");
        if (isNumerator) {
             partsToProcess = pathForSchema.split("$");
        } else if (isDenominator) {
             partsToProcess = pathForSchema.split("$");
        }

        for (const part of partsToProcess) {
             if (!currentSchema[part]) {
                 throw new Error(`Schema path mismatch at ${part} in ${key}`);
             }
             const entry = currentSchema[part];
             jsonPathParts.push(part);
             
             if (entry.Kind === "Variant") {
                 // For children of Variant, we go into "Children"
                 // BUT, wait. If we are processing the Variant ITSELF (the selected tab value), path is just "Var".
                 // Loop ends here. jsonPathParts is ["Var"].
                 // If we are processing a child "Var$Child", we are in the loop.
                 // The next part will be "Child".
                 // We need to inject "Children".
                 // Wait, if I am at "Var", and there is a next part?
                 // Currently loop iterates parts.
                 // If I have "Var", I push "Var".
                 // schema becomes entry.Children.
             }
             if (entry.Kind === "Variant" || entry.Kind === "Sequence") {
                  currentSchema = entry.Children;
                  if (entry.Kind === "Variant") {
                      // If there are more parts, it implies we are accessing a child.
                      // So we need to add "Children" to the json path *before* the next part is added.
                      // But we don't know if there is a next part yet?
                      // Actually, we do. The loop will continue.
                      // BUT we shouldn't add "Children" if we are at the end (saving the variant selection).
                      // We can check index.
                   }
             }
        }

        // Post-processing path for Variant Children
        // We need to insert "Children" node for every Variant parent if we are deeper than the variant itself.
        // Actually, let's re-traverse.
        const outputParts: string[] = [];
        let s = unifiedSchema;
        for (let i = 0; i < partsToProcess.length; i++) {
            const part = partsToProcess[i];
            outputParts.push(part);
            const node = s[part];
            if (node.Kind === "Variant") {
                if (i < partsToProcess.length - 1) { // If not the last part, we are going deeper
                    outputParts.push("Children");
                } else if (isNumerator || isDenominator) {
                    // Logic for variant that IS a float ratio? Not possible, Variant has children.
                } else {
                     // Last part. If we are saving the Variant SELECTION, we target "Selected".
                     // But the loop above handled "tab-container" check to determine value source.
                     // The path key for checking schema is "Var".
                     // If we are saving the "Var" selection, we want "Var", "Selected".
                     if (element.classList.contains("tab-container") && element.getAttribute("data-kind") === "Variant") {
                         outputParts.push("Selected");
                     }
                }
            }
            if (node.Kind === "Variant" || node.Kind === "Sequence") {
                s = node.Children;
            }
        }
        
        if (isNumerator) outputParts.push("Numerator");
        if (isDenominator) outputParts.push("Denominator");

        setDeep(outerConfig.Config[ownerModule].Config, outputParts, value);
    }

    const response = await fetch("./config/values", {
        method: "POST",
        headers: {
            "Content-Type": "application/json",
        },
        body: JSON.stringify(outerConfig),
    });

    if (!response.ok) {
        const message = `Failed to save config:\n${response.statusText}\n${await response.text()}`;
        console.error(message);
        alert(message);
    } else {
        const responseData = await response.json();
        // TODO: Error handling for bad JSON.
        updateValues(responseData);
        alert("Settings saved. Restart Prunt to apply new settings.");
    }
}

function updateValues(values: { Values: OuterConfig; Errors: { Key: string; Message: string }[] }): void {
    // Flatten values from OuterConfig to Path->Value map
    const flatValues: Record<string, any> = {};

    function flatten(current: any, path: string, schema: Record<string, SettingsSchemaEntry>) {
        // We need schema to know how to traverse (Variant vs Sequence)
        // current is the Config object or sub-object.
        
        for (const key of Object.keys(current)) {
            // If current object has "Prunt config version" etc, we skipped that by calling flatten on Config.Module.Config
            
            // Current key is a field name in the schema?
            // Special handling for Variant: { Selected: "...", Children: { ... } }
            // Special handling for Float_Ratio: { Numerator: ..., Denominator: ... }
            
            // But 'current' might be the Module Config root.
            // 'schema' corresponds to 'current'.
            
            if (!schema[key]) {
                // If key is not in schema, it might be "Selected" or "Children" or "Numerator" etc.
                // But we iterate keys of DATA.
                // So if we are in a Variant, we see "Selected" and "Children".
                continue; 
            }
            
            const node = schema[key];
            const nextPath = path ? `${path}$${key}` : key;
            const val = current[key];
            
            if (node.Kind === "Variant") {
                // val should be { Selected: "...", Children: { ... } }
                if (val && val.Selected !== undefined) {
                    flatValues[nextPath] = val.Selected;
                }
                if (val && val.Children) {
                    flatten(val.Children, nextPath, node.Children);
                }
            } else if (node.Kind === "Sequence") {
                // val is { Child: ... }
                // Recurse
                flatten(val, nextPath, node.Children);
            } else if (node.Kind === "Float_Ratio") {
                // val is { Numerator: ..., Denominator: ... }
                if (val) {
                    flatValues[`${nextPath}$Numerator`] = val.Numerator;
                    flatValues[`${nextPath}$Denominator`] = val.Denominator;
                }
            } else {
                // Leaf
                flatValues[nextPath] = val;
            }
        }
    }

    // Iterate modules in OuterConfig
    if (values.Values && values.Values.Config) {
        for (const [modName, modConfig] of Object.entries(values.Values.Config)) {
             // We need to merge this module's data into the flat view.
             // We use the unified schema to guide flattening?
             // Actually, we can use the unified schema, but we need to start at the right place.
             // But Wait. Unified Schema is MERGED. Module Schema is subset.
             // But we recorded ownership!
             // So for any path, we know if it belongs to this module.
             // Simpler: Just recursively flatten using the Unified Schema, but skipping keys not present in the data?
             // Yes. The data structure should match the schema structure (except for flattened Sequences).
             // When flattening a module's config, we are traversing IT'S structure.
             // Does it match Unified Schema?
             // Yes, except Unified Schema has MORE keys (from other modules).
             // But "current[key]" check handles that.
             
             flatten(modConfig.Config, "", unifiedSchema);
        }
    }

    for (const [key, label] of configErrorLabels) {
        label.textContent = "";
    }

    for (const [key, element] of configElements) {
        if (element.classList.contains("tab-container")) {
            for (const child of Array.from(element.children)) {
                if (flatValues[key] === child.textContent) {
                    child.dispatchEvent(setVariant);
                }
            }
        } else if (
            element instanceof HTMLSelectElement || (element instanceof HTMLInputElement && element.type === "number")
        ) {
            if (flatValues[key] !== undefined) {
                 element.value = flatValues[key];
                 element.dispatchEvent(configFieldValidationReset);
            }
        } else if (element instanceof HTMLInputElement && element.type === "checkbox") {
            if (flatValues[key] !== undefined) {
                element.checked = flatValues[key];
            }
        } else {
            // console.log(key);
        }
    }

    for (const error of values.Errors) {
        const errorLabel = configErrorLabels.get(error.Key);
        if (errorLabel) {
            errorLabel.textContent += ` ${error.Message} Save after fixing error to clear this message.`;
        }
    }

    updateValidation();
}

export async function setupSettings(): Promise<void> {
    const configTabContent = document.getElementById("configTabContent") as HTMLElement;
    configTabContent.innerHTML = "";
    
    // Reset global state
    configElements.clear();
    configErrorLabels.clear();
    configOwnerTabs.clear();
    fieldOwnerModules.clear();
    unifiedSchema = {};

    const schemaResponse = await fetch("./config/schema");

    if (!schemaResponse.ok) {
        const message = `Failed to load config schema:\n${schemaResponse.statusText}\n${await schemaResponse.text()}`;
        console.error(message);
        throw new Error(message);
    } else {
        const outerConfigSchema: OuterConfig = await schemaResponse.json();
        
        // Merge schemas
        if (outerConfigSchema.Config) {
            for (const [modName, mod] of Object.entries(outerConfigSchema.Config)) {
                mergeSchemas(unifiedSchema, mod.Config, "", modName);
            }
        }

        buildTabbedSequence(unifiedSchema, "", configTabContent);

        const valuesResponse = await fetch("./config/values");

        if (!valuesResponse.ok) {
            const message = `Failed to load config values:\n${valuesResponse.statusText}\n${await valuesResponse.text()}`;
            console.error(message);
            throw new Error(message);
        } else {
            const values = await valuesResponse.json();
            updateValues(values);

            const saveButton = document.createElement("button");
            saveButton.textContent = "Save all options";
            saveButton.addEventListener("click", saveConfig);
            configTabContent.appendChild(saveButton);
        }
    }
};
