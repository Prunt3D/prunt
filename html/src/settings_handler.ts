export { };

interface SettingsSchemaBase {
    Description: string;
}

interface IntegerSettingsSchema extends SettingsSchemaBase {
    Kind: "Integer";
    Min: number;
    Max: number;
    Unit: string;
}

interface FloatSettingsSchema extends SettingsSchemaBase {
    Kind: "Float";
    Min: number;
    Max: number;
    Unit: string;
}

interface FloatRatioSettingsSchema extends SettingsSchemaBase {
    Kind: "Float_Ratio";
    Min: number;
    Max: number;
}

interface TabbedSequenceSettingsSchema extends SettingsSchemaBase {
    Kind: "Tabbed_Sequence";
    Children: Record<string, SettingsSchemaEntry>;
}

interface SequenceSettingsSchema extends SettingsSchemaBase {
    Kind: "Sequence";
    Children: Record<string, SettingsSchemaEntry>;
}

interface VariantSettingsSchema extends SettingsSchemaBase {
    Kind: "Variant";
    Children: Record<string, SettingsSchemaEntry>;
}

interface DiscreteSettingsSchema extends SettingsSchemaBase {
    Kind: "Discrete";
    Options: string[];
}

interface BooleanSettingsSchema extends SettingsSchemaBase {
    Kind: "Boolean";
}

type SettingsSchemaEntry =
    | IntegerSettingsSchema
    | FloatSettingsSchema
    | FloatRatioSettingsSchema
    | TabbedSequenceSettingsSchema
    | SequenceSettingsSchema
    | VariantSettingsSchema
    | DiscreteSettingsSchema
    | BooleanSettingsSchema;

const configElements = new Map<string, HTMLElement>();
const configErrorLabels = new Map<string, HTMLElement>();
const configOwnerTabs = new Map<string, HTMLElement>();

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
        case "Tabbed_Sequence":
            buildTabbedSequence(schema.Children, path, container);
            break;
        case "Sequence":
            if (!container.classList.contains("tab-content")) {
                container.classList.add("config-sequence");
            }
            buildSequence(schema.Children, path, container, ownerTab);
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

    const contentContainer = document.createElement("div");
    contentContainer.classList.add("tab-content-container");

    for (const [name, data] of Object.entries(schema)) {
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

        buildField(data, `${path}${path === "" ? "" : "$"}${name}`, tabContent, tab);

        tabContainer.appendChild(tab);
        contentContainer.appendChild(tabContent);
    }

    container.appendChild(tabContainer);
    container.appendChild(contentContainer);
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

async function saveConfig(): Promise<void> {
    const configData: Record<string, any> = {};

    if (updateValidation() !== 0) {
        alert("There are out of range numbers in the config. Fix them before saving.");
        return;
    }

    for (const [key, element] of configElements) {
        if (element.classList.contains("tab-container")) {
            configData[key] = "";
            const activeTab = Array.from(element.children).find(
                (child) => child.classList.contains("active")
            );
            if (activeTab) {
                configData[key] = activeTab.textContent;
            }
        } else if (element instanceof HTMLSelectElement) {
            configData[key] = element.value;
        } else if (element instanceof HTMLInputElement && element.type === "checkbox") {
            configData[key] = element.checked;
        } else if (element instanceof HTMLInputElement && element.type === "number" && element.step === "any") {
            configData[key] = parseFloat(element.value);
        } else if (element instanceof HTMLInputElement && element.type === "number") {
            configData[key] = parseInt(element.value, 10);
        } else {
            console.log(key);
            console.log(element);
            throw new Error("Unhandled field type.");
        }
    }

    const response = await fetch("./config/values", {
        method: "POST",
        headers: {
            "Content-Type": "application/json",
        },
        body: JSON.stringify(configData),
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

function updateValues(values: { Values: Record<string, any>; Errors: { Key: string; Message: string }[] }): void {
    for (const [key, label] of configErrorLabels) {
        label.textContent = "";
    }

    for (const [key, element] of configElements) {
        if (element.classList.contains("tab-container")) {
            for (const child of Array.from(element.children)) {
                if (values.Values[key] === child.textContent) {
                    child.dispatchEvent(setVariant);
                }
            }
        } else if (
            element instanceof HTMLSelectElement || (element instanceof HTMLInputElement && element.type === "number")
        ) {
            element.value = values.Values[key];
            element.dispatchEvent(configFieldValidationReset);
        } else if (element instanceof HTMLInputElement && element.type === "checkbox") {
            element.checked = values.Values[key];
        } else {
            console.log(key);
            console.log(element);
            throw new Error("Unhandled field type.");
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

    const schemaResponse = await fetch("./config/schema");

    if (!schemaResponse.ok) {
        const message = `Failed to load config schema:\n${schemaResponse.statusText}\n${await schemaResponse.text()}`;
        console.error(message);
        throw new Error(message);
    } else {
        const schema: Record<string, SettingsSchemaEntry> = await schemaResponse.json();
        buildTabbedSequence(schema, "", configTabContent);

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
