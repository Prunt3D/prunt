configElements = new Map();
configErrorLabels = new Map();
configOwnerTabs = new Map();

const configFieldValidationReset = new Event("configFieldValidationReset");
const setVariant = new Event("setVariant");

function updateValidation() {
  numErrors = 0;
  invalidTabs = new Set();

  for (const [key, element] of configElements) {
    if ((element.type === "number" && !element.checkValidity()) || configErrorLabels.get(key).innerText !== "") {
      numErrors += 1;
      key2 = key;
      do {
        if (configOwnerTabs.has(key2)) {
          invalidTabs.add(configOwnerTabs.get(key2));
        }
        key2 = key2.split("$").slice(0, -1).join("$");
      } while (key2 !== "");
    }
  }

  if (numErrors !== 0) {
    invalidTabs.add(document.getElementById("configTab"));
  }

  document
    .querySelectorAll(".tab")
    .forEach(t => invalidTabs.has(t) ? t.classList.add("invalid") : t.classList.remove("invalid"));

  return numErrors;
}

function buildField(schema, path, container, ownerTab) {
  configOwnerTabs.set(path, ownerTab);

  const label = document.createElement("div");
  label.innerText = schema.Description;
  container.appendChild(label);
  const errorLabel = document.createElement("div");
  errorLabel.classList.add("config-error-label");
  errorLabel.innerText = "";
  container.appendChild(errorLabel);

  configErrorLabels.set(path, errorLabel);

  if (schema.Kind === "Tabbed Sequence") {
    buildTabbedSequence(schema.Children, path, container);
  } else if (schema.Kind === "Sequence") {
    buildSequence(schema.Children, path, container, ownerTab);
  } else if (schema.Kind === "Variant") {
    label.innerText += " The selected tab when the save button is pressed is the option that will be used.";
    buildVariant(schema.Children, path, container);
  } else if (schema.Kind === "Discrete") {
    buildDiscrete(schema.Options, path, container);
  } else if (schema.Kind === "Boolean") {
    buildBoolean(schema, path, container);
  } else if (schema.Kind === "Integer") {
    if (schema.Unit !== "") {
      label.innerText += " (Units: " + schema.Unit + ")";
    }
    buildInteger(schema, path, container);
  } else if (schema.Kind === "Float") {
    if (schema.Unit !== "") {
      label.innerText += " (Units: " + schema.Unit + ")";
    }
    buildFloat(schema, path, container);
  } else {
    throw new Error("Unknown field type.");
  }
}

function buildTabbedSequence(schema, path, container) {
  const tabContainer = document.createElement("div");
  tabContainer.classList.add("tab-container");

  const contentContainer = document.createElement("div");
  contentContainer.classList.add("tab-content-container");

  for (const [name, data] of Object.entries(schema)) {
    const tab = document.createElement("div");
    tab.classList.add("tab");
    tab.innerText = name;

    const tabContent = document.createElement("div");
    tabContent.classList.add("tab-content");
    tabContent.classList.add("hidden");

    tab.addEventListener("click", () => {
      tabContainer.querySelectorAll(":scope > .tab").forEach(t => t.classList.remove("active"));
      contentContainer.querySelectorAll(":scope > .tab-content").forEach(c => c.classList.add("hidden"));

      tab.classList.add("active");
      tabContent.classList.remove("hidden");
    });

    buildField(data, path + (path === "" ? "" : "$") + name, tabContent, tab);

    tabContainer.appendChild(tab);
    contentContainer.appendChild(tabContent);
  }

  container.appendChild(tabContainer);
  container.appendChild(contentContainer);
}

function buildSequence(schema, path, container, ownerTab) {
  for (const [name, data] of Object.entries(schema)) {
    const row = document.createElement("div");
    row.classList.add("config-form-row");
    const title = document.createElement("div");
    title.classList.add("title");
    title.innerText = name;
    row.appendChild(title);

    buildField(data, path + "$" + name, row, ownerTab);

    container.appendChild(row);
  }
}

function buildVariant(schema, path, container) {
  const tabContainer = document.createElement("div");
  tabContainer.classList.add("tab-container");

  const contentContainer = document.createElement("div");
  contentContainer.classList.add("tab-content-container");

  const dialog = document.createElement("dialog");
  dialog.classList.add("tab-switch-dialog");
  const dialogText = document.createElement("p");
  dialog.appendChild(dialogText);

  const yesButton = document.createElement("button");
  yesButton.innerText = "Yes";
  dialog.appendChild(yesButton);

  const noButton = document.createElement("button");
  noButton.innerText = "No";
  dialog.appendChild(noButton);

  document.body.appendChild(dialog);

  for (const [name, data] of Object.entries(schema)) {
    const tab = document.createElement("div");
    tab.classList.add("tab");
    tab.innerText = name;

    const tabContent = document.createElement("div");
    tabContent.classList.add("tab-content");
    tabContent.classList.add("hidden");

    tab.addEventListener("click", () => {
      if (tab.classList.contains("active")) {
        return;
      }

      dialogText.innerText = "Are you sure you want to switch to " + name + "?";
      dialog.showModal();

      yesButton.onclick = () => {
        dialog.close();

        tabContainer.querySelectorAll(":scope > .tab").forEach(t => t.classList.remove("active"));
        contentContainer.querySelectorAll(":scope > .tab-content").forEach(c => c.classList.add("hidden"));

        tab.classList.add("active");
        tabContent.classList.remove("hidden");
      };

      noButton.onclick = () => {
        dialog.close();
      };
    });

    tab.addEventListener("setVariant", () => {
      tabContainer.querySelectorAll(":scope > .tab").forEach(t => t.classList.remove("active"));
      contentContainer.querySelectorAll(":scope > .tab-content").forEach(c => c.classList.add("hidden"));

      tab.classList.add("active");
      tabContent.classList.remove("hidden");
    });

    buildField(data, path + (path === "" ? "" : "$") + name, tabContent, tab);

    tabContainer.appendChild(tab);
    contentContainer.appendChild(tabContent);
  }

  configElements.set(path, tabContainer);

  container.appendChild(tabContainer);
  container.appendChild(contentContainer);
}

function buildDiscrete(schema, path, container) {
  const select = document.createElement("select");

  for (const x of schema) {
    const option = document.createElement("option");
    option.value = x;
    option.innerText = x;
    select.appendChild(option);
  }

  configElements.set(path, select);

  container.appendChild(select);
}

function buildBoolean(schema, path, container) {
  const input = document.createElement("input");
  input.type = "checkbox";

  configElements.set(path, input);

  container.appendChild(input);
}

function buildInteger(schema, path, container) {
  const input = document.createElement("input");
  input.type = "number";
  input.min = schema.Min;
  input.max = schema.Max;
  input.step = 1;
  input.setAttribute("required", "");

  oldValidity = null;
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
  rangeMessage.innerText = "Range: " + schema.Min + " to " + schema.Max + "(integers only)";
  rangeMessage.classList.add("input-validity-error-label");

  configElements.set(path, input);

  container.appendChild(input);
  container.appendChild(rangeMessage);
}

function buildFloat(schema, path, container) {
  const input = document.createElement("input");
  input.type = "number";
  input.min = schema.Min;
  input.max = schema.Max;
  input.step = "any";
  input.setAttribute("required", "");

  oldValidity = null;
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
  rangeMessage.innerText = "Range: " + schema.Min + " to " + schema.Max;
  rangeMessage.classList.add("input-validity-error-label");

  configElements.set(path, input);

  container.appendChild(input);
  container.appendChild(rangeMessage);
}

function saveConfig() {
  const configData = {};

  for (const [key, element] of configElements) {
    if (element.classList.contains("tab-container")) {
      configData[key] = "";
      for (x of element.children) {
        if (x.classList.contains("active")) {
          configData[key] = x.innerText;
        }
      }
    } else if (element.nodeName === "SELECT") {
      configData[key] = element.value;
    } else if (element.type === "checkbox") {
      configData[key] = element.checked;
    } else if (element.type === "number" && element.step === "any") {
      configData[key] = parseFloat(element.value);
    } else if (element.type === "number") {
      configData[key] = parseInt(element.value);
    } else {
      console.log(key);
      console.log(element);
      throw new Error("Unhandled field type.");
    }
  }

  console.log(configData);
}

(async function() {
  const topTabContainer = document.getElementById("topTabContainer");
  const topTabContentContainer = document.getElementById("topTabContentContainer");
  for (name of ["status", "config", "console", "file"]) {
    const tab = document.getElementById(name + "Tab");
    const tabContent = document.getElementById(name + "TabContent");
    tab.addEventListener("click", () => {
      topTabContainer.querySelectorAll(":scope > .tab").forEach(t => t.classList.remove("active"));
      topTabContentContainer.querySelectorAll(":scope > .tab-content").forEach(c => c.classList.add("hidden"));

      tab.classList.add("active");
      tabContent.classList.remove("hidden");
    });
  }

  const configTabContent = document.getElementById("configTabContent");
  configTabContent.innerHTML = "";
  const schema = await (await fetch("./config/schema")).json();
  buildTabbedSequence(schema, "", configTabContent);

  const values = await (await fetch("./config/values")).json();

  for (const [key, element] of configElements) {
    configErrorLabels.get(key).innerText = "";

    if (element.classList.contains("tab-container")) {
      for (x of element.children) {
        if (values.Values[key] === x.innerText) {
          x.dispatchEvent(setVariant);
        }
      }
    } else if (element.nodeName === "SELECT" || element.type === "number") {
      element.value = values.Values[key];
      element.dispatchEvent(configFieldValidationReset);
    } else if (element.type === "checkbox") {
      element.checked = values.Values[key];
    } else {
      console.log(key);
      console.log(element);
      throw new Error("Unhandled field type.");
    }
  }

  for (const error of values.Errors) {
    configErrorLabels.get(error.Key).innerText += " " + error.Message;
  }

  updateValidation();

  const saveButton = document.createElement("button");
  saveButton.innerText = "Save all options";
  saveButton.addEventListener("click", saveConfig);
  configTabContent.appendChild(saveButton);
})();
