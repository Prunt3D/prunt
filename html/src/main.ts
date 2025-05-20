import { setupSettings } from "./settings_handler";
import { setupStatus } from "./status_handler";

export function uploadFile(): void {
    const fileInput = document.getElementById("fileUploadInput") as HTMLInputElement;

    if (fileInput.files && fileInput.files.length > 0) {
        const file = fileInput.files[0];

        const uploadProgress = document.getElementById("fileUploadProgress") as HTMLProgressElement;

        const xhr = new XMLHttpRequest();
        xhr.upload.addEventListener("progress", (event) => {
            if (event.lengthComputable) {
                uploadProgress.value = event.loaded / event.total * 100;
            }
        });
        xhr.addEventListener("loadend", () => {
            if (xhr.status == 204) {
                alert("File upload complete.");
            } else {
                const message = `File upload failed:\n${xhr.statusText}\n${xhr.responseText}`;
                console.error(message);
                alert(message);
            }
        });
        xhr.open("PUT", `/uploads/${encodeURIComponent(file.name)}`, true);
        xhr.send(file);
    } else {
        alert("Please select a file to upload.");
    }
}

export function refreshFiles(): void {
    fetch("./uploads").then((response) => {
        if (!response.ok) {
            response.text().then((error) => {
                const message = `File refresh failed:\n${response.statusText}\n${error}`;
                console.error(message);
                alert(message);
            });
        } else {
            response.json().then((files: string[]) => {
                // TODO: Error handling for bad JSON.
                const fileRunInput = document.getElementById("fileRunInput") as HTMLSelectElement;

                fileRunInput.innerHTML = "";

                const option = document.createElement("option");
                option.value = "";
                option.textContent = "";
                fileRunInput.appendChild(option);

                files.forEach((file) => {
                    const option = document.createElement("option");
                    option.value = file;
                    option.textContent = file;
                    fileRunInput.appendChild(option);
                });
            });
        }
    });
}

export async function runFile(): Promise<void> {
    const fileRunInput = document.getElementById("fileRunInput") as HTMLSelectElement;
    const selectedFile = fileRunInput.value;

    if (selectedFile == "") {
        alert("Please select a file to run.");
        return;
    }

    const response = await fetch("./run-file", {
        method: "POST",
        headers: {
            "Content-Type": "text/plain",
        },
        body: selectedFile
    });

    if (response.ok) {
        alert("File queued.");
    } else {
        response.text().then((error) => {
            const message = `Failed to run file:\n${response.statusText}\n${error}`;
            console.error(message);
            alert(message);
        });
    }
}

export async function downloadFile(): Promise<void> {
    const fileRunInput = document.getElementById("fileRunInput") as HTMLSelectElement;
    const selectedFile = fileRunInput.value;

    if (selectedFile == "") {
        alert("Please select a file to download.");
        return;
    }

    const link = document.createElement("a");
    link.href = `./uploads/${selectedFile}`;
    link.download = selectedFile;
    link.click();
}

export async function runCommand(): Promise<void> {
    const commandLog = document.getElementById("commandLog") as HTMLDivElement;
    const commandRunInput = document.getElementById("commandRunInput") as HTMLInputElement;
    const command = commandRunInput.value;
    const timestamp = new Date().toLocaleTimeString();

    if (command == "") {
        alert("Please enter a command to run.");
        return;
    }

    const response = await fetch("./run-command", {
        method: "POST",
        headers: {
            "Content-Type": "text/plain",
        },
        body: command
    });

    const entry = document.createElement("p");

    if (response.ok) {
        entry.textContent = `${timestamp}: Enqueued command: ${command}`;
        commandRunInput.value = "";
    } else {
        entry.textContent = `${timestamp}: Failed to enqueue command: ${command}`;
        response.text().then((error) => {
            const message = `Failed to run command:\n${response.statusText}\n${error}`;
            console.error(message);
            alert(message);
        });
    }

    commandLog.appendChild(entry);
    commandLog.scrollTop = commandLog.scrollHeight;
}

const topTabContainer = document.getElementById("topTabContainer") as HTMLElement;
const topTabContentContainer = document.getElementById("topTabContentContainer") as HTMLElement;
const messageLog = document.getElementById("messageLog") as HTMLDivElement;

const tabs = ["status", "config", "console", "file", "log"];
for (const name of tabs) {
    const tab = document.getElementById(`${name}Tab`) as HTMLElement;
    const tabContent = document.getElementById(`${name}TabContent`) as HTMLElement;

    tab.addEventListener("click", () => {
        topTabContainer.querySelectorAll<HTMLElement>(':scope > [role="tab"]').forEach(t => {
            t.classList.remove("active");
            t.setAttribute('aria-selected', 'false');
        });
        topTabContentContainer.querySelectorAll<HTMLElement>(':scope > [role="tabpanel"]').forEach(c => {
            c.classList.add("hidden");
        });

        tab.classList.add("active");
        tab.setAttribute('aria-selected', 'true');
        tab.classList.remove("has-update");
        tabContent.classList.remove("hidden");

        if (name === 'log') {
            messageLog.scrollTop = messageLog.scrollHeight;
        }
    });

    tab.addEventListener("keydown", (event: KeyboardEvent) => {
        if (event.key === "Enter" || event.key === " ") {
            event.preventDefault();
            tab.click();
        }
    });
}

export async function pauseStepgen(): Promise<void> {
    await fetch("./pause/pause", {
        method: "POST"
    }).then((response) => {
        if (!response.ok) {
            response.text().then((error) => {
                const message = `Failed to pause:\n${response.statusText}\n${error}`;
                console.error(message);
                alert(message);
            });
        }
    }).catch((error) => {
        const message = `Failed to pause:\n${error}\n${error.stack}`;
        console.error(message);
        alert(message);
    });
}

export async function resumeStepgen(): Promise<void> {
    await fetch("./pause/resume", {
        method: "POST"
    }).then((response) => {
        if (!response.ok) {
            response.text().then((error) => {
                const message = `Failed to resume:\n${response.statusText}\n${error}`;
                console.error(message);
                alert(message);
            });
        }
    }).catch((error) => {
        const message = `Failed to resume:\n${error}\n${error.stack}`;
        console.error(message);
        alert(message);
    });
}

export function showReloadModal(): void {
    const reloadServerDialog = document.getElementById("reloadServerDialog") as HTMLDialogElement;
    reloadServerDialog.showModal();
}

async function reloadServer(): Promise<void> {
    const reloadServerDialog = document.getElementById("reloadServerDialog") as HTMLDialogElement;
    reloadServerDialog.close();

    await fetch("./reload-server", {
        method: "POST"
    }).then((response) => {
        if (!response.ok) {
            response.text().then((error) => {
                const message = `Failed to reload server:\n${response.statusText}\n${error}`;
                console.error(message);
                alert(message);
            });
        } else {
            const reloadingSoonDialog = document.getElementById("reloadingSoonDialog") as HTMLDialogElement;
            reloadingSoonDialog.showModal();
        }
    }).catch((error) => {
        const message = `Failed to reload server:\n${error}\n${error.stack}`;
        console.error(message);
        alert(message);
    });
}

async function setupPruntDisabledWarning(): Promise<void> {
    const response = await fetch("./prunt-is-enabled");

    if (!response.ok) {
        const message = `Failed to check if Prunt is enabled:\n${response.statusText}\n${await response.text()}`;
        console.error(message);
        throw new Error(message);
    } else if (await response.json() === false) {
        document.getElementById("pruntDisabledWarning").classList.remove("hidden");
    }
}

export async function allowFirmwareUpdate(): Promise<void> {
    const firmwareUpdateDialog = document.getElementById("firmwareUpdateDialog") as HTMLDialogElement;

    await fetch("./allow-firmware-update", {
        method: "POST"
    }).then((response) => {
        if (!response.ok) {
            response.text().then((error) => {
                const message = `Failed to allow firmware update:\n${response.statusText}\n${error}`;
                console.error(message);
                alert(message);
            });
        } else {
            firmwareUpdateDialog.close();
        }
    }).catch((error) => {
        const message = `Failed to allow firmware update:\n${error}\n${error.stack}`;
        console.error(message);
        alert(message);
    });
}

const reloadServerDialog = document.getElementById("reloadServerDialog") as HTMLDialogElement;
const confirmReloadButton = document.getElementById("confirmReloadButton") as HTMLButtonElement;
const cancelReloadButton = document.getElementById("cancelReloadButton") as HTMLButtonElement;

confirmReloadButton.addEventListener("click", reloadServer);
cancelReloadButton.addEventListener("click", () => {
    reloadServerDialog.close();
});

function checkServerReloadFlag(): void {
    const reloadInfoString = localStorage.getItem("serverReloadTriggered");
    localStorage.removeItem("serverReloadTriggered");

    if (reloadInfoString) {
        try {
            const reloadInfo = JSON.parse(reloadInfoString);
            const now = Date.now();

            if (reloadInfo && reloadInfo.magic === "e339 9f9a 0b42 07fe c2d2 b84b b189 e851"
                && typeof reloadInfo.timestamp === "number") {
                if (now - reloadInfo.timestamp < 10000) {
                    const serverRestartedDialog = document.getElementById("serverRestartedDialog") as HTMLDialogElement;
                    serverRestartedDialog.showModal();
                } else {
                    console.warn("Ignoring stale server reload flag.");
                }
            } else {
                console.warn("Ignoring malformed server reload flag in localStorage.");
            }
        } catch (e) {
            console.error("Failed to parse server reload flag from localStorage:", e);
        }
    }
}

async function checkUpdate(): Promise<void> {
    const updateCheckFailedWarning = document.getElementById("updateCheckFailedWarning") as HTMLDivElement;
    const updateAvailableWarning = document.getElementById("updateAvailableWarning") as HTMLDivElement;
    const updateAvailableLink = document.getElementById("updateAvailableLink") as HTMLAnchorElement;

    try {
        const response = await fetch("./update_check");
        if (!response.ok) {
            console.error("Error during update check:", response.text());
            updateAvailableWarning.classList.add("hidden");
            updateCheckFailedWarning.classList.remove("hidden");
            return;
        }
        const data = await response.json();
        if (data.Available) {
            updateAvailableLink.href = data.URL;
            updateAvailableLink.textContent = data.URL;
            updateAvailableWarning.classList.remove("hidden");
        } else {
            updateAvailableWarning.classList.add("hidden");
        }
    } catch (error) {
        console.error("Error during update check:", error);
        updateAvailableWarning.classList.add("hidden");
        updateCheckFailedWarning.classList.remove("hidden");
    }
}

const mainBody = document.getElementById("mainBody");
Promise.all([setupStatus(), setupSettings(), refreshFiles(), setupPruntDisabledWarning()]).then(() => {
    mainBody.classList.remove("hidden");
    checkServerReloadFlag();
}).catch((error) => {
    alert("Error occurred during loading.");
    mainBody.innerText = error + "\n\n" + error.stack;
    mainBody.classList.remove("hidden");
});

checkUpdate();
setInterval(checkUpdate, 3600 * 1000);
