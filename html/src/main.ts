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
                alert(`File upload failed:\n${xhr.statusText}\n${xhr.responseText}`);
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
                alert(`File refresh failed:\n${response.statusText}\n${error}`);
            });
        } else {
            response.json().then((files: string[]) => {
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
            alert(`Failed to run file:\n${response.statusText}\n${error}`);
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

const mainBody = document.getElementById("mainBody");
Promise.all([setupStatus(), setupSettings(), refreshFiles()]).then(() => {
    mainBody.classList.remove("hidden");
}).catch((error) => {
    alert("Error occurred during loading.");
    mainBody.innerText = error + "\n\n" + error.stack;
    mainBody.classList.remove("hidden");
});
