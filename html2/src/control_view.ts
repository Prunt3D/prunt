import { runCommand, listUploads, uploadFile, runFile, restartServer, allowFirmwareUpdate } from './api';

export function initControlView() {
    setupCommandInput();
    setupFileInput();
    setupServerActions();
    refreshFileList();
}

function setupCommandInput() {
    const btn = document.getElementById('btn-send-command');
    const input = document.getElementById('command-input') as HTMLInputElement;

    btn?.addEventListener('click', async () => {
        const cmd = input.value.trim();
        if (cmd) {
            try {
                await runCommand(cmd);
                input.value = ''; // clear upon success
            } catch (e) {
                console.error(e);
                alert("Failed to send command.");
            }
        }
    });

    input?.addEventListener('keyup', (e) => {
        if (e.key === 'Enter') btn?.click();
    });
}

function setupFileInput() {
    const dropZone = document.getElementById('drop-zone');
    const fileInput = document.getElementById('file-input') as HTMLInputElement;

    dropZone?.addEventListener('click', () => fileInput.click());

    dropZone?.addEventListener('dragover', (e) => {
        e.preventDefault();
        dropZone.classList.add('dragover');
    });

    dropZone?.addEventListener('dragleave', () => {
        dropZone.classList.remove('dragover');
    });

    dropZone?.addEventListener('drop', (e) => {
        e.preventDefault();
        dropZone.classList.remove('dragover');
        if (e.dataTransfer && e.dataTransfer.files.length > 0) {
            handleUpload(e.dataTransfer.files[0]);
        }
    });

    fileInput?.addEventListener('change', () => {
        if (fileInput.files && fileInput.files.length > 0) {
            handleUpload(fileInput.files[0]);
        }
    });
}

async function handleUpload(file: File) {
    try {
        await uploadFile(file);
        // Refresh file list after upload
        await refreshFileList();
        alert("File uploaded successfully.");
    } catch (e) {
        console.error(e);
        alert("Failed to upload file");
    }
}

async function refreshFileList() {
    const fileListEl = document.getElementById('file-list');
    if (!fileListEl) return;

    fileListEl.innerHTML = '<li>Loading...</li>';
    try {
        const files = await listUploads();
        fileListEl.innerHTML = '';

        if (files.length === 0) {
            fileListEl.innerHTML = '<li>No files uploaded yet.</li>';
            return;
        }

        files.forEach(filename => {
            const li = document.createElement('li');
            li.innerHTML = `<span>${filename}</span>
                <div class="file-actions">
                    <button class="btn btn-sm btn-primary">Run</button>
                    <a href="/uploads/${filename}" class="btn btn-sm btn-secondary" target="_blank">Download</a>
                </div>`;

            const btnRun = li.querySelector('.btn-primary');
            btnRun?.addEventListener('click', async () => {
                try {
                    await runFile(filename);
                    alert(`Now running ${filename}`);
                } catch (e) {
                    console.error(e);
                    alert("Failed to run file");
                }
            });

            fileListEl.appendChild(li);
        });
    } catch (e) {
        console.error(e);
        fileListEl.innerHTML = '<li>Error loading file list.</li>';
    }
}

function setupServerActions() {
    const btnRestart = document.getElementById('btn-restart-server');
    const btnFirmware = document.getElementById('btn-firmware-update');

    btnRestart?.addEventListener('click', async () => {
        if (confirm("Are you sure you want to restart the server?")) {
            try {
                await restartServer();
                alert("Server restart command sent.");
            } catch (e) {
                console.error(e);
                alert("Failed to restart server.");
            }
        }
    });

    btnFirmware?.addEventListener('click', async () => {
        try {
            await allowFirmwareUpdate();
            alert("Firmware update allowed.");
        } catch (e) {
            console.error(e);
            alert("Failed to allow firmware update.");
        }
    });
}
