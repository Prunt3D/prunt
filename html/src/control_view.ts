import { listUploads, uploadFile, runFile, restartServer, allowFirmwareUpdate } from './api.js';
import { onLocaleChange, t } from './localization.js';

export function initControlView() {
    setupFileInput();
    setupServerActions();
    onLocaleChange(() => {
        void refreshFileList();
    });
    refreshFileList();
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
        await refreshFileList();
        alert(t('ui.control.fileUploaded', 'File uploaded successfully.'));
    } catch (e) {
        console.error(e);
        alert(t('ui.control.fileUploadFailed', 'Failed to upload file'));
    }
}

async function refreshFileList() {
    const fileListEl = document.getElementById('file-list');
    if (!fileListEl) return;

    fileListEl.replaceChildren(createMessageItem(t('ui.control.loadingFiles', 'Loading...')));
    try {
        const files = await listUploads();
        fileListEl.replaceChildren();

        if (files.length === 0) {
            fileListEl.appendChild(createMessageItem(t('ui.control.noFiles', 'No files uploaded yet.')));
            return;
        }

        files.forEach(filename => {
            const li = document.createElement('li');
            const name = document.createElement('span');
            name.innerText = filename;
            li.appendChild(name);

            const actions = document.createElement('div');
            actions.className = 'file-actions';

            const btnRun = document.createElement('button');
            btnRun.className = 'btn btn-sm btn-primary';
            btnRun.innerText = t('ui.control.run', 'Run');
            actions.appendChild(btnRun);

            const download = document.createElement('a');
            download.href = `/uploads/${encodeURIComponent(filename)}`;
            download.className = 'btn btn-sm btn-secondary';
            download.target = '_blank';
            download.rel = 'noopener noreferrer';
            download.innerText = t('ui.control.download', 'Download');
            actions.appendChild(download);

            li.appendChild(actions);

            btnRun.addEventListener('click', async () => {
                try {
                    await runFile(filename);
                    alert(t('ui.control.runningFile', 'Now running {filename}', { filename }));
                } catch (e) {
                    console.error(e);
                    alert(t('ui.control.runFailed', 'Failed to run file'));
                }
            });

            fileListEl.appendChild(li);
        });
    } catch (e) {
        console.error(e);
        fileListEl.replaceChildren(createMessageItem(t('ui.control.fileListError', 'Error loading file list.')));
    }
}

function setupServerActions() {
    const btnRestart = document.getElementById('btn-restart-server');
    const btnFirmware = document.getElementById('btn-firmware-update');

    btnRestart?.addEventListener('click', async () => {
        if (confirm(t('ui.control.restartConfirm', 'Are you sure you want to restart the server?'))) {
            try {
                await restartServer();
                alert(t('ui.control.restartSent', 'Server restart command sent.'));
            } catch (e) {
                console.error(e);
                alert(t('ui.control.restartFailed', 'Failed to restart server.'));
            }
        }
    });

    btnFirmware?.addEventListener('click', async () => {
        try {
            await allowFirmwareUpdate();
            alert(t('ui.control.firmwareAllowed', 'Firmware update allowed.'));
        } catch (e) {
            console.error(e);
            alert(t('ui.control.firmwareFailed', 'Failed to allow firmware update.'));
        }
    });
}

function createMessageItem(message: string): HTMLLIElement {
    const item = document.createElement('li');
    item.innerText = message;
    return item;
}
