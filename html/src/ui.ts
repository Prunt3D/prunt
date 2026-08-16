import { ServerException, StartupState, wsClient } from './ws.js';
import { allowFirmwareUpdate, pauseMachine, resumeMachine } from './api.js';
import { initConfigView } from './config_view.js';
import { initStatusView } from './status_view.js';
import { initControlView } from './control_view.js';
import { initGcodeEntryView } from './gcode_entry_view.js';
import { initGcodeExplorerView } from './gcode_explorer_view.js';
import { initLogView } from './log_view.js';
import { initLocalization, onLocaleChange, t } from './localization.js';
import { activateView } from './navigation.js';

export async function initUI() {
    await initLocalization();

    setupNavigation();
    setupGlobalControls();
    setupUpdatePrompt();

    // Initialize individual views
    initConfigView();
    initStatusView();
    initControlView();
    initGcodeEntryView();
    initGcodeExplorerView();
    initLogView();

    setupTheme();

    // Connect WebSocket
    wsClient.on('connected', () => {
        const statusEl = document.getElementById('connection-status');
        if (statusEl) {
            statusEl.className = 'status-indicator connected';
            statusEl.innerText = t('ui.connection.connected', 'Connected');
        }
    });

    wsClient.on('disconnected', () => {
        const statusEl = document.getElementById('connection-status');
        if (statusEl) {
            statusEl.className = 'status-indicator disconnected';
            statusEl.innerText = t('ui.connection.disconnected', 'Disconnected');
        }
    });

    wsClient.on('restarted', () => {
        // Reload page if server restarted
        window.location.reload();
    });

    wsClient.on('serverException', (serverException: ServerException | null) => {
        updateServerExceptionBanner(serverException);
    });

    wsClient.connect();
}

function setupUpdatePrompt() {
    const dialog = document.getElementById('firmware-update-dialog') as HTMLDialogElement | null;
    const title = document.getElementById('firmware-update-title');
    const message = document.getElementById('firmware-update-message');
    const allowButton = document.getElementById('btn-allow-firmware-update') as HTMLButtonElement | null;
    if (!dialog || !title || !message || !allowButton) return;

    let startupState: StartupState = 'Waiting';

    const render = () => {
        switch (startupState) {
            case 'Update_Required':
                title.textContent = t('ui.update.requiredTitle', 'Firmware update required');
                message.textContent = t(
                    'ui.update.requiredMessage',
                    'The controller firmware must be updated before Prunt can start. Confirm when you are ready to begin.'
                );
                allowButton.textContent = t('ui.update.allow', 'Update firmware');
                allowButton.hidden = false;
                allowButton.disabled = false;
                if (!dialog.open) dialog.showModal();
                break;

            case 'Update_Running':
                title.textContent = t('ui.update.runningTitle', 'Updating firmware');
                message.textContent = t(
                    'ui.update.runningMessage',
                    'The firmware update is in progress.'
                );
                allowButton.hidden = true;
                allowButton.disabled = true;
                if (!dialog.open) dialog.showModal();
                break;

            case 'Done':
            case 'Waiting':
                if (dialog.open) dialog.close();
                break;
        }
    };

    dialog.addEventListener('cancel', event => event.preventDefault());
    allowButton.addEventListener('click', async () => {
        allowButton.disabled = true;
        try {
            await allowFirmwareUpdate();
            startupState = 'Update_Running';
            render();
        } catch (error) {
            console.error(error);
            allowButton.disabled = false;
            alert(t('ui.update.allowFailed', 'Failed to start the firmware update.'));
        }
    });

    wsClient.on('startup', (state: StartupState) => {
        if (!['Done', 'Update_Running', 'Update_Required', 'Waiting'].includes(state)) return;
        startupState = state;
        render();
    });
    onLocaleChange(render);
}

function updateServerExceptionBanner(serverException: ServerException | null) {
    const banner = document.getElementById('server-exception-banner');
    const title = document.getElementById('server-exception-title');
    const message = document.getElementById('server-exception-message');
    if (!banner || !title || !message) return;

    if (!serverException || typeof serverException.Message !== 'string' || serverException.Message.length === 0) {
        banner.hidden = true;
        message.textContent = '';
        return;
    }

    const isFatal = serverException.Fatal === true;
    banner.classList.toggle('fatal', isFatal);
    title.textContent = isFatal
        ? t('ui.serverException.fatalTitle', 'Fatal server error')
        : t('ui.serverException.title', 'Server error');
    message.textContent = serverException.Message;
    banner.hidden = false;
}

function setupNavigation() {
    const navItems = document.querySelectorAll('.nav-item');

    navItems.forEach(item => {
        item.addEventListener('click', () => {
            const targetId = item.getAttribute('data-target');
            if (targetId) {
                activateView(targetId);
            }
        });
    });
}

function setupGlobalControls() {
    const btnPause = document.getElementById('btn-pause');
    const btnResume = document.getElementById('btn-resume');

    btnPause?.addEventListener('click', async () => {
        try {
            await pauseMachine();
            console.log("Machine paused");
        } catch (e) {
            console.error(e);
            alert(t('ui.global.pauseFailed', 'Failed to pause machine.'));
        }
    });

    btnResume?.addEventListener('click', async () => {
        try {
            await resumeMachine();
            console.log("Machine resumed");
        } catch (e) {
            console.error(e);
            alert(t('ui.global.resumeFailed', 'Failed to resume machine.'));
        }
    });
}

function setupTheme() {
    const themeSelect = document.getElementById('theme-select') as HTMLSelectElement | null;
    if (!themeSelect) return;

    const savedTheme = localStorage.getItem('prunt-theme') || 'auto';
    themeSelect.value = savedTheme;

    const applyTheme = (theme: string) => {
        if (theme === 'auto') {
            const prefersLight = window.matchMedia('(prefers-color-scheme: light)').matches;
            document.documentElement.setAttribute('data-theme', prefersLight ? 'light' : 'dark');
        } else {
            document.documentElement.setAttribute('data-theme', theme);
        }
    };

    applyTheme(savedTheme);

    themeSelect.addEventListener('change', () => {
        const theme = themeSelect.value;
        localStorage.setItem('prunt-theme', theme);
        applyTheme(theme);
    });

    window.matchMedia('(prefers-color-scheme: light)').addEventListener('change', () => {
        if (localStorage.getItem('prunt-theme') === 'auto' || !localStorage.getItem('prunt-theme')) {
            applyTheme('auto');
        }
    });
}
