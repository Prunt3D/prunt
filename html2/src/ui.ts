import { wsClient } from './ws';
import { pauseMachine, resumeMachine } from './api';
import { initConfigView } from './config_view';
import { initStatusView } from './status_view';
import { initControlView } from './control_view';

export function initUI() {
    setupNavigation();
    setupGlobalControls();

    // Initialize individual views
    initConfigView();
    initStatusView();
    initControlView();

    // Connect WebSocket
    wsClient.on('connected', () => {
        const statusEl = document.getElementById('connection-status');
        if (statusEl) {
            statusEl.className = 'status-indicator connected';
            statusEl.innerText = 'Connected';
        }
    });

    wsClient.on('disconnected', () => {
        const statusEl = document.getElementById('connection-status');
        if (statusEl) {
            statusEl.className = 'status-indicator disconnected';
            statusEl.innerText = 'Disconnected';
        }
    });

    wsClient.on('restarted', () => {
        // Reload page if server restarted
        window.location.reload();
    });

    wsClient.connect();
}

function setupNavigation() {
    const navItems = document.querySelectorAll('.nav-item');
    const views = document.querySelectorAll('.view');

    navItems.forEach(item => {
        item.addEventListener('click', () => {
            // Remove active class from all
            navItems.forEach(n => n.classList.remove('active'));
            views.forEach(v => v.classList.remove('active'));

            // Add active class to clicked
            item.classList.add('active');
            const targetId = item.getAttribute('data-target');
            if (targetId) {
                document.getElementById(targetId)?.classList.add('active');
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
            alert("Failed to pause machine.");
        }
    });

    btnResume?.addEventListener('click', async () => {
        try {
            await resumeMachine();
            console.log("Machine resumed");
        } catch (e) {
            console.error(e);
            alert("Failed to resume machine.");
        }
    });
}
