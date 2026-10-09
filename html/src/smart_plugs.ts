import { switchSmartPlug } from './api.js';
import { onLocaleChange, t } from './localization.js';
import { SmartPlugStatus, wsClient } from './ws.js';

export function initSmartPlug() {
    const section = document.getElementById('smart-plug');
    const state = document.getElementById('smart-plug-state');
    const error = document.getElementById('smart-plug-error');
    const on = document.getElementById('btn-smart-plug-on') as HTMLButtonElement | null;
    const off = document.getElementById('btn-smart-plug-off') as HTMLButtonElement | null;
    if (!section || !state || !error || !on || !off) return;

    let status: SmartPlugStatus | undefined;
    let connected = false;
    let pending = false;

    const render = () => {
        section.hidden = !connected || !status?.Enabled;
        on.disabled = !connected || pending || !status?.Enabled || status.Busy || status.Watchdog_Active;
        off.disabled = !connected || pending || !status?.Enabled;
        state.textContent = pending || status?.Busy ? t('ui.smartPlugs.switching', 'Switching…')
            : status?.Error ? status.Error
            : status?.Power === 'ON' && status.Watchdog_Active
                ? t('ui.smartPlugs.armed', 'On · watchdog active')
                : status?.Power === 'ON' ? t('ui.smartPlugs.unarmed', 'On · watchdog inactive')
                : status?.Power === 'OFF' ? t('ui.smartPlugs.off', 'Off')
                : t('ui.smartPlugs.unknown', 'Unknown');
        state.title = status ? `${status.Provider}: ${status.Host} · ${status.Watchdog_Seconds}s` : '';
    };

    const switchPlug = async (enabled: boolean) => {
        if (!connected || pending || !status?.Enabled) return;
        pending = true;
        error.hidden = true;
        render();
        try {
            await switchSmartPlug(enabled);
        } catch (cause) {
            error.textContent = `${t('ui.smartPlugs.switchFailed', 'Unable to switch smart plug')}: ${String(cause)}`;
            error.hidden = false;
        } finally {
            pending = false;
            render();
        }
    };

    on.addEventListener('click', () => { void switchPlug(true); });
    off.addEventListener('click', () => { void switchPlug(false); });
    wsClient.on('connected', () => {
        connected = true;
        render();
    });
    wsClient.on('tick', (message: { Smart_Plug?: SmartPlugStatus }) => {
        if (!connected) return;
        status = message.Smart_Plug;
        render();
    });
    wsClient.on('disconnected', () => {
        connected = false;
        status = undefined;
        error.hidden = true;
        render();
    });
    onLocaleChange(render);
    render();
}
