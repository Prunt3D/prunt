import { onLocaleChange, t } from './localization.js';
import { wsClient } from './ws.js';

const MAX_LOG_ENTRIES = 1000;
const AUTO_SCROLL_THRESHOLD_PX = 24;

function isScrolledToBottom(output: HTMLElement): boolean {
    return output.scrollHeight - output.scrollTop - output.clientHeight <= AUTO_SCROLL_THRESHOLD_PX;
}

function showEmptyState(output: HTMLElement) {
    const emptyState = document.createElement('div');
    emptyState.className = 'log-output-empty';
    emptyState.textContent = t('ui.log.empty', 'Log messages will appear here.');
    output.replaceChildren(emptyState);
}

function updateUnreadIndicator(indicator: HTMLElement, unreadCount: number) {
    if (unreadCount === 0) {
        indicator.hidden = true;
        indicator.textContent = '';
        indicator.removeAttribute('aria-label');
        indicator.removeAttribute('title');
        return;
    }

    const label = unreadCount === 1
        ? t('ui.log.unreadSingle', '1 unread log entry')
        : t('ui.log.unreadMultiple', '{count} unread log entries', { count: unreadCount });

    indicator.hidden = false;
    indicator.textContent = unreadCount > 99 ? '99+' : String(unreadCount);
    indicator.setAttribute('aria-label', label);
    indicator.setAttribute('title', label);
}

function appendLogMessage(output: HTMLElement, message: string, followOutput: boolean) {
    output.querySelector('.log-output-empty')?.remove();

    const entry = document.createElement('div');
    entry.className = 'log-output-entry';
    entry.textContent = message;
    output.appendChild(entry);

    while (output.childElementCount > MAX_LOG_ENTRIES) {
        output.firstElementChild?.remove();
    }

    if (followOutput) {
        output.scrollTop = output.scrollHeight;
    }
}

export function initLogView() {
    const output = document.getElementById('log-output');
    const clearButton = document.getElementById('btn-clear-log');
    const logView = document.getElementById('log-view');
    const unreadIndicator = document.getElementById('log-unread-count');
    if (!output || !clearButton || !logView || !unreadIndicator) return;

    let followOutput = true;
    let unreadCount = 0;
    showEmptyState(output);

    const markReadIfVisible = () => {
        if (logView.classList.contains('active') && isScrolledToBottom(output)) {
            unreadCount = 0;
            updateUnreadIndicator(unreadIndicator, unreadCount);
        }
    };

    output.addEventListener('scroll', () => {
        if (output.clientHeight > 0) {
            followOutput = isScrolledToBottom(output);
            if (followOutput) {
                markReadIfVisible();
            }
        }
    });

    clearButton.addEventListener('click', () => {
        followOutput = true;
        unreadCount = 0;
        updateUnreadIndicator(unreadIndicator, unreadCount);
        showEmptyState(output);
    });

    new MutationObserver(() => {
        if (logView.classList.contains('active') && followOutput) {
            requestAnimationFrame(() => {
                output.scrollTop = output.scrollHeight;
                markReadIfVisible();
            });
        }
    }).observe(logView, { attributes: true, attributeFilter: ['class'] });

    onLocaleChange(() => {
        updateUnreadIndicator(unreadIndicator, unreadCount);
        const emptyState = output.querySelector<HTMLElement>('.log-output-empty');
        if (emptyState) {
            emptyState.textContent = t('ui.log.empty', 'Log messages will appear here.');
        }
    });

    wsClient.on('log', (message: unknown) => {
        if (typeof message === 'string') {
            const messageIsVisible = logView.classList.contains('active') && followOutput;
            appendLogMessage(output, message, followOutput);
            if (!messageIsVisible) {
                unreadCount = Math.min(Number.MAX_SAFE_INTEGER, unreadCount + 1);
                updateUnreadIndicator(unreadIndicator, unreadCount);
            }
        }
    });
}
