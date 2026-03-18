function normalizeSegment(segment: string): string {
    return segment
        .trim()
        .toLowerCase()
        .replace(/[\s-]+/g, '_')
        .replace(/[^a-z0-9_]+/g, '');
}

function normalizePath(path: string[]): string[] {
    return path.map(normalizeSegment).filter(segment => segment.length > 0);
}

function splitTarget(target: string): string[] {
    return target.split('.').map(normalizeSegment).filter(segment => segment.length > 0);
}

function parseJsonPath(value: string | undefined): string[] | null {
    if (!value) return null;

    try {
        const parsed = JSON.parse(value);
        return Array.isArray(parsed) ? parsed : null;
    } catch {
        return null;
    }
}

function isPrefix(prefix: string[], full: string[]): boolean {
    if (prefix.length > full.length) return false;

    for (let index = 0; index < prefix.length; index++) {
        if (prefix[index] !== full[index]) return false;
    }

    return true;
}

function activateTargetHighlight(target: HTMLElement) {
    document.querySelectorAll('.link-target-highlight').forEach(element => {
        element.classList.remove('link-target-highlight');
    });

    target.classList.add('link-target-highlight');
    window.setTimeout(() => {
        target.classList.remove('link-target-highlight');
    }, 2000);
}

function revealTarget(target: HTMLElement) {
    activateTargetHighlight(target);
    target.scrollIntoView({ behavior: 'smooth', block: 'center', inline: 'nearest' });
}

function activateConfigTabs(target: HTMLElement) {
    const ancestorTabPaths: string[] = [];
    let current: HTMLElement | null = target;

    while (current) {
        if (current.classList.contains('tab-pane') && current.dataset.tabPath) {
            ancestorTabPaths.push(current.dataset.tabPath);
        }
        current = current.parentElement;
    }

    ancestorTabPaths
        .map(path => ({ encoded: path, parsed: parseJsonPath(path) }))
        .filter((entry): entry is { encoded: string; parsed: string[] } => entry.parsed !== null)
        .sort((left, right) => left.parsed.length - right.parsed.length)
        .forEach(entry => {
            const button = document.querySelector<HTMLElement>(`.config-tab[data-tab-path='${CSS.escape(entry.encoded)}']`);
            button?.click();
        });
}

function getConfigPathVariants(path: string[]): string[][] {
    const full = normalizePath(path);
    const variants = [full];

    if (path.length >= 3 && path[0] === 'Config' && path[2] === 'Config') {
        variants.push(normalizePath([path[1], ...path.slice(3)]));
    }

    return variants.filter(variant => variant.length > 0);
}

function findConfigTarget(targetPath: string[]): HTMLElement | null {
    let bestExact: HTMLElement | null = null;
    let bestPrefix: HTMLElement | null = null;
    let bestPrefixLength = Number.POSITIVE_INFINITY;

    document.querySelectorAll<HTMLElement>('.form-group[data-path]').forEach(element => {
        const rawPath = parseJsonPath(element.dataset.path);
        if (!rawPath) return;

        for (const variant of getConfigPathVariants(rawPath)) {
            if (variant.length === targetPath.length && isPrefix(targetPath, variant)) {
                bestExact = element;
                return;
            }

            if (isPrefix(targetPath, variant) && variant.length < bestPrefixLength) {
                bestPrefix = element;
                bestPrefixLength = variant.length;
            }
        }
    });

    return bestExact || bestPrefix;
}

function findGcodeTarget(target: string): HTMLElement | null {
    const normalizedTarget = normalizeSegment(target.replace(/\./g, '_'));
    return document.querySelector<HTMLElement>(`[data-gcode-target='${CSS.escape(normalizedTarget)}']`);
}

function retryNavigation(navigate: () => boolean, attempts = 20) {
    if (navigate()) return;
    if (attempts <= 0) return;

    window.setTimeout(() => retryNavigation(navigate, attempts - 1), 100);
}

export function activateView(targetId: string) {
    const navItems = document.querySelectorAll<HTMLElement>('.nav-item');
    const views = document.querySelectorAll<HTMLElement>('.view');

    navItems.forEach(item => item.classList.remove('active'));
    views.forEach(view => view.classList.remove('active'));

    const navItem = document.querySelector<HTMLElement>(`.nav-item[data-target='${CSS.escape(targetId)}']`);
    navItem?.classList.add('active');
    document.getElementById(targetId)?.classList.add('active');
}

export function navigateToConfigTarget(target: string) {
    const targetPath = splitTarget(target);
    if (targetPath.length === 0) return;

    activateView('config-view');
    retryNavigation(() => {
        const element = findConfigTarget(targetPath);
        if (!element) return false;

        activateConfigTabs(element);
        revealTarget(element);
        return true;
    });
}

export function navigateToGcodeTarget(target: string) {
    if (!target.trim()) return;

    activateView('gcode-explorer-view');
    retryNavigation(() => {
        const element = findGcodeTarget(target);
        if (!element) return false;

        revealTarget(element);
        return true;
    });
}
