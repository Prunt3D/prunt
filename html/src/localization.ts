type LocaleManifestEntry = {
    code: string;
    label: string;
};

type LocaleManifest = {
    defaultLocale?: string;
    locales?: LocaleManifestEntry[];
};

type TranslationBundle = Record<string, string>;

const LOCALE_STORAGE_KEY = 'prunt-locale';
const DEFAULT_LOCALE = 'en';
const DEFAULT_MANIFEST: LocaleManifest = {
    defaultLocale: DEFAULT_LOCALE,
    locales: [{ code: DEFAULT_LOCALE, label: 'English' }]
};

let localeManifest: LocaleManifest = DEFAULT_MANIFEST;
let translations: TranslationBundle = {};
let localePreference = 'auto';
let activeLocale = DEFAULT_LOCALE;
const localeListeners = new Set<() => void>();

function normalizeLocale(locale: string): string {
    return locale.trim().replace(/_/g, '-').toLowerCase();
}

function escapeKeySegment(segment: string): string {
    return encodeURIComponent(segment).replace(/\./g, '%2E');
}

function buildPathKey(prefix: string, path: string[] = [], suffix: string): string {
    const escapedPath = path.map(escapeKeySegment).join('.');
    return escapedPath.length > 0 ? `${prefix}.${escapedPath}.${suffix}` : `${prefix}.${suffix}`;
}

function interpolate(template: string, params?: Record<string, string | number>): string {
    if (!params) return template;
    return template.replace(/\{(\w+)\}/g, (_, key: string) => {
        const value = params[key];
        return value === undefined ? '' : String(value);
    });
}

function getStoredLocalePreference(): string {
    try {
        return localStorage.getItem(LOCALE_STORAGE_KEY) || 'auto';
    } catch {
        return 'auto';
    }
}

function setStoredLocalePreference(locale: string) {
    try {
        localStorage.setItem(LOCALE_STORAGE_KEY, locale);
    } catch {
        // Ignore storage failures.
    }
}

function getManifestLocales(): LocaleManifestEntry[] {
    if (localeManifest.locales && localeManifest.locales.length > 0) {
        return localeManifest.locales;
    }
    return DEFAULT_MANIFEST.locales || [];
}

function getLocaleCandidates(locale: string): string[] {
    const normalized = normalizeLocale(locale);
    if (!normalized) return [];

    const parts = normalized.split('-');
    const candidates: string[] = [];
    for (let i = parts.length; i > 0; i--) {
        candidates.push(parts.slice(0, i).join('-'));
    }
    return Array.from(new Set(candidates));
}

function pickSupportedLocale(requestedLocales: string[], fallbackLocale: string): string {
    const available = new Set(getManifestLocales().map(locale => normalizeLocale(locale.code)));

    for (const requestedLocale of requestedLocales) {
        for (const candidate of getLocaleCandidates(requestedLocale)) {
            if (available.has(candidate)) {
                return candidate;
            }
        }
    }

    const normalizedFallback = normalizeLocale(fallbackLocale);
    if (available.has(normalizedFallback)) {
        return normalizedFallback;
    }

    return normalizeLocale(DEFAULT_LOCALE);
}

function getRequestedLocales(): string[] {
    if (localePreference !== 'auto') {
        return [localePreference];
    }

    if (typeof navigator === 'undefined') {
        return [DEFAULT_LOCALE];
    }

    const browserLocales = navigator.languages && navigator.languages.length > 0
        ? navigator.languages
        : [navigator.language];

    return browserLocales.filter(Boolean);
}

async function fetchJson<T>(path: string): Promise<T | null> {
    try {
        const response = await fetch(path, { cache: 'no-cache' });
        if (!response.ok) return null;
        return await response.json() as T;
    } catch {
        return null;
    }
}

async function loadManifest(): Promise<void> {
    const manifest = await fetchJson<LocaleManifest>('locales/index.json');
    if (!manifest) {
        localeManifest = DEFAULT_MANIFEST;
        return;
    }

    localeManifest = {
        defaultLocale: manifest.defaultLocale || DEFAULT_LOCALE,
        locales: manifest.locales && manifest.locales.length > 0 ? manifest.locales : DEFAULT_MANIFEST.locales
    };
}

async function loadBundle(locale: string): Promise<TranslationBundle> {
    const normalizedLocale = normalizeLocale(locale);
    const bundle = await fetchJson<TranslationBundle>(`locales/${normalizedLocale}.json`);
    return bundle || {};
}

function updateLocaleSelect() {
    const select = document.getElementById('locale-select') as HTMLSelectElement | null;
    if (!select) return;

    if (!select.dataset.localeBound) {
        select.dataset.localeBound = 'true';
        select.addEventListener('change', () => {
            void setLocalePreference(select.value);
        });
    }

    select.innerHTML = '';

    const autoOption = document.createElement('option');
    autoOption.value = 'auto';
    autoOption.textContent = t('ui.language.auto', 'Auto');
    select.appendChild(autoOption);

    getManifestLocales().forEach(locale => {
        const option = document.createElement('option');
        option.value = normalizeLocale(locale.code);
        option.textContent = locale.label;
        select.appendChild(option);
    });

    select.value = localePreference;
}

export function t(key: string, fallback: string, params?: Record<string, string | number>): string {
    return interpolate(translations[key] || fallback, params);
}

function humanizeUnderscoreFallback(fallback: string): string {
    return fallback.replace(/_/g, ' ');
}

export function translateSchemaKind(kind: string): string {
    return t(`schema.kind.${escapeKeySegment(kind)}`, humanizeUnderscoreFallback(kind));
}

export function translateConfigLabel(path: string[] | undefined, fallback: string): string {
    return t(buildPathKey('config.path', path, 'label'), humanizeUnderscoreFallback(fallback));
}

export function translateConfigDescription(path: string[] | undefined, fallback: string): string {
    return t(buildPathKey('config.path', path, 'description'), fallback);
}

export function translateConfigUnit(path: string[] | undefined, fallback: string): string {
    return t(buildPathKey('config.path', path, 'unit'), fallback);
}

export function translateConfigOption(path: string[] | undefined, option: string, fallback: string): string {
    return t(buildPathKey('config.path', [...(path || []), 'options', option], 'label'), humanizeUnderscoreFallback(fallback));
}

export function translateStatusModuleLabel(moduleName: string, fallback: string): string {
    return t(buildPathKey('status.path', [moduleName], 'label'), humanizeUnderscoreFallback(fallback));
}

export function translateStatusGroupLabel(moduleName: string, groupName: string, fallback: string): string {
    return t(buildPathKey('status.path', [moduleName, groupName], 'label'), humanizeUnderscoreFallback(fallback));
}

export function translateStatusValueLabel(moduleName: string, groupName: string, valueName: string, fallback: string): string {
    return t(buildPathKey('status.path', [moduleName, groupName, valueName], 'label'), humanizeUnderscoreFallback(fallback));
}

export function translateStatusUnit(moduleName: string, groupName: string, valueName: string, fallback: string): string {
    return t(buildPathKey('status.path', [moduleName, groupName, valueName], 'unit'), fallback);
}

export function translateGcodeModuleLabel(moduleName: string, fallback: string): string {
    return t(buildPathKey('gcode.module', [moduleName], 'label'), humanizeUnderscoreFallback(fallback));
}

export function translateGcodeCommandName(identifier: string, fallback: string): string {
    return t(buildPathKey('gcode.command', [identifier], 'name'), humanizeUnderscoreFallback(fallback));
}

export function translateGcodeCommandDescription(identifier: string, fallback: string): string {
    return t(buildPathKey('gcode.command', [identifier], 'description'), fallback);
}

export function translateGcodeArgumentLabel(identifier: string, argumentName: string, fallback: string): string {
    return t(buildPathKey('gcode.command', [identifier, 'argument', argumentName], 'label'), humanizeUnderscoreFallback(fallback));
}

export function translateGcodeArgumentDescription(identifier: string, argumentName: string, fallback: string): string {
    return t(buildPathKey('gcode.command', [identifier, 'argument', argumentName], 'description'), fallback);
}

export function applyDomTranslations(root: ParentNode = document) {
    root.querySelectorAll<HTMLElement>('[data-i18n]').forEach(element => {
        if (!element.dataset.i18nTextFallback) {
            element.dataset.i18nTextFallback = element.textContent || '';
        }
        element.textContent = t(element.dataset.i18n || '', element.dataset.i18nTextFallback);
    });

    root.querySelectorAll<HTMLElement>('[data-i18n-title]').forEach(element => {
        if (!element.dataset.i18nTitleFallback) {
            element.dataset.i18nTitleFallback = element.getAttribute('title') || '';
        }
        element.setAttribute(
            'title',
            t(element.dataset.i18nTitle || '', element.dataset.i18nTitleFallback)
        );
    });

    root.querySelectorAll<HTMLInputElement | HTMLTextAreaElement>('[data-i18n-placeholder]').forEach(element => {
        if (!element.dataset.i18nPlaceholderFallback) {
            element.dataset.i18nPlaceholderFallback = element.getAttribute('placeholder') || '';
        }
        element.setAttribute(
            'placeholder',
            t(element.dataset.i18nPlaceholder || '', element.dataset.i18nPlaceholderFallback)
        );
    });
}

async function applyLocale(): Promise<void> {
    const defaultLocale = localeManifest.defaultLocale || DEFAULT_LOCALE;
    activeLocale = pickSupportedLocale(getRequestedLocales(), defaultLocale);
    translations = await loadBundle(activeLocale);

    document.documentElement.lang = activeLocale;
    updateLocaleSelect();
    applyDomTranslations();
    localeListeners.forEach(listener => listener());
}

export async function initLocalization(): Promise<void> {
    localePreference = normalizeLocale(getStoredLocalePreference()) || 'auto';
    if (localePreference === '') {
        localePreference = 'auto';
    }

    await loadManifest();
    await applyLocale();
}

export async function setLocalePreference(locale: string): Promise<void> {
    localePreference = normalizeLocale(locale) || 'auto';
    if (localePreference === '') {
        localePreference = 'auto';
    }
    setStoredLocalePreference(localePreference);
    await applyLocale();
}

export function onLocaleChange(listener: () => void): () => void {
    localeListeners.add(listener);
    return () => localeListeners.delete(listener);
}

export function getActiveLocale(): string {
    return activeLocale;
}
