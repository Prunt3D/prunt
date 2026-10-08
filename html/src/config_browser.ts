export type ConfigCategory = {
    id: string;
    label: string;
    order: number;
};

// The empty ID is reserved for properties whose declarations do not specify a category.
const fallbackCategory: ConfigCategory = { id: '', label: 'Other settings', order: Infinity };

type PropertySchema = {
    Category?: { Id: string; Label: string; Order: number };
};

type ConfigSchema = {
    Config?: Record<string, { Config: Record<string, PropertySchema> }>;
};

export function configCategoryForProperty(schema: PropertySchema): string {
    return schema.Category?.Id || fallbackCategory.id;
}

export function configCategoriesFromSchema(schema: ConfigSchema): ConfigCategory[] {
    const categories = new Map<string, ConfigCategory>();
    for (const module of Object.values(schema.Config || {})) {
        for (const property of Object.values(module.Config || {})) {
            const metadata = property.Category;
            const category = metadata?.Id
                ? { id: metadata.Id, label: metadata.Label, order: metadata.Order }
                : fallbackCategory;
            if (!categories.has(category.id)) categories.set(category.id, category);
        }
    }
    return Array.from(categories.values()).sort((left, right) =>
        left.order - right.order || left.id.localeCompare(right.id));
}

function normalizeSearch(text: string): string {
    return text.normalize('NFKD').replace(/\p{Diacritic}/gu, '').replace(/_/g, ' ').toLocaleLowerCase();
}

export function matchesConfigSearch(text: string, query: string): boolean {
    const normalized = normalizeSearch(text);
    const words = normalized.split(/[^\p{L}\p{N}]+/u);
    return normalizeSearch(query).trim().split(/\s+/).every(term =>
        /^[xyze]$/u.test(term) ? words.includes(term) : normalized.includes(term));
}
