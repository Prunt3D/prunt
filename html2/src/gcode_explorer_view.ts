import { fetchGcodeSchema } from './api';

let currentSchema: any = null;
let groupByModule = false;

export async function initGcodeExplorerView() {
    const container = document.getElementById('gcode-explorer-content');
    if (!container) return;

    const toggle = document.getElementById('gcode-group-toggle') as HTMLInputElement;
    if (toggle) {
        groupByModule = toggle.checked;
        toggle.addEventListener('change', (e) => {
            groupByModule = (e.target as HTMLInputElement).checked;
            if (currentSchema) renderGcodeExplorer(container, currentSchema);
        });
    }

    try {
        currentSchema = await fetchGcodeSchema();
        renderGcodeExplorer(container, currentSchema);
    } catch (e) {
        console.error("Failed to fetch gcode schema", e);
        container.innerHTML = '<p class="error-message" style="display:block;">Failed to load G-Code schema.</p>';
    }
}

function renderGcodeExplorer(container: HTMLElement, schema: any) {
    container.innerHTML = '';

    if (groupByModule) {
        for (const [moduleName, commands] of Object.entries(schema)) {
            if (!Array.isArray(commands) || commands.length === 0) continue;

            const modContainer = document.createElement('div');
            modContainer.className = 'gcode-module-group';
            modContainer.style.marginBottom = '24px';

            const title = document.createElement('h3');
            title.innerText = moduleName;
            title.style.borderBottom = '1px solid var(--border)';
            title.style.paddingBottom = '8px';
            title.style.marginBottom = '16px';
            modContainer.appendChild(title);

            for (const cmd of commands) {
                modContainer.appendChild(createCommandCard(cmd));
            }

            container.appendChild(modContainer);
        }
    } else {
        // Flat list
        let allCommands: any[] = [];
        for (const [moduleName, commands] of Object.entries(schema)) {
            if (!Array.isArray(commands)) continue;
            // Optionally could tag them with moduleName if needed, but JSON might have dupes across modules
            // Typically in flat view, we might want to deduplicate based on Identifier + Name
            allCommands.push(...commands);
        }

        const sortedCommands = allCommands.sort((a, b) => {
            const argCmp = a.Identifier.Argument.localeCompare(b.Identifier.Argument);
            if (argCmp !== 0) return argCmp;
            return a.Identifier.Number - b.Identifier.Number;
        });

        const flatContainer = document.createElement('div');
        flatContainer.className = 'gcode-flat-group';

        for (const cmd of sortedCommands) {
            flatContainer.appendChild(createCommandCard(cmd));
        }

        container.appendChild(flatContainer);
    }

    if (container.children.length === 0) {
        container.innerHTML = '<p style="color: var(--text-muted);">No G-Code commands available.</p>';
    }
}

function createCommandCard(cmd: any): HTMLElement {
    const cmdCard = document.createElement('div');
    cmdCard.className = 'card gcode-command-card';
    cmdCard.style.marginBottom = '16px';
    cmdCard.style.padding = '16px';

    const header = document.createElement('div');
    header.style.display = 'flex';
    header.style.alignItems = 'baseline';
    header.style.gap = '12px';
    header.style.marginBottom = '8px';

    const identifier = document.createElement('strong');
    identifier.style.fontSize = '1.2rem';
    identifier.style.color = 'var(--primary)';
    identifier.innerText = `${cmd.Identifier.Argument}${cmd.Identifier.Number}`;

    const name = document.createElement('span');
    name.style.fontSize = '1.1rem';
    name.style.fontWeight = 'bold';
    name.innerText = cmd.Name.replace(/_/g, ' ');

    header.appendChild(identifier);
    header.appendChild(name);
    cmdCard.appendChild(header);

    if (cmd.Description) {
        const desc = document.createElement('p');
        desc.style.color = 'var(--text-muted)';
        desc.style.marginBottom = '16px';
        desc.style.whiteSpace = 'pre-wrap';
        desc.style.lineHeight = '1.5';
        for (const pText of cmd.Description.split('\n')) {
            if (pText.trim().length === 0) continue;
            const desc = document.createElement('p');
            desc.className = 'description';
            desc.innerText = pText;
            cmdCard.appendChild(desc);
        }
    }

    if (cmd.Arguments && Object.keys(cmd.Arguments).length > 0) {
        const argsTitle = document.createElement('h4');
        argsTitle.innerText = 'Arguments';
        argsTitle.style.marginBottom = '8px';
        argsTitle.style.fontSize = '0.95rem';
        cmdCard.appendChild(argsTitle);

        const argsList = document.createElement('ul');
        argsList.style.listStyleType = 'none';
        argsList.style.paddingLeft = '0';
        argsList.style.margin = '0';

        // Sort arguments alphabetically
        const argNames = Object.keys(cmd.Arguments).sort();

        for (const argName of argNames) {
            const argDef = cmd.Arguments[argName];
            const argItem = document.createElement('li');
            argItem.style.marginBottom = '12px';
            argItem.style.paddingLeft = '16px';
            argItem.style.borderLeft = '2px solid var(--border)';

            const argHeader = document.createElement('div');
            argHeader.style.marginBottom = '4px';

            const argLabel = document.createElement('strong');
            argLabel.innerText = argName;
            argLabel.style.display = 'inline-block';
            argLabel.style.width = '24px';
            argLabel.style.color = 'var(--text-main)';

            const allowedKinds = argDef.Allowed_Kinds.filter((k: string) => k !== 'Non_Existent');
            const typesSpan = document.createElement('span');
            typesSpan.style.fontSize = '0.85rem';
            typesSpan.style.color = 'var(--text-muted)';
            typesSpan.style.fontFamily = 'monospace';
            typesSpan.innerText = `[${allowedKinds.join(', ')}]`;

            argHeader.appendChild(argLabel);
            argHeader.appendChild(typesSpan);
            argItem.appendChild(argHeader);

            const argDesc = document.createElement('div');
            argDesc.style.color = 'var(--text-muted)';
            argDesc.style.fontSize = '0.9rem';
            argDesc.style.lineHeight = '1.4';
            argDesc.innerText = argDef.Description;
            argItem.appendChild(argDesc);

            argsList.appendChild(argItem);
        }
        cmdCard.appendChild(argsList);
    }

    return cmdCard;
}
