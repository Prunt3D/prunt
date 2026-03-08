import { fetchGcodeSchema } from './api.js';

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
        container.innerHTML = '<p class="error-message error-block">Failed to load G-Code schema.</p>';
    }
}

function renderGcodeExplorer(container: HTMLElement, schema: any) {
    container.innerHTML = '';

    if (groupByModule) {
        for (const [moduleName, commands] of Object.entries(schema)) {
            if (!Array.isArray(commands) || commands.length === 0) continue;

            const modContainer = document.createElement('div');
            modContainer.className = 'gcode-module-group';

            const title = document.createElement('h3');
            title.innerText = moduleName;
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
        container.innerHTML = '<p class="text-muted">No G-Code commands available.</p>';
    }
}

function createCommandCard(cmd: any): HTMLElement {
    const cmdCard = document.createElement('div');
    cmdCard.className = 'card gcode-command-card';

    const header = document.createElement('div');
    header.className = 'gcode-command-header';

    const identifier = document.createElement('strong');
    identifier.className = 'gcode-command-id';
    identifier.innerText = `${cmd.Identifier.Argument}${cmd.Identifier.Number}`;

    const name = document.createElement('span');
    name.className = 'gcode-command-name';
    name.innerText = cmd.Name.replace(/_/g, ' ');

    header.appendChild(identifier);
    header.appendChild(name);
    cmdCard.appendChild(header);

    if (cmd.Description) {
        const descContainer = document.createElement('div');
        descContainer.className = 'gcode-command-desc';
        for (const pText of cmd.Description.split('\n')) {
            if (pText.trim().length === 0) continue;
            const p = document.createElement('p');
            p.className = 'description';
            p.innerText = pText;
            descContainer.appendChild(p);
        }
        cmdCard.appendChild(descContainer);
    }

    if (cmd.Arguments && Object.keys(cmd.Arguments).length > 0) {
        const argsTitle = document.createElement('h4');
        argsTitle.innerText = 'Arguments';
        argsTitle.className = 'gcode-args-title';
        cmdCard.appendChild(argsTitle);

        const argsList = document.createElement('ul');
        argsList.className = 'gcode-args-list';

        // Sort arguments alphabetically
        const argNames = Object.keys(cmd.Arguments).sort();

        for (const argName of argNames) {
            const argDef = cmd.Arguments[argName];
            const argItem = document.createElement('li');
            argItem.className = 'gcode-arg-item';

            const argHeader = document.createElement('div');
            argHeader.className = 'gcode-arg-header';

            const argLabel = document.createElement('strong');
            argLabel.innerText = argName;
            argLabel.className = 'gcode-arg-label';

            const isOptional = argDef.Allowed_Kinds.includes('Non_Existent');
            const allowedKinds = argDef.Allowed_Kinds.filter((k: string) => k !== 'Non_Existent');
            const typesSpan = document.createElement('span');
            typesSpan.className = 'gcode-arg-types';
            typesSpan.innerText = `[${allowedKinds.join(', ')}]`;

            argHeader.appendChild(argLabel);
            argHeader.appendChild(typesSpan);

            if (isOptional) {
                const optionalBadge = document.createElement('span');
                optionalBadge.className = 'gcode-arg-optional';
                optionalBadge.innerText = 'optional';
                argHeader.appendChild(optionalBadge);
            }
            argItem.appendChild(argHeader);

            const argDesc = document.createElement('div');
            argDesc.className = 'gcode-arg-desc';
            argDesc.innerText = argDef.Description;
            argItem.appendChild(argDesc);

            argsList.appendChild(argItem);
        }
        cmdCard.appendChild(argsList);
    }

    return cmdCard;
}
