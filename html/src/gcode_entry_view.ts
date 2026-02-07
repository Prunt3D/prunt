import { fetchGcodeSchema, runCommand } from './api.js';
import {
    onLocaleChange,
    t,
    translateGcodeArgumentDescription,
    translateGcodeArgumentLabel,
    translateGcodeCommandDescription,
    translateGcodeCommandName,
    translateGcodeModuleLabel,
    translateSchemaKind
} from './localization.js';

type GcodeArgumentDefinition = {
    Allowed_Kinds: string[];
    Description: string;
};

type GcodeCommand = {
    Identifier: {
        Argument: string;
        Number: number;
    };
    Name: string;
    Description: string;
    Arguments?: Record<string, GcodeArgumentDefinition>;
};

type GcodeSchema = Record<string, GcodeCommand[]>;

type IndexedGcodeCommand = {
    moduleName: string;
    command: GcodeCommand;
    identifier: string;
};

type EntryState = {
    commandFragment: string;
    commandResolved: boolean;
    commandMatches: IndexedGcodeCommand[];
    exactCommandMatches: IndexedGcodeCommand[];
    currentArgumentFragment: string;
    usedArguments: Set<string>;
};

type HistoryEntry = {
    command: string;
};

const MAX_COMMAND_CANDIDATES = 8;

let indexedCommands: IndexedGcodeCommand[] = [];
let schemaReady = false;
let schemaLoadFailed = false;
let popupShouldBeVisible = false;
let commandHistory: HistoryEntry[] = [];

export function initGcodeEntryView() {
    const input = document.getElementById('gcode-entry-input') as HTMLInputElement | null;
    const sendButton = document.getElementById('btn-send-gcode');
    const popup = document.getElementById('gcode-entry-popup');
    const history = document.getElementById('gcode-entry-history');
    const status = document.getElementById('gcode-entry-status');

    if (!input || !sendButton || !popup || !history || !status) return;

    const render = () => {
        renderPopup(input, popup);
        renderHistory(history);
        renderStatus(status);
    };

    sendButton.addEventListener('click', async () => {
        const command = input.value.trim();
        if (!command) return;

        try {
            await runCommand(command);
            commandHistory = [...commandHistory, { command }];
            input.value = '';
            render();
        } catch (error) {
            console.error(error);
            alert(t('ui.control.sendFailed', 'Failed to send command.'));
        }
    });

    input.addEventListener('keydown', event => {
        if (event.key === 'Enter') {
            event.preventDefault();
            sendButton.click();
            return;
        }

        if (event.key === 'Escape') {
            event.preventDefault();
            popupShouldBeVisible = false;
            renderPopup(input, popup);
        }
    });

    input.addEventListener('input', () => {
        popupShouldBeVisible = true;
        render();
    });
    input.addEventListener('focus', () => {
        popupShouldBeVisible = true;
        render();
    });
    input.addEventListener('blur', () => {
        window.setTimeout(() => {
            popupShouldBeVisible = false;
            render();
        }, 150);
    });

    onLocaleChange(render);
    render();
    void loadSchema(render);
}

async function loadSchema(onChange: () => void) {
    schemaReady = false;
    schemaLoadFailed = false;
    onChange();

    try {
        const schema = await fetchGcodeSchema() as GcodeSchema;
        indexedCommands = buildCommandIndex(schema);
        schemaReady = true;
    } catch (error) {
        console.error('Failed to fetch gcode schema for entry view', error);
        indexedCommands = [];
        schemaLoadFailed = true;
    }

    onChange();
}

function buildCommandIndex(schema: GcodeSchema): IndexedGcodeCommand[] {
    const commands: IndexedGcodeCommand[] = [];

    for (const [moduleName, moduleCommands] of Object.entries(schema)) {
        if (!Array.isArray(moduleCommands)) continue;

        for (const command of moduleCommands) {
            commands.push({
                moduleName,
                command,
                identifier: formatCommandIdentifier(command)
            });
        }
    }

    return commands.sort((left, right) => {
        const letterOrder = left.command.Identifier.Argument.localeCompare(right.command.Identifier.Argument);
        if (letterOrder !== 0) return letterOrder;
        return left.command.Identifier.Number - right.command.Identifier.Number;
    });
}

function formatCommandIdentifier(command: GcodeCommand): string {
    return `${command.Identifier.Argument}${command.Identifier.Number}`.toUpperCase();
}

function analyzeInput(value: string): EntryState {
    const trimmedStart = value.trimStart();
    const endsWithWhitespace = value.length > 0 && /\s$/.test(value);
    const commandMatch = trimmedStart.match(/^([A-Za-z]\d*)(.*)$/);
    const commandFragment = (commandMatch?.[1] || trimmedStart.split(/\s+/, 1)[0] || '').toUpperCase();
    const rawArgumentSource = commandMatch?.[2] || '';
    const commandResolved = rawArgumentSource.length > 0 && (/^\s/.test(rawArgumentSource) || /^[A-Za-z]/.test(rawArgumentSource));
    const exactCommandMatches = indexedCommands.filter(command => command.identifier === commandFragment);
    const commandMatches = findCommandMatches(commandFragment);
    const argumentSource = commandResolved ? rawArgumentSource.trimStart() : '';
    const argumentTokens = tokenizeArgumentSource(argumentSource);
    const usedArguments = new Set<string>();

    for (const token of argumentTokens) {
        const match = token.match(/^[A-Za-z]/);
        if (match) {
            usedArguments.add(match[0].toUpperCase());
        }
    }

    let currentArgumentFragment = '';
    if (exactCommandMatches.length > 0 && commandResolved && argumentTokens.length > 0 && !endsWithWhitespace) {
        const currentToken = argumentTokens[argumentTokens.length - 1];
        if (/^[A-Za-z]+$/.test(currentToken)) {
            currentArgumentFragment = currentToken.toUpperCase();
        }
    }

    return {
        commandFragment,
        commandResolved,
        commandMatches,
        exactCommandMatches,
        currentArgumentFragment,
        usedArguments
    };
}

function tokenizeArgumentSource(argumentSource: string): string[] {
    const tokens: string[] = [];
    let currentToken = '';

    for (const character of argumentSource.trimStart()) {
        if (/\s/.test(character)) {
            if (currentToken) {
                tokens.push(currentToken);
                currentToken = '';
            }
            continue;
        }

        if (/^[A-Za-z]$/.test(character) && currentToken.length > 0) {
            tokens.push(currentToken);
            currentToken = character;
            continue;
        }

        currentToken += character;
    }

    if (currentToken) {
        tokens.push(currentToken);
    }

    return tokens;
}

function findCommandMatches(fragment: string): IndexedGcodeCommand[] {
    if (!schemaReady) return [];

    if (!fragment) {
        return indexedCommands.slice(0, MAX_COMMAND_CANDIDATES);
    }

    const upperFragment = fragment.toUpperCase();
    const exactIdentifierMatches = indexedCommands.filter(command => command.identifier === upperFragment);
    const prefixIdentifierMatches = indexedCommands.filter(command => command.identifier.startsWith(upperFragment) && command.identifier !== upperFragment);
    const namedMatches = indexedCommands.filter(command => {
        const translatedName = translateGcodeCommandName(command.identifier, command.command.Name);
        return translatedName.toUpperCase().includes(upperFragment) && !command.identifier.startsWith(upperFragment);
    });

    return [...exactIdentifierMatches, ...prefixIdentifierMatches, ...namedMatches].slice(0, MAX_COMMAND_CANDIDATES);
}

function renderStatus(status: HTMLElement) {
    if (schemaLoadFailed) {
        status.className = 'gcode-entry-status error-message';
        status.textContent = t('ui.gcodeEntry.loadFailed', 'Failed to load G-Code schema.');
        return;
    }

    if (!schemaReady) {
        status.className = 'gcode-entry-status text-muted';
        status.textContent = t('ui.gcodeEntry.loading', 'Loading G-Code schema...');
        return;
    }

    status.className = 'gcode-entry-status text-muted';
    status.textContent = t('ui.gcodeEntry.subtitle', 'Type a command to see matching commands and argument candidates.');
}

function renderHistory(history: HTMLElement) {
    history.replaceChildren();

    if (commandHistory.length === 0) {
        const empty = document.createElement('div');
        empty.className = 'gcode-entry-history-empty';
        empty.textContent = t('ui.gcodeEntry.historyEmpty', 'Sent commands will appear here.');
        history.appendChild(empty);
        return;
    }

    for (const entry of commandHistory) {
        const line = document.createElement('div');
        line.className = 'gcode-entry-history-line';
        line.textContent = `> ${entry.command}`;
        history.appendChild(line);
    }

    history.scrollTop = history.scrollHeight;
}

function renderPopup(input: HTMLInputElement, popup: HTMLElement) {
    popup.replaceChildren();

    if (!popupShouldBeVisible) {
        popup.classList.remove('visible');
        return;
    }

    if (schemaLoadFailed) {
        popup.appendChild(createPopupMessage(t('ui.gcodeEntry.loadFailed', 'Failed to load G-Code schema.')));
        popup.classList.add('visible');
        return;
    }

    if (!schemaReady) {
        popup.appendChild(createPopupMessage(t('ui.gcodeEntry.loading', 'Loading G-Code schema...')));
        popup.classList.add('visible');
        return;
    }

    const state = analyzeInput(input.value);
    const resolvedCommands = findResolvedCommands(state);

    if (resolvedCommands.length > 0) {
        popup.appendChild(createSectionTitle(
            resolvedCommands.length === 1
                ? t('ui.gcodeEntry.currentCommand', 'Current command')
                : t('ui.gcodeEntry.matchingCommands', 'Matching commands')
        ));

        for (const command of resolvedCommands) {
            popup.appendChild(createResolvedCommandGroup(input, state, command));
        }
    } else if (state.commandMatches.length > 0) {
        popup.appendChild(createSectionTitle(t('ui.gcodeEntry.matchingCommands', 'Matching commands')));
        for (const command of state.commandMatches) {
            popup.appendChild(createCommandCandidateButton(input, command));
        }
    } else if (state.commandFragment) {
        popup.appendChild(createPopupMessage(t('ui.gcodeEntry.noMatches', 'No matching G-Code commands.')));
    } else {
        popup.appendChild(createSectionTitle(t('ui.gcodeEntry.matchingCommands', 'Matching commands')));
        for (const command of indexedCommands.slice(0, MAX_COMMAND_CANDIDATES)) {
            popup.appendChild(createCommandCandidateButton(input, command));
        }
    }

    popup.classList.add('visible');
}

function findResolvedCommands(state: EntryState): IndexedGcodeCommand[] {
    if (!state.commandResolved || state.exactCommandMatches.length === 0) {
        return [];
    }

    const compatibleCommands = state.exactCommandMatches.filter(command => isCommandCompatible(command, state));
    return compatibleCommands.length > 0 ? compatibleCommands : state.exactCommandMatches;
}

function isCommandCompatible(command: IndexedGcodeCommand, state: EntryState): boolean {
    return [...state.usedArguments].every(argumentName => Boolean(command.command.Arguments?.[argumentName]));
}

function findArgumentCandidates(
    state: EntryState,
    command: IndexedGcodeCommand
): Array<[string, GcodeArgumentDefinition]> {
    if (!command.command.Arguments || !isCommandCompatible(command, state)) {
        return [];
    }

    return Object.entries(command.command.Arguments)
        .sort(([left], [right]) => left.localeCompare(right))
        .filter(([argumentName]) => {
            const upperName = argumentName.toUpperCase();
            if (state.currentArgumentFragment) {
                return upperName.startsWith(state.currentArgumentFragment)
                    && (!state.usedArguments.has(upperName) || upperName === state.currentArgumentFragment);
            }

            return !state.usedArguments.has(upperName);
        });
}

function createSectionTitle(text: string): HTMLElement {
    const title = document.createElement('div');
    title.className = 'gcode-entry-popup-title';
    title.textContent = text;
    return title;
}

function createPopupMessage(text: string): HTMLElement {
    const message = document.createElement('div');
    message.className = 'gcode-entry-popup-message';
    message.textContent = text;
    return message;
}

function createResolvedCommandGroup(
    input: HTMLInputElement,
    state: EntryState,
    command: IndexedGcodeCommand
): HTMLElement {
    const group = document.createElement('div');
    group.className = 'gcode-entry-popup-group';

    group.appendChild(createCommandCandidateButton(input, command));

    const argumentsContainer = document.createElement('div');
    argumentsContainer.className = 'gcode-entry-popup-arguments';

    const argumentsTitle = document.createElement('div');
    argumentsTitle.className = 'gcode-entry-popup-subtitle';
    argumentsTitle.textContent = t('ui.gcodeEntry.argumentCandidates', 'Available arguments');
    argumentsContainer.appendChild(argumentsTitle);

    const argumentCandidates = findArgumentCandidates(state, command);
    if (argumentCandidates.length === 0) {
        argumentsContainer.appendChild(createPopupMessage(t('ui.gcodeEntry.noArguments', 'No more arguments available for this command.')));
    } else {
        for (const [argumentName, definition] of argumentCandidates) {
            argumentsContainer.appendChild(createArgumentCandidateButton(input, command, argumentName, definition));
        }
    }

    group.appendChild(argumentsContainer);
    return group;
}

function createCommandCandidateButton(input: HTMLInputElement, indexedCommand: IndexedGcodeCommand): HTMLButtonElement {
    const button = document.createElement('button');
    button.type = 'button';
    button.className = 'gcode-entry-popup-item';

    const heading = document.createElement('div');
    heading.className = 'gcode-entry-popup-heading';

    const identifier = document.createElement('strong');
    identifier.className = 'gcode-entry-popup-identifier';
    identifier.textContent = indexedCommand.identifier;
    heading.appendChild(identifier);

    const name = document.createElement('span');
    name.className = 'gcode-entry-popup-name';
    name.textContent = translateGcodeCommandName(indexedCommand.identifier, indexedCommand.command.Name);
    heading.appendChild(name);

    const moduleName = document.createElement('span');
    moduleName.className = 'gcode-entry-popup-module';
    moduleName.textContent = translateGcodeModuleLabel(indexedCommand.moduleName, indexedCommand.moduleName);
    heading.appendChild(moduleName);

    const description = document.createElement('div');
    description.className = 'gcode-entry-popup-description';
    description.textContent = translateGcodeCommandDescription(indexedCommand.identifier, indexedCommand.command.Description || '');

    button.appendChild(heading);
    if (description.textContent) {
        button.appendChild(description);
    }

    button.addEventListener('mousedown', event => {
        event.preventDefault();
    });
    button.addEventListener('click', () => {
        input.value = applyCommandCandidate(input.value, indexedCommand.identifier);
        input.focus();
        popupShouldBeVisible = true;
        renderPopup(input, document.getElementById('gcode-entry-popup') as HTMLElement);
    });

    return button;
}

function createArgumentCandidateButton(
    input: HTMLInputElement,
    indexedCommand: IndexedGcodeCommand,
    argumentName: string,
    definition: GcodeArgumentDefinition
): HTMLButtonElement {
    const button = document.createElement('button');
    button.type = 'button';
    button.className = 'gcode-entry-popup-item gcode-entry-popup-item-argument';

    const heading = document.createElement('div');
    heading.className = 'gcode-entry-popup-heading';

    const label = document.createElement('strong');
    label.className = 'gcode-entry-popup-identifier';
    label.textContent = translateGcodeArgumentLabel(indexedCommand.identifier, argumentName, argumentName);
    heading.appendChild(label);

    const types = document.createElement('span');
    types.className = 'gcode-entry-popup-types';
    const allowedKinds = definition.Allowed_Kinds.filter(kind => kind !== 'Non_Existent');
    types.textContent = `[${allowedKinds.map(kind => translateSchemaKind(kind)).join(', ')}]`;
    heading.appendChild(types);

    if (definition.Allowed_Kinds.includes('Non_Existent')) {
        const optional = document.createElement('span');
        optional.className = 'gcode-entry-popup-optional';
        optional.textContent = t('ui.gcode.optional', 'optional');
        heading.appendChild(optional);
    }

    const description = document.createElement('div');
    description.className = 'gcode-entry-popup-description';
    description.textContent = translateGcodeArgumentDescription(indexedCommand.identifier, argumentName, definition.Description || '');

    button.appendChild(heading);
    if (description.textContent) {
        button.appendChild(description);
    }

    button.addEventListener('mousedown', event => {
        event.preventDefault();
    });
    button.addEventListener('click', () => {
        input.value = applyArgumentCandidate(input.value, argumentName.toUpperCase(), definition);
        input.focus();
        popupShouldBeVisible = true;
        renderPopup(input, document.getElementById('gcode-entry-popup') as HTMLElement);
    });

    return button;
}

function applyCommandCandidate(existingValue: string, identifier: string): string {
    const trimmed = existingValue.trim();
    if (!trimmed) {
        return `${identifier} `;
    }

    const tokens = trimmed.split(/\s+/);
    tokens[0] = identifier;
    return `${tokens.join(' ')} `;
}

function applyArgumentCandidate(existingValue: string, argumentName: string, definition: GcodeArgumentDefinition): string {
    const trimmedStart = existingValue.trimStart();
    const commandMatch = trimmedStart.match(/^([A-Za-z]\d*)(.*)$/);
    const allowsValue = definition.Allowed_Kinds.some(kind => kind !== 'Non_Existent');

    if (!commandMatch) {
        return allowsValue ? argumentName : `${argumentName} `;
    }

    const commandToken = commandMatch[1].toUpperCase();
    const argumentSource = commandMatch[2].trimStart();
    const tokens = tokenizeArgumentSource(argumentSource);
    const endsWithWhitespace = existingValue.length > 0 && /\s$/.test(existingValue);

    const currentToken = !endsWithWhitespace && tokens.length > 0 ? tokens[tokens.length - 1] : '';
    if (tokens.length > 0 && /^[A-Za-z]+$/.test(currentToken)) {
        tokens[tokens.length - 1] = argumentName;
    } else {
        tokens.push(argumentName);
    }

    const suffix = allowsValue ? '' : ' ';
    return `${commandToken} ${tokens.join(' ')}${suffix}`;
}
