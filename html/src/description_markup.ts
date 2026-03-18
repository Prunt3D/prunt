import { navigateToConfigTarget, navigateToGcodeTarget } from './navigation.js';

function createLink(label: string, target: string): Node | null {
    if (target.startsWith('image:')) {
        const image = document.createElement('img');
        image.className = 'description-image';
        image.alt = label;
        image.loading = 'lazy';
        image.src = target.slice('image:'.length);
        return image;
    }

    const link = document.createElement('a');
    link.className = 'description-link';

    if (target.startsWith('gcode:')) {
        link.href = '#gcode-explorer-view';
        link.addEventListener('click', event => {
            event.preventDefault();
            navigateToGcodeTarget(target.slice('gcode:'.length));
        });
    } else if (target.startsWith('config:')) {
        link.href = '#config-view';
        link.addEventListener('click', event => {
            event.preventDefault();
            navigateToConfigTarget(target.slice('config:'.length));
        });
    } else if (/^(https?:\/\/|\/|\.{1,2}\/)/.test(target)) {
        link.href = target;
        link.target = '_blank';
        link.rel = 'noopener noreferrer';
    } else {
        return null;
    }

    appendInlineMarkdown(link, label);
    return link;
}

function appendInlineMarkdown(parent: HTMLElement, text: string) {
    let index = 0;

    while (index < text.length) {
        if (text.startsWith('**', index)) {
            const closeIndex = text.indexOf('**', index + 2);
            if (closeIndex !== -1) {
                const strong = document.createElement('strong');
                appendInlineMarkdown(strong, text.slice(index + 2, closeIndex));
                parent.appendChild(strong);
                index = closeIndex + 2;
                continue;
            }
        }

        if (text[index] === '*') {
            const closeIndex = text.indexOf('*', index + 1);
            if (closeIndex !== -1) {
                const emphasis = document.createElement('em');
                appendInlineMarkdown(emphasis, text.slice(index + 1, closeIndex));
                parent.appendChild(emphasis);
                index = closeIndex + 1;
                continue;
            }
        }

        if (text[index] === '`') {
            const closeIndex = text.indexOf('`', index + 1);
            if (closeIndex !== -1) {
                const code = document.createElement('code');
                code.className = 'description-code';
                code.textContent = text.slice(index + 1, closeIndex);
                parent.appendChild(code);
                index = closeIndex + 1;
                continue;
            }
        }

        if (text[index] === '[') {
            const labelEnd = text.indexOf(']', index + 1);
            if (labelEnd !== -1 && text[labelEnd + 1] === '(') {
                const targetEnd = text.indexOf(')', labelEnd + 2);
                if (targetEnd !== -1) {
                    const node = createLink(text.slice(index + 1, labelEnd), text.slice(labelEnd + 2, targetEnd));
                    if (node) {
                        parent.appendChild(node);
                        index = targetEnd + 1;
                        continue;
                    }
                }
            }
        }

        const nextSpecialIndex = (() => {
            for (let probe = index + 1; probe < text.length; probe++) {
                if (text[probe] === '[' || text[probe] === '*' || text[probe] === '`') {
                    return probe;
                }
            }
            return text.length;
        })();

        parent.appendChild(document.createTextNode(text.slice(index, nextSpecialIndex)));
        index = nextSpecialIndex;
    }
}

export function renderDescription(container: HTMLElement, text: string, paragraphClass = 'description'): boolean {
    let hasContent = false;

    for (const paragraphText of text.split('\n')) {
        if (paragraphText.trim().length === 0) continue;

        const paragraph = document.createElement('p');
        paragraph.className = paragraphClass;
        appendInlineMarkdown(paragraph, paragraphText);
        container.appendChild(paragraph);
        hasContent = true;
    }

    return hasContent;
}
