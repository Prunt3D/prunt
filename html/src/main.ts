import { initUI } from './ui.js';

document.addEventListener('DOMContentLoaded', () => {
    console.log("Prunt UI Initializing...");
    void initUI().catch(error => {
        console.error("Failed to initialize UI", error);
    });
});
