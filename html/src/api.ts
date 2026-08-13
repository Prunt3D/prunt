const API_BASE = '';

export async function fetchConfigSchema() {
    const res = await fetch(`${API_BASE}/config/schema`);
    if (!res.ok) throw new Error("Failed to fetch config schema");
    return await res.json();
}

export async function fetchConfigValues() {
    const res = await fetch(`${API_BASE}/config/values`);
    if (!res.ok) throw new Error("Failed to fetch config values");
    return await res.json();
}

export async function fetchGcodeSchema() {
    const res = await fetch(`${API_BASE}/gcode-schema`);
    if (!res.ok) throw new Error("Failed to fetch gcode schema");
    return await res.json();
}

export async function patchConfigValues(values: any) {
    const res = await fetch(`${API_BASE}/config/values`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(values)
    });
    return await res.json();
}

export async function fetchStatusSchema() {
    const res = await fetch(`${API_BASE}/status/schema`);
    if (!res.ok) throw new Error("Failed to fetch status schema");
    return await res.json();
}

export async function fetchStatusValues() {
    const res = await fetch(`${API_BASE}/status/values`);
    if (!res.ok) throw new Error("Failed to fetch status values");
    return await res.json();
}

// Machine Control
export async function pauseMachine() {
    const res = await fetch(`${API_BASE}/pause/pause`, { method: "POST" });
    if (!res.ok) throw new Error("Failed to pause");
}

export async function resumeMachine() {
    const res = await fetch(`${API_BASE}/pause/resume`, { method: "POST" });
    if (!res.ok) throw new Error("Failed to resume");
}

export async function cancelMachine() {
    const res = await fetch(`${API_BASE}/cancel`, { method: "POST" });
    if (!res.ok) throw new Error("Failed to cancel");
}

export async function runFile(filename: string) {
    const res = await fetch(`${API_BASE}/run-file`, {
        method: "POST",
        body: filename
    });
    if (!res.ok) throw new Error("Failed to run file");
}

export type GcodeCommandSubmission = {
    ID: string;
};

export async function runCommand(command: string): Promise<GcodeCommandSubmission> {
    const res = await fetch(`${API_BASE}/run-command`, {
        method: "POST",
        body: command
    });
    if (!res.ok) {
        const message = await res.text();
        throw new Error(message || "Failed to run command");
    }

    const result = await res.json() as Partial<GcodeCommandSubmission>;
    if (typeof result.ID !== 'string' || result.ID.length === 0) {
        throw new Error("Command response did not include an ID");
    }

    return { ID: result.ID };
}

export async function restartServer() {
    const res = await fetch(`${API_BASE}/reload-server`, { method: "POST" });
    if (!res.ok) throw new Error("Failed to restart server");
}

export async function allowFirmwareUpdate() {
    const res = await fetch(`${API_BASE}/allow-firmware-update`, { method: "POST" });
    if (!res.ok) throw new Error("Failed to allow firmware update");
}

// File Management
export async function listUploads(): Promise<string[]> {
    const res = await fetch(`${API_BASE}/uploads/`);
    if (!res.ok) throw new Error("Failed to fetch uploads list");
    return await res.json();
}

export async function uploadFile(file: File) {
    const res = await fetch(`${API_BASE}/uploads/${file.name}`, {
        method: "PUT",
        body: file
    });
    if (!res.ok) throw new Error("Failed to upload file");
}
