const API_BASE = '';

export async function fetchConfigSchema() {
    const res = await fetch(`${API_BASE}/config/schema`);
    if (!res.ok) throw new Error("Failed to fetch config schema");
    return await res.json();
}

/** The stored configuration together with its revision and the errors found in it, all taken at the same moment. */
export type StoredConfigResponse = {
    Values: any;
    /** Changes whenever the stored configuration changes, from any source. Send it back as "Base revision". */
    Revision: string;
    /** True when a patch was rejected because its "Base revision" was no longer stored. Nothing was applied. */
    Conflict: boolean;
    Errors: any[];
};

export async function fetchConfigValues(): Promise<StoredConfigResponse> {
    const res = await fetch(`${API_BASE}/config/values`);
    if (!res.ok) throw new Error("Failed to fetch config values");
    return await res.json();
}

export async function fetchGcodeSchema() {
    const res = await fetch(`${API_BASE}/gcode-schema`);
    if (!res.ok) throw new Error("Failed to fetch gcode schema");
    return await res.json();
}

/** Apply a patch. A 409 response still carries the stored configuration, with Conflict set. */
export async function patchConfigValues(values: any): Promise<StoredConfigResponse> {
    const res = await fetch(`${API_BASE}/config/values`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(values)
    });
    if (!res.ok && res.status !== 409) throw new Error(`Failed to save config values (${res.status})`);
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

// Machine control from the status page. These paths are stubs on the server for now: it replies 501 Not Implemented
// to everything under control/. Bodies are JSON in the units of the status values (mm, mm/s, °C, and scale factors
// where 1 is 100 %), and axes and heaters are named by their status keys, such as X_AXIS.

export class ControlNotImplementedError extends Error { }

async function postControl(path: string, body: object): Promise<void> {
    const res = await fetch(`${API_BASE}/control/${path}`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(body)
    });
    if (res.status === 501 || res.status === 404) {
        throw new ControlNotImplementedError(await res.text());
    }
    if (!res.ok) {
        throw new Error((await res.text()) || `Control request failed (${res.status}).`);
    }
}

/** Move one axis by Distance mm from where it is, at Feedrate mm/s, without changing the G-code modal state. */
export function jogAxis(axis: string, distance: number, feedrate: number): Promise<void> {
    return postControl("jog", { Axis: axis, Distance: distance, Feedrate: feedrate });
}

/** Home the given axes, or every axis when the list is empty, as G28 does. */
export function homeAxes(axes: string[]): Promise<void> {
    return postControl("home", { Axes: axes });
}

/** Disable every motor, as M84 does. */
export function disableMotors(): Promise<void> {
    return postControl("motors-off", {});
}

/** Set a heater's target temperature by its configured name. A target of 0 turns the heater off. */
export function setHeaterTarget(heater: string, target: number): Promise<void> {
    return postControl("heater-target", { Heater: heater, Target: target });
}

/** Set the feedrate override, as M220 does. */
export function setFeedrateScale(scale: number): Promise<void> {
    return postControl("feedrate-scale", { Scale: scale });
}

/** Set the flow override, as M221 does. */
export function setFlowScale(scale: number): Promise<void> {
    return postControl("flow-scale", { Scale: scale });
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

export async function switchSmartPlug(enabled: boolean): Promise<void> {
    const res = await fetch(`${API_BASE}/smart-plug/${enabled ? 'on' : 'off'}`, {
        method: 'POST',
        signal: AbortSignal.timeout(5000)
    });
    if (!res.ok) throw new Error(await res.text());
}
