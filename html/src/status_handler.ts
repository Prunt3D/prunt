import uPlot from "uplot";

interface StatusSchema {
    Position: string[];
    Thermistor_Temperatures: string[];
    Stepper_Temperatures: string[];
    Board_Probe_Temperatures: string[];
    Heater_Powers: string[];
    Switch_Is_High_State: string[];
    Tachometer_Frequencies: string[];
}

interface StatusValues {
    Time: number;
    Position: Record<string, number>;
    Thermistor_Temperatures: Record<string, number>;
    Stepper_Temperatures: Record<string, number>;
    Board_Probe_Temperatures: Record<string, number>;
    Heater_Powers: Record<string, number>;
    Switch_Is_High_State: Record<string, boolean>;
    Tachometer_Frequencies: Record<string, number>;
    Stepgen_Is_Paused: boolean;
    Startup: "Done" | "Update running" | "Update required" | "Waiting";
}

interface WebsocketValue {
};

interface WebsocketStatusValue extends WebsocketValue {
    Status: StatusValues;
};

interface WebsocketFatalErrorValue extends WebsocketValue {
    Fatal_Error: string;
};

interface WebsocketLogValue extends WebsocketValue {
    Log: string;
};

interface WebsocketServerStartTimeValue extends WebsocketValue {
    Server_Start_Time: string;
};

let plotData: number[][];
let plot: uPlot;

// From https://github.com/bryc/code/blob/master/jshash/experimental/cyrb53.js
function cyrb53(str: string, seed = 0) {
    let h1 = 0xdeadbeef ^ seed, h2 = 0x41c6ce57 ^ seed;
    for (let i = 0, ch; i < str.length; i++) {
        ch = str.charCodeAt(i);
        h1 = Math.imul(h1 ^ ch, 2654435761);
        h2 = Math.imul(h2 ^ ch, 1597334677);
    }
    h1 = Math.imul(h1 ^ (h1 >>> 16), 2246822507);
    h1 ^= Math.imul(h2 ^ (h2 >>> 13), 3266489909);
    h2 = Math.imul(h2 ^ (h2 >>> 16), 2246822507);
    h2 ^= Math.imul(h1 ^ (h1 >>> 13), 3266489909);
    return 4294967296 * (2097151 & h2) + (h1 >>> 0);
};

function stringToHSL(input: string): string {
    const hash = cyrb53(input);

    const hue = hash % 360;
    const saturation = 50 + ((hash / 360) % 50);
    const lightness = 20 + ((hash / 360 / 50) % 60);

    return `hsl(${hue}, ${saturation}%, ${lightness}%)`;
}

async function setupPlot(schema: StatusSchema): Promise<void> {
    const makeFmt = (suffix: string) => (u: any, v: any, sidx: any, didx: any) => {
        if (didx == null) {
            let d = u.data[sidx];
            v = d[d.length - 1];
        }

        return v == null ? null : v.toFixed(1) + suffix;
    };

    let opts: uPlot.Options = {
        title: "Temperatures and Heater Powers",
        width: Math.max(300, Math.min(600, document.documentElement.clientWidth - 35)),
        height: 300,
        cursor: {
            drag: {
                setScale: false,
            }
        },
        // @ts-ignore
        select: {
            show: false,
        },
        series: [
            {
                label: "Seconds"
            }
        ].concat(
            schema.Thermistor_Temperatures.map((t) => {
                {
                    return {
                        label: t,
                        scale: "°C",
                        value: makeFmt("°C"),
                        stroke: stringToHSL(t + "thermistor")
                    }
                }
            }),
            schema.Stepper_Temperatures.map((s) => {
                {
                    return {
                        label: s,
                        scale: "°C",
                        value: makeFmt("°C"),
                        stroke: stringToHSL(s + "stepper")
                    }
                }
            }),
            schema.Board_Probe_Temperatures.map((p) => {
                {
                    return {
                        label: p,
                        scale: "°C",
                        value: makeFmt("°C"),
                        stroke: stringToHSL(p + "board probe")
                    }
                }
            }),
            schema.Heater_Powers.map((h) => {
                {
                    return {
                        label: h,
                        scale: "%",
                        value: makeFmt("%"),
                        stroke: stringToHSL(h + "heater")
                    }
                }
            }),
        ),
        axes: [
            {},
            {
                scale: "%",
                values: (u, vals, space) => vals.map(v => +v.toFixed(0) + "%"),
            },
            {
                side: 1,
                scale: "°C",
                values: (u, vals, space) => vals.map(v => +v.toFixed(0) + "°C"),
                grid: { show: false },
            },
        ],
        scales: {
            "x": {
                time: false,
            },
            "%": {
                auto: false,
                range: [0, 100],
            }
        },
    };

    plotData = Array.from({ length: opts.series.length }, (): number[] => []);
    plot = new uPlot(opts, plotData as uPlot.AlignedData, document.getElementById("statusPlot"));
}

export async function setupStatus(): Promise<void> {
    const messageLog = document.getElementById("messageLog") as HTMLDivElement;
    const logTab = document.getElementById("logTab");
    const webSocketConnectionWarning = document.getElementById("webSocketConnectionWarning");
    const boardConnectionWarning = document.getElementById("boardConnectionWarning");
    const firmwareUpdateDialog = document.getElementById("firmwareUpdateDialog") as HTMLDialogElement;
    const fatalErrorWarning = document.getElementById("fatalErrorWarning");
    const fatalErrorWarningText = document.getElementById("fatalErrorWarningText");
    const statusDetails = document.getElementById("statusDetails");
    let websocket: WebSocket | null = null;
    let lastMessageTime = Date.now();
    let serverStartTime: string | null = null;
    let updatePromptAlreadyShown = false;

    const schemaResponse = await fetch("./status/schema");

    if (!schemaResponse.ok) {
        const message = `Failed to load status schema:\n${schemaResponse.statusText}\n${await schemaResponse.text()}`;
        console.error(message);
        throw new Error(message);
    }

    const schema: StatusSchema = await schemaResponse.json();

    function connectWebSocket() {
        websocket = new WebSocket("./websocket/everything");

        websocket.onopen = () => {
            lastMessageTime = Date.now();
            webSocketConnectionWarning.classList.add("hidden");
        };

        websocket.onmessage = (event) => {
            lastMessageTime = Date.now();
            const data: WebsocketValue = JSON.parse(event.data);
            // TODO: Error handling for bad JSON.
            handleWebSocketMessage(data);
        };

        websocket.onclose = (event) => {
            webSocketConnectionWarning.classList.remove("hidden");
            attemptReconnect();
        };

        websocket.onerror = (error) => {
            webSocketConnectionWarning.classList.remove("hidden");
            websocket?.close();
        };
    }

    function handleWebSocketMessage(data: WebsocketValue) {
        if ((data as WebsocketStatusValue).Status) {
            const status = (data as WebsocketStatusValue).Status;

            switch (status.Startup) {
                case "Done":
                    boardConnectionWarning.classList.add("hidden");
                    break;
                case "Waiting":
                case "Update running":
                    boardConnectionWarning.classList.remove("hidden");
                    break;
                case "Update required":
                    boardConnectionWarning.classList.remove("hidden");
                    if (!updatePromptAlreadyShown) {
                        firmwareUpdateDialog.showModal();
                        updatePromptAlreadyShown = true;
                    }
                    break;
            }

            statusDetails.innerText =
                (status.Stepgen_Is_Paused ? "MACHINE IS PAUSED\n\n" : "") +
                "Position:\n" + Object.entries(status.Position).map(([i, v], _) => `${i}: ${v}`).join("\n") +
                "\n\nSwitch states:\n" + Object.entries(status.Switch_Is_High_State).map(([i, v], _) => `${i}: ${v ? "High" : "Low"}`).join("\n") +
                "\n\nTachometers:\n" + Object.entries(status.Tachometer_Frequencies).map(([i, v], _) => `${i}: ${v} Hz`).join("\n");

            plotData[0].push(status.Time);

            let i = 1;
            for (const t of schema.Thermistor_Temperatures) {
                plotData[i].push(status.Thermistor_Temperatures[t]);
                ++i;
            }
            for (const s of schema.Stepper_Temperatures) {
                plotData[i].push(status.Stepper_Temperatures[s]);
                ++i;
            }
            for (const p of schema.Board_Probe_Temperatures) {
                plotData[i].push(status.Board_Probe_Temperatures[p]);
                ++i;
            }
            for (const h of schema.Heater_Powers) {
                plotData[i].push(status.Heater_Powers[h] * 100);
                ++i;
            }

            if (plotData[0].length > 600) {
                for (let x of plotData) {
                    x.shift();
                }
            }

            plot.setData(plotData as uPlot.AlignedData);
        } else if ((data as WebsocketFatalErrorValue).Fatal_Error) {
            if (fatalErrorWarningText.innerText == "") {
                fatalErrorWarningText.innerText = (data as WebsocketFatalErrorValue).Fatal_Error;
            }
            fatalErrorWarning.classList.remove("hidden");
        } else if ((data as WebsocketLogValue).Log) {
            const entry = document.createElement("p");
            entry.innerText = `${new Date().toLocaleTimeString()}: ${(data as WebsocketLogValue).Log}`;
            messageLog.appendChild(entry);
            messageLog.scrollTop = messageLog.scrollHeight;
            if (!logTab.classList.contains("active")) {
                logTab.classList.add("has-update");
            }
        } else if ((data as WebsocketServerStartTimeValue).Server_Start_Time) {
            if (serverStartTime === null) {
                serverStartTime = (data as WebsocketServerStartTimeValue).Server_Start_Time;
            }

            if (serverStartTime != (data as WebsocketServerStartTimeValue).Server_Start_Time) {
                window.location.reload();
            }
        }
    }

    function attemptReconnect() {
        if (websocket) {
            websocket.onopen = null;
            websocket.onmessage = null;
            websocket.onclose = null;
            websocket.onerror = null;
            websocket = null;
        }
        setTimeout(() => connectWebSocket(), 1000);
    }

    function checkInactivity() {
        if (Date.now() - lastMessageTime > 10000) {
            websocket?.close();
        }
    }

    setupPlot(schema);

    connectWebSocket();

    lastMessageTime = Date.now();
    setInterval(checkInactivity, 5000);
};
