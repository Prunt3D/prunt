export type ServerException = {
    Message: string;
    Fatal: boolean;
};

export type AppEvent = 'connected' | 'disconnected' | 'tick' | 'restarted' | 'log' | 'serverException';

export class PruntWebSocket {
    private ws: WebSocket | null = null;
    private listeners: Record<string, Function[]> = {};
    private shouldReconnect = true;
    private initialStartTime: string | null = null;

    constructor() { }

    on(event: AppEvent, callback: Function) {
        if (!this.listeners[event]) this.listeners[event] = [];
        this.listeners[event].push(callback);
    }

    emit(event: AppEvent, data?: any) {
        if (this.listeners[event]) {
            this.listeners[event].forEach(cb => cb(data));
        }
    }

    connect() {
        const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
        const wsUrl = `${protocol}//${window.location.host}/websocket/everything`;

        this.ws = new WebSocket(wsUrl);

        this.ws.onopen = () => {
            console.log("WebSocket connected");
            this.emit('connected');
        };

        this.ws.onmessage = (event) => {
            try {
                const message = JSON.parse(event.data);

                if (message.Server_Start_Time) {
                    if (this.initialStartTime === null) {
                        this.initialStartTime = message.Server_Start_Time;
                    } else if (this.initialStartTime !== message.Server_Start_Time) {
                        this.emit('restarted', message);
                    }
                }

                if (message.Status_Values) {
                    this.emit('tick', message);
                }

                if (message.Log) {
                    this.emit('log', message.Log);
                }

                if (Object.prototype.hasOwnProperty.call(message, 'Server_Exception')) {
                    this.emit('serverException', message.Server_Exception);
                }

            } catch (err) {
                console.error("Failed to parse WS message", err);
            }
        };

        this.ws.onclose = () => {
            console.log("WebSocket disconnected");
            this.emit('disconnected');
            if (this.shouldReconnect) {
                setTimeout(() => this.connect(), 2000);
            }
        };

        this.ws.onerror = (err) => {
            console.error("WebSocket error", err);
        };
    }

    sendThrottle(intervalTicks: number) {
        if (this.ws && this.ws.readyState === WebSocket.OPEN) {
            this.ws.send(intervalTicks.toString());
        }
    }

    disconnect() {
        this.shouldReconnect = false;
        if (this.ws) this.ws.close();
    }
}

export const wsClient = new PruntWebSocket();
