/**
 * HookLine WebSocket client — bidirectional real-time transport.
 */

export interface WSEvent {
  type: "event";
  id: string;
  topic: string;
  payload: Record<string, unknown>;
  timestamp: number;
}

export interface PresenceUpdate {
  type: "presence";
  event: "online" | "offline";
  user_id: string;
  topics?: string[];
  ts: number;
}

export interface AckMessage {
  type: "ack";
  client_id?: string;
  event_id: string;
}

export interface HooklineWSOptions {
  topics?: string[];
  userId?: string;
  onEvent?: (event: WSEvent) => void;
  onPresence?: (update: PresenceUpdate) => void;
  onAck?: (ack: AckMessage) => void;
  onReconnect?: () => void;
  onError?: (err: Error) => void;
}

export class HooklineWS {
  private ws: WebSocket | null = null;
  private reconnectDelay = 1000;
  private shouldReconnect = true;

  constructor(private url: string, private opts: HooklineWSOptions) {}

  connect(): this {
    this.ws = new WebSocket(this.url);
    this.ws.onmessage = (e) => {
      try {
        this.handleMessage(JSON.parse(e.data as string));
      } catch {
        // ignore malformed frames
      }
    };
    this.ws.onclose = () => this.scheduleReconnect();
    this.ws.onerror = (e) => this.opts.onError?.(new Error(String(e)));
    return this;
  }

  publish(topic: string, payload: unknown, clientId?: string): void {
    this.send({ type: "publish", topic, payload, client_id: clientId });
  }

  subscribe(topics: string[]): void {
    this.send({ type: "subscribe", topics });
  }

  unsubscribe(topics: string[]): void {
    this.send({ type: "unsubscribe", topics });
  }

  markRead(eventId: string): void {
    this.send({ type: "read", event_id: eventId });
  }

  disconnect(): void {
    this.shouldReconnect = false;
    this.ws?.close();
  }

  private handleMessage(msg: Record<string, unknown>): void {
    if (msg.type === "event") this.opts.onEvent?.(msg as unknown as WSEvent);
    if (msg.type === "presence") this.opts.onPresence?.(msg as unknown as PresenceUpdate);
    if (msg.type === "ack") this.opts.onAck?.(msg as unknown as AckMessage);
  }

  private send(msg: object): void {
    if (this.ws?.readyState === WebSocket.OPEN) {
      this.ws.send(JSON.stringify(msg));
    }
  }

  private scheduleReconnect(): void {
    if (!this.shouldReconnect) return;
    setTimeout(() => {
      this.connect();
      this.opts.onReconnect?.();
    }, this.reconnectDelay);
    this.reconnectDelay = Math.min(this.reconnectDelay * 2, 30_000);
  }
}
