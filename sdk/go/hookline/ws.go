package hookline

import (
	"encoding/json"
	"errors"
	"fmt"
	"strings"
	"sync"
	"sync/atomic"
	"time"

	"github.com/gorilla/websocket"
)

// WSEvent is an event message received over the WebSocket.
type WSEvent struct {
	Type      string                 `json:"type"`
	ID        string                 `json:"id"`
	Topic     string                 `json:"topic"`
	Payload   map[string]interface{} `json:"payload"`
	Timestamp int64                  `json:"timestamp"`
}

// PresenceUpdate is a presence notification received over the WebSocket.
type PresenceUpdate struct {
	Type   string   `json:"type"`
	Event  string   `json:"event"`
	UserID string   `json:"user_id"`
	Topics []string `json:"topics,omitempty"`
	Ts     int64    `json:"ts"`
}

// AckMessage is an acknowledgement for a published message.
type AckMessage struct {
	Type     string `json:"type"`
	ClientID string `json:"client_id,omitempty"`
	EventID  string `json:"event_id"`
}

// WSOptions configures a WSClient.
type WSOptions struct {
	Topics     []string
	UserID     string
	OnEvent    func(WSEvent)
	OnPresence func(PresenceUpdate)
	OnAck      func(AckMessage)
	OnReconnect func()
	OnError    func(error)
}

// WSClient is a goroutine-safe WebSocket client with auto-reconnect.
type WSClient struct {
	url  string
	opts WSOptions

	mu   sync.Mutex
	conn *websocket.Conn

	closed         int32 // atomic; 1 = permanently disconnected
	reconnectDelay time.Duration
}

// newWSClient creates a new WSClient. Use Client.Connect instead of calling
// this directly.
func newWSClient(baseURL, apiKey string, opts WSOptions) *WSClient {
	wsURL := strings.Replace(baseURL, "http", "ws", 1)
	wsURL = fmt.Sprintf("%s/v1/ws?token=%s", wsURL, apiKey)
	if len(opts.Topics) > 0 {
		wsURL += "&topics=" + strings.Join(opts.Topics, ",")
	}
	return &WSClient{
		url:            wsURL,
		opts:           opts,
		reconnectDelay: time.Second,
	}
}

// Connect dials the server and starts the read loop in a goroutine.
func (c *WSClient) Connect() error {
	conn, _, err := websocket.DefaultDialer.Dial(c.url, nil)
	if err != nil {
		return err
	}
	c.mu.Lock()
	c.conn = conn
	c.mu.Unlock()
	go c.readLoop()
	return nil
}

// Publish sends a publish message to the server.
func (c *WSClient) Publish(topic string, payload interface{}, clientID string) error {
	return c.send(map[string]interface{}{
		"type":      "publish",
		"topic":     topic,
		"payload":   payload,
		"client_id": clientID,
	})
}

// Subscribe adds topics to the server-side filter.
func (c *WSClient) Subscribe(topics []string) error {
	return c.send(map[string]interface{}{
		"type":   "subscribe",
		"topics": topics,
	})
}

// Unsubscribe removes topics from the server-side filter.
func (c *WSClient) Unsubscribe(topics []string) error {
	return c.send(map[string]interface{}{
		"type":   "unsubscribe",
		"topics": topics,
	})
}

// MarkRead sends a read receipt for an event.
func (c *WSClient) MarkRead(eventID string) error {
	return c.send(map[string]interface{}{
		"type":     "read",
		"event_id": eventID,
	})
}

// Disconnect permanently closes the connection (no auto-reconnect).
func (c *WSClient) Disconnect() {
	atomic.StoreInt32(&c.closed, 1)
	c.mu.Lock()
	if c.conn != nil {
		_ = c.conn.Close()
	}
	c.mu.Unlock()
}

// send serialises msg to JSON and writes it as a text frame.
func (c *WSClient) send(msg interface{}) error {
	data, err := json.Marshal(msg)
	if err != nil {
		return err
	}
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.conn == nil {
		return errors.New("hookline: not connected")
	}
	return c.conn.WriteMessage(websocket.TextMessage, data)
}

func (c *WSClient) readLoop() {
	for {
		c.mu.Lock()
		conn := c.conn
		c.mu.Unlock()

		_, data, err := conn.ReadMessage()
		if err != nil {
			if atomic.LoadInt32(&c.closed) == 1 {
				return
			}
			if c.opts.OnError != nil {
				c.opts.OnError(err)
			}
			c.scheduleReconnect()
			return
		}
		c.dispatch(data)
	}
}

func (c *WSClient) dispatch(data []byte) {
	var raw map[string]json.RawMessage
	if err := json.Unmarshal(data, &raw); err != nil {
		return
	}
	var msgType string
	if t, ok := raw["type"]; ok {
		_ = json.Unmarshal(t, &msgType)
	}
	switch msgType {
	case "event":
		if c.opts.OnEvent != nil {
			var evt WSEvent
			if json.Unmarshal(data, &evt) == nil {
				c.opts.OnEvent(evt)
			}
		}
	case "presence":
		if c.opts.OnPresence != nil {
			var p PresenceUpdate
			if json.Unmarshal(data, &p) == nil {
				c.opts.OnPresence(p)
			}
		}
	case "ack":
		if c.opts.OnAck != nil {
			var ack AckMessage
			if json.Unmarshal(data, &ack) == nil {
				c.opts.OnAck(ack)
			}
		}
	}
}

func (c *WSClient) scheduleReconnect() {
	delay := c.reconnectDelay
	time.AfterFunc(delay, func() {
		if atomic.LoadInt32(&c.closed) == 1 {
			return
		}
		conn, _, err := websocket.DefaultDialer.Dial(c.url, nil)
		if err != nil {
			if c.opts.OnError != nil {
				c.opts.OnError(err)
			}
			// double the delay and retry again
			c.reconnectDelay = min(c.reconnectDelay*2, 30*time.Second)
			c.scheduleReconnect()
			return
		}
		c.mu.Lock()
		c.conn = conn
		c.mu.Unlock()
		c.reconnectDelay = time.Second // reset on success
		if c.opts.OnReconnect != nil {
			c.opts.OnReconnect()
		}
		go c.readLoop()
	})
	c.reconnectDelay = min(delay*2, 30*time.Second)
}

func min(a, b time.Duration) time.Duration {
	if a < b {
		return a
	}
	return b
}
