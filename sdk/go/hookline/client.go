// Package hookline provides a Go client for the HookLine webhook delivery service.
//
// Usage:
//
//	client := hookline.New("http://localhost:8080", "my-api-key")
//	event, err := client.Events.Publish(ctx, hookline.PublishRequest{
//	    Topic:   "orders.created",
//	    Payload: map[string]any{"orderId": "123"},
//	})
package hookline

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"strconv"
	"strings"
	"time"
)

// Client is the HookLine API client.
type Client struct {
	baseURL    string
	apiKey     string
	httpClient *http.Client

	Events        *EventsService
	Endpoints     *EndpointsService
	Subscriptions *SubscriptionsService
	DLQ           *DLQService
	Admin         *AdminService
}

// New creates a HookLine client.
func New(baseURL, apiKey string) *Client {
	c := &Client{
		baseURL: strings.TrimRight(baseURL, "/"),
		apiKey:  apiKey,
		httpClient: &http.Client{
			Timeout: 30 * time.Second,
		},
	}
	c.Events = &EventsService{c}
	c.Endpoints = &EndpointsService{c}
	c.Subscriptions = &SubscriptionsService{c}
	c.DLQ = &DLQService{c}
	c.Admin = &AdminService{Tenants: &TenantsService{c}}
	return c
}

// APIError is returned when the API responds with a non-2xx status.
type APIError struct {
	Status  int
	Message string
	Body    []byte
}

func (e *APIError) Error() string {
	return fmt.Sprintf("HookLine API error %d: %s", e.Status, e.Message)
}

func (c *Client) do(ctx context.Context, method, path string, body any, out any) error {
	var reqBody io.Reader
	if body != nil {
		b, err := json.Marshal(body)
		if err != nil {
			return fmt.Errorf("marshal request: %w", err)
		}
		reqBody = bytes.NewReader(b)
	}
	req, err := http.NewRequestWithContext(ctx, method, c.baseURL+path, reqBody)
	if err != nil {
		return err
	}
	req.Header.Set("Authorization", "Bearer "+c.apiKey)
	if body != nil {
		req.Header.Set("Content-Type", "application/json")
	}
	resp, err := c.httpClient.Do(req)
	if err != nil {
		return err
	}
	defer resp.Body.Close()
	respBytes, _ := io.ReadAll(resp.Body)
	if resp.StatusCode < 200 || resp.StatusCode >= 300 {
		msg := ""
		var errBody map[string]any
		if json.Unmarshal(respBytes, &errBody) == nil {
			if m, ok := errBody["message"].(string); ok {
				msg = m
			} else if m, ok := errBody["error"].(string); ok {
				msg = m
			}
		}
		return &APIError{Status: resp.StatusCode, Message: msg, Body: respBytes}
	}
	if out != nil && len(respBytes) > 0 {
		return json.Unmarshal(respBytes, out)
	}
	return nil
}

// ── Events ────────────────────────────────────────────────────────────────────

type EventsService struct{ c *Client }

type PublishRequest struct {
	Topic          string         `json:"topic"`
	Payload        map[string]any `json:"payload,omitempty"`
	IdempotencyKey string         `json:"idempotency_key,omitempty"`
	OccurredAt     int64          `json:"occurred_at,omitempty"`
}

type Event struct {
	ID         string         `json:"id"`
	EventID    string         `json:"event_id"`
	TenantID   string         `json:"tenant_id"`
	Topic      string         `json:"topic"`
	Payload    map[string]any `json:"payload"`
	OccurredAt int64          `json:"occurred_at"`
	CreatedAt  int64          `json:"created_at"`
}

type ListEventsParams struct {
	Limit  int
	Cursor string
	FromMs int64
	ToMs   int64
	Topic  string
}

type EventList struct {
	Items      []Event `json:"items"`
	Count      int     `json:"count"`
	NextCursor string  `json:"next_cursor,omitempty"`
}

func (s *EventsService) Publish(ctx context.Context, req PublishRequest) (*Event, error) {
	var out Event
	if err := s.c.do(ctx, http.MethodPost, "/v1/events", req, &out); err != nil {
		return nil, err
	}
	return &out, nil
}

func (s *EventsService) List(ctx context.Context, p ListEventsParams) (*EventList, error) {
	qs := url.Values{}
	if p.Limit > 0 {
		qs.Set("limit", strconv.Itoa(p.Limit))
	}
	if p.Cursor != "" {
		qs.Set("cursor", p.Cursor)
	}
	if p.FromMs > 0 {
		qs.Set("from_ms", strconv.FormatInt(p.FromMs, 10))
	}
	if p.ToMs > 0 {
		qs.Set("to_ms", strconv.FormatInt(p.ToMs, 10))
	}
	if p.Topic != "" {
		qs.Set("topic", p.Topic)
	}
	path := "/v1/events"
	if len(qs) > 0 {
		path += "?" + qs.Encode()
	}
	var out EventList
	if err := s.c.do(ctx, http.MethodGet, path, nil, &out); err != nil {
		return nil, err
	}
	return &out, nil
}

func (s *EventsService) Get(ctx context.Context, eventID string) (*Event, error) {
	var out Event
	if err := s.c.do(ctx, http.MethodGet, "/v1/events/"+eventID, nil, &out); err != nil {
		return nil, err
	}
	return &out, nil
}

// ── Endpoints ─────────────────────────────────────────────────────────────────

type EndpointsService struct{ c *Client }

type CreateEndpointRequest struct {
	URL           string            `json:"url"`
	Name          string            `json:"name,omitempty"`
	Secret        string            `json:"secret,omitempty"`
	Enabled       *bool             `json:"enabled,omitempty"`
	MaxInFlight   int               `json:"max_in_flight,omitempty"`
	RateLimitRPS  int               `json:"rate_limit_rps,omitempty"`
	TimeoutMs     int               `json:"timeout_ms,omitempty"`
	Headers       map[string]string `json:"headers,omitempty"`
}

type Endpoint struct {
	EndpointID string `json:"endpoint_id"`
	Name       string `json:"name"`
	URL        string `json:"url"`
	Enabled    bool   `json:"enabled"`
	TenantID   string `json:"tenant_id"`
	CreatedAt  int64  `json:"created_at"`
}

type EndpointList struct {
	Items []Endpoint `json:"items"`
	Count int        `json:"count"`
}

func (s *EndpointsService) Create(ctx context.Context, req CreateEndpointRequest) (*Endpoint, error) {
	var out Endpoint
	if err := s.c.do(ctx, http.MethodPost, "/v1/endpoints", req, &out); err != nil {
		return nil, err
	}
	return &out, nil
}

func (s *EndpointsService) List(ctx context.Context) (*EndpointList, error) {
	var out EndpointList
	if err := s.c.do(ctx, http.MethodGet, "/v1/endpoints", nil, &out); err != nil {
		return nil, err
	}
	return &out, nil
}

func (s *EndpointsService) Get(ctx context.Context, endpointID string) (*Endpoint, error) {
	var out Endpoint
	if err := s.c.do(ctx, http.MethodGet, "/v1/endpoints/"+endpointID, nil, &out); err != nil {
		return nil, err
	}
	return &out, nil
}

func (s *EndpointsService) Delete(ctx context.Context, endpointID string) error {
	return s.c.do(ctx, http.MethodDelete, "/v1/endpoints/"+endpointID, nil, nil)
}

// ── Subscriptions ─────────────────────────────────────────────────────────────

type SubscriptionsService struct{ c *Client }

type CreateSubscriptionRequest struct {
	EndpointID   string `json:"endpoint_id"`
	TopicPattern string `json:"topic_pattern,omitempty"`
}

type Subscription struct {
	SubscriptionID string `json:"subscription_id"`
	EndpointID     string `json:"endpoint_id"`
	TopicPattern   string `json:"topic_pattern"`
	TenantID       string `json:"tenant_id"`
	CreatedAt      int64  `json:"created_at"`
}

type SubscriptionList struct {
	Items []Subscription `json:"items"`
	Count int            `json:"count"`
}

func (s *SubscriptionsService) Create(ctx context.Context, req CreateSubscriptionRequest) (*Subscription, error) {
	if req.TopicPattern == "" {
		req.TopicPattern = "#"
	}
	var out Subscription
	if err := s.c.do(ctx, http.MethodPost, "/v1/subscriptions", req, &out); err != nil {
		return nil, err
	}
	return &out, nil
}

func (s *SubscriptionsService) List(ctx context.Context) (*SubscriptionList, error) {
	var out SubscriptionList
	if err := s.c.do(ctx, http.MethodGet, "/v1/subscriptions", nil, &out); err != nil {
		return nil, err
	}
	return &out, nil
}

func (s *SubscriptionsService) Delete(ctx context.Context, subscriptionID string) error {
	return s.c.do(ctx, http.MethodDelete, "/v1/subscriptions/"+subscriptionID, nil, nil)
}

// ── DLQ ──────────────────────────────────────────────────────────────────────

type DLQService struct{ c *Client }

type DLQEntry struct {
	JobID        string `json:"job_id"`
	EventID      string `json:"event_id"`
	EndpointID   string `json:"endpoint_id"`
	Reason       string `json:"reason"`
	AttemptCount int    `json:"attempt_count"`
	CreatedAt    int64  `json:"created_at"`
}

type DLQList struct {
	Items []DLQEntry `json:"items"`
	Count int        `json:"count"`
}

func (s *DLQService) List(ctx context.Context) (*DLQList, error) {
	var out DLQList
	if err := s.c.do(ctx, http.MethodGet, "/v1/dlq", nil, &out); err != nil {
		return nil, err
	}
	return &out, nil
}

func (s *DLQService) Requeue(ctx context.Context, jobID string) error {
	return s.c.do(ctx, http.MethodPost, "/v1/dlq/"+jobID+"/requeue", struct{}{}, nil)
}

func (s *DLQService) Delete(ctx context.Context, jobID string) error {
	return s.c.do(ctx, http.MethodDelete, "/v1/dlq/"+jobID, nil, nil)
}

// ── WebSocket ─────────────────────────────────────────────────────────────────

// Connect opens a WebSocket connection to HookLine and returns a WSClient.
// The caller must call WSClient.Connect() on the returned value (or use
// ConnectWS which does both).
func (c *Client) Connect(opts WSOptions) *WSClient {
	return newWSClient(c.baseURL, c.apiKey, opts)
}

// ConnectWS opens a WebSocket connection and starts the read loop.
// Returns the connected WSClient or an error.
func (c *Client) ConnectWS(opts WSOptions) (*WSClient, error) {
	wsc := newWSClient(c.baseURL, c.apiKey, opts)
	if err := wsc.Connect(); err != nil {
		return nil, err
	}
	return wsc, nil
}
