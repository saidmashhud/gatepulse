package hookline

import (
	"crypto/hmac"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"strings"
)

// ErrInvalidSignature is returned when webhook signature verification fails.
var ErrInvalidSignature = errors.New("invalid webhook signature")

// WebhookEvent is a verified, parsed HookLine event received at your webhook endpoint.
type WebhookEvent struct {
	ID         string         `json:"id"`
	EventID    string         `json:"event_id"`
	Topic      string         `json:"topic"`
	Payload    map[string]any `json:"payload"`
	OccurredAt int64          `json:"occurred_at"`
	CreatedAt  int64          `json:"created_at"`
}

// VerifyWebhook verifies the HMAC-SHA256 signature of an incoming webhook request
// and returns the parsed event. Returns ErrInvalidSignature if verification fails.
//
// Example:
//
//	func WebhookHandler(w http.ResponseWriter, r *http.Request) {
//	    body, _ := io.ReadAll(r.Body)
//	    event, err := hookline.VerifyWebhook(body, r.Header, os.Getenv("WEBHOOK_SECRET"))
//	    if err != nil {
//	        http.Error(w, "Unauthorized", http.StatusUnauthorized)
//	        return
//	    }
//	    log.Printf("received %s event: %v", event.Topic, event.Payload)
//	    w.WriteHeader(http.StatusOK)
//	}
func VerifyWebhook(body []byte, headers http.Header, secret string) (*WebhookEvent, error) {
	sig := headers.Get("x-gp-signature")
	if sig == "" {
		return nil, fmt.Errorf("%w: missing x-gp-signature header", ErrInvalidSignature)
	}
	ts := headers.Get("x-gp-timestamp")
	if ts == "" {
		return nil, fmt.Errorf("%w: missing x-gp-timestamp header", ErrInvalidSignature)
	}
	// Signing input: "{timestamp_ms}.{body}"  (matches gp_core_signature.erl)
	sigHex := strings.TrimPrefix(sig, "v1=")
	sigBytes, err := hex.DecodeString(sigHex)
	if err != nil {
		return nil, fmt.Errorf("%w: malformed signature", ErrInvalidSignature)
	}
	input := append([]byte(ts+"."), body...)
	mac := hmac.New(sha256.New, []byte(secret))
	mac.Write(input)
	expected := mac.Sum(nil)
	if !hmac.Equal(sigBytes, expected) {
		return nil, ErrInvalidSignature
	}
	var event WebhookEvent
	if err := json.Unmarshal(body, &event); err != nil {
		return nil, fmt.Errorf("parse event body: %w", err)
	}
	return &event, nil
}

// WebhookHandlerFunc returns an http.HandlerFunc that verifies the signature and
// calls onEvent with the parsed event. Responds 401 on bad signature, 200 on success.
//
// Example:
//
//	http.HandleFunc("/webhook", hookline.WebhookHandlerFunc(
//	    os.Getenv("WEBHOOK_SECRET"),
//	    func(event *hookline.WebhookEvent) error {
//	        log.Printf("event: %+v", event)
//	        return nil
//	    },
//	))
func WebhookHandlerFunc(
	secret string,
	onEvent func(*WebhookEvent) error,
) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		body, err := io.ReadAll(r.Body)
		if err != nil {
			http.Error(w, "read error", http.StatusBadRequest)
			return
		}
		event, err := VerifyWebhook(body, r.Header, secret)
		if err != nil {
			if errors.Is(err, ErrInvalidSignature) {
				http.Error(w, "Unauthorized", http.StatusUnauthorized)
				return
			}
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}
		if err := onEvent(event); err != nil {
			http.Error(w, "handler error", http.StatusInternalServerError)
			return
		}
		w.WriteHeader(http.StatusOK)
	}
}
