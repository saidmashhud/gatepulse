// GatePulse webhook receiver — Go + chi router
//
// Build: go build -o receiver .
// Run:   WEBHOOK_SECRET=my-secret ./receiver
package main

import (
	"encoding/json"
	"log"
	"net/http"
	"os"

	"github.com/go-chi/chi/v5"
	"github.com/go-chi/chi/v5/middleware"
	"github.com/gatepulse/gatepulse-go/gatepulse"
)

var secret = os.Getenv("WEBHOOK_SECRET")

func main() {
	r := chi.NewRouter()
	r.Use(middleware.Logger)
	r.Use(middleware.Recoverer)

	r.Get("/healthz", func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte("ok"))
	})

	// Use the built-in handler builder for simple cases
	r.Post("/webhook", gatepulse.WebhookHandlerFunc(secret, handleEvent))

	port := "3001"
	log.Printf("GatePulse receiver on http://localhost:%s/webhook", port)
	log.Fatal(http.ListenAndServe(":"+port, r))
}

func handleEvent(event *gatepulse.WebhookEvent) error {
	log.Printf("Event received: topic=%s id=%s", event.Topic, event.ID)
	b, _ := json.MarshalIndent(event.Payload, "  ", "  ")
	log.Printf("  payload: %s", b)
	return nil
}
