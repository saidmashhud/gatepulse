package hookline

import (
	"context"
	"net/http"
)

// Tenant represents a HookLine tenant returned from the admin API.
type Tenant struct {
	ID        string `json:"id"`
	Name      string `json:"name"`
	ApiKey    string `json:"api_key,omitempty"`
	CreatedAt int64  `json:"created_at"`
}

// CreateTenantRequest is the payload for creating a new tenant.
type CreateTenantRequest struct {
	ID   string `json:"id,omitempty"`
	Name string `json:"name,omitempty"`
}

type tenantList struct {
	Items []*Tenant `json:"items"`
}

// TenantsService provides admin operations on tenants.
type TenantsService struct{ c *Client }

// Create creates a new tenant. The response includes the tenant's API key (only shown once).
func (s *TenantsService) Create(ctx context.Context, req CreateTenantRequest) (*Tenant, error) {
	var out Tenant
	if err := s.c.do(ctx, http.MethodPost, "/v1/tenants", req, &out); err != nil {
		return nil, err
	}
	return &out, nil
}

// List returns all tenants.
func (s *TenantsService) List(ctx context.Context) ([]*Tenant, error) {
	var out tenantList
	if err := s.c.do(ctx, http.MethodGet, "/v1/tenants", nil, &out); err != nil {
		return nil, err
	}
	return out.Items, nil
}

// Get returns a single tenant by ID.
func (s *TenantsService) Get(ctx context.Context, id string) (*Tenant, error) {
	var out Tenant
	if err := s.c.do(ctx, http.MethodGet, "/v1/tenants/"+id, nil, &out); err != nil {
		return nil, err
	}
	return &out, nil
}

// Delete removes a tenant by ID.
func (s *TenantsService) Delete(ctx context.Context, id string) error {
	return s.c.do(ctx, http.MethodDelete, "/v1/tenants/"+id, nil, nil)
}

// AdminService groups admin-only sub-services.
type AdminService struct {
	Tenants *TenantsService
}
