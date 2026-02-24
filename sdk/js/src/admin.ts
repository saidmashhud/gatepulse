/**
 * HookLine Admin API client — tenant management.
 * The bearer token must be an admin API key.
 */

export interface Tenant {
  id: string;
  name: string;
  api_key?: string;
  created_at: number;
}

export interface CreateTenantRequest {
  id?: string;
  name?: string;
}

type RequestFn = <T>(method: string, path: string, body?: unknown) => Promise<T>;

class TenantsAdmin {
  constructor(private readonly _request: RequestFn) {}

  create(req: CreateTenantRequest = {}): Promise<Tenant> {
    return this._request("POST", "/v1/tenants", req);
  }

  list(): Promise<{ items: Tenant[] }> {
    return this._request("GET", "/v1/tenants");
  }

  get(id: string): Promise<Tenant> {
    return this._request("GET", `/v1/tenants/${id}`);
  }

  delete(id: string): Promise<void> {
    return this._request("DELETE", `/v1/tenants/${id}`);
  }
}

export class AdminClient {
  readonly tenants: TenantsAdmin;

  constructor(requestFn: RequestFn) {
    this.tenants = new TenantsAdmin(requestFn);
  }
}
