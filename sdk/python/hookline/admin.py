"""HookLine Admin API — tenant management."""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    from .client import HookLineClient


@dataclass
class Tenant:
    id: str
    name: str
    api_key: str | None
    created_at: int

    @classmethod
    def _from_dict(cls, d: dict) -> "Tenant":
        return cls(
            id=d["id"],
            name=d.get("name", ""),
            api_key=d.get("api_key"),
            created_at=d.get("created_at", 0),
        )


class TenantsAdmin:
    def __init__(self, client: "HookLineClient") -> None:
        self._client = client

    def create(self, id: str | None = None, name: str | None = None) -> Tenant:
        """Create a new tenant. Returns the tenant including its api_key (shown once)."""
        payload: dict[str, Any] = {}
        if id is not None:
            payload["id"] = id
        if name is not None:
            payload["name"] = name
        data = self._client._request("POST", "/v1/tenants", payload)
        return Tenant._from_dict(data)

    def list(self) -> list[Tenant]:
        """Return all tenants."""
        data = self._client._request("GET", "/v1/tenants")
        return [Tenant._from_dict(t) for t in data.get("items", [])]

    def get(self, id: str) -> Tenant:
        """Return a single tenant by ID."""
        data = self._client._request("GET", f"/v1/tenants/{id}")
        return Tenant._from_dict(data)

    def delete(self, id: str) -> None:
        """Delete a tenant by ID."""
        self._client._request("DELETE", f"/v1/tenants/{id}")


class AdminClient:
    def __init__(self, client: "HookLineClient") -> None:
        self.tenants = TenantsAdmin(client)
