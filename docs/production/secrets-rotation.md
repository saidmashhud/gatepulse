# Secrets Rotation

## Current status

`POST /v1/admin/rotate-secrets` exists, but full secrets-at-rest rotation workflow is
**not GA** in the current runtime.

Operationally, treat endpoint webhook secrets as sensitive application data in the
primary HookLine store and protect them with infrastructure controls.

## Recommended production controls (current)

1. Use strong, per-endpoint webhook secrets.
2. Rotate endpoint secrets at application level via `PATCH /v1/endpoints/:id`.
3. Protect `HL_DATA_DIR` with filesystem/disk encryption and strict access controls.
4. Restrict `/v1/admin/*` endpoints to trusted networks/operators.
5. Store bootstrap secrets (`HL_API_KEY`, service tokens) in a real secret manager.

## Endpoint-level secret rotation (supported)

```bash
curl -X PATCH http://localhost:8080/v1/endpoints/<endpoint_id> \
  -H "Authorization: Bearer $HL_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{"secret":"new-strong-secret"}'
```

After rotating:

1. Update the consumer-side verification secret.
2. Keep old secret briefly only if your rollout requires overlap.
3. Monitor delivery failures/signature mismatches during the transition.

## Future direction

The project keeps `HL_MASTER_KEY` and rotate API surface for future hardening, but
you should not rely on them today as a complete at-rest key management system.
