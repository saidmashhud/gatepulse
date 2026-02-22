#!/bin/sh
set -e

SOCKET="${HL_STORE_SOCKET:-/run/hookline/hl_store.sock}"
DATA_DIR="${HL_DATA_DIR:-/var/lib/hookline}"

echo "Starting hl_store daemon..."
hl_store "${SOCKET}" "${DATA_DIR}" &
HL_STORE_PID=$!

# Wait for socket to appear
for i in $(seq 1 30); do
    if [ -S "${SOCKET}" ]; then
        echo "hl_store ready."
        break
    fi
    sleep 0.2
done

if [ ! -S "${SOCKET}" ]; then
    echo "ERROR: hl_store did not start in time."
    exit 1
fi

echo "Starting HookLine Erlang node..."
exec /opt/hookline/bin/hookline foreground
