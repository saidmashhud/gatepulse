# Stage 1: Build C daemon
FROM gcc:13 AS c-builder

WORKDIR /build/c
COPY c/ .
RUN make daemon

# Stage 2: Build Erlang release
FROM erlang:28 AS erl-builder

WORKDIR /build
COPY rebar.config .
COPY apps/ apps/
COPY config/ config/
COPY openapi/ openapi/

# Install rebar3
RUN curl -fsSL https://s3.amazonaws.com/rebar3/rebar3 -o /usr/local/bin/rebar3 \
    && chmod +x /usr/local/bin/rebar3

# Build release
RUN rebar3 as prod release

# Stage 3: Runtime image — use erlang:28 to match the build environment exactly
FROM erlang:28

RUN apt-get update && apt-get install -y --no-install-recommends curl \
    && rm -rf /var/lib/apt/lists/*

# Create directories
RUN mkdir -p /opt/hookline /var/lib/hookline /run/hookline

# Copy C daemon
COPY --from=c-builder /build/c/build/hl_store /usr/local/bin/hl_store

# Copy Erlang release
COPY --from=erl-builder /build/_build/prod/rel/hookline /opt/hookline

# Copy entrypoint
COPY docker-entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh

EXPOSE 8080

VOLUME ["/var/lib/hookline"]

ENV HL_PORT=8080 \
    HL_LISTEN_ADDR=0.0.0.0 \
    HL_STORE_SOCKET=/run/hookline/hl_store.sock \
    HL_DATA_DIR=/var/lib/hookline \
    HL_SINGLE_TENANT=true \
    HL_API_KEY=change-me \
    HL_TENANT_ID=default \
    HL_DELIVERY_WORKERS=16 \
    HL_STORE_POOL_SIZE=8

ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
