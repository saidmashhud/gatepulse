.PHONY: all build test test-unit test-c-asan test-c-fuzz test-integration test-e2e \
	c-build c-test eunit lint e2e-integration e2e-traces \
	dev dev-multi docker-up docker-down ops-up ops-down prod-gate \
	release-baseline clean help

# ── Help ──────────────────────────────────────────────────────────────────

help: ## Show this help message
	@awk 'BEGIN{FS=":.*##"} /^[a-zA-Z0-9_-]+:.*##/{printf "  %-22s %s\n",$$1,$$2}' $(MAKEFILE_LIST)

# ── Build ─────────────────────────────────────────────────────────────────

all: build ## Build everything (default)

build: c-build ## Build C daemon + Erlang release
	rebar3 compile

c-build: ## Build the C hl_store daemon
	$(MAKE) -C c

# ── Tests ─────────────────────────────────────────────────────────────────

test: c-test eunit ## Run all tests (C + Erlang EUnit)

test-unit: ## Run Erlang EUnit tests (rebar3 eunit)
	rebar3 eunit

test-c-asan: ## Run C tests with AddressSanitizer (make -C c test-asan)
	$(MAKE) -C c test-asan

test-c-fuzz: ## Run AFL++ fuzz targets for 60s (make -C c fuzz)
	$(MAKE) -C c fuzz

test-integration: ## Run integration test suite (./test/integration.sh)
	./test/integration.sh

test-e2e: ## Run production readiness gate (./test/production-readiness.sh)
	./test/production-readiness.sh

c-test: ## Run C unit tests
	$(MAKE) -C c test

eunit: ## Run Erlang EUnit tests
	rebar3 eunit

e2e-integration: ## Run integration tests
	./test/integration.sh

e2e-traces: ## Run trace propagation tests
	./test/trace-propagation.sh

# ── Lint ──────────────────────────────────────────────────────────────────

lint: ## Run Dialyzer type analysis
	rebar3 dialyzer

# ── Docker ────────────────────────────────────────────────────────────────

docker-up: ## Start HookLine via Docker Compose
	docker compose -f deploy/docker-compose.yml up -d --build

docker-down: ## Stop HookLine containers
	docker compose -f deploy/docker-compose.yml down

ops-up: ## Start full ops stack (HookLine + observability)
	docker compose -f deploy/docker-compose.yml up -d --build

ops-down: ## Stop ops stack
	docker compose -f deploy/docker-compose.yml down

prod-gate: ## Run production readiness gate
	./test/production-readiness.sh

release-baseline: ## Tag a release baseline
	bash scripts/release/tag-baseline.sh

# ── Dev ───────────────────────────────────────────────────────────────────

dev: c-build ## Start single-tenant dev environment
	@echo "Starting hl_store..."
	@mkdir -p /tmp/hl_data
	@./c/build/hl_store /tmp/hl_store.sock /tmp/hl_data &
	@sleep 0.3
	HL_API_KEY=dev-secret rebar3 shell

dev-multi: c-build ## Start two-tenant dev environment (HL_SINGLE_TENANT=false)
	@echo "Starting hl_store..."
	@mkdir -p /tmp/hl_data
	@./c/build/hl_store /tmp/hl_store.sock /tmp/hl_data &
	@sleep 0.3
	HL_SINGLE_TENANT=false HL_API_KEY=dev-secret rebar3 shell

# ── Assets ────────────────────────────────────────────────────────────────

assets: ## Copy console assets to priv/console/
	@mkdir -p apps/hl_api/priv/console
	@echo "Console assets are in apps/hl_api/priv/console/index.html"

# ── Clean ─────────────────────────────────────────────────────────────────

clean: ## Remove build artifacts
	$(MAKE) -C c clean
	rebar3 clean
