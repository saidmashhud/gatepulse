CC      = gcc
CFLAGS  = -std=c11 -Wall -Wextra -Wpedantic -O2 -D_GNU_SOURCE \
           -I./src
LDFLAGS = -lm

SRC_DIR   = src
TEST_DIR  = tests
FUZZ_DIR  = fuzz
BUILD_DIR = build

SRCS = $(SRC_DIR)/main.c \
       $(SRC_DIR)/store.c \
       $(SRC_DIR)/index.c \
       $(SRC_DIR)/queue.c \
       $(SRC_DIR)/ipc.c \
       $(SRC_DIR)/dlq.c

TEST_SRCS = $(TEST_DIR)/test_store.c \
            $(SRC_DIR)/store.c \
            $(SRC_DIR)/index.c \
            $(SRC_DIR)/queue.c \
            $(SRC_DIR)/ipc.c \
            $(SRC_DIR)/dlq.c

DAEMON  = $(BUILD_DIR)/hl_store
TESTS   = $(BUILD_DIR)/hl_store_tests

.PHONY: all clean test daemon test-asan test-valgrind fuzz-protocol fuzz-log

all: $(BUILD_DIR) daemon

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

daemon: $(BUILD_DIR) $(SRCS)
	$(CC) $(CFLAGS) -o $(DAEMON) $(SRCS) $(LDFLAGS)

test: $(BUILD_DIR) $(TEST_SRCS)
	$(CC) $(CFLAGS) -DUNIT_TEST -o $(TESTS) $(TEST_SRCS) $(LDFLAGS)
	$(TESTS)

# ── Sanitizers ────────────────────────────────────────────────────────────────

test-asan: $(BUILD_DIR) $(TEST_SRCS)
	$(CC) $(CFLAGS) -DUNIT_TEST -fsanitize=address,undefined -fno-omit-frame-pointer -g \
		-o $(BUILD_DIR)/hl_store_tests_asan $(TEST_SRCS) $(LDFLAGS)
	$(BUILD_DIR)/hl_store_tests_asan

test-valgrind: $(BUILD_DIR) $(TEST_SRCS)
	$(CC) $(CFLAGS) -DUNIT_TEST -g -o $(BUILD_DIR)/hl_store_tests_vg $(TEST_SRCS) $(LDFLAGS)
	valgrind --leak-check=full --error-exitcode=1 --track-origins=yes \
		$(BUILD_DIR)/hl_store_tests_vg

# ── Fuzz targets (AFL++) ──────────────────────────────────────────────────────
# Usage:
#   make fuzz-protocol  — fuzz the IPC protocol parser
#   make fuzz-log       — fuzz the append-only log reader
#
# Requires afl-cc (AFL++). Run inside the repo:
#   AFL_SKIP_CPUFREQ=1 afl-fuzz -i fuzz/corpus/protocol -o fuzz/out/protocol \
#       -- ./build/fuzz_protocol @@

fuzz-protocol: $(BUILD_DIR) $(FUZZ_DIR)/fuzz_protocol.c
	afl-cc $(CFLAGS) -fsanitize=address,undefined -g \
		-o $(BUILD_DIR)/fuzz_protocol \
		$(FUZZ_DIR)/fuzz_protocol.c \
		$(SRC_DIR)/ipc.c $(SRC_DIR)/store.c $(SRC_DIR)/index.c \
		$(SRC_DIR)/queue.c $(SRC_DIR)/dlq.c $(LDFLAGS)

fuzz-log: $(BUILD_DIR) $(FUZZ_DIR)/fuzz_log.c
	afl-cc $(CFLAGS) -fsanitize=address,undefined -g \
		-o $(BUILD_DIR)/fuzz_log \
		$(FUZZ_DIR)/fuzz_log.c \
		$(SRC_DIR)/store.c $(SRC_DIR)/index.c $(SRC_DIR)/queue.c \
		$(SRC_DIR)/ipc.c $(SRC_DIR)/dlq.c $(LDFLAGS)

# ── Maintenance ───────────────────────────────────────────────────────────────

clean:
	rm -rf $(BUILD_DIR)

install: daemon
	install -m 755 $(DAEMON) /usr/local/bin/hl_store
