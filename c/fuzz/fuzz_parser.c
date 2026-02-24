/*
 * fuzz_parser.c — AFL++ fuzzer for the HookLine protocol parser.
 *
 * The Erlang→C boundary over the UNIX socket is the primary attack surface.
 * This fuzzer feeds arbitrary bytes through the protocol parser to uncover
 * crashes, hangs, or memory corruption that ASAN/UBSan will catch.
 *
 * Build (with AFL++ instrumentation):
 *   AFL_USE_ASAN=1 afl-clang-fast -fsanitize=address,undefined \
 *       -g -O1 -o build/fuzz_parser c/fuzz/fuzz_parser.c c/src/*.c \
 *       -I c/src
 *
 * Run (60 seconds per PR):
 *   afl-fuzz -i c/fuzz/seeds -o /tmp/fuzz_out -t 5000 -- ./build/fuzz_parser @@
 *
 * Persistent-mode entry point for maximum throughput:
 *   LLVMFuzzerTestOneInput is also exported for libFuzzer compatibility.
 */

#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "../src/store.h"
#include "../src/index.h"
#include "../src/queue.h"
#include "../src/dlq.h"

/* ─── Protocol parser stub ───────────────────────────────────────────────────
 *
 * The real parser lives in ipc.c (hl_ipc_parse_frame / hl_ipc_dispatch).
 * We expose its header here to avoid duplicating the parser API.
 * If the function is not yet exported, define the prototype below and
 * link against the compiled object files.
 */
#include "../src/ipc.h"

/* ─── Seed corpus directory ──────────────────────────────────────────────────
 *
 * c/fuzz/seeds/ should contain representative valid frames captured from
 * a real Erlang→C session. AFL++ mutates these during fuzzing.
 *
 * Minimum seeds:
 *   seeds/empty       — zero-length frame
 *   seeds/valid_event — a well-formed event frame
 *   seeds/valid_ack   — a well-formed ack frame
 */

/* ─── libFuzzer entry point (also used by AFL++ persistent mode) ──────────── */

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size == 0) return 0;

    /* Feed the raw bytes into the protocol parser.
     * The parser must never crash, hang, or corrupt memory regardless of input.
     * ASAN will catch out-of-bounds reads/writes; UBSan catches UB. */
    hl_ipc_parse_result_t result;
    memset(&result, 0, sizeof(result));

    /* Call the real parser — it may return an error code for invalid input,
     * but must NOT crash. */
    (void)hl_ipc_parse_frame(data, size, &result);

    /* If the parser allocated output memory, free it. */
    if (result.payload != NULL) {
        free(result.payload);
        result.payload = NULL;
    }

    return 0;
}

/* ─── AFL++ main (file-based fuzzing mode) ───────────────────────────────── */

#ifdef __AFL_FUZZ_TESTCASE_LEN
/*
 * AFL++ persistent mode: AFL++ injects __AFL_FUZZ_TESTCASE_BUF and
 * __AFL_FUZZ_TESTCASE_LEN as shared memory regions. This avoids fork()
 * overhead and dramatically increases throughput.
 */
__AFL_FUZZ_INIT();

int main(void)
{
    __AFL_INIT();
    uint8_t *buf = __AFL_FUZZ_TESTCASE_BUF;

    while (__AFL_LOOP(10000)) {
        size_t len = __AFL_FUZZ_TESTCASE_LEN;
        LLVMFuzzerTestOneInput(buf, len);
    }

    return 0;
}

#else /* File-based fallback (standard AFL without persistent mode) */

int main(int argc, char **argv)
{
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("fopen");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fsize = ftell(fp);
    rewind(fp);

    if (fsize <= 0 || fsize > 65536) {
        /* Skip empty files and files larger than 64 KB (not realistic frames) */
        fclose(fp);
        return 0;
    }

    uint8_t *buf = malloc((size_t)fsize);
    if (!buf) { fclose(fp); return 1; }

    size_t n = fread(buf, 1, (size_t)fsize, fp);
    fclose(fp);

    LLVMFuzzerTestOneInput(buf, n);
    free(buf);

    return 0;
}

#endif /* __AFL_FUZZ_TESTCASE_LEN */
