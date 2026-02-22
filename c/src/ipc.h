#ifndef HL_IPC_H
#define HL_IPC_H

#include <stdint.h>
#include <stddef.h>

#define HL_IPC_MAX_MSG  (8 * 1024 * 1024)  /* 8MB */

/* Length-prefixed framing: [length:4 LE][json:N] */

/* Read a complete JSON message from fd; caller frees *buf */
int  hl_ipc_recv(int fd, uint8_t **buf, uint32_t *len);

/* Send a JSON message to fd */
int  hl_ipc_send(int fd, const uint8_t *buf, uint32_t len);

/* Send a string (convenience) */
int  hl_ipc_send_str(int fd, const char *str);

#endif /* HL_IPC_H */
