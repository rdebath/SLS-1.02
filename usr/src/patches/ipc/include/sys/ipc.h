#ifndef _SYS_IPC_H
#define _SYS_IPC_H
#include <linux/ipc.h>

#ifdef __cplusplus
extern "C" {
#endif

extern key_t ftok (char *_pathname, char _proc_id);

#ifdef __cplusplus
}
#endif

#endif /* _SYS_IPC_H */


