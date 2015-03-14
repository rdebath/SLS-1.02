#ifndef _SYS_SHM_H_
#define _SYS_SHM_H_
#include <sys/ipc.h>
#include <linux/shm.h>

#ifdef __cplusplus
extern "C" {
#endif

extern int shmctl (int _shmid, int _cmd, struct shmid_ds *_buf);
extern int shmget(key_t _key, int _size, int _flag);
extern char *shmat (int _shmid, char *_shmaddr, int _shmflg);
extern int shmdt (char *_shmaddr);

#ifdef __cplusplus
}
#endif

#endif /* _SYS_SHM_H_ */






