#ifndef _SYS_SEM_H
#define _SYS_SEM_H
#include <sys/ipc.h>
#include <linux/sem.h>
#include <asm/bitops.h>
/*
 * bitops.h:
 * set_bit, clear_bit, test_bit
 * all return the value of the bit prior to alteration.
 * It seems they are more useful than sysv sems if you 
 * dont have a need for semaphore arrays.
 */

#ifdef __cplusplus
extern "C" {
#endif

extern int semget (key_t _key, int _nsems, int _semflg);
extern int semop (int _semid, struct sembuf *_sops, unsigned _nsops);
extern int semctl (int _semid, int _semnum, int _cmd, union semun _arg);

#ifdef __cplusplus
}
#endif

#endif /* _SYS_SEM_H */



