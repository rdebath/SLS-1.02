#ifndef _SYS_MSG_H
#define _SYS_MSG_H
#include <sys/ipc.h>
#include <linux/msg.h>

#ifdef __cplusplus
extern "C" {
#endif 

extern int msgget (key_t _key, int _msgflg);
extern int msgsnd (int _msqid, struct msgbuf *_msgp, int _msgsz, int _msgflg);
extern int msgrcv (int _msqid, struct msgbuf *_msgp, int _msgsz, long _msgtyp,
			int _msgflg);
extern int msgctl (int _msqid, int _cmd, struct msqid_ds *_buf);
#ifdef __cplusplus
}
#endif

#endif /* _SYS_MSG_H */




