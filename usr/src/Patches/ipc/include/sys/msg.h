#ifndef _SYS_MSG_H
#define _SYS_MSG_H
#include <sys/ipc.h>

/* msgrcv options */
#define MSG_NOERROR     010000  /* no error if message is too big */
#define MSG_EXCEPT      020000  /* recv any meg except of specified type.*/

/* upper byte flags in msg_perm.mode */
#define MSG_RWAIT       001000  /* reader is waiting for a message */
#define MSG_WWAIT       002000  /* writer is waiting to send */
/* #define MSG_LOCKED     IPC_LOCKED  message is locked */

/* one msg structure for each message */
struct msg {
    struct msg *msg_next;   /* next message on queue */
    long msg_type;          
    char *msg_spot;         /* message text map address */
    short msg_ts;           /* message text size */
};

/* one msqid structure for each queue on the system */
struct msqid_ds {
    struct ipc_perm msg_perm;
    struct msg *msg_first;  /* first message on queue */
    struct msg *msg_last;   /* last message in queue */
    time_t msg_stime;       /* last msgsnd time */
    time_t msg_rtime;       /* last msgrcv time */
    time_t msg_ctime;       /* last change time */
    ushort msg_cbytes;      /* current number of bytes on queue */
    ushort msg_qnum;        /* number of messages in queue */
    ushort msg_qbytes;      /* max number of bytes on queue */
    ushort msg_lspid;       /* pid of last msgsnd */
    ushort msg_lrpid;       /* last receive pid */
};

/* message buffer for msgsnd and msgrcv calls */
struct msgbuf {
    long mtype;         /* type of message */
    char mtext[1];      /* message text .. why is this not a ptr?*/
};


#ifdef __cplusplus
extern "C" {
#endif 

extern int msgget (key_t key, int msgflg);
extern int msgsnd (int msqid, struct msgbuf *msgp, int msgsz, int msgflg);
extern int msgrcv (int msqid, struct msgbuf *msgp, int msgsz, long msgtyp,
			int msgflg);
extern int msgctl (int msqid, int cmd, struct msqid_ds *buf);
#ifdef __cplusplus
}
#endif

#endif /* _SYS_MSG_H */




