/* command.h - definitions for expect commands

Written by: Don Libes, NIST, 2/6/90

Design and implementation of this program was paid for by U.S. tax
dollars.  Therefore it is public domain.  However, the author and NIST
would appreciate credit if this program or parts of it are used.
*/

#define update_master(x)	exp_update_master(interp,x)
struct f *exp_update_master();

#define get_var(x)	exp_get_var(interp,x)
char *exp_get_var();

int get_timeout();

extern int default_match_max;
int exp_eval_with_one_arg();
void exp_lowmemcpy();


#define USER_SPAWN_ID		0
#define USER_SPAWN_ID_LIT	"0"
/* only defined on FD_VALID descriptors */
#define is_user(x)	((x == 0) && (fs[x].flags & FD_USER))
#define f_is_user(x)	((x == fs) && (f->flags & FD_USER))

/* fd flags */
#define FD_VALID	0x1	/* valid entry in fds table */
#define FD_CLOSED	0x2	/* has been closed */
#define FD_USER		0x4	/* stdin or stdout */
#define FD_SYSWAITED	0x8	/* wait() has been called */
#define FD_USERWAITED	0x10	/* expect's wait has been called */

/* each process is associated with a 'struct f'.  An array of these ('fs') */
/* keeps track of all processes.  They are indexed by the true fd to the */
/* master side of the pty */
struct f {
	int pid;
	char *buffer;	/* input buffer */
	char *lower;	/* input buffer in lowercase */
	int size;	/* current size of data */
	int msize;	/* size of buffer */
	int umsize;	/* user view of size of buffer */
	int flags;	/* see above */
	int wait;	/* raw status from wait() */
	int printed;	/* # of characters written to stdout (if logging on) */
			/* but not actually returned via a match yet */
	int echoed;	/* additional # of chars (beyond "printed" above) */
			/* echoed back but not actually returned via a match */
			/* yet.  This supports interact -echo */
	int key;	/* unique id that identifies what command instance */
			/* last touched this buffer */
	int force_read;	/* force read to occur (even if buffer already has */
			/* data).  This supports interact CAN_MATCH */
	int armed;	/* If Tk_CreateFileHandler is active */
};
extern struct f *fs;

struct f *fd_to_f();
int f_adjust();

#define exp_deleteProc (void (*)())0

extern int expect_key;

extern int exp_disconnected;

void exp_create_commands();
void exp_init_pty();
void exp_init_expect();
void exp_init_spawn();
void exp_init_trap();
void exp_init_unit_random();
void exp_init_sig();

void exp_generic_sighandler();

