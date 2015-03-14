/* expect.h - include file for using the expect library, libexpect.a

Written by: Don Libes, libes@cme.nist.gov, NIST, 12/3/90

Design and implementation of this program was paid for by U.S. tax
dollars.  Therefore it is public domain.  However, the author and NIST
would appreciate credit if this program or parts of it are used.
*/

#include <stdio.h>
#include <setjmp.h>

/* return values */
/* -1 is reserved for system errors */
#define EXP_TIMEOUT			-2
#define EXP_EOF				-3

struct exp_case {		/* case for expect command */
	char *pattern;		/* if pattern not in list */
	int value;		/* value to be returned upon match */
};

#ifdef __cplusplus
extern "C" {
#endif

extern char *exp_match;
extern int exp_match_max;		/* bytes */
extern int exp_timeout;	/* seconds */
extern int exp_pid;			/* process-id of spawned process */
extern int exp_autoallocpty;		/* if TRUE, we do allocation */
extern int exp_pty[2];			/* master is [0], slave is [1] */
extern int exp_disconnected;		/* proc. disc'd from controlling tty */

extern jmp_buf exp_readenv;		/* for interruptable read() */
extern int exp_reading;			/* whether we can longjmp or not */

extern int exp_logfile_all;
extern int exp_loguser;
extern int exp_is_debugging;
extern FILE *exp_debugfile;
extern FILE *exp_logfile;

/* support for Standard C and C++ prototypes */
#ifdef __cplusplus
#define EXP_PROTOTYPES
#define EXP_VARARGS	...
#else
#define EXP_VARARGS	, ...
#ifdef __STDC__
#define EXP_PROTOTYPES
#endif
#endif

#ifdef EXP_PROTOTYPES
#define EXP_PROTO(x)	x
#ifdef EXP_DEFINE_FNS
/* when functions are really being defined, we have to use va_alist as arg */
#define EXP_PROTOV(x)	va_alist
#else
#define EXP_PROTOV(x)	x
#endif
#else
#define EXP_PROTO(x)	()
#define EXP_PROTOV(x)	()
#endif

/* Put double parens around macro args so they all look like a single arg */
/* to preprocessor.  That way, don't need a different macro for functions */
/* with a different number of arguments. */

extern int exp_disconnect EXP_PROTO(());
extern FILE *exp_popen	EXP_PROTO((char *command));

#ifndef EXP_DEFINE_FNS
extern int exp_spawnl	EXP_PROTOV((char *file EXP_VARARGS));
extern int exp_expectl	EXP_PROTOV((int fd EXP_VARARGS));
extern int exp_fexpectl	EXP_PROTOV((FILE *fp EXP_VARARGS));
#endif

extern int exp_spawnv	EXP_PROTO((char *file, char *argv[]));
extern int exp_expectv	EXP_PROTO((int fd, struct exp_case *cases));
extern int exp_fexpectv	EXP_PROTO((FILE *fp, struct exp_case *cases));

#ifdef __cplusplus
}
#endif

