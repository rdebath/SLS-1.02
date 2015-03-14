/*
 * This file is part of the Minicom Communications Program,
 * written by Miquel van Smoorenburg 1991/1992.
 *
 * minicom.h  -  constants, defaults, globals etc.
 */

#include "config.h"

#define VT100		1
#define MINIX		2
#define ANSI		3

#define A_OK_EXIST	1
#define A_OK_NOTEXIST	2

/* This one's for Coherent */
#ifdef _COHERENT
#  include <access.h>
#  define W_OK AWRITE
#  define R_OK AREAD
#endif

/* And for ancient systems like SVR2 */
#ifndef W_OK
#  define W_OK 2
#  define R_OK 4
#endif
#ifndef SEEK_SET
#  define SEEK_SET 0
#  define SEEK_END 2
#endif

#ifndef EXTERN
# define EXTERN extern
#endif

EXTERN char stdattr;	/* Standard attribute */

EXTERN WIN *us;		/* User screen */
EXTERN WIN *st;		/* Status Line */

EXTERN int real_uid;	/* Real uid */
EXTERN int real_gid;	/* Real gid */
EXTERN short terminal;	/* terminal type */
EXTERN time_t online;	/* Time online in minutes */
EXTERN short portfd;	/* File descriptor of the serial port. */
EXTERN short lines;	/* Nr. of lines on the screen */
EXTERN short cols;	/* Nr. of cols of the screen */

EXTERN jmp_buf ksigbuf;	/* To jump to when key pressed */
EXTERN int docap;	/* Capture data to capture file */
EXTERN FILE *capfp;	/* File to capture to */
EXTERN int addlf;	/* Add LF after CR */
EXTERN int linewrap;	/* Also known as automatic margins */
EXTERN int tempst;	/* Status line is temporary */

EXTERN char lockfile[128]; /* UUCP lock file of terminal */
EXTERN char *homedir;	/* Home directory of user */
EXTERN char *username;	/* Who is using minicom? */

EXTERN int bogus_dcd;	/* This indicates the dcd status if no 'real' dcd */
EXTERN int alt_override;/* -m option */

EXTERN char parfile[256]; /* Global parameter file */
EXTERN char pparfile[256]; /* Personal parameter file */

EXTERN char scr_name[33];   /* Name of last script */
EXTERN char scr_user[33];   /* Login name to use with script */
EXTERN char scr_passwd[33]; /* Password to use with script */

/* Global functions */

#ifndef _PROTO
#  if __STDC__
#    define _PROTO(fun, args) fun args
#  else
#  define _PROTO(fun, args) fun()
#  endif
#endif

/*      Prototypes from file: " config.c "      */

_PROTO(void read_parms, ( void ));
_PROTO(int waccess, ( char *s ));
_PROTO(int config, ( int setup ));
_PROTO(void get_bbp, ( char *ba , char *bi , char *pa ));

/*      Prototypes from file: " dial.c "      */

_PROTO(void mputs, ( char *s ));
_PROTO(void modeminit, ( void ));
_PROTO(void modemreset, ( void ));
_PROTO(void hangup, ( void ));
_PROTO(void sendbreak, ( void ));
_PROTO(int dial, ( char *name , char *num , int keypress ));
_PROTO(int readdialdir, ( void ));
_PROTO(void dialdir, ( void ));

/*      Prototypes from file: " fastsystem.c "      */

_PROTO(int fastexec, ( char *cmd ));
_PROTO(int fastsystem, ( char *cmd , char *in , char *out , char *err ));

/*      Prototypes from file: " help.c "      */

_PROTO(int help, ( void ));

/*      Prototypes from file: " ipc.c "      */

_PROTO(void keyserv, ( int cmd , int arg ));
_PROTO(void setbskey, ( void ));
_PROTO(void setesckey, ( void ));

/*      Prototypes from file: " keyserv.c "      */

_PROTO(void handler, ( int dummy ));
_PROTO(void sendstr, ( char *s ));
_PROTO(int main, ( int argc , char **argv ));

/*      Prototypes from file: " main.c "      */

_PROTO(void leave, ( char *s ));
_PROTO(char *esc_key, ( void ));
_PROTO(void open_term, ( int doinit ));
_PROTO(void init_emul, ( int type ));
_PROTO(void mode_status, ( void ));
_PROTO(void time_status, ( void ));
_PROTO(void show_status, ( void ));
_PROTO(void scriptname, ( char *s ));
_PROTO(int do_terminal, ( void ));

/*      Prototypes from file: " rwconf.c "      */

_PROTO(int writepars, ( FILE *fp , int all ));
_PROTO(int readpars, ( FILE *fp , int init ));

/*      Prototypes from file: " sysdep1.c "      */

_PROTO(void m_dtrtoggle, ( int fd ));
_PROTO(void m_break, ( int fd ));
_PROTO(int m_getdcd, ( int fd ));
_PROTO(void m_setdcd, ( int fd , int what ));
_PROTO(void m_savestate, ( int fd ));
_PROTO(void m_restorestate, ( int fd ));
_PROTO(void m_nohang, ( int fd ));
_PROTO(void m_flush, ( int fd ));
_PROTO(void m_setparms, ( int fd , char *baudr , char *par , char *bits ));
_PROTO(int m_wait, ( int *st ));

/*      Prototypes from file: " sysdep2.c "      */

_PROTO(void getrowcols, ( int *rows , int *cols ));
_PROTO(int setcbreak, ( int mode ));

#if 0 /* Only if you don't have it already */
_PROTO(char *strtok, ( char *s , char *delim ));
#endif
#ifdef _SVR2
_PROTO(int dup2, ( int from, int to ));
#endif

/*      Prototypes from file: " updown.c "      */

_PROTO(void updown, ( int what ));
_PROTO(void kermit, ( void ));
_PROTO(void runscript, ( int ask, char *s , char *l , char *p ));

/*      Prototypes from file: " vt100.c "      */

_PROTO(void init_vt, ( void ));
_PROTO(void termout, ( char *s ));
_PROTO(void out_vt100, ( int ch ));

/*      Prototypes from file: " windiv.c "      */

/* Should use stdarg et al. */
WIN *tell();
void werror();
_PROTO(int ask, ( char *what , char *s []));
_PROTO(char *input, ( char *s , char *buf ));

/*      Prototypes from file: " wkeys.c "      */

_PROTO(int getch, ( void ));
