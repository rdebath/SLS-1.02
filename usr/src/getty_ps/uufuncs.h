/*
**	uufuncs.h
**
**	$Id: main.c, v 2.1 93/04/01 17:00:00 kris Rel $
**
**	function prototypes and global variables needed by uugetty
*/

/* function prototypes
*/

#undef EXTERN
#ifdef UUFUNCS
#define EXTERN
#else
#define EXTERN extern
#endif /* UUFUNCS */

#include <sys/wait.h>

EXTERN int	makelock();
EXTERN int	readlock();
EXTERN boolean	checklock();
EXTERN sig_t	rmlocks();
EXTERN void	watchlocks();
EXTERN void	waitlocks();
EXTERN void	lockline();


/* globals
*/

EXTERN char	*lock;
EXTERN char	*altlock;
EXTERN int	chpid;
