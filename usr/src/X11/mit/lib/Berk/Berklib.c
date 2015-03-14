/* $XConsortium: Berklib.c,v 1.15 91/09/10 08:50:04 rws Exp $ */

/*
 * These are routines found in BSD but not on all other systems.  The core
 * MIT distribution does not use them except for ffs in the server, unless
 * the system is seriously deficient.  You should enable only the ones that
 * you need for your system.  Use Xfuncs.h in clients to avoid using the
 * slow versions of bcopy, bcmp, and bzero provided here.
 */

#include <sys/types.h>

#ifdef hpux
#define WANT_BFUNCS
#define WANT_FFS
#define WANT_RANDOM
#endif

#ifdef macII
/* silly bcopy in A/UX 2.0.1 does not handle overlaps */
#define WANT_BFUNCS
#define NO_BZERO
#endif

#ifdef SVR4
#define WANT_BFUNCS
#define WANT_FFS
#define WANT_RANDOM
#endif

#ifdef hcx
#define WANT_FFS
#endif

#ifdef SYSV386
#ifdef SYSV
#define WANT_FFS
#endif
#endif

/* you should use Xfuncs.h in code instead of relying on Berklib */
#ifdef WANT_BFUNCS

#include <X11/Xosdefs.h>

#if (__STDC__ && defined(X_NOT_STDC_ENV)) || defined(SVR4) || defined(hpux)

#include <string.h>

void bcopy (b1, b2, length)
    register char *b1, *b2;
    register int length;
{
    memmove(b2, b1, (size_t)length);
}

int bcmp (b1, b2, length)
    register char *b1, *b2;
    register int length;
{
    return memcmp(b1, b2, (size_t)length);
}

void bzero (b, length)
    register char *b;
    register int length;
{
    memset(b, 0, (size_t)length);
}

#else

void bcopy (b1, b2, length)
    register char *b1, *b2;
    register int length;
{
    if (b1 < b2) {
	b2 += length;
	b1 += length;
	while (length--)
	    *--b2 = *--b1;
    } else {
	while (length--)
	    *b2++ = *b1++;
    }
}

#if defined(SYSV)

#include <memory.h>

int bcmp (b1, b2, length)
    register char *b1, *b2;
    register int length;
{
    return memcmp(b1, b2, length);
}

#ifndef NO_BZERO

bzero (b, length)
    register char *b;
    register int length;
{
    memset(b, 0, length);
}

#endif

#else

int bcmp (b1, b2, length)
    register char *b1, *b2;
    register int length;
{
    while (length--) {
	if (*b1++ != *b2++) return 1;
    }
    return 0;
}

void bzero (b, length)
    register char *b;
    register int length;
{
    while (length--)
	*b++ = '\0';
}

#endif
#endif
#endif /* WANT_BFUNCS */

#ifdef WANT_FFS
int
ffs(mask)
unsigned int	mask;
{
    register i;

    if ( ! mask ) return 0;
    i = 1;
    while (! (mask & 1)) {
	i++;
	mask = mask >> 1;
    }
    return i;
}
#endif

#ifdef WANT_RANDOM
#if defined(SYSV) || defined(SVR4)

long lrand48();

long random()
{
   return (lrand48());
}

void srandom(seed)
    int seed;
{
   srand48(seed);
}

#else

long random()
{
   return (rand());
}

void srandom(seed)
    int seed;
{
   srand(seed);
}

#endif
#endif /* WANT_RANDOM */

/*
 * insque, remque - insert/remove element from a queue
 *
 * DESCRIPTION
 *      Insque and remque manipulate queues built from doubly linked
 *      lists.  Each element in the queue must in the form of
 *      ``struct qelem''.  Insque inserts elem in a queue immedi-
 *      ately after pred; remque removes an entry elem from a queue.
 *
 * SEE ALSO
 *      ``VAX Architecture Handbook'', pp. 228-235.
 */

#ifdef WANT_QUE
struct qelem {
    struct    qelem *q_forw;
    struct    qelem *q_back;
    char *q_data;
    };

insque(elem, pred)
register struct qelem *elem, *pred;
{
    register struct qelem *q;
    /* Insert locking code here */
    if ( elem->q_forw = q = (pred ? pred->q_forw : pred) )
	q->q_back = elem;
    if ( elem->q_back = pred )
	pred->q_forw = elem;
    /* Insert unlocking code here */
}

remque(elem)
register struct qelem *elem;
{
    register struct qelem *q;
    if ( ! elem ) return;
    /* Insert locking code here */

    if ( q = elem->q_back ) q->q_forw = elem->q_forw;
    if ( q = elem->q_forw ) q->q_back = elem->q_back;

    /* insert unlocking code here */
}
#endif /* WANT_QUE */

/*
 * gettimeofday emulation
 * Caution -- emulation is incomplete
 *  - has only second, not microsecond, resolution.
 *  - does not return timezone info.
 */

#if WANT_GTOD
int gettimeofday (tvp, tzp)
    struct timeval *tvp;
    struct timezone *tzp;
{
    time (&tvp->tv_sec);
    tvp->tv_usec = 0L;

    if (tzp) {
	fprintf( stderr,
		 "Warning: gettimeofday() emulation does not return timezone\n"
		);
    }
}
#endif /* WANT_GTOD */

