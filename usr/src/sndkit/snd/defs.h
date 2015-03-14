/* defs.h */

/* $Id: defs.h,v 3.2 1992/11/27 10:29:00 espie Exp espie $ 
 * $Log: defs.h,v $
 * Revision 3.2  1992/11/27  10:29:00  espie
 * General cleanup
 *
 * Revision 3.1  1992/11/19  20:44:47  espie
 * Protracker commands.
 *
 * Revision 3.0  1992/11/18  16:08:05  espie
 * New release.
 */

#define LOCAL static
#define X extern

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

#define BOOL int

#define MIN(A,B) ((A)<(B) ? (A) : (B))
#define MAX(A,B) ((A)>(B) ? (A) : (B))
     
#define D fprintf(stderr, "%d\n", __LINE__);


