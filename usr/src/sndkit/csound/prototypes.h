                                                         /* PROTOTYPES.H */

#ifdef __STDC__                                          /* ANSI prototypes */

#include <stdlib.h> 
char *mmalloc(long), *mcalloc(long);
void auxalloc(long, AUXCH *), auxchfree(INSDS *);
void fdrecord(FDCH *), fdclose(FDCH *), fdchclose(INSDS *);
char argtyp(char *);
double modf(double, double *);  /* not included in math.h */

#else                                                    /* non-ANSI dclr'ns */

char *malloc(), *calloc();
char *mmalloc(), *mcalloc();
void auxalloc(), auxchfree();
void fdrecord(), fdclose(), fdchclose();
char argtyp();
double modf();

#endif

                                           /* common globals */
extern int   inerrcnt, synterrcnt, perferrcnt;

extern int   odebug, initonly, sfread, sfwrite, sfheader;   /* orch control flags: */
extern int   iobufsamps, informat, outformat, insampsiz, outsampsiz;
extern int   displays, graphsoff, msglevel;
extern char  *orchname, *infilename, *outfilename;
extern int   infd, outfd;

extern float esr, ekr, ensmps, hfkprd, onedsr;    /* set in oload */
extern int   ksmps, nchnls;
extern float pi, twopi, tpidsr, mtpdsr, pid100;
extern float sicvt, kicvt, fmaxlen, dv32768;
extern char  strmsg[], errmsg[];
