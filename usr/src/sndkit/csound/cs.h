#include <stdio.h>		/*					CS.H	*/
#include <sysdep.h>

#define	INSTR	1
#define	ENDIN	2
#define	LABEL	3

#define MAXINSNO 100
#define	PMAX	 150
#define VARGMAX  150

#define	ORTXT	   h.optext->t
#define	INCOUNT	   ORTXT.inlist->count
#define	OUTCOUNT   ORTXT.outlist->count
#define	XINCODE	   ORTXT.xincod

#define	MAXLEN	   0x1000000L
#define	PMASK	   0x0FFFFFFL
#define	PFRAC(x)   ((x & ftp->lomask) * ftp->lodiv)
#define MAXPOS     0x7FFFFFFFL

#define BYTREVS(n) (n>>8  & 0xFF | n<<8 & 0xFF00)
#define BYTREVL(n) (n>>24 & 0xFF | n>>8 & 0xFF00L | n<<8 & 0xFF0000L | n<<24 & 0xFF000000L)

typedef struct polish {
	char	opcod[12];
	short	incount;
	char	arg[4][12];
} POLISH;

typedef struct arglst {
	short	count;
	char	*arg[1];
} ARGLST;

typedef struct text {
	short	linenum;
	short	opnum;
	char	*opcod;
	ARGLST	*inlist;
	ARGLST	*outlist;
	short	xincod;
	char	pftype;
} TEXT;	
	
typedef struct instr {
	struct op * nxtop;
	TEXT	t;
	short	pmax;
	short	pextrab;
	int	localen;
	struct insds * instance;
	struct instr * nxtinstxt;
} INSTRTXT;

typedef struct op {
	struct op * nxtop;
	TEXT	t;
} OPTXT;

typedef struct fdch {
	struct fdch * nxtchp;
        int    fd;
} FDCH;

typedef struct auxch {
	struct auxch * nxtchp;
	long   size;
        char   *auxp, *endp;
} AUXCH;

typedef struct insds {
	struct opds * nxtopds;
	struct opds * nxti;
	struct opds * nxtp;
	struct opds * nxtlbl;
	struct insds * nxtinstance;
	struct insds * prvinstance;
	struct insds * nxtact;
	struct insds * prvact;
	struct insds * nxtoff;
	FDCH	fdch;
	AUXCH	auxch;
	short	insno;
	short	actflg;
	float	offbet;
	float	offtim;
	float	p0;
	float	p1;
	float	p2;
	float	p3;
} INSDS;

typedef	int	(*SUBR)();

typedef struct opds {
	struct opds * nxtopds;
	struct opds * nxti;
	struct opds * nxtp;
	struct opds * nxtlbl;
	SUBR	iopadr;
	SUBR	opadr;
	OPTXT * optext;
	INSDS * insdshead;
} OPDS;
	
typedef struct lblblk {
	OPDS	h;
	OPDS	*prvi;
	OPDS	*prvp;
	char	*lbltxt;
} LBLBLK;

typedef struct oentry { 
	char	*opname;
	unsigned short	dsblksiz;
	unsigned short	thread;
	char	*outypes;
	char	*intypes;
	SUBR	iopadr;
	SUBR	kopadr;
	SUBR	aopadr;
} OENTRY;

typedef struct {
	float   *begp, *curp, *endp, feedback[6];
	long    scount;
} OCTDAT;

#define MAXOCTS 8

typedef struct {
	long	npts, nocts, nsamps, lofrq, hifrq;
	OCTDAT  octdata[MAXOCTS];
	AUXCH	auxch;
} DOWNDAT;

typedef struct {
	long	ktimstamp, ktimprd;
	long	npts, dbout;
	DOWNDAT *downsrcp;
	AUXCH	auxch;
} SPECDAT;

typedef struct {
	long	flen;
	long	lenmask;
	long	lobits;
	long	lomask;
	float	lodiv;
	float	ftable[1];
} FUNC;

typedef struct {
	char	filename[20];
	char	*beginp;
	char	*endp;
	long	length;
} MEMFIL;

typedef struct event {
	char	opcod;
	short	pcnt;
	float	p2orig;
	float	p3orig;
	float	offtim;
	float	p[PMAX+1];
} EVTBLK;

typedef	struct {
	OPDS	h;
	float	*ktempo, *istartempo;
	float	prvtempo;
} TEMPO;

FUNC   *ftfind();
MEMFIL *ldmemfile();

#include "prototypes.h"
