
/*										UGENS2.H	*/

typedef struct {
	OPDS	h;
	float	*sr, *xcps, *iphs;
	float	curphs;
} PHSOR;

typedef struct {
	OPDS	h;
	float	*rslt, *indx, *ifn, *ixmode, *ixoff, *iwrap;
	float   offset;
	long   	xbmul, wrap;
	FUNC	*ftp;
} TABLE;

typedef struct {
	OPDS	h;
	float	*rslt, *idel, *kamp, *idur, *ifn;
	long 	kinc, phs;
	long 	dcnt;
	FUNC	*ftp;
} OSCIL1;

typedef struct {
	OPDS	h;
	float	*sr, *xamp, *xcps, *ifn, *iphs;
	long	lphs;
	FUNC	*ftp;
} OSC;

typedef struct {
	OPDS	h;
	float	*rslt, *xamp, *kcps, *kcar, *kmod, *kndx, *ifn, *iphs;
	long	mphs, cphs;
	FUNC	*ftp;
} FOSC;

typedef struct {
	short	tim;
	short	val;
} DUPLE;

typedef struct {
	DUPLE	*ap;
	DUPLE	*fp;
	short	amp,frq;
	long	phs;
} PTLPTR;

#define MAXPTLS 100

typedef struct {
	OPDS	h;
	float	*rslt, *kamod, *kfmod, *ifilno;
	MEMFIL	*mfp;
	long	kcnt;
	PTLPTR  ptlptrs[MAXPTLS];
} ADSYN;
