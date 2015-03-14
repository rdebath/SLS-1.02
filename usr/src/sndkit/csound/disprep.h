        		/*						DISPREP.H	*/

typedef struct {
	OPDS	h;
	float	*iargs[VARGMAX];
} PRINTV;

typedef struct {
	OPDS	h;
	float	*signal, *iprd, *iwtflg;
	long	npts;
	WINDAT  dwindow;
	float	*nxtp;
	AUXCH	auxch;
} DSPLAY;

#define	WINDMAX	2048
#define WINDMIN 16

typedef struct {
	OPDS	h;
	float	*signal, *iprd, *inpts, *ihann, *idbout, *iwtflg;
	float	sampbuf[WINDMAX], *bufp, *endp, *fftlut, overN;
	int	windsize, overlap, hanning, dbout, ncoefs;
	WINDAT  dwindow;
	AUXCH	auxch;
} DSPFFT;

typedef struct {
	OPDS	h;
	DOWNDAT *dsig;
	float	*signal, *iocts, *isamps, *idisprd;
	int     disprd, countdown;
	WINDAT  dwindow;
	AUXCH	auxch;
} OCTDOWN;

#define MAXFRQS 48

typedef struct {
	OPDS	h;
	SPECDAT *wsig;
	DOWNDAT *dsig;
	float	*iprd, *ifrqs, *iq, *ihann, *idbout, *idsines;
	int	nfreqs, hanning, ncoefs, dbout;
	float	curq, *sinp, *cosp, *linbufp;
	int     countdown, timcount, winlen[MAXFRQS];
	WINDAT  dwindow;
	AUXCH	auxch;
} NOCTDFT;

typedef struct {
	OPDS	h;
	float	*ksum;
	SPECDAT *wsig;
	float   *interp;
	int     kinterp;
	float	kval, kinc;
} SPECSUM;

typedef struct {
	OPDS	h;
	SPECDAT	*waddm;
	SPECDAT *wsig1, *wsig2;
	float   *imul2;
	float	mul2;
} SPECADDM;

typedef struct {
	OPDS	h;
	SPECDAT *wdiff;
	SPECDAT *wsig;
	float	*imode;
	int     mode;
	SPECDAT specsave;
} SPECDIFF;

typedef struct {
	OPDS	h;
	SPECDAT *wscaled;
	SPECDAT *wsig;
	float	*ifscale, *ifthresh;
	int	thresh;
	float	*fscale, *fthresh;
	AUXCH	auxch;
} SPECSCAL;

typedef struct {
	OPDS	h;
	SPECDAT *wfil;
	SPECDAT *wsig;
	float	*ifhtim;
	float   *coefs, *states;
	AUXCH	auxch;
} SPECFILT;

typedef struct {
	OPDS	h;
	SPECDAT *wsig;
	float	*iprd, *iwtflg;
	int     countdown, timcount;
	WINDAT  dwindow;
} SPECDISP;

typedef struct {
	OPDS	h;
	float	*kout,*kin,*iprd,*imindur,*imemdur,*ihp,*ithresh,*ihtim,*ixfdbak;
	float   *istartempo,*ifn,*idisprd,*itweek;
	int     countdown, timcount, npts, minlam, maxlam;
	float	*hbeg, *hcur, *hend;
	float	*xbeg, *xcur, *xend;
	float	*stmemp, *linexp, *ftable, *xscale, *lmults;
	short   *lambdas;
	float   *stmemnow, ncross, coef0, coef1, yt1, thresh;
	float   fwdcoef, fwdmask, xfdbak, avglam, tempscal, tempo, tweek;
	int     dcntdown, dtimcnt;
	WINDAT  dwindow;
	AUXCH	auxch;
} TEMPEST;

