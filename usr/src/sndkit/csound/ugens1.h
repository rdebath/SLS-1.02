/*									UGENS1.H	*/

typedef struct {
	OPDS	h;
	float	*xr, *ia, *idur, *ib;
	float	val, incr;
} LINE;

typedef struct 
{
	OPDS   	h;
	float 	*xr, *ia, *idur, *ib;
	float	val, mlt;
} EXPON;

typedef struct {
        float  val, inc;
	long   cnt;
} LSEG;

typedef struct {
        float  val, mlt;
	long   cnt;
} XSEG;

typedef struct {
	OPDS	h;
	float	*rslt, *argums[VARGMAX];
	LSEG	*cursegp;
	long	nsegs;
	AUXCH   auxch;
} LINSEG;

typedef struct {
	OPDS	h;
	float	*rslt, *argums[VARGMAX];
	XSEG	*cursegp;
	long	nsegs;
	AUXCH   auxch;
} EXPSEG;

typedef struct
{
	OPDS 	h;
	float	*rslt, *sig, *iris, *idur, *idec;
	float 	inc1,inc2,val,lin1,lin2;
	long 	cnt1,cnt2;
} LINEN;

typedef struct {
	OPDS	h;
	float  *rslt, *xamp, *irise, *idur, *idec, *ifn, *iatss,*iatdec,*ixmod;
	long 	phs, ki, cnt1;
	float	val, mlt1, mlt2, asym;
	FUNC	*ftp;
} ENVLPX;
