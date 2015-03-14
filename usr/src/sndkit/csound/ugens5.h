/*									UGENS5.H	*/

typedef struct {
	OPDS	h;
	float	*kr, *asig, *ilen;
	int	len;
} DOWNSAMP;

typedef struct {
	OPDS	h;
	float	*ar, *ksig;
} UPSAMP;

typedef struct {
	OPDS	h;
	float	*rslt, *xsig, *istor;
	float	prev;
} INDIFF;

typedef struct {
	OPDS	h;
	float	*xr, *xsig, *xgate, *ival, *istor;
	float	state;
	int	audiogate;
} SAMPHOLD;

typedef struct {
	OPDS	h;
	float	*ar, *asig, *idlt, *istor;
	float	*curp;
	long	npts;
	AUXCH	auxch;
} DELAY;

typedef struct {
	OPDS	h;
	float	*ar, *idlt, *istor;
	float	*curp;
	long	npts;
	AUXCH	auxch;
} DELAYR;

typedef	struct {
	OPDS	h;
	float	*ar, *xdlt;
	DELAYR	*delayr;
} DELTAP;

typedef struct {
	OPDS	h;
	float	*asig;
	DELAYR	*delayr;
} DELAYW;

typedef struct {
	OPDS	h;
	float	*ar, *asig, *istor;
	float	sav1;
} DELAY1;

typedef	struct {
	OPDS	h;
	float	*ar, *asig, *krvt, *ilpt, *istor;
	float	coef, prvt, *pntr;
	AUXCH	auxch;
} COMB;

typedef	struct {
	OPDS	h;
	float	*ar, *asig, *krvt, *istor;
	float	c1, c2, c3, c4, c5, c6, prvt;
	float	*p1, *p2, *p3, *p4, *p5, *p6;
	float	*adr1, *adr2, *adr3, *adr4, *adr5, *adr6;
	AUXCH	auxch;
} REVERB;

typedef	struct {
	OPDS	h;
	float	*r1, *r2, *r3, *r4, *asig, *kx, *ky, *ifn, *imode, *ioffset;
	float	xmul;
	long	xoff;
	FUNC	*ftp;
} PAN;
