#include <lpc.h>	/*						UGENS4.H	*/

typedef	struct {
	OPDS	h;
	float	*kr, *ksig, *ihtim, *isig;
	float	c1, c2, yt1;
} PORT;

typedef struct {
	OPDS	h;
	float	*ar, *asig, *khp, *istor;
	float	c1, c2, yt1, prvhp;
} TONE;

typedef	struct {
	OPDS	h;
	float	*ar, *asig, *kcf, *kbw, *iscl, *istor;
	int     scale;
	float	c1, c2, c3, yt1, yt2, cosf, prvcf, prvbw;
} RESON;

typedef struct {
	OPDS	h;
	float	*krmr, *krmo, *kerr, *kcps, *ktimpt, *ifilno, *inpoles, *ifrmrate;
	long	headlongs, npoles, nvals, lastfram16, lastmsg;
	float	kcoefs[MAXPOLES], framrat16;
	MEMFIL	*mfp;
} LPREAD;

typedef	struct {
	OPDS	h;
	float	*ar, *asig;
	float	circbuf[MAXPOLES<<1], *circjp, *jp2lim;
	LPREAD	*lpread;
} LPRESON;

typedef	struct {
	OPDS	h;
	float	*ar, *asig, *kfrqratio;
	float	past[MAXPOLES], prvratio, d, prvout;
	LPREAD	*lpread;
} LPFRESON;

typedef	struct {
	OPDS	h;
	float	*kr, *asig, *ihp, *istor;
	float	c1, c2, prvq;
} RMS;

typedef	struct {
	OPDS	h;
	float	*ar, *asig, *krms, *ihp, *istor;
	float	c1, c2, prvq, prva;
} GAIN;

typedef	struct {
	OPDS	h;
	float	*ar, *asig, *csig, *ihp, *istor;
	float	c1, c2, prvq, prvr, prva;
} BALANCE;
