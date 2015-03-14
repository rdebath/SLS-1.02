/*									UGENS3.H	*/

typedef struct {
	OPDS	h;
	float	*ar, *xamp, *xcps, *knh, *ifn, *iphs;
	short	ampcod, cpscod;
	long	lphs;
	FUNC	*ftp;
} BUZZ;

typedef struct {
	OPDS	h;
	float	*ar, *xamp, *xcps, *kn, *kk, *kr, *ifn, *iphs;
	short	ampcod, cpscod, prvn;
	float	prvr, twor, rsqp1, rtn, rtnp1, rsumr;
	long	lphs;
	FUNC	*ftp;
} GBUZZ;

typedef struct {
	OPDS	h;
	float	*ar, *kamp, *kcps, *icps, *ifn, *imeth, *ipar1, *ipar2;
	float	sicps, param1, param2;
	short	thresh1, thresh2, method;
	long	phs256, npts, maxpts;
	AUXCH	auxch;
} PLUCK;

typedef struct {
	OPDS	h;
	float	*ar, *xamp, *iseed;
	short	ampcod, rand;
} RAND;

typedef struct {
	OPDS	h;
	float	*ar, *xamp, *xcps, *iseed;
	short	ampcod, cpscod, rand;
	long	phs;
	float	num1;
} RANDH;

typedef struct {
	OPDS	h;
	float	*ar, *xamp, *xcps, *iseed;
	short	ampcod, cpscod, rand;
	long	phs;
	float	num1, num2, dfdmax;
} RANDI;
