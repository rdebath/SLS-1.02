typedef	struct {			/*					INSERT.H		*/
	OPDS	h;
	LBLBLK	*lblblk;
} GOTO;

typedef	struct {
	OPDS	h;
	int	*cond;
	LBLBLK	*lblblk;
} CGOTO;

typedef	struct {
	OPDS	h;
	float	*idel, *idur;
	LBLBLK	*lblblk;
	long	cnt1, cnt2;
} TIMOUT;

typedef	struct {
	OPDS	h;
} LINK;

