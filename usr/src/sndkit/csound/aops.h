/*										AOPS.H		*/

typedef struct {
	OPDS	h;
	float	*r, *a;
} ASSIGN;

typedef	struct {
	OPDS	h;
	int	*rbool;
	float	*a, *b;
} RELAT;

typedef	struct {
	OPDS	h;
	int	*rbool, *ibool, *jbool;
} LOGCL;

typedef	struct {
	OPDS	h;
	float	*r;
	int	*cond;
	float	*a, *b;
} CONVAL;

typedef	struct {
	OPDS	h;
	float	*r, *a, *b;
} AOP;

typedef	struct {
	OPDS	h;
	float	*r, *a;
} EVAL;

typedef struct {
	OPDS	h;
	float	*ar;
} IN;

typedef struct {
	OPDS	h;
	float	*ar1, *ar2;
} INS;

typedef struct {
	OPDS	h;
	float	*ar1, *ar2, *ar3, *ar4;
} INQ;

typedef struct {
	OPDS	h;
	float	*asig;
} OUT;

typedef struct {
	OPDS	h;
	float	*asig1, *asig2;
} OUTS;

typedef struct {
	OPDS	h;
	float	*asig1, *asig2, *asig3, *asig4;
} OUTQ;

