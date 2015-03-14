#define	NCONSTS	300			/*				OLOAD.H		*/
#define	NNAMES	500
#define	NGOTOS	100

typedef struct	{
	char	*namep;
	int	offset;
} NAME;

typedef	struct	{
	char	*lbltxt;
	float	**argpp;
} LBLARG;
