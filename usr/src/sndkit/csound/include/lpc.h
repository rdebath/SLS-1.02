/*										LPC.H	*/

#define	LP_MAGIC    999
#define	MAXPOLES    34
#define	MAXFRAME    (MAXPOLES + 4)
#define LPBUFSIZ    4096           /* used in lpanal */

typedef	struct {
	long	headersize, lpmagic, npoles, nvals;
	float	framrate, srate, duration;
	char	text[4];
} LPHEADER;
