/*									UGENS6.H	*/

/* the following due to Michael Clarke, Huddersfield Polytechnic */

#define	CLOCK_TIM	*localp
#define	FORM_INC	*(localp +1)
#define FORM_PHS	*(localp +2)

#define TEX_INC		*(localp +3)
#define TEX_IND		*(localp +4)
#define BAND_CUR	*(localp +5)
#define BAND_EXP	*(localp +6)
#define ATT_IND		*(localp +7)
#define ATT_INC		*(localp +8)
#define END_TIM		*(localp +9)
#define OCT_AMP		*(localp +10)
#define LOCAL_BUFFER_SIZE         11

#define	TRUE	1
#define	FALSE	0
#define	NEGPI	-3.1415927

typedef struct {
	OPDS	h;
	float	*ar, *xamp, *xfund, *xforma, *xformb, *koct, *ktex, *kband;
	float	*kdebat, *katt, *iolaps, *ifna, *ifnb, *idur, *iphs, *icor;
	float	*fofaux, clock_incr, duration, tab_quart_flt;
	float   prvtex, prvband, ampcor;
	long	fundphs,fund_incr,tab_quart,tab_three_quart,pass_no;
	short	fund_per_flag, fof_count, print_flag, olaps_int, olaps_trig;
	short	ampcod, fundcod, formacod, formbcod;
	AUXCH	auxch;
	FUNC	*ftp1, *ftp2;
} FOFS;
