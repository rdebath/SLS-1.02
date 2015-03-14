#include "y.tab.h"
#include "data.h"

octave_following(ip)
 INSTRUMENT *ip;
{
	int o_oct;		/* old octave value */
	o_oct = ip->old_octave;
/*fry*/ if(follow_octaves) {
	    int sc_deg_dif;	    /* scale degree difference */
	    sc_deg_dif = ip->old_scale_deg - ip->scale_deg;
	    if (sc_deg_dif > 3) 	    oct_assign(ip, o_oct + 1);
	    else if (sc_deg_dif < -3)	    oct_assign(ip, o_oct - 1);
	    else			    oct_assign(ip, o_oct);
	}
	else				oct_assign(ip, o_oct);
}


/* compute pitch_deg, possibly modifying octave to
   get a result in canonical octave-pitch form.
*/

pitch_compute(ip)
 INSTRUMENT *ip;
{
	if (ip->accidental)
		ip->pitch_deg = key[ip->scale_deg].nat+key[ip->scale_deg].acc;
	else if ((ip->tied) && (ip->scale_deg == ip->old_scale_deg))
		ip->pitch_deg = ip->old_pitch_deg;
	else if (carry_accidentals &&
		  rcmp( &key[ip->scale_deg].when, &ip->event_start ) <= 0)
		ip->pitch_deg = key[ip->scale_deg].nat+key[ip->scale_deg].acc;
	else	ip->pitch_deg = key[ip->scale_deg].nat;
	while (ip->pitch_deg > 11) {
		ip->pitch_deg -= 12;
		ip->octave += 1;	/* do not change nom_octave */
	}
	while (ip->pitch_deg < 0) {
		ip->pitch_deg += 12;
		ip->octave -= 1;	/* likewise */
	}
	if (ip->octave < 0)
		yyerror("Music 11 PCH representation is negative");
	if (ip->tied && ((ip->pitch_deg != ip->old_pitch_deg) ||
	   (ip->octave != ip->old_octave)) )
		yyerror("tied notes not same pitch");
}


/* assign a value to start and event_start */

st_assign(ip, stval)
 INSTRUMENT *ip;
 RAT *stval;
{
	rassn(&ip->start, stval);
	rassn(&ip->event_start, stval);
}


/* carry old_dur into dur, then adds to total_dur */

dur_carry(ip)
 register INSTRUMENT *ip;
{
	rassn(&ip->dur, &ip->old_dur);
	rgadd(&ip->total_dur, &ip->dur);
}


/* This procedure assigns a value to dur when the duration ::= INTEGER rule
   is used.  The integer passed to the procedure represents the type of
   note (e.g., 4 means a quarter note), so this number is converted into
   quarter-note beats and then assigned, as a double, to dur.  Total_dur
   is incremented by the new value of dur.
*/

dur_assign(ip, dur_int)
 register INSTRUMENT *ip;
 int dur_int;		/* INTEGER from duration ::= INTEGER reduction */
{
	register RAT *d;
	d = &ip->dur;
	rint(d,4);
	rdiv(d,dur_int);
	rgadd(&ip->total_dur,d);
}


/* This procedure assigns a value to dur when the duration ::= INTEGER dot_list
   rule is used.  The first integer refers to the type of note (as in the
   dur_assign procedure), and the second integer is the length of the dot_list.
   These values are used to compute the duration of the note in quarter note
   beats, which is then assigns to dur.  Total_dur is incremented by the
   new value of dur.
*/

dot_dur_assign(ip, dur_int, dot_list_length)
 register INSTRUMENT *ip;
 int dur_int;
 int dot_list_length;
{
	register int i;
	rint(&temp_dur,4);
	rdiv(&temp_dur,dur_int);
	rassn(&ip->dur,&temp_dur);
	for (i = 1; i <= dot_list_length; i++) {
		rdiv(&temp_dur,2);
		radd(&ip->dur,&temp_dur);
	}
	rgadd(&ip->total_dur,&ip->dur);
}

/* update dur accounting for groupettes */

rgadd(dur,upd)
 RAT *dur, *upd;
{
	if (igroup>=0) {
		rassn(&temp_dur,upd);
		rmul(&temp_dur,&groupstk[igroup]);
		radd(dur,&temp_dur);
	} else	radd(dur,upd);
}

/* This procedure outputs an i-statement for the score file.
   Any p-fields marked by ramp indicators are reset to '.'
*/

output(ip)
 register INSTRUMENT *ip;
{
	double rval();
	int oval;			/* octave value */
	int pdval;			/* pitch degree value */
	int pf; 			/* counter for printing pfields */
	char pfldtxt[PLEN+1];
	if (ip->transposing) {
		oval = ip->octave + ip->trans_octave;
		pdval = ip->pitch_deg + ip->trans_pitch;
		while (pdval > 11) {
			pdval -= 12;
			oval += 1;
		}
		while (pdval < 0) {
			pdval += 12;
			oval -= 1;
		}
		if (oval < 0)
		  yyerror("transposition yields negative PCH");
	} else {
		oval = ip->octave;
		pdval = ip->pitch_deg;
	}
#ifdef LSCOT
	fprintf(outfil,"(%d ",insnum[ip->ins_number-1]);
	tprint(&ip->total_dur);
	fprintf(outfil," %d (%d %d %d)",
		ip->this_slurred+(2*ip->last_slurred),
		ip->rest==FALSE ? oval : 0,
		ip->rest==FALSE ? pdval : 0,
		ip->rest==FALSE ? ip->scale_deg : 0);
#else
	fprintf(outfil,"i%d.%1d%1d",
		insnum[ip->ins_number-1],ip->ins_copy/10,ip->ins_copy%10);
	tprint(rval(&ip->start));
	tprint(rval(&ip->total_dur));
	fprintf(outfil," %d %d.%1d%1d",
		ip->this_slurred+(2*ip->last_slurred),oval,pdval/10,pdval%10);
#endif LSCOT
	for (pf = 0; pf <= ip->pmax; pf++) {
		decode(ip->pflds[pf],pfldtxt);
		fprintf(outfil,PLENs,pfldtxt);
		if ((index('<',pfldtxt) >= 0) ||
		    (index('>',pfldtxt) >= 0))
			encode(dot,ip->pflds[pf]);
		}
#ifdef LSCOT
	if (remark[0]) fprintf(outfil," \"%s\"",remark);
	fprintf(outfil,")\n");
#else
	fprintf(outfil,"\n");
#endif LSCOT
}

/* push a new entry on the groupette stack */

groupette(n,d)
 register int n,d;
{
	register RAT *g;
	register int m;
	if (igroup==GROUPS-1) yyerror("groupettes nested too deep");
	else if (d==0 || n==0) yyerror("invalid groupette");
	else {
		m = n;
		if (d<0) for (d=1; n>>=1; d<<=1);
		n = m;
		g = &groupstk[++igroup];
		g->num = d;
		g->den = n;
		if (igroup>0) rmul(g,g-1);
	}
}

/* This procedure assigns a string to the p-field indexed by the
   second argument.  If vertical carrying is in effect, propagate
   assignment to all following copies.	The value of pmax is increased
   if necessary.
*/

pf_assign(iptr,pnumber,pvalue,rampc)
 register INSTRUMENT *iptr;
 int pnumber;			/* index into pfields array */
 char *pvalue;			/* string to be assigned */
				/* if null, then append ramp sign */
 char rampc;			/* ramp char to be appended */
{
	register char *pc;
	char pfldtxt[PLEN+1];
	if (pnumber<0 || pnumber>=PMAX) {
		yyerror("illegal pfield number");
		return;
	}
	do {
		if (*pvalue=='\'')
			encode(pvalue+1,iptr->pflds[pnumber]);
		else {
			encode(pvalue,iptr->pflds[pnumber]);
			encode(pvalue,iptr->pfldg[pnumber]);
		}
		if (pnumber>iptr->pmax) iptr->pmax = pnumber;
		if (carry_vertically) {
			iptr = iptr->next_ins;
		} else	iptr = NUL;
	} while (iptr!=NUL);
}
