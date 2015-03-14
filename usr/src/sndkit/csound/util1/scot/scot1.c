#include "y.tab.h"
#include "data.h"
double rval();

/* This procedure checks a character to see if it is a valid token
   character, returning TRUE if it is and FALSE otherwise.
   Valid token characters are alphanumerics plus '_'.
*/

int tokenchar(tc)
 register char tc;
{
	if ((tc >= 'a' && tc <= 'z') ||
	    (tc >= 'A' && tc <= 'Z') ||
	    (tc >= '0' && tc <= '9') ||
	    (tc == '_'))
		return(TRUE);
	else	return(FALSE);
}

/* return a new, cleared instrument */

INSTRUMENT *insalloc(ins_num)
 int ins_num;
{
	register INSTRUMENT *insp;
	insp = (INSTRUMENT *)calloc(1, sizeof(INSTRUMENT));
	insclear(insp, ins_num, FALSE);
	return(insp);
}


/* Changes the current instrument pointer to the next instrument in the
   backup chain.  If the new instrument does not already exist, it
   inherits some values from the old instrument: ins_copy, trans_octave,
   trans_pitch, start, dur, old_dur, transposing, and pfield values.
   Pfield values are always inherited if carry_vertically is enabled.
*/

INSTRUMENT *instransfer(ip)
 register INSTRUMENT *ip;
{
	register INSTRUMENT *new;
	register int *pp;
	int *pn;
	copy_depth++;
	if ((new=ip->next_ins)==0) {
		new = ip->next_ins = insalloc(ip->ins_number);
		new->ins_copy = (ip->ins_copy) + 1;
		rassn(&new->old_dur, &ip->dur);
		new->pmax = ip->pmax;
		copy_pflds(ip,new);
	} else {
		if (carry_vertically) {
			if (ip->pmax>new->pmax) new->pmax = ip->pmax;
			rassn(&new->old_dur, &ip->dur);
			copy_pflds(ip,new);
		}
	}
	new->trans_octave = ip->trans_octave;
	new->trans_pitch = ip->trans_pitch;
	rassn(&new->event_start, &last_start);
	if (!new->tied) rassn(&new->start, &last_start);
	new->transposing = ip->transposing;
	return(new);
}

/* copy pfield arrays from instrument to another */

copy_pflds(finst,tinst)
 INSTRUMENT *finst, *tinst;
{
	register char *pp, *pn, *px;
	px = finst->pfldg[PMAX];
	pp = finst->pflds[0];
	pn = tinst->pflds[0];
	while (pp<px) *pn++ = *pp++;
}

/* Setup initial values for instrument structure */

insclear(ip, ins_no, old_instr)
 register INSTRUMENT *ip;
 int ins_no;			/* instrument number */
 int old_instr; 		/* TRUE if old instrument--else don't have */
				/* to free old pointers in next_ins list. */
{
	register int p; 	/* counter for pfields */
	ip->ins_number = ins_no;
	ip->ins_copy = ++(maxcopy[ins_no - 1]);
	ip->octave = 8;
	ip->nom_octave = 8;
	ip->pitch_deg = 0;
	ip->scale_deg = 0;
	ip->old_octave = 8;
	ip->old_pitch_deg = 0;
	ip->old_scale_deg = 0;
	ip->last_slurred = FALSE;
	ip->this_slurred = FALSE;
	ip->tied = FALSE;
	ip->pmax = -1;
	ip->rest = FALSE;
	ip->accidental = FALSE;
	rint(&ip->start, 0);
	rint(&ip->event_start, 0);
	rint(&ip->dur, 0);
	rint(&ip->total_dur, 0);
	rint(&ip->old_dur, 1);		/* quarter note */
	for (p=0; p<PMAX; ) {
		encode(dot,ip->pflds[p]);
		encode(dot,ip->pfldg[p++]);
	}
	if (old_instr) free_next(ip);
	else {				/* clear transposition values */
		ip->trans_octave = 0;
		ip->trans_pitch = 0;
		ip->transposing = FALSE;
	}
	ip->next_ins = NUL;
}

/* free the remaining instruments in a backup list */

free_next(ip)
 register INSTRUMENT *ip;
{
	register INSTRUMENT *ti;
	for (ti = ip->next_ins, ip->next_ins = NUL; ti!=NUL; ) {
		ip = ti->next_ins;
		cfree(ti);
		ti = ip;
	}
}

/* print out the contents of instrument ip */

#ifdef DEBUG

insprint(ip)
 register INSTRUMENT *ip;
{
	int pp; 	/* counter for printing pfields */
	char pfldtxt[PNUM+1];
	printf("\nins_number = %d", ip->ins_number);
	printf("\nins_copy = %d", ip->ins_copy);
	printf("\ntrans_octave = %d", ip->trans_octave);
	printf("\ntrans_pitch = %d", ip->trans_pitch);
	printf("\noctave = %d", ip->octave);
	printf("\nnom_octave = %d", ip->nom_octave);
	printf("\npitch_deg = %d", ip->pitch_deg);
	printf("\nscale_deg = %d", ip->scale_deg);
	printf("\nold_octave = %d", ip->old_octave);
	printf("\nold_pitch_deg = %d", ip->old_pitch_deg);
	printf("\nold_scale_deg = %d", ip->old_scale_deg);
	printf("\nlast_slurred = %d", ip->last_slurred);
	printf("\nthis_slurred = %d", ip->this_slurred);
	printf("\ntied = %d", ip->tied);
	printf("\npmax = %d", ip->pmax);
	printf("\nstart = %f", rval(&ip->start));
	printf("\nevent_start = %f", rval(&ip->event_start));
	printf("\ndur = %f", rval(&ip->dur));
	printf("\ntotal_dur = %f", rval(&ip->total_dur));
	printf("\nold_dur = %f", rval(&ip->old_dur));
	printf("\ntransposing = %d", ip->transposing);
	printf("\nrest = %d", ip->rest);
	printf("\naccidental = %d", ip->accidental);
	for (pp = 0; pp <= (ip->pmax); pp++) {
		decode(ip->pflds[pp],pfldtxt);
		printf("\np%d = %s,", pp + 6, pfldtxt);
		decode(ip->pfldg[pp],pfldtxt);
		printf("%s", pfldtxt);
	}
	printf("\n");
}
#endif

/* This procedure moves current values into the "old_" values, and
   increments the value of start.
*/

insinit(ip)
 register INSTRUMENT *ip;
{
	register char *pg, *pl;
	register char *pn;
	backing_up = FALSE;
	rassn(&last_start, &ip->event_start);
	ip->old_octave = ip->nom_octave;
	ip->old_pitch_deg = ip->pitch_deg;
	ip->old_scale_deg = ip->scale_deg;
	rassn(&ip->old_dur, &ip->dur);
	rgadd(&ip->event_start, &ip->dur);
	if (!ip->tied) {
		radd(&ip->start, &ip->total_dur);
		rint(&ip->total_dur, 0);
		ip->last_slurred = ip->this_slurred;
	}
	if (rcmp(&sect_dur,&ip->start)<0) rassn(&sect_dur,&ip->start);
	for (pg=ip->pfldg[0], pl=ip->pflds[0], pn=ip->pfldg[ip->pmax+1];
		pg<pn;
		*pl++ = *pg++ );
}

/* Check time signatures by checking that all copies of an instr are
   at the end of a bar.
*/

int new_bar(ip)
 register INSTRUMENT *ip;
{
	int cmpresult;
	RAT actual;		/* actual stop time of bar */
	RAT standard;		/* what stop time should be */
	char beatmsg[100];
	rassn(&standard, &bar_start);
	radd(&standard, &bar_dur);
	while (ip!=NUL && ip->ins_copy<=copy_depth) {
	    rassn(&actual,&ip->start);
	    radd(&actual,&ip->total_dur);
	    if ((cmpresult=rcmp(&actual,&standard))!=0) {
		if (cmpresult>0)
		     sprintf(beatmsg,"Too many beats in measure, copy %d",
			    ip->ins_copy);
		else sprintf(beatmsg,"Too few beats in measure, copy %d",
			    ip->ins_copy);
		yyerror(beatmsg);
	    }
	    ip = ip->next_ins;
	}
}

/* assign a value to octave and nom_octave */

oct_assign(ip, oct_val)
 register INSTRUMENT *ip;
 int oct_val;
{
	ip->octave = oct_val;
	ip->nom_octave = oct_val;
}

/* add an integer to the current value of octave and nom_octave */

oct_modify(ip, oct_difference)
 register INSTRUMENT *ip;
 int oct_difference;
{
	ip->octave += oct_difference;
	ip->nom_octave += oct_difference;
}
