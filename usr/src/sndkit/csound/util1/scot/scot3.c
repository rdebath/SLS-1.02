#include "y.tab.h"
#include "data.h"
double rval();

/* This procedure, invoked at the start of a measure, clears away the
   accidental carried throughout the bar.  No need to call this if
   carry_accidentals is FALSE.
*/

clear_acc() {
	register int k;
	for (k = 0; k <= 6; k++)
		key[k].acc = key[k].sig;
}


/* finishes changing key signatures by naturaling all
 * unchanged pitches in the scale.
 */


key_change() {
	register int kc;	/* index to key array */
#ifdef LSCOT
	register int cnt = 0;
#endif LSCOT
	for (kc = 0; kc < 7; kc++) {
		if (key [kc].changed)
			key [kc].changed = FALSE;
		else	key [kc].sig = key [kc].acc = 0;
#ifdef LSCOT
		cnt += key[kc].sig;
#endif LSCOT
	}
#ifdef LSCOT
	fprintf(outfil,"(%d keysig %d)\n",insnum[insptr->ins_number-1],cnt);
#endif LSCOT
}


/* add an instrument name to the instrs array */

add_instr(new_instr,inum)
char *new_instr;	/* string to be added to the array */
{
	int li; 	/* result of lookup_instr */
	if (insindex >= NOINSTRS)
		yyerror("too many instruments defined");
	else if ((li = lookup_instr(new_instr)) > 0)
		yyerror("instrument already defined");
	else {
		curins = insindex;
		copy(new_instr, instrs[insindex], '\0');
		insnum[insindex] = inum;
		insindex++;
	}
}


/* This procedure looks up an instrument name in the instrs array.
   If found, the instrument number (index in array plus 1) is returned.
   Otherwise zero is returned.
*/

int lookup_instr(linstr)
char *linstr;			/* string to be looked up */
{
	register int l; 	/* counter */
	for (l = 0; l <insindex; l++)
		if (compare(linstr,instrs[l]))
			return(++l);
	return(0);		/* not found */
}


/* This procedure pushes a new start time onto the start_times array.
*/

push_start(new_start)
RAT *new_start; 		/* new starting time */
{
	if (start_index < MAXDEPTH)
		rassn(&start_times[start_index++], new_start);
	else	yyerror("groups nested too deep");
}


/* pop a start time from the start_times array */

RAT *pop_start() {
	if (start_index > 0)
		return(&start_times[--start_index]);
	else	yyerror("No group open");
}


/* add a new tempo indication into the tempi array */

add_tempo(new_tempo,ip)
 int new_tempo; 		/* tempo indication */
 register INSTRUMENT *ip;	/* instrument containing time in beats */
{
	if (tindex >= TEMPOVALS)
		yyerror("too many tempos in section");
	else if (tindex != 0 && (rval(&ip->start) < tempi[tindex - 2]))
		yyerror("attempt to respecify tempo");
	else {
		tempi[tindex++] = rval(&ip->start);
		tempi[tindex++] = new_tempo;
	}
}

/* output a tempo statement */

#ifdef LSCOT

toutput() {}

#else

toutput() {
	register int t;
	fprintf(outfil,"t0 60");
	for (t=0; t<tindex; t++) tprint(tempi[t]);
	fprintf(outfil,"\n");
	tindex = 0;
}

#endif LSCOT

/* output a time value */

#ifdef LSCOT

tprint(time) RAT *time; {
	fprintf(outfil,"%ld %ld",time->num, 4 * time->den);
}

#else

tprint(time) double time; {
	register char *p;
	char str[20];
	sprintf(str," %.4fx",time);
	for (p=str+1; *p++ != 'x'; );
	--p;
	while (*--p == '0');
	if (*p != '.')	p++;
	*p = '\0';
	fputs(str,outfil);
}

#endif LSCOT

/* The following are used to encode and decode pfield text byte strings into
 * the four-bit values actually stored in instrument structures.
 *  (Note nonportability!)
 */

char codekey[16] = {
	 0,  '0', '1', '2', '3', '4', '5', '6',
	'7', '8', '9', '.', '+', '-',  0,   0,
};

encode(from,to)
 register char *from, *to;
{
	register n;
	register char *p;
	int cnt = 0;
	do {
		codekey[15] = *from++;
		for (p=codekey, n=0; *p++!=codekey[15]; n++);
		if ((cnt++)&01) *to++ += n<<4;
		else		*to = n;
		if (n == 15) {		/* if not in table, */
			if ((cnt++)&01) to++;	/* align and */
			*to++ = *(from-1);	/* pack whole char */
			cnt = 0;		/* (.. align) */
		}
        } while (n);
}

decode(from,to)
 register char *from, *to;
{
	register int cnt = 0;
	codekey[15] = -1;		/* (identify overflow char) */
	while (*to = codekey[017&( ((cnt++)&01) ? (*from++)>>4 : *from)] )
		if (*to++ < 0) {	/* if not in table, */
			if ((cnt++)&01) from++; /* align and */
			*(to-1) = *from++;	/* unpack whole char */
			cnt = 0;		/* (.. align) */
		}
}
