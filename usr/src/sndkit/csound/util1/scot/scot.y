/* Scot compiler - yacc grammar */

%token PITCH INTEGER INAME TOKEN REMARK
%token ORCHESTRA FUNCTIONS SCORE
%token ACC ENDV KEY NEXTV OCT TIME TRANSPOSE VERT
%token PFLD PFLDR RAMP PNUM
%{
#include "data.h"
double rval();
INSTRUMENT *instransfer();
%}

%%
scot_file:	orchestra body = {
#ifndef LSCOT
					fprintf(outfil,"e\n");
#endif
				}

orchestra:	orch_start defs instr_lst '}' ;

orch_start:	ORCHESTRA '{' = 	curins = -1;
|		TOKEN '{' =
			yyerror("Score must start with orchestra section");

instr_lst:	instr ;
|		instr_lst instr ;

instr:		insdef defs ;
insdef: 	TOKEN '=' INTEGER =	add_instr(tokenval,$3);

defs:	;
defs:	'[' deflst ']' =	begxx();
deflst: =			begd1();
|	deflst defin;
|	deflst error;
defin:	delft '=' TOKEN = {	begd1();
				maksym($1,$3,curins<0?&symgbl:&symins[curins]);
			}

delft:	TOKEN = 		begd2();

body:	sect;
|	body sect;
sect:	fnc_sec scr_sec ;
|	scr_sec ;

fnc_sec:	FUNCTIONS '{' = 	copysec();

scr_sec:	scr_strt inst_pt_lst '}' = {
			toutput();
#ifdef LSCOT
			fprintf(outfil,"(0 ");
			tprint(&sect_dur);
			fprintf(outfil,")\n");
#else
			fprintf(outfil,"f0");
			tprint(rval(&sect_dur));
			fprintf(outfil,"\ns\n");
#endif LSCOT
			scoresec = 0;
			if (igroup>=0)
				yyerror("unclosed groupette at section end");
			begxx();
		}

scr_strt:	SCORE '{' = {
				register int m;
			xscore: begsc();
				scoresec++;
				section++;
				rint(&sect_dur,0);
				for (m = 0; m < insindex; m++)
				maxcopy[m] = 0;
		}
|	TOKEN '{' = {
			yyerror("Invalid section: score assumed");
			goto xscore;
		}

inst_pt_lst:	inst_pt ;
|		inst_pt_lst inst_pt ;

inst_pt:	inst_name event_lst = {
			measure = 1;	/* measure is instrument relative */
		}

inst_name:	INAME = {
		register int k; 	/* counter */
		rint(&bar_start,0);	/* reset for time signature check */
		if ((curins = lookup_instr(tokenval)-1) < 0)
			yyerror("Instrument not declared in orchestra");
		if (maxcopy[curins] > 0)
			yyerror("Instrument already used in this section");
		insclear(insptr,curins+1,TRUE);
		key_change();		/* clear key signature */
		for (k=0; k<7; k++)	/* initialize when fields */
			rint(&key[k].when,-1);
		rint(&bar_dur,0);		/* clear time signature */
		igroup = -1;			/* no groupettes in force */
		copy_depth = 1;
		}

event_lst:	event ;
|		event_lst event ;

event:		control_ev ;
|		comp_nt_ev = {
				free_next(insptr);
				insptr = &ins1;
				copy_depth = 1;
			}

control_ev:	bar_line ;
|		global ;

bar_line:	'/' = {
		if (carry_accidentals) clear_acc();
		if (bar_dur.num)	/* check time sig if nonzero */
			new_bar(insptr);
		rassn(&bar_start,&insptr->start);
		radd(&bar_start,&insptr->total_dur);
		measure++;		/* measure count */
		}

tempo_mark:	't' INTEGER =	add_tempo($2,insptr);

global: keysig ;
|	xpose ;
|	timsig ;
|	ne_dcl =	need_carry = TRUE;
|	switch ;
|	badglb ;

keysig: keykey =		key_change();
|	keykey keyval = 	key_change();

keykey: gmark KEY =		begsc();

keyval: '"' key_pt_lst '"' ;
|	badval =		yyerror("Error in key signature");

key_pt_lst:	key_pt ;
|		key_pt_lst key_pt ;

key_pt: accidental_lst PITCH = {
		$$ = key[$2].sig = key[$2].acc = $1;
		key[$2].changed = TRUE;
		}
|	key_pt PITCH = {
		$$ = key[$2].sig = key[$2].acc = $1;
		key[$2].changed = TRUE;
		}

xpose:	xposky = {
		doing_trans = FALSE;		/* done with it now */
		insptr->trans_octave = 0;	/* clear transposition */
		insptr->trans_pitch = 0;
		insptr->transposing = FALSE;
		}
|	xposky xposvl = {
		doing_trans = FALSE;		/* done with it now */
		if ((insptr->trans_octave != 0) || (insptr->trans_pitch != 0))
			insptr->transposing = TRUE;
		else	insptr->transposing = FALSE;
		}

xposky: gmark TRANSPOSE = {
		begsc();
		doing_trans = TRUE;
		}

xposvl: '"' comp_pch_pt '"' ;
|	badval =	yyerror("Error in transposition");

timsig: timkey =	rint (&bar_dur, 0);	 /* clear time signature */
|	timkey timval = {
#ifdef LSCOT
			fprintf(outfil,"(%d timesig %d %d)\n",
				insnum[insptr->ins_number-1],
				bar_dur.num,4*bar_dur.den);
#endif LSCOT
		}

timkey: gmark TIME =	begsc();

timval: '"' INTEGER '/' INTEGER '"' = {
		rint(&bar_dur, 4 * $2);
		rdiv(&bar_dur, $4);
		}
|	badval =	yyerror("Error in time signature");

ne_dcl: gmark nxtend 'p' INTEGER '"' 'p' INTEGER '"' = {
		fprintf(outfil,"r%d %c %d %d\n",
			insnum[insptr->ins_number-1],$2,$7,$4);
		}

nxtend: NEXTV = {	$$ = 'n';
		ne_com: begsc();
		}
|	ENDV  = {	$$ = 'e';
			goto ne_com;
		}

switch: gmark ACC '"' TOKEN '"' = {
		begsc();
		if (compare(tokenval, on))
			carry_accidentals = TRUE;
		else if (compare(tokenval, off))
			carry_accidentals = FALSE;
		else
			on_off: yyerror("\"on\" or \"off\" expected");
		}
|	gmark ACC badval = {
		accerror:
		begsc();
		goto on_off;
		}
|	gmark ACC error =	goto accerror;
|	gmark OCT '"' TOKEN '"' = {
		begsc();
		if (compare(tokenval, on))
			follow_octaves = TRUE;
		else if (compare(tokenval, off))
			follow_octaves = FALSE;
		else goto on_off;
		}
|	gmark OCT badval = {
		octerror:
		begsc();
		goto on_off;
		}
|	gmark OCT error =	goto octerror;
|	gmark VERT '"' TOKEN '"' = {
		begsc();
		if (compare(tokenval, on))
			carry_vertically = TRUE;
		else if (compare(tokenval, off))
			carry_vertically = FALSE;
		else goto on_off;
		}
|	gmark VERT badval = {
		verterror:
		begsc();
		}
|	gmark VERT error =	goto verterror;

gmark:	'!' =		begxx();

badglb: badkey =	kywerr: yyerror("Keyword expected");
|	badkey badval = 	goto kywerr;

badkey: gmark TOKEN =	begsc();

badval: '"' error '"' ;

comp_nt_ev:	simp_nt_ev ;
|		comp_nt_ev backup back_nt_ev ;

back_nt_ev:	simp_nt_ev;
|		backup back_nt_ev;

simp_nt_ev:	note ;
|		group ;
|		tempo_mark ;

backup: '<' = { 	backing_up = TRUE;

			insptr = instransfer(insptr);
		}

group:	grp_strt note_lst grp_end ;

grp_strt:	'(' = {
		RAT rtemp;
		if (backing_up) {
			push_start(&last_start);
			if (insptr->tied) {
			    rassn(&rtemp,&last_start);
			    rsub(&rtemp,&insptr->total_dur);
			    if (rcmp(&rtemp,&insptr->start) != 0)
				yyerror("bad tie to start of group");
			} else	st_assign(insptr,&last_start);
		} else	push_start(&insptr->event_start);
	}

grp_end:	')' =	rassn(&last_start,pop_start());

note_lst:	simp_nt_ev;
|		note_lst simp_nt_ev;

note:	note_term = {
		if (!insptr->tied) output(insptr);
		insinit(insptr);
		}
|	note_term plist = {
		if (!insptr->tied) output(insptr);
		insinit(insptr);
		}
|	rest = {
		insptr->this_slurred = FALSE;
		insptr->rest = TRUE;
		if (insptr->last_slurred) yyerror("Slur to rest");
		if (insptr->tied) yyerror("Tie to rest");
		insptr->tied = FALSE;
#ifdef LSCOT
		output(insptr);		/* lscot outputs rests */
#endif LSCOT
		insinit(insptr);	/* no score output */
		}
|	groupette ;

groupette:	':' '}' = {
				if (igroup<0) yyerror("no groupette open");
				else igroup--;
			}
|		'{' ':' =			groupette(3,2);
|		'{' INTEGER ':' ':' =		groupette($2,-1);
|		'{' INTEGER ':' INTEGER ':' =	groupette($2,$4);

plist:	'[' pfields ']' = {
				begsc();
				yyerrok;
		}

pfields:	= {		begex();
				whichpf = 0;
		}
|	pfields pfield =	begex();

pfield: '.' =			whichpf++;
|	PFLD =		pfldx:	pf_assign(insptr,whichpf++,yytext);
|	PNUM =			whichpf = $1-(RESERVED+1);
|	error ;

note_term:
	note_factor = {
		insptr->tied = FALSE;
		insptr->this_slurred = FALSE;
		insptr->rest = FALSE;
	}
|	note_factor slur = {
		insptr->tied = FALSE;
		insptr->this_slurred = TRUE;
		insptr->rest = FALSE;
	}
|	note_factor tie = {
		insptr->tied = TRUE;
		insptr->this_slurred = FALSE;
		insptr->rest = FALSE;
	}

slur:	'_' ;

tie:	'_' '_' ;

note_factor:	comp_pch_pt remark =		dur_carry(insptr);
|		duration comp_pch_pt remark ;

remark:	=			remark[0] = '\0';
|		REMARK =	strcpy(remark,yytext);

rest:	'r' =			dur_carry(insptr);
|	duration 'r' ;

comp_pch_pt:	simp_pch_pt = {
			if (doing_trans)
				insptr->trans_pitch = $1;
			else {
				insptr->accidental = FALSE;
				pitch_compute(insptr);
			}
		}
|		simp_pch_pt accidental_lst = {
		    if (doing_trans) insptr->trans_pitch = $1 + $2;
		    else {
			insptr->accidental = TRUE;
			key[insptr->scale_deg].acc = $2;
			/* If the bar started after when was last changed,
			   or when was changed later (on a previous copy),
			   then change the value of when.
			*/
			if ((rcmp(&bar_start,&key[insptr->scale_deg].when)==1)
			  ||(rcmp(&insptr->event_start,
				&key[insptr->scale_deg].when) == -1) )
			  rassn(&key[insptr->scale_deg].when,
				&insptr->event_start);
			pitch_compute(insptr);
		    }
		}

simp_pch_pt:	PITCH = {
			if (doing_trans) {
				$$ = key[$1].nat;
				insptr->trans_octave = 0;
			} else {
				insptr->scale_deg = $1;
/*fry*/ 			octave_following(insptr);
			}
		}
|		octave_marker PITCH = {
			if (doing_trans)
				$$ = key[$2].nat;
			else {
				insptr->scale_deg = $2;
				if (follow_octaves)
					octave_following(insptr);
				oct_modify(insptr,$1);
			}
		}
|		error PITCH =	$$ = $2;

duration:	INTEGER =		dur_assign(insptr,$1);
|		INTEGER dotlst =	dot_dur_assign(insptr,$1,$2);

dotlst: '.' =		$$ = 1;
|	dotlst '.' =	$$ = $1 + 1;

accidental_lst: 'n' =		$$ = 0;
|		flat_lst =	$$ = $1;
|		sharp_lst =	$$ = $1;

flat_lst:	'-' =		$$ = -1;
|		flat_lst '-' =	$$ = $1 - 1;

sharp_lst:	'#' =		$$ = 1;
|		sharp_lst '#' = $$ = $1 + 1;

octave_marker:	'=' = {
			if (doing_trans) {
				insptr->trans_octave = 0;
				$$ = 0;
			} else {
				insptr->old_octave = 8;
				if (follow_octaves!=1)oct_assign (insptr, 8);
				insptr->old_scale_deg = 0;
				$$ = 0;
			}
		}
|		comma_lst = {
		ap_com: if (doing_trans) {
				insptr->trans_octave = $1;
				$$ = 0;
			} else	$$ = $1;	/* modify after pitch known */
		}
|	apostrophe_lst =	goto ap_com;
|	'=' comma_lst =  {
		ap_cml: if (doing_trans) {
				insptr->trans_octave = $2;
				$$ = 0;
			} else {
				insptr->old_octave = 8;
				if (follow_octaves!=1) oct_assign (insptr, 8);
				insptr->old_scale_deg = 0;
				$$ = $2;
			}
		}
|	'=' apostrophe_lst =	goto ap_cml;

comma_lst:	',' =			$$ = -1;
|		comma_lst ',' = 	$$ = $1 - 1;

apostrophe_lst: '\'' =			$$ = 1;
|		apostrophe_lst '\'' =	$$ = $1 + 1;

%%
