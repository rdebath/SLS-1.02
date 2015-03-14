#include "cs.h"					/*				EXPRESS.C	*/

#define	LENTOT	200
#define	TOKMAX	50
#define	POLMAX	30
#define	XERROR(CAUSE)	{ strcpy(xprmsg,CAUSE);  goto error; }

typedef struct token {
	char	*str;
	short	prec;
} TOKEN;

POLISH	polish[POLMAX];
char	tokenstring[LENTOT];

static  TOKEN	tokens[TOKMAX], *token, *tokend = tokens+TOKMAX;
static  TOKEN	*tokenlist[TOKMAX], **revp, **pushp, **argp, **endlist;
static  int	acount, kcount, icount, Bcount, bcount;
static	char	xprmsg[40], *stringend = tokenstring+LENTOT;
static	char	strminus1[] = "-1", strmult[] = "*";

resetouts()  {	acount = kcount = icount = Bcount = bcount = 0;	}

express(s)
 char *s;
{
	POLISH	*pp;
	char	b, c, d, nextc, *t, *op, outype, *sorig;
	int	open, prec, polcnt, argcnt;

	sorig = s;
	token = tokens;
	token->str = t = tokenstring;
	open = 1;
	while (c = *s++) {
		if (open) {			/* if unary possible here,   */
			if (c == '+')		/*   look for signs:	     */
				continue;
			if (c == '-') {		/* neg const:  get past sign */
				if (*s == '.' || *s >= '0' && *s <= '9')
					*t++ = c;
				else {		/* neg symbol: prv / illegal */
					if (token > tokens
					  && *(token-1)->str == '/')
					 	XERROR("divide by unary minus")
					token++->str = strminus1;
					token++->str = strmult;
					token->str = t;  /* else -1 * symbol */
				}
				c = *s++;		/* beg rem of token */
			}
			else if (c == '*' || c == '/')	   /* unary mlt, div */
				XERROR("unary mult or divide")	/*   illegal */
			open = 0;
		}
		*t++ = c;			/* copy this character or    */
		if ((nextc = *s) == c && (c == '&' || c == '|') /* double op */
		  || nextc == '=' && (c=='<' || c=='>' || c=='=' || c=='!'))
			*t++ = c = *s++;
		if ( c == '(' || c == '+' || c == '-' || c == '*' || c == '/'
		  || c == '>' || c == '<' || c == '=' || c == '&' || c == '|'
		  || c == '?' || c == ':' )
			open = 1;		/* decl if unary can follow */
		else if (nontermin(c))
			while (nontermin(*s))	/* if not just a termin char */
				*t++ = *s++;	/*	copy entire token    */
		*t++ = '\0';			/* terminate this token      */
		(++token)->str = t;		/* & record begin of nxt one */
		if (t >= stringend)
			XERROR("token storage LENTOT exceeded");
		if (token >= tokend)
			XERROR("token storage TOKMAX exceeded");
	}
	token->str = NULL;		/* expr end:  terminate tokens array */
	if (token - tokens <= 1)	/*		& return if no expr  */
		return(0);

	token = tokens;
	while ((s = token->str) != NULL) {	/* now for all tokens found, */
		if ((c = *s) == ')')		/*  assign precedence values */
			prec = 0;
		else if (c == ',')
			prec = 1;
		else if (c == '?' || c == ':')
			prec = 2;
		else if (c == '|')
			prec = 3;
		else if (c == '&')
			prec = 4;
		else if (c == '>' || c == '<' || c == '=' || c == '!')
			prec = 5;
		else if ((c == '+' || c == '-') && *(s+1) == '\0')
			prec = 6;
		else if (c == '*' || c == '/')
			prec = 7;
		else if (c >= 'a' && c <= 'z'
		     && (t = (token+1)->str) != NULL && *t == '(')
			prec = 8;
		else if (c == '(')
			prec = 9;
		else if ((c = argtyp(s)) == 'a')
			prec = 10;
		else if (c == 'k')
			prec = 11;
		else	prec = 12;
		(token++)->prec = prec;
	}
	if (odebug) putokens();

#define	CONDVAL	2
#define	LOGOPS	3
#define RELOPS	5
#define	AOPS	6
#define	FCALL	8
#define	TERMS	10

	token = tokens;
	revp = tokenlist;
	pushp = endlist = tokenlist+TOKMAX;	/* using precedence vals, */
	while (token->str != NULL) {		/*  put tokens rev pol order */
		if (*token->str == '(') {
			token->prec = -1;
			*--pushp = token++;
		}
		else if (pushp < endlist && (*pushp)->prec >= token->prec) {
			if (*token->str == ':' && *(*pushp)->str == '?')
				*pushp = token++;	/* replace ? with : */
			else *revp++ = *pushp++;
		}
		else if (*token->str == ')') {
			if (token++ && *(*pushp++)->str != '(')
				XERROR("within parens")
		}
		else if ((token+1)->str!=NULL && token->prec < (token+1)->prec)
			*--pushp = token++;
		else *revp++ = token++;
	}
	while (pushp < endlist)
		*revp++ = *pushp++;

	endlist = revp;				   /* count of pol operators */
	if (odebug) putoklist();
	for (revp=tokenlist, polcnt=0;  revp<endlist; )	
		if ((*revp++)->prec < TERMS)	   /*  is no w. prec < TERMS */
			polcnt++;
	if (!polcnt) {				    /* if no real operators, */
		strcpy(tokenstring,tokenlist[0]->str); /* cpy arg to beg str */
		return(-1);			    /*  and return this info */
	}
	if (polcnt > POLMAX)
		XERROR("polish storage POLMAX exceeded");
	pp = &polish[polcnt-1];
	op = pp->opcod;
	for (revp=argp=tokenlist;  revp<endlist;  ) {	/* for all tokens:  */
		if ((prec = (*revp)->prec) >= TERMS) {
			*argp++ = *revp++;		/* arg: push back    */
			continue;			/*      till later   */
		}
		argcnt = argp - tokenlist;
		if (prec == FCALL && argcnt >= 1) {	/* function call:  */
			pp->incount = 1;		/*    takes one arg */
			strcpy(pp->arg[1],(*--argp)->str);
			c = argtyp(pp->arg[1]);		/* whose aki type */
			if (c == 'B' || c == 'b')
				XERROR("misplaced relational op")
			if (c != 'a' && c != 'k')
				c = 'i';		/*   (simplified)  */
			*op++ = c;
			strcpy(op,(*revp)->str);	/*    prefixes optxt */
			if (strcmp(--op,"ki") == 0)
				outype = 'i';		/* i(karg) is irreg. */
			else outype = c;		/* else outype=intype */
		}
		else if (prec >= AOPS && argcnt >= 2) {	/* arith op:	*/
			if ((c = *(*revp)->str) == '+')
				strcpy(op,"add");
			else if (c == '-')
				strcpy(op,"sub");	/*   create op text */
			else if (c == '*')
				strcpy(op,"mul");
			else	strcpy(op,"div");
			pp->incount = 2;		/*   copy 2 arg txts */
			strcpy(pp->arg[2],(*--argp)->str);
			strcpy(pp->arg[1],(*--argp)->str);
			c = argtyp(pp->arg[1]);
			d = argtyp(pp->arg[2]);		/*   now use argtyps */
			if (c == 'B' || c == 'b' || d == 'B' || d == 'b' )
				XERROR("misplaced relational op")
			if (c == 'a') {			/*   to complet optxt*/
				if (d == 'a')	strcat(op,"aa");
				else		strcat(op,"ak");
				outype = 'a';
			}
			else if (d == 'a') {
				strcat(op,"ka");
				outype = 'a';
			}
			else if (c == 'k' || d == 'k') {
				strcat(op,"kk");
				outype = 'k';
			}
			else {	strcat(op,"ii");
				outype = 'i';
			}
		}
		else if (prec >= RELOPS && argcnt >= 2) { /* relationals:   */
			strcpy(op,(*revp)->str);	/*   copy rel op    */
			if (strcmp(op,"=") == 0)
				strcpy(op,"==");
			pp->incount = 2;		/*   & 2 arg txts   */
			strcpy(pp->arg[2],(*--argp)->str);
			strcpy(pp->arg[1],(*--argp)->str);
			c = argtyp(pp->arg[1]);
			d = argtyp(pp->arg[2]);		/*   now use argtyps */
			if (c == 'a' || d == 'a')	/*   to determ outs  */
				XERROR("audio relational")
			if (c == 'B' || c == 'b' || d == 'B' || d == 'b' )
				XERROR("misplaced relational op")
			if (c == 'k' || d == 'k')
				outype = 'B';
			else outype = 'b';
		}
		else if (prec >= LOGOPS && argcnt >= 2) { /* logicals:    */
			strcpy(op,(*revp)->str);	/*   copy rel op  */
			pp->incount = 2;		/*   & 2 arg txts */
			strcpy(pp->arg[2],(*--argp)->str);
			strcpy(pp->arg[1],(*--argp)->str);
			c = argtyp(pp->arg[1]);
			d = argtyp(pp->arg[2]);		/*   now use argtyps */
			if (c == 'b' && d == 'b')	/*   to determ outs  */
				outype = 'b';
			else if ((c == 'B' || c == 'b')
			     && (d == 'B' || d == 'b'))
				outype = 'B';
			else XERROR("incorrect logical argumemts")
		}
		else if (prec == CONDVAL && argcnt >= 3) { /* cond vals:     */
			strcpy(op,": ");		/*   init op as ': ' */
			pp->incount = 3;		/*   & cpy 3 argtxts */
			strcpy(pp->arg[3],(*--argp)->str);
			strcpy(pp->arg[2],(*--argp)->str);
			strcpy(pp->arg[1],(*--argp)->str);
			b = argtyp(pp->arg[1]);
			c = argtyp(pp->arg[2]);
			d = argtyp(pp->arg[3]);
			if (b != 'B' && b != 'b'	/*   chk argtypes, */
			 || c == 'B' || c == 'b'
			 || d == 'B' || d == 'b'
			 || c == 'a' && d != 'a'
			 || d == 'a' && c != 'a')
			 	XERROR("incorrect cond value format")
			outype = 'i';			/*   determine outyp */
			if (b == 'B' || c == 'k' || d == 'k')
				outype = 'k';
			if (c == 'a' || d == 'a')
				outype = 'a';
			*(op+1) = outype;		/*   & complet opcod */
		}
		else XERROR("insufficient terms")
		s = pp->arg[0];				/* now create outarg */
		if (outype=='a') sprintf(s,"#a%d",acount++); /* acc. to type */
		else if (outype=='k') sprintf(s,"#k%d",kcount++);
		else if (outype=='B') sprintf(s,"#B%d",Bcount++);
		else if (outype=='b') sprintf(s,"#b%d",bcount++);
		else sprintf(s,"#i%d",icount++);
		(*argp++)->str = s;		   /* & point argstack there */
		revp++;
		pp--;	op = pp->opcod;			/* prep for nxt pol */
	}
	if (argp - tokenlist == 1)
		return(polcnt);			/* finally, return w. polcnt */
	XERROR("term count")

error:	synterr("expression syntax");		/* or gracefully report error*/
	printf(" %s: %s\n",xprmsg,sorig);
	strcpy(tokenstring,"1");
	return(-1);
}


nontermin(c)
 register int c;
{
	if (c == '(' || c == ')' || c == '\0'
	 || c == '+' || c == '-' || c == '*' || c == '/'
	 || c == '>' || c == '<' || c == '=' || c == '!'
	 || c == '&' || c == '|' || c == '?' || c == ':' )
	 	return(0);
	else return(1);
}

putokens()				/* for debugging check only */
{
register TOKEN	*tp = tokens;
	while (tp->str != NULL)
		putstr((tp++)->str);
	putchar('\n');
}

putoklist()				/*	ditto		*/
{
register TOKEN	**tpp = tokenlist;
	while (tpp < endlist)
		putstr((*tpp++)->str);
	putchar('\n');
}
