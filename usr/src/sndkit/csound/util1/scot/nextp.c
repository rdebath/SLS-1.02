/* nextp - cheap version of ramp
 *
 *	cc -o ramp -O nextp.c -lS
 */

#include <stdio.h>
#include <ctype.h>
#define NNEXT 50
#define NINST 50
double atof();

struct next {
	int	n_insno;
	int	n_pfld;
	int	n_targ;
};

struct inst {
	int	i_insno;
	float	i_fnsno;
	char	*i_line;
};

struct next next[NNEXT];
struct inst inst[NINST];
int	nextcnt;
char	*fillp;
char	fillval[50];
char	ibuf[300];

main(argc,argv) char **argv; {
	register char *pc;
	register i;
	int	c, nnxt, insti, j;
	int	insno, pfld, targ;
	float	fnsno;

	if (argc<2) {
		fprintf(stderr,"nextp file [ ... ]\n");
		exit(1);
	}
	argc--;
	argv++;
  while (argc--) {
	if (freopen(*argv,"r",stdin)==NULL) {
		fprintf(stderr,"Can't read %s\n",*argv);
		exit(1);
	}
	argv++;
    while (fgets(ibuf,sizeof ibuf,stdin)!=NULL) {
	for (pc=ibuf; (c = *pc)==' ' || c=='\t' ; pc++);
	switch(c) {
	case 'r':
			pc++;
			while (*pc==' ' || *pc=='\t') pc++;
			if (*pc=='\n') goto specerr;
			insno = atoi(pc);
			while(isdigit(*pc)) pc++;
			while (*pc==' ' || *pc=='\t') pc++;
			if (*pc=='\n') goto specerr;
			if (*pc++ != 'n') goto specerr;
			while (*pc==' ' || *pc=='\t') pc++;
			if (*pc=='\n') goto specerr;
			pfld = atoi(pc);
			while(isdigit(*pc)) pc++;
			while (*pc==' ' || *pc=='\t') pc++;
			if (*pc=='\n') goto specerr;
			targ = atoi(pc);
			while(isdigit(*pc)) pc++;
			while (*pc==' ' || *pc=='\t') pc++;
			if (*pc!='\n') {
specerr:			crash("BAD r DECL");
			}
			if (nextcnt>=NNEXT) crash("TOO MANY NEXT DCLS");
			for (i=0; i<nextcnt; i++)
				if ( next[i].n_insno<insno ||
					( next[i].n_insno==insno &&
					  next[i].n_pfld<pfld ) )
					continue;
				else	break;
			if (next[i].n_insno==insno && next[i].n_pfld==pfld)
				crash("duplicate declaration of next");
			j = i;
			for (i=nextcnt++; i>j; i--) {
				next[i].n_insno = next[i-1].n_insno;
				next[i].n_pfld	= next[i-1].n_pfld;
				next[i].n_targ	= next[i-1].n_targ;
			}
			next[j].n_insno = insno;
			next[j].n_pfld = pfld;
			next[j].n_targ = targ;
			continue;
	case 'i':	insno = atoi(pc+1);
			fnsno = atof(pc+1);
/* printf("ICARD: %d %.3f :%s",insno,fnsno,pc);
*/			if (insno<=0) crash("BAD ICARD");
			insti = -1;
			for (i=0;i<NINST;i++)
				if (  inst[i].i_insno==insno
				   && inst[i].i_fnsno==fnsno
				) {
					insti = i;
/* printf("FILL FROM :%s",inst[i].i_line);
*/					fill(&inst[i],pc);
					break;
				}
			for (i=nnxt=0; i<nextcnt; i++)
				if (next[i].n_insno==insno) nnxt++;
			if (nnxt==0) goto copy;
			if ((i=insti)<0) {
				for (i=0; inst[i].i_insno; i++);
				insti = i;
			}
			inst[i].i_insno = insno;
			inst[i].i_fnsno = fnsno;
			inst[i].i_line = calloc(strlen(pc)+1,1);
			strcpy(inst[i].i_line,pc);
			continue;
	case 's':
	case 'e':	iflush();
	default:
	copy:		fputs(pc,stdout);
	case '\n':	continue;
	}
    }
	iflush();
	if (ferror(stdin)) crash("error on input");
  }
}

fill(pi,pc)
 struct inst *pi;
 char *pc;
{
	register struct next *pn;
	register char *pp;
	struct next *pne = &next[nextcnt];
	int pnum = 1;
	int pnumi;
	fillp = pi->i_line;
	for (pn=next; pn<pne && pn->n_insno<pi->i_insno; pn++);
    while (pn<pne && pn->n_insno==pi->i_insno) {
	while (pnum<pn->n_pfld) {
		if (pnum>1) putc(' ',stdout);
		fillfld();
		fputs(fillval,stdout);
		pnum++;
	}
	fillfld();
	putc(' ',stdout);
	if (fillval[0] != '.' || fillval[1]!='\0') {
		fputs(fillval,stdout);
		pnum++;
		pn++;
		continue;
	}
	for (pnumi=1, pp=pc; pnumi<pn->n_targ; pnumi++) {
		while(*pp==' ' || *pp=='\t') pp++;
		if (*pp=='\n') break;
		while (!isspace(*pp)) pp++;
	}
	while (*pp==' ' || *pp=='\t') pp++;
	if (*pp=='\n')	fputs(fillval,stdout);
	else		while (!isspace(*pp)) putc(*pp++,stdout);
	pnum++;
	pn++;
    }
	fputs(fillp,stdout);
	cfree(pi->i_line);
	pi->i_line = 0;
}

fillfld() {
	register char *pp, *pf;
	pf = fillval;
	pp = fillp;
	while (*pp==' ' || *pp=='\t') pp++;
	if (*pp=='\n') goto filxit;
	while (!isspace(*pp)) *pf++ = *pp++;
	*pf = '\0';
	fillp = pp;
	return;
filxit:
	*pf++ = '.';
	*pf++ = '\0';
	fillp = pp;
}

iflush() {
	register char *pc;
	register struct inst *pi;
	for (pi=inst; pi < &inst[NINST];pi++) {
		if (pi->i_insno && (pc=pi->i_line) ) {
			fill(pi,pi->i_line);
			pi->i_insno = 0;
		}
	}
}

crash(s) char *s; {
	fprintf(stderr,"next: %s\n",s);
	if (ibuf[0]) fputs(ibuf,stderr);
	exit(1);
}
