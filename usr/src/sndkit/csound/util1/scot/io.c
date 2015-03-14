#ifdef YYDEBUG
#define DEBUG DEBUG
#endif

#include "y.tab.h"
#include "data.h"
#include <ctype.h>

main (argc, argv)
 int argc;
 char **argv;
{
	s_av = argv;
	s_ac = argc;
	s_ac0 = argc;
	nextin();
	if ((outfil=fopen("score","w"))==NULL) {
		fprintf(stderr,"scot: cannot open output file\n");
		exit(ERROR);
	}
	if (yyparse())			/* Last error was "fatal" */
		printf("\nprocessing of score terminated.\n");
	fclose(outfil);
	if (errcnt>0) {
		fprintf(stderr,"scot: %d errors detected.\n",errcnt);
		exit(ERROR);		/* indicates error */
	}
	if (!need_carry) exit(OK);
	unlink(scrtmp);
	if (link(scrnam,scrtmp)<0) goto nogo;
	if (unlink(scrnam)<0) goto nogo;
	if (freopen(scrnam,"w",stdout)==NULL) goto nogo;
	execl(ramper,ramper,scrtmp,0);	/* no return */
nogo:	fprintf(stderr,"scot: can't exec ramp\n");
	exit(ERROR);
}

nextin() {
	if ((--s_ac)<=0) return(0);
	if (freopen(*++s_av,"r",stdin)==NULL) {
		fprintf(stderr,"scot: can't read %s\n",*s_av);
		exit(ERROR);
	}
	yylineno = 1;
	return(1);
}

gchar() {
#ifdef DEBUG
	extern int yydebug;
#endif
	register char c;
	if ((!exping)&&(expr<expw)) return(*expr++);
	if (pback) return(pbackb[--pback]);
	while(1) {
		if (c = *pin++) {
#ifdef DEBUG
			if (c=='~') {
				yydebug = (yydebug+1)&1;
				continue;
			}
#endif
			if (c=='\n') yylineno++;
			return(c);
		}
		pin = inbuf;
		if (fgets(inbuf,INBUFL,stdin)!=NULL) continue;
		if (ferror(stdin)) {
			fprintf(stderr,"scot: io error on input\n");
			exit(ERROR);
		}
		inbuf[0] = '\0';
		if (nextin()!=0) continue;
		pin++;
		return(0);
	}
}

ungchar(c) {
	if ((!exping)&&(expr<expw))	expr--;
	else				pbackb[pback++] = c;
}

lookup() {
	register char **ps;
	register char *p, *r;
	int n;
	for (n=0, ps=delimiters; p = *ps++; n++ ) {
		if (prefix(yytext,p)) {
			if (*ps && prefix(yytext,*ps))
				yyerror("ambiguous abbreviation");
			return(delimtokens[n]);
		}
	}
	for (r=tokenval, p=yytext; *r++ = *p++; );
	return(TOKEN);
}

	struct symbol *
symsrch(name,s)
  char *name;
  register struct symbol *s;
{
	while (s)
		if (compare(name,s->s_name)) return(s);
		else s=s->s_nxt;
	return(0);
}

copysec() {
	register char c;
	register comsw = 0;
	while ((c=gchar()) != '}') {
		if (c==';') comsw++;
		if (c=='\n') comsw = 0;
		if (comsw==0) fputc(c,outfil);
	}
	fputc('\n',outfil);
}

char corexx[] =	"Out of macro space";

	char *
copytok(p)
 char *p;
{
	register char *rp, *rt;
	while (1) {
		for (rp=p, rt=tokpol; (rt<tokpox) && (*rt++ = *rp); rp++);
		if (*rp=='\0') {
			rp = tokpol;
			tokpol = rt;
			return(rp);
		}
		if ((tokpox=tokpol=(char *)calloc(TOKALO,sizeof *p))==0) {
			yyerror(corexx);
			return(0);
		}
		tokpox = tokpol + TOKALO;
	}
}

	struct symbol *
maksym(name,txt,chn)
 char *name, *txt;
 struct symbol **chn;
{
	register struct symbol *p;
	while (1) {
		if ((p=sympol++)<sympox) {
			p->s_name = name;
			p->s_txt = txt;
			p->s_nxt = *chn;
			*chn = p;
			return(p);
		}
		if ((sympox=sympol=(struct symbol *)
				calloc(SYMALO,sizeof(struct symbol)))==0) {
			yyerror(corexx);
			return(0);
		}
		sympox = sympol + SYMALO;
	}
}

yyerror(s)
 char *s;
{
	register int n;
	register char *p;
	extern int yychar;
	errcnt++;
	if (s_ac0>2) printf("%s: ",*s_av);
	printf("line %d: %s\n",yylineno,s);
	if (yychar==0) {
		printf("unexpected end of file\n");
		return;
	}
	for (n=0, p=inbuf; *p; putchar(*p++)) {
		if (p<pin) {
			if (*p=='\t')	n = (n|7) + 1;
			else		n++;
		}
	}
	for ( ; (--n)>0 ; putchar(' '));
	printf("|\n");
}
