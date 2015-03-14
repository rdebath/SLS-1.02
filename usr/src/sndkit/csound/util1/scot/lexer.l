%{
#include "y.tab.h"
#include "data.h"
#define output(c)	return(yylval=c)
#define input(c)	gchar()
#define unput(c)	ungchar(c)
int	yylval;
%}

%S SC BR EX D1 D2 RM
D [0-9]
T [0-9a-zA-Z_]
X [\040\t\n\f]
B [\+\-\.\<\>\:\']
%%

;.*$			{}
<INITIAL,SC,D1,D2>{X}+	{}
<INITIAL,SC>{D}+	{	yylval = atoi(yytext);
				return(INTEGER);
			}
<INITIAL>{T}+			return(yylval=lookup());
<SC>\${T}+		{	register char *p, *r;
				p=tokenval;
				r=yytext+1;
				yylval=0;
				while (*p++ = *r++) yylval++;
				return(INAME);
			}
<SC>[a-gA-G]		{	register c;
				c = yytext[0] | 040;
				yylval = (c>'b' ? c-'a'-2 : c-'a'+5);
				return(PITCH);
			}
<BR>(\'?([+-]?(({D}+(\.{D}*)?)|(\.{D}+))))|[<>]|np{D}+|pp{D}+	  {
				return(PFLD);
			}
<BR>{D}+{X}*:		{	yylval = atoi(yytext);
				return(PNUM);
			}
<BR>{X}+			begex();
<EX>(np|pp){D}+ 	{	register char *s, *t;
				for (t=yytext, s=expw;
					(s<exptxt+EXPTXT)&&(*s = *t++); s++);
				expw = s;
			}
<EX>[a-zA-Z]+		{	register struct symbol *p;
				register char *t, *x;
				struct symbol *symsrch();
				if ((p=symsrch(yytext,symins[curins]))==0 &&
				    (p=symsrch(yytext,symgbl))==0 ) {
					yyerror("undefined macro");
					break;
				}
				for (t=x=p->s_txt; *x++; );
				x--;
				while (x>t) unput(*--x);
			}
<EX>({D}|{B})+		{	register char *s, *t;
				for (t=yytext, s=expw;
					(s<exptxt+EXPTXT)&&(*s = *t++); s++);
				expw = s;
			}
<EX>({X}+|\])		{	unput(yytext[0]);
				begbr();
			}
<D1>[a-zA-Z]+			goto cpytok;
<D2>({T}|{B})+		{
			cpytok: yylval = copytok(yytext);
				return(TOKEN);
			}
<D2>\"({T}|{B}|{X})*\"	{	yytext[--yyleng] = '\0';
				yylval = copytok(yytext+1);
				return(TOKEN);
			}
<SC>\\				begrm();
<RM>[^\\]*			return(REMARK);
<RM>\\				begsc();
<RM>\n			{	yyerror("unterminated \\remark\\");
				begsc();
			}
.				return(yylval=yytext[0]);
%%

begxx() { BEGIN INITIAL; }
begsc() { BEGIN SC; }
begd1() { BEGIN D1; }
begd2() { BEGIN D2; }
begrm()	{ BEGIN RM; }
begbr() { BEGIN BR; exping = 0; }
begex() { BEGIN EX;
		exping++;
		if (expr==expw)
			expr = expw = exptxt;
}
