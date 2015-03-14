%{
/*
                         Copyright (c) 1988 Bellcore
                             All Rights Reserved
        Permission is granted to copy or use this program, EXCEPT that it
        may not be sold for profit, the copyright notice must be reproduced
        on copies, and credit should be given to Bellcore where it is due.
        BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.

	$Header: croff.l,v 1.2 88/07/07 10:17:25 sau Exp $
	$Source: /tmp/mgrsrc/doc/usrman/croff/RCS/croff.l,v $
*/
static char	RCSid_[] = "$Source: /tmp/mgrsrc/doc/usrman/croff/RCS/croff.l,v $$Revision: 1.2 $";

/* is this lex abuse? */


#include "hash.h"

#define dprintf	if(debug)fprintf

extern struct table_entry keywords_data[];
extern struct table_entry *keywords[];

char *ks,*ke;	/* keyword delimeters */
char *cs,*ce;	/* comment delimeters */
char *is,*ie;	/* identifier delimeters */
char *fs,*fe;	/* function delimeters */
char *ps,*pe;	/* pre-proc delimeters */

int force = 0;		/* force xlation without .SS */
int no_troff = 0;	/* dont xlate \ etc */
int no_lig = 0;		/* on't emit troff ligatures (brain damaged imagen) */
int debug = 0;		/* do debugging */

%}

%START start comment quote
W	[ 	]*
K	[a-zA-Z_][a-zA-Z0-9_]*

%%


^".SS".*$ {			/* start source processing */
        static char t1[25],t2[25],t3[25],t4[25];
	int n;
	BEGIN start;
        if (!force) {
	   ECHO;
	   n = sscanf(yytext,".SS %s %s %s %s",t1,t2,t3,t4);
           switch(n) {
              case 4: case 5: case 6: case 7: case 8: case 9:
                 ce = t4;
              case 3:
                 ke = t3;
              case 2:
                 cs = t2;
              case 1:
                 ks = t1;
              }
	   }
	}
<start,comment,quote>^".SE" {	/* end source processing */
	if (!force)
           BEGIN 0;
	ECHO;
	}
<start>"/*" {			/* start a comment */
	BEGIN comment;
	printf("%s",cs);
	ECHO;
	dprintf(stderr,"[CS]");
	}
<comment>"*/" {			/* end a comment */
	ECHO;
	printf("%s",ce);
	BEGIN start;
	dprintf(stderr,"[CE]");
	}
<start,comment,quote>"\-|" {	/* handle troff escapes */
	if (no_troff)
           ECHO;
	else switch(*yytext) {
           case '-':  printf("\\-");   break;
	   case '\\': printf("\\e");   break;
	   case '|':  printf("\\(or"); break;
           }
	dprintf(stderr,"%s",yytext);
	}
<start>\" {			/* start quote */
	BEGIN quote;
	ECHO;
	dprintf(stderr,"[\"");
	}
<quote>\\\" {			/* \" in a string */
	printf("\\e\"");
	}
<quote>\" {			/* end quoted string */
	BEGIN start;
	ECHO;
	dprintf(stderr,"\"]");
	}
<start,comment,quote>[fF][il]	|
<start,comment,quote>ff {	/* defeat troff ligatures */
	if (no_lig)
	   printf("%c\\&%c",yytext[0],yytext[1]);
	else
           ECHO;
	dprintf(stderr,"[%s]\n",yytext);
	}
<start>^#{W}define	|
<start>^#{W}undef	|
<start>^#{W}line	|
<start>^#{W}include	|
<start>^#{W}ifdef	|
<start>^#{W}ifndef	|
<start>^#{W}if		|
<start>^#{W}endif	|
<start>^#{W}else {		/* pre-processor tokens */
	   printf("%s%s%s",ps,yytext,pe);
	   dprintf(stderr,"%s%s%s","[PS]",yytext,"[PE]");
	}
<start>{K}/{W}\( {		/* look for key words  - might be functions */
	if (is_entry(keywords,HSIZE,yytext)) 
	   printf("%s%s%s",ks,yytext,ke);
 	else
	   printf("%s%s%s",fs,yytext,fe);
	}
<start>{K} {			/* look for key words  - might be identifiers */
	if (is_entry(keywords,HSIZE,yytext)) 
	   printf("%s%s%s",ks,yytext,ke);
 	else
	   printf("%s%s%s",is,yytext,ie);
	}
%%

#define GET_OPT(i)	\
	strlen(argv[i])>2 ? argv[i]+2 : argv[++i]

main(argc,argv)
int argc;
char **argv;
   {
   register int i;
   register char c = '\0';

   /* set default values */

   ks = "\\fB";		/* keywords in bold */
   ke = "\\fP";
   cs = "\\fI";		/* comments in bold */
   ce = "\\fP";
   is = "";
   ie = "";
   fs = "";
   fe = "";
   ps = "\\fB";		/* cpp decl's in bold */
   pe = "";

   debug = getenv("DEBUG");

   /* get arguments */

   for(i=1;i<argc;i++) {
      if (*argv[i] == '-')
         switch (c = argv[i][1]) {
            case 'a':	/* add keyword to list */
               add_entry(keywords,HSIZE,GET_OPT(i));
               break;
            case 'd':	/* delete keyword from list */
               dlt_entry(keywords,HSIZE,GET_OPT(i));
               break;
            case 'F':	/* force processing without .SS */
               unput('\n');unput('S'); unput('S'); unput('.');
               force++;
               break;
            case 'l':	/* don't pass any ligatures to troff */
               no_lig++;	
               break;
            case 't':	/* don't futz with \, | or - \n */
               no_troff++;	
               break;
            case 'p':	/* set pre-processor escape prefix */
               ps = GET_OPT(i);
               break;
            case 'i':	/* set identifier escape prefix */
               is = GET_OPT(i);
               break;
            case 'f':	/* set function escape prefix */
               fs = GET_OPT(i);
               break;
            case 'k':	/* set keyword escape prefix */
               ps = ks = GET_OPT(i);
               break;
            case 'c':	/* set comment escape prefix */
               cs = GET_OPT(i);
               break;
            case 'e':	/* set all ending escape prefixen */
               ie = pe = fe = ke = ce = GET_OPT(i);
               break;
            default:
               fprintf(stderr,"%s: flag %s ignored\n",*argv,argv[i]);
               break;
            }
         else switch(c) {	/* optional turn-off codes */
            case 'p':
               pe = argv[i];
               break;
            case 'i':
               ie = argv[i];
               break;
            case 'f':
               fe = argv[i];
               break;
            case 'k':
               pe = ke = argv[i];
               break;
            case 'c':
               ce = argv[i];
               break;
            default:
               fprintf(stderr,"%s: arg [%s] ignored\n",*argv,argv[i]);
               break;
            }
      }
   yylex();
   }

#ifndef yywrap
#define yywrap() 1
#endif
