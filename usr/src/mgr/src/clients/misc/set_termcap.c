/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */
/*	$Header: set_termcap.c,v 4.3 88/06/23 09:10:41 bianchi Exp $
	$Source: /tmp/mgrsrc/demo/misc/RCS/set_termcap.c,v $
*/
static char	RCSid_[] = "$Source: /tmp/mgrsrc/demo/misc/RCS/set_termcap.c,v $$Revision: 4.3 $";

/* print the current termcap entry on stdout */

#include <stdio.h>
#include "window.h"

#define ENTRY	"\'px|mgr|mgr teminal emulator:am:li#%d:co#%d:bs:cl=^L:ce=\\Ec:cd=\\EC:cm=\\E%%r%%d,%%dM:al=\\Ea:dl=\\Ed:ic=\\EA:dc=\\EE:ta=^I:up=\\Eu:do=\\Ef:nd=\\Er:ku=\\E[A:kd=\\E[B:kr=\\E[C:kl=\\E[D:so=\\Ei:se=\\En:vs=\\EV:ve=\\Ev:\'"

main(argc,argv)
int argc;
char **argv;
   {
   int b_flag=1;
   int cols, rows;
   char *rindex(), *getenv(), *getpass();
   char *shell = getenv("SHELL");
   char get[5];

   ckmgrterm( *argv );

   if (argc>1 && strcmp("-b",argv[1])==0)
      b_flag=0;

   if (shell && rindex(shell,'/'))
      shell = rindex(shell,'/')+1;

   sprintf(get,"%c%d%c",ESC,G_WINSIZE,E_GETINFO);
   sscanf(getpass(get),"%d %d\n",&cols,&rows);
   
   if (b_flag && shell && strcmp(shell+strlen(shell)-3,"csh")==0) {		/* csh */
      printf("set noglob;");
      printf("setenv TERMCAP ");
      printf(ENTRY,rows,cols);
      printf(";unset noglob\n");
      }
   else							/* /bin/sh */
     {
      printf("export TERMCAP\n");
      printf("TERMCAP=");
      printf(ENTRY,rows,cols);
      putchar('\n');
     }
   } 
