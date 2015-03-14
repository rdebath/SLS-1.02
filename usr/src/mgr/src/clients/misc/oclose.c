/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */
/*	$Header: oclose.c,v 4.2 88/06/22 14:37:55 bianchi Exp $
	$Source: /tmp/mgrsrc/demo/misc/RCS/oclose.c,v $
*/
static char	RCSid_[] = "$Source: /tmp/mgrsrc/demo/misc/RCS/oclose.c,v $$Revision: 4.2 $";

/* close a window */

#include <signal.h>
#include "term.h"

#define POS(x)		800+(x)/18		/* where to put window */
#define Max(x,y)	((x)>(y)?(x):(y))

main(argc,argv)
int argc;
char **argv;
   {
   char host[16];
   char *text;
   char line[2];
   int x,y,wide,high;
   int clean();
   int font;

   /* get icon text */

   if (argc>1)
      text = argv[1];
   else {
      gethostname(host,sizeof(host));
      text = host;
      }

   m_setup(0);
   m_push(P_ALL);	
   signal(SIGINT,clean);
   signal(SIGTERM,clean);
   m_setevent(ACTIVATED,"A\n");
   m_setevent(REDRAW,"R\n");

   get_size(&x,&y,&wide,&high);
   if (argc>2 && (font=atoi(argv[2])) > 0)
      m_font(font);
   m_sizeall(x,POS(y+high/2),Max(strlen(text),5),1);
   m_setmode(M_NOWRAP);
   m_setmode(M_BACKGROUND);
   m_clear();
   m_printstr(text);
   m_clearmode(M_ACTIVATE);
   m_flush();

   while(1) {
      read(0,line,sizeof(line));
      m_clear();
      m_printstr(text);
      m_flush();
      if (*line == 'A')
         clean();
      }
   }
clean()
   {
   m_pop();
   exit(1);
   }
