/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */
/*	$Header: mgrmsgs.c,v 4.2 88/06/22 14:37:53 bianchi Exp $
	$Source: /tmp/mgrsrc/demo/misc/RCS/mgrmsgs.c,v $
*/
static char	RCSid_[] = "$Source: /tmp/mgrsrc/demo/misc/RCS/mgrmsgs.c,v $$Revision: 4.2 $";

/* check for new messages */

#include <stdio.h>
#include <signal.h>
#include <sgtty.h>
#include "term.h"

#define Isflag(arg,flag)	(!strncmp(arg,flag,strlen(flag)))
#define Max(x,y)		((x)>(y)?(x):(y))
#define MENU_COUNT	(sizeof(menu)/sizeof(struct menu_entry))
#define SCMP(x,y)	(strcmp(x+(strlen(x)-strlen(y)),y)==0)

#define RESUME	"Resume\n"
#define MSGSCMD	"msgs -p;echo -n Done?\ "
#define BOUNDS	"/usr/msgs/bounds"
#define RC	".msgsrc"
#define POLL	 60				/* polling interval */
#define XPOS	220				/* start of msgs window */
#define YPOS	170				/* start of msgs window */


#define MSGS()	(1 + get_bounds(bounds) - get_rc(rc))

FILE *bounds, *rc;
int msg_cnt, old_msg_cnt;
char line[100];
int poll=POLL;

struct menu_entry menu[] = {
   "yes",    "y\n",
   "skip",   "n\n",
   "again",  "-\n",
   "save",   "s\n",
   "quit",   "q\n",
   };

main(argc,argv)
int argc;
char **argv;
   {
   register int i;
   int xpos = XPOS;
   int ypos = YPOS;
   int font = -1;
   int shape = 1;

   char *getenv();
   char *home = getenv("HOME");
   int clean(), update();

   /* make sure we have a valid environment to run in */

   ckmgrterm( *argv );

   if (home==NULL || *home=='\0') {
      fprintf(stderr,"%s: Can't find your home directory\n",argv[0]);
      exit(1);
      }

   if ((bounds = fopen(BOUNDS,"r")) == NULL) {
      fprintf(stderr,"%s: Can't find a bounds file\n",argv[0]);
      exit(2);
      }

   sprintf(line,"%s/%s",home,RC);
   
   if ((rc = fopen(line,"r")) == NULL) {
      fprintf(stderr,"%s: Can't find %s\n",argv[0],line);
      exit(3);
      }

   /* process arguments */

   for(i=1;i<argc;i++) {
      if (Isflag(argv[i],"-s"))
         shape = 0;
      else if (Isflag(argv[i],"-x"))
         xpos = atoi(argv[i]+2);
      else if (Isflag(argv[i],"-y"))
         ypos = atoi(argv[i]+2);
      else if (Isflag(argv[i],"-f"))
         font = atoi(argv[i]+2);
      else if (Isflag(argv[i],"-p"))
         poll  = Max(atoi(argv[i]+2),10);
      else
         usage(argv[0],argv[i]);
      }

   /* setup mgr stuff */

   m_setup(M_FLUSH);
   m_push(P_MENU|P_EVENT|P_FLAGS);
   m_setmode(M_NOWRAP);
   m_ttyset();

   signal(SIGTERM,clean);
   signal(SIGINT,clean);
   signal(SIGALRM,update);

   menu_load(1,MENU_COUNT,menu);
   m_selectmenu(1);

   old_msg_cnt = MSGS();
   get_msg(line,old_msg_cnt);
   if (shape) {
      m_setmode(M_ACTIVATE);
      m_size(strlen(line)+2,1);
      }
   m_printstr(line);
   m_setevent(REDRAW,"R\n");
   m_setevent(ACTIVATED,"A\n");
   m_clearmode(M_ACTIVATE);
   alarm(poll);

   while(1) {
      char buff[80];
      m_gets(buff);
      alarm(0);

      /* read msgs */

      old_msg_cnt = msg_cnt;
      msg_cnt = MSGS();
      if (msg_cnt > 0 && *buff == 'A') {
         m_push(P_POSITION|P_EVENT|P_FLAGS|P_FONT);
         if (font != -1)
            m_font(font);
         m_sizeall(xpos,ypos,80,24);
         m_printstr("\freading msgs...\r");
         m_ttyreset();
         system(MSGSCMD);
         m_gets(buff);
         m_ttyset();
         m_pop();
         }

      /* wait for window to deactivate */

      else if (*buff == 'A') {
         m_setevent(DEACTIVATED,RESUME);
         do {
            m_printstr("\f Your msgs system here        ");
            m_gets(buff);
            } while(!SCMP(buff,RESUME));
         m_clearevent(DEACTIVATED);
         }
      old_msg_cnt = msg_cnt;
      msg_cnt = MSGS();
      get_msg(line,msg_cnt);
      m_printstr(line);
      m_clearmode(M_ACTIVATE);
      alarm(poll);
      }
   }
    
int
get_rc(file)
FILE *file;
   {
   char line[100], *fgets();
   fseek(file,0,0);
   if (fgets(line,sizeof(line),file) != NULL) 
      return(atoi(line));
   else
      return(0);
   }

int
get_bounds(file)
FILE *file;
   {
   char buff[100], *line, *fgets();
   fseek(file,0,0);
   if ((line=fgets(buff,sizeof(buff),file)) != NULL) {
      while(*line != ' ') line++;
      while(*line == ' ') line++;
      return(atoi(line));
      }
   else return(0);
   }

get_msg(msg,cnt)
int cnt;
char *msg;
   {
   if (cnt > 0)
      sprintf(msg,"\fYou have %d message%s waiting\r",cnt,cnt!=1?"s":"");
   else if (cnt == 0)
      sprintf(msg,"\fLooking for new messages\r");
   else 
      sprintf(msg,"\fMessage system scrunched\r");
   }

clean()
   {
   m_ttyreset();
   m_pop();
   exit(1);
   }

update()
   {
   msg_cnt = MSGS();
   get_msg(line,msg_cnt);
   if (msg_cnt != old_msg_cnt) {
      if (msg_cnt > old_msg_cnt) 		/* new messages */
         m_printstr("\007");
      m_printstr(line);
      }
   old_msg_cnt = msg_cnt;
   alarm(poll);
   }

usage(name,error)
char *name, *error;
{
	fprintf(stderr,"Invalid flag: %s\n",error);
	fprintf(stderr,"usage: %s -[s|x<pos>|y<pos>|f<font>|p<poll>\n",name);
	exit(1);
}
