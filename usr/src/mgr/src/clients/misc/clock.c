/*{{{}}}*/
/*{{{  Notes*/
/* get today's date */
/*}}}  */
/*{{{  #includes*/
#include <errno.h>
#include <time.h>
#include <unistd.h>
#include <signal.h>
#include <stdlib.h>
#include <stdio.h>

#include "term.h"
/*}}}  */
/*{{{  #defines*/
#define POLL		15			/* poll interval */
#define FIX(x)		((x)>12?(x)-12:(x))
#define dprintf		if (debug) fprintf
/*}}}  */

/*{{{  variables*/
static char line[MAXLINE];
static int x, y;		/* starting coord for clock */
static int fired = 0;		/* alarm fired */
static int armed = 0;		/* alarm armed */
static int covered = 0;		/* window is covered */
static int fx, fy;		/* font size */
static int wx, wy;		/* window size */
static char *alarm_time = NULL;
static int debug;

static struct menu_entry enable[] = {
	"ALARM CLOCK","",
	"enable alarm","+\n",
};

static struct menu_entry disable[] = {
	"ALARM CLOCK","",
	"disable alarm","-\n",
};
/*}}}  */

/*{{{  clean*/
void clean()
   {
   m_pop();
   m_ttyreset();
   exit(1);
   }
/*}}}  */
/*{{{  today*/
char *today()
   {
   struct tm *tme;
   static char result[9];
   long tmp;

   tmp = time(0);
   tme = localtime(&tmp);
   sprintf(result,"%02d:%02d",FIX(tme->tm_hour),tme->tm_min);
   return(result);
   }
/*}}}  */
/*{{{  setxy*/
void setxy()
   {
   get_font(&fx,&fy);
   get_size(0,0,&wx,&wy);
   x = (wx - strlen(today())*fx)/2;
   y = fy + (wy - fy)/2;
   if (x<0) x = 0;
   if (y<0) y = 0;
   m_clear();
   dprintf(stderr,"Setting %d x %d at %d x %d in %d x %d\n",
          strlen(today())*fx,fy,x,y,wx,wy);
   m_flush();
   }
/*}}}  */
/*{{{  update*/
void
update()
   {
   char *s = today();
   m_moveprint(x,y,s);
   m_movecursor(wx+fx,y);
   if (fired || (armed && alarm_time && strcmp(alarm_time,s)==0)) {
      dprintf(stderr,"Fireing alarm %s\n",fired?"":"FIRST TIME");
      fired = 1;
      if (covered) {
         m_setmode(M_NOINPUT);
         m_setmode(M_ACTIVATE);
         }
      m_setmode(M_WOB);
      m_printstr("\007");
      m_flush();
      sleep(2);
      m_clearmode(M_WOB);
      if (covered) {
         m_clearmode(M_ACTIVATE); 
         m_clearmode(M_NOINPUT);
         }
      }
   m_flush();
   signal(SIGALRM,update);
   alarm(POLL);
   }
/*}}}  */

/*{{{  main*/
int main(argc,argv) int argc; char **argv;
   {
   int f_flag=0, s_flag=0, b_flag=0;
   int font;
   register int i;

   ckmgrterm( *argv );

   debug = (getenv("DEBUG") != NULL);
   for(i=1;i<argc;i++)
      if (*argv[i] == '-') switch (argv[i][1]) {
         case 'f':	/* -f<fontnumber>   select alternate font */
	     f_flag++;
	     font=atoi(argv[i]+2);
	     break;
         case 'b':	/* -b   bury the window immediately */
	     b_flag++;
	     break;
         case 's':	/* -s   don't resize window, center the display */
	     s_flag++;
	     break;
         }
      else {
         alarm_time = argv[i];
         dprintf(stderr,"Setting alarm time to: %s\n",alarm_time);
         armed=1;
         };

   m_setup(0);
   signal(SIGALRM,update);
   signal(SIGINT,clean);
   signal(SIGTERM,clean);
   m_ttyset();
   if (armed) {
      m_push(P_MENU|P_EVENT|P_FLAGS|P_FONT);
      menu_load(1,2,enable);
      menu_load(2,2,disable);
      m_selectmenu(1+armed);
      }
   else
      m_push(P_EVENT|P_FLAGS|P_FONT);
   m_setmode(M_NOWRAP);
   m_setmode(M_ABS);
   m_setevent(BUTTON_1,"A\n");
   m_setevent(BUTTON_1U,"X\n");
   m_setevent(RESHAPE,"X\n");
   m_setevent(REDRAW,"X\n");
   m_setevent(COVERED,"C\n");
   m_setevent(UNCOVERED,"E\n");

   if (f_flag) m_font(font);
   if (!s_flag)
      m_size(5,1);
   if (b_flag)
      m_clearmode(M_ACTIVATE);

   setxy();
   update();
   while (1) {
      errno = 0;
      *line = '\0';
      if (m_gets(line) == NULL  &&  errno  &&  errno != EINTR)
         clean();
      alarm(0);
      dprintf(stderr,"Got: %s\n",line);
      switch (*line) {
         case 'C':			/* covered */
            covered = 1;
            break;
         case 'E':			/* exposed */
            covered = 0;
            break;
         case '+':			/* enable alarm */
            dprintf(stderr,"Arming alarm\n");
            armed = 1;
            m_selectmenu(armed+1);
            m_flush();
            break;
         case '-':			/* disable alarm */
            dprintf(stderr,"Disarming alarm\n");
            armed = 0;
            fired = 0;
            m_selectmenu(armed+1);
            m_flush();
            break;
         case 'A':			/* show alarm time */
            if (alarm_time) {
               m_moveprint(x,y,alarm_time);
               m_movecursor(wx+fx,y);
               m_flush();
               m_gets(line);
               fired = 0;
               }
            break;
         case 'X':			/* redraw, reshape */
            setxy();
            break;
         }
      update();
      }
   }
/*}}}  */
