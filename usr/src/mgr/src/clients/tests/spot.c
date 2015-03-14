/* print info about current mouse spot */

#include <sys/time.h>
#include <signal.h>
#include "term.h"

#define SLEEP	90000			/* us to sleep between polls */
#define REST	50				/* # of still mouse reports before "resting" */

char line[100];				/* mgr input buffer */

#define fsleep(x) \
   { \
   struct timeval time; \
   time.tv_sec = 0; \
   time.tv_usec = x; \
   select(0,0,0,0,&time); \
   }

char *but = " 12";			/* button names */

main()
	{
	int count = 0;				/* # of polls since a change occurred */
	int x,y;						/* mouse coords */
	int b;						/* button state */
	int old_x = -1, old_y = -1, old_b = -1;	/* previous mouse coords */
	int id,pid;					/* window id and pid for window mouse is over */
	char tty[3];				/* space for last 2 chars of tty name */

	int cleanup();

	m_setup(M_MODEOK);
	m_ttyset();

	signal(SIGINT,cleanup);		/* program loops forever */
	signal(SIGTERM,cleanup);

	m_clear();
	m_push(P_CURSOR);
	m_setcursor(CS_INVIS);
	while(1) {	
		m_getinfo(G_MOUSE);
		m_gets(line);
		sscanf(line,"%d %d %d",&x,&y,&b);
		if (x != old_x || y != old_y || b != old_b) {
			count=0;
			m_whatsat(x,y);		/* ask for which window is mouse over */
			m_gets(line);
			if (sscanf(line,"%*s %s %d %d",tty,&id,&pid)>1)	{ /* over a window */
				fprintf(m_termout,"\r%4d %3d %1c /dev/tty%s id=%d.%d",
					x,y,but[b],tty,pid,id);
				m_cleareol();
				}
			else {										/* over background */ 
				fprintf(m_termout,"\r%4d %3d %1c",x,y,but[b]);
				m_cleareol();
				}
			old_x = x;
			old_y = y;
			old_b = b;
			}
		else
			count++;

		/* we could do a stat on /dev/mouse and watch the access time */

		if (count == REST)
			fprintf(m_termout," (Z)");
		m_flush();
		if (count > REST)
			sleep(1);
		else
			fsleep(SLEEP);
		}
	}

int
cleanup()
	{
	m_pop();
	m_ttyreset();
	exit(0);
	}
