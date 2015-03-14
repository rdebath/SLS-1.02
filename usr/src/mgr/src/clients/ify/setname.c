/* setname.c - specify the name to appear when window is iconified
   Jim Blandy - Monday, July 17, 1989 - Bell Communications Research */

#include <stdlib.h>
#include <stdio.h>

#include "term.h"

char *progname;			/* name of program */

#define MAXHOSTLEN (80)		/* maximum length of host name  */
char *buf;			/* where we accumulate the name */
int bufsize;			/* how many bytes it has allocated to it */

#define ADD(size) assure(bufsize+(size))

char usage[] = "\
Usage: %s <window name>\n\
changes the name of the window to <window name>.\n\
<window name> may contain:\n\
  %%m - current host name\n\
  %%w - width of the window, in columns of text\n\
  %%h - height of the window, in rows of text\n\
  %%p - vague verbal description of the window's position\n\
  %%%% - a single %%\n";

main(argc, argv)
     int argc;
     char **argv;
{
    int bufend;
    int cols, rows;
    char hostname[MAXHOSTLEN];
    int disp_w, disp_h;
    int wind_x, wind_y, wind_w, wind_h;
    int eps_x, eps_y;

    progname = argv[0];

    if (argc == 1) {
	fprintf(stderr, usage, progname);
	exit(1);
    }
    
    m_setup(M_FLUSH);
    m_ttyset();
    get_param(NULL, &disp_w, &disp_h, NULL);
    gethostname(hostname, MAXHOSTLEN);
    eps_x = disp_w>>3;
    eps_y = disp_h>>3;
    get_colrow(&cols, &rows);
    get_size(&wind_x, &wind_y, &wind_w, &wind_h);
    m_ttyreset();
    assure(100);

    bufend = 0;
    for (argc--, argv++; argc>0; argc--, argv++) {
	char *p;

	assure(bufend+strlen(*argv));

	for (p = *argv; *p; p++)
	    if (*p == '%')
	      switch(p[1]) {
		case '%':	/* an ordinary % */
		  buf[bufend++] = *++p;
		  break;
		case 'm':	/* machine name */
		  {
		      int len;

		      ADD(len = strlen(hostname));
		      strcpy(buf+bufend, hostname);
		      bufend+=len;
		      p++;
		  }
		  break;
		case 'w':	/* columns in window */
		  ADD(3);
		  sprintf(buf+bufend, "%d", cols);
		  bufend+=strlen(buf+bufend);
		  p++;
		  break;
		case 'h':	/* rows in window */
		  ADD(3);
		  sprintf(buf+bufend, "%d", rows);
		  bufend+=strlen(buf+bufend);
		  p++;
		  break;
		case 'p':	/* vague description of window's */
				/* position*/
		  {
		      int h = wind_x - (disp_w - (wind_x+wind_w));
		      int v = wind_y - (disp_h - (wind_y+wind_h));

		      ADD(15);

		      if (v<-eps_y)
			strcpy(buf+bufend, "upper");
		      else if (v>eps_y)
			strcpy(buf+bufend, "lower");
		      else
			strcpy(buf+bufend, "middle");

		      bufend += strlen(buf+bufend);

		      if (h<-eps_x)
			strcpy(buf+bufend, " left");
		      else if (h>eps_x)
			strcpy(buf+bufend, " right");
		      else if (v<-eps_x || v>eps_x)
			strcpy(buf+bufend, " middle");

		      bufend += strlen(buf+bufend);
		      p++;
		  }
		  break;
		default:
		  buf[bufend++] = *p;
		  break;
	      }
	    else
	      buf[bufend++] = *p;

	if (argc>1)
	  buf[bufend++] = ' ';
    }

    buf[bufend] = '\0';
    m_setevent(NOTIFY, buf);
    die(NULL);
}


die(mesg, err)
     char *mesg;
     int err;
{
    if (mesg && mesg[0] != '\0')
      fprintf(stderr, "%s: %s\n", progname, mesg);
    exit(err);
}


assure(size)
     int size;
{
    if (size < bufsize)
      return;
    size = (size + 127L) & ~127L;
    if ((buf == NULL && (buf = malloc(size)) == NULL)
	|| (buf = realloc(buf, size)) == NULL)
      die("out of memory");
}
