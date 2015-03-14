/*************************************************************************
 * 
 * A short X-windows program to display kernel messages in a window.
 * Created because xterm -C doesn't work in linux, and its nice to
 * see error messages when a mount or mtools operation fails, etc.
 *
 * Written by Robert Nation (nation@rocket.sanders.lockheed.com) 2/93.
 *
 * This file Copyright 1992 Robert J. Nation 
 * It may be distributed under the GNU Public License, version 2, or
 * any higher version.  See section COPYING of the GNU Public license
 * for conditions under which this file may be redistributed.
 *
 * Note: this is my first X-windows program. If I did it all wrong, please
 * tell me.
 *************************************************************************/

#include <stdio.h>
#include <sys/types.h>
#include <sys/utsname.h>
#include <unistd.h>
#include <time.h>

#include <fcntl.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Text.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Command.h>

Widget toplevel;
Widget logwindow,compwindow,timewindow,quitbutton;
XtAppContext app_con;

/* file descriptor for the kernel /proc/kmsg file */
int kmsg=0;

/* Items used to retrieve the system name */
#define UTSLENGTH 64
char line[UTSLENGTH*3+10];
struct utsname name;
char tbuf[50];

/* file descriptor for the log file */
FILE *logfd = (FILE *)0;


/***************************************************************************
 * 
 * This subroutine gets called at most once per minute to update the time.
 * It is called by a time-out callback 
 *
 **************************************************************************/
void print_date(client_data, id)
XtPointer client_data;
XtIntervalId *id;
{
  time_t tp;
  struct tm *curtime;
  Arg arg[10];
  int nargs;

  time(&tp);
  curtime = localtime(&tp);
  strftime(tbuf,49,"%a %b %d %H:%M %Y",curtime);
  nargs=0;
  XtSetArg( arg[nargs], XtNlabel, tbuf);nargs++;
  XtSetValues( timewindow, (ArgList)arg, nargs);
  XtAppAddTimeOut(app_con,60000,print_date,NULL);
}

/***************************************************************************
 * 
 * This subroutine gets called when the user presses the quit button.
 * It closes the open files and exits.
 *
 **************************************************************************/
void quit_callback(widget,closure,callData)
     Widget widget;
     caddr_t closure;
     caddr_t callData;
{
  close(kmsg);
  fclose(logfd);
  exit(0);
}


/***************************************************************************
 * 
 * This subroutine gets called when there is some input waiting in 
 * /proc/kmsg 
 *
 **************************************************************************/
void handle_kmsg_input(client_data, source, id)
caddr_t client_data;
int source;
XtInputId id;
{
   XawTextPosition pos1;
   XawTextBlock tt;
   int chars_read,nargs,i;
   Arg arg[10];

   /* read the input */
   chars_read =read(kmsg,line,99);
   /* make sure that there really was something to read */
   if(chars_read < 1)
     return;

   /* Get rid of Carriage returns, since they cause double-spacing */
   for(i=0;i<chars_read;i++)
     if(line[i]==13)
       {
	 strcpy(&line[i],&line[i+1]);
	 chars_read--;
       }
   line[chars_read]=0;
   tt.firstPos = 0;
   tt.ptr = line;
   tt.length = strlen(tt.ptr);
   if(line[0]=='\r')
     {
       tt.length--;
       tt.ptr++;
     }

   tt.format = FMT8BIT;

   /* decide where to put the text */
   pos1 = XawTextGetInsertionPoint(logwindow);
   pos1 += tt.length;

   /* display it */
   nargs=0;
   XtSetArg( arg[nargs], XtNinsertPosition, pos1); nargs++;
   XtSetValues( logwindow, (ArgList)arg, nargs);
   XawTextReplace(logwindow, pos1, pos1, &tt);

   /* if the log file is open, send message there, too */
   if(logfd != (FILE *)0)
     {
       fprintf(logfd,line);
       fflush(logfd);
     }
}

/***************************************************************************
 * 
 * Main routine. Opens windows and files, sets up callback routines
 *
 **************************************************************************/
void main(int argc, char **argv)
{
  int nargs,i;
  Arg arg[10];
  time_t tp;
  struct tm *curtime;
  static XtCallbackRec callback[2];

  /* open /proc/kmsg. Exit if the open fails */
  kmsg = open("/proc/kmsg",O_RDONLY);
  if(kmsg < 0)
    {
      fprintf(stderr,"Unable to open /proc/kmsg\n");
      exit(1);
    }

  /* check to see if a log file was specified.
   * If specified, try to open it. If the open fails, print a warning and 
   * continue */
  for(i=1;i<argc;i++)
    {
      if(strcmp(argv[i],"-log")==0)
	{
	  if(i<argc-1)
	    {
	      logfd=fopen(argv[i+1],"a");
	      if(logfd==(FILE *)0)
		{
		  fprintf(stderr,"Unable to open log file %s\n",argv[i+1]);
		  exit(1);
		}
	    }
	  else
	    {
	      fprintf(stderr,"Log file name not found in command line\n");
	      exit(1);
	    }
	}
    }
	  
  /* get the system name and kernel release number */
  uname(&name);
  sprintf(line,"%s Console (%s %s)",name.nodename,name.sysname,name.release);

  /* By replacing argv[0] with the system name line, it shows up in the 
   * window's title bar. The must be a better way to do this */
  argv[0]=line;
  

  toplevel = XtAppInitialize( &app_con, "Linux Console", NULL, 0, &argc, argv,
			  NULL,NULL,0);

  /* create a window to hold all the sub-windows */
  compwindow = XtCreateManagedWidget( "Composite", formWidgetClass,
				     toplevel,(ArgList)0,0);

  /* create a window to display the time, and put the current
   * time in it*/
  time(&tp);
  curtime = localtime(&tp);
  strftime(tbuf,49,"%a %b %d %H:%M %Y",curtime);
  nargs=0;
  XtSetArg( arg[nargs], XtNwidth, 180 );nargs++;
  XtSetArg( arg[nargs], XtNlabel, tbuf);nargs++;
  XtSetArg( arg[nargs], XtNvertDistance, 2);nargs++;
  XtSetArg( arg[nargs], XtNhorizDistance, 2);nargs++;
  timewindow = XtCreateManagedWidget( "Composite", labelWidgetClass,
				     compwindow,arg, nargs);

  /* create a quit button */
  nargs=0;
  callback[0].callback = quit_callback;
  callback[0].closure = (caddr_t)toplevel;
  XtSetArg( arg[nargs], XtNcallback, callback );nargs++;
  XtSetArg( arg[nargs], XtNfromHoriz, timewindow );nargs++;
  XtSetArg( arg[nargs], XtNvertDistance, 2);nargs++;
  XtSetArg( arg[nargs], XtNhorizDistance, 2);nargs++;
  quitbutton =
    XtCreateManagedWidget("quit ", commandWidgetClass, compwindow, arg, nargs);

  /* create a window, with a scroll-bar, to display the kernel messages in */
  nargs=0;
  XtSetArg( arg[nargs], XtNeditType, XtEtextEdit );nargs++;
  XtSetArg( arg[nargs], XtNwidth, 495 );nargs++;
  XtSetArg( arg[nargs], XtNheight, 44);nargs++;
  XtSetArg( arg[nargs], XtNscrollVertical,XawtextScrollAlways);nargs++;
  XtSetArg( arg[nargs], XtNfromVert,timewindow);nargs++;
  XtSetArg( arg[nargs], XtNvertDistance, 2);nargs++;
  XtSetArg( arg[nargs], XtNhorizDistance, 2);nargs++;
  logwindow = XtCreateManagedWidget( "Console", asciiTextWidgetClass, 
				     compwindow, arg, nargs);
  
  /* display the whole thing */
  XtRealizeWidget(toplevel);

  /* add a callback for when there is input in /proc/kmsg */
  XtAppAddInput(app_con,kmsg, (XtPointer)XtInputReadMask, 
		handle_kmsg_input, NULL);

  /* add a time-out callback once a minute for the time
   * when the system is heavily loaded, the callbacks can
   * become less frequent. */
  XtAppAddTimeOut(app_con,60000,print_date,NULL);

  XtAppMainLoop(app_con);
}
