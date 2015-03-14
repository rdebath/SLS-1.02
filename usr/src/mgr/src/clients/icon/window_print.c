/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

/* print any window on the hp think jet (use lpr filter) */

#include <stdio.h>
#include <signal.h>
#include <sys/file.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "term.h"
#include "bitmap.h"

#define TEMP	"/tmp/pr."			/* temp file name */
#define MESSAGE	"\rwindow dump"			/* window message */
#define WORKING	"\rworking..................."	/* print in progress */
#define JOB	"window"			/* default page header */
#define OPTION	'v'				/* lpr filter option */

#define MSG_OK		"ok"			/* message ack - success */
#define MSG_BAD		"x"			/* message nac - failure */
#define MSG_FILE	"F"			/* service type - no data */
#define NAME		"print bitmap"		/* name of service */

#define dprintf	if(debug) fprintf

#define GET_OPT(i)	\
	strlen(argv[i])>2 ? argv[i]+2 : argv[++i]
#define Min(x)		((x)<5 ? 5 : (x))

char command[100];				/* lpr command to run */
char line[100];					/* event input buffer */
char temp[20];					/* temp file name */
char my_host[20];				/* my host */
char mgr_host[20];				/* mgr host */
char title[40];					/* window title */

int remote;					/* remote from mgr host */
int debug;

void clean();

void
main(argc,argv)
int argc;
char **argv;
   {
   register int i, c;
   int x,y;					/* bitmap size */
   int id,sub;					/* window to dump */

   char *printer = NULL;			/* printer name */
   char *job = JOB;				/* job name */
   char *filter = NULL;				/* pre-filter */
   char *message = NULL;			/* message line */
   char option = OPTION;			/* lpr filter */
   char *noprint = NULL;			/* just same file */

   ckmgrterm( *argv );
   gethostname(my_host,sizeof(my_host));
   if (getenv("DEBUG")) debug = 1;

   /* check arguments */

   for(i=1;i<argc;i++) {
      if (*argv[i] == '-')
         switch (argv[i][1]) {
            case 'f':				/* specify filter */
               filter = GET_OPT(i);
               break;
            case 'p':				/* printer name */
            case 'P':				/* printer name */
               printer = GET_OPT(i);
               break;
            case 'm':				/* message line */
               message = GET_OPT(i);
               break; 
            case 'j':				/* job name */
            case 'J':				/* job name */
               job = GET_OPT(i);
               break; 
            case 'o':				/* option flag */
               option = *(GET_OPT(i));
               break; 
            case 'x':				/* dont print name */
               noprint = GET_OPT(i);
               break; 
            default:
               fprintf(stderr,"%s: invalid flag %c ignored\n",argv[0],argv[i][1]);
            }
      else
         fprintf(stderr,"%s: invalid argument %s ignored\n",argv[0],argv[i]);
      }

   /* setup mgr */

   m_setup(M_FLUSH);
   m_push(P_FLAGS|P_EVENT);
   m_ttyset();

   get_param(mgr_host,0,0,0);
   remote = strcmp(mgr_host,my_host);
   dprintf(stderr,"my host (%s), mgr host (%s)\n",my_host,mgr_host);

   if (message)
      sprintf(title,"\r%s",message);
   else if (printer && remote)
      sprintf(title,"%s at %s on %s",MESSAGE,my_host,printer);
   else if (remote)
      sprintf(title,"%s at %s",MESSAGE,my_host);
   else if (printer)
      sprintf(title,"%s on %s",MESSAGE,printer);
   else
      strcpy(title,MESSAGE);

   get_size(&x,&y,0,0);
   if (!debug)
      m_sizeall(x,y,Min(strlen(title)-1),1);
   m_setmode(M_NOWRAP);
   m_clear();
   m_printstr(title);
   m_clearmode(M_ACTIVATE);
   
   m_setevent(ACTIVATED,"A\n");			/* window made active */
   m_setevent(DEACTIVATED,"D\n");		/* window made in-avtive */
   m_setevent(BUTTON_1,"B %w\n");		/* button 1 hit */
   m_setevent(REDRAW,"R\n");
   m_setevent(RESHAPE,"S\n");
   
   signal(SIGHUP,clean);
   signal(SIGINT,clean);
   signal(SIGTERM,clean);

   /* build command */

   sprintf(temp,"%s%s%d",TEMP,my_host,getpid());
   dprintf(stderr,"temp file name: (%s)\n",temp);
   
   if (noprint) {
      if (remote)
         sprintf(command,"rcp %s:%s %s",mgr_host,temp,noprint);
      else
         sprintf(command,"cp %s %s",temp,noprint);
      }
   else {
      if (remote)
         sprintf(command,"rsh %s cat %s | ",mgr_host,temp);
      else
         sprintf(command,"< %s ",temp);
      if (filter) 
         sprintf(command+strlen(command),"%s | ",filter);
      strcat(command,"lpr ");
      if (printer)
         sprintf(command+strlen(command),"-P%s ",printer);
      sprintf(command+strlen(command),"-J%s -%c",job,option);
      }
   dprintf(stderr,"command: (%s)\n",command);
   
   while (m_gets(line) != NULL) {
     dprintf(stderr,"main loop got: %s",line);
     switch(c = *line) {
        case 'S':				/* window reshaped */
           get_size(&x,&y,0,0);
           if (!debug)
              m_sizeall(x,y,strlen(title)-1,1);
           m_clear();
           m_printstr(title);
           break;
        case 'R':				/* window redrawn */
           m_printstr(title);
           break;
        case 'A':				/* window activated */
           m_setmode(M_WOB);
           break;
        case 'D':				/* window deactivated */
           m_clearmode(M_WOB);
           break;
        case 'B':				/* button hit */
           id = 0;
           sscanf(line+2,"%d.%d",&id,&sub);
           dprintf(stderr," got: %d, %d\n",id,sub);
           if (id) {
              m_othersave(id,sub,temp);
              m_clearmode(M_WOB);
              m_printstr(WORKING);
              sleep(1); 
	      strcat(command, temp);
              system(command);
              if (remote) {
                 sprintf(command,"rsh %s rm %s\n",mgr_host,temp);
                 system(command);
	      }
              else
                 unlink(temp);
           }
           m_printstr(title);
           m_clearmode(M_ACTIVATE);
           break;  
        }
   }
}

void
clean(n)
int n;
   {
   m_ttyreset();
   if (remote) {
      sprintf(command,"rsh %s rm %s\n",mgr_host,temp);
      system(command);
   }
   else
      unlink(temp);
   m_pop();
   m_clear();
   exit(n);
}
