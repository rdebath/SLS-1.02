/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

/* debugging routines for printing status info */

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "bitblit.h"

#include "defs.h"

static char *flags[] = {
   "active",
   "escape",
   "snarfable",
   "reverse",
   "standout",
   "died",
   "expose",
   "background",
   "nokill",
   "vi",
   "downloading",
   "noinput",
   "nowrap",
   "overstrike",
   "abs-coords",
   "minus",
   "snarf-lines",
   "snarf-tabs",
   "snarf-hard",
   (char *) 0
   };

static char *events[] = {
   "1_up",
   "no-event",
   "2_up",
   "3_up",
   "no-event",
   "3_down",
   "2_down",
   "no-event",
   "1_down",
   "shape",
   "redraw",
   "activated",
   "deactivated",
   "covered",
   "uncovered",
   "moved",
   "destroyed",
   "accept",
   "notify",
   "tell-me",
   "snarfed",
   "paste",
   "stack",
   "stack_flag",
   (char *) 0
   };

static char *stack[] = {
   "menu",
   "event",
   "font",
   "cursor",
   "bitmap",
   "position",
   "window",
   "flags",
   "mouse",
   "text",
   "CLEAR",
   (char *) 0
   };

static char *menu_states[] = {
    "choice",
    "bottom",
    "top",
    "top & bottom",
    "left",
    "left & bottom",
    "left & top",
    "left & top & bottom",
    "right",
    "right & bottom",
    "right & top",
    "right & top & bottom",
    "right & left",
    "right & left & bottom",
    "right & left & top",
    "right & left & top & bottom",
   };

static char buff[512];

/* get flag names */

char *print_flags(n)
int n;
   {
   char *binary();
   register int i,j;

   sprintf(buff,"(%s)",binary(n));
   for(j=1,i=0;flags[i];i++,j<<=1)
      if (j&n) {
         if (*buff) strcat(buff,",");
         strcat(buff,flags[i]);
         }
   return(buff);
   }

/* get menu states */

char
*print_menu(n)
int n;		/* menu state */
   {
   return(menu_states[n%16]);
   }

/* get stack codes */

char *print_stack(n)
int n;
   {
   register int i,j;
   char *binary();

   sprintf(buff,"(%s)",binary(n));
   for(j=1,i=0;stack[i];i++,j<<=1)
      if (j&n) {
         if (*buff) strcat(buff,",");
         strcat(buff,stack[i]);
         }
   return(buff);
   }

/* get event names */

char *print_events(n)
int n;
   {
   register int i,j;
   char *binary();

   sprintf(buff,"(%s)",binary(n));
   for(j=1,i=0;events[i];i++,j<<=1)
      if (j&n) {
         if (*buff) strcat(buff,",");
         strcat(buff,events[i]);
         }
   return(buff);
   }


/* get ps info */

static char *list[100];
static int p_count = 0;

int
get_ps()
   {
   FILE *ps = popen("ps a","r");
   char line[81];

   fgets(line,sizeof(line),ps);
   while(fgets(line,sizeof(line),ps) != NULL) {
      list[p_count++] = strcpy(malloc(strlen(line+6)+1),line+6);
      }
   pclose(ps);
   return(p_count);
   }

int
free_ps()
   {
   register int i;

   for(i=0;i<p_count;i++) {
      if (list[i])
         free(list[i]);
      }
   p_count = 0;
   }

char *
print_ps(tty)
char *tty;
   {
   register int i;
   register char *check = tty + (strlen(tty) - 2);
   register char *p1,*p2;
   char *index(), *strcpy();

   *buff = '\0';
   for(i=0;i<p_count;i++)
      if (list[i] && strncmp(check,list[i],2)==0 &&
                     (int)(p1 = index(list[i],':')+4)>4) {
         if (p2 = index(p1,'\n'))
            *p2 = '\0';
         strcpy(buff,p1);
         if (*p1 != '-')
            break;
         }
   return(buff ? buff : strcpy(buff,"?? unknown ??"));
   } 

/*****************************************************************************
 *	print x in binary
 */

char *
binary(x)
int x;
   {
   register int i;
   static char out[33];
   int n=32;

   out[n+1] = '\0';
   for(i=0;i<n+1;i++)
      if (x&(1<<i))
         out[n-i] = '1';
      else
         out[n-i] = '0';
   for(i=0;out[i]=='0' && i<33;i++);
   return(i>0?out+i-1:out);
   }
