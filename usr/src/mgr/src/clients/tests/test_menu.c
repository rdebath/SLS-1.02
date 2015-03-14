/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

/* test out menus */

#include <signal.h>
#include <stdlib.h>
#include <stdio.h>

#include "term.h"

#define MAX		3			/* # of menus */

char foo[25];

struct menu_entry menu1[] = {
	"menu 1","1",
	foo,"g",
	"cat","c",
	"mouse","m",
	"elephant","e",
	};

struct menu_entry menu2[] = {
	"menu 2","1",
	foo,"g",
	"slate","s",
	"sand","m",
	"quartz","q",
	};

struct menu_entry menu3[] = {
	"menu 3","3",
	foo,"g",
	"carrot","c",
	"egg plant","e",
	"string beans","q",
	};

struct menus {
   struct menu_entry *menu;
   int count;
   };

struct menus menus[] = {
   menu1, 5,
   menu2, 5,
   menu3, 5,
   (struct menu_entry *) 0, 0
   };

/* clean up and exit */

void clean(n)
int n;
   {
   m_pop();
   m_ttyreset();
   exit(n);
   }

int main(argc,argv)
int argc;
char **argv;
   {
   int c, n, x, y;

   ckmgrterm(*argv);

   signal(SIGINT,clean);
   signal(SIGTERM,clean);

   m_setup(M_FLUSH);
   m_ttyset();
   m_push(P_MENU|P_EVENT);

   m_nomenu();
   m_setevent(BUTTON_2,"[%p]");
   m_setevent(BUTTON_2U,"$");
   m_setraw();

   fprintf(stderr,"Use the middle button to activate a menu\r\n");
   while ((c=getc(m_termin)) != 'q') {
     switch(c) {
        case '[':				/* button down */
           fscanf(m_termin,"%d %d]",&x,&y);
           n=x*MAX/1000+1;
           fprintf(stderr,"got %d %d selecting %d ",x,y,n);
           sprintf(foo,"at %d,%d",x,y);
           menu_load(n,menus[n-1].count,menus[n-1].menu);
           m_selectmenu(n);
           break;
        case '$':				/* button up */
           fprintf(stderr,"done\r\n");
           m_nomenu();
           break;
        default:				/* menu selection */
           fprintf(stderr,"got %c ",c);
           break;
        }
     }
     /* The button has to be released before we quit, otherwise stdin gets messy */
     getc(m_termin);
     fprintf(stderr,"done\r\n");
     m_nomenu();
     clean(0);
     return(255);
   }
