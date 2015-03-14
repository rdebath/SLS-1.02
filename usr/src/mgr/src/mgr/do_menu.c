/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

/* high level menu manipulation routines */

/*{{{}}}*/
/*{{{  #includes*/
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>

#include "bitblit.h"
#include "font.h"

#include "defs.h"
#include "menu.h"

#include "Write.h"
#include "do_button.h"
#include "font_subs.h"
#include "get_menus.h"
/*}}}  */

/* do a tree of menus */

static int x_page = 5;		/* offset for paging menu */
static int y_page = -5;
static int x_slide = 40;	/* offset for scrolling menu */
static int y_slide = 10;


/*{{{  add_result -- add a value to list of menu values*/
static struct menu_result *
add_result(state,list)
struct menu_state *state;		/* menu to add choice to */
struct menu_result *list;		/* current list of results */
   {
   register struct menu_result *current;	/* current result */

   /* set up list */

   if (list == (struct menu_result *) 0) {
      list = (struct menu_result *) malloc(sizeof(struct menu_result));
      list->next = NULL;
      list->value = NULL;
      }
   else if (list->value == NULL)
      return(list);

   /* add entry to existing list */

   if (menu_value(state) && *menu_value(state) && 
                      (current = (struct menu_result *) 
                       malloc(sizeof(struct menu_result)))) {
      current->value = menu_value(state);
      current->next = list;
      }
   else
      current = list;
   return(current);
   }
/*}}}  */
/*{{{  get_fields -- break a line into its component fields*/
static int
get_fields(line,delim,fields,max)
char *line;				/* line to break into fields */
char **fields;				/* resultant fields */
char delim;				/* field delimeter */
int max;				/* max # fields */
   {
   register char c, *start;
   register int count;
 
   for(count=0,start=line; count<max && (c = *line); line++)
      if (c == delim) {
         fields[count++]=start;
         *line = '\0';
         start=line+1;
         }
   if (start<line)
       fields[count++] = start;
   fields[count]=(char *) 0;
   return(count);
   }
/*}}}  */

/*{{{  set_slide -- set slideing defaults*/
int set_slide(x,y) int x,y;
   {
   if (x || y) {
      x_slide = x;
      y_slide = y;
      }
   return(0);
   }
/*}}}  */
/*{{{  set_page -- set paging defaults*/
int set_page(x,y)
int x,y;
   {
   if (x || y) {
      x_page = x;
      y_page = y;
      }
   return(0);
   }
/*}}}  */
/*{{{  do_menu -- define a menu from menu download string*/
struct menu_state *
do_menu(line,font,color)
char *line;
struct font *font;
int color;		/* raster op containing colors */
   {
   register int count;
   char *fields[MAXITEMS];

   count = get_fields(line+1,*line,fields,MAXITEMS)/2;

#ifdef DEBUG
   dprintf(m)(stderr,"Setting up a menu, %d items\n",count);
#endif
   if (count < 1)
      return((struct menu_state *) 0);

   return(menu_define(font,fields,fields+count,count,color));
   }
/*}}}  */
/*{{{  do_menus*/
struct menu_result *
do_menus(screen,mouse,x,y,font,menu_list,menu,exit_code)
BITMAP *screen;			/* bitmap screen */
int mouse;			/* fd to get mouse coordinates */
int x,y;			/* where the menu goes on the screen */
struct font *font;		/* font to use for menus */
struct menu_state *menu_list[];	/* list of available menus */
int menu;			/* current menu number */
int exit_code;			/* valid exit codes */
   {
   struct menu_state  *state;	/* 'cookie' for menu system */
   struct menu_result *result;	/* messages for nodes of menu tree chosen */
   int done=0;			/* true if ok to backup a level */
   int first = 1;		/* true upon entry */
   int next;			/* next menu # */
   struct menu_result *add_result();
   char *print_menu();

   /* set up menu, get menu 'cookie' */

   state = menu_list[menu];	/* fetch the menu state */
   result = (struct menu_result *) 0;
   state = menu_setup(state,screen,x,y,menu_choice(state));

   if (state == (struct menu_state *) 0) {
       perror("Error setting up menu");
       return(NULL);
       }

   /* see if another page */

   if (state->next >= 0)
      exit_code |= EXIT_BOTTOM;
   else
      exit_code &= ~EXIT_BOTTOM;

#ifdef DEBUG
   dprintf(m)(stderr,"  Setting up menu %d at %d,%d: valid states %s\n",
                      menu,x,y,print_menu(exit_code));
#endif

   /* get selection on current menu */

   while (!done)
     {

     /* get menu state from user */
#ifdef DEBUG
   dprintf(m)(stderr,"  from user ..."); fflush(stderr);
#endif

     /* do auto right menus */

     if (state->flags&MENU_PAGE && exit_code&EXIT_BOTTOM && first &&
                     state->current >= state->count ) {
        first=0;
        state->exit = EXIT_BOTTOM;
        }
     else if (state->flags&MENU_AUTO && exit_code&EXIT_RIGHT && first) {
        first=0;
        state->exit = EXIT_RIGHT;
        }
     else
        menu_get(state,mouse,0,exit_code);

     /* execute appropriate state action */

#ifdef DEBUG
   dprintf(m)(stderr,"got menu %d (at %d,%d) selection %d (%s)\n",
              menu,x,y,menu_choice(state),print_menu(menu_exit(state)));
#endif

     switch (menu_exit(state)) {
        case EXIT_LEFT:		/* slid off to the left */
        case EXIT_TOP:		/* slid of the top */
             result = NULL;
             done++;
             break;
        case EXIT_CHOICE:	/* add current choice onto list */
             result = add_result(state,result);
             done++;
             break;
        case EXIT_RIGHT:	/* slid off top the right */
             if ((next = menu_next(state)) >=0 &&	/* link exists */
                     menu_list[next] &&			/* menu exists */
                     menu_list[next]->save == (BITMAP *) 0 &&	/* not used */
                     (result=do_menus(screen,mouse,		/* choice */
                                      x+x_slide,y-y_slide,font,menu_list,
                                      next,exit_code|EXIT_LEFT))) {
                done++;
                }
             break;
        case EXIT_BOTTOM:
             if ((next = menu_next(state)) >=0 &&	/* menu exists */
                 menu_list[next]->save == (BITMAP *) 0 &&	/* not used */
                     (result=do_menus(screen,mouse,		/* choice */
                                      x+x_page,y+y_page,font,menu_list,
                                      next,exit_code|EXIT_TOP))) {
                done++;
                }
             break;
        default:
	     if( !debug )
		break;
             fprintf(stderr,"invalid menu state: 0%o\n",menu_exit(state));
             result = NULL;
             done++;
             break;
        }
     }
  
#ifdef DEBUG
   dprintf(m)(stderr,"  Tearing down %d at %d,%d choice: %d, returning: %s\n",
              menu,x,y,menu_choice(state),print_menu(menu_exit(state)));
#endif

   /* add our action onto action list */

   if (menu_exit(state) == EXIT_RIGHT && result && !(state->flags&MENU_SNIP)) {
      result = add_result(state,result);
      }

   /* erase menu from the screen */

   menu_remove(state);
   return(result);
   }
/*}}}  */
/*{{{  go_menu -- do a tree of menus*/
void go_menu(n)
int n;					/* which menu button (0 or 1) */
   {
   struct menu_result *result = NULL;		/* result of menu selection */
   register struct menu_result *current;	/* current action */
   int exit = EXIT_RIGHT;			/* enable sliding and paging */
   register int menu = ACTIVE(menu[n]);

#ifdef DEBUG
   dprintf(m)(stderr,"Starting menu %d, button %d\n",menu,n);
#endif

   /* go get a menu selection, return list of actions */

   if (menu>=0 /* && mousein(mousex,mousey,active,0) */) {
      result = do_menus(screen,mouse,mousex,mousey,
               font,ACTIVE(menus),menu,exit);

      /* send list of actions, and free action space */

      for(current=result;current;) {
         if (current->value)
            Write(ACTIVE(to_fd),current->value,strlen(current->value));
         result = current;
         current = current->next;
         free(result);
         }
        
      /* button is no longer pushed down; record that fact */
      do_button( 0 );
      }
   return;
   }
/*}}}  */
