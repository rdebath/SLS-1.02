/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

/* LRU routines for font management */

/*{{{}}}*/
/*{{{  #includes*/
#include <errno.h>
#include <limits.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "bitblit.h"
#include "font.h"

#include "defs.h"

#include "font_subs.h"
/*}}}  */

/*{{{  types*/
static struct list {
   struct list *next, *prev;		/* next or previous list item */
   struct font *font;			/* pointer to font structure */
   char *name;				/* font name */
   };
/*}}}  */
/*{{{  variables*/
struct list *list_top = (struct list*)0;
/*}}}  */

/*{{{  insert_font -- insert an element at the top of the list */
static void insert_font(ptr) register struct list *ptr;
   {
   ptr->next = list_top;
   ptr->prev = (struct list*)0;
   if (ptr->next != (struct list*)0)
      ptr->next->prev = ptr;
   list_top = ptr;
   }
/*}}}  */
/*{{{  unlink_font -- unlink an element from the list*/
static void unlink_font(ptr) register struct list *ptr;
   {
   if (ptr->next != (struct list*)0)
      ptr->next->prev = ptr->prev;
   if (ptr->prev != (struct list*)0)
      ptr->prev->next = ptr->next;
   }
/*}}}  */
/*{{{  create_font -- get font, name for an element*/
static void create_font(name,ptr)
char *name;
register struct list *ptr;
   {
   char *save_line();

   if (ptr->name != NULL)
      free(ptr->name);
   if (ptr->font != (struct font *) 0) 
      free_font(ptr->font);
   ptr->name = strcpy(malloc(strlen(name)+1),name);
   ptr->font = open_font(name);
   }
/*}}}  */

/*{{{  get_font -- manage the list of fonts (using LRU)*/
struct font *
get_font(name)
char *name;
   {
   static count = 0;
   register struct list *ptr;
   register int found=0;

   if (name == (char *) 0 || *name == '\0')
      return((struct font *) 0);

   if (count>0 && strcmp(name,list_top->name)==0) {
      return(list_top->font);
      }
   
   for(ptr=list_top;count>0;ptr=ptr->next) {
      if (found=(ptr->name && strcmp(name,ptr->name)==0)) {
         unlink_font(ptr);
         insert_font(ptr);
         break;
         }
      if (ptr->next == (struct list*)0)
         break;
      }
   if (!found && count<MAXFONT) {
      ptr = malloc(sizeof(struct list));
      ptr->name = NULL;
      ptr->font = (struct font *) NULL;
      create_font(name,ptr);
      insert_font(ptr);
      count++; 
      }
   else if (!found) {
      unlink_font(ptr);
      create_font(name,ptr);
      insert_font(ptr);
      }
   return(list_top->font);
   }
/*}}}  */
/*{{{  Get_font -- Get a font from font numbers.*/
/* Font numbers run 0 through MAXFONT; font 0 is the default font. */
struct font *
Get_font(fnt)
int fnt;				/* font number */
   {
   struct font *new;
   char buff[MAX_PATH];
   char *name;

   if (fnt<=0 || fnt>MAXFONT || fontlist[fnt-1] == (char *) 0)
      return(font);

   if (*fontlist[fnt-1] == '/') 
      name = fontlist[fnt-1];
   else {
      sprintf(buff, "%s/%s", font_dir, fontlist[fnt-1]);
      name = buff;
      }
   if ((new = get_font(name))==(struct font *)0)
      new = font;
   else
      new->ident = fnt;
   return(new);
   }
/*}}}  */
