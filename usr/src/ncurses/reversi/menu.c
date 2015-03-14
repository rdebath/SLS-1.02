/*
    reversi - play a game of reversi against the computer or a human
    Copyright (C) 1992  Elias Martenson

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

    Contact me by email at elias@proxxi.se
*/

/*
 *  These menu routines is written by Stefan Rapp (rappen@proxxi.se)
 *
 */

#include <ncurses.h>
#include "menu.h"

/*                      */
/*  Start: 0 = First    */
/*                      */

menu (x, y, width, height, start, flags, names)
int x, y, width, height, start, flags;
MenuEntry *names;
{
  MenuEntry *namesstart = names;
  WINDOW *menu_window;
  int max_width=0, number_names= 0, counter, names_select = FALSE;
  char ch;
  char *str;

  do{
    if( strlen( names->name ) > max_width  ){ 
      max_width = strlen(names->name);
    }
    number_names++;
  } while((++names) -> name != NULL );

  if( height == 0){
    height = number_names;
  }

  if( width == 0){
    width = max_width;
  }
  
  names=namesstart;
  names+=start;

  counter = 0;
  
  menu_window = newwin( height, width + 1, y, x );
  if( ( flags & DRAW_BOX ) == DRAW_BOX ){
    box( menu_window, DRAW_BOX_SYMBOL_VERT, DRAW_BOX_SYMBOL_HORIZ );
 }
  wclear( menu_window );
  refresh_names( menu_window, 1, width, height, names+1);
  wstandout( menu_window );
  mvwaddstr(menu_window, 0, 0, (names)->name);
  wstandend( menu_window );
 
  do{
    wrefresh(menu_window);
    ch = wgetch( menu_window );

    wstandend( menu_window );
    mvwaddstr( menu_window, counter, 0, names-> name);
        
    switch( ch ){
    case MV_UP:
      if(names-namesstart > 0){
	names--;
	counter--;
        if( counter < 0 ){	
	  counter = 0;
	  refresh_names( menu_window, 0, width, height, names);
        }
      }
      else if( (flags & WRAP) == WRAP ){
	names = namesstart;
	names += number_names -1;
        counter = height -1;
	refresh_names( menu_window, 0, width, height, names - (height -1));
      }
      break;
    case MV_DOWN:
      if( (names+1)->name != NULL){
	names++;
	counter++;
        if( counter >= height ){
	  counter = height -1;
	  refresh_names( menu_window, 0, width, height, names - (height -1));
	}
      }
      else if(( flags & WRAP) == WRAP ){
	names = namesstart;
	counter = 0;
	refresh_names( menu_window, 0, width, height, names);
      }
      break;
    case PG_UP:
      break;
    case PG_DOWN:
      break;
    case SELECT:
      names_select = TRUE;
      break;
    case QUIT:
      if( (flags & ENABLE_QUIT) == ENABLE_QUIT){
	names_select = -1;
      }
      break;
    case HELP:
      if( (flags & ENABLE_HELP) == ENABLE_HELP){
	/* help command */
      }
      break;
    } /* switch */
    
    wstandout( menu_window);
    mvwaddstr( menu_window, counter, 0, (names -> name ));
  } while( names_select == FALSE);
  
  if( (flags & CLEAR_ON_EXIT) == CLEAR_ON_EXIT ){
    wstandend( menu_window );
    wclear( menu_window );
    wrefresh( menu_window );
  }
  delwin( menu_window );
  
  if( names_select == -1){
    return(-1);
  }
  else{
    return(names - namesstart);
  }
}   
	   

refresh_names( menu_window, y, width, height,  names)
WINDOW *menu_window;
MenuEntry *names;
int width, height, y;
{
  int counter;
  
  for( counter = y; ( counter < height  ) && ( names -> name != NULL ); counter++ ){
    mvwaddstr( menu_window, counter , 0 , (names) -> name );
    if( strlen( names -> name) < width){
      write_space( menu_window, width - strlen(names->name));
      write_space( menu_window, 1);
    }
    names++;
  }
}

write_space( menu_window, spaces)
WINDOW *menu_window;
int spaces;
{
  int counter;
  
  for( counter = 0; counter < spaces; counter++ ){
    waddch( menu_window, ' ' );
  }
}
