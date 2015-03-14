/*
    reversi - play a game of reversi against the computer or human
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


#include <ncurses.h>
#include "reversi.h"
#include "menu.h"

extern int *game_board;

/*
 * Do all curses screen init
 *
 */

init_screen()
{
  initscr();
  cbreak();
  nonl();
  noecho();

  clear();
  refresh();
}

exit_screen()
{
  nl();
  endwin();
}

draw_board( board )
int *board;
{
  int x, y;
  int num_w = 0, num_b = 0;
  char str[80];

  for( x = 0 ; x < 8 ; x++ ){
    for( y = 0 ; y < 8 ; y++ ){
      if( get_board( board, x, y ) == 1 ) num_w++;
      else if( get_board( board, x, y ) == 2 ) num_b++;
    }
  }

  mvaddstr( 17, 52, "           " );
  mvaddstr( 18, 52, "           " );
  sprintf( str, "White: %d", num_w );
  mvaddstr( 17, 52, str );
  sprintf( str, "Black: %d", num_b );
  mvaddstr( 18, 52, str );

  mvaddstr( 20, 52, "ESC for menu" );
  mvaddstr( 21, 52, "By Elias Martenson" );

  for( y = 0 ; y < 8 ; y++ ){
    for( x = 0 ; x < 8 ; x++ ){
      switch( get_board( board, x, y ) ){
      case 0:
	mvaddstr( y * 3, x * 6, " .. " );
	mvaddstr( y * 3 + 1, x * 6, " .. " );
	break;
      case 1:
	mvaddstr( y * 3, x * 6, "<  >" );
	mvaddstr( y * 3 + 1, x * 6, "<  >" );
	break;
      case 2:
	mvaddstr( y * 3, x * 6, "####" );
	mvaddstr( y * 3 + 1, x * 6, "####" );
	break;
      default:
	printf( "Error: I see there is an illegal square on the board!\n" );
	break;
      }
    }
  }

  refresh();
}

get_move( player, x, y )
int player;
int *x, *y;
{
  int x2, y2, end_getmove = FALSE;

  x2 = 3;
  y2 = 3;

  do{
    move( y2 * 3, x2 * 6 + 1 );
    refresh();

    switch( getch() ){
    case 'h':
      if( x2 >= 1 ) x2--;
      break;
    case 'j':
      if( y2 <= 6 ) y2++;
      break;
    case 'k':
      if( y2 >= 1 ) y2--;
      break;
    case 'l':
      if( x2 <= 6 ) x2++;
      break;
    case ' ':
      if( possible_move_square( game_board, x2, y2, player ) ){
	end_getmove = TRUE;
      }
      break;
    case '?':
      do_help();
      draw_board( game_board );
      break;
    case 12:
      clearok( stdscr, TRUE );
      refresh();
      break;
    case 27:
      do_game_menu( player );
      break;
    }
  }while( !end_getmove );

  *x = x2;
  *y = y2;
}

do_help( player )
int player;
{
  WINDOW *win;

  win = newwin( 24, 80, 0, 0 );

  wclear( win );
  touchwin( win );

  mvwaddstr( win, 0, 0, "reversi, V1.24" );
  mvwaddstr( win, 1, 0, "Copyright 1992 Elias Martenson" );
  mvwaddstr( win, 3, 0, "Move the cursor by using the keys h, j, k, l" );
  mvwaddstr( win, 4, 0, "And place your tile on a square with space." );
  mvwaddstr( win, 6, 0, "The object of the game is to have the most number" );
  mvwaddstr( win, 7, 0, "of tiles with your colour when none of the players" );
  mvwaddstr( win, 8, 0, "has a legal move." );
  mvwaddstr( win, 10, 0, "Each tile is white on one side and black on the" );
  mvwaddstr( win, 11, 0, "other. When a tile is placed, you flip the enemys" );
  mvwaddstr( win, 12, 0, "tiles that has become \"trapped\" between the one" );
  mvwaddstr( win, 13, 0, "you just put and another tile of your colour." );
  mvwaddstr( win, 23, 0, "Press a key to resume play." );

  wrefresh( win );

  getch();

  delwin( win );
  touchwin(stdscr);
  refresh();
}

do_game_menu( player )
int player;
{
  int selected_menu;
  MenuEntry game_menu[] = {
    "About", NULL, SELECTABLE,
    "Setup", NULL, SELECTABLE,
    "Undo last move", NULL, SELECTABLE,
    "Show possible moves", NULL, SELECTABLE,
    "Save game", NULL, SELECTABLE,
    "Quit game", NULL, SELECTABLE,
    "Resume game", NULL, SELECTABLE,
    NULL, NULL, 0
  };

  do{
    selected_menu = menu( 52, 9, 0, 0, 0, WRAP | ENABLE_QUIT | CLEAR_ON_EXIT, game_menu );
    
    switch( selected_menu ){
    case 0:
      show_about();
      break;
    case 1:
      /*    do_setup();*/
      break;
    case 2:
      do_undo_move();
      break;
    case 3:
      do_show_possible( player );
      break;
    case 4:
      save_game( player );
      break;
    case 5:
      exit_rout( 0 );
      break;
    }

  }while( selected_menu != -1 && selected_menu != 5 );
}

show_about()
{
  WINDOW *win;

  win = newwin( 24, 80, 0, 0 );

  wclear( win );
  touchwin( win );

  mvwaddstr( win, 0, 0, "reversi, V1.24" );
  mvwaddstr( win, 1, 0, "Copyright 1992 Elias Martenson (elias@proxxi.se)" );
  mvwaddstr( win, 3, 0, "Menu routines by Stefan Rapp (rappen@proxxi.se)" );
  mvwaddstr( win, 5, 0, "Beta testers:" );
  mvwaddstr( win, 6, 0, "Gustav Berggren (gustav@proxxi.se)" );
  mvwaddstr( win, 7, 0, "Anders Bollmark (bollen@proxxi.se)" );
  mvwaddstr( win, 8, 0, "Martin Green (bullen@proxxi.se)" );
  mvwaddstr( win, 9, 0, "Magnus Krafft (krafft@proxxi.se)" );
  mvwaddstr( win, 10, 0, "Stefan Rapp (rappen@proxxi.se)" );
  mvwaddstr( win, 11, 0, "Carl Serrander (calle@proxxi.se)" );
  mvwaddstr( win, 23, 0, "Press any key to resume play." );

  wrefresh( win );

  getch();

  delwin( win );
  touchwin(stdscr);
  refresh();
}

do_show_possible( player )
int player;
{
  int count_x, count_y;

  for( count_x = 0 ; count_x < 8 ; count_x++ ){
    for( count_y = 0 ; count_y < 8 ; count_y++ ){
      if( possible_move_square( game_board, count_x, count_y, player ) ){
	standout();
	mvaddstr( count_y * 3, count_x * 6, "    " );
	mvaddstr( count_y * 3 + 1, count_x * 6, "    " );
	standend();
      }
    }
  }

  refresh();
  getch();

  draw_board( game_board );
}
