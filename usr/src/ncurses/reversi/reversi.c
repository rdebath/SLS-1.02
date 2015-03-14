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

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <ncurses.h>
#include <string.h>
#include "reversi.h"
#include "menu.h"


int game_board2[64];
int *game_board;
int *old_gameboard;
int game_lev1, game_lev2;
int old_pointer;
int to_load = 0;
int p1, p2;

#ifdef HAVE_VOID_SIGNALS
void exit_rout();
#else
int exit_rout();
#endif

main( argc, argv )
int argc;
char **argv;
{
  int w_b;
  char str[20];
  char c;
  int starting_player;
  char tmp_str[80];
  FILE *fp;
  int curr_scr;
  int x, y;
  int t;
  MenuEntry game_setup[] = {
    "Standard", NULL, SELECTABLE,
    "Inverse ", NULL, SELECTABLE,
    NULL, NULL, 0
  };
  MenuEntry select_player[] = {
    "Human   ", NULL, SELECTABLE,
    "Computer", NULL, SELECTABLE,
    NULL, NULL, 0
  };
  MenuEntry level_menu[] = {
    "Apprentice", NULL, SELECTABLE,
    "Amateur   ", NULL, SELECTABLE,
    "Regular   ", NULL, SELECTABLE,
    "Advanced  ", NULL, SELECTABLE,
    "Hard      ", NULL, SELECTABLE,
    "Very Hard ", NULL, SELECTABLE,
    NULL, NULL, 0 
  };

  old_pointer = 0;
  starting_player = 0;

  if( argc == 2 ){
    if( strcmp( argv[1], "-r" ) == 0 ){
      to_load = TRUE;
    }
    else{
      usage();
    }
  }
  else if( argc > 2 ){
    usage();
  }

  init_screen();
  
  signal( SIGINT, exit_rout );

  game_board = (int *)game_board2;
  if( (old_gameboard = (int *)malloc( sizeof(int) * 64 * 60 )) == NULL ){
    fprintf( stderr, "Could not allocate memory.\n" );
    endwin();
    exit( 1 );
  }

  addstr( "reversi version 1.24, Copyright (C) 1992 Elias Martenson\n" );
  addstr( "reversi comes with ABSOLUTELY NO WARRANTY.\n" );
  addstr( "This is free software, and you are welcome to redistribute it\n" );
  addstr( "under certain conditions; see the file COPYING for details.\n\n" );

  refresh();

  if( !to_load ){
    
    mvaddstr( 8, 0, "Select game setup" );
    refresh();
    w_b = menu( 0, 10, 18, 2, 0, WRAP, game_setup );
    
    mvaddstr( 8, 20, "White played by?" );
    refresh();
    p1 = menu( 20, 10, 18, 2, 0, WRAP, select_player );
    if( p1 == 1 ){
      mvaddstr( 14, 20, "Enter level:" );
      refresh();
      game_lev1 = menu( 20, 16, 18, 6, 0, WRAP, level_menu );
    }
    
    mvaddstr( 8, 40, "Black played by?" );
    refresh();
    p2 = menu( 40, 10, 18, 2, 0, WRAP, select_player );
    if( p2 == 1 ){
      mvaddstr( 14, 40, "Enter level:" );
      refresh();
      game_lev2 = menu( 40, 16, 18, 6, 0, WRAP, level_menu );
    }
    
    clear();
    refresh();
    init_board( w_b );

  }
  else{
    strcpy( tmp_str, getenv( "HOME" ) );
    strcat( tmp_str, "/.reversi_save" );
    if( (fp = fopen( tmp_str, "r" )) == NULL ){
      fprintf( stderr, "Could not open save file\n" );
      exit_rout( 0 );
    }
    fread( &starting_player, sizeof( int ), 1, fp );
    fread( &p1, sizeof( int ), 1, fp );
    fread( &p2, sizeof( int ), 1, fp );
    fread( &game_lev1, sizeof( int ), 1, fp );
    fread( &game_lev2, sizeof( int ), 1, fp );

    curr_scr = 0;
    do{
      for( x = 0 ; x < 8 ; x++ ){
	for( y = 0 ; y < 8 ; y++ ){
	  fread( &t, sizeof( int ), 1, fp );
	  get_board( old_gameboard + curr_scr, x, y ) = t;
	}
      }
      curr_scr += 64;
    }while( !feof( fp ) );
    fclose( fp );
    curr_scr -= 64;
    old_pointer = curr_scr;
    for( x = 0 ; x < 8 ; x++ ){
      for( y = 0 ; y < 8 ; y++ ){
	get_board( game_board, x, y ) = get_board( old_gameboard + curr_scr - 64, x, y );
      }
    }
  }
  clear();

  draw_board( game_board );
  play( p1, p2, starting_player );
  exit_rout( 0 );
}

usage()
{
  fprintf( stderr, "Usage: reversi [-r]\n" );
  exit( 1 );
}

/*
 * Init game board
 *
 */

init_board( w_b )
int w_b;
{
  int x, y;
  
  for( x = 0 ; x < 8 ; x++ ){
    for( y = 0 ; y < 8 ; y++ ){
      get_board( game_board, x, y ) = 0;
    }
  }

  if( w_b == 0 ){
    get_board( game_board, 3, 3 ) = 1;
    get_board( game_board, 4, 3 ) = 2;
    get_board( game_board, 3, 4 ) = 2;
    get_board( game_board, 4, 4 ) = 1;
  }
  else{
    get_board( game_board, 3, 3 ) = 2;
    get_board( game_board, 4, 3 ) = 1;
    get_board( game_board, 3, 4 ) = 1;
    get_board( game_board, 4, 4 ) = 2;
  }
}

#ifdef HAVE_VOID_SIGNALS
void exit_rout( sig )
#else
int exit_rout( sig )
#endif
int sig;
{
  move( 23, 0 );
  refresh();
  exit_screen();
  exit(0);
}

play( p1, p2, starting_player )
int p1, p2, starting_player;
{
  int x, y, x2, y2;
  int c_w, c_w2, cur_player, end_w = FALSE;
  int p[2], game_l[2];
  int a;
  char str[200];
  char player_str[2][40];
  int count_x, count_y;
  
  c_w = 0;
  c_w2 = 0;
  
  p[0] = p1;
  p[1] = p2;
  game_l[0] = game_lev1;
  game_l[1] = game_lev2;

  if( p[0] == 0 ){
    strcpy( player_str[0], "Human plays white" );
  }
  else{
    sprintf( player_str[0], "Computer level %d plays white", game_l[0] );
  }

  if( p[1] == 0 ){
    strcpy( player_str[1], "Human plays black" );
  }
  else{
    sprintf( player_str[1], "Computer level %d plays black", game_l[1] );
  }

  mvaddstr( 2, 52, player_str[0] );
  mvaddstr( 3, 52, player_str[1] );
  refresh();

  if( !to_load ){
    for( count_x = 0 ; count_x < 8 ; count_x++ ){
      for( count_y = 0 ; count_y < 8 ; count_y++ ){
	get_board( old_gameboard, count_x, count_y ) = get_board( game_board, count_x, count_y );
      }
    }
    
    old_pointer += 64;
    
    for( count_x = 0 ; count_x < 8 ; count_x++ ){
      for( count_y = 0 ; count_y < 8 ; count_y++ ){
	get_board( old_gameboard + old_pointer, count_x, count_y ) = get_board( game_board, count_x, count_y );
      }
    }
    
    old_pointer += 64;
  }

  do{
    for( cur_player = starting_player ; cur_player < 2 ; cur_player++ ){
      starting_player = 0;
      if( possible_move( game_board, cur_player ) ){
	if( p[cur_player] == 0 ){
	  get_move( cur_player, &x, &y );
	}
	else{
#ifndef DEBUG
	  get_comp_move( cur_player, game_board, &x, &y, game_l[cur_player], 1 );
#else /* DEBUG */
	  a = get_comp_move( cur_player, game_board, &x, &y, game_l[cur_player], 1 );
	  mvaddstr( 6, 52, "                       " );
	  sprintf( str, "Score for move: %d", a );
	  mvaddstr( 6, 52, str );
#endif /* DEBUG */
	  if( !possible_move_square( game_board, x, y, cur_player ) ){
	    printf( "Internal panic! Tried to put square at: %d, %d\n", x, y );
	    exit( 1 );
	  }
	}
	put_square( cur_player, game_board, x, y );
	draw_board( game_board );

      }
      else{
	if( possible_move( game_board, !cur_player ) ){
	  mvaddstr( 7, 52, "Cannot move, press space." );
	  refresh();
	  getch();
	  mvaddstr( 7, 52, "                         " );
	  refresh();
	}
	else{
	  end_w = TRUE;
	}
      }

      for( count_x = 0 ; count_x < 8 ; count_x++ ){
	for( count_y = 0 ; count_y < 8 ; count_y++ ){
	  get_board( old_gameboard + old_pointer, count_x, count_y ) = get_board( game_board, count_x, count_y );
	}
      }
      old_pointer += 64;
	
    }
  }while( !end_w );

  for( x2 = 0 ; x2 < 8 ; x2++ ){
    for( y2 = 0 ; y2 < 8 ; y2++ ){
      if( get_board( game_board, x2, y2 ) == 1 ){
	c_w++;
      }
      else if( get_board( game_board, x2, y2 ) == 2 ){
	c_w2++;
      }
    }
  }
  
  if( c_w == c_w2 ){
    mvaddstr( 9, 52, "This game was a draw." );
    refresh();
  }
  else if( c_w > c_w2 ){
    mvaddstr( 9, 52, "White is the winner." );
    refresh();
  }
  else{
    mvaddstr( 9, 52, "Black is the winner." );
    refresh();
  }
  sprintf( str, "White: %d, Black: %d", c_w, c_w2 );
  mvaddstr( 10, 52, str );
  move( 23, 0 );
  refresh();
}

do_undo_move()
{
  int count_x, count_y;

  if( old_pointer > 192 ){
    old_pointer -= 192;
    for( count_x = 0 ; count_x < 8 ; count_x++ ){
      for( count_y = 0 ; count_y < 8 ; count_y++ ){
	get_board( game_board, count_x, count_y ) = get_board( old_gameboard + old_pointer, count_x, count_y );
      }
    }

    draw_board( game_board );
    old_pointer += 64;
  }
}

save_game( player )
int player;
{
  FILE *fp;
  char home_path[80];
  int c;
  int x, y;
  int t;

  strcpy( home_path, getenv( "HOME" ) );
  strcat( home_path, "/.reversi_save" );
  
  if( (fp = fopen( home_path, "w" )) == NULL ){
    mvaddstr( 8, 52, "Could not open save file" );
    refresh();
    return;
  }

  fwrite( &player, sizeof( int ), 1, fp );
  fwrite( &p1, sizeof( int ), 1, fp );
  fwrite( &p2, sizeof( int ), 1, fp );
  fwrite( &game_lev1, sizeof( int ), 1, fp );
  fwrite( &game_lev2, sizeof( int ), 1, fp );

  for( c = 0 ; c < old_pointer ; c += 64 ){
    for( x = 0 ; x < 8 ; x++ ){
      for( y = 0 ; y < 8 ; y++ ){
	t = get_board( old_gameboard + c, x, y );
	fwrite( &t, sizeof( int ), 1, fp );
      }
    }
  }
  fclose( fp );
}
