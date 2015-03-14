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

#include "reversi.h"

extern int *game_board;

/*
 * possible_move, returns FALSE if there are no possible moves for
 * the player, and TRUE if there is.
 *
 */

possible_move( board, player )
int *board;
int player;
{
  int x, y;
  
  for( x = 0 ; x < 8 ; x++ ){
    for( y = 0 ; y < 8 ; y++ ){
      if( get_board( board, x, y ) == 0 ){
	if( possible_move_square( board, x, y, player ) ){
	  return( TRUE );
	}
      }
    }
  }
  return( FALSE );
}

/*
 * possible_move_square, returns FALSE if x, y is an illegal move
 * for player.
 *
 */

possible_move_square( board, x, y, player )
int *board;
int x, y, player;
{
  int dir, x2, y2;
  
  if( get_board( board, x, y ) != 0 ) return( FALSE );
  for( dir = 0 ; dir < 8 ; dir++ ){
    if( possible_move_dir( board, x, y, dir, player, &x2, &y2 ) != 0 ){
      return( TRUE );
    }
  }
  return( FALSE );
}

/*
 * possible_move_dir, returns 0 if x, y, dir is an illegal move
 * for player, otherwise the number of turned squares
 *
 */

possible_move_dir( board, x, y, dir, player, x2, y2 )
int *board;
int x, y, dir, player;
int *x2, *y2;
{
  *x2 = x;
  *y2 = y;


  switch( dir ){
  case 0: /* Up */
    if( --*y2 < 1 ) return( 0 );
    if( get_board( board, x, *y2 ) == 0 || get_board( board, x, *y2 ) == (player + 1)) return( 0 );
    while( *y2 > 0 ){
      (*y2)--;
      if( get_board( board, x, *y2 ) == 0 ) return( 0 );
      if( get_board( board, x, *y2 ) == (player + 1) ) return( y - *y2 );
    }
    break;
  case 1: /* Up Right */
    if( --*y2 < 1 || ++*x2 > 6 ) return( 0 );
    if( get_board( board, *x2, *y2 ) == 0 || get_board( board, *x2, *y2 ) == (player + 1)) return( 0 );
    while( *y2 > 0 && *x2 < 7 ){
      (*y2)--;
      (*x2)++;
      if( get_board( board, *x2, *y2 ) == 0 ) return( 0 );
      if( get_board( board, *x2, *y2 ) == (player + 1) ) return( *x2 - x );
    }
    break;
  case 2: /* Right */
    if( ++*x2 > 6 ) return( 0 );
    if( get_board( board, *x2, y ) == 0 || get_board( board, *x2, y ) == (player + 1)) return( 0 );
    while( *x2 < 7 ){
      (*x2)++;
      if( get_board( board, *x2, y ) == 0 ) return( 0 );
      if( get_board( board, *x2, y ) == (player + 1) ) return( *x2 - x );
    }
    break;
  case 3: /* Down Right */
    if( ++*y2 > 6 || ++*x2 > 6 ) return( 0 );
    if( get_board( board, *x2, *y2 ) == 0 || get_board( board, *x2, *y2 ) == ( player + 1)) return( 0 );
    while( *y2 < 7 && *x2 < 7 ){
      (*y2)++;
      (*x2)++;
      if( get_board( board, *x2, *y2 ) == 0 ) return( 0 );
      if( get_board( board, *x2, *y2 ) == (player + 1) ) return( *x2 - x );
    }
    break;
  case 4: /* Down */
    if( ++*y2 > 6 ) return( 0 );
    if( get_board( board, x, *y2 ) == 0 || get_board( board, x, *y2 ) == (player + 1)) return( 0 );
    while( *y2 < 7 ){
      (*y2)++;
      if( get_board( board, x, *y2 ) == 0 ) return( 0 );
      if( get_board( board, x, *y2 ) == (player + 1) ) return( *y2 - y );
    }
    break;
  case 5: /* Down Left */
    if( ++*y2 > 6 || --*x2 < 1 ) return( 0 );
    if( get_board( board, *x2, *y2 ) == 0 || get_board( board, *x2, *y2 ) == (player + 1)) return( 0 );
    while( *y2 < 7 && *x2 > 0 ){
      (*y2)++;
      (*x2)--;
      if( get_board( board, *x2, *y2 ) == 0 ) return( 0 );
      if( get_board( board, *x2, *y2 ) == (player + 1) ) return( x - *x2 );
    }
    break;
  case 6: /* Left */
    if( --*x2 < 1 ) return( 0 );
    if( get_board( board, *x2, y ) == 0 || get_board( board, *x2, y ) == (player + 1)) return( 0 );
    while( *x2 > 0 ){
      (*x2)--;
      if( get_board( board, *x2, y ) == 0 ) return( 0 );
      if( get_board( board, *x2, y ) == (player + 1) ) return( x - *x2 );
    }
    break;
  case 7: /* Up Left */
    if( --*y2 < 1 || --*x2 < 1 ) return( 0 );
    if( get_board( board, *x2, *y2 ) == 0 || get_board( board, *x2, *y2 ) == (player + 1)) return( 0 );
    while( *y2 > 0 && *x2 > 0 ){
      (*y2)--;
      (*x2)--;
      if( get_board( board, *x2, *y2 ) == 0 ) return( 0 );
      if( get_board( board, *x2, *y2 ) == (player + 1) ) return( x - *x2 );
    }
    break;
  }
  return( 0 );
}

/*
 * put_square, puts a square of mark player on the board, turning
 * other squares
 *
 */

put_square( player, board, x, y )
int player;
int *board;
int x, y;
{
  int dir, x2, y2;
  
  for( dir = 0 ; dir < 8 ; dir++ ){
    if( possible_move_dir( board, x, y, dir, player, &x2, &y2 ) != 0 ){
      get_board( board, x, y ) = player + 1;
      flip( board, x, y, x2, y2, dir, player );
    }
  }
}

/*
 * flip, flips all squares in a line, direction dir
 *
 */

flip( board, x, y, x2, y2, dir, player )
int *board;
int x, y, x2, y2, dir, player;
{
  int a_x, a_y;
  
  switch( dir ){
  case 0: /* Up */
    a_x = 0;
    a_y = -1;
    break;
  case 1: /* Up Right */
    a_x = 1;
    a_y = -1;
    break;
  case 2: /* Right */
    a_x = 1;
    a_y = 0;
    break;
  case 3: /* Down Right */
    a_x = 1;
    a_y = 1;
    break;
  case 4: /* Down */
    a_x = 0;
    a_y = 1;
    break;
  case 5: /* Down Left */
    a_x = -1;
    a_y = 1;
    break;
  case 6: /* Left */
    a_x = -1;
    a_y = 0;
    break;
  case 7: /* Up Left */
    a_x = -1;
    a_y = -1;
    break;
  }
  
  x += a_x;
  y += a_y;
  
  do{
    get_board( board, x, y ) = player + 1;
    x += a_x;
    y += a_y;
  }while( get_board( board, x, y ) == ( !player + 1 ) );
}
