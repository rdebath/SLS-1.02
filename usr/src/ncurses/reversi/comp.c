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
#include <ncurses.h>
#include "reversi.h"

extern int *game_board;

/*
 * get_comp_move, calculates the best move for player
 *
 */

get_comp_move( player, board, x, y, level, f )
int player;
int *board;
int *x, *y;
int level, f;
{
  int score[8][8];
  int max_score = -10000;
  int x2, y2, c = 0;
  int t = FALSE;
  char str[20];
  
  for( x2 = 0 ; x2 < 8 ; x2++ ){
    for( y2 = 0 ; y2 < 8 ; y2++ ){
      score[x2][y2] = -10000;
    }
  }
  
  if( f ){
    for( x2 = 0 ; x2 < 8 ; x2++ ){
      for( y2 = 0 ; y2 < 8 ; y2++ ){
	if( get_board( board, x2, y2 ) == 0 ){
	  c++;
	}
      }
    }
    if( c <= 15 ){
      level++;
    }

    mvaddstr( 7, 52, "                   " );
    move( 7, 52 );
    refresh();
  }	
  
  /* Go though all possible squares */
  for( x2 = 0 ; x2 < 8 ; x2++ ){
    for( y2 = 0 ; y2 < 8 ; y2++ ){
      if( possible_move_square( board, x2, y2, player ) ){
	score[x2][y2] = calc_score_move( board, x2, y2, player, level );
	if( f ) addch( '.' );
	refresh();
      }
    }
  }

#ifndef DEBUG
  /* Return best score for move */
  for( x2 = 0 ; x2 < 8 ; x2++ ){
    for( y2 = 0 ; y2 < 8 ; y2++ ){
      if( possible_move_square( board, x2, y2, player ) ){
	if( score[x2][y2] > max_score ){
	  max_score = score[x2][y2];
	  *x = x2;
	  *y = y2;
	}
	t = TRUE;
      }
    }
  }
#else /* DEBUG */
  /* Return best score for move */
  if( !f ){
    for( x2 = 0 ; x2 < 8 ; x2++ ){
      for( y2 = 0 ; y2 < 8 ; y2++ ){
	if( possible_move_square( board, x2, y2, player ) ){
	  if( score[x2][y2] > max_score ){
	    max_score = score[x2][y2];
	    *x = x2;
	    *y = y2;
	  }
	  t = TRUE;
	}
      }
    }
  }
  else{
    for( x2 = 0 ; x2 < 8 ; x2++ ){
      for( y2 = 0 ; y2 < 8 ; y2++ ){
	if( possible_move_square( board, x2, y2, player ) ){
	  sprintf( str, "%d", score[x2][y2] );
	  mvaddstr( y2 * 3, x2 * 6, str );
	  if( score[x2][y2] > max_score ){
	    max_score = score[x2][y2];
	    *x = x2;
	    *y = y2;
	  }
	  t = TRUE;
	}
      }
    }
    refresh();
    getch();
    draw_board( game_board );
  }
#endif /* DEBUG */

  if( !t ){
    printf( "Internal panic! No possible move found!\n" );
    printf( "x = %d, y = %d, score = %d\n", *x, *y, max_score );
    exit( 1 );
  }
  return( max_score );
}

/*
 * calc_score_move, returns the score for move
 *
 */

calc_score_move( board, x, y, player, level )
int *board;
int x, y, player, level;
{
  int temp_board[64];
  int dir, score;
  int x2, y2, x3, y3, tmp;
  
  score = 0;

  for( x2 = 0 ; x2 < 8 ; x2++ ){
    for( y2 = 0 ; y2 < 8 ; y2++ ){
      get_board( temp_board, x2, y2 ) = get_board( board, x2, y2 );
    }
  }

  for( dir = 0 ; dir < 8 ; dir++ ){
    if( (tmp = possible_move_dir( board, x, y, dir, player, &x2, &y2 )) != 0 ){
      tmp *= SQUARE_SCORE;
      score += tmp;
    }
  }
  tmp = add_score_move( board, x, y, player );
  score += tmp;
  
  if( level != 0 ){
    put_square( player, temp_board, x, y );
    if( possible_move( temp_board, !player ) ){
      tmp = get_comp_move( !player, temp_board, &x3, &y3, level - 1, 0 );
      score += 4000 - (int)((float)tmp / DIV_POINTS);
    }
    else{
      score += NO_MOVE_BONUS;
    }
  }
  
  return( score );
}

/*
 * add_score_move, calulate the bonus for this position
 *
 */

add_score_move( board, x, y, player )
int *board;
int x, y, player;
{
  int tmp;
  int c;

  if( (x == 0 && y == 0 ) ||
     (x == 0 && y == 7 ) ||
     (x == 7 && y == 0 ) ||
     (x == 7 && y == 7 )){
    return( CORNER_SCORE );
  }
  
  if( (x == 0) || (x == 7) ){
    if( y <= 3 ){
      tmp = BORDER_SCORE + BORDER_POS_BONUS * (y + 1);
    }
    else{
      tmp = BORDER_SCORE + (BORDER_POS_BONUS * ((3 - (y - 4)) + 1));
    }
    
    if( get_board( board, x, y - 1 ) == (!player + 1) && get_board( board, x, y + 1 ) != (!player + 1) ){
      c = y - 1;
      while( (get_board( board, x, c ) == (!player + 1)) && (c > 0) ) c--;
      if( get_board( board, x, c ) != (player + 1) ){
	return( 0 );
      }
    }
    else if( get_board( board, x, y + 1 ) == (!player + 1) && get_board( board, x, y - 1 ) != (!player + 1) ){
      c = y + 1;
      while( (get_board( board, x, c ) == (!player + 1)) && (c < 7) ) c++;
      if( get_board( board, x, c ) != (player + 1) ){
	return( 0 );
      }
    }

/* Check if this is a double score position */
    if( get_board( board, x, y - 1 ) == (!player + 1) && get_board( board, x, y + 1 ) == (!player + 1) ){
      return( tmp * 2 );
    }
    else{
      return( tmp );
    }
  }
  
  if( (y == 0) || (y == 7) ){
    if( x <= 3 ){
      tmp = BORDER_SCORE + BORDER_POS_BONUS * (x + 1);
    }
    else{
      tmp = BORDER_SCORE + (BORDER_POS_BONUS * ((3 - (x - 4)) + 1));
    }
    
    if( get_board( board, x - 1, y ) == (!player + 1) && get_board( board, x + 1, y ) != (!player + 1) ){
      c = x - 1;
      while( (get_board( board, c, y ) == (!player + 1)) && (c > 0) ) c--;
      if( get_board( board, c, x ) != (player + 1) ){
	return( 0 );
      }
    }
    else if( get_board( board, x + 1, y ) == (!player + 1) && get_board( board, x - 1, y ) != (!player + 1) ){
      c = y - 1;
      while( (get_board( board, c, y ) == (!player + 1)) && (c < 7) ) c++;
      if( get_board( board, c, x ) != (player + 1) ){
	return( 0 );
      }
    }

/* Check if this is a double score position */
    if( get_board( board, x - 1, y ) == (!player + 1) && get_board( board, x + 1, y ) == (!player + 1) ){
      return( tmp * 2 );
    }
    else{
      return( tmp );
    }
  }
  
  if( (x == 1 && y == 1) ||
     (x == 1 && y == 6) ||
     (x == 6 && y == 1) ||
     (x == 6 && y == 6)){
    return( BORDER2_CORNER_SCORE );
  }
  
  if( (x == 1) || (x == 6) ){
    if( y <= 3 ) return( BORDER2_SCORE + BORDER2_POS_BONUS * y );
    else return( BORDER2_SCORE + (BORDER2_POS_BONUS * (4 - (y - 3))) );
  }
  
  if( (y == 1) || (y == 6) ){
    if( x <= 3 ) return( BORDER2_SCORE + BORDER2_POS_BONUS * x );
    else return( BORDER2_SCORE + (BORDER2_POS_BONUS * (4 - (x - 3))) );
  }
  
  if( (x == 2 && y == 2) ||
     (x == 2 && y == 5) ||
     (x == 5 && y == 2) ||
     (x == 5 && y == 5)){
    return( BORDER3_CORNER_SCORE );
  }
  
  if( (x == 2) || (y == 5) ){
    return( BORDER3_SCORE );
  }
  
  if( (y == 2) || (y == 5) ){
    return( BORDER3_SCORE );
  }
  
  return( 0 );
}
