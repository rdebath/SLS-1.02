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

#define TRUE (1)
#define FALSE (0)

/* Some score definitions */

#define SQUARE_SCORE 10
#define BORDER_SCORE 800
#define CORNER_SCORE 4000
#define BORDER_POS_BONUS 10
#define BORDER2_POS_BONUS 10
#define BORDER2_SCORE 50
#define BORDER2_CORNER_SCORE -200
#define BORDER3_CORNER_SCORE 150
#define BORDER3_SCORE 400

#define NO_MOVE_BONUS 10000
#define DIV_POINTS 1.2  /* This is a float! */

#define get_board(board,X,Y) (board)[((X)*8) + (Y)]

