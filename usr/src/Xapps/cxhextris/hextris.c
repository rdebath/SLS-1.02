/*
 * hextris Copyright 1990 David Markley, dm3e@+andrew.cmu.edu, dam@cs.cmu.edu
 *
 * Permission to use, copy, modify, and distribute, this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of the copyright holders be used in
 * advertising or publicity pertaining to distribution of the software with
 * specific, written prior permission, and that no fee is charged for further
 * distribution of this software, or any modifications thereof.  The copyright
 * holder make no representations about the suitability of this software for
 * any purpose.  It is provided "as is" without express or implied warranty.
 *
 * THE COPYRIGHT HOLDER DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA, PROFITS, QPA OR GPA, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE 
 * OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 */

/* This file contains the heart of hextris. It contains absolutely no
 * direct I/O calls, for maximum portability. All I/O that is performed is
 * done so by either the I/O handler directly, calls from the I/O handler
 * to functions in this file that call functions in the I/O handler, or by
 * functions in this file that call functions in the I/O handler.
 */

#include "header.h"

/* This places the piece on the board in its starting position. All the
 * hexes in the piece are displayed.
 */
init_piece(piece, upos)
piece_t *piece; int upos;
{
    int base, form, i;
    base = (piece->column % 2) ? 0 : 8;
    form = piece->type * 6 + piece->rotation;

    if (upos)
      redraw_position();
    for (i = 0; i < 7; i += 2) {
      draw_hex(piece->row+shape[form][base+i],
		 piece->column+shape[form][base+1+i],1,piece->type);
      if (upos)
	draw_pos(piece->column+shape[form][base+1+i],1,piece->type);
    }
}

redraw_position()
{
  int i;

  for (i = 0; i < MAXCOLUMN; i++)
    draw_hex(MAXROW + 2, i, 0, 10);

}

/* This places the piece in its new position. This is done by comparing
 * the current piece's position (piece) to the new position (tpiece),
 * removing any hexes that will no longer be covered, and placing
 * any hexes that will be covered. In this way, any piece that is already
 * covered, and will be covered, is not redrawn.
 */
place_piece(piece,tpiece,upos)
piece_t *piece, *tpiece; int upos;
{
    int base, form, tbase, tform, i, j, diff;
    int pos[MAXCOLUMN];
    base = (piece->column % 2) ? 0 : 8;
    form = piece->type * 6 + piece->rotation;
    tbase = (tpiece->column % 2) ? 0 : 8;
    tform = tpiece->type * 6 + tpiece->rotation;

    if (upos) {
      for (i = 0; i < MAXCOLUMN; i++)
	pos[i] = 0;
      redraw_position();
    }

    for (i = 0; i < 7; i += 2) {
	for (j = 0; j < 7; j += 2) {
	    diff = 1;
	    if ((tpiece->row+shape[tform][tbase+i] == 
		 piece->row+shape[form][base+j])
		&& (tpiece->column+shape[tform][tbase+1+i] == 
		    piece->column+shape[form][base+1+j])) {
		diff = 0;
		break;
		    }
	}
	if (diff)
	  draw_hex(tpiece->row+shape[tform][tbase+i],
		     tpiece->column+shape[tform][tbase+1+i],1,tpiece->type);
	if (upos) {
	  if (!pos[i]) {
	    draw_pos(tpiece->column+shape[tform][tbase+1+i],1,tpiece->type);
	    pos[i] = 1;
	  }
	}
    }

    for (i = 0; i < 7; i += 2) {
	for (j = 0; j < 7; j += 2) {
	    diff = 1;
	    if ((piece->row+shape[form][base+i] == 
		    tpiece->row+shape[tform][tbase+j])
		   && (piece->column+shape[form][base+1+i] == 
		       tpiece->column+shape[tform][tbase+1+j])) {
		diff = 0;
		break;
		       }
	}
	if (diff)
	  draw_hex(piece->row+shape[form][base+i],
		     piece->column+shape[form][base+1+i],0,piece->type);
    }
}

/* This checks to see if the proposed position of the piece (tpiece) will
 * legally fit in the grid.
 */
check_piece(tpiece,grid)
piece_t *tpiece;
position_t grid[MAXROW][MAXCOLUMN];
{
    int base, form, i;
    base = (tpiece->column % 2) ? 0 : 8;
    form = tpiece->type * 6 + tpiece->rotation;


    for (i = 0; i < 7; i += 2)
      if (grid[tpiece->row+shape[form][base+i]]
	  [tpiece->column+shape[form][base+1+i]].filled)
	return 1;

    if ((tpiece->row+3) >= MAXROW) /* If the piece is near the bottom */
      for (i = 0; i < 7; i += 2)
	if (((tpiece->row)+shape[form][base+i]) >= MAXROW)
	  return 1;

    if ((tpiece->column+3) >= MAXCOLUMN) /* If the piece is near the right */
      for (i = 1; i < 8; i += 2)
	if (tpiece->column+shape[form][base+i] >= MAXCOLUMN)
	  return 1;

    if ((tpiece->column-2) < 1) /* If the piece is near the left */
      for (i = 1; i < 8; i += 2)
	if (tpiece->column + shape[form][base+i] < 0)
	  return 1;

    return 0;
}

/* This drops the piece into the grid, in its final resting place.
 */
drop_piece(piece,grid)
piece_t *piece;
position_t grid[MAXROW][MAXCOLUMN];
{
    int base, form, i;
    base = (piece->column & 1) ? 0 : 8;
    form = piece->type * 6 + piece->rotation;

    for (i = 0; i < 7; i += 2) {
	grid[piece->row+shape[form][base+i]]
	  [piece->column+shape[form][base+1+i]].type = piece->type;
	grid[piece->row+shape[form][base+i]]
	  [piece->column+shape[form][base+1+i]].filled = 1;
    }
}

/* This redraws the entire grid.
 */
redraw_grid(grid)
position_t grid[MAXROW][MAXCOLUMN];
{
    int row, column;

    for (row = MAXROW - 1; row >= 0; row--)
	for (column = 0; column < MAXCOLUMN; column++)
	    draw_hex(row,column,grid[row][column].filled,
		       grid[row][column].type);
}

/* This clears the current, normal row, and moves the rest of the
 * grid down.
 */
shift_redraw_grid(start_row,grid)
int start_row;
position_t grid[MAXROW][MAXCOLUMN];
{
    int row, column;

    start_row = (start_row < MAXROW) ? start_row : MAXROW - 1;

    for (row = start_row; row > 0; row--)
	for (column = 0; column < MAXCOLUMN; column++) {
	    if (grid[row][column].filled)
	      draw_hex(row,column,0,0);
	    grid[row][column].type = grid[row-1][column].type;
	    grid[row][column].filled = grid[row-1][column].filled;
	    if (grid[row][column].filled)
	      draw_hex(row,column,grid[row][column].filled,
			 grid[row][column].type);
	}
    row = 0;
    for (column = 0; column < MAXCOLUMN; column++) {
	grid[row][column].filled = 0;
	draw_hex(row,column,grid[row][column].filled,grid[row][column].type);
    }
}

/* This clears the current, offset row, and moves the rest of the
 * grid down.
 */
shift_offset_redraw_grid(start_row,grid)
int start_row;
position_t grid[MAXROW][MAXCOLUMN];
{
    int row, column;

    start_row = (start_row < MAXROW) ? start_row : MAXROW - 1;

    for (row = start_row; row > 1; row--)
	for (column = 0; column < MAXCOLUMN; column++) {
	    if (grid[row-(column & 1)][column].filled)
	      draw_hex(row-(column & 1),column,0,0);
	    grid[row-(column & 1)][column].type =
	      grid[row-1-(column & 1)][column].type;
	    grid[row-(column & 1)][column].filled =
	      grid[row-1-(column & 1)][column].filled;
	    if (grid[row-(column & 1)][column].filled)
	      draw_hex(row-(column & 1),column,
			 grid[row-(column & 1)][column].filled,
			 grid[row-(column & 1)][column].type);
	}
    row = 1;
    for (column = 0; column < MAXCOLUMN; column++) {
	grid[row-(column & 1)][column].filled = 0;
	draw_hex(row-(column & 1),column,
		   grid[row-(column & 1)][column].filled,
		   grid[row-(column & 1)][column].type);
    }
}

/* This checks for any cleared rows, be they normal or offset.
 */
check_rows(grid)
position_t grid[MAXROW][MAXCOLUMN];
{
    int row, column, clear_row, clear_offset_row, total_clear;

    total_clear = 0;
    for (row = MAXROW-1; row >= 0; row--) {
	clear_row = 1;
	for (column = 0; column < MAXCOLUMN; column++)
	  if (! grid[row][column].filled) {
	      clear_row = 0;
	      break;
	  }
	if (clear_row) {
	    shift_redraw_grid(row,grid);
	    total_clear++;
	}
	clear_offset_row = 1;
	for (column = 0; column < MAXCOLUMN; column++)
	  if (! grid[row-(column & 1)][column].filled) {
	      clear_offset_row = 0;
	      break;
	  }
	if (clear_offset_row) {
	    shift_offset_redraw_grid(row,grid);
	    total_clear++;
	}
	if (clear_row || clear_offset_row)
	  row++;
    }
    return total_clear;
}

/* This handles the users choices.
 *
 * Choices:
 * 0 - Left
 * 1 - Right
 * 2 - Rotate CCW
 * 3 - Rotate CW
 * 4 - Drop
 * 6 - Quit
 */
update(choice,grid,npiece,piece,score,rows)
int choice;
position_t grid[MAXROW][MAXCOLUMN];
piece_t *npiece,*piece;
int *score, *rows;
{
    piece_t tpiece;

    tpiece.row = piece->row;
    tpiece.column = piece->column;
    tpiece.type = piece->type;
    tpiece.rotation = piece->rotation;
    
    switch (choice) {
    case 0:
	tpiece.column--;
	if (! check_piece(&tpiece,grid)) {
	    place_piece(piece,&tpiece, 1);
	    piece->column = tpiece.column;
	    piece->row = tpiece.row;
	} else {
	    tpiece.row -= (1-((piece->column & 1)*2));
	    if (! check_piece(&tpiece,grid)) {
		place_piece(piece,&tpiece, 1);
		piece->column = tpiece.column;
		piece->row = tpiece.row;
	    }
	}
	break;
    case 1:
	tpiece.column++;
	if (! check_piece(&tpiece,grid)) {
	    place_piece(piece,&tpiece, 1);
	    piece->column = tpiece.column;
	    piece->row = tpiece.row;
	} else {
	    tpiece.row -= (1-((piece->column & 1)*2));
	    if (! check_piece(&tpiece,grid)) {
		place_piece(piece,&tpiece, 1);
		piece->column = tpiece.column;
		piece->row = tpiece.row;
	    }
	}
	break;
    case 2:
	tpiece.rotation++;
	tpiece.rotation = (tpiece.rotation > 5) ? 0 : tpiece.rotation;
	if (! check_piece(&tpiece,grid)) {
	    place_piece(piece,&tpiece, 1);
	    piece->rotation = tpiece.rotation;
	    piece->column = tpiece.column;
	    piece->row = tpiece.row;
	}
	break;
    case 3:
	tpiece.rotation--;
	tpiece.rotation = (tpiece.rotation < 0) ? 5 : tpiece.rotation;
	if (! check_piece(&tpiece,grid)) {
	    place_piece(piece,&tpiece, 1);
	    piece->rotation = tpiece.rotation;
	    piece->column = tpiece.column;
	    piece->row = tpiece.row;
	}
	break;
    case 4:
	tpiece.row++;
	while (! check_piece(&tpiece,grid)) {
	    place_piece(piece,&tpiece,0);
	    piece->row++;
	    tpiece.row++;
	    *score += 10;
	}
	return update_drop(grid,npiece,piece,score,rows);
	break;
    case 6:
	return 1;
	break;
    }
    return 0;
}

/* This process the normal dropping caused by the passage of time. The io
 * handler calls this, when it is time to drop the piece.
 */
update_drop(grid,npiece,piece,score,rows)
position_t grid[MAXROW][MAXCOLUMN];
piece_t *npiece,*piece;
int *score, *rows;
{
    int cleared_rows, game_over;
    piece_t tpiece;

    tpiece.row = piece->row;
    tpiece.column = piece->column;
    tpiece.type = piece->type;
    tpiece.rotation = piece->rotation;

    tpiece.row++;
    if (check_piece(&tpiece,grid)) {
	*score += 25;
	drop_piece(piece,grid);
	cleared_rows = check_rows(grid);
	*score += (cleared_rows * 5000);
	*rows += cleared_rows;
	display_scores(score,rows);
	game_over =  (piece->row < 2) ? 1 : 0;
	new_piece(npiece,piece);
	init_piece(piece, 1);
	show_next_piece(npiece);
    } else {
	tpiece.row--;
	piece->row++;
	place_piece(&tpiece,piece, 0);
	game_over = 0;
    }
    return game_over;
}

/* This sets up things for a new game.
 */
new_game(grid,npiece,piece,score,rows)
position_t grid[MAXROW][MAXCOLUMN];
piece_t *npiece,*piece;
int *score, *rows;
{
    int row, column;

    for (row = 0; row < MAXROW; row++)
      for (column = 0; column < MAXCOLUMN; column++)
	grid[row][column].filled = 0;

    npiece->rotation = -1;
    new_piece(npiece,piece);
    *score = 0;
    *rows = 0;
    init_piece(piece, 1);
    show_next_piece(npiece);
}

/* This draws the borders of the game.
 */
draw_borders()
{
    int i;

    for (i = 0; i <= MAXROW; i++) {
	draw_hex(i,-1,1,10); 
	draw_hex(i,MAXCOLUMN,1,10); 
    }
    for (i = 0; i < MAXCOLUMN; i++)
      draw_hex(MAXROW,i,1,10);
}

/* This checks to see if the score received from a game is high enough
 * to be in the high scores. If the user already has his limit of
 * high scores, it also makes sure he beat one of his own. If he did beat
 * one of his own, the lowest of hist scores is removed, and the new one
 * is placed.
 */
is_high_score(name,userid,score,rows,high_scores)
char name[MAXNAMELENGTH];
char userid[MAXUSERIDLENGTH];
int score;
int rows;
high_score_t high_scores[MAXHIGHSCORES];
{
    int i,j, user_highs, added, equal, over;

    user_highs = added = over = equal = 0;
    for (i = 0; i < MAXHIGHSCORES; i++) {
	equal = (! strcmp(userid,high_scores[i].userid));
	if (equal)
	    over = (++user_highs > MAXUSERHIGHS) ? 1 : 0;
	if (over && equal) {
	    for (j = i; j < (MAXHIGHSCORES - 1); j++) {
		strcpy(high_scores[j].name, high_scores[j+1].name);
		strcpy(high_scores[j].userid, high_scores[j+1].userid);
		high_scores[j].score = high_scores[j+1].score;
		high_scores[j].rows = high_scores[j+1].rows;
	    }
	    strcpy(high_scores[MAXHIGHSCORES-1].name, "nobody");
	    strcpy(high_scores[MAXHIGHSCORES-1].userid, "NON");
	    high_scores[MAXHIGHSCORES-1].score = 0;
	    high_scores[MAXHIGHSCORES-1].rows = 0;
	    i--;
	    continue;
	}
	if ((high_scores[i].score <= score)
	    && ((equal && (user_highs == MAXUSERHIGHS))
	    || (user_highs < MAXUSERHIGHS)) && (! added))  {
	    if (! equal)
	      over = (++user_highs > MAXUSERHIGHS);
	    for (j = MAXHIGHSCORES - 1; j > i; j--) {
		strcpy(high_scores[j].name, high_scores[j-1].name);
		strcpy(high_scores[j].userid, high_scores[j-1].userid);
		high_scores[j].score = high_scores[j-1].score;
		high_scores[j].rows = high_scores[j-1].rows;
	    }
	    strcpy(high_scores[i].name, name);
	    strcpy(high_scores[i].userid, userid);
	    high_scores[i].score = score;
	    high_scores[i].rows = rows;
	    added = 1;
	    }
    }
    return added;
}

/* This takes the raw information from the I/O handler, and breaks down the
 * key stroke into a choice. Notice: It also transfers the variable the
 * I/O handler supplies it.
 */
do_choice(choice,grid,npiece,piece,score,rows,game_over,game_view,high_scores)
char *choice;
position_t grid[MAXROW][MAXCOLUMN];
piece_t *npiece,*piece;
int *score, *rows, *game_over, *game_view;
high_score_t high_scores[MAXHIGHSCORES];
{
    switch (choice[0]) {
    case 'j': case 'J': case '4':
	if (! *game_over)
	  update(0,grid,npiece,piece,score,rows);
	break;
    case 'l': case 'L': case '6':
	if (! *game_over)
	  update(1,grid,npiece,piece,score,rows);
	break;
    case 'k': case 'K': case '5':
	if (! *game_over)
	  update(2,grid,npiece,piece,score,rows);
	break;
    case 'i': case 'I': case '8':
	if (! *game_over)
	  update(3,grid,npiece,piece,score,rows);
	break;
    case ' ': case '0':
	if (! *game_over)
	  *game_over = update(4,grid,npiece,piece,score,rows);
	break;
    case 'r': case 'R':
	clear_display();
	redraw_game(grid,npiece,piece,score,rows,*game_view,high_scores);
	break;
    case 'q': case 'Q':
	update(6,grid,npiece,piece,score,rows);
	end_game();
	break;
    case 'p': case 'P':
	if (! *game_over) {
	    *game_over = 2;
	    display_high_scores(high_scores);
	    display_help_score();
	}
	break;
    case 'u': case 'U':
	if (*game_over == 2) {
	    *game_over = *game_view = 0;	
	    clear_display();
	    redraw_game(grid,npiece,piece,score,rows,*game_view,high_scores);
	}
	break;
    case 'H': case 'h':
	if (*game_over) {
	    *game_view = 1;
	    clear_display();
	    redraw_game(grid,npiece,piece,score,rows,*game_view,high_scores);
	}
	break;
    case 'G': case 'g':
	if (*game_over) {
	    *game_view = 0;
	    clear_display();
	    redraw_game(grid,npiece,piece,score,rows,*game_view,high_scores);
	}
	break;
    case 'n': case 'N':
	*game_over = *game_view = 0;
	new_game(grid, npiece, piece, score, rows);
	clear_display();
	redraw_game(grid,npiece,piece,score,rows,*game_view,high_scores);
#ifdef LOG
	loguse(LOGHOST,"xhexlog",log_message);
#endif
      case '\024':
	if (strcmp(choice, "\024[A") == 0) {
	  if (! *game_over)
	    update(3,grid,npiece,piece,score,rows);
	}
	else if (strcmp(choice, "\024[B") == 0) {
	  if (! *game_over)
	    update(0,grid,npiece,piece,score,rows);
	}
	else if (strcmp(choice, "\024[C") == 0) {
	  if (! *game_over)
	    update(1,grid,npiece,piece,score,rows);
	}
	else if (strcmp(choice, "\024[D") == 0) {
	  if (! *game_over)
	    update(2,grid,npiece,piece,score,rows);
	}
	break;
    }
}

/* This redraws all the parts of the game through the I/O handler.
 */
redraw_game(grid,npiece,piece,score,rows,game_view,high_scores)
position_t grid[MAXROW][MAXCOLUMN];
piece_t *npiece,*piece;
int *score, *rows, game_view;
high_score_t high_scores[MAXHIGHSCORES];
{
    if (! game_view) {
	redraw_grid(grid);
	draw_borders();
	init_piece(piece, 1);
	display_scores(score,rows);
	display_help();
	show_next_piece(npiece);
    } else {
	if (read_high_scores(high_scores))
	  display_high_scores(high_scores);
	display_help_score();
/*	display_scores(score,rows);*/
    }
}
