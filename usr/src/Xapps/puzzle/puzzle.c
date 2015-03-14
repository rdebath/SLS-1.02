/*
 *	$XConsortium: puzzle.c,v 1.11 91/07/25 13:48:17 rws Exp $
 */

/* Puzzle - (C) Copyright 1987, 1988 Don Bennett.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation.
 */

/**
 **  Puzzle
 **
 ** Don Bennett, HP Labs
 ** 
 ** this is the code that does the real work to solve the
 ** puzzle.  (Commonly seen as a 4x4 grid of sliding pieces
 ** numbered 1-15 with one empty space.) 
 **
 ** The idea for the solution algorithm - solving the puzzle
 ** in layers working from the outside in - comes to me
 ** indirectly from John Nagle.
 **/

#include <X11/Xos.h>
#include <stdio.h>
#include <setjmp.h>

#define min(x,y)	(((x)>(y))?(x):(y))
    
#define MAX_PLAN	1000

#define LEFT	0
#define RIGHT	1
#define UP	2
#define	DOWN	3

int other_dir[4] = { RIGHT, LEFT, DOWN, UP };

/** layer info macros ->  (innermost 4 tiles are layer zero, ordinal goes up
 **			   as you move out)
 ** layer_depth		- returns number of (rows down),(cols across) the layer starts;
 ** layer_width   	- number of blocks wide the layer is;
 **/

#define layer_depth(l)	(layers-1-(l))
#define layer_width(l)	(PuzzleSize - 2*layer_depth(l))

/** macros for finding the corners of each layer **/

#define UL(l)	(layer_depth(l)*(PuzzleSize+1) + \
		 ExtraRows*PuzzleWidth + ExtraColumns*(layers-(l)))
#define UR(l)   (layer_depth(l)*(PuzzleSize+1) + layer_width(l) - 1 + \
		 ExtraRows*PuzzleWidth + ExtraColumns*(layers-(l)))
#define LL(l)   ((layer_depth(l)+layer_width(l)-1)*PuzzleSize+layer_depth(l)+ \
	     ExtraRows*PuzzleSize + ExtraColumns*(PuzzleHeight+1+(l)-layers))
#define LR(l)	((layer_depth(l)+layer_width(l)-1)*(PuzzleSize+1) + \
	     ExtraRows*PuzzleSize + ExtraColumns*(PuzzleHeight+1+(l)-layers))

/** get the x and y coordinates of a location in the matrix **/

#define get_x(loc)	((loc) % PuzzleWidth)
#define get_y(loc)	((loc) / PuzzleWidth)
#define indx(x,y)	(((y)*PuzzleWidth) + (x))

#define next_left(loc)	(loc - 1)
#define next_right(loc)	(loc + 1)
#define next_up(loc)	(loc - PuzzleWidth)
#define next_down(loc)	(loc + PuzzleWidth)

#define sign(foo)	(((foo)>0)?1:-1)

int OutputLogging = 0;

static int SolvingFlag = 0;
static int AbortSolvingFlag = 0;

static int ExtraRows = 0;
static int ExtraColumns = 0;

/** PuzzleSize MUST be a multiple of 2; **/
extern int PuzzleSize;
extern int PuzzleWidth, PuzzleHeight;

int layers;

int *tmp_matrix;
int *targetm;
int *locked;
int *loclist;
int *position;

int space_x, space_y;		/** location of space in the position matrix **/

static jmp_buf solve_env;

/**
 ** this piece of code needs to be fixed if you want to use it
 ** for non-square matrices;
 **/
print_matrix(mat)
int (*mat)[];
{
   int i,j;

   printf("\n");
   for (i=0; i<PuzzleHeight; i++) {
      for (j=0; j<PuzzleWidth; j++)
         printf(" %2d ",(*mat)[indx(j,i)]);
      printf("\n");
   }
   printf("\n");
}

find_piece(piece)
int piece;
{
   int i;

   for (i=0; i<PuzzleWidth*PuzzleHeight; i++)
      if (position[i] == piece)
         return(i);      

   printf("piece %d not found!\n",piece);
   exit(1);
}

move_space_to(loc)
int loc;
{
   int i,current_dir,dist;
   int plan[MAX_PLAN];

   plan_move(indx(space_x,space_y),loc,plan);
   current_dir = plan[1];
   dist = 0;
   for (i=1; i<=plan[0]; i++)
      if (plan[i] == current_dir)
         dist++;
      else if (plan[i] == other_dir[current_dir])
         dist--;
      else {
         move_space(current_dir,dist);
         current_dir = plan[i];
         dist = 1;
      }
   move_space(current_dir,dist);
}

move_piece(loc,targetm)
int loc,targetm;
{
   int i;
   int plan[MAX_PLAN];

   plan_move(loc,targetm,plan);
   for (i=1; i<=plan[0]; i++)
      switch(plan[i]) {
      case LEFT:	locked[loc] = 1;
			move_space_to(next_left(loc));
			locked[loc] = 0;
			move_space_to(loc);
			loc = next_left(loc);
			break;
      case RIGHT:	locked[loc] = 1;
			move_space_to(next_right(loc));
			locked[loc] = 0;
			move_space_to(loc);
			loc = next_right(loc);
			break;
      case UP:		locked[loc] = 1;
			move_space_to(next_up(loc));
			locked[loc] = 0;
			move_space_to(loc);
			loc = next_up(loc);
			break;
      case DOWN:	locked[loc] = 1;
			move_space_to(next_down(loc));
			locked[loc] = 0;
			move_space_to(loc);
			loc = next_down(loc);
			break;
      }
}

plan_move(start_loc,end_loc,path)
int start_loc, end_loc;
int (*path)[];
{

#define QUEUE_SIZE	1000
#define DIST(loc1,loc2)	(abs(get_x(loc1) - get_x(loc2)) + abs(get_y(loc1) - get_y(loc2)))

   int i, next_loc, next_dist, chosen, found_path, move_num;
   int loc_x, loc_y;
   int loc_queue[QUEUE_SIZE];
   int loc_dist[QUEUE_SIZE];
   int loc_queue_used[QUEUE_SIZE];
   int queue_head, queue_tail;
   int candidate[4];

   found_path = 0;

   for (i=0; i<PuzzleWidth*PuzzleHeight; i++) {
      tmp_matrix[i] = -locked[i];
      loclist[i] = -1;
   }

   for (i=0; i<QUEUE_SIZE; i++)
      loc_queue_used[i] = 0;

   queue_head = 0;
   queue_tail = 0;

   loc_queue[0] = start_loc;
   loc_dist[0]  = DIST(end_loc,start_loc);
   tmp_matrix[start_loc] = 1;
   queue_tail++;

   /** if the selected element has a distance of zero, we've found it;
    **  (This really isn't a queue, but rather a range of elements
    ** to be searched for an element of the desired properties;    
    **/

   /** as we search for a path, 
    ** LINK       array is used to indicate the direction from which 
    **            we moved into a location;  
    ** TMP_MATRIX array is used to keep track of the move number;
    **/

   while(queue_head < queue_tail && !found_path) {
      /** find the entry that
       ** (1) has the smallest distance and
       ** (2) has the smallest move number;
       **/

      next_loc = loc_queue[queue_head];
      next_dist = loc_dist[queue_head];
      chosen = queue_head;

      for (i=queue_head+1; i<queue_tail; i++)
         if (!loc_queue_used[i]                &&
             (   loc_dist[i] < next_dist)         ||
                 (      (loc_dist[i] == next_dist)    && 
                        (tmp_matrix[loc_queue[i]] < tmp_matrix[next_loc])
              )) {
            next_loc = loc_queue[i];
            next_dist = loc_dist[i];
            chosen = i;
         }

      if (next_dist == 0) {
         found_path = 1;
         break;
      }

      loc_queue_used[chosen] = 1;

      /********************************/
      /** permute the chosen element **/
      /********************************/

      candidate[0] = next_left(next_loc);
      candidate[1] = next_right(next_loc);
      candidate[2] = next_up(next_loc);
      candidate[3] = next_down(next_loc);

      loc_x = get_x(next_loc);
      loc_y = get_y(next_loc);
      
      if (loc_x == 0)			candidate[0] = -1;
      if (loc_x == PuzzleWidth-1)	candidate[1] = -1;
      if (loc_y == 0)			candidate[2] = -1;
      if (loc_y == PuzzleHeight-1)	candidate[3] = -1;

      move_num = tmp_matrix[next_loc] + 1;

      for (i=0; i<4; i++)
         if (candidate[i] != -1 && tmp_matrix[candidate[i]] == 0) {
            tmp_matrix[candidate[i]] = move_num;
            /** the next line works because the candidate index is  
             ** same as the direction moved to reach the candidate;
             **/
            loclist[candidate[i]] = i;
            loc_queue[queue_tail] = candidate[i];
            loc_dist[queue_tail] = DIST(end_loc,candidate[i]);
            queue_tail++;
            if (queue_tail == QUEUE_SIZE) goto broke;
         }

      /***************************************************/
      /** delete used items from the front of the queue **/
      /***************************************************/

      while(loc_queue_used[queue_head] && queue_head < queue_tail)
         queue_head++;
   }

   if (!found_path) {
      printf("couldn't find a way to move (%d,%d) to (%d,%d).\n",
              get_x(start_loc),get_y(start_loc),
              get_x(end_loc),get_y(end_loc));
#ifdef UNDEFINED
      print_matrix(position);
      printf("\n");
      print_matrix(locked);
      printf("\n");
#endif /* UNDEFINED */
      return(0);
   }

broke:   if (queue_tail == QUEUE_SIZE) {
            printf("it didn't work.\n");
            return(0);
         }

   /** copy the path we found into the path array;
    ** element 0 will contain the number of moves in the path;
    **/

   /** by the time we get there, next_loc is in the final location **/

   (*path)[0] = tmp_matrix[next_loc] - 1;
   for (i=(*path)[0]; i>0; i--) {
      (*path)[i] = loclist[next_loc];
      switch(loclist[next_loc]) {
      case LEFT:	next_loc = next_right(next_loc);
			break;
      case RIGHT:	next_loc = next_left(next_loc);
			break;
      case UP:		next_loc = next_down(next_loc);
			break;
      case DOWN:	next_loc = next_up(next_loc);
			break;
      }
   }
}

move_space(dir,dist)
int dir,dist;
{
   int i, step, count;
   int first_x,first_y;
   int last_x,last_y, shift_dir;


   if (PuzzlePending()) ProcessEvents();
   if (SolvingFlag && AbortSolvingFlag)
      longjmp(solve_env,1);

   if (dist == 0)
      return;

   if (dir == LEFT) {
      dir = RIGHT;
      dist = -dist;
   }

   if (dir == UP) {
      dir = DOWN;
      dist = -dist;
   }

   first_x = space_x;
   first_y = space_y;

   step = 1;
   count = dist;
   if (dist < 0) {
      step  = -1;
      count = -count;
   }

   /** first_x,y are the location of the first piece to be shifted **/
   if (dir == RIGHT)
      first_x += step;
   else
      first_y += step;

   /** shift_dir is the direction the pieces need to be shifted **/
   if (dist < 0)
      shift_dir = dir;
   else
      switch (dir) {
      case LEFT:  shift_dir = RIGHT; break;
      case RIGHT: shift_dir = LEFT;  break;
      case UP:    shift_dir = DOWN;  break;
      case DOWN:  shift_dir = UP;    break;
      }

   for (i=0; i<count; i++)
      if (dir == RIGHT) {
         position[indx(space_x,space_y)] = position[indx(space_x+step,space_y)];
         position[indx(space_x+step,space_y)] = 0;
         space_x += step;
      }
      /** dir == DOWN **/
      else {
         position[indx(space_x,space_y)] = position[indx(space_x,space_y+step)];
         position[indx(space_x,space_y+step)] = 0;
         space_y += step;
      }

   last_x = space_x;
   last_y = space_y;
   
   /** the blocks first_x,y through last_x,y need to be shifted
    ** one block in the shift_dir direction;
    **/

   if (OutputLogging)
     LogMoveSpace(first_x,first_y,last_x,last_y,shift_dir);
}

/* SYSV386 gets this from libBerk.a */
#if defined(USG) && !defined(CRAY) && !defined(SYSV386)
int gettimeofday (tvp, tzp)
    struct timeval *tvp;
    struct timezone *tzp;
{
    time (&tvp->tv_sec);
    tvp->tv_usec = 0L;

    /* ignore tzp for now since this file doesn't use it */
}
#endif

initialize()
{
   /** Initialize the position and
    ** the targetm matrices;
    **/

   int i;
   int sp_x, sp_y;
   struct timeval tv;

   gettimeofday (&tv, NULL);
   srand ((int) tv.tv_usec);
   layers = PuzzleSize / 2;

   ExtraRows    = PuzzleHeight - PuzzleSize;
   ExtraColumns = PuzzleWidth - PuzzleSize;

   tmp_matrix = (int *) malloc(PuzzleWidth*PuzzleHeight*sizeof(int));
   targetm     = (int *) malloc(PuzzleWidth*PuzzleHeight*sizeof(int));
   locked     = (int *) malloc(PuzzleWidth*PuzzleHeight*sizeof(int));
   loclist    = (int *) malloc(PuzzleWidth*PuzzleHeight*sizeof(int));
   position   = (int *) malloc(PuzzleWidth*PuzzleHeight*sizeof(int));

   for (i=0; i<PuzzleWidth*PuzzleHeight; i++)
      locked[i] = 0;

   if (!tmp_matrix || !targetm || !locked || !loclist || !position) {
       printf("matrix allocation failed.\n");
       exit(1);
   }

   for (i=0; i<PuzzleWidth*PuzzleHeight-1; i++) {
      targetm[i] = i+1;
      position[i] = i+1;
   }

   /** assert i == PuzzleWidth * PuzzleHeight - 1; **/
   position[i] = 0;
   targetm[i] = 0;

   space_x = PuzzleWidth - 1;
   space_y = PuzzleHeight - 1;


   /** Move the space into the LR corner of the
    ** innermost layer; 
    ** For each of the outer layers, move the space
    ** left one and up one;
    **/

   sp_x = space_x;
   sp_y = space_y;

   for (i=0; i<layers-1; i++) {
      /** move the space left one; **/
      targetm[indx(sp_x,sp_y)] = targetm[indx(sp_x-1,sp_y)];
      targetm[indx(sp_x-1,sp_y)] = 0;
      sp_x -= 1;

      /** move the space up one; **/
      targetm[indx(sp_x,sp_y)] = targetm[indx(sp_x,sp_y-1)];
      targetm[indx(sp_x,sp_y-1)] = 0;
      sp_y -= 1;
   }
}

Scramble()
{
   int i;
   int new_x, new_y;
   int old_output_state;
   struct timeval tv;

   old_output_state = OutputLogging;
   OutputLogging = 0;

   gettimeofday (&tv, NULL);
   srand ((int) tv.tv_usec);

   for (i=0; i<10*PuzzleWidth*PuzzleHeight; i++) {
      new_x = (rand() >> 3) % PuzzleWidth;
      new_y = (rand() >> 3) % PuzzleHeight;

      move_space(RIGHT,new_x-space_x);
      move_space(DOWN,new_y-space_y);
   }

   OutputLogging = old_output_state;
}

/** To solve this puzzle, work from the outside in;
 ** For each successive ring working your way in, 
 ** 
 ** (1) put the corners in place;
 ** (2) finish off the rest of the boundaries;
 ** (3) do the next layer in;
 **/

solve_layer_0()
{
   move_piece(find_piece(targetm[UL(0)]),UL(0));
   move_space_to(LR(0));   
}

do_last_two_on_edge(ntlast,last,tmp,emergency)
int ntlast,last,tmp,emergency;
{
   int last_piece, ntlast_piece;
   last_piece = targetm[last];
   ntlast_piece = targetm[ntlast];

   move_piece(find_piece(ntlast_piece),last);
   locked[last] = 1;

   /** if the last piece is stuck where the next to the last
    ** piece should go, do some magic to fix things up;
    **/
   if (find_piece(0) == ntlast)
      move_space_to(tmp);

   if (find_piece(last_piece) == ntlast) {
   /** a rescue is necessary **/
      locked[last] = 0;
      move_piece(find_piece(ntlast_piece),ntlast);
      locked[ntlast] = 1;
      move_piece(find_piece(last_piece),emergency);
      locked[emergency] = 1;
      locked[ntlast] = 0;
      move_piece(find_piece(ntlast_piece),last);
      locked[emergency] = 0;
      locked[last] = 1;
   }

   move_piece(find_piece(last_piece),tmp);
   locked[tmp] = 1;
   move_space_to(ntlast);
   locked[tmp]  = 0;
   locked[last] = 0;
   move_space_to(last);
   move_space_to(tmp);
   locked[ntlast] = 1;
   locked[last] = 1;
}

solve_layer(layer)
int layer;
{
   int i, tmp, last, ntlast, emergency;
   int ul, ur, ll, lr;


   if (layer == 0)
      solve_layer_0();
   else {
      /** find and put each of the corners into place **/      
      ul = UL(layer);      
      ur = UR(layer);
      ll = LL(layer);
      lr = LR(layer);

      move_piece(find_piece(targetm[ul]),ul);
      locked[ul] = 1;
      move_piece(find_piece(targetm[ur]),ur);
      locked[ur] = 1;
      move_piece(find_piece(targetm[ll]),ll);
      locked[ll] = 1;
      move_piece(find_piece(targetm[lr]),lr);
      locked[lr] = 1;

      /** Strategy for doing the pieces between the corners:
       ** (1) put all but the last two edge pieces in place; 
       ** (2) put the next to the last piece next to the corner;
       ** (3) put the last piece one move in from its final position;
       ** (4) move the space to the final position of the next
       **     to the last piece;
       ** (5) slide the next to the last piece over and the last 
       **     piece into the edge where it goes.
       **/

      /**************/
      /** top edge **/
      /**************/
      for (i=ul+1; i<ur-2; i++) {
         move_piece(find_piece(targetm[i]),i);
         locked[i] = 1;
      }

      ntlast = i;
      last   = i+1;
      tmp    = UR(layer-1);
      emergency = next_down(tmp);
      do_last_two_on_edge(ntlast,last,tmp,emergency);

      /*****************/
      /** bottom edge **/
      /*****************/
      for (i=ll+1; i<lr-2; i++) {
         move_piece(find_piece(targetm[i]),i);
         locked[i] = 1;
      }

      ntlast = i;
      last   = i+1;
      tmp    = LR(layer-1);
      emergency = next_up(tmp);
      do_last_two_on_edge(ntlast,last,tmp,emergency);

      /***************/
      /** left side **/
      /***************/
      for (i=ul+PuzzleWidth; i<ll-2*PuzzleWidth; i+=PuzzleWidth) {
         move_piece(find_piece(targetm[i]),i);
         locked[i] = 1;
      }

      ntlast = i;
      last   = i + PuzzleWidth;
      tmp    = LL(layer-1);
      emergency = next_right(tmp);
      do_last_two_on_edge(ntlast,last,tmp,emergency);

      /****************/
      /** right side **/
      /****************/
      for (i=ur+PuzzleWidth; i<lr-2*PuzzleWidth; i+=PuzzleWidth) {
         move_piece(find_piece(targetm[i]),i);
         locked[i] = 1;
      }

      ntlast = i;
      last   = i + PuzzleWidth;
      tmp    = LR(layer-1);
      emergency = next_left(tmp);
      do_last_two_on_edge(ntlast,last,tmp,emergency);
   }
}

solve_row(row)
int row;
{
    int i, loc, last, ntlast, tmp, emergency;

    for (i=0; i<PuzzleWidth-2; i++) {
	loc = indx(i,row);
	move_piece(find_piece(targetm[loc]),loc);
	locked[loc] = 1;
    }

    ntlast = indx(PuzzleWidth-2,row);
    last = indx(PuzzleWidth-1,row);
    tmp = last + PuzzleWidth;
    emergency = tmp + PuzzleWidth;
    do_last_two_on_edge(ntlast,last,tmp,emergency);
}

solve_col(col)
int col;
{
    int i, loc, last, ntlast, tmp, emergency;

    for (i=0; i<PuzzleHeight-2; i++) {
	loc = indx(col,i);
	move_piece(find_piece(targetm[loc]),loc);
	locked[loc] = 1;
    }

    ntlast = indx(col,PuzzleHeight-2);
    last = indx(col,PuzzleHeight-1);
    tmp = last + 1;
    emergency = tmp + 1;
    do_last_two_on_edge(ntlast,last,tmp,emergency);
}

AbortSolving()
{
   if (SolvingFlag) 
      AbortSolvingFlag = 1;
}

SolvingStatus()
{
    return(SolvingFlag);
}

Solve()
{
   /** determine the position we want to be in when
    ** we are done; This position will have the space in 
    ** the center;  Then, we'll move the space back to 
    ** the outside.
    **/

   int i;  


   if (SolvingFlag)
      return;

   if (!setjmp(solve_env)) {
      SolvingFlag = 1;

      for (i=0; i<PuzzleWidth*PuzzleHeight; i++)
         locked[i] = 0;

      /** solve the extra rows and cols **/
      for (i=0; i<ExtraRows; i++)
	  solve_row(i);

      for (i=0; i<ExtraColumns; i++)
	  solve_col(i);

      /** solve each layer **/
      for (i=layers-1; i>=0; i--)
         solve_layer(i);

      /** move the space back out to the LR corner; **/
      /** i is the layer the space is moving into **/
      for (i=1; i<layers; i++) {
         move_space(DOWN,1);
         move_space(RIGHT,1);
      }
      flushLogging();
   }
   else {
      flushLogging();
      RepaintTiles();
   }

   for (i=0; i<PuzzleWidth*PuzzleHeight; i++)
      locked[i] = 0;

   AbortSolvingFlag = 0;
   SolvingFlag = 0;
}

#ifdef UNDEFINED
main()
{
#ifdef DEBUG
   int plan[1000];
   int i;
#endif /* DEBUG */

   initialize();

#ifdef DEBUG
   print_matrix(position);
#endif /* DEBUG */

   scramble();

#ifdef DEBUG
   print_matrix(position);

#ifdef UDEFINED
   locked[indx(4,3)] = 1;
   locked[indx(4,2)] = 1;
   locked[indx(4,1)] = 1;
   locked[indx(5,2)] = 1;

   plan_move(indx(space_x,space_y),indx(5,1),plan);
   print_matrix(tmp_matrix);
   printf("\nplan has %d moves.\n",plan[0]);
   for (i=0; i<plan[0]; i++) {
      switch(plan[i+1]) {
      case UP:    printf("up\n");
                  break;
      case DOWN:  printf("down\n");
                  break;
      case LEFT:  printf("left\n");
                  break;
      case RIGHT: printf("right\n");
                  break;
      }
   }
#endif /* UDEFINED */
#endif /* DEBUG */

   solve();
}
#endif /* UNDEFINED */
