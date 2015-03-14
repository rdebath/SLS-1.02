/***************************************
*  bulboard.c --
*      A demonstration program illustrating 
*      how children are placed inside their 
*      enclosing parent widget.
*      
******************************************/

#include "suit.h"
SUIT_object board, my_clock, my_ball;

void main (int argc, char *argv[])
{
    SUIT_init(argv[0]);


    board = SUIT_createBulletinBoard("sample");		  /* 1 */   
    my_clock  = SUIT_createClock("my clock");
    my_ball   = SUIT_createBouncingBall ("my ball");


    SUIT_addChildToObject (board, my_clock);		  /* 2 */
    SUIT_addChildToObject (board, my_ball);

							  /* 3 */
    SUIT_setViewport (my_ball, VIEWPORT, 
		      SUIT_mapToParent(my_clock, 0.75, 0.5, 0.9, 0.9));
    SUIT_setViewport (my_clock,VIEWPORT, 
		      SUIT_mapToParent (my_ball, 0.1, 0.1, 0.9, 0.25));

    SUIT_createDoneButton (NULL);
    SUIT_beginStandardApplication();
}

/* 
 * --------------------------------------------------
 * NOTES
 * --------------------------------------------------
 * This routine shows how objects are added to a bulletin board:
 * 
 * 1.) CREATE THE BULLETIN BOARD and all the objects that 
 *     are to be children of the bulletin board. In this case, 
 *     a bounded value and a clock. 
 *     
 * 2.) ADD THE CHILDREN to the bulletin board.
 * 
 * 3.) SET THE VIEWPORTS of the children. 
 *     Use SUIT_setViewport() with SUIT_mapToParent(). This
 *     allows you to determine the viewports of children using GP 
 *     coordinates rather than explicit integer pixel 
 *     coordinates. This makes it much easier to layout widgets 
 *     by saying "This widget goes half way across the bulletin 
 *     board, however many pixels that is".
 *     
 *     As a crude picture, this program should give the layout:
 *			       
 *             ------------------------------------------
 *             |                 	                |	     
 *             |                 	    ----------  |
 *             |                 	    |        |  |	     
 *             |                 	    |        |  |	     
 *             |                 	    |  ball  |  |	     
 *             |                 	    |        |  |	     
 *             |            	            |        |  |	     
 *             |                 	    ----------  |	     
 *             |                 	                |	     
 *             |                 	                |	     
 *             |  ----------------------------          |
 *             |  |              	       |        |	     
 *             |  |     clock                  |        |	     
 *             |  |____________________________|        |	     
 *             |                 	                |	     
 *             -----------------------------------------
 */

