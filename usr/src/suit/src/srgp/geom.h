/**   G E O M E T R I C    A R I T H M E T I C    **/

/* Adds two points, returning the result */
srgp__point GEOM_sumOfPoints (srgp__point pt1, srgp__point pt2);

/* Computes one of the dimensions of a given rectangle */
int GEOM_widthOfRect (srgp__rectangle r);
int GEOM_heightOfRect (srgp__rectangle r);

/* Computes rectangle result of binary operation on two rects */
void GEOM_computeRectUnion (srgp__rectangle r1, srgp__rectangle r2, 
			    srgp__rectangle *result);
int  GEOM_computeRectIntersection (srgp__rectangle r1, srgp__rectangle r2, 
				   srgp__rectangle *result);
        /* RETURNS BOOLEAN: is there a non-null intersection? */

/* Returns the rectangle whose center is the same as the center of
 * the given rectangle R and whose dimensions are as given.
 */
srgp__rectangle GEOM_rectWithCommonCenter 
   (srgp__rectangle r, int width, int height);

/* Returns the rectangle one of whose diagonals is specified by the
 * two given points.  Rectangles are defined by the SW-NE diagonal.
 * This procedure is useful when the caller "has" a diagonal but
 * knows not which it is.
 */
srgp__rectangle GEOM_rectFromDiagPoints (srgp__point pt1, srgp__point pt2);

/* Returns TRUE if and only if the given point is on the boundary of
 * or in the interior of the given rectangle.
 */
int GEOM_ptInRect (srgp__point pt, srgp__rectangle rect);
