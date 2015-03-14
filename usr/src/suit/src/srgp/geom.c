#include "HEADERS.h"

#include "srgp.h"
#include "geom.h"


/*!*/
srgp__point GEOM_sumOfPoints (srgp__point pt1, srgp__point pt2)
{
   point psum;

   psum.x = pt1.x + pt2.x;
   psum.y = pt1.y + pt2.y;
   return psum;
}



/*!*/
int GEOM_widthOfRect (srgp__rectangle r)
{
   return r.top_right.x - r.bottom_left.x + 1;
}

int GEOM_heightOfRect (srgp__rectangle r)
{
   return r.top_right.y - r.bottom_left.y + 1;
}




/*!*/
static void
GEOM_calc_intersection_on_1_axis
   (int min_a, int max_a, int min_b, int max_b, 
    int *does_intersect, int *min_c, int *max_c)
{
   int min_t, max_t;

   /* FIRST, REARRANGE a && b SO THAT mina <== minb */
   if (! (min_a <= min_b)) {
      min_t = min_a;  max_t = max_a;
      min_a = min_b;  max_a = max_b;
      min_b = min_t;  max_b = max_t;
   }

   /* THEN, PERFORM INTERSECTION */
   if (min_b > max_a) 
      *does_intersect = FALSE;
   else {
      *does_intersect = TRUE;
      *min_c = min_b;
      if (max_a < max_b) 
	 *max_c = max_a;
      else
	 *max_c = max_b;
   }
}
      


/*!*/
int GEOM_computeRectIntersection 
   (srgp__rectangle r1, srgp__rectangle r2, 
    srgp__rectangle *result)
{
   int does_intersect;

   /* first: X axis */
   GEOM_calc_intersection_on_1_axis
      (r1.bottom_left.x, r1.top_right.x, r2.bottom_left.x, r2.top_right.x,
       &does_intersect, 
       &(result->bottom_left.x), &(result->top_right.x));

   if (does_intersect)
      /* then: Y axis */
      GEOM_calc_intersection_on_1_axis
	 (r1.bottom_left.y, r1.top_right.y, r2.bottom_left.y, r2.top_right.y,
	  &does_intersect, 
	  &(result->bottom_left.y), &(result->top_right.y));

   return does_intersect;
}




/*!*/
void
GEOM_computeRectUnion 
   (srgp__rectangle r1, srgp__rectangle r2, 
    srgp__rectangle *result)
{
   *result = r2;
   if (r1.bottom_left.x < result->bottom_left.x) 
      result->bottom_left.x = r1.bottom_left.x;
   if (r1.top_right.x > result->top_right.x) 
      result->top_right.x =  r1.top_right.x;
   if (r1.bottom_left.y < result->bottom_left.y) 
      result->bottom_left.y = r1.bottom_left.y;
   if (r1.top_right.y > result->top_right.y) 
      result->top_right.y =  r1.top_right.y;
}




/*!*/
srgp__rectangle GEOM_rectWithCommonCenter 
   (srgp__rectangle r, int rect_width, int rect_height)
{
   rectangle rect_with_common_center;

   rect_with_common_center.bottom_left = 
      GEOM_sumOfPoints
	 (r.bottom_left,
	  SRGP_defPoint
	     ((GEOM_widthOfRect(r) - rect_width) >> 1,
	      (GEOM_heightOfRect(r) - rect_height) >> 1));

   rect_with_common_center.top_right =
      GEOM_sumOfPoints
	 (rect_with_common_center.bottom_left,
	  SRGP_defPoint (rect_width-1, rect_height-1));

   return rect_with_common_center;
}




/*!*/
srgp__rectangle GEOM_rectFromDiagPoints (srgp__point pt1, srgp__point pt2)
{
   rectangle result;

   if (pt1.x < pt2.x) 
      {
	 result.bottom_left.x = pt1.x;
	 result.top_right.x = pt2.x;
      }
   else
      {
	 result.top_right.x = pt1.x;
	 result.bottom_left.x = pt2.x;
      }

   if (pt1.y < pt2.y) 
      {
	 result.bottom_left.y = pt1.y;
	 result.top_right.y = pt2.y;
      }
   else
      {
	 result.top_right.y = pt1.y;
	 result.bottom_left.y = pt2.y;
      }
   
   return result;
}




/*!*/
/** GEOM PT IN RECT
This function strains for efficiency by exiting as soon as a
reject can be made.  The "nicer" Pascal version (using the &&
conjunction and only one if statement) would not be efficient.
**/
int GEOM_ptInRect (srgp__point pt, srgp__rectangle r)
{
   return (pt.x >= r.bottom_left.x) &&
          (pt.x <= r.top_right.x) &&
	  (pt.y >= r.bottom_left.y) &&
	  (pt.y <= r.top_right.y);
}
