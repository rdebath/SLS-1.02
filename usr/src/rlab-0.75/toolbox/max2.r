//-------------------------------------------------------------------//
//
//  Syntax:     max2 ( A, B )

//  Description:

//  Return a matrix the same size as A and B with the largest
//  elements taken from A and B.

//-------------------------------------------------------------------//

max2 = function(a,b)
{
  return (b+(a-b).*(a>b));
};
