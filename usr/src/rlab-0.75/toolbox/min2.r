//-------------------------------------------------------------------//
//
//  Syntax:     min2 ( A, B )

//  Description:

//  Return a matrix the same size as A and B with the smallest
//  elements taken from A and B.

//-------------------------------------------------------------------//

min2 = function(a,b)
{
  return (b+(a-b).*(a<b));
};
