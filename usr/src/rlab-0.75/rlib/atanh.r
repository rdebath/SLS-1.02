//-------------------------------------------------------------------//

//  Syntax:  atanh ( X )

//  Description:

//  Atanh is the inverse hyperbolic tangent of the element(s) of X.

//-------------------------------------------------------------------//

atanh = function ( x )
{
  return log ((1 + x) ./ (1 - x))/2;
};
