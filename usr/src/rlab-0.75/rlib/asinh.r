//-------------------------------------------------------------------//

//  Syntax:  asinh ( X )

//  Description:

//  Asinh is the inverse hyperbolic sine of the element(s) of X.

//-------------------------------------------------------------------//

asinh = function ( x )
{
  return log (x + sqrt (x.^2 + 1));
};
