//-------------------------------------------------------------------//

//  Syntax:  tanh ( X )

//  Description:

//  Tanh is the hyperbolic tangent of the element(s) of X.

//-------------------------------------------------------------------//

tanh = function ( x )
{
  return (sinh (x) ./ cosh (x));
};
