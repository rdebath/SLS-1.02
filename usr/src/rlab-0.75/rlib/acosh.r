//-------------------------------------------------------------------//

//  Syntax:  acosh ( X )

//  Description:

//  Acosh is the inverse hyperbolic cosine of the element(s) of X.

//-------------------------------------------------------------------//

acosh = function ( x )
{
  return log (x + sqrt (x.^2 - 1));
};
