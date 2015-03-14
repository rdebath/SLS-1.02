//-------------------------------------------------------------------//

//  Syntax:	isinf ( X )

//  Description:

//  Isinf returns a matrix the same size as X containing zeros and
//  ones. The ones locate the values in X that are infinte.

//-------------------------------------------------------------------//

isinf = function ( X )
{
  return ones (size (X))*inf() == X;
};

