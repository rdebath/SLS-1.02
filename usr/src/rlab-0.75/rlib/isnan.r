//-------------------------------------------------------------------//

//  Syntax:	isnan ( X )

//  Description:

//  Isnan returns a matrix the same size as X containing zeros and
//  ones. The ones locate the values in X that are NaNs.

//-------------------------------------------------------------------//

isnan = function ( X )
{
  return ! ( X == X );
};
