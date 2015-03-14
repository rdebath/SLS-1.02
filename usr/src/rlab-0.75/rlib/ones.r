//-------------------------------------------------------------------//

//  Syntax:	ones ( M , N )
//		ones ( A )

//  Description:

//  Create a matrix of ones. If the input is two scalars,
//  then create a matrix of 1s with dimensions NxM. 

//  If the input is a 2 element matrix, then create a matrix with row
//  and column dimensions equal to A[1] and A[2] respectively. This is
//  useful when used in conjunction with size():

//	ones( size( X ) )

//  will return a matrix of ones the same size as X.

//-------------------------------------------------------------------//


ones = function( m, n ) 
{
  local(i, N);

  if (name (n) == "dummy")
  {
    if(m.n != 2) { error("only 2-el MATRIX allowed as ones() arg"); }
    return (zeros (m[1], m[2]) + 1);
  else
    if (type (m) == "string" || type (n) == "string") 
    {
      error ("eye(), string arguments not allowed");
    }
    if (max (size (m)) == 1 && max (size (n)) == 1)
    {
      return (zeros (m[1], n[1]) + 1);
    else
      error ("matrix arguments to eye() must be 1x1");
    }
  }
};
