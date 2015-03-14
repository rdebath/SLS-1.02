//-------------------------------------------------------------------//
// 
//  Syntax:	eye ( S1 , S2 )
//		eye ( A )
//
//  Description:

//  Create an identity matrix with number of rows and columns
//  specified by two scalars (S1 - rows, and S2 - columns).

//  If the input is A, a matrix, with two elements, then the 1st
//  element of A determines the number of rows, and the 2nd element of
//  A determines the number of columns of the new identity matrix.

//  The matrix input option exists so that:
//
//		eye( size( X ) )
//  will work.

//  See Also: ones, zeros
//-------------------------------------------------------------------//

eye = function( m , n ) 
{
  local(i, N, new);

  if (name (n) == "dummy")
  {
    if(m.n != 2) { error("only 2-el MATRIX allowed as eye() arg"); }
    new = zeros (m[1], m[2]);
    N = min ([m[1], m[2]]);
  else
    if (type (m) == "string" || type (n) == "string") {
      error ("eye(), string arguments not allowed");
    }
    if (max (size (m)) == 1 && max (size (n)) == 1)
    {
      new = zeros (m[1], n[1]);
      N = min ([m[1], n[1]]);
    else
      error ("matrix arguments to eye() must be 1x1");
    }
  }
  for(i in 1:N)
  {
    new[i;i] = 1.0;
  }
  return new;
};
