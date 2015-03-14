//-------------------------------------------------------------------//
//  Euclid's algorithm
//

//  Syntax:	euclid ( M , N )

//  Description:

//  Euclid's algorithm computes the greatest common divisor of two
//  integers (M, and N).
//-------------------------------------------------------------------//

euclid = function ( M , N )
{
  local( m , n , r );

  m = M;
  n = N;
  r = 1;
  while(r)
  {
    r = mod(m,n);
    if(r == 0) { break; }
    m = n;
    n = r;
  }
  return n;
};
