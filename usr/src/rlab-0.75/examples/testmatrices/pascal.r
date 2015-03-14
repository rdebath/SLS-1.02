//
//PASCAL  PASCAL(N) is the Pascal matrix of order N: a symmetric positive
//        definite matrix with integer entries, made up from Pascal's
//        triangle.  Its inverse has integer entries.
//        [Conjecture by NJH: the Pascal matrix is totally positive.]
//        PASCAL(N,1) is the lower triangular Cholesky factor (up to signs
//        of columns) of the Pascal matrix.   It is involutary (is its own
//        inverse).
//        PASCAL(N,2) is a transposed and permuted version of PASCAL(N,1)
//        which is a cube root of the identity.

//    J. Todd, Basic Numerical Mathematics, Vol. 2: Numerical Algebra,
//    Birkhauser, Basel, and Academic Press, New York, 1977, p. 172.

pascal = function(n, k)
{
  local(P);

  if( name(k) == "dummy" ) { k = 0; }

  P = diag( (-1).^[0:n-1] );
  P[;1] = ones(n,1);

  // Generate the Pascal Cholesky factor (up to signs).
  for( j in 2:n-1 )
  {
    for( i in j+1:n )
    {
      P[i;j] = P[i-1;j] - P[i-1;j-1];
    }
  }

  if( k == 0 ) 
  {
    P = P*P';
  else if( k == 2 ) {
    P = P';
    P = P[ n:1:-1 ;];
    for( i in 1:n-1 )
    {
      P[i;] = -P[i;];
      P[;i] = -P[;i];
    }
    if( n/2 == round(n/2) ) 
    {
      P = -P;
    }
  }}

  return P;
}
