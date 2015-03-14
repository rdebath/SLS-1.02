//
// roots(): Find polynomial roots. 

// roots(V) computes the roots of the polynomial whose coefficients
// are the elements of the vector V. If V has N+1 components, the
// polynomial is: V(1)*X^N + ... + V(N)*X + V(N+1)
//

roots = function(c) 
{
  local(a, inz, n, nnz, r, v);

  // Make a copy
  v = c;
  n = size(v);

  if( !sum( n <= 1 ) ) {
    error("Must be a vector");
  }

  n = max( n );

  // Strip leading zeros and throw away.  Strip trailing zeros,
  // but remember them as roots at zero.

  inz = find( abs(v) );
  nnz = max( size(inz) );

  if(nnz != 0) 
    {
      v = v[ inz[1]:inz[nnz] ];
      r = zeros( 1, n - inz[nnz] );
    }

  // Polynomial roots via a companion matrix
  n = max( size(v) );
  a = diag( [ones(1,n-2)], -1);
  if(n > 1) {
    a[1;] = -v[2:n] ./ v[1];
  }

  return conj([r, eig(a).val])';
};
