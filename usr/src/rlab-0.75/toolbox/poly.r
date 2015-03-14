//
// poly() Characteristic polynomial.
// If X is an N by N matrix, poly(A) is a row vector with N+1 elements
// which are the coefficients of the characteristic polynomial,
// det(lambda*eye(A) - A) . 

// If X is a vector, poly(X) is a vector whose elements are the
// coefficients of the polynomial whose roots are the elements of X.
// For vectors, roots and poly are inverse functions of each other, up
// to ordering, scaling, and roundoff error.

poly = function(x) 
{
  local(c, e, ee, i, j, n);

  n = size(x);
  e = x;

  // If e is a matrix, get the eigenvalues
  if( !sum( n == 1 ) ) 
    {
      e = eig(e).val;
    }

  if( max( size(e) ) == 0 ) 
    {
      return 1;
    }

  // Strip infinities and throw away.
  for(i in 1:e.n) 
    {
      if(e[i] != (1/0)) {
	ee[i] = e[i];
      }
    }
  n = max( size(ee) );

  // Expand recursion formula
  c = [1, zeros(1,n)];

  for(j in 1:n) 
    {
      c[2:(j+1)] = c[2:(j+1)] - ee[j].*c[1:j];
    }

  // Force REAL if input X was REAL
  if(all (all (imag (x))) == 0) { c = real(c); }
  return c;
};
