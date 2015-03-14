//
// CAUCHY  C = CAUCHY(X,Y), where X,Y are N-vectors, is the N-by-N matrix
//         with C(i,j) = 1/(X(i)+Y(j)).   By default, Y = X.
//         Special case: if X is a scalar CAUCHY(X) is the same as CAUCHY(1:X).
//         Explicit formulas are known for DET(C) (which is nonzero if X and Y
//         both have distinct elements) and the elements of INV(C).

//         Reference: D.E. Knuth, The Art of Computer Programming, Volume 1,
//         Fundamental Algorithms, Second Edition, Addison-Wesley, Reading,
//         Massachusetts, 1973, p. 36.

cauchy = function(X, Y)
{
  local(C, n, x, y);

  x = X; y = Y;
  n = max(size(x));

  //  Handle scalar x.
  if( n == 1 )
  {
    n = x;
    x = 1:n;
  }

  if( name(Y) == "dummy" ) { y = x; }

  // Ensure x and y are column vectors.
  x = reshape(x, prod(size(x)), 1);
  y = reshape(y, prod(size(y)), 1);
  if( any(size(x) != size(y))) {
    error("Parameter vectors must be of same dimension.")
  }

  C = x*ones(1,n) + ones(n,1)*conj(y');
  C = ones(n,n) ./ C;
  return C;
}
