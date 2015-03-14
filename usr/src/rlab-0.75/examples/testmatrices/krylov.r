//
//KRYLOV    KRYLOV(A, x, j) is the Krylov matrix
//          [x, Ax, A^2x, ..., A^(j-1)x], where A is an n-by-n matrix and
//          x is an n-vector.
//          Defaults: x = ONES(n,1), j = n.
//          KRYLOV(n) is the same as KRYLOV(RAND(n)).

//  Reference: G.H. Golub and C.F. Van Loan, Matrix Computations, Johns Hopkins
//  University Press, Baltimore, Maryland, 1983, p. 224.

krylov = function(A, x, j)
{
  local(B, a, m, n);

  m = size(A)[1]; n = size(A)[2];

  if( n == 1 )  // Handle special case A = scalar.
  {
    n = A;
    a = rand(n,n);
  else
    a = A;
  }

  if( name(j) == "dummy" ) { j = n; }
  if( name(x) == "dummy" ) { x = ones(n,1); }

  B = ones(n,j);
  B[;1] = reshape(x, prod(size(x)), 1);
  for( i in 2:j )
  {
    B[;i] = a*B[;i-1];
  }

  return B;
}
