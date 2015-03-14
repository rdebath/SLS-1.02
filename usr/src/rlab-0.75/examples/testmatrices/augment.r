//
// AUGMENT  AUGMENT(A) is the square matrix [EYE(m) A; A' ZEROS(n)] of dimension
//          m+n, where A is m-by-n.  It is the symmetric and indefinite
//          coefficient matrix of the augmented system associated with a least
//          squares problem minimize NORM(A*x-b).
//
//          Special case: if A is a scalar, n say, then AUGMENT(A) is the same
//                        as AUGMENT(RAND(p,q)) where n = p+q and p = ROUND(n/2),
//                        that is, a random augmented matrix of dimension n is
//                        produced.

//    Reference:  G.H. Golub and C.F. Van Loan, Matrix Computations, Second
//    Edition, Johns Hopkins University Press, Baltimore, Maryland, 1989, p. 253.


augment = function( A )
{
  local( a , m , n , p, q );

  a = A;
  m = size(A)[1]; n = size(A)[2];

  if( max( [m,n] ) == 1 )
  {
    n = a;
    p = round(n/2);
    q = n - p;
    a = rand(p,q);
    m = p; n = q;
  }

  return [ eye(m,m) , a ; a' , zeros(n,n) ];
}
