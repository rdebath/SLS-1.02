//
// Syntax:	compan ( P )

// Description:

// Compan(P), where P is an (n+1)-vector, is the n-by-n companion
// matrix whose first row is -P(2:n+1)/P(1). Special case: if P is a
// scalar then compan(P) is the P-by-P matrix compan(1:P+1).

//  References:
//  J.H. Wilkinson, The Algebraic Eigenvalue Problem, Oxford University Press,
//       1965, p. 12.
//  C. Kenney and A.J. Laub, Controllability and stability radii for companion
//       form systems, Math. Control Signals Systems, 1 (1988), pp. 239-256.
//       (Gives explicit formulas for the singular values of COMPAN(P).)
//

compan = function( P )
{
  local(A, n, p);

  n = max(size(P));

  // Handle scalar P.
  if( n == 1 )
  {
    n = P+1;
    p = 1:n;
  else
    p = reshape( P, 1, prod(size(P)) );	// Ensure p is a row vector.
  }

  // Construct matrix of order n-1.
  if( n == 2 )
  {
    A = 1;
  else
    A = diag(ones(1,n-2),-1);
    A[1;] = -p[2:n]/p[1];
  }

  return A;
}
