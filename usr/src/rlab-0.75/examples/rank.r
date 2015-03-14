//
// Compute the rank of the matrix X. Rank returns the number of
// values that are larger than max( size(x) ) * norm(x,"2") * eps
// If the user specifies TOL, the the number of singular values
// larger than TOL is returned.
//

rank = function(x,tol)
{
  s = svd(x);
  if( tol == 0 ) 
    { 
      tol = max(size(x)) * s.sigma[1] * epsilon();
    }
  return sum(s.sigma > tol);
}
