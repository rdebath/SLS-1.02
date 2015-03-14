//-------------------------------------------------------------------//
//
//  Syntax:	rank ( A )
//		rank ( A , tol )

//  Description:

//  Compute the rank of the matrix A. Rank returns the number of
//  values that are larger than max( size(x) ) * norm(x,"2") * eps. If
//  the user specifies tol, the the number of singular values larger
//  than tol is returned.

//-------------------------------------------------------------------//

rank = function(x, tol)
{
  local(s);
  s = svd(x);
  if( tol == 0 ) 
    { 
      tol = max(size(x)) * norm(x,"2") * epsilon();
    }
  return sum(s.sigma > tol);
};
