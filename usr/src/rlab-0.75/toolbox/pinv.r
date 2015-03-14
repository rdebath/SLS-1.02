//-------------------------------------------------------------------//
//
//  Syntax:	pinv ( A )
//		pinv ( A , tol )

//  Description:

//  Pseudoinverse. X = PINV(A) produces a matrix X of the same
//  dimensions as A' so that A*X*A = A , X*A*X = X and AX and XA are
//  Hermitian. The computation is based on svd(A) and any singular
//  values less than a tolerance are treated as zero. The default
//  tolerance is max(size(A)) * norm(A,"2") * eps.  This tolerance may be
//  overridden with X = pinv(A,tol).  

//  See Also: rank

//-------------------------------------------------------------------//

pinv = function(A, tol)
{
  local(eps, r, S, s, X);

  // Pseudo-inverse, ignore singular values <= tol.
  // Default tol = max(size(A)) * s(1) * eps.

  s = svd(A);
  if(tol = 0) { tol = max(size(A)) * norm(A,"2") * epsilon(); }
 
  r = norm( (s.sigma > tol)' );
  if(r == 0) { 
    X = zeros( A' ); 
  else
    S = diag(ones(r,1) ./ s.sigma'[1:r]);
    X = s.vt'[;1:r] * S * s.u[;1:r]';
  }

  return X;
};
