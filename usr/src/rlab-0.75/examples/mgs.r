//
// Modified Gram-Schmidt
// Given A (MxN), with rank(A) = N. The following algorithm computes
// the factorization A = Q*R (skinny QR) where Q (MxN) has orthonormal
// columns and R (NxN) is upper triangular
//
// From MATRIX Computations, G.H. Golub, C.F. Van Loan (page 219)
// 

mgs = function(A)
{
  local(a,k,j,n,m,q,r);

  a = A;
  m = a.nr;
  n = a.nc;
  for(k in 1:n)
    {
      r[k;k] = norm( a[1:m;k], "2");
      q[1:m;k] = a[1:m;k]/r[k;k];
      for(j in k+1:n)
	{
	  r[k;j] = q[1:m;k]' * a[1:m;j];
          a[1:m;j] = a[1:m;j] - q[1:m;k] * r[k;j];
        }
     }
  return << q = q; r = r >>;
}

