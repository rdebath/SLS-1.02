//-------------------------------------------------------------------//

//  Syntax:	tril ( A )
//		tril ( A , K )

//  Description:

//  tril(x) returns the lower triangular part of A.

//  tril(A,K) returns the elements on and below the K-th diagonal of
//  A.

//  K = 0: main diagonal
//  K > 0: above the main diag.
//  K < 0: below the main diag.

//  See Also: triu
//-------------------------------------------------------------------//

tril = function(x, k) 
{
  local(i, j, nr, nc, y);

  nr = size(x)[1]; nc = size(x)[2];
  if(abs(k) > nr) { error("invalid k value for tril()"); }
  y = zeros(nr, nc);

  for(i in max( [1,1-k] ):nr) {
    j = 1:min( [nc, i+k] );
    y[i;j] = x[i;j];
  }

  return y;
};
