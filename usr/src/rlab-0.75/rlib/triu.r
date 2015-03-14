//-------------------------------------------------------------------//

//  Syntax:	triu ( A )
//		triu ( A , K )

//  Description:

//  triu(x) returns the upper triangular part of A.

//  tril(x; k) returns the elements on and above the k-th diagonal of
//  A. 

//  K = 0: main diagonal
//  K > 0: above the main diag.
//  K < 0: below the main diag.

//  See Also: tril
//-------------------------------------------------------------------//

triu = function(x, k) 
{
  local(i, j, nr, nc, y);

  nr = size(x)[1]; nc = size(x)[2];
  if(abs(k) > nr) { error("invalid k value for triu()"); }
  y = zeros(nr, nc);

  for(j in max( [1,1+k] ):nc) {
    i = 1:min( [nr, j-k] );
    y[i;j] = x[i;j];
  }

  return y;
};
