//DIFF	Difference function.  If X is a vector [x(1) x(2) ... x(n)],
//	then DIFF(X) returns a vector of differences between adjacent
//	elements [x(2)-x(1)  x(3)-x(2) ... x(n)-x(n-1)].  If X is a
//	matrix, the differences are calculated down each column:
//	DIFF(X) = X(2:n,:) - X(1:n-1,:).
//	DIFF(X,n) is the n'th difference function.

diff = function ( X, k )
{
  local (i, m, n, x);

  if (name (k) == "dummy") { k = 1; }

  x = X;
  for (i in 1:k)
  {
    m = size(x)[1]; n = size(x)[2];
    if (m == 1)
    {
      x = x[2:n] - x[1:n-1];
    else
      x = x[2:m;] - x[1:m-1;];
    }
  }
  return x;
};
