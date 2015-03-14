//-------------------------------------------------------------------//

//  Syntax:	cumprod ( X )

//  Description:

//  Compute the cumulative product of a vector or a matrix.

//  See Also: prod

//-------------------------------------------------------------------//

cumprod = function ( x )
{
  local (i, j, m, n, new);

  m = x.nr;
  n = x.nc;

  new = zeros (m, n);
  if (min ([m, n]) == 1)
  {
    // cumsum on a vector
    new[1] = x[1];
    for (i in 2:max ([m,n]))
    {
      new[i] = x[i] * new[i-1];
    }
  else
    // cumsum on the columns of a matrix
    for (i in 1:n)
    {
      new[1;i] = x[1;i];
      for (j in 2:m)
      {
        new[j;i] = x[j;i] * new[j-1;i];
      }
    }
  }
  return (new);
};
