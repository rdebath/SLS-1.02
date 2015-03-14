//  polyval.r:
//  polyval(v, x)      Polynomial evaluation.
//  If v is a vector whose elements are the coefficients of a
//  polynomial, then polyval(v,x) is the value of the
//  polynomial evaluated at x.  If x is a vector,
//  the polynomial is evaluated at all points in x.
//

polyval = function (c, x) 
{
  local(i, m, nc, y);

  m = size(x);
  nc = max( size(c) );
  y = zeros(m);
  for(i in 1:nc) {
    y = x.*y + c[i]*ones(m);
  }

  return y;
};
