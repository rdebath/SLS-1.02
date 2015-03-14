//
// CHOP    CHOP(X, t) is the matrix obtained by rounding the elements of X
//         to t significant binary places.
//         Default is t=23, corresponding to IEEE single precision.
//

chop = function(x, t)
{
  local( c, e, m, n, p, y );
  if(name(t) == "dummy") { t = 23; }
  m = size(x)[1]; n = size(x)[2];

  // Use the representation:
  // x[i,j] = 2^e[i,j] * .d[1]d[2]...d[s] * sign(x[i,j])

  // On the next line `+(x==0)' avoids passing a zero argument to LOG, which
  // would cause a warning message to be generated.

  y = abs(x) + (x == 0);
  e = floor(log(y)./log(2) + 1);
  p = (2.*ones(m,n)).^(t-e);
  c = round(p.*x)./p;

  return c;
}
