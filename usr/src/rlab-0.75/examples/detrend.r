// detrend.r:
//  Remove a linear trend from a vector.
//  y = detrend(x) removes the best straight-line fit 
//  from the data in vector x and returns it in vector y.

detrend = function(x) 
{
  local(a, b, m, q1, qra, r, x, y, z);

  m = max(size(x));
  y = x;

  // Least Squares Solution

  a = [(1:m)./m; ones(1,m)]';
  qra = qr(a);
  q1 = qra.q[;1:size(a)[2]];
  r = qra.r[1:size(a)[2];];

  z = y*q1;
  b = solve(r, z');

  // Subtract out trend
  y = y' - a*b;

  return y';
};

//
// Simple example using above function
//

 t=0:100;
 y = t + t .* sin(t/2);
 ydt = detrend(y);
 plot( "set nologscale" );
 plot( [ [t;y;ydt]' ] );

