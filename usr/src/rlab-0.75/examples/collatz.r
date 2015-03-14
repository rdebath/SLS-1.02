//
// The 3n + 1 problem
//

collatz = function(start)
{
  local(c, n);
  c = n = start;

  while(n > 1)
  {
    if(mod(n,2) == 0) {
      n = n/2;
    else
      n = 3*n + 1;
    }
    c = [c, n];
  }
  return c;
};

//
// Try it out
//
 c = collatz(100);
 plot( [1:c.nc; c]' );
 plot("set logscale y");
 plot( [1:c.nc; c]' );
//
