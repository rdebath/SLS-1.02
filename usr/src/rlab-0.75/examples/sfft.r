//
// Calculate Fourier transform of a finite signal (SLOWLY)
//

sfft = function ( x )
{
  local(i, k, n, X);

  if(class(x) != "matrix") 
  {
    error("input to sfft must be a matrix");
  }

  X = zeros( size(x) );

  for( k in 1:x.n )
  {
    for( n in 1:x.n )
    {
      X[k] = X[k] + x[n]*exp( (-1i*2*pi*(k-1)*(n-1))/x.n );
    }
  }

  return X;
}
