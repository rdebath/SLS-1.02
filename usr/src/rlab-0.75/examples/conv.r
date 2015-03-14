
conv = function( x , y )
{
  local( X , Y , tmp );

  X = fft( [ x , zeros(1, y.n-1) ] );
  Y = fft( [ y , zeros(1, x.n-1) ] );

  tmp = X.n .* ifft( X .* Y );
  if( type(x) == "real" && type(y) == "real") { tmp = real(tmp); }

  return tmp;
};
