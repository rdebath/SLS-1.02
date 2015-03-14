//FFT2	Two-dimensional Fast Fourier Transform.
//	FFT2(X) returns the two-dimensional Fourier transform of matrix X.
//	FFT2(X,MROWS,NCOLS) pads matrix X with zeros to size MROWS-by-NCOLS
//	before transforming.

// dependencies

rfile FFT

FFT2 = function(x_, mrows, ncols) {
  local(f);

  if (name(mrows) == "dummy") {
    f = conj(FFT( conj(FFT(x_)') )');
  else if (name(ncols) == "dummy") {
    f = conj(FFT( conj(FFT(x_,mrows)') )');
  else 
    f = conj(FFT( conj(FFT(x_,mrows)') ,ncols)');
  }}

  return f;
}

IFFT2 = function(x_, mrows, ncols) {
  local(f);

  if (name(mrows) == "dummy") {
    f = conj(IFFT( conj(IFFT(x_)') )');
  else if (name(ncols) == "dummy") {
    f = conj(IFFT( conj(IFFT(x_,mrows)') )');
  else   
    f = conj(IFFT( conj(IFFT(x_,mrows)') ,ncols)');
  }}

  return f;
}
