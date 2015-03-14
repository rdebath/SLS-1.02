// FFT -- perform MATLAB-like FFT.
//
//        This routine uses the built-in fft function to calculate the
//        DFT of a matrix. The FFT is performed on each column separately.
//        F = FFT(X) returns the un-normalized FFT (unlike the built-in function
//        which divides by N). FFT(X,N) pads or truncates the column vectors
//        to length N.

FFT = function(M_,dim) {
  local(F,col,M);

  if (class(M_) != "matrix") { error("FFT: Argument must be a matrix"); }

  M = M_;
  if (name(dim) != "dummy") {
    if (class(dim) != "scalar") { error("FFT: Dimension must be a scalar"); }
    if (dim < M.nr) {
      M = M[1:dim;];
    else if (dim > M.nr) {
      M = [M; zeros(dim-M.nr,M.nc)];
    }}
  }
      
  F = fft(M);
  return M.nr*F;
}

IFFT = function(M_,dim) {
  local(F,col,M);

  if (class(M_) != "matrix") { error("IFFT: Argument must be a matrix"); }

  M = M_;
  if (name(dim) != "dummy") {
    if (class(dim) != "scalar") { error("IFFT: Dimension must be a scalar"); }
    if (dim < M.nr) {
      M = M[1:dim;];
    else if (dim > M.nr) {
      M = [M; zeros(dim-M.nr,M.nc)];
    }}
  }

  F = ifft(M);
  return F/M.nr;
}
