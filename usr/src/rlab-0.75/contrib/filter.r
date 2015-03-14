//FILTER Digital filter.
//       Y = FILTER(B, A, X) filters the data in vector X with the
//       filter described by vectors A and B to create the filtered
//       data Y.  The filter is a "Direct Form II Transposed"
//       implementation of the standard difference equation:
//
//       a(1)*y(n) = b(1)*x(n) + b(2)*x(n-1) + ... + b(nb+1)*x(n-nb)
//                        - a(2)*y(n-1) - ... - a(na+1)*y(n-na)

// dependencies

rfile is
rfile fliplr

filter = function(a_,b_,x_) {
  local(y,M,N,outlength,ix,a,b,x,column)

  a = a_; b = b_; x = x_;
  if (!isvector(b) || !isvector(a) || !isvector(x)) {
    error("filter: Inputs must be vectors");
  }

  M = length(a); N = length(b);
  // Output may be infinite length (could be IIR) so we arbitrarily decide that
  // the output will have the same length as the input x.
  outlength = length(x);
  
  if (M < outlength) {  // Pad a if necessary
    a[outlength] = 0;
  }
  if (N < outlength) {  // Pad b if necessary
    b[outlength] = 0;
  }
  if (length(x) < outlength) { // Pad x if necessary
    x[outlength] = 0;
  }

  // We want the output to have the same shape as the input (row for row input
  // and column for column input). However, for easier processing, we force
  // data to be column vectors and coefficients to be row vectors. Then, we
  // we have to flip a,b since we're really doing a convolution.
  y = zeros(outlength,1);
  a = reshape(a,1,length(a)); a = fliplr(a);
  b = reshape(b,1,length(b)); b = fliplr(b);
  if (x.nr == 1) {
    column = 0;      // Indicate that output is a row vector
    x = x[:];
  else
    column = 1;      // Indicate that output is a column vector
  }

  y[1] = (a[outlength]/b[outlength])*x[1];
  if (outlength == 1) { return y; }

  for (ix in 2:outlength) {
    y[ix] = (-(b[outlength-ix+1:outlength-1]*y[1:ix-1]) + ...
             (a[outlength-ix+1:outlength]*x[1:ix])) / b[outlength];
  }

  if (column == 0) {
    y = reshape(y,1,length(y));
  }
  return y;
}
