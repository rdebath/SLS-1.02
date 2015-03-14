//	TOEPLITZ(C,R) is a non-symmetric Toeplitz matrix having C as its
//	first column and R as its first row.   TOEPLITZ(C) is a symmetric
//	(or Hermitian) Toeplitz matrix.  See also HANKEL.

// dependencies

rfile is
rfile fliplr
rfile flipud
rfile hankel

toeplitz = function(c_,r_) {
  local(c,r,nc,h,j,nr);

  if (class(c_) != "matrix") { error("toeplitz: Inputs must be vectors"); }
  if (!isvector(c_)) { error("toeplitz: Inputs must be vectors"); }

  c = c_[:];
  nc = length(c);

  if (name(r_) == "dummy") {
    r = c;
  else
    if (class(r_) != "matrix") { error("toeplitz: Inputs must be vectors"); }
    if (!isvector(r_)) { error("toeplitz: Inputs must be vectors"); }
    if (length(c_) != length(r_)) {
      error("toeplitz: Inputs must have the same length");
    }
    r = r_[:];
    if (c[1] != r[1]) {
      error("toeplitz: First element of row must match first element of column");
    }
  }

  r = flipud(r);
  h = zeros(nc,nc);
  h[;1] = c;
  for (j in 2:nc) {
    h[;j] = [r[nc-j+1:nc-1]; c[1:nc-j+1]];
  }
  return h;
}
