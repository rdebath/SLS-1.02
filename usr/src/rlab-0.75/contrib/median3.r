// median3 -- compute the 3-point median filter. This function performs three
//            point median filtering along a vector or along the columns of
//            a matrix.

median3 = function(M) {
  local(M1,M2,M3,M4,Mout);

  if (class(M) != "matrix") { error("median3: Input must be a matrix"); }

  if (isvector(M)) {
    if (length(M) < 3) { return M; }

    M1 = M[2:length(M)-1];
    M2 = M[1:length(M)-2];
    M3 = M[3:length(M)];
    M4 = M1 - (0.5)*M2 - (0.5)*M3;
    Mout = M;
    Mout[2:length(M)-1] = (M2+M3+abs(M4+abs(M2-M3)/2)-abs(M4-abs(M2-M3)/2))/2;
    return Mout;
  }

  if (M.nr < 3) { return M; }

  M1 = M[2:M.nr-1;];
  M2 = M[1:M.nr-2;];
  M3 = M[3:M.nr;];

  M4 = M1 - (0.5)*M2 - (0.5)*M3;

  Mout = M;
  Mout[2:M.nr-1;] = (M2+M3+abs(M4+abs(M2-M3)/2)-abs(M4-abs(M2-M3)/2))/2;
  return Mout;
}
