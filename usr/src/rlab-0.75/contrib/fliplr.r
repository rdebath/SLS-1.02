// fliplr -- flip all the elements of a vector (or matrix) from left to right

fliplr = function(A) {
  local(B);
  if (class(A) != "matrix") {
    error("fliplr: Only matrix arguments supported");
  }

  B[;1:A.nc] = A[;A.nc:1:-1];
  return B;
}
