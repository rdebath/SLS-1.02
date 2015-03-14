// flipud -- flip all the elements of a vector (or matrix) from top to bottom

flipud = function(A) {
  if (class(A) != "matrix") {
    error("flipud: Only matrix arguments supported");
  }

  // The obvious way to do this is painfully slow. Calling fliplr is
  // much faster.
  return fliplr(A')';
}
