//-------------------------------------------------------------------//
//
//  Syntax:	diag ( A )
//		diag ( A , K )
//
//  Description:

//  If the 1st argument is a 1xN MATRIX construct a diagonal matrix
//  from the input . Optionally if K (SCALAR) is specified then create
//  a matrix with the vector as the Kth diagonal.

//  If the 1st argument is a MxN MATRIX, construct a 1xN MATRIX from
//  the diagonal elements of the input matrix. Optionally if K is
//  specified return the vector from the Kth diagonal of the input
//  matrix.
//
// 	K < 0 is below the main diagonal.
//  	K > 0 is above the main diagonal.
//
//-------------------------------------------------------------------//

diag = function(x, k) 
{
  local(i, m, n, v);

  if(min(size(x)) == 1)
  {
    // Form a matrix from a vector containing
    // the diagonal elements
    n = max( size(x) ) + abs(k);
    m = zeros(n, n);

    // Always construct for the k<0 case. If
    // k > 0 we will transpose when done.
    for(i in 1:x.n) {
      m[i+abs(k); i] = x[i];
    }

    if(k <= 0) { return m; }
    if(k > 0) { return m'; }
  }

  // Extract the diagonal elements
  // and create a vector.

   n = x.nr - abs(k);
   v = zeros(n,1);

  // Use 1 method of extracting diagonal, but
  // transpose input if necessary.
  if(k > 0) { 
    m = x'; 
  else
    m = x;
  }

  for(i in 1:n) {
    v[i] = m[i+abs(k); i];
  }
  return v;
};
