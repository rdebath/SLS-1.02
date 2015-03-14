//-------------------------------------------------------------------//
//
//  Syntax:	hilb ( N )

//  Description:

//  Generate a NxN Hilbert matrix.

//-------------------------------------------------------------------//

hilb = function(n)
{
  local(i,j,h);

  for(i in 1:n) {
    for(j in 1:n) {
      h[i;j] = 1/(i+j-1);
    }
  }
  return h;
};
