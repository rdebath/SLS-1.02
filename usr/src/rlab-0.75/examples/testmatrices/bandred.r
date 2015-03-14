//
// BANDRED  B = BANDRED(A, KL, KU) is a matrix orthogonally equivalent to A
//          with lower bandwidth KL and upper bandwidth KU
//          (i.e. B(i,j) = 0 if i>j+KL or j>i+KU).
//          The reduction is performed using Householder transformations.
//          If KU is omitted it defaults to KL.

//          This routine is called by RANDSVD.
//          This is a `standard' reduction.  Cf. reduction to bidiagonal form
//          prior to computing the SVD.  This code is a little wasteful in that
//          it computes certain elements which are immediately set to zero!

rfile house

bandred = function(A, kl, ku)
{
  local(a, beta, flip, j, ltmp, m, n, temp, v, z);

  if( name(ku) == "dummy" ) { ku = kl; }

  if( kl == 0 && ku == 0 ) {
    error("You've asked for a diagonal matrix. In that case use the SVD!");
  }

  // Check for special case where order of left/right transformations matters.
  // Easiest approach is to work on the transpose, flipping back at the end.

  a = A;
  flip = 0;
  if( ku == 0 )
  {
    a = a';
    temp = kl; 
    kl = ku;
    ku = temp; 
    flip = 1;
  }

  m = size(a)[1]; n = size(a)[2];

  if( (z=min( [min([m,n]),max([m-kl-1,n-ku-1]) ] )) >= 1 )
  {
    for( j in 1:z )
    {
      if( j+kl+1 <= m )
      {
         ltmp = house( a[j+kl:m; j] );
         beta = ltmp.beta; v = ltmp.v;
         temp = a[j+kl:m; j:n];
         a[j+kl:m; j:n] = temp - beta*v*(v'*temp);
         a[j+kl+1:m; j] = zeros(m-j-kl,1);
      }

      if( j+ku+1 <= n )
      {
         ltmp = house( a[j; j+ku:n]' );
         beta = ltmp.beta; v = ltmp.v;
         temp = a[j:m; j+ku:n];
         a[j:m; j+ku:n] = temp - beta*(temp*v)*v';
         a[j; j+ku+1:n] = zeros(1,n-j-ku);
      }
    }
  }

  if( flip ) {
   a = a';
  }
  return a;
}

