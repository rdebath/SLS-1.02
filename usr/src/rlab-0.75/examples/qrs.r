//
//  Syntax: qrs ( A )

//  Description:

//  Qrs stands for QRSimple. This is a simple QR decomposition that
//  employs Householder transforms to generate the upper-triangular R.
//  Q, as returned, is the product of the separate Houselholder
//  transforms.

//  This function is VERY INEFFICIENT. All of the intermediate matrix
//  products are saved. For educational purposes only.
//

//  Dependencies

rfile house

qrs = function( A )
{
  local(a, i, j, l, m, n, p, q, v);

  m = A.nr; n = A.nc;
  if( m >= n ) {
    l = n;
  else
    l = n - m;
  }

  // 1st transformation
  v.[1] = house.v( A[;1] );
  p.[1] = P ( v.[1] );
  a.[1] = p.[1]*A;

  q = p.[1];

  for ( i in 2:l )
  {
    v.[i] = house.v( a.[i-1][i:a.[i-1].nr;i] );
    p.[i] = P ( v.[i] );

    p.[i] = [eye(i-1,i-1),zeros(i-1,p.[i].nc);zeros(p.[i].nr,i-1),p.[i]];
    a.[i] = p.[i]*a.[i-1];

    q = p.[i]*q;
  }

  return << q = q ; r = a.[i] >>;
}
