//-------------------------------------------------------------------//
//
//  A Simple Least Squares Data Fitting Example
//

//
//  Set the random number generator to uniform distribution, with
//  lower bound = -2, and upper bound = 5.
//

  rand("uniform",-2, 5);

//  Generate random data with a linearly varying component.  The
//  linearly varying component is formed, and stored in `off'. The
//  simulated, measured data is stored in `b'.
//

  off = 0:-20:-.2;
  b = ((off + 22) + rand( size(off) ));

//
//  Generate the Data matrix, A.
//
//      | 1  t  t^2 |
//  A = | 1  t  t^2 |
//      | 1  t  t^2 |
//        .  .   .
//        .  .   .
//        .  .   .
//

  m = b.n;
  t = (1:m)/m;
  plot( [ t; b ]' );

  A = [ ones(m,1), t', (t.^2)' ];

//
//  Now use left division (least squares) to solve for `x'.
//

  x = A\b';

//
//  Create a simple function that uses the computed parameters to
//  make predictions.
//

  ls = function(t)
  {
    return x[1] + x[2]*t + x[3]*t.^2;
  }

//
//  Plot a comparison of the original data, and the computed
//  values.
//

  plot( "set title 'RLaB Least Squares Example'" );
  plot( "set xlabel 'Independent Variable'" );
  plot( "set ylabel 'Dependent Variable'" );

  plot( [ t; b; ls( t ) ]' );
