//
// HOUSE   Householder matrix.
//         If [v, beta] = HOUSE(x) then H = EYE - beta*v*v' is a Householder
//         matrix such that Hx = -sign(x(1))*norm(x)*e_1.
//         NB: If x = 0 then v = 0, beta = 1 is returned.
//             x can be real or complex.
//             sign(x) := exp(i*arg(x)) ( = x./abs(x) when x ~= 0).

//         Theory: (textbook references Golub & Van Loan 1983, 38-43;
//                  Stewart 1973, 231-234, 262; Wilkinson 1965, 48-50).
//         Hx = y: (I - beta*v*v')x = -s*e_1.
//         Must have |s| = norm(x), v = x+s*e_1, and
//         x'y = x'Hx =(x'Hx)' real => arg(s) = arg(x(1)).
//         So take s = sign(x(1))*norm(x) (which avoids cancellation).
//         v'v = (x(1)+s)^2 + x(2)^2 + ... + x(n)^2
//             = 2*norm(x)*(norm(x) + |x(1)|).

house = function( x )
{
  local(beta, m, n, s, v);
  m = size(x)[1]; n = size(x)[2];
  if( n > 1 ) { error("Argument must be a column vector."); }

  s = norm(x, "2") * (sign(x[1]) + (x[1]==0));    // Modification for sign(0)=1.
  v = x;
  if( s == 0 ) { 
    beta = 1; 
    return beta;                                  // Quit if x is the zero vector.
  }
  v[1] = v[1] + s;
  beta = 1/(s'*v[1]);                             // NB the conjugated s.

  //  beta = 1/(abs(s)*(abs(s)+abs(x(1)))) would guarantee beta real.
  //  But beta as above can be non-real (due to rounding) only when x is complex.

  return << v = v ; beta = beta >>;
}
