//
// HILB   HILB(N) is the N-by-N Hilbert matrix with elements 1/(i+j-1),
//        which is a famous example of a badly conditioned matrix.
//        See INVHILB (standard MATLAB routine) for the exact inverse, which
//        has integer entries.
//        HILB(N) is symmetric positive definite, totally positive, and a
//        Hankel matrix.

//        This routine is shorter and faster than the one supplied with MATLAB.

//        References: D.E. Knuth, The Art of Computer Programming,
//        Volume 1, Fundamental Algorithms, Second Edition, Addison-Wesley,
//        Reading, Massachusetts, 1973, p. 37.
//        M.-D. Choi, Tricks or treats with the Hilbert matrix, Amer. Math.
//        Monthly, 90 (1983), pp. 301-312.

// Dependencies

rfile cauchy

mhilb = function( n )
{
  local( H );

  if( n == 1 )
  {
    H = 1;
  else
    H = cauchy( (1:n) - .5);
  }

  return H;
}
