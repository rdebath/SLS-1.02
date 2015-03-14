//-------------------------------------------------------------------//
//
// Syntax:	gamma ( A )

// Description:

// Computes the value of the gamma function. The input, A must be a
// row, or column vector. Allowable values of A are: A > 0.

// See Also: gammaln
//-------------------------------------------------------------------//

gamma = function( A )
{
  return exp( gammaln( A ) );
};

//-------------------------------------------------------------------//
//
// Syntax:	gammaln ( A )

// Description:

// Computes the value of the log-gamma function. The input, A must be
// a row, or column vector. Allowable values of A are: A > 0.

// Reference: Numerical Recipes in C, pp 168.
//-------------------------------------------------------------------//

gammaln = function( A )
{
  local(a, cof, j, ser, tmp);

  if( !all( A >= 0 ) )	// Test for correct input range
  { 
    return inf(); 
  }

  if( min(size(A)) != 1 )
  {
    error("input to gamma() must be a row, or column matrix");
  }

  cof = [76.18009173, -86.50532033, 24.01409822, ...
         -1.231739516, 0.120858003e-2, -0.536382e-5];

  a = A - 1;
  tmp = a + 5.5;
  tmp = tmp - (a + 0.5) .* log(tmp);
  ser = 1;
  for( j in 1:6 )
  {
    a = a + 1;
    ser = ser + cof[j]./a;
  }
  return ( -tmp + log(2.50662827465*ser) );
};

//-------------------------------------------------------------------//
//
// Syntax:	gammp ( A )

// Description:

// Computes the value of the incomplete gamma function, P(a,x).
// Currently the input, A, must be a row, or column matrix.

// Reference: Numerical Recipes in C, pp 172.
//-------------------------------------------------------------------//

gammp = function(A, X)
{
  local(gammcf, gammser, gln);

  if( any(X < 0) || any(A <= 0) ) {
    error("invalid arguments to gammp()");
  }

  if( any( X < (A + 1) ) )
  {
    return gser( A, X, gln );
  else
    return 1 - gcf( A, X, gln );
  }
};

//-------------------------------------------------------------------//
//
// Syntax:	gammq ( A )

// Description:

// Computes the value of the incomplete gamma function, 
// Q(a,x) = 1 - P(a,x). 
// Currently the input, A, must be a row, or column matrix.

// Reference: Numerical Recipes in C, pp 173.
//-------------------------------------------------------------------//

gammq = function(A, X)
{
  local(gammcf, gammser, gln);

  if( any(X < 0) || any(A <= 0) ) {
    error("invalid arguments to gammq()");
  }

  if( any( X < (A + 1) ) )
  {
    return 1 - gser( A, X, gln );
  else
    return gcf( A, X, gln );
  }
};

//-------------------------------------------------------------------//
//
// Syntax:	gser ( A )

// Description:

// Computes the value of the incomplete gamma function, evaluated by
// it's series representation.

// Reference: Numerical Recipes in C, pp 173.
//-------------------------------------------------------------------//

gser = function( A, X, GLN )
{
  local(ITMAX, ap, del, eps, n, sum);

  GLN = gammaln(A);
  if( any( X <= 0 ) )
  {
    if( any( X < 0 ) ) {
      error("X less than 0 in function gser()");
    }
    return 0;
  else
    ITMAX = 100; eps = epsilon();
    ap = A;
    del = sum = 1./A;
    for( n in 1:ITMAX )
    {
      ap = ap + 1;
      del = del .* X./ap;
      sum = sum + del;
      if( all( abs(del) < abs(sum).*eps ) )
      {
        return sum.*exp(-X + A.*log(X) - GLN);
      }
    }
    error("A too large, ITMAX too small in function gser()");
    return 0;
  }
};

//-------------------------------------------------------------------//
//
// Syntax:	gcf ( A )

// Description:

// Computes the value of the incomplete gamma function, evaluated by
// it's continued fraction representation.

// Reference: Numerical Recipes in C, pp 174.
//-------------------------------------------------------------------//

gcf = function( A, X, GLN )
{
  local(ITMAX, a0, a1, an, ana, anf, b0, b1, eps, fac, g, gold, n);

  ITMAX = 100;
  a0 = 1; b0 = 0; b1 = 1; fac = 1; gold = 0;
  eps = epsilon();
  GLN = gammaln(A);
  a1 = X;
  for( n in 1:ITMAX )
  {
    an = n;
    ana = an - A;
    a0 = (a1 + a0.*ana).*fac;
    b0 = (b1 + b0.*ana).*fac;
    anf = an.*fac;
    a1 = X.*a0 + anf.*a1;
    b1 = X.*b0 + anf.*b1;
    if( all(a1) )
    {
      fac = 1./a1;
      g = b1.*fac;
      if( abs((g - gold)/g) < eps )
      {
        return exp(-X + A.*log(X) - GLN).*g;
      }
    gold = g;
    }
  }
  error("A too large, ITMAX too small in function gcf()");
};
