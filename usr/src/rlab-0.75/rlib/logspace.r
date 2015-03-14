//-------------------------------------------------------------------//
//
//  Syntax:	logspace ( L1 , L2 )
//		logspace ( L1 , L2 , n )

//  Description:

//  Generate a logarithmically spaced vector. logspace(L1, L2)
//  generates a vector of 50 points logarithmically spaced between
//  decades 10^L1 and 10^L2.  If L2 is pi, then the points are between
//  10^L1 and pi.  logspace(L1, L2, N) generates N points.
//
//-------------------------------------------------------------------//

logspace = function(d1, d2, n) 
{
  local( D2 );

  if(n == 0) { 
    n = 50; 
  }

  if(d2 == pi) { 
    D2 = log10(pi); 
  else
    D2 = d2;
  }

  return (10).^ [d1+(0:n-2)*(D2-d1)/(n-1), D2]; 
};
