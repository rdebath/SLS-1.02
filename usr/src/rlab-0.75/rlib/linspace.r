//-------------------------------------------------------------------//
//
//  Syntax:	linspace ( s1 , s2 )
//		linspace ( s1 , s2 , n )

//  Description:

//  Linspace generates a linearly spaced vector. linspace(s1, s2)
//  generates a vector of 100 linearly equally spaced points between
//  s1 and s2. linspace(s1, s2, n) generates n points between s1 and
//  s2. 
//-------------------------------------------------------------------//


linspace = function(d1, d2, n) 
{
  if(n == 0) { n = 100; }
  if(n == 1) { error("must choose more than 1 point"); }
  return [d1+(0:n-2)*(d2-d1)/(n-1), d2];
};
