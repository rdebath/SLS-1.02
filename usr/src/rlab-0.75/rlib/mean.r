//-------------------------------------------------------------------//
//
//  Syntax:	mean ( A )

//  Description:

//  Calculate the mean value. If the input is a 1xN, then compute the
//  mean value of all the elements. 

//  If the input is a MxN matrix the compute a row matrix of the mean
//  value of each column of the input. 
//
//-------------------------------------------------------------------//

mean = function(x)
{
  local(m);

  m = size (x)[1];
  if( m == 1 ) 
  { 
    m = size (x)[2];
  }

  return sum( x ) / m;
};
