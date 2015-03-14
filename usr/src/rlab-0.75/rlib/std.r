//-------------------------------------------------------------------//

//  Syntax:	std ( A )

//  Description:

//  Compute the Standard Deviation. For a vector (row-matrix), std()
//  returns the standard deviation. For a MxN matrix, std() returns a
//  row-matrix containing the standard deviation of each column.

//  See Also: mean, rand, srand
//-------------------------------------------------------------------//

std = function(x)
{
  local(i, m, s);

  if(class(x) != "matrix") { error("std() requires MATRIX input"); }

  m = x.nr;
  if( m == 1 ) 
  { 
    return sqrt( sum( (x - mean(x)) .^ 2 ) / (x.nc - 1) );
  else
    for( i in 1:x.nc) {
      s[i] = sqrt( sum( (x[;i] - mean(x[;i])) .^ 2 ) / (x.nr - 1) );
    }
    return s;
  }
};
