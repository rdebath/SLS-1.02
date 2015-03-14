//
// Fac.r
//

fac = function(a) 
{
  if(a <= 1) 
  {
      return 1;
  else
      return a*$self(a-1);
  }
};
