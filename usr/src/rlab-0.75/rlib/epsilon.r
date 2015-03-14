//
//  Compute machine epsilon
//

epsilon = function() 
{
  local(eps);

  eps = 1.0;
  while((eps + 1.0) != 1.0) 
  {
    eps = eps/2.0;
  }
  return eps;
};
