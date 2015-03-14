//-------------------------------------------------------------------//
//  Syntax:	cross ( A , B )
//
//  Description:

//  Given A and B (two 1-by-3 matrices), compute the vector
//  cross-product.
//-------------------------------------------------------------------//

cross = function(v1, v2) 
{
  local(i, tmp);

  if(v1.n != 3 || v2.n != 3) 
  { 
    printf("cross product requires 3-length matrices\n");
    return 0;
  }

  tmp = zeros(1,3);
  tmp[1] =  ((v1[2]*v2[3]) - (v1[3]*v2[2]));
  tmp[2] = -((v1[1]*v2[3]) - (v1[3]*v2[1]));
  tmp[3] =  ((v1[1]*v2[2]) - (v1[2]*v2[1]));

  return tmp;
};
