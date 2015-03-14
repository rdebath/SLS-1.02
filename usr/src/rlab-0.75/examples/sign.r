//
// 1st cut at sign function
// Return 1,  if element >= 0
// Return 0,  if element == 0
// Return -1, if element < 0
//
// Does not handle complex quantites like MATLAB
//

sign = function(a)
{
  local(i,r);
  
  if(class(a) == "scalar") {
    if(a >  0) { return 1; }
    if(a == 0) { return 0; }
    if(a <  0) { return -1; }
  }

  r = zeros( size(a) );
  for(i in 1:size(a)[1]) {
    for(j in 1:size(a)[2]) {
      if(a[i;j] >  0) { r[i;j] = 1; }
      if(a[i;j] == 0) { r[i;j] = 0; }
      if(a[i;j] <  0) { r[i;j] =-1; }
    }
  }

  return r;
}

      
  