//-------------------------------------------------------------------//
//
//  Syntax:     rem ( A, B )

//  Description:

//  Calculate remainders.
//  `mod ( A, B )' is equivilant to `rem ( A, B)'. Mod is a builtin
//  function, and is much faster when operating on matrices. Rem is
//  provided mostly for MATLAB compatibility.

//-------------------------------------------------------------------//

rem = function(x,y) 
{
  return (x-y.*int(x./y));
};
