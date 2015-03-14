//-------------------------------------------------------------------//
//  Syntax:	funm ( A , fun )

//  Description:

//  Funm evaluates matrix functions. The 1st argument, `A' is the
//  matrix used in the evaluation, and the 2nd argument, `fun' is the
//  name of the scalar function that will be used.

//  For example:

//  next = funm ( A , exp );

//  The above will calculate the matrix exponential.

//-------------------------------------------------------------------//

funm = function ( A, fun )
{
  local (e, tmp);

  if (size (A)[1] != size (A)[2])
  {
    error ("Matrix argument to funm() must be square");
  }
  e = eig (A);
  tmp = diag (fun (e.val));
  return e.vec*tmp/e.vec;
};
