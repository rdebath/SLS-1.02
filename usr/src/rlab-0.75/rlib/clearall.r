//-------------------------------------------------------------------//

//  Syntax:	clearall ( )

//  Description:

//  The function clearall, clears all data objects from the workspace.
//  Scalars, strings, matrices, and lists are cleared with the clear
//  function. User function are not affected by clearall. If you wish
//  to remove user functions you must do so explicitly with clear.
//

//  See Also: clear
//-------------------------------------------------------------------//


clearall = function ( )
{
  local (i);

  for (i in members ($$))
  {
    if (class ($$.[i]) != "built-in" && class ($$.[i]) != "user-function")
    {
      clear ($$.[i]);
    }
  }
};
