//-------------------------------------------------------------------//

//  Syntax:	save ( )
//		save ( FILE )

//  Description:

//  The save function writes the contents of all the workspace
//  variables to a file. The default file, if none is specified is
//  "SAVE".
//

//-------------------------------------------------------------------//

save = function ( FILE )
{
  local (i);

  if (name (FILE) == "dummy")
  {
    FILE = "SAVE";
  }
  for (i in members ($$))
  {
    if (class ($$.[i]) != "built-in" && class ($$.[i]) != "user-function")
    {
      write (FILE, $$.[i]);
    }
  }
  close (FILE);
};
