//-------------------------------------------------------------------

// Syntax:	redit ( F , editor )

// Description:

// The redit function is a convenient way to edit an rfile, and have
// it automatically re-loaded upon exiting the editor. The argument F
// is a string, and is the name of the rfile (including the extension).

// The file must be in the present working directory to function
// properly.

// The second string argument is optional, and specifies an editor
// other than the default (vi).
//------------------------------------------------------------------

redit = function ( file , editor )
{
  local (ED, r);

  if (class (file) != "string") 
  {
    error ("1st argument to edit() must be string");
  }
  if (name (editor) == "dummy")
  {
    ED = "vi";
  else
    if (class (editor) != "string")
    {
      error ("2nd argument ot edit() must be string");
    }
    ED = editor;
  }

  if (!system ("test -r " + file))
  {
    system (ED + " " + file);
    r = load (file);
    printf ("edited and loaded rfile: %s\n", file);
    return r;
  else
    printf ("cannot read file: %s\n", file);
    return 0;
  } 
};

