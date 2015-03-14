/* make_suffix.c: unconditionally add a filename suffix.  */

#include "config.h"

/* Make the suffix of S be SUFFIX, regardless of what it was before. 
   This returns a newly allocated string.  */

string
make_suffix (s, suffix)
    string s, suffix;
{
  string new_s;
  string dot_pos = strrchr (s, '.');
  string slash_pos = strrchr (s, '/');

  if (dot_pos == NULL || dot_pos < slash_pos)
    new_s = concat3 (s, ".", suffix);
  else
    {
      unsigned past_dot_index = dot_pos + 1 - s;
      
      new_s = (string) xmalloc (past_dot_index + strlen (suffix) + 1);
      strncpy (new_s, s, dot_pos + 1 - s);
      strcpy (new_s + past_dot_index, suffix);
    }

  return new_s;
}
