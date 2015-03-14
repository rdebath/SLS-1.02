#include <stdio.h>
#include "global.h"
#include "line.h"

void
set_line (line, string)
     struct line *line;
     char *string;
{
  int len;

  len = strlen (string);
  if (line->alloc <= len)
    {
      if (len < LINE_MIN)
	len = LINE_MIN;
      else
	len++;
      line->alloc = len + 1;
      if (line->buf)
	line->buf = ck_realloc (line->buf, line->alloc);
      else
	line->buf = ck_malloc (line->alloc);
    }
  strcpy (line->buf, string);
}

#ifdef __STDC__
void
sprint_line (struct line *line, char * fmt, ...)
#else
void
sprint_line (line, fmt, va_alist)
     struct line *line;
     char *fmt;
     va_dcl
#endif
{
  va_list iggy;
  int len;

  len = strlen (fmt) + 200;
  if (!line->alloc)
    {
      line->buf = ck_malloc (len);
      line->alloc = len;
    }
  else if (line->alloc < len)
    {
      line->buf = ck_realloc (line->buf, len);
      line->alloc = len;
    }
  var_start (iggy, fmt);
  vsprintf (line->buf, fmt, iggy);
  va_end (iggy);
}
