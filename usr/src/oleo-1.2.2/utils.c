/*	Copyright (C) 1990, 1992, 1993 Free Software Foundation, Inc.

This file is part of Oleo, the GNU Spreadsheet.

Oleo is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Oleo is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Oleo; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <stdio.h>
#include <errno.h>
#include "sysdef.h"


/* unistd.h defines _POSIX_VERSION on POSIX.1 systems.  */
#if defined(DIRENT) || defined(_POSIX_VERSION)
#include <dirent.h>
#define NLENGTH(dirent) (strlen((dirent)->d_name))
#else /* not (DIRENT or _POSIX_VERSION) */
#define dirent direct
#define NLENGTH(dirent) ((dirent)->d_namlen)
#ifdef SYSNDIR
#include <sys/ndir.h>
#endif /* SYSNDIR */
#ifdef SYSDIR
#include <sys/dir.h>
#endif /* SYSDIR */
#ifdef NDIR
#include <ndir.h>
#endif /* NDIR */
#endif /* not (DIRENT or _POSIX_VERSION) */

#ifdef __STDC__
#define CONST const
#undef NULL
#else
#define CONST
#endif

#include <ctype.h>
#include "utils.h"

#ifndef F_OK
#define F_OK 0
#endif
#ifndef _IOSTRG
#define _IOSTRG 0
#endif

#ifdef __STDC__
extern void abort (void);
extern void *malloc (size_t);
extern void *calloc (size_t, size_t);
extern void *realloc (void *, size_t);
char *strdup (CONST char *);
#else
extern void abort ();
extern void *malloc ();
extern void *calloc ();
extern void *realloc ();
char *strdup ();
#endif

extern int sys_nerr;
extern char *sys_errlist[];

struct id
  {
    int flag;
    FILE *fp;
    char *name;
  };

struct id *__id_s;
int __id_n;
int __id_f;

int __make_backups;
int __backup_by_copying;

/* Stash argv[0] here so panic will know what the program is called */
char *argv_name = 0;

/* Blow chunks! */
#ifdef __STDC__
void
panic (const char *s,...)
#else
void
panic (s, va_alist)
     char *s;
     va_dcl
#endif
{
  va_list iggy;

  var_start (iggy, s);
  if (argv_name)
    fprintf (stderr, "%s:", argv_name);
  vfprintf (stderr, s, iggy);
  putc ('\n', stderr);
  va_end (iggy);
  exit (2);
}

/* Given a file name, come up with a backup file name. . . */
char *
backup_file_name (file_name)
     char *file_name;
{
  char *dir_name, *dir_end;

  DIR *dir;
  register struct dirent *dp;
  int len;
  int max_fnum;
  int cur_fnum;

  char *tmp_ptr;

  char *return_value;

  dir_end = (char *)rindex (file_name, '/');
  if (dir_end)
    {
      dir_name = file_name;
      file_name = dir_end + 1;
      *dir_end = '\0';
    }
  else
    {
      dir_name = ".";
    }
  len = strlen (file_name);

  dir = opendir (dir_name);
  if (dir == 0)
    {
      if (dir_end)
	*dir_end = '/';
      return (char *) 0;
    }

  max_fnum = 0;
  while (dp = readdir (dir))
    {
      if (!dp->d_ino
	  || NLENGTH (dp) <= len
	  || strncmp (dp->d_name, file_name, len)
	  || dp->d_name[len] != '.'
	  || dp->d_name[len + 1] != '~'
	  || dp->d_name[NLENGTH(dp) - 1] != '~')
	continue;

      tmp_ptr = &(dp->d_name[len + 2]);
      for (cur_fnum = 0; isdigit (*tmp_ptr); tmp_ptr++)
	cur_fnum = cur_fnum * 10 + *tmp_ptr - '0';
      if (tmp_ptr != &(dp->d_name[NLENGTH(dp) - 1]) || cur_fnum < max_fnum)
	continue;
      max_fnum = cur_fnum;
    }
  closedir (dir);
  max_fnum++;
  return_value = (char *) malloc (strlen (dir_name) + len + 12);
  if (!return_value)
    return (char *) 0;
  sprintf (return_value, "%s/%s.~%d~", dir_name, file_name, max_fnum);
  if (dir_end)
    *dir_end = '/';
  return return_value;
}


char *
__fp_name (fp)
     FILE *fp;
{
  int n;

  for (n = 0; n < __id_n; n++)
    {
      if (__id_s[n].fp == fp)
	return __id_s[n].name;
    }
  return "{Unknown file pointer}";
}

void
__set_fp (fp, name, flag)
     FILE *fp;
     CONST char *name;
     int flag;
{
  if (__id_s == 0)
    {
      __id_s = ck_malloc (20 * sizeof (struct id));
      __id_n = 0;
      __id_f = 20;
    }
  else
    {
      int n;

      for (n = 0; n < __id_n; n++)
	if (__id_s[n].fp == fp)
	  {
	    free (__id_s[n].name);
	    __id_s[n] = __id_s[--__id_n];
	    __id_f++;
	    break;
	  }
    }
  if (__id_f == 0)
    {
      __id_f = 20;
      __id_s = ck_realloc (__id_s, (__id_f + __id_n) * sizeof (struct id));
    }
  __id_s[__id_n].flag = flag;
  __id_s[__id_n].name = strdup (name);
  __id_s[__id_n].fp = fp;
  __id_n++;
  __id_f--;
}

/* Open a file or a pipe */
FILE *
xopen (name, mode)
     CONST char *name;
     CONST char *mode;
{
  int flag = 0;
  FILE *ret;

  while (*name == ' ')
    name++;
  if (*name == '!')
    {
      name++;
      ret = popen (name, mode);
      flag = 1;
    }
  else
    ret = fopen (name, mode);
  if (ret == 0)
    return ret;
  __set_fp (ret, name, flag);
  return ret;
}

/* Open a file, creating a backup file if needed. . . */
FILE *
fopen_with_backup (name, mode)
     char *name;
     CONST char *mode;
{
  char *newname;

  if (__make_backups && *mode == 'w' && access (name, F_OK) == 0)
    {
      newname = backup_file_name (name);
      if (!newname)
	return (FILE *) 0;
      if (__backup_by_copying)
	{
	  FILE *c_in, *c_out;
	  int n_read;
#ifdef __TURBOC__
	  char buf[512];
#else
	  char buf[4096];
#endif
	  c_in = fopen (name, "r");
	  c_out = fopen (newname, "w");
	  if (!c_in || !c_out)
	    return (FILE *) 0;
	  while ((n_read = fread (buf, 1, sizeof (buf), c_in)) > 0)
	    if (fwrite (buf, 1, n_read, c_out) != n_read)
	      return (FILE *) 0;
	  if (fclose (c_in) == EOF || fclose (c_out) == EOF)
	    return (FILE *) 0;
	}
      else
#if defined(HAVE_RENAME)
      if (rename (name, newname) < 0)
#else
      if (link (name, newname) || unlink (name))
#endif
	return (FILE *) 0;
      free (newname);
    }
  return fopen (name, mode);
}

/* Open a file or a pipe, creating a backup file if it's a file */
FILE *
xopen_with_backup (name, mode)
     CONST char *name;
     CONST char *mode;
{
  int flag;
  FILE *ret;

  while (*name == ' ')
    name++;
  if (*name == '|')
    {
      ret = popen (name + 1, mode);
      flag = 1;
    }
  else
    {
      ret = fopen_with_backup (name, mode);
      flag = 0;
    }
  if (ret == 0)
    return ret;
  __set_fp (ret, name, flag);
  return ret;
}

/* Close something opened with xopen. . . */
int
xclose (fp)
     FILE *fp;
{
  int ret;
  int n;

  for (n = 0; n < __id_n; n++)
    {
      if (__id_s[n].fp == fp)
	break;
    }
  if (n == __id_n)
    panic ("Unknown file pointer %p given to xclose", fp);
  if (__id_s[n].flag)
    ret = pclose (fp);
  else
    ret = fclose (fp);
  return ret;
}

/* Fclose or panic */
void
ck_fclose (stream)
     FILE *stream;
{
  if (fclose (stream) == EOF)
    panic ("Couldn't close %s", __fp_name (stream));
}

/* fopen or panic */
void *
ck_malloc (size)
     size_t size;
{
  void *ret;

  ret = malloc (size);
  if (ret == (void *) 0)
    panic ("Couldn't allocate %u bytes", size);
  return ret;
}

char *
ck_savestr (str)
     char *str;
{
  char *newstr = 0;
  if (str)
    {
      int len = strlen (str) + 1;
      newstr = (char *) ck_malloc (len);
      bcopy (str, newstr, len);
    }
  return newstr;
}

void *
ck_calloc (size)
     size_t size;
{
  void *ret;

  ret = calloc (size, 1);
  if (ret == (void *) 0)
    panic ("Couldn't allocate %u bytes", size);
  return ret;
}

/* Realloc or panic */
void *
ck_realloc (ptr, size)
     void *ptr;
     size_t size;
{
  void *ret;

  if (!ptr)
    ret = malloc (size);
  else
    ret = realloc (ptr, size);
  if (ret == (void *) 0)
    panic ("Couldn't re-allocate %u bytes from %p", size, ptr);
  return ret;
}

/* Do a sprintf into an allocated buffer. */
char *
#ifdef __STDC__
mk_sprintf (char *str,...)
{
  va_list iggy;
#ifdef __TURBOC__
  static
#endif
  char tmpbuf[1024 * 8];
  char *ret;

  va_start (iggy, str);
  vsprintf (tmpbuf, str, iggy);
  va_end (iggy);
  ret = (char *) ck_malloc (strlen (tmpbuf) + 1);
  strcpy (ret, tmpbuf);
  return ret;
}

#else

mk_sprintf (str, va_alist)
     char *str;
     va_dcl
{
  va_list iggy;
#ifdef __TURBOC__
  static
#endif
  char tmpbuf[1024 * 8];
  char *ret;

  va_start (iggy);
  vsprintf (tmpbuf, str, iggy);
  va_end (iggy);

  ret = (char *) ck_malloc (strlen (tmpbuf) + 1);
  strcpy (ret, tmpbuf);
  return ret;
}

#endif

/* Implement a variable sized LIFO stack of pointers to void */

struct stack
  {
    int allocated;
    int used;
    void **buf;
  };

#define MIN_STACK 20

void *
init_stack ()
{
  struct stack *b;

  b = (struct stack *) ck_malloc (sizeof (struct stack));
  b->allocated = MIN_STACK;
  b->used = 0;
  b->buf = (void **) ck_malloc (MIN_STACK * sizeof (void *));
  return (void *) b;
}

void
flush_stack (bb)
     void *bb;
{
  struct stack *b;

  b = (struct stack *) bb;
  free (b->buf);
  b->buf = 0;
  b->allocated = 0;
  b->used = 0;
  free (b);
}

void
push_stack (bb, add)
     void *bb;
     void *add;
{
  struct stack *b;

  b = (struct stack *) bb;
  if (b->allocated == b->used)
    {
      b->allocated *= 2;
      b->buf = (void **) ck_realloc (b->buf, b->allocated * sizeof (void *));
    }
  b->buf[(b->used)++] = add;
}

void *
pop_stack (bb)
     void *bb;
{
  struct stack *b;

  b = (struct stack *) bb;
  if (b->used == 0)
    return (void *) 0;
  return b->buf[--(b->used)];
}

int
size_stack (bb)
     void *bb;
{
  struct stack *b;

  b = (struct stack *) bb;
  return b->used;
}

#ifndef HAVE_STRDUP
char *
strdup (str)
     CONST char *str;
{
  char *ret;

  ret = (char *) ck_malloc (strlen (str) + 2);
  strcpy (ret, str);
  return ret;
}
#endif

#ifndef HAVE_STRICMP
/*
 * stricmp - compare string s1 to s2, ignoring case
 */

#ifdef __STDC__
int
stricmp (const char * s1, const char * s2)
#else
int
stricmp (s1, s2)
     CONST char *s1;
     CONST char *s2;
#endif
{
  register CONST char *scan1;
  register CONST char *scan2;
  register char chr1, chr2;

  scan1 = s1;
  scan2 = s2;
  do
    {
      chr1 = isupper (*scan1) ? tolower (*scan1) : *scan1;
      chr2 = isupper (*scan2) ? tolower (*scan2) : *scan2;
      scan1++;
      scan2++;
    }
  while (chr1 && chr1 == chr2);

  /*
	 * The following case analysis is necessary so that characters
	 * which look negative collate low against normal characters but
	 * high against the end-of-string NUL.
	 */
  if (chr1 == '\0' && chr2 == '\0')
    return 0;
  else if (chr1 == '\0')
    return -1;
  else if (chr2 == '\0')
    return 1;
  else
    return chr1 - chr2;
}
#endif

#ifndef HAVE_STRINCMP
/* strincmp - compare first N chars of strings S1 and S2 */
#ifdef __STDC__
int
strincmp (const char * s1, const char * s2, size_t n)
#else
int
strincmp (s1, s2, n)
     CONST char *s1;
     CONST char *s2;
     size_t n;
#endif
{
  register CONST char *scan1;
  register CONST char *scan2;
  register size_t count;
  register char chr1, chr2;

  scan1 = s1;
  scan2 = s2;
  count = n;
  do
    {
      chr1 = isupper (*scan1) ? tolower (*scan1) : *scan1;
      chr2 = isupper (*scan2) ? tolower (*scan2) : *scan2;
      scan1++;
      scan2++;
    }
  while (--count != 0 && chr1 && chr1 == chr2);

  /* if (count == (size_t)-1)
		return 0; */

  /*
	 * The following case analysis is necessary so that characters
	 * which look negative collate low against normal characters but
	 * high against the end-of-string NUL.
	 */
  if (chr1 == '\0' && chr2 == '\0')
    return 0;
  else if (chr1 == '\0')
    return -1;
  else if (chr2 == '\0')
    return 1;
  else
    return chr1 - chr2;
}
#endif

#ifndef HAVE_STRSTR
CONST char *
strstr (s, wanted)
     CONST char *s;
     CONST char *wanted;
{
  register CONST char *scan;
  register size_t len;
  register char firstc;

  /*
	 * The odd placement of the two tests is so "" is findable.
	 * Also, we inline the first char for speed.
	 * The ++ on scan has been moved down for optimization.
	 */
  firstc = *wanted;
  len = strlen (wanted);
  for (scan = s; *scan != firstc || strncmp (scan, wanted, len) != 0;)
    if (*scan++ == '\0')
      return (char *) 0;
  return scan;
}
#endif

#ifdef __STDC__
char *
err_msg (void)
#else
char *
err_msg ()
#endif
{
  int n;
  static char buf[80];

  n = errno;

  if (n < sys_nerr)
    return sys_errlist[n];
  sprintf (buf, "Unknown error code %d (%#x)", n, n);
  return buf;
}


/* Take a quoted string and return the character it represents */
#ifdef __STDC__
int
string_to_char (char ** ptr)
#else
int
string_to_char (ptr)
     char **ptr;
#endif
{
  char *str;
  int i;
  char c1, c2;

  str = *ptr;
  if (str[0] == '\\')
    {
      switch (str[1])
	{
	case '\\':
	  i = '\\';
	  break;
	case 'b':
	  i = '\b';
	  break;
	case 'f':
	  i = '\f';
	  break;
	case 'n':
	  i = '\n';
	  break;
	case 'r':
	  i = '\r';
	  break;
	case 't':
	  i = '\t';
	  break;
	case 'x':
	  c1 = str[2];
	  c2 = str[3];
	  if (isxdigit (c1))
	    {
	      if (isdigit (c1))
		c1 -= '0';
	      else if (isupper (c1))
		c1 -= 'A';
	      else
		c1 -= 'a';
	      if (isxdigit (c2))
		{
		  if (isdigit (c2))
		    c2 -= '0';
		  else if (isupper (c2))
		    c2 -= 'A';
		  else
		    c2 -= 'a';
		  i = c1 * 0x10 + c2;
		  str++;
		}
	      else
		i = c1;
	    }
	  else
	    i = 'x';
	  break;

	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	  if (str[2] >= '0' && str[2] <= '7')
	    {
	      if (str[3] >= '0' && str[3] <= '7')
		{
		  i = (str[1] - '0') * 0100 + (str[2] - '0') * 010 + (str[3] - '0');
		  str += 2;
		}
	      else
		{
		  i = (str[1] - '0') * 010 + (str[2] - '0');
		  str++;
		}
	    }
	  else
	    i = str[1] - '0';
	  break;
	default:
	  i = str[0];
	  --str;
	  break;
	}
      str += 2;
      *ptr = str;
      return i;
    }

  if (str[0] == 'M' && str[1] == '-')
    {
      i = 0x80;
      str += 2;
    }
  else
    i = 0;

  if (str[0] == '^')
    {
      if (str[1] == '?')
	i += 0x7f;
      else if (str[1] >= '@' && str[1] <= '_')
	i |= str[1] - '@';
      else if (str[1] >= 'a' && str[1] <= 'z')
	i = str[1] - 'a' + 1;
      else if (str[1] == '\0' || isspace (str[1]))
	i = '^';
      else
	return -1;
      str += 2;
    }
  else
    {
      i |= str[0];
      str++;
    }
  *ptr = str;
  return i;
}

/* Take a char and turn it into a readable string */
#ifdef __STDC__
char *
char_to_string (int ch)
#else
char *
char_to_string (ch)
     int ch;
#endif
{
  static char buf[5] = "M-";

  if (ch >= ' ' && ch <= '~')
    {
      buf[3] = ch;
      return &buf[3];
    }
  if (ch & 0x80)
    {
      ch &= 0x7f;
      if (ch == 0x7f || ch < ' ')
	{
	  buf[2] = '^';
	  buf[3] = (ch == 0x7f ? '?' : ch + '@');
	}
      else
	{
	  buf[2] = ch;
	  buf[3] = '\0';
	}
      return &buf[0];
    }
  if (ch == 0x7f || ch < ' ')
    {
      buf[2] = '^';
      buf[3] = (ch == 0x7f ? '?' : ch + '@');
      return &buf[2];
    }
  return "huh";
}


#ifdef __STDC__
long
astol (char **ptr)
#else
long
astol (ptr)
     char **ptr;
#endif
{
  register long i = 0;
  register int c;
  int sign = 1;
  char *s;

  s = *ptr;
  /* Skip whitespace */
  while (isspace (*s))
    if (*s++ == '\0')
      {
	*ptr = s;
	return (0);
      }
  /* Check for - or + */
  if (*s == '-')
    {
      s++;
      sign = -1;
    }
  else if (*s == '+')
    s++;

  /* Read in the digits */
  for (; c = *s; s++)
    {
      if (!isdigit (c) || i > 214748364 || (i == 214748364 && c > (sign > 0 ? '7' : '8')))
	break;
      i = i * 10 + c - '0';
    }
  *ptr = s;
  return i * sign;
}

/*
 *	astof - accept a number of the form:
 *		(and ignores leading white space)
 *
 *	null	::=
 *	digit	::= 0|1|2|3|4|5|6|7|8|9
 *	digits	::= <digit>*
 *	DIGITS	::= <digit>+
 *	sign	::= <null> | + | -
 *	-------------------------------
 *		accepted:
 *	-------------------------------
 *	integer	::= <sign><DIGITS>
 *	real	::= <integer> . <digits> | <null> . <DIGITS>
 *	epart	::= e <integer> | E <integer>
 *	float	::= <integer> <epart> | <real> <epart>
 *
 *	Always returned as a double
 *
 *	There is no particular attempt to reduce mpys/divs
 *	those machines that are still out there (eg. PDP11/Small)
 *	that shun floating point arithmetic might rethink this routine.
 */

static double exps0[10] =
{1E0, 1E1, 1E2, 1E3, 1E4, 1E5, 1E6, 1E7, 1E8, 1E9};
static double exps1[10] =
{1E00, 1E10, 1E20, 1E30
#ifndef vax
 ,1E40, 1E50, 1E60, 1E70, 1E80, 1E90
#endif
};

#define REGISTER register

#ifdef __STDC__
double
astof (char **sp)
#else
double
astof (sp)
     char **sp;
#endif
{
  REGISTER char *s;
  REGISTER char *cp;
  long ipart, epart;
  int neg = 0;
  double res;
  int n;

  s = *sp;
  while (isspace (*s))
    {
      s++;
      if (*s == '\0')
	{
	  *sp = s;
	  return (0.0);
	}
    }
  /*
	 *	Need to handle sign here due to '-.3' or '-0.3'
	 */
  if (*s == '-')
    {
      ++neg;
      ++s;
    }
  else if (*s == '+')
    ++s;
  cp = s;
  /*
	 *	get ipart handling '.n' case
	 */
  res = 0.0;
  while (isdigit (*s))
    {
      for (n = 0, ipart = 0; n < 6 && isdigit (*s); n++)
	ipart = ipart * 10 + *s++ - '0';
      res = res * exps0[n] + (double) ipart;
    }
  if (s == cp)
    {
      if (*s == '.')
	ipart = 0;
      else
	{
	  *sp = s;
	  return (0.0);
	}
    }
  /*
	 *	either we find a '.' or e|E or done
	 */
  if (*s == '.')
    {
      int m;
      ++s;

      m = 0;
      while (isdigit (*s))
	{
	  for (n = 0, ipart = 0; n < 6 && isdigit (*s); n++)
	    ipart = ipart * 10 + *s++ - '0';
	  m += n;
	  if (m >= 100)
	    continue;
	  if (m >= 10)
	    res += ((double) ipart) / (exps1[m / 10] * exps0[m % 10]);
	  else
	    res += ((double) ipart) / exps0[m];
	}
    }
  /*
	 *	In either case (.) handle E part
	 */
  if (*s == 'e' || *s == 'E')
    {
      int eneg;

      ++s;
      epart = 0;
      eneg = 0;
      if (*s == '-')
	{
	  eneg++;
	  s++;
	}
      else if (*s == '+')
	s++;
      while (isdigit (*s))
	epart = epart * 10 + *s++ - '0';
      if (eneg)
	{
#ifndef vax
	  while (epart >= 100)
	    {
	      res /= 1E100;
	      epart -= 100;
	    }
#endif
	  if (epart > 9)
	    {
	      res /= exps1[epart / 10];
	      epart %= 10;
	    }
	  if (epart)
	    res /= exps0[epart];
	}
      else
	{
#ifndef vax
	  while (epart >= 100)
	    {
	      res *= 1E100;
	      epart -= 100;
	    }
#endif
	  if (epart > 9)
	    {
	      res *= exps1[epart / 10];
	      epart %= 10;
	    }
	  if (epart)
	    res *= exps0[epart];
	}
    }
  /*
	 *	fix sign
	 */
  if (neg)
    res = -res;
  *sp = s;
  return (res);
}

#ifdef TEST_ASTOF
main ()
{
  char buf[80];
  char *ptr;
  double at, ast;
  double atof ();

  while (gets (buf))
    {
      at = atof (buf);
      ptr = buf;
      ast = astof (&ptr);
      printf ("%15.6f %15.6f %s ", at, ast, at == ast ? "eq" : "NEQ");
      if (*ptr)
	printf ("%s->'%s'\n", buf, ptr);
      else
	printf ("%s\n", buf);
    }
}
char *
ck_savestr (str)
     char *str;
{
  char *newstr = 0;
  if (str)
    {
      int len = strlen (str) + 1;
      newstr = (char *) ck_malloc (len);
      bcopy (str, newstr, len);
    }
  return newstr;
}

#endif
