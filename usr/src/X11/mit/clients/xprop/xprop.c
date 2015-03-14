/* $XConsortium: xprop.c,v 1.37 91/05/12 18:23:07 rws Exp $*/
/*
 * Copyright 1990 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 */
	
#include <X11/Xlib.h>
#include <X11/Xos.h>
#include <X11/Xfuncs.h>
#include <X11/Xutil.h>
#include <stdio.h>
#include <ctype.h>

#include <X11/Xatom.h>
#include <X11/Xmu/WinUtil.h>

#include "dsimple.h"

#ifndef X_NOT_STDC_ENV
#include <stdlib.h>
#else
char *getenv();
#endif

#define MAXSTR 10000

#define min(a,b)  ((a) < (b) ? (a) : (b))

/*
 *
 * The Thunk Manager - routines to create, add to, and free thunk lists
 *
 */

typedef struct {
  int thunk_count;
  long value;
  char *extra_value;
  char *format;
  char *dformat;
} thunk;

thunk *Create_Thunk_List()
{
  thunk *tptr;

  tptr = (thunk *) Malloc( sizeof(thunk) );

  tptr->thunk_count = 0;

  return(tptr);
}

Free_Thunk_List(list)
thunk *list;
{
  free(list);
}

thunk *Add_Thunk(list, t)
thunk *list;
thunk t;
{
  int i;

  i = list->thunk_count;

  list = (thunk *) realloc(list, (i+1)*sizeof(thunk) );
  if (!list)
    Fatal_Error("Out of memory!");

  list[i++] = t;
  list->thunk_count = i;

  return(list);
}

/*
 * Misc. routines
 */

char *Copy_String(string)
     char *string;
{
	char *new;
	int length;

	length = strlen(string) + 1;

	new = (char *) Malloc(length);
	bcopy(string, new, length);

	return(new);
}

int Read_Char(stream)
     FILE *stream;
{
	int c;

	c = getc(stream);
	if (c==EOF)
	  Fatal_Error("Bad format file: Unexpected EOF.");
	return(c);
}

Read_White_Space(stream)
     FILE *stream;
{
	int c;

	while ((c=getc(stream))==' ' || c=='\n' || c=='\t');
	ungetc(c, stream);
}

static char _large_buffer[MAXSTR+10];

char *Read_Quoted(stream)
     FILE *stream;
{
	char *ptr;
	int c, length;

	Read_White_Space(stream);
	if (Read_Char(stream)!='\'')
	  Fatal_Error("Bad format file format: missing dformat.");

	ptr = _large_buffer; length=MAXSTR;
	for (;;) {
		if (length<0)
		  Fatal_Error("Bad format file format: dformat too long.");
		c = Read_Char(stream);
		if (c==(int) '\'')
		  break;
		ptr++[0]=c; length--;
		if (c== (int) '\\') {
			c=Read_Char(stream);
			if (c=='\n') {
				ptr--; length++;
			} else
			  ptr++[0]=c; length--;
		}
	}
	ptr++[0]='\0';

	return(Copy_String(_large_buffer));
}

/*
 *
 * Atom to format, dformat mapping Manager
 *
 */

#define D_FORMAT "0x"              /* Default format for properties */
#define D_DFORMAT " = $0+\n"       /* Default display pattern for properties */

static thunk *_property_formats = 0;   /* Holds mapping */

Apply_Default_Formats(format, dformat)
     char **format;
     char **dformat;
{
  if (!*format)
    *format = D_FORMAT;
  if (!*dformat)
    *dformat = D_DFORMAT;
}
  
Lookup_Formats(atom, format, dformat)
     Atom atom;
     char **format;
     char **dformat;
{
  int i;

  if (_property_formats)
    for (i=_property_formats->thunk_count-1; i>=0; i--)
      if (_property_formats[i].value==atom) {
	if (!*format)
	  *format = _property_formats[i].format;
	if (!*dformat)
	  *dformat = _property_formats[i].dformat;
	break;
      }
}

Add_Mapping(atom, format, dformat)
     Atom atom;
     char *format;
     char *dformat;
{
  thunk t;

  if (!_property_formats)
    _property_formats = Create_Thunk_List();

  t.value=atom;
  t.format=format;
  t.dformat=dformat;

  _property_formats = Add_Thunk(_property_formats, t);
}

/*
 *
 * Setup_Mapping: Routine to setup default atom to format, dformat mapping:
 * 
 */

typedef struct _propertyRec {
    char *	name;
    Atom	atom;
    char *	format;
    char *	dformat;
} propertyRec;

#define ARC_DFORMAT	":\n\
\t\tarc at $0, $1\n\
\t\tsize: $2 by $3\n\
\t\tfrom angle $4 to angle $5\n"

#define RECTANGLE_DFORMAT	":\n\
\t\tupper left corner: $0, $1\n\
\t\tsize: $2 by $3\n"

#define RGB_COLOR_MAP_DFORMAT	":\n\
\t\tcolormap id #: $0\n\
\t\tred-max: $1\n\
\t\tred-mult: $2\n\
\t\tgreen-max: $3\n\
\t\tgreen-mult: $4\n\
\t\tblue-max: $5\n\
\t\tblue-mult: $6\n\
\t\tbase-pixel: $7\n"

#define WM_HINTS_DFORMAT	":\n\
?m0(\t\tClient accepts input or input focus: $1\n)\
?m1(\t\tInitial state is \
?$2=0(Don't Care State)\
?$2=1(Normal State)\
?$2=2(Zoomed State)\
?$2=3(Iconic State)\
?$2=4(Inactive State)\
.\n)\
?m2(\t\tbitmap id # to use for icon: $3\n)\
?m5(\t\tbitmap id # of mask for icon: $7\n)\
?m3(\t\twindow id # to use for icon: $4\n)\
?m4(\t\tstarting position for icon: $5, $6\n)\
?m6(\t\twindow id # of group leader: $8\n)"

#define WM_ICON_SIZE_DFORMAT	":\n\
\t\tminimum icon size: $0 by $1\n\
\t\tmaximum icon size: $2 by $3\n\
\t\tincremental size change: $4 by $5\n"

#define WM_SIZE_HINTS_DFORMAT ":\n\
?m0(\t\tuser specified location: $1, $2\n)\
?m2(\t\tprogram specified location: $1, $2\n)\
?m1(\t\tuser specified size: $3 by $4\n)\
?m3(\t\tprogram specified size: $3 by $4\n)\
?m4(\t\tprogram specified minimum size: $5 by $6\n)\
?m5(\t\tprogram specified maximum size: $7 by $8\n)\
?m6(\t\tprogram specified resize increment: $9 by $10\n)\
?m7(\t\tprogram specified minimum aspect ratio: $11/$12\n\
\t\tprogram specified maximum aspect ratio: $13/$14\n)\
?m8(\t\tprogram specified base size: $15 by $16\n)\
?m9(\t\twindow gravity: \
?$17=0(Forget)\
?$17=1(NorthWest)\
?$17=2(North)\
?$17=3(NorthEast)\
?$17=4(West)\
?$17=5(Center)\
?$17=6(East)\
?$17=7(SouthWest)\
?$17=8(South)\
?$17=9(SouthEast)\
?$17=10(Static)\
\n)"

#define WM_STATE_DFORMAT	 ":\n\
\t\twindow state: ?$0=0(Withdrawn)?$0=1(Normal)?$0=3(Iconic)\n\
\t\ticon window: $1\n"

propertyRec windowPropTable[] = {
    {"ARC",		XA_ARC,		"16iiccii",   ARC_DFORMAT },
    {"ATOM",		XA_ATOM,	 "32a",	      0 },
    {"BITMAP",		XA_BITMAP,	 "32x",	      ": bitmap id # $0\n" },
    {"CARDINAL",	XA_CARDINAL,	 "0c",	      0 },
    {"COLORMAP",	XA_COLORMAP,	 "32x",	      ": colormap id # $0\n" },
    {"CURSOR",		XA_CURSOR,	 "32x",	      ": cursor id # $0\n" },
    {"DRAWABLE",	XA_DRAWABLE,	 "32x",	      ": drawable id # $0\n" },
    {"FONT",		XA_FONT,	 "32x",	      ": font id # $0\n" },
    {"INTEGER",		XA_INTEGER,	 "0i",	      0 },
    {"PIXMAP",		XA_PIXMAP,	 "32x",	      ": pixmap id # $0\n" },
    {"POINT",		XA_POINT,	 "16ii",      " = $0, $1\n" },
    {"RECTANGLE",	XA_RECTANGLE,	 "16iicc",    RECTANGLE_DFORMAT },
    {"RGB_COLOR_MAP",	XA_RGB_COLOR_MAP,"32xccccccc",RGB_COLOR_MAP_DFORMAT},
    {"STRING",		XA_STRING,	 "8s",	      0 },
    {"WINDOW",		XA_WINDOW,	 "32x",	      ": window id # $0\n" },
    {"VISUALID",	XA_VISUALID,	 "32x",	      ": visual id # $0\n" },
    {"WM_COLORMAP_WINDOWS",	0,	 "32x",       ": window id # $0+\n"},
    {"WM_COMMAND",	XA_WM_COMMAND,	 "8s",	      " = { $0+ }\n" },
    {"WM_HINTS",	XA_WM_HINTS,	 "32mbcxxiixx",	WM_HINTS_DFORMAT },
    {"WM_ICON_SIZE",	XA_WM_ICON_SIZE, "32cccccc",	WM_ICON_SIZE_DFORMAT},
    {"WM_PROTOCOLS",		0,	 "32a",	      ": protocols  $0+\n"},
    {"WM_SIZE_HINTS",	XA_WM_SIZE_HINTS,"32mii",     WM_SIZE_HINTS_DFORMAT },
    {"WM_STATE",		0,	 "32cx",      WM_STATE_DFORMAT}
};
#undef ARC_DFORMAT
#undef RECTANGLE_DFORMAT
#undef RGB_COLOR_MAP_DFORMAT
#undef WM_ICON_SIZE_DFORMAT
#undef WM_HINTS_DFORMAT
#undef WM_SIZE_HINTS_DFORMAT
#undef WM_STATE_DFORMAT

/* 
 * Font-specific mapping of property names to types:
 */
propertyRec fontPropTable[] = {

    /* XLFD name properties */

    "FOUNDRY",			0, 	 		"32a",	0,
    "FAMILY_NAME",		XA_FAMILY_NAME,		"32a",	0,
    "WEIGHT_NAME",		0,			"32a",	0,
    "SLANT",			0,			"32a",	0,
    "SETWIDTH_NAME",		0,			"32a",	0,
    "ADD_STYLE_NAME",		0,			"32a",	0,
    "PIXEL_SIZE",		0,			"32c",	0,
    "POINT_SIZE",		XA_POINT_SIZE,		"32c",	0,
    "RESOLUTION_X",		0,			"32c",	0,
    "RESOLUTION_Y",		0,			"32c",	0,
    "SPACING",			0,			"32a",	0,
    "AVERAGE_WIDTH",		0,			"32c",	0,
    "CHARSET_REGISTRY",		0,			"32a",	0,
    "CHARSET_ENCODING",		0,			"32a",	0,

    /* other font properties referenced in the XLFD */

    "QUAD_WIDTH",		XA_QUAD_WIDTH,		"32i",	0,
    "RESOLUTION",		XA_RESOLUTION,		"32c",	0,
    "MIN_SPACE",		XA_MIN_SPACE,		"32c",	0,
    "NORM_SPACE",		XA_NORM_SPACE,		"32c",	0,
    "MAX_SPACE",		XA_MAX_SPACE,		"32c",	0,
    "END_SPACE",		XA_END_SPACE,		"32c",	0,
    "SUPERSCRIPT_X",		XA_SUPERSCRIPT_X,	"32i",	0,
    "SUPERSCRIPT_Y",		XA_SUPERSCRIPT_Y,	"32i",	0,
    "SUBSCRIPT_X",		XA_SUBSCRIPT_X,		"32i",	0,
    "SUBSCRIPT_Y",		XA_SUBSCRIPT_Y,		"32i",	0,
    "UNDERLINE_POSITION",	XA_UNDERLINE_POSITION,	"32i",	0,
    "UNDERLINE_THICKNESS",	XA_UNDERLINE_THICKNESS,	"32i",	0,
    "STRIKEOUT_ASCENT",		XA_STRIKEOUT_ASCENT,	"32i",	0,
    "STRIKEOUT_DESCENT",	XA_STRIKEOUT_DESCENT,	"32i",	0,
    "ITALIC_ANGLE",		XA_ITALIC_ANGLE,	"32i",	0,
    "X_HEIGHT",			XA_X_HEIGHT,		"32i",	0,
    "WEIGHT",			XA_WEIGHT,		"32i",	0,
    "FACE_NAME",		0,			"32a",	0,
    "COPYRIGHT",		XA_COPYRIGHT,		"32a",	0,
    "AVG_CAPITAL_WIDTH",	0,			"32i",	0,
    "AVG_LOWERCASE_WIDTH",	0,			"32i",	0,
    "RELATIVE_SETWIDTH",	0,			"32c",	0,
    "RELATIVE_WEIGHT",		0,			"32c",	0,
    "CAP_HEIGHT",		XA_CAP_HEIGHT,		"32c",	0,
    "SUPERSCRIPT_SIZE",		0,			"32c",	0,
    "FIGURE_WIDTH",		0,			"32i",	0,
    "SUBSCRIPT_SIZE",		0,			"32c",	0,
    "SMALL_CAP_SIZE",		0,			"32i",	0,
    "NOTICE",			XA_NOTICE,		"32a",	0,
    "DESTINATION",		0,			"32c",	0,

    /* other font properties */

    "FONT",			XA_FONT,		"32a",	0,
    "FONT_NAME",		XA_FONT_NAME,		"32a",	0,
};    

static int XpropMode;
#define XpropWindowProperties 0
#define XpropFontProperties   1

Setup_Mapping()
{
    int n;
    propertyRec *p;
    
    if (XpropMode == XpropWindowProperties) {
	n = sizeof(windowPropTable) / sizeof(propertyRec);
	p = windowPropTable;
    } else {
	n = sizeof (fontPropTable) / sizeof (propertyRec);
	p = fontPropTable;
    }
    for ( ; --n >= 0; p++) {
	if (! p->atom) {
	    p->atom = XInternAtom(dpy, p->name, True);
	    if (p->atom == None)
		continue;
	}
	Add_Mapping(p->atom, p->format, p->dformat);
    }	
}

char *GetAtomName(atom)
    Atom atom;
{
    int n;
    propertyRec *p;
        
    if (XpropMode == XpropWindowProperties) {
	n = sizeof(windowPropTable) / sizeof(propertyRec);
	p = windowPropTable;
    } else {
	n = sizeof (fontPropTable) / sizeof (propertyRec);
	p = fontPropTable;
    }
    for ( ; --n >= 0; p++)
	if (p->atom == atom)
	    return p->name;

    return (char *) NULL;
}

/*
 * Read_Mapping: routine to read in additional mappings from a stream
 *               already open for reading.
 */

Read_Mappings(stream)
     FILE *stream;
{
	char format_buffer[100];
	char name[1000], *dformat, *format;
	int count, c;
	Atom atom, Parse_Atom();

	while ((count=fscanf(stream," %990s %90s ",name,format_buffer))!=EOF) {
		if (count != 2)
		  Fatal_Error("Bad format file format.");

		atom = Parse_Atom(name, False);
		format = Copy_String(format_buffer);

		Read_White_Space(stream);
		dformat = D_DFORMAT;
		c = getc(stream);
		ungetc(c, stream);
		if (c==(int)'\'')
		  dformat = Read_Quoted(stream);

		Add_Mapping(atom, format, dformat);
	}
}

/*
 *
 * Formatting Routines: a group of routines to translate from various
 *                      values to a static read-only string useful for output.
 *
 * Routines: Format_Hex, Format_Unsigned, Format_Signed, Format_Atom,
 *           Format_Mask_Word, Format_Bool, Format_String, Format_Len_String.
 *
 * All of the above routines take a long except for Format_String and
 * Format_Len_String.
 *
 */
static char _formatting_buffer[MAXSTR+100];
static char _formatting_buffer2[10];

char *Format_Hex(wrd)
     long wrd;
{
  sprintf(_formatting_buffer2, "0x%lx", wrd);
  return(_formatting_buffer2);
}

char *Format_Unsigned(wrd)
     long wrd;
{
  sprintf(_formatting_buffer2, "%lu", wrd);
  return(_formatting_buffer2);
}

char *Format_Signed(wrd)
     long wrd;
{
  sprintf(_formatting_buffer2, "%ld", wrd);
  return(_formatting_buffer2);
}

/*ARGSUSED*/
int ignore_errors (dpy, ev)
    Display *dpy;
    XErrorEvent *ev;
{
    return 0;
}

char *Format_Atom(atom)
     Atom atom;
{
  char *name;
  int (*handler)();

  if (name = GetAtomName(atom)) {
      strncpy(_formatting_buffer, name, MAXSTR);
      return(_formatting_buffer);
  }

  handler = XSetErrorHandler (ignore_errors);
  name=XGetAtomName(dpy, atom);
  XSetErrorHandler(handler);
  if (! name)
      sprintf(_formatting_buffer, "undefined atom # 0x%lx", atom);
  else {
      strncpy(_formatting_buffer, name, MAXSTR);
      XFree(name);
  }
  return(_formatting_buffer);
}

char *Format_Mask_Word(wrd)
     long wrd;
{
  long bit_mask, bit;
  int seen = 0;

  strcpy(_formatting_buffer, "{MASK: ");
  for (bit=0, bit_mask=1; bit<=sizeof(long)*8; bit++, bit_mask<<=1) {
    if (bit_mask & wrd) {
      if (seen) {
	strcat(_formatting_buffer, ", ");
      }
      seen=1;
      strcat(_formatting_buffer, Format_Unsigned(bit));
    }
  }
  strcat(_formatting_buffer, "}");

  return(_formatting_buffer);
}

char *Format_Bool(value)
     long value;
{
  if (!value)
    return("False");

  return("True");
}

static char *_buf_ptr;
static int _buf_len;

_put_char(c)
     char c;
{
	if (--_buf_len<0) {
		_buf_ptr[0]='\0';
		return;
	}
	_buf_ptr++[0] = c;
}

_format_char(c)
     char c;
{
	switch (c) {
	      case '\\':
	      case '\"':
		_put_char('\\');
		_put_char(c);
		break;
	      case '\n':
		_put_char('\\');
		_put_char('n');
		break;
	      case '\t':
		_put_char('\\');
		_put_char('t');
		break;
	      default:
		if (c<' ') {
			_put_char('\\');
			sprintf(_buf_ptr, "%o", (int) c);
			_buf_ptr += strlen(_buf_ptr);
			_buf_len -= strlen(_buf_ptr);
		} else
		  _put_char(c);
	}
}

char *Format_String(string)
     char *string;
{
	char c;

	_buf_ptr = _formatting_buffer;
	_buf_len = MAXSTR;
	_put_char('\"');

	while (c = string++[0])
	  _format_char(c);

	_buf_len += 3;
	_put_char('\"');
	_put_char('\0');
	return(_formatting_buffer);
}

char *Format_Len_String(string, len)
     char *string;
     int len;
{
  char *data, *result;

  data = (char *) Malloc(len+1);

  bcopy(string, data, len);
  data[len]='\0';

  result = Format_String(data);
  free(data);

  return(result);
}  

/*
 *
 * Parsing Routines: a group of routines to parse strings into values
 *
 * Routines: Parse_Atom, Scan_Long, Skip_Past_Right_Paran, Scan_Octal
 *
 * Routines of the form Parse_XXX take a string which is parsed to a value.
 * Routines of the form Scan_XXX take a string, parse the beginning to a value,
 * and return the rest of the string.  The value is returned via. the last
 * parameter.  All numeric values are longs!
 *
 */

char *Skip_Digits(string)
     char *string;
{
	while (isdigit(string[0])) string++;
	return(string);
}

char *Scan_Long(string, value)
     char *string;
     long *value;
{
	if (!isdigit(*string))
	  Fatal_Error("Bad number: %s.", string);

	*value = atol(string);
	return(Skip_Digits(string));
}

char *Scan_Octal(string, value)
     char *string;
     long *value;
{
	if (sscanf(string, "%lo", value)!=1)
	  Fatal_Error("Bad octal number: %s.", string);
	return(Skip_Digits(string));
}

Atom Parse_Atom(name, only_if_exists)
     char *name;
     int only_if_exists;
{
	Atom atom;

	if ((atom = XInternAtom(dpy, name, only_if_exists))==None)
	  return(0);

	return(atom);
}

char *Skip_Past_Right_Paran(string)
     char *string;
{
	char c;
	int nesting=0;

	while (c=string++[0], c!=')' || nesting)
	  switch (c) {
		case '\0':
		  Fatal_Error("Missing ')'.");
		case '(':
		  nesting++;
		  break;
		case ')':
		  nesting--;
		  break;
		case '\\':
		  string++;
		  break;
	  }
	return(string);
}

/*
 *
 * The Format Manager: a group of routines to manage "formats"
 *
 */

int Is_A_Format(string)
char *string;
{
	return(isdigit(string[0]));
}

int Get_Format_Size(format)
  char *format;
{
  long size;

  Scan_Long(format, &size);

  /* Check for legal sizes */
  if (size != 0 && size != 8 && size != 16 && size != 32)
    Fatal_Error("bad format: %s", format);

  return((int) size);
}

char Get_Format_Char(format, i)
     char *format;
     int i;
{
  long size;

  /* Remove # at front of format */
  format = Scan_Long(format, &size);
  if (!*format)
    Fatal_Error("bad format: %s", format);

  /* Last character repeats forever... */
  if (i >= (int)strlen(format))
    i=strlen(format)-1;

  return(format[i]);
}

char *Format_Thunk(t, format_char)
     thunk t;
     char format_char;
{
  long value;
  value = t.value;

  switch (format_char) {
  case 's':
    return(Format_Len_String(t.extra_value, t.value));
  case 'x':
    return(Format_Hex(value));
  case 'c':
    return(Format_Unsigned(value));
  case 'i':
    return(Format_Signed(value));
  case 'b':
    return(Format_Bool(value));
  case 'm':
    return(Format_Mask_Word(value));
  case 'a':
    return(Format_Atom(value));
  default:
    Fatal_Error("bad format character: %c", format_char);
  }
}

char *Format_Thunk_I(thunks, format, i)
     thunk *thunks;
     char *format;
     int i;
{
  if (i >= thunks->thunk_count)
    return("<field not available>");

  return(Format_Thunk(thunks[i], Get_Format_Char(format, i)));
}

long Mask_Word(thunks, format)
     thunk *thunks;
     char *format;
{
	int j;

	for (j=0; j<(int)strlen(format); j++)
	  if (Get_Format_Char(format, j) == 'm')
	    return(thunks[j].value);
	return(0L);
}

/*
 *
 * The Display Format Manager:
 *
 */

int Is_A_DFormat(string)
     char *string;
{
  return( string[0] && string[0] != '-' &&
	 !(isalpha(string[0]) || string[0] == '_') );
}

char *Handle_Backslash(dformat)
     char *dformat;
{
	char c;
	long i;

	if (!(c = *(dformat++)))
	  return(dformat);

	switch (c) {
	      case 'n':
		putchar('\n');
		break;
	      case 't':
		putchar('\t');
		break;
	      case '0':
	      case '1':
	      case '2':
	      case '3':
	      case '4':
	      case '5':
	      case '6':
	      case '7':
		dformat = Scan_Octal(dformat, &i);
		putchar((int) i);
		break;
	      default:
		putchar(c);
		break;
	}
	return(dformat);
}

char *Handle_Dollar_sign(dformat, thunks, format)
     char *dformat;
     thunk *thunks;
     char *format;
{
	long i;

	dformat = Scan_Long(dformat, &i);

	if (dformat[0]=='+') {
		int seen=0;
		dformat++;
		for (; i<thunks->thunk_count; i++) {
			if (seen)
			  printf(", ");
			seen = 1;
			printf("%s", Format_Thunk_I(thunks, format, (int) i));
		}
	} else
	  printf("%s", Format_Thunk_I(thunks, format, (int) i));

	return(dformat);
}

int Mask_Bit_I(thunks, format, i)
     thunk *thunks;
     char *format;
     int i;
{
	long value;

	value = Mask_Word(thunks, format);

	value = value & (1L<<i);
	if (value)
	  value=1;
	return(value);
}

char *Scan_Term(string, thunks, format, value)
     thunk *thunks;
     char *string, *format;
     long *value;
{
	long i;

	*value=0;

	if (isdigit(*string))
	  string = Scan_Long(string, value);
	else if (*string=='$') {
		string = Scan_Long(++string, &i);
		if (i>=thunks->thunk_count)
		  i=thunks->thunk_count;
		*value = thunks[i].value;
	} else if (*string=='m') {
		string = Scan_Long(++string, &i);
		*value = Mask_Bit_I(thunks, format, (int) i);
	} else
	  Fatal_Error("Bad term: %s.", string);

	return(string);
}

char *Scan_Exp(string, thunks, format, value)
     thunk *thunks;
     char *string, *format;
     long *value;
{
	long temp;

	if (string[0]=='(') {
		string = Scan_Exp(++string, thunks, format, value);
		if (string[0]!=')')
		  Fatal_Error("Missing ')'");
		return(++string);
	}
	if (string[0]=='!') {
		string = Scan_Exp(++string, thunks, format, value);
		*value = !*value;
		return(string);
	}

	string = Scan_Term(string, thunks, format, value);

	if (string[0]=='=') {
		string = Scan_Exp(++string, thunks, format, &temp);
		*value = *value == temp;
	}

	return(string);
}

char *Handle_Question_Mark(dformat, thunks, format)
     thunk *thunks;
     char *dformat, *format;
{
	long true;

	dformat = Scan_Exp(dformat, thunks, format, &true);

	if (*dformat!='(')
	  Fatal_Error("Bad conditional: '(' expected: %s.", dformat);
	++dformat;

	if (!true)
	  dformat = Skip_Past_Right_Paran(dformat);

	return(dformat);
}

Display_Property(thunks, dformat, format)
     thunk *thunks;
     char *dformat, *format;
{
  char c;

  while (c = *(dformat++))
    switch (c) {
	  case ')':
	    continue;
	  case '\\':
	    dformat = Handle_Backslash(dformat);
	    continue;
	  case '$':
	    dformat = Handle_Dollar_sign(dformat, thunks, format);
	    continue;
	  case '?':
	    dformat = Handle_Question_Mark(dformat, thunks, format);
	    continue;
	  default:
	    putchar(c);
	    continue;
    }
}

/*
 *
 * Routines to convert property data to and from thunks
 *
 */

long Extract_Value(pointer, length, size, signedp)
     char **pointer;
     int *length;
     int size;
{
	long value, mask;

	switch (size) {
	      case 8:
		value = (long) * (char *) *pointer;
		*pointer += 1;
		*length -= 1;
		mask = 0xff;
		break;
	      case 16:
		value = (long) * (short *) *pointer;
		*pointer += 2;
		*length -= 2;
		mask = 0xffff;
		break;
	      default:
		/* Error */
	      case 32:
		value = (long) * (long *) *pointer;
		*pointer += 4;
		*length -= 4;
		mask = 0xffffffff;
		break;
	}
	if (!signedp)
	  value &= mask;
	return(value);
}

long Extract_Len_String(pointer, length, size, string)
     char **pointer;
     int *length;
     int size;
     char **string;
{
	int len;

	if (size!=8)
	 Fatal_Error("can't use format character 's' with any size except 8.");
	len=0; *string = *pointer;
	while ((len++, --*length, *((*pointer)++)) && *length>0);

	return(len);
}

thunk *Break_Down_Property(pointer, length, format, size)
     char *pointer, *format;
     int length, size;
{
	thunk *thunks;
	thunk t;
	int i;
	char format_char;

	thunks = Create_Thunk_List();
	i=0;

	while (length>=(size/8)) {
	    format_char = Get_Format_Char(format, i);
	    if (format_char=='s')
	      t.value=Extract_Len_String(&pointer,&length,size,&t.extra_value);
	    else
	      t.value=Extract_Value(&pointer,&length,size,format_char=='i');
	    thunks = Add_Thunk(thunks, t);
	    i++;
        }

	return(thunks);
}

/*
 * 
 * Routines for parsing command line:
 *
 */

usage()
{
    char **cpp;
    static char *help_message[] = {
"where options include:",
"    -grammar                       print out full grammar for command line",
"    -display host:dpy              the X server to contact",
"    -id id                         resource id of window to examine",
"    -name name                     name of window to examine",
"    -font name                     name of font to examine",
"    -remove propname               name of property to remove",
"    -root                          examine the root window",
"    -len n                         display at most n bytes of any property",
"    -notype                        do not display the type field",
"    -fs filename                   where to look for formats for properties",
"    -frame                         don't ignore window manager frames",
"    -f propname format [dformat]   formats to use for property of given name",
"    -spy                           examine window properties forever",
NULL};

    fflush (stdout);
    fprintf (stderr, "usage:  %s [-options ...] [[format [dformat]] atom]\n\n", 
	     program_name);
    for (cpp = help_message; *cpp; cpp++) {
	fprintf (stderr, "%s\n", *cpp);
    }
    fprintf (stderr, "\n");
    exit (1);
}

grammar ()
{
	printf ("Grammar for xprop:\n\n");
	printf("\t%s [<disp>] [<select option>] <option>* <mapping>* <spec>*",
	     program_name);
	printf("\n\n\tdisp ::= -display host:dpy\
\n\tselect option ::= -root | -id <id> | -font <font> | -name <name>\
\n\toption ::= -len <n> | -notype | -spy | {-formats|-fs} <format file>\
\n\tmapping ::= {-f|-format} <atom> <format> [<dformat>] | -remove <propname>\
\n\tspec ::= [<format> [<dformat>]] <atom>\
\n\tformat ::= {0|8|16|32}{a|b|c|i|m|s|x}*\
\n\tdformat ::= <unit><unit>*             (can't start with a letter or '-')\
\n\tunit ::= ?<exp>(<unit>*) | $<n> | <display char>\
\n\texp ::= <term> | <term>=<exp> | !<exp>\
\n\tterm ::= <n> | $<n> | m<n>\
\n\tdisplay char ::= <normal char> | \\<non digit char> | \\<octal number>\
\n\tnormal char ::= <any char except a digit, $, ?, \\, or )>\
\n\n");
	exit(0);
}

Parse_Format_Mapping(argc, argv)
     int *argc;
     char ***argv;
#define ARGC (*argc)
#define ARGV (*argv)
#define OPTION ARGV[0]
#define NXTOPT if (++ARGV, --ARGC==0) usage()
{
  char *type_name, *format, *dformat;
  
  NXTOPT; type_name = OPTION;

  NXTOPT; format = OPTION;
  if (!Is_A_Format(format))
    Fatal_Error("Bad format: %s.", format);

  dformat=0;
  if (ARGC>0 && Is_A_DFormat(ARGV[1])) {
    ARGV++; ARGC--; dformat=OPTION;
  }

  Add_Mapping( Parse_Atom(type_name, False), format, dformat);
}

/*
 *
 * The Main Program:
 *
 */

Window target_win=0;
int notype=0;
int spy=0;
int max_len=MAXSTR;
XFontStruct *font;

main(argc, argv)
int argc;
char **argv;
{
  FILE *stream;
  char *name;
  thunk *props, *Handle_Prop_Requests();
  char *remove_propname = NULL;
  Bool frame_only = False;
  int n;
  char **nargv;

  INIT_NAME;

  /* Handle display name, opening the display */
  Setup_Display_And_Screen(&argc, argv);

  /* Handle selecting the window to display properties for */
  target_win = Select_Window_Args(&argc, argv);

  /* Set up default atom to format, dformat mapping */
  XpropMode = XpropWindowProperties;
  for (n=argc, nargv=argv; n; nargv++, n--)
      if (! strcmp(nargv[0], "-font")) {
	  XpropMode = XpropFontProperties;
	  break;
      }
  Setup_Mapping();
  if (name = getenv("XPROPFORMATS")) {
	  if (!(stream=fopen(name, "r")))
	    Fatal_Error("unable to open file %s for reading.", name);
	  Read_Mappings(stream);
	  fclose(stream);
  }

  /* Handle '-' options to setup xprop, select window to work on */
  while (argv++, --argc>0 && **argv=='-') {
    if (!strcmp(argv[0], "-"))
      continue;
    if (!strcmp(argv[0], "-grammar")) {
      grammar ();
      /* NOTREACHED */
    }
    if (!strcmp(argv[0], "-notype")) {
      notype=1;
      continue;
    }
    if (!strcmp(argv[0], "-spy")) {
	    spy=1;
	    continue;
    }
    if (!strcmp(argv[0], "-len")) {
	    if (++argv, --argc==0) usage();
	    max_len = atoi(argv[0]);
	    continue;
    }
    if (!strcmp(argv[0], "-formats") || !strcmp(argv[0], "-fs")) {
	    if (++argv, --argc==0) usage();
	    if (!(stream=fopen(argv[0], "r")))
	      Fatal_Error("unable to open file %s for reading.", argv[0]);
	    Read_Mappings(stream);
	    fclose(stream);
	    continue;
    }
    if (!strcmp(argv[0], "-font")) {
	    if (++argv, --argc==0) usage();
	    font = Open_Font(argv[0]);
	    target_win = -1;
	    continue;
    }
    if (!strcmp(argv[0], "-remove")) {
	    if (++argv, --argc==0) usage();
	    remove_propname = argv[0];
	    continue;
    }
    if (!strcmp(argv[0], "-frame")) {
	    frame_only = True;
	    continue;
    }
    if (!strcmp(argv[0], "-f") || !strcmp(argv[0], "-format")) {
      Parse_Format_Mapping(&argc, &argv);
      continue;
    }
    usage();
  }

  if (target_win == None) {
      target_win = Select_Window(dpy);
      if (target_win != None && !frame_only) {
	Window root;
	int dummyi;
	unsigned int dummy;

	if (XGetGeometry (dpy, target_win, &root, &dummyi, &dummyi,
			  &dummy, &dummy, &dummy, &dummy) &&
	    target_win != root)
	  target_win = XmuClientWindow (dpy, target_win);
      }
  }

  if (remove_propname) {
    remove_property (dpy, target_win, remove_propname);
    XCloseDisplay (dpy);
    exit (0);
  }

  props=Handle_Prop_Requests(argc, argv);

  if (spy && target_win != -1) {
	XEvent event;
        char *format, *dformat;
	
	XSelectInput(dpy, target_win, PropertyChangeMask);
	for (;;) {
		XNextEvent(dpy, &event);
		format = dformat = NULL;
		if (props) {
			int i;
			for (i=0; i<props->thunk_count; i++)
			  if (props[i].value == event.xproperty.atom)
			    break;
			if (i>=props->thunk_count)
			  continue;
			format = props[i].format;
			dformat = props[i].dformat;
	        }
		Show_Prop(format, dformat, Format_Atom(event.xproperty.atom));
        }
  }
  exit (0);
}

/*
 *
 * Other Stuff (temp.):
 *
 */

remove_property (dpy, w, propname)
    Display *dpy;
    Window w;
    char *propname;
{
    Atom id = XInternAtom (dpy, propname, True);

    if (id == None) {
	fprintf (stderr, "%s:  no such property \"%s\"\n",
		 program_name, propname);
	return;
    }
    XDeleteProperty (dpy, w, id);
    return;
}

thunk *Handle_Prop_Requests(argc, argv)
     int argc;
     char **argv;
{
  char *format, *dformat, *prop;
  thunk *thunks, t;

  thunks = Create_Thunk_List();

  /* if no prop referenced, by default list all properties for given window */
  if (!argc) {
    Show_All_Props();
    return(NULL);
  }

  while (argc>0) {
    format = 0;
    dformat = 0;

    /* Get overriding formats, if any */
    if (Is_A_Format(argv[0])) {
      format = argv++[0]; argc--;
      if (!argc) usage();
    }
    if (Is_A_DFormat(argv[0])) {
      dformat = argv++[0]; argc--;
      if (!argc) usage();
    }

    /* Get property name */
    prop = argv++[0]; argc--;

    t.value = Parse_Atom(prop, True);
    t.format = format;
    t.dformat = dformat;
    if (t.value)
      thunks = Add_Thunk(thunks, t);
    Show_Prop(format, dformat, prop);
  }
  return(thunks);
}

Show_All_Props()
{
  Atom *atoms, atom;
  char *name;
  int count, i;

  if (target_win!=-1) {
	  atoms = XListProperties(dpy, target_win, &count);
	  for (i=0; i<count; i++) {
		  name = Format_Atom(atoms[i]);
		  Show_Prop(0, 0, name);
	  }
  } else
    for (i=0; i<font->n_properties; i++) {
	    atom = font->properties[i].name;
	    name = Format_Atom(atom);
	    Show_Prop(0, 0, name);
    }
}

Set_Prop(format, dformat, prop, mode, value)
     char *format, *dformat, *prop, *value;
     char mode;
{
  outl("Seting prop %s(%s) using %s mode %c to %s",
       prop, format, dformat, mode, value);
}

static unsigned long _font_prop;

char *Get_Font_Property_Data_And_Type(atom, length, type, size)
     Atom atom;
     long *length;
     Atom *type;
     int *size;
{
	int i;
	
	*type = 0;
	
	for (i=0; i<font->n_properties; i++)
	  if (atom==font->properties[i].name) {
		  _font_prop=font->properties[i].card32;
		  *length=4;
		  *size=32;
		  return((char *) &_font_prop);
	  }
	return(0);
}

char *Get_Window_Property_Data_And_Type(atom, length, type, size)
     Atom atom;
     long *length;
     Atom *type;
     int *size;
{
	Atom actual_type;
	int actual_format;
	unsigned long nitems;
	unsigned long bytes_after;
	unsigned char *prop;
	int status;
	
	status = XGetWindowProperty(dpy, target_win, atom, 0, (max_len+3)/4,
				    False, AnyPropertyType, &actual_type,
				    &actual_format, &nitems, &bytes_after,
				    &prop);
	if (status==BadWindow)
	  Fatal_Error("window id # 0x%lx does not exists!", target_win);
	if (status!=Success)
	  Fatal_Error("XGetWindowProperty failed!");
	
	*length = min(nitems * actual_format/8, max_len);
	*type = actual_type;
	*size = actual_format;
	return((char *)prop);
}

char *Get_Property_Data_And_Type(atom, length, type, size)
     Atom atom;
     long *length;
     Atom *type;
     int *size;
{
	if (target_win == -1)
	  return(Get_Font_Property_Data_And_Type(atom, length, type, size));
	else
	  return(Get_Window_Property_Data_And_Type(atom, length, type, size));
}

Show_Prop(format, dformat, prop)
     char *format, *dformat, *prop;
{
  char *data;
  long length;
  Atom atom, type;
  thunk *thunks;
  int size, fsize;

  printf("%s", prop);
  if (!(atom = Parse_Atom(prop, True))) {
	  printf(":  no such atom on any window.\n");
	  return;
  }

  data = Get_Property_Data_And_Type(atom, &length, &type, &size);
  if (!size) {
          puts(":  not found.");
	  return;
  }

  if (!notype && type)
    printf("(%s)", Format_Atom(type));

  Lookup_Formats(atom, &format, &dformat);
  if (type)
    Lookup_Formats(type, &format, &dformat);
  Apply_Default_Formats(&format, &dformat);

  fsize=Get_Format_Size(format);
  if (fsize!=size && fsize!=0) {
	printf(": Type mismatch: assumed size %d bits, actual size %d bits.\n",
	 fsize, size);
	  return;
  }

  thunks = Break_Down_Property(data, length, format, size);

  Display_Property(thunks, dformat, format);
}
