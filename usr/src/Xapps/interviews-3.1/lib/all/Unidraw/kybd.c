/*
 * Copyright (c) 1990, 1991 Stanford University
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Stanford not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  Stanford makes no representations about
 * the suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * STANFORD DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.
 * IN NO EVENT SHALL STANFORD BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION
 * WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

/*
 * Keycode values.
 */

#include <Unidraw/kybd.h>

/*
 * components
 */
const char* KLBL_LINE		= "l",	*CODE_LINE		    = "l";
const char* KLBL_ELLIPSE	= "o",  *CODE_ELLIPSE		    = "o";
const char* KLBL_RECT		= "r",  *CODE_RECT		    = "r";
const char* KLBL_POLY		= "w",  *CODE_POLY		    = "w";
const char* KLBL_MULTILINE	= "e",  *CODE_MULTILINE		    = "e";
const char* KLBL_SPLINE		= "h",  *CODE_SPLINE		    = "h";
const char* KLBL_CSPLINE	= "y",  *CODE_CSPLINE		    = "y";
const char* KLBL_TEXT		= "t",  *CODE_TEXT		    = "t";
const char* KLBL_PIN		= "^P", *CODE_PIN		    = "";
const char* KLBL_SLOT		= "^-", *CODE_SLOT		    = "";
const char* KLBL_PAD		= "^W", *CODE_PAD		    = "";
const char* KLBL_LINK		= "L",  *CODE_LINK		    = "L";

/*
 * commands
 */
const char* KLBL_NEWCOMP        = "^N", *CODE_NEWCOMP               = "\016";
const char* KLBL_REVERT         = "^R", *CODE_REVERT                = "\022";
const char* KLBL_VIEWCOMP       = "^O", *CODE_VIEWCOMP              = "\017";
const char* KLBL_SAVECOMP       = "^S", *CODE_SAVECOMP              = "\023";
const char* KLBL_SAVECOMPAS     = "^A", *CODE_SAVECOMPAS            = "\001";
const char* KLBL_PRINT          = "P",  *CODE_PRINT                 = "P";
const char* KLBL_IMPORT         = "^I",  *CODE_IMPORT               = "\011";
const char* KLBL_QUIT		= "^Q",	*CODE_QUIT		    = "\021";

const char* KLBL_UNDO		= "U",	*CODE_UNDO		    = "U";
const char* KLBL_REDO		= "R",	*CODE_REDO		    = "R";
const char* KLBL_CUT		= "x",	*CODE_CUT		    = "x";
const char* KLBL_COPY		= "c",	*CODE_COPY		    = "c";
const char* KLBL_PASTE		= "v",	*CODE_PASTE		    = "v";
const char* KLBL_DUP		= "d",	*CODE_DUP		    = "d";
const char* KLBL_DEL		= "^D",	*CODE_DEL		    = "\4";
const char* KLBL_SLCTALL	= "a",  *CODE_SLCTALL		    = "a";
const char* KLBL_HFLIP          = "_",  *CODE_HFLIP                 = "_";
const char* KLBL_VFLIP          = "|",  *CODE_VFLIP                 = "|";
const char* KLBL_CW90           = "]",  *CODE_CW90                  = "]";
const char* KLBL_CCW90          = "[",  *CODE_CCW90                 = "[";

const char* KLBL_GROUP		= "g",	*CODE_GROUP		    = "g";
const char* KLBL_UNGROUP	= "u",  *CODE_UNGROUP		    = "u";
const char* KLBL_FRONT		= "f",	*CODE_FRONT		    = "f";
const char* KLBL_BACK		= "b",	*CODE_BACK		    = "b";
const char* KLBL_INSTANCE	= "I",  *CODE_INSTANCE		    = "I";

const char* KLBL_ALGNLEFT	= "1",  *CODE_ALGNLEFT		    = "1";
const char* KLBL_ALGNRIGHT	= "2",  *CODE_ALGNRIGHT		    = "2";
const char* KLBL_ALGNBOT	= "3",  *CODE_ALGNBOT		    = "3";
const char* KLBL_ALGNTOP	= "4",  *CODE_ALGNTOP		    = "4";
const char* KLBL_ALGNVCTR	= "5",  *CODE_ALGNVCTR		    = "5";
const char* KLBL_ALGNHCTR	= "6",  *CODE_ALGNHCTR		    = "6";
const char* KLBL_ALGNCTR	= "7",  *CODE_ALGNCTR		    = "7";
const char* KLBL_ABUTLEFT	= "8",  *CODE_ABUTLEFT		    = "8";
const char* KLBL_ABUTRIGHT	= "9",  *CODE_ABUTRIGHT		    = "9";
const char* KLBL_ABUTDOWN	= "0",  *CODE_ABUTDOWN		    = "0";
const char* KLBL_ABUTUP		= "-",	*CODE_ABUTUP		    = "-";
const char* KLBL_ALGNTOGRID	= ".",	*CODE_ALGNTOGRID	    = ".";

const char* KLBL_NORMSIZE	= "n",  *CODE_NORMSIZE		    = "n";
const char* KLBL_REDTOFIT	= "=",  *CODE_REDTOFIT		    = "=";
const char* KLBL_CENTER		= "/",	*CODE_CENTER		    = "/";
const char* KLBL_GRID		= "?",	*CODE_GRID		    = "?";
const char* KLBL_GRIDSPC	= "S",	*CODE_GRIDSPC		    = "S";
const char* KLBL_GRAVITY	= ",",	*CODE_GRAVITY		    = ",";
const char* KLBL_ORIENTATION	= "+",	*CODE_ORIENTATION	    = "+";
const char* KLBL_CLOSEEDITOR    = "K",  *CODE_CLOSEEDITOR           = "K";

/*
 * tools
 */
const char* KLBL_SELECT		= "s",  *CODE_SELECT		    = "s";
const char* KLBL_MOVE		= "m",  *CODE_MOVE		    = "m";
const char* KLBL_CONNECT	= "C",  *CODE_CONNECT		    = "C";
const char* KLBL_DETACH		= "D",  *CODE_DETACH		    = "D";
const char* KLBL_SCALE		= "j",  *CODE_SCALE		    = "j";
const char* KLBL_STRETCH	= ";",  *CODE_STRETCH		    = ";";
const char* KLBL_ROTATE		= "k",  *CODE_ROTATE		    = "k";
const char* KLBL_RESHAPE	= "q",	*CODE_RESHAPE		    = "q";
const char* KLBL_MAGNIFY	= "z",  *CODE_MAGNIFY		    = "z";
