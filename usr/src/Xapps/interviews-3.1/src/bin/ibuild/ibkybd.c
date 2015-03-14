/*
 * Copyright (c) 1991 Stanford University
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
 * User interface builder keycode values.
 */

#include "ibkybd.h"

const char* KLBL_PMOVE          = "^M",  *CODE_PMOVE                = "\015";
const char* KLBL_PSCALE         = "^J",  *CODE_PSCALE               = "\012";
const char* KLBL_PROTATE        = "^K",  *CODE_PROTATE              = "\013";
const char* KLBL_ABOUT          = "$",   *CODE_ABOUT                 = "$";
const char* KLBL_CHECKBOX       = "",   *CODE_CHECKBOX               = "3";
const char* KLBL_EXAMINE        = "i",  *CODE_EXAMINE                = "i";
const char* KLBL_FBROWSER       = "",   *CODE_FBROWSER               = "4";
const char* KLBL_HBORDER        = "",   *CODE_HBORDER                = "5";
const char* KLBL_HBOX           = "h",  *CODE_HBOX                   = "h";
const char* KLBL_HGLUE          = "",   *CODE_HGLUE                  = "6";
const char* KLBL_HIDEGLUE       = "<",  *CODE_HIDEGLUE               = "<";
const char* KLBL_HSCROLLER      = "",   *CODE_HSCROLLER              = "8";
const char* KLBL_MANIP          = "",   *CODE_MANIP                  = "9";
const char* KLBL_MESSAGE        = "",   *CODE_MESSAGE                = ".";
const char* KLBL_NATURALSIZE    = "N",  *CODE_NATURALSIZE            = "N";
const char* KLBL_NEWVIEW        = "V",  *CODE_NEWVIEW                = "V";
const char* KLBL_PUSHBUTTON     = "",   *CODE_PUSHBUTTON             = "!";
const char* KLBL_RADIOBUTTON    = "",   *CODE_RADIOBUTTON            = ",";
const char* KLBL_SHOWGLUE       = ">",  *CODE_SHOWGLUE               = ">";
const char* KLBL_STREDIT        = "",   *CODE_STREDIT                = "]";
const char* KLBL_VBORDER        = "",   *CODE_VBORDER                = "[";
const char* KLBL_VBOX           = "j",  *CODE_VBOX                   = "j";
const char* KLBL_VGLUE          = "",   *CODE_VGLUE                  = "}";
const char* KLBL_VSCROLLER      = "",   *CODE_VSCROLLER              = "{";
const char* KLBL_NEWTOOL        = "^T", *CODE_NEWTOOL                = "\024";
const char* KLBL_TOOLS          = "T",  *CODE_TOOLS                  = "T";
const char* KLBL_EXE            = "e",  *CODE_EXE                    = "e";
const char* KLBL_LMOVER         = "",   *CODE_LMOVER                 = "+";
const char* KLBL_RMOVER         = "",   *CODE_RMOVER                 = "\"";
const char* KLBL_UMOVER         = "",   *CODE_UMOVER                 = "-";
const char* KLBL_DMOVER         = "",   *CODE_DMOVER                 = "_";
const char* KLBL_ENLARGER       = "",   *CODE_ENLARGER               = ")";
const char* KLBL_REDUCER        = "",   *CODE_REDUCER                = "(";
const char* KLBL_PANNER         = "",  *CODE_PANNER                  = "\\";
const char* KLBL_GRBLOCK        = "",  *CODE_GRBLOCK                 = "?";

const char* KLBL_FRAME          = "F",  *CODE_FRAME                  = "F";
const char* KLBL_SHADOWFRAME    = "w",  *CODE_SHADOWFRAME            = "w";
const char* KLBL_MARGINFRAME    = "",   *CODE_MARGINFRAME            = "&";
const char* KLBL_DECK    	= "D",  *CODE_DECK             	     = "D";

const char* KLBL_RELATE         = "l",  *CODE_RELATE                 = "l";
const char* KLBL_MENUITEM       = "",   *CODE_MENUITEM               = "%";
const char* KLBL_PDCMD          = "",   *CODE_PDCMD                  = ":";
const char* KLBL_PRCMD          = "",   *CODE_PRCMD                  = "#";
const char* KLBL_MBCMD          = "M",  *CODE_MBCMD                  = "M";
const char* KLBL_SLIDER         = "",   *CODE_SLIDER                 = "@";
const char* KLBL_REORDER        = "o",  *CODE_REORDER                = "o";
const char* KLBL_PUCMD          = "^P", *CODE_PUCMD                  = "\020";
const char* KLBL_TEXTEDITOR     = "",   *CODE_TEXTEDITOR             = "~";
const char* KLBL_VIEWPORT       = "O",  *CODE_VIEWPORT               = "O";
const char* KLBL_MSCLASS        = "^L", *CODE_MSCLASS                = "\014";
const char* KLBL_DIALOGCLASS    = "^G", *CODE_DIALOGCLASS            = "\07";
const char* KLBL_SHAPER         = "S",  *CODE_SHAPER                 = "S";
const char* KLBL_VIEWPARENT     = "^",  *CODE_VIEWPARENT             = "^";
const char* KLBL_VIEWROOT       = "*",  *CODE_VIEWROOT               = "*";
const char* KLBL_STRBROWSER     = "",   *CODE_STRBROWSER             = "|";
const char* KLBL_NARROW         = "p",  *CODE_NARROW                 = "p";
const char* KLBL_TAB            = "\t", *CODE_TAB                    = "\011";
const char* KLBL_EDCOMP         = "E",  *CODE_EDCOMP                 = "E";
const char* KLBL_IBVIEWER       = "",   *CODE_IBVIEWER               = "\128";
const char* KLBL_HPCTRL         = "",   *CODE_HPCTRL                 = "\129";
const char* KLBL_VPCTRL         = "",   *CODE_VPCTRL                 = "\130";
const char* KLBL_COMMANDCTRL    = "",   *CODE_COMMANDCTRL            = "\131";
