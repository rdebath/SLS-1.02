/*
 * $XConsortium: Dialog.h,v 1.5 91/02/22 18:20:14 converse Exp $
 *
 * Copyright 1989 Massachusetts Institute of Technology
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
 * Author:  Davor Matic, MIT X Consortium
 */


/*#define None   0*/
#define Yes    1<<1
#define No     1<<2
#define Maybe  1<<3  /* :-) */
#define Okay   1<<4
#define Abort  1<<5
#define Cancel 1<<6
#define Retry  1<<7

typedef struct {
  Widget top_widget, shell_widget, dialog_widget;
  unsigned long options;
} _Dialog, *Dialog;

typedef struct {
    String name;
    unsigned long flag;
} DialogButton;

extern Dialog CreateDialog();
extern unsigned long PopupDialog();
extern void PopdownDialog();
