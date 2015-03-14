/* xkeycaps, Copyright (c) 1991, 1992 Jamie Zawinski <jwz@lucid.com>
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation.  No representations are made about the suitability of this
 * software for any purpose.  It is provided "as is" without express or 
 * implied warranty.
 */


#ifndef _KeyWidget_H_
#define _KeyWidget_H_

typedef struct _KeyRec *KeyWidget;
typedef struct _KeyClassRec *KeyWidgetClass;

extern WidgetClass keyWidgetClass;

#if __STDC__
extern void KeyHighlight (KeyWidget);
extern void KeyDehighlight (KeyWidget);
#endif

#endif /* _KeyWidget_H_ */
