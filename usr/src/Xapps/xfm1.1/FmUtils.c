/*****************************************************************************/
/* Module FmUtils.c                                                          */
/*                                                                           */
/* General utility functions for creating menus, buttons, questions,         */
/* and functions for desensetising and 'ticking' menu entries.               */
/*****************************************************************************/

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/SmeLine.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/AsciiText.h>

#include "Am.h"
#include "Fm.h"

#define PADDING 20
#define TEXT_WIDTH 200

/*****************************************************************************/
/*                          STATIC DATA                                      */
/*****************************************************************************/

/*****************************************************************************/
/* Widget argument lists                                                     */
/*****************************************************************************/

static Arg shell_args[] = {
  { XtNtitle, (XtArgVal) NULL }
};

static Arg form_args[] = {
  { XtNdefaultDistance, PADDING }
};

static Arg bitmap_args[]  = {
  { XtNfromHoriz, (XtArgVal) NULL },
  { XtNfromVert, (XtArgVal) NULL },
  { XtNbitmap, (XtArgVal) NULL },
  { XtNtop, (XtArgVal) XtChainTop },
  { XtNbottom, (XtArgVal) XtChainTop },
  { XtNleft, (XtArgVal) XtChainLeft },
  { XtNright, (XtArgVal) XtChainLeft }
};

static Arg label_args[] = {
  { XtNfromHoriz, (XtArgVal) NULL },
  { XtNfromVert, (XtArgVal) NULL },
  { XtNlabel, (XtArgVal) NULL },
  { XtNwidth, (XtArgVal) 0 },
  { XtNfont, (XtArgVal) NULL },
  { XtNjustify, XtJustifyRight },
  { XtNinternalWidth, (XtArgVal) 0 },
  { XtNinternalHeight, (XtArgVal) 0 },
  { XtNtop, XtChainTop },
  { XtNbottom, XtChainTop },
  { XtNleft, XtChainLeft },
  { XtNright, XtChainLeft }
};

static Arg text_args[] = {
  { XtNfromHoriz, (XtArgVal) NULL },
  { XtNfromVert, (XtArgVal) NULL },
  { XtNstring, (XtArgVal) NULL },
  { XtNlength, (XtArgVal) NULL },
  { XtNwidth, (XtArgVal) TEXT_WIDTH },
  { XtNfont, (XtArgVal) NULL },
  { XtNtop, XtChainTop },
  { XtNbottom, XtChainTop },
  { XtNleft, XtChainLeft },
  { XtNright, XtChainRight },
  { XtNeditType, XawtextEdit },
  { XtNtype, XawAsciiString },
  { XtNuseStringInPlace, (XtArgVal) True },
};

static Arg button_box_args[] = {
  { XtNfromHoriz, (XtArgVal) NULL },
  { XtNfromVert, (XtArgVal) NULL },
  { XtNtop, XtChainTop },
  { XtNbottom, XtChainTop },
  { XtNleft, XtChainLeft },
  { XtNright, XtChainLeft }
};

static Arg button_args[] = {
  { XtNlabel, (XtArgVal) NULL },
  { XtNfont, (XtArgVal) NULL }
};

static Arg menu_button_args[] = {
  { XtNlabel, (XtArgVal) NULL },
  { XtNfont, (XtArgVal) NULL }
};

static Arg menu_item_args[] = {
  { XtNlabel, (XtArgVal) NULL },
  { XtNfont, (XtArgVal) NULL },
  { XtNleftMargin , (XtArgVal) 0 }
};

/*****************************************************************************/
/*                          PUBLIC FUNCTIONS                                 */
/*****************************************************************************/

/*****************************************************************************/
/* Function: initUtils                                                       */
/* Arguments: None                                                           */
/* Initialise the utilities module (setting up fonts)                        */
/*****************************************************************************/

void initUtils()
{
  button_args[1].value = (XtArgVal) resources.button_font;
  menu_button_args[1].value = (XtArgVal) resources.button_font;
  menu_item_args[1].value = (XtArgVal) resources.menu_font;
  label_args[4].value = (XtArgVal) resources.label_font;
  text_args[5].value = (XtArgVal) resources.cell_font;
};

/*****************************************************************************/
/* Function: createMenu                                                      */
/* Arguments: menu_name   :  The menu widget name                            */
/*            menu_label  :  The label for the menu button                   */
/*            items       :  Items to put in menu                            */
/*            n_items     :  Number of items                                 */
/*            left_margin :  left_margin in pixels (in case ticks are needed */
/*            parent      :  The parent widget to use                        */
/*            client_data :  Client data to be returned by any callback      */
/*                                                                           */
/* Create a menu with the specified attributes and place in it the           */
/* specifed items                                                            */
/*****************************************************************************/

Widget *createMenu(String menu_name, String menu_label, MenuItemList items,
		   Cardinal n_items, Dimension left_margin,
		   Widget parent, XtPointer client_data)
{
  register int i;
  Widget menu_widget, button_widget, *item_widgets;
  
  item_widgets = (Widget *) XtMalloc(n_items * sizeof(Widget));

  menu_button_args[0].value = (XtArgVal) menu_label;
  button_widget = XtCreateManagedWidget(menu_name, menuButtonWidgetClass,
    parent, menu_button_args, XtNumber(menu_button_args));
  menu_widget = XtCreatePopupShell( "menu", simpleMenuWidgetClass,
      button_widget, NULL, 0 );
    
  menu_item_args[2].value = (XtArgVal) left_margin;

  for (i = 0; i < n_items; i++) {
    if (items[i].callback == NULL)
      XtCreateManagedWidget(items[i].item_name, smeLineObjectClass,
			    menu_widget, NULL, 0 );
    else {
      menu_item_args[0].value = (XtArgVal) items[i].item_label;
      item_widgets[i] = XtCreateManagedWidget(items[i].item_name,
					      smeBSBObjectClass, menu_widget,
					      menu_item_args, 
					      XtNumber(menu_item_args));
      XtAddCallback(item_widgets[i], XtNcallback, 
		    (XtCallbackProc) items[i].callback, client_data );
    }
  }

  return item_widgets;
}

/*****************************************************************************/
/* Function: createButtons                                                   */
/* Arguments: buttons     :  The list of buttons to create                   */
/*            n_buttons   :  Number of buttons                               */
/*            parent      : The parent widget to use                         */
/*            client data :  Client data returned by all buttons             */
/*                                                                           */
/* Create a set of buttons (usually in a box) with the attributes specified  */
/*****************************************************************************/


Widget *createButtons(ButtonList buttons, Cardinal n_buttons, Widget parent,
		   XtPointer client_data)
{
  register int i;
  Widget *button_widgets;
  
  button_widgets = (Widget *) XtMalloc(n_buttons * sizeof(Widget));

  for (i = 0; i < n_buttons; i++) {
    button_args[0].value = (XtArgVal) buttons[i].button_label;
    button_widgets[i] = XtCreateManagedWidget(buttons[i].button_name,
      commandWidgetClass, parent, button_args, XtNumber(button_args));
    XtAddCallback(button_widgets[i], XtNcallback, 
		  (XtCallbackProc) buttons[i].callback, client_data );
  }

  return button_widgets;
}

/*****************************************************************************/
/* Function: createPopupQuestions                                            */
/* Arguments: name        :  The widget name for the shell                   */
/*            title       :  The title of the popup window                   */
/*            bitmap      :  A bitmap to display to the left of the box      */
/*            questions   :  A list of questions to use                      */
/*            n_questions :  Number of questions                             */
/*            buttons     :  A set of buttons to put at the bottom           */
/*            n_buttons   :  Number of buttons                               */
/*                                                                           */
/* Create a popup questionaire with a bitmap to the left (or none), several  */
/* questions (each consisting of a label and a text area) to the right of    */
/* the bitmap, and a set of buttons underneath all this.                     */
/*****************************************************************************/

Widget createPopupQuestions(String name, String title, Pixmap bitmap,
			    QuestionList questions, Cardinal n_questions,
			    ButtonList buttons, Cardinal n_buttons)
{
  register int i, l;
  Widget form_widget, box_widget, bitmap_widget = NULL, shell,
    vert = NULL, horiz = NULL;

  /* create popup shell */
  shell_args[0].value = (XtArgVal) title;
  shell = XtCreatePopupShell(name, transientShellWidgetClass, aw.shell,
			     shell_args, XtNumber(shell_args) );

  /* create form */
  form_widget = XtCreateManagedWidget("popup form", formWidgetClass, shell, 
				      form_args, XtNumber(form_args) );

  /* create bitmap */
  if (bitmap != None) {
    bitmap_args[2].value = (XtArgVal) bitmap;
    bitmap_widget = XtCreateManagedWidget("bitmap", labelWidgetClass,
	  form_widget, bitmap_args, XtNumber(bitmap_args));
  }

  /* Find width of label */
  label_args[3].value = (XtArgVal) 0;
  for (i=0; i<n_questions; i++) {
    l = XTextWidth(resources.label_font, questions[i].label, 
		   strlen(questions[i].label));
    if (l > label_args[3].value)
      label_args[3].value = l;
  }

  for (i = 0; i<n_questions; i++) {
    label_args[0].value = (XtArgVal) bitmap_widget;
    label_args[1].value = (XtArgVal) vert;
    label_args[2].value = (XtArgVal) questions[i].label;
    horiz = XtCreateManagedWidget("label", labelWidgetClass, form_widget,
					 label_args, XtNumber(label_args));
    if (n_questions == 1) {
      text_args[0].value = (XtArgVal) bitmap_widget;
      text_args[1].value = (XtArgVal) horiz;
    }
    else {
      text_args[0].value = (XtArgVal) horiz;
      text_args[1].value = (XtArgVal) vert;
    }      
    text_args[2].value = (XtArgVal) questions[i].value;
    text_args[3].value = (XtArgVal) questions[i].length;
    vert = questions[i].widget = XtCreateManagedWidget("text", 
	   asciiTextWidgetClass, form_widget, text_args, XtNumber(text_args));
  }

  if (buttons != NULL) {
    button_box_args[0].value = (XtArgVal) NULL;
    button_box_args[1].value = (XtArgVal) vert;
    box_widget = XtCreateManagedWidget("button box", boxWidgetClass, 
	        form_widget, button_box_args, XtNumber(button_box_args));
    createButtons(buttons, n_buttons, box_widget, NULL);
  }

  XtRealizeWidget(shell);

  return shell;
}

/*****************************************************************************/
/* Function: fillIn                                                          */
/* Arguments: w : The widget to fill in                                      */
/*                                                                           */
/* sensitize a menu entry                                                    */
/*****************************************************************************/

void fillIn(Widget w)
{
  XtVaSetValues(w, XtNsensitive, (XtArgVal) True, NULL);
}

/*****************************************************************************/
/* Function: grayOut                                                         */
/* Arguments: w : the widget to gray out                                     */
/*                                                                           */
/* desensitises a menu entry                                                 */
/*****************************************************************************/

void grayOut(Widget w)
{
  XtVaSetValues(w, XtNsensitive, (XtArgVal) False, NULL);
}

/*****************************************************************************/
/* Function: tick                                                            */
/* Arguments: w : the widget to tick                                         */
/*                                                                           */
/* place a tick to the left of the specifed menu entry                       */
/*****************************************************************************/

void tick(Widget w)
{
  XtVaSetValues(w, XtNleftBitmap, (XtArgVal) bm[TICK_BM], NULL);
}

/*****************************************************************************/
/* Function: notick                                                          */
/* Arguments: w : the widget                                                 */
/*                                                                           */
/* remove a tick from a menu entry                                           */
/*****************************************************************************/

void noTick(Widget w)
{
  XtVaSetValues(w, XtNleftBitmap, (XtArgVal) bm[NOTICK_BM], NULL);
}

/*****************************************************************************/
/* Function: popupByCursor                                                   */
/* Arguments: shell       :  the shell to popup                              */
/*            grab_kind   :  parameter passed to XtPopup                     */
/*                                                                           */
/* Try to popup a shell by the cursor                                        */
/*****************************************************************************/

void popupByCursor(Widget shell, XtGrabKind grab_kind)
{
  Display *dpy;
  Window root, child;
  int x_root, y_root, x_win, y_win;
  Dimension width, height;
  unsigned int mask;

  dpy = XtDisplay(aw.shell);

  XQueryPointer(dpy, DefaultRootWindow(dpy), &root, &child, &x_root, &y_root, 
		&x_win, &y_win, &mask);

  XtVaGetValues(shell, XtNwidth, &width, XtNheight, &height, NULL);

  x_root -= width/2;
  y_root -= height/2;

  XtVaSetValues(shell, XtNx, (XtArgVal) x_root, XtNy, (XtArgVal) y_root, NULL);

  XtPopup(shell, grab_kind);
}
  
/*****************************************************************************/

void zzz(void)
{
  register FileWindowRec *fw;
  Display *dpy = XtDisplay(aw.shell);

  for (fw = file_windows; fw; fw = fw->next)
    XDefineCursor(dpy, XtWindow(fw->viewport), curs[WATCH_CUR]);

  XDefineCursor(dpy, XtWindow(aw.shell), curs[WATCH_CUR]);
}

void wakeUp(void)
{
  register FileWindowRec *fw;
  Display *dpy = XtDisplay(aw.shell);

  for (fw = file_windows; fw; fw = fw->next)
    XUndefineCursor(dpy, XtWindow(fw->viewport));

  XUndefineCursor(dpy, XtWindow(aw.shell));
}
