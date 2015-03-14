#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Xw/Xw.h>
#include <Xw/SText.h>

extern char *concat_arg();

main(argc, argv)
     int argc;
     char *argv[];
{
  Widget toplevel, msg_widget;
  Arg wargs[1];
  int n;
  char *message;

  toplevel = XtInitialize(argv[0], "Memo" NULL, 0,
			  &argc, argv);


  n = 0;
  XtSetArg(wargs[n], XtNstring, "Hello World"); n++;


  msg_widget = XtCreateManagedWidget("message",
				     XwstatictextWidgetClass,
				     toplevel, wargs, n);

  XtRealizeWidget(toplevel);
  XtMainLoop();
}
