/*
 * Sample application that communicates with the Function Keys process
 */
#include <ctype.h>
#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>
#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/font.h>


Display        *dpy;
GC              gc;
XFontStruct    *font_info;
Frame           frame;
Canvas          canvas;
static int      function_mode = 2;
char           *prev_char;

char           *soft_labels[4] = {"Brick\nBlue\nMaroon\nOrchid\nViolet\nBlack\nGreen\n\nMagenta\nRed\nClear\n\n\n",
                                  "Coral\nNavy\nOlive\nTurquoise\nYellow\n \n \n \n \nClear\nFonts\nMore\n",
                                  "Fixed\nBembo\nTimes\nCourier\nLucida\n9x15\nGillsans\nPalatino\nScreen\nClear\n\n\n",
                                  "Zapfding\nHelvetica\nBookman\nVtsingle\nTerminal\n \n \n \n \nClear\nColors\nMore\n"
                                 };

char           *colors_fonts[49] = {"Empty",
                                    "FireBrick", "Blue", "Maroon", "Orchid", "Violet Red", "Black", "Green", "Magenta", "Red", "Clear", "Fonts", "More",
                                    "Coral","Navy","Olive Drab","Turquoise", "Yellow",0,0,0,0,"Clear","Fonts","More",

                                    "fixed", "bembo-bold-14", "times-bold-14", "courier-bold-14", "lucidasans-bold-14", "9x15", "gillsans-bold-14", "palatino-bold-14", "screen-bold","Clear","Colors","More",
                                    "zapfdingbats-14","helvetica-bold","bookman-demi","vtsingle","terminal",0,0,0,0,"Clear","Colors","More"
				      };

short          icon_bits[]  = {
#include "textdemo.icon"
                            };

main(argc, argv)
     int             argc;
     char           *argv[];
{
  XGCValues       gcvalues;
  Xv_Font         font;
  void            my_event_proc();
  extern void     exit();
  Notify_error    notify_interpose_event_func();
  Notify_value    canvas_interposer();
  void            put_labels();
  Icon            icon;
  Server_image    icon_image;
  
  xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);
  
  frame = (Frame) xv_create(XV_NULL, FRAME,
			    FRAME_LABEL, "Virtual Keyboard - Fonts - Demo",
			    NULL);

  icon_image = (Server_image)xv_create(NULL,SERVER_IMAGE,
                                       XV_WIDTH,64,
                                       XV_HEIGHT,64,
                                       SERVER_IMAGE_BITS,icon_bits,
                                       NULL);
  
  icon =  (Icon) xv_create(NULL,ICON,
                          ICON_IMAGE, icon_image,
                          ICON_MASK_IMAGE, icon_image,
                          ICON_TRANSPARENT,TRUE,
                          NULL);
  
  canvas = (Canvas) xv_create(frame, CANVAS,
			      XV_WIDTH, 400,
			      XV_HEIGHT, 400,
			      CANVAS_X_PAINT_WINDOW, TRUE,
			      NULL);
  
  
  xv_set(canvas_paint_window(canvas),
         WIN_SOFT_FNKEY_LABELS,soft_labels[2],
	 WIN_EVENT_PROC, my_event_proc,
	 NULL);
  
  window_fit(frame);
  
  dpy = (Display *) xv_get(frame, XV_DISPLAY);
  
  xv_set(frame,FRAME_ICON,icon,NULL);
  
  font = (Xv_Font) xv_get(frame, XV_FONT);
  font_info = (XFontStruct *) xv_get(font, FONT_INFO);
  
  gcvalues.font = (Font) xv_get(font, XV_XID);
  gcvalues.foreground = BlackPixel(dpy, DefaultScreen(dpy));
  gcvalues.background = WhitePixel(dpy, DefaultScreen(dpy));
  gcvalues.graphics_exposures = False;
  gc = XCreateGC(dpy, RootWindow(dpy, DefaultScreen(dpy)),
		 GCForeground | GCBackground | GCFont | GCGraphicsExposures, &gcvalues);
  
  xv_main_loop(frame);
  
}

void
  my_event_proc(win, event)
Xv_Window       win;
Event          *event;

{
  static int      x = 10, y = 15;
  Window          xwin = (Window) xv_get(win, XV_XID);
  char            c;
  void            change_attributes();
  
  
  if (event_is_up(event))
    return;
  
  else {			/* down event */
    
    if (event_is_key_top(event)) {
      switch (event_id(event)) {
	
      case KEY_TOP(1):
	change_attributes(1 + (12*function_mode));
	break;
      case KEY_TOP(2):
	change_attributes(2 + (12*function_mode));
	break;
      case KEY_TOP(3):
	change_attributes(3 + (12*function_mode));
	break;
      case KEY_TOP(4):
	change_attributes(4 + (12*function_mode));
	break;
      case KEY_TOP(5):
	change_attributes(5 + (12*function_mode));
	break;
      case KEY_TOP(6):
	change_attributes(6 + (12*function_mode));
	break;
      case KEY_TOP(7):
	change_attributes(7 + (12*function_mode));
	break;
      case KEY_TOP(8):
	change_attributes(8 + (12*function_mode));
	break;
      case KEY_TOP(9):
	change_attributes(9 + (12*function_mode));
	break;
      case KEY_TOP(10):/* Clear */
	XClearWindow(dpy, xwin);
	x = 10;
	y = 15;
	break;
      }
      
    } else if (event_action(event) == ACTION_SELECT) {
      x = event_x(event);
      y = event_y(event);

   } else if (event_id(event) < 256)  {

      c = (char) event_id(event);
      if (c == '\n' || c == '\r') {
	
	y += font_info->max_bounds.ascent +
	  font_info->max_bounds.descent;
	x = 10;
	
      } else if (c == 7 || c == 127) {	/* backspace or delete */
      	
	if (x > 10)
	  x -= XTextWidth(font_info, "m", 1);

	
	/* use XDrawImageString to overwrite previous text */
	XDrawImageString(dpy, xwin, gc, x, y,"   ", 3);
	
      } else {
	
	XDrawString(dpy, xwin, gc, x, y, &c, 1);
	x += XTextWidth(font_info, &c, 1);
	
      }
    }
  }
}


      
void
  change_attributes(attr)
int             attr;

{
  XColor          exact, color;
 

  if (soft_labels[attr])  {
  
  if (attr < 24) {
    
    if (XAllocNamedColor(dpy, DefaultColormap(dpy, DefaultScreen(dpy)),
			 colors_fonts[attr], &exact, &color) == 0) {
      fprintf(stderr, "could not allocate color %s\n", colors_fonts[attr]);
      return;
    } else
      XSetForeground(dpy, gc, color.pixel);
    
  } else {

    if ((font_info = XLoadQueryFont(dpy, colors_fonts[attr])) == 0) {
      fprintf(stderr, "could not load font %s\n", colors_fonts[attr]);
      return;
    } else {
      XSetFont(dpy, gc, font_info->fid);
    }
    
  }
  }
  return;
  
}
