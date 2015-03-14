#include <X11/Xlib.h>
#include <X11/Xutil.h>

main(argc, argv)
char *argv[];
{
   Display     *display;
   int         screen;
   XSizeHints  sizehints;      
   Window      window;
   XEvent      event;
   XButtonEvent *button;
   XColormapEvent *colormap;
   short	i;
   int         done = 0;
   Colormap    cmap, orig_cmap;
   XVisualInfo   vTemplate, *visualList;
   int          visualsMatched;
   Visual       *visual;
   XSetWindowAttributes atts;
   XColor      color;

   display = XOpenDisplay("");
   screen = DefaultScreen ( display );
   window = XDefaultRootWindow(display);

   vTemplate.screen = screen;
   vTemplate.depth = 8;
   vTemplate.class = PseudoColor;
   visualList = XGetVisualInfo(display, 
      VisualScreenMask|VisualClassMask|VisualDepthMask,
      &vTemplate, &visualsMatched);
   if ( visualsMatched == 0 ) {
      puts("visual not found, using default");
      visual = DefaultVisual( display, screen );
   } else {
      printf("found %d visuals\n", visualsMatched);
      visual = visualList[0].visual;
   }

   sizehints.x = 200;
   sizehints.y = 0;
   sizehints.width = sizehints.height = 150;
   sizehints.flags = USSize | USPosition;

   cmap = XCreateColormap( display,window, visual, AllocAll);
   orig_cmap = DefaultColormap(display, DefaultScreen(display));
   atts.colormap = cmap;

   window = XCreateWindow(display,DefaultRootWindow(display), 
            sizehints.x, sizehints.y, sizehints.width, sizehints.height,
	    0, 8, InputOutput, visual, CWColormap, &atts);

   XSetStandardProperties(display, window, argv[0],
		      argv[0], None, argv, 1, &sizehints);

   color.flags = DoRed|DoGreen|DoBlue;
   srandom(time(0));
   printf("Allocating %d colors\n", visual->map_entries);
   for (color.pixel=0; color.pixel < visual->map_entries; color.pixel++) {
      color.red =   random()%65536;
      color.green = random()%65536;
      color.blue =  random()%65536;
      XStoreColor ( display, cmap, &color );
   }

   while (visualsMatched > 0)
       XFree(visualList[--visualsMatched]);

   /*  Now that the screen is defined, select inputs and get them  */
   XSelectInput(display, window, ButtonPressMask | ColormapChangeMask);
   XMapWindow(display, window);

   do {
      XNextEvent ( display, &event );
      switch (event.type) {
	  case ButtonPress:
	     printf ("*********  Found ButtonPress event ********\n");
	     button = (XButtonEvent *) &event;
	     printf ("button is %d\n", button->button );
		 if (button->button == 3)
		    done = -1;
	     break;
	  case ColormapNotify:
	     printf ("*********  Found ColormapNotify event ********\n");
	     colormap = (XColormapEvent *) &event;
	     if ( colormap->state == ColormapInstalled )
		 printf ("Colormap has been installed\n");
	     else 
		 printf ("Colormap has been uninstalled\n");
	     printf ("Colormap is %d\n", colormap->colormap );
	     printf ("Associated window is %d\n", colormap->window );
      }
   } while (!done);
   XCloseDisplay(display);
}
