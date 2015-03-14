/* X include files */

#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/X.h>
#include <X11/Xresource.h>
#include <strings.h>

#include "covered.pat"
#include "uncovered.pat"
#include "gray.pat"
#include "uncoveredcolor.pat"
#include "mine.pat"
#include "highlight.pat"
#include "ohno.pat"
#include "smiley.pat"
#include "shades.pat"
#include "marked.pat"

#include "xmines.h"

extern int score;
extern int height;
extern int width;

char *getenv();

char fontname[80] = "8x13bold";
Display *display;
int screen;
Window  window;
Window  hswindow = 0;
XEvent event;
XSizeHints hint;
GC gc,hsgc;
GC highlightgc;
XFontStruct *xfontinfo;
KeySym  key;
Pixmap graypix;
Pixmap flagpix;
Pixmap minepix;
Pixmap boompix;
Pixmap ohnopix;
Pixmap highpix;
Pixmap markpix;
Pixmap missmarkpix;
Pixmap uncoveredpix;
Pixmap coveredpix;
Pixmap smileypix;
Pixmap patpix[16];
XColor exact_def;
Colormap cmap;
unsigned long foreground, background,patbg,patfg;
unsigned long getcolor();
char colorname[10][64] =
{
    "white","red","blue","green","yellow","cyan","magenta",
    "orange","violet"
};
int depth;
int pauseflag = 0;
int volume;

#define SWIDTH  20
#define SHEIGHT 20
#define BWIDTH  14
#define XOFFSET 14
#define YOFFSET 80
#define BHEIGHT 14

char DisplayName[256];
int showscores = 0;

typedef struct S_BUTTON
{
    int x,y;
    int w,h;
    char name[80];
    int (*func)();
} BUTTON;

int paws();
int quit();
int show_high_scores();

#define NUMBUTTONS 3
BUTTON button[NUMBUTTONS] = 
{
    {XOFFSET+10*SWIDTH+10,280,40,20,"Scores",show_high_scores},
    {XOFFSET+10*SWIDTH+10,310,40,20,"Pause",paws},
    {XOFFSET+10*SWIDTH+10,340,40,20,"Quit",quit}
};

paws()
{
    extern int start_time;
    static int pause_start;
    if (pauseflag) {
	start_time += (time(0) - pause_start);
    }
    else {
	pause_start = time(0);
    }
    pauseflag = pauseflag ^ 1;
    draw_screen();
}

quit()
{
    exit(1);
}

static int opTableEntries = 2;
static XrmOptionDescRec opTable[] =
{
{"-display", ".display", XrmoptionSepArg, (caddr_t) NULL},
{"-volume",  ".volume", XrmoptionSepArg, (caddr_t) NULL}
};

static XrmDatabase rDB;
static XrmDatabase homeDB;
static XrmDatabase commandlineDB;

parseOptions(argc,argv)
    int argc;
    char *argv[];
{
    XrmValue value;
    char *str_type[40];
    char str[80];
    char buffer[80];
    char filename[1024];
    char *sptr;
    int i;

    /* Get command line options */
    XrmParseCommand(&commandlineDB,opTable,opTableEntries, "xmines",
        &argc, argv);
    DisplayName[0] = '\0';

    if(XrmGetResource(commandlineDB, "xmines.display",
        "Xcol.Display", str_type, &value) == True)
    {
        strncpy(DisplayName,value.addr,(int) value.size);
    }

    sptr = getenv("HOME");
    strncpy(filename,sptr,sizeof(filename));
    strncat(filename,"/.Xdefaults",sizeof(filename) - strlen(filename));
    filename[sizeof(filename)-1] = '\0';
    homeDB = XrmGetFileDatabase(filename);
    XrmMergeDatabases(homeDB,&rDB);
    XrmMergeDatabases(commandlineDB,&rDB);

    if(XrmGetResource(rDB, "xcol.volume",
        "Xcol.Volume", str_type, &value) == True)
    {
        strncpy(buffer,value.addr,(int) value.size);
        buffer[value.size] = '\0';
        volume = atoi(buffer);
        if(0 <= volume && volume <= 100)
        {
            volume = 2*volume - 100;
        }
    }
    else
    {
        volume = 0;
    }
    for(i=1; i < 9; i++)
    {
        sprintf(str,"xcol.color%1d",i);
        if(XrmGetResource(rDB, str,
            "Xcol.Color", str_type, &value) == True)
        {
            strncpy(colorname[i],value.addr,(int) value.size);
            colorname[i][value.size] = '\0';
        }
    }
}

char *pixstr[] = 
{
    "0","1","2","3","4","5","6","7","8","9","10"
};

int hsdone;

XStuff(argc,argv)
    int argc;
    char *argv[];
{
    int i;
    unsigned long bg;


    /* initialization */
    if(!(display = XOpenDisplay(DisplayName)))
    {
        fprintf(stderr,"Error: can't open display\n");
        exit(1);
    }
    screen = DefaultScreen(display);
    depth = DefaultDepth(display,screen);

    /* default pixel values */
    cmap = DefaultColormap(display,screen);

    background = WhitePixel(display,screen);
    foreground = BlackPixel(display,screen);
    bg = getcolor("gray",background);

    /* default program-specified window position and size */
    hint.x = 20; hint.y = 30;
    hint.width = width*SWIDTH+BWIDTH+XOFFSET;
    hint.height = (height)*SHEIGHT+YOFFSET+BHEIGHT;
    hint.flags = PPosition | PSize;

    /* window creation */
    window = XCreateSimpleWindow (display,
                DefaultRootWindow(display),
                hint.x, hint.y, hint.width, hint.height,
                5, foreground, bg);
    XSetStandardProperties(display, window,"xmines", "xmines",
        None, argv, argc, &hint);

    /* GC creation and initialization */
    gc = XCreateGC(display, window, 0, 0);
    XSetBackground(display, gc, background);
    XSetForeground(display, gc, foreground);
    highlightgc = XCreateGC(display, window, 0, 0);

    if((xfontinfo = XLoadQueryFont(display,fontname)) == NULL)
    {
        fprintf(stderr,"Can't find font '%s'\n",fontname);
        xfontinfo = XQueryFont(display,XGContextFromGC(gc));
    }
    XSetFont(display,gc,xfontinfo->fid);

    /* input event selection */
    XSelectInput(display, window,
        Button1MotionMask | Button2MotionMask | Button3MotionMask |
         ButtonPressMask | ButtonReleaseMask | 
        KeyPressMask | ExposureMask);

    CreatePixmaps();

    if(depth == 1)
    {
        XSetWindowBackgroundPixmap(display,window,graypix);
    }

    /* window mapping */
    XMapRaised(display, window);
    XFlush(display);
}

CreatePixmaps()
{
    int i;

    for(i=0; i < 9; i++)
    {
        if(depth != 1)
        {
            patbg = getcolor("gray",background);
            patpix[i] = XCreatePixmapFromBitmapData(display,window,
                uncoveredcolor_bits,SWIDTH,SHEIGHT,foreground,
                patbg, depth);
            
        }
        else
        {
            patpix[i] = XCreatePixmapFromBitmapData(display,window,
                uncovered_bits,SWIDTH,SHEIGHT,foreground,
                background, depth);
        }
        if(i != 0)
        {
            patfg = getcolor(colorname[i],foreground);  
            XSetForeground(display, gc, patfg);
            XDrawString(display,patpix[i],gc,
            (SWIDTH-XTextWidth(xfontinfo,pixstr[i],strlen(pixstr[i])))/2,
            SHEIGHT - ((xfontinfo->ascent+xfontinfo->descent))/2,
            pixstr[i],1);
            XSetForeground(display, gc, foreground);
        }
        patbg = getcolor("gray",background);
        missmarkpix = XCreatePixmapFromBitmapData(display,window,
                    mine_bits,SWIDTH,SHEIGHT,foreground,
                    patbg, depth);
        minepix = XCreatePixmapFromBitmapData(display,window,
                    mine_bits,SWIDTH,SHEIGHT,foreground,
                    patbg, depth);
        XSetForeground(display, gc, getcolor("red",foreground));
        XDrawLine(display,missmarkpix,gc,1,1,SWIDTH-2,SHEIGHT-2);
        XDrawLine(display,missmarkpix,gc,1,SHEIGHT-2,SWIDTH-2,1);
        XSetForeground(display, gc, foreground);
        patbg = getcolor("red",foreground);
        patfg = getcolor("black",background);
        boompix = XCreatePixmapFromBitmapData(display,window,
                    mine_bits,SWIDTH,SHEIGHT,patfg,
                    patbg, depth);
        graypix = XCreatePixmapFromBitmapData(display,window,
                    gray_bits,SWIDTH,SHEIGHT,foreground,
                    background, depth);
        coveredpix = XCreatePixmapFromBitmapData(display,window,
                    covered_bits,SWIDTH,SHEIGHT,foreground,
                    background, depth);
        if(depth != 1)
        {
            XSetForeground(display, gc, getcolor("gray",foreground));

            XFillRectangle(display,coveredpix,gc,2,2,
                SWIDTH-4,SHEIGHT-4);
            XSetForeground(display, gc, foreground);
        }
        CreateMarked();

/*
        highpix = XCreatePixmapFromBitmapData(display,window,
                    highlight_bits,SWIDTH,SHEIGHT,foreground,
                    background, depth);
*/
        highpix = patpix[0];
        ohnopix = XCreatePixmapFromBitmapData(display,window,
                    ohno_bits,SWIDTH,SHEIGHT,foreground,
                    background, depth);
        smileypix = XCreatePixmapFromBitmapData(display,window,
                    smiley_bits,SWIDTH,SHEIGHT,foreground,
                    background, depth);
    }
}

XPoint flag[3] =
{
    {11,4},{11,10},{6,7}
};

XPoint base[3] =
{
    {7,16},{16,16},{11,12}
};

CreateMarked()
{
    GC gc;

    gc = XCreateGC(display, window, 0, 0);
    XSetBackground(display, gc, background);
    XSetForeground(display, gc, foreground);

    markpix = XCreatePixmapFromBitmapData(display,window,
                marked_bits,SWIDTH,SHEIGHT,foreground,
                background, depth);

    if(depth != 1)
    {
        XSetForeground(display, gc, getcolor("gray",foreground));

        XFillRectangle(display,markpix,gc,2,2,
                SWIDTH-4,SHEIGHT-4);
    }
    patfg = getcolor("red",foreground);  
    XSetForeground(display, gc, patfg);
    XFillPolygon(display,markpix,gc,flag,3,Convex,CoordModeOrigin);
    XSetForeground(display, gc, foreground);
    XDrawLine(display,markpix,gc,11,4,11,14);
    XFillPolygon(display,markpix,gc,base,3,Convex,CoordModeOrigin);
}

unsigned long getcolor(s,monocolor)
    char *s;
    unsigned long monocolor;
{
    if(depth != 1)
    {
        XParseColor(display,cmap,s,&exact_def);
        XAllocColor(display,cmap,&exact_def);
        return exact_def.pixel;
    }
    else
    {
        return monocolor;
    }
}

CheckforEvent()
{
    while((XPending(display)) || pauseflag)
    {
	  getXevent();
    }
}

getXevent()
{
    char text[10];
    int i,j;
    static int x,y;
    int newx,newy;

    XNextEvent(display, &event);
    switch(event.type)
    {
        case Expose:
            if(event.xexpose.window == window)
            {
                if(event.xexpose.count == 0)
                {
                    draw_screen();
                }
            }
            else if(event.xexpose.window == hswindow)
            {
                print_scores();
            }
            break;
        case KeyPress:
            if(event.xkey.window == window && !showscores)
            {
                i = XLookupString(&event.xkey, text, 1, &key, 0);
                keyboard(text[0]);
            }
            else if (event.xkey.window == hswindow)
            {
                hsdone = 1;
            }
            break;
        case MotionNotify:
            if(!pauseflag)
            {
                newx = (event.xmotion.x-XOFFSET)/SWIDTH;
                newy = (event.xmotion.y-YOFFSET)/SHEIGHT;
                if(newx != x || newy != y)
                {
    
                    if(event.xmotion.state & Button1Mask)
                    {
                        unHighlight(x,y);
                        Highlight(newx,newy);
                    }
                    else if(event.xmotion.state & Button2Mask)
                    {
                        unhighlightInvisible(x,y);
                        highlightInvisible(newx,newy);
                    }
                    x = newx; y = newy;
                }
            }
            break;
        case ButtonPress:
            if(event.xkey.window == window && !showscores && !pauseflag)
            {
                x = (event.xbutton.x-XOFFSET)/SWIDTH;
                y = (event.xbutton.y-YOFFSET)/SHEIGHT;
                if(InArray(x,y))
                {
                    switch(event.xbutton.button)
                    {
                        case Button1:
                            ohno();
                            Highlight(x,y);
                            break;
                        case Button2:
                            ohno();
                            highlightInvisible(x,y);
                            break;
                        default:
                            break;
                    }
                }
            }
            break;
        case ButtonRelease:
            if(event.xkey.window == window && !showscores && !pauseflag)
            {
/*
                checkbuttons(event.xbutton.x,event.xbutton.y);
*/
                x = (event.xbutton.x-XOFFSET)/SWIDTH;
                y = (event.xbutton.y-YOFFSET)/SHEIGHT;
                if(InArray(x,y))
                {
                    switch(event.xbutton.button)
                    {
                        case Button1:
                            Show(x,y);
                            break;
                        case Button2:
                            fillNumbers(x,y);
                            break;
                        case Button3:
                            MarkCell(x,y);
                            break;
                        default:
                            break;
                    }
                }
                smiley();
            }
            else if (event.xkey.window == hswindow)
            {
                hsdone = 1;
            }
            break;
        default:
            break;
    }
}

DrawButton(button)
    BUTTON *button;
{
    int yoffset;
    int xoffset;

    yoffset = (button->h - (xfontinfo->ascent+xfontinfo->descent))/2;
    yoffset = yoffset+button->y+xfontinfo->ascent+1;
    xoffset = XTextWidth(xfontinfo,button->name,strlen(button->name));
    xoffset = button->x+(button->w - xoffset)/2+1;
    XDrawRectangle(display,window,gc,button->x,button->y,
            button->w,button->h);
    XDrawImageString(display,window,gc,xoffset,yoffset,
        button->name,strlen(button->name));

}

checkbuttons(mx,my)
{
    int i;

    for(i=0; i < NUMBUTTONS; i++)
    {
        if(button[i].x < mx && mx < button[i].x + button[i].w &&
            button[i].y < my && my < button[i].y + button[i].h)
        {
            button[i].func();
        }
    }
}

show_high_scores(blank)
{
    int i;
    int tmpflag;

    readScores();
    tmpflag = pauseflag;
    if(blank)
    {
        pauseflag = 1;
    }
    draw_screen();
    /* window creation */
    showscores = 1;
    if (! hswindow) {
	 hswindow = XCreateSimpleWindow (display,
					 DefaultRootWindow(display),
					 0, 0, 200, 300,
					 5, foreground, background);
	 hsgc = XCreateGC(display, hswindow, 0, 0);
	 XSetBackground(display, hsgc, background);
	 XSetForeground(display, hsgc, foreground);
	 XSelectInput(display, hswindow,
		      ButtonPressMask | ButtonReleaseMask |
		      KeyPressMask | ExposureMask);
	 XSetStandardProperties(display, hswindow,
				"high scores", "high score list",
				None, NULL, 0, &hint);
	 XMapRaised(display, hswindow);
    }
    else {
	 XClearArea(display, hswindow, 0, 0, 200, 300, True);
	 XRaiseWindow(display, hswindow);
    }
    
    hsdone = 0;
    while(!hsdone)
    {
        getXevent();
    }
    pauseflag = tmpflag;
    draw_screen();
    showscores = 0;
}

draw_screen()
{
    int i;

    for(i=0; i < NUMBUTTONS; i++)
    {
/*
        DrawButton(&button[i]);
*/
    }
    show_score(1);
    smiley();
    DrawShadow(0,0,hint.width-1,hint.height-1,3);
    DrawShadow(XOFFSET-4,10,XOFFSET+width*SWIDTH+3,
        YOFFSET-10,-3);
    DrawShadow(XOFFSET-4,YOFFSET-4,XOFFSET+width*SWIDTH+3,
        YOFFSET+height*SHEIGHT+3,-3);
    if(pauseflag)
    {
        /* keep bill from cheating */
        XFillRectangle(display,window,gc,XOFFSET-1,YOFFSET-1,
            width*SWIDTH+1,height*SHEIGHT+1);
    
    }
    else
    {
        XDrawRectangle(display,window,gc,XOFFSET-1,YOFFSET-1,
            width*SWIDTH+1,height*SHEIGHT+1);
        redraw_array();
    }
}

DrawShadow(x1,y1,x2,y2,dir)
{
    int i;
    GC highlightgc,shadowgc;

    highlightgc = XCreateGC(display, window, 0, 0);
    
    shadowgc = XCreateGC(display, window, 0, 0);

    if(dir < 0)
    {
        XSetForeground(display, highlightgc, foreground);
        XSetForeground(display, shadowgc, background);
        dir = -dir;
    }
    else
    {
        XSetForeground(display, shadowgc, foreground);
        XSetForeground(display, highlightgc, background);
    }

    for(i=0; i < dir; i++)
    {
        XDrawLine(display,window,highlightgc,x1,y1,x1,y2);
        XDrawLine(display,window,shadowgc,x1,y2,x2,y2);
        XDrawLine(display,window,shadowgc,x2,y1,x2,y2);
        XDrawLine(display,window,highlightgc,x1,y1,x2,y1);
        x1++; x2--; y1++; y2--;
    }
}

keyboard(c)
    int c;
{
    switch(c)
    {
        case 'p':
            paws();
	    break;
        case 'q':
            quit();
            break;
        default:
            break;
    }
}

redraw_array()
{
    int i;
    int j;

    for(i=0; i <height; i++)
    {
        for(j=0; j < width; j++)
        {
            DrawCell(j,i);
        }
    }
}

DrawCell(x,y)
    int x;
    int y;
{
    int val;

        val = ReadCell(x,y);

    if(InArray(x,y))
    {
        if(val < 9)
        {
            XDrawCell(x,y,patpix[val]);
        }
        else if(val == 10)
        {
            XDrawCell(x,y,missmarkpix);
        }
        else if(val == 11)
        {
            XDrawCell(x,y,boompix);
        }
        else if(IsMarked(x,y))
        {
            XDrawCell(x,y,markpix);
        }
        else if(!IsVisible(x,y))
        {
            XDrawCell(x,y,coveredpix);
        }
        else if(IsMine(x,y))
        {
            XDrawCell(x,y,minepix);
        }
    }
}

ohno()
{
    XCopyArea(display,ohnopix,window,gc,0,0,SWIDTH,SHEIGHT
        ,(width*SWIDTH)/2+XOFFSET,30);
}

smiley()
{
    XCopyArea(display,smileypix,window,gc,0,0,SWIDTH,SHEIGHT
        ,(width*SWIDTH)/2+XOFFSET,30);
}

DrawHighlight(x,y)
{
    if(InArray(x,y))
    {
        XCopyArea(display,highpix,window,gc,0,0,SWIDTH,SHEIGHT
            ,x*SWIDTH+XOFFSET,y*SHEIGHT+YOFFSET);
    }
}

XDrawCell(x,y,pixmap)
    int x,y;
    Pixmap pixmap;
{
    XCopyArea(display,pixmap,window,gc,0,0,SWIDTH,SHEIGHT
        ,x*SWIDTH+XOFFSET,y*SHEIGHT+YOFFSET);
}

show_score(force)
int force;
{
    char s[10];
    static int mines = 0;
    static int score = 1000;
    int nmines, nscore;

    nmines = getminesleft();
    nscore = getscore();

    if (nmines != mines || nscore != score || force) {
	mines = nmines;
	score = nscore;
	sprintf(s,"%6d",getminesleft());
	XDrawImageString(display,window,gc,
			 XOFFSET+10,50,s,strlen(s));
	sprintf(s,"%6d",getscore());
	XDrawImageString(display,window,gc,
			 width*SWIDTH+XOFFSET-60,50,s,strlen(s));
    }
}

WriteScore(i,s)
    int i;
    char *s;
{
    XDrawImageString(display,hswindow,hsgc,10
        ,i*(xfontinfo->ascent+xfontinfo->descent) +
        xfontinfo->ascent
        ,s,strlen(s));
}
