/*	SVII clock.c
	nannette	09-13-88	 

	last edit:	01/19/89

	Copyright (c) 1888,  Sun Microsystems, Inc.  All Rights Reserved.
	Sun considers its source code as an unpublished, proprietary
	trade secret, and it is available only under strict license 
	provisions.  This copyright notice is placed here only to protect
	Sun in the event the source is deemed a published work.  Dissassembly,
	decompilation, or other means of reducing the object code to human
	readable form is prohibited by the license agreement under which
	this code is provided to the user or company in posesion of this copy.

	RESTRICTED RIGHTS LEGEND:  Use, duplication, or disclosure by the
	Government is subject to restrictions as set forth in subparagraph
	(c)(1)(ii) of the Rights in Technical Data and Computer Software
	clause at DFARS 52.227-7013 and in similar clauses in the FAR and
	NASA FAR Supplement.
*/

#include  <stdio.h>
#include  <pwd.h>
#include  <math.h>
#include  <xview/xview.h>
#include  <xview/panel.h>
#include  <xview/canvas.h>
#include  <xview/font.h>
#include  <xview/svrimage.h>
#include  <xview/pixwin.h>
#include  <xview/rectlist.h>
#include  <sys/time.h>
#include  "ds_popup.h"

#define		FROMRIM			15	/* tip of hour hand to rim */
#define		LEFTEXT			6
#define		TOPEXT			8
#define		ITIMER_NULL ((struct itimerval *) 0)
#define		MIN_ANALOG_WIDTH	90
#define		MIN_ANALOG_HEIGHT	90
#define		MIN_DIG_WIDTH		150
#define		MIN_DIG_HEIGHT		70
#define		DEF_ANALOG_WIDTH	150
#define		DEF_ANALOG_HEIGHT	150
#define		DEF_DIG_WIDTH		145
#define		DEF_DIG_HEIGHT		70
#define		MIN_WINDOW_WIDTH	100
#define		MIN_WINDOW_HEIGHT	35
#define		MIN_FONT_WIDTH   	18
#define		MIN_FONT_HEIGHT		24
#define		SMALL_FONT_HEIGHT	8
#define		SMALL_FONT_WIDTH	6
#define		NULLPR	(struct pixrect *) NULL
#define		DIGITAL_ON		0

extern double rint();

static int initializing;
static int centerX, centerY;

static char *months[] = {"Jan", "Feb", "Mar", "Apr", "May", "Jun",
		         "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
static char *nums[] = {"00", "01", "02", "03", "04", "05", "06", "07",
		       "08", "09", "10", "11", "12", "13", "14", "15", 
		       "16", "17", "18", "19", "20", "21", "22", "23",
		       "24", "25", "26", "27", "28", "29", "30", "31",
		       "32", "33", "34", "35", "36", "37", "38", "39",
		       "40", "41", "42", "43", "44", "45", "46", "47",
		       "48", "49", "50", "51", "52", "53", "54", "55",
		       "56", "57", "58", "59", "60"
		       };

static int majorHour[] = {0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,1,1};
static int minorHour[] = {0,1,2,3,4,5,6,7,8,9,0,1,2,1,2,3,4,5,6,7,8,9,0,1};


struct pr_pos partA[4] = { {1,0},   {3,2},   {15,2},  {17,0}  };
struct pr_pos partB[4] = { {0,0},   {2,2},   {2,10},  {0,12}  };
struct pr_pos partC[4] = { {18,0},  {16,2},  {16,10}, {18,12} };
struct pr_pos partD[4] = { {0,12},  {2,14},  {2,22},  {0,24}  };
struct pr_pos partE[4] = { {18,12}, {16,14}, {16,22}, {18,24} };
struct pr_pos partF[4] = { {1,24},  {3,22},  {15,22}, {17,24} };
struct pr_pos partG[4] = { {12,0},  {10,2},  {10,10}, {12,12} };
struct pr_pos partH[4] = { {12,12}, {10,14}, {10,22}, {12,24} };
struct pr_pos partI[4] = { {0,0},   {2,2},   {15,2},  {17,0}  };
struct pr_pos partJ[4] = { {1,24},  {3,22},  {16,22}, {18,24} };
struct pr_pos partK[4] = { {0,24},  {2,22},  {15,22}, {17,24} };
struct pr_pos partL[4] = { {1,0},   {3,2},   {16,2},  {18,0}  };
struct pr_pos partM[4] = { {10,6},  {12,8},  {10,10}, {8,8}   };
struct pr_pos partN[4] = { {10,14}, {12,16}, {10,18}, {8,16}  };
struct pr_pos partO[6] = { {1,12},  {2,11},  {16,11}, {17,12}, {16,13}, {2,13}};

struct pr_pos dummyA[4] = { {0,0},  {0,0},  {0,0},  {0,0}  };
struct pr_pos dummyB[4] = { {0,0},  {0,0},  {0,0},  {0,0}  };
struct pr_pos dummyC[4] = { {0,0},  {0,0},  {0,0},  {0,0}  };
struct pr_pos dummyD[4] = { {0,0},  {0,0},  {0,0},  {0,0}  };
struct pr_pos dummyE[4] = { {0,0},  {0,0},  {0,0},  {0,0}  };
struct pr_pos dummyF[4] = { {0,0},  {0,0},  {0,0},  {0,0}  };
struct pr_pos dummyG[4] = { {0,0},  {0,0},  {0,0},  {0,0}  };
struct pr_pos dummyH[4] = { {0,0},  {0,0},  {0,0},  {0,0}  };
struct pr_pos dummyI[4] = { {0,0},  {0,0},  {0,0},  {0,0}  };
struct pr_pos dummyJ[4] = { {0,0},  {0,0},  {0,0},  {0,0}  };
struct pr_pos dummyK[4] = { {0,0},  {0,0},  {0,0},  {0,0}  };
struct pr_pos dummyL[4] = { {0,0},  {0,0},  {0,0},  {0,0}  };
struct pr_pos dummyM[4] = { {0,0},  {0,0},  {0,0},  {0,0}  };
struct pr_pos dummyN[4] = { {0,0},  {0,0},  {0,0},  {0,0}  };
struct pr_pos dummyO[6] = { {0,0},  {0,0},  {0,0},  {0,0}, {0,0}, {0,0}};


struct pr_pos *defaultFont[] = {partA, partB, partC, partD, partE, partF,
                                partG, partH, partI, partJ, partK, partL,
                                partM, partN, partO};

struct pr_pos *workingFont[] = {dummyA, dummyB, dummyC, dummyD, dummyE,
				dummyF, dummyG, dummyH, dummyI, dummyJ, 
				dummyK, dummyL, dummyM, dummyN, dummyO};

static int cs[360], sn[360];
static char date_buf[25];

static short	my_fifty_data[] = {
	0xAAAA,0x5555,0xAAAA,0x5555,0xAAAA,0x5555,0xAAAA,0x5555,
  	0xAAAA,0x5555,0xAAAA,0x5555,0xAAAA,0x5555,0xAAAA,0x5555
};
mpr_static(my_fifty_patch, 16, 16, 1, my_fifty_data);

Server_image gray_patch;

static struct itimerval timer;
static void draw_circle();
static void paint_hands();
static void paint_ticks();
static void erase_second_hand();
static void enable_timer();

static Notify_value clock_resize_proc();
static Notify_value clock_repaint_proc();
static void analog_resize_proc();
static void analog_repaint();
static void icon_repaint();
static void dig_repaint();
static void dig_resize_proc();
static void backup_values();
static void show_props();
static Notify_value timer_expired ();
static Notify_value analog_timer_expired();
static Notify_value icon_timer_expired();
static Notify_value dig_timer_expired();

typedef enum {digital, analog} Face;

/*	Clock Options.  BAK values are used when property sheet is reset	*/

typedef struct clckOptions {
	Frame		frame;
	Panel		panel;
	Panel_item	apply_button;
	Panel_item	reset_button;
	Panel_item	defaults_button;
	Panel_item	faceStr;		/* "Clock Face"			*/
	Panel_item	faceChoice;		/* digital, analog		*/
	Face		face;			/* faceChoice value		*/
	Face		faceBAK;		/* faceChoice temp value	*/
	Panel_item	displayStr;		/* "Display Options"		*/
	Panel_item	secondsToggle;		/* seconds toggle item		*/
	int		seconds;		/* seconds toggle value		*/
	int		secondsBAK;		/* seconds toggle temp value	*/
	Panel_item	dateToggle;		/* date toggle item		*/
	int		date;			/* date toggle value 		*/
	int		dateBAK;		/* date toggle temp value	*/
	} ClockOptions, *Options;

/*	Coordinates & measures used to display/resize  both clock faces		*/

typedef struct displayInfo {
	Server_image	images[12];		/* number images for digital clock */
	int		fontHeight;		/* number images' height	   */
	int		fontWidth;		/* number images' width		   */
	int		y_coord;		/* number images' y-coordinate	   */
	Xv_Font		font;			/* roman-sans8			   */
	int		slots[6];		/* cached x_coor for digital layout*/
        struct          {
                        int lastSecX;           /* cached last second_hand x_coor  */
                        int lastSecY;           /* cached last second_hand y_coor  */
                        int lastSecX1;          /* cached last second_hand x1_coor */
                        int lastSecY1;          /* cached last second_hand y1_coor */
                        } secondhand;
        struct          {                       /* cached last hands coor          */
                        int angle1;
                        int angle2;
                        int width;
			int radius;		/* circle radius		   */
                        } hands;
	} DisplayInfo,  *ClockDisplay;
	 
/*	Main Object								*/

typedef struct clckObject {
	Frame			frame;
	Canvas			canvas;
	Pixwin *		pw;
	Icon			icon;
	Pixwin *		iconpw;
	Options			options;
	ClockDisplay		display;
	Menu			menu;
	} ClockObject, *Clock;

Server_image	handspr;
Server_image	spotpr;
Server_image	dotspr;
Server_image	tempr;
Server_image	iconpr;
Server_image	iconhandspr;
Server_image	icondotspr;
Server_image	iconspotpr;
Server_image	icontempr;
int		key;		/* squirrled away for KEY_DATA */
int		lastw;
int		lasth;

void
print_event (handle, event)
Xv_window	handle;
Event		*event;
{
	static int i = 0;


    if (event_action (event) == LOC_MOVE)
        printf ("LOC_MOVE");
    else if (event_action (event) == LOC_WINENTER)
        printf ("LOC_WINENTER");
    else if (event_action (event) == LOC_WINEXIT)
        printf ("LOC_WINEXIT");
    else if (event_action (event) == ACTION_SELECT)
        printf ("ACTION_SELECT");
    else if (event_action (event) == ACTION_ADJUST)
        printf ("ACTION_ADJUST");
    else if (event_action (event) == ACTION_MENU)
        printf ("ACTION_MENU");
    else if (event_action (event) == LOC_MOVEWHILEBUTDOWN)
        printf ("LOC_MOVEWHILEBUTDOWN");
    else if (event_action (event) == LOC_DRAG)
        printf ("LOC_DRAG");
    else if (event_action (event) == WIN_REPAINT)
        printf ("WIN_REPAINT");
    else if (event_action (event) == WIN_RESIZE)
        printf ("WIN_RESIZE");
    else if (event_action (event) == WIN_MAP_NOTIFY)
        printf ("WIN_MAP_NOTIFY");
    else if (event_action (event) == WIN_UNMAP_NOTIFY)
        printf ("WIN_UNMAP_NOTIFY");
    else if (event_action (event) == KBD_USE)
        printf ("KBD_USE\t");
    else if (event_action (event) == KBD_DONE)
        printf ("KBD_DONE");
    else if (event_action (event) == WIN_CLIENT_MESSAGE)
        printf ("WIN_CLIENT_MESSAGE");
    else if (event_action (event) == WIN_UNUSED_11)
        printf ("WIN_UNUSED_11");
    else if (event_action (event) == ACTION_DRAG_LOAD)
        printf ("ACTION_DRAG_LOAD");
	else
		printf ("unknown");

	printf ("\t");

	if (event_is_up (event))
		printf ("UP");
	if (event_is_down (event))
		printf ("DOWN");

	printf ("\tx = %d  y = %d  w = %d  h = %d\n", event_x(event), event_y(event),
		(int) xv_get(handle, XV_WIDTH, 0), (int) xv_get(handle, XV_HEIGHT, 0));
}

static void
readrc(o)
	Options o;
{
        struct  passwd *pw;
        char    buf[100];
        FILE    *fp;
 
	if (o==NULL) return;
	o-> face = analog;
	o-> seconds = 0;
	o-> date = 0;
        pw = getpwuid (getuid ());
        if (pw == NULL) {
                fprintf(stderr,"clock: can't find home directory\n");
                exit(1);
        }
        strcpy(buf, pw->pw_dir);
        strcat(buf, "/");
        strcat(buf, ".clockrc");
        fp = fopen(buf, "r");
        if (fp == NULL) return;
        fscanf(fp, "%d %d %d", &o->face, &o->seconds, &o->date);
	backup_values (o);
        fclose(fp);
}

static void
writerc(o)
	Options o;
{
	struct	passwd *pw;
	char	buf[100];
	FILE	*fp;

	if (o==NULL) return;
	pw = getpwuid (getuid());
	if (pw==NULL) {
		fprintf (stderr, "clock: can't find home directory\n");
		exit (1);
	}
	strcpy (buf, pw->pw_dir);
	strcat (buf, "/");
	strcat (buf, ".clockrc");
	fp = fopen (buf, "w");
	if (fp==NULL) return;
	fprintf (fp, "%d %d %d\n", o->face, o->seconds, o->date);
	fclose (fp);
}


static void
cleanup(c)
        Clock c;
{
        if (c==NULL) return;
        free(c->options);
        free(c->display);
        free(c);
}


grow_font (factor, startingFont)
	int factor;
	struct pr_pos *startingFont[];    
{
	int i, j;

        for (i = 0; i < 14; i++)
	for (j = 0; j < 4;  j++)
          { workingFont[i][j].x = (startingFont[i][j].x * factor);
            workingFont[i][j].y = (startingFont[i][j].y * factor);
          }
        for (i = 14; i < 15; i++)
        for (j=0; j < 6; j++)
	  { workingFont[i][j].x = (startingFont[i][j].x * factor);
            workingFont[i][j].y = (startingFont[i][j].y * factor);
          }
}

static Notify_value
canvas_interpose(pw, event, arg, type)
	Pixwin *pw; Event *event;
	Notify_arg arg; Notify_event_type type;
{
	Clock c;
	Options o;
	Notify_value rc;
	int id;

	/*print_event(pw, event);   */
	rc = notify_next_event_func (pw, event, arg, type);
	id = event_action(event);
	
	c = (Clock) xv_get(pw, XV_KEY_DATA, key, 0);
	switch(id) {
	case WIN_REPAINT:
		(void)clock_repaint_proc(c->canvas, pw, NULL);
		break;
	case WIN_RESIZE:  /* pw doesn't get WIN_RESIZE; more horse shit */
		(void)clock_resize_proc(pw, (int)xv_get(pw, XV_WIDTH, 0), (int)xv_get(pw, XV_HEIGHT, 0));
		break;
	case ACTION_MENU:
		if (event_is_down(event)) {
			menu_show((Menu) xv_get(pw, WIN_MENU, 0), pw, event, 0);
/*			menu_show(c->menu, c->canvas, event, 0);   */
/*			xv_set(c->menu, XV_SHOW, TRUE, 0);	   */
		}
		break;

	}
	return(rc);
}


/*	Interpose only to watch for open & close of tool to repaint icon */

static Notify_value
frame_interpose (frame, event, arg, type)
	Frame frame; Event *event;
	Notify_arg arg; Notify_event_type type;
{
	int w, h, smaller;
	Notify_value rc;
	Clock c;

	rc = notify_next_event_func (frame, event, arg, type);
	if (event_action(event) == ACTION_CLOSE) {
		c = (Clock) xv_get (frame, XV_KEY_DATA, key, 0);
		w = (int) xv_get (c->canvas, XV_WIDTH, 0);
		h = (int) xv_get (c->canvas, XV_HEIGHT, 0);
		centerX=0; centerY=0;
		pw_write(c->pw, 0, 0, w, h, PIX_CLR, 0, 0, 0);
		init_images(c, 64, 64);
		icon_repaint(c->icon, c->iconpw, NULL);
	}
	if (event_action(event) == ACTION_OPEN) {
		c = (Clock) xv_get (frame, XV_KEY_DATA, key, 0);
		w = (int) xv_get (c->canvas, XV_WIDTH, 0);
		h = (int) xv_get (c->canvas, XV_HEIGHT, 0);
		centerX=0; centerY=0;
		smaller = min(w, h);
		init_images(c, smaller, smaller);
		clock_repaint_proc(c->canvas, c->pw, NULL);
	}
	return(rc);
}
			
	
static Notify_value
icon_interpose (icon, event, arg, type)
	Icon icon; Event *event;
	Notify_arg arg; Notify_event_type type;
{
	Notify_value rc;
	Clock c;

	rc = notify_next_event_func(icon, event, arg, type);
	c = (Clock) xv_get(icon, XV_KEY_DATA, key, 0);

	if (event_action(event) == WIN_REPAINT) {
		icon_repaint(icon, c->iconpw, NULL);
	}
	return(rc);
}



static Notify_value
clock_repaint_proc (canvas, pw, area)
	Canvas canvas;
	Pixwin * pw;
	Rectlist *area;
{
	int w, h;
	Clock c;

	c = (Clock) xv_get (canvas, XV_KEY_DATA, key, 0);
	w = (int) xv_get (canvas, XV_WIDTH, 0);
	h = (int) xv_get (canvas, XV_HEIGHT, 0);
	pw_write (pw, 0, 0, w, h, PIX_CLR, 0, 0, 0);
	switch (c->options->face) {
	case analog:
		analog_repaint (canvas, pw, area);
		break;
	case digital:
		dig_repaint (canvas, pw, area);
		break;
	}
	return(NOTIFY_DONE);
}



static Server_image
make_image (w, h, kd) 
	int w, h;
	caddr_t kd;
{
	Server_image i;
	i = (Server_image) xv_create (NULL, SERVER_IMAGE,
		XV_WIDTH, w,
		XV_HEIGHT, h,
		SERVER_IMAGE_DEPTH, 1,
		0);
	xv_set (i, XV_KEY_DATA, key, kd, 0);
	pw_write (i, 0, 0, w, h, PIX_CLR, 0, 0, 0);
	return (i);
}

init_images (c, w, h)
	Clock c; int w, h;
{
	int now;
	struct tm *tm;

	/* resize the remote images */

	now = time(0);
	tm  = localtime (&now);

	if (tempr != NULL) xv_destroy (tempr);
	tempr = make_image (w, h, c);

	if (handspr != NULL) xv_destroy (handspr);
	handspr = make_image (w, h, c);
	paint_hands (c, handspr, tm-> tm_min*6,
		tm-> tm_hour*30 + tm-> tm_min/2, min(w, h));

	if (dotspr != NULL) xv_destroy (dotspr);
	dotspr = make_image (w, h, c);

	if (spotpr != NULL) xv_destroy (spotpr);
	spotpr = make_image (w/12, h/12, c);
	draw_circle (spotpr, armwidth(w)/8);

	paint_ticks (dotspr, w/2, spotpr);
}

static int
min (a, b)
	int a, b;
{
	return (a<b?a:b);
}
	

static Notify_value 
clock_resize_proc (canvas, width, height)
	Canvas canvas;
	int width, height;
{
	int	scaleFactor, ratio, smaller;
	int	fontHeight, fontWidth, cwidth, cheight;
	Clock	c;
	ClockDisplay d;

	if (initializing) {
		initializing = 0;
		return;
	}
	c	= (Clock) xv_get (canvas, XV_KEY_DATA, key, 0);
	cwidth	= (int) xv_get (canvas, XV_WIDTH, 0);
	cheight = (int) xv_get (canvas, XV_HEIGHT, 0);
	d	= c-> display;
	smaller	= min(cwidth,cheight);

	pw_write(c->pw, 0, 0, cwidth, cheight, PIX_CLR, 0, 0, 0);
	switch (c->options->face) {
		case digital:
		if (cwidth < MIN_DIG_WIDTH) {
			cwidth = MIN_DIG_WIDTH;
/*
			return (NOTIFY_DONE);
*/
		}
		break;
		default:
		break;
	}
			

	/* ANALOG STUFF */
	d->secondhand.lastSecX		= -1;
	init_images (c, smaller, smaller);	/* make clock fit */

	/* DIGITAL STUFF */
	if (smaller < MIN_DIG_WIDTH) 
		smaller = 0;
	fontWidth		= (3.0/4.0 * (double) cwidth / 6.0);
	fontHeight		= (4.0/3.0 * (double) fontWidth);
	while (fontHeight > (3.0/4.0 * cheight)) {
		fontWidth = fontWidth - 1;
		fontHeight = 4.0/3.0 * (double) fontWidth;
	}
	d-> fontHeight		= fontHeight; 
	d-> fontWidth		= fontWidth;
	ratio			= fontHeight/MIN_FONT_HEIGHT;
	scaleFactor = (ratio >= 1) ? ratio : 1;
	grow_font (scaleFactor, defaultFont);
	update_slots (c);
       	build_numbers (c);

}

float   pi = 3.14159265;
 
static int
rotx(x, y, r, th)                       /* th is in degrees */
        int     x, y;
        int     r, th;
{
        float th1;
 
        th1 = (th*2.0*pi)/360.0;
        return((int)((x - r)*cos(th1) - (y - r)*sin(th1) + r));
}
 
static int
roty(x, y, r, th)                       /* th is in degrees */
        int     x, y;
        int     r, th;
{
        float th1;
 
        th1 = (th*2.0*pi)/360.0;
        return((int)((x - r)*sin(th1) + (y - r)*cos(th1) + r));
}

static void
init_numbers ()
{
	int i;
	for (i=0; i<360; i++) {
		cs[i] = (int) rint (100.0*cos(i*pi/180));
		sn[i] = (int) rint (100.0*sin(i*pi/180));
	}
}

static void
draw_line(pr, offset, x1,y1,x2,y2,color)
{
	pw_vector(pr,x1+offset,y1+offset,
		x2+offset,y2+offset,PIX_SRC,color);
	pw_vector(pr,y1+offset,x1+offset,
		y2+offset,x2+offset,PIX_SRC,color);
	pw_vector(pr,-x1+offset,y1+offset,
		-x2+offset,y2+offset,PIX_SRC,color);
	pw_vector(pr,x1+offset,-y1+offset,
		x2+offset,-y2+offset,PIX_SRC,color);
	pw_vector(pr,y1+offset,-x1+offset,
		y2+offset,-x2+offset,PIX_SRC,color);
	pw_vector(pr,-y1+offset,x1+offset,
		-y2+offset,x2+offset,PIX_SRC,color);
	pw_vector(pr,-y1+offset,-x1+offset,
		-y2+offset,-x2+offset,PIX_SRC,color);
	pw_vector(pr,-x1+offset,-y1+offset,
		-x2+offset,-y2+offset,PIX_SRC,color);
}

static int
armwidth (r) 
{
	int w;
	float fudge = 1.0 + (20.0/r);
	if (fudge > 1.6) fudge = 1.6;
	w = (int) rint ((double)fudge * r/ 8.0);
	if (w%2 == 0) w++;
	return (w);
}


static void
paint_ticks (pw, radius, spotpr)
	Pixwin *pw; Server_image spotpr;
	int radius;
{
	int i;
	int arm_width = armwidth (radius);

	for (i=0; i<12; i++)
	pw_write (pw,  
		cs[i*30] * 20 * radius/2400+radius-arm_width/4,
		sn[i*30] * 20 * radius/2400+radius-arm_width/4,
		arm_width+1,
		arm_width+1,					
		PIX_SRC | PIX_DST, spotpr, 0, 0); 
}


static void
draw_circle (pr, r)
	Server_image pr; int r;
{
	int x, y, y_pos[100], d;
	
	x=0; y=r; d = 3-2*r;
	y_pos[0]=y;

	while (x < y) {
	  if (d < 0) d+=4 * x + 6;
	  else {
	    d+=4 * (x-y) + 10;
	    y--;
	  }
	  x++;
	  y_pos[x] = y;
	}
	y_pos[x+1] = -1;
	x=0;
	while (y_pos[x] > 0) {
	  draw_line (pr, r, x, 0, x, y_pos[x], 1);
	  x++;
	}
}
	
static void
erase_hand (c, x1, y1, x2, y2, x3, y3, angle, diameter)
        Clock c;
{
        int     nptarr[1];
        struct pr_pos   vlist[3];
        int     xx1, yy1, xx2, yy2, xx3, yy3;
        Pixwin *pw;

        nptarr[0] = 3;
        pw = c->pw;

        /* rotate */
        xx1 = rotx(x1, y1, diameter/2, angle);
        yy1 = roty(x1, y1, diameter/2, angle);
        xx2 = rotx(x2, y2, diameter/2, angle);
        yy2 = roty(x2, y2, diameter/2, angle);
        xx3 = rotx(x3, y3, diameter/2, angle);
        yy3 = roty(x3, y3, diameter/2, angle);

        vlist[0].x = xx1;
        vlist[0].y = yy1;
        vlist[1].x = xx2;
        vlist[1].y = yy2;
        vlist[2].x = xx3;
        vlist[2].y = yy3;

        pw_polygon_2(pw, 0, 0, 1, nptarr, vlist, PIX_CLR,
                0, 0, 0);
        pw_vector(pw, vlist[0].x, vlist[0].y, vlist[1].x, vlist[1].y,
                PIX_CLR, 1);
        pw_vector(pw, vlist[0].x, vlist[0].y, vlist[2].x, vlist[2].y,
                PIX_CLR, 1);
        pw_vector(pw, vlist[1].x, vlist[1].y, vlist[2].x, vlist[2].y,
                PIX_CLR, 1);
}


erase_hands (c)
        Clock c;
{
        int w, angle1, angle2;
        int x1, y1, yy1, x2, y2, x3, y3;
        int fromrim, topext, leftext;
        ClockDisplay d;
        Pixwin *pw;


        d       = c->display;
        pw      = c->pw;
        w       = d->hands.width;
        angle1  = d->hands.angle1;
        if (angle1==-1) return;
        angle2  = d->hands.angle2;

        fromrim = (FROMRIM*w)/128;
        leftext = (LEFTEXT*w)/128;
        topext  = (TOPEXT*w)/128;

        /* tip of hand */
        x1 = w/2;
        y1 = fromrim;
        yy1 = w/2 - (2*(w/2 - fromrim))/3;

        /* lower left hand of hand */
        x2 = w/2 - leftext;
        y2 = w/2 + topext;
        
        /* lower right hand of hand */
        x3 = w/2 + leftext;
        y3 = w/2 + topext;
 
        /* hour hand */
        erase_hand (c, x1, yy1, x2, y2, x3, y3, angle2, w);
 
        /* minute hand */
        erase_hand (c, x1, y1, x2, y2, x3, y3, angle1, w);
 
 
}

	
static void
paint_hand (pr, x1, y1, x2, y2, x3, y3, angle, diameter)
        Server_image pr;
{
        int     nptarr[1];
        struct pr_pos   vlist[3];
        int     xx1, yy1, xx2, yy2, xx3, yy3;

	nptarr[0] = 3;

        /* rotate */
        xx1 = rotx(x1, y1, diameter/2, angle);
        yy1 = roty(x1, y1, diameter/2, angle);
        xx2 = rotx(x2, y2, diameter/2, angle);
        yy2 = roty(x2, y2, diameter/2, angle);
        xx3 = rotx(x3, y3, diameter/2, angle);
        yy3 = roty(x3, y3, diameter/2, angle);

        vlist[0].x = xx1;
        vlist[0].y = yy1;
        vlist[1].x = xx2;
        vlist[1].y = yy2;
        vlist[2].x = xx3;
        vlist[2].y = yy3;
 
        pw_polygon_2(pr, 0, 0, 1, nptarr, vlist, PIX_SRC,
            gray_patch, 0, 0);
        pw_vector(pr, vlist[0].x, vlist[0].y, vlist[1].x, vlist[1].y,
            PIX_SET, 1);
        pw_vector(pr, vlist[0].x, vlist[0].y, vlist[2].x, vlist[2].y,
            PIX_SET, 1);
        pw_vector(pr, vlist[1].x, vlist[1].y, vlist[2].x, vlist[2].y,
            PIX_SET, 1);
}


static void
paint_hands (c, pr, angle1, angle2, w)
	Clock c;
	Server_image pr;
	int angle1;		/* long hand */
	int angle2;		/* short hand */
	int w;			/* canvas width */
{
	int	x1, y1, yy1, x2, y2, x3, y3;
	int	fromrim, topext, leftext;
	ClockDisplay d;

        d = c->display;

        /*      cache hands positions for erasing later.  perf mod. */
        d->hands.angle1 = angle1;
        d->hands.angle2 = angle2;
        d->hands.width = w;

	fromrim = (FROMRIM*w)/128;
	leftext = (LEFTEXT*w)/128;
	topext  = (TOPEXT*w)/128;

	/* tip of hand */
	x1 = w/2;
	y1 = fromrim;
	yy1 = w/2 - (2*(w/2 - fromrim))/3;

	/* lower left hand of hand */
	x2 = w/2 - leftext;
	y2 = w/2 + topext;
	
	/* lower right hand of hand */
	x3 = w/2 + leftext;
	y3 = w/2 + topext;

	/* hour hand */
	paint_hand (pr, x1, yy1, x2, y2, x3, y3, angle2, w);

	/* minute hand */
	paint_hand (pr, x1, y1, x2, y2, x3, y3, angle1, w);

}

static void
erase_date (c)
        Clock c;
{
	xv_set(c->frame, FRAME_LABEL, "", 0);
	date_buf[0] = NULL;
}


static void
paint_date (c) 
	Clock c;
{
	int now;
	struct tm *tm;
	char buf[25];
	Frame f;

	f	= c-> frame;
	now	= time(0);
	tm	= localtime (&now);
	sprintf (buf, "%s %d",  months[tm->tm_mon], tm->tm_mday);
	if (strcmp(buf, date_buf))
        {
		xv_set (f, FRAME_LABEL, buf, 0);
		strcpy(date_buf, buf);
        }

}

static void
erase_second_hand (c)
	Clock c;
{
	int x1, y1, x2, y2;
	ClockDisplay d	= c->display;
	Pixwin *pw;

	d = c->display;
	if (xv_get(c->frame, FRAME_CLOSED, 0)) {
		pw = (Pixwin *)c->icon;
	}
	else {
		pw = c->pw;
	}
	/* burn the last displayed second hand off  */
	x1 = d->secondhand.lastSecX;
	y1 = d->secondhand.lastSecY;
	x2 = d->secondhand.lastSecX1;
	y2 = d->secondhand.lastSecY1;
	if (x1 != -1) pw_vector (pw, x1, y1, x2, y2, PIX_SRC ^ PIX_DST, 1); 
}
	
static void
paint_second_hand (c) 
	Clock c; 
{
	int x, y, diameter, radius, now, fromrim, angle, height, width;
	struct tm *tm;
	ClockDisplay d;
	Pixwin *pw;

	if (!seconds_on (c->options)) return;
	now	= time(0);
	tm	= localtime (&now);
	angle	= tm-> tm_sec * 6;
	d	= c-> display;

	if (xv_get(c->frame, FRAME_CLOSED, 0)) {
		pw = (Pixwin *)c->icon;	/* pretty bogus */
		width = 64;
		height = 64;
		diameter = 64;
	}
	else {
		pw = c->pw;
		width = (int)xv_get (pw, XV_WIDTH, 0);
		height = (int)xv_get (pw, XV_HEIGHT, 0);
		diameter= (int)xv_get(handspr, XV_WIDTH, 0);
		/*fprintf(stderr, "w=%d, h=%d, di=%d\n", width, height, diameter);*/
	}
	radius	= diameter/2;
	fromrim	= (FROMRIM*diameter)/128;
	x	= rotx (radius, fromrim, radius, angle);
	y	= roty (radius, fromrim, radius, angle);
	/*fprintf(stderr, "ra=%d, rim=%d, x=%d, y=%d\n\n", radius, fromrim, x, y);*/

	/* cache the new second, then paint */

	d->secondhand.lastSecX = width/2;
	d->secondhand.lastSecY = height/2;
	d->secondhand.lastSecX1 = centerX+x;	
	d->secondhand.lastSecY1 = centerY+y;

	/*fprintf(stderr, "centerX=%d, centerY=%d\n", centerX, centerY);*/
	pw_vector (pw, width/2, height/2,
		centerX+x, centerY+y, PIX_SRC ^ PIX_DST, 1);
}
	
static Notify_value
timer_expired (me, which) 
	Notify_value	me;
	int		which;
{
	int		closed;
	Clock		c;
	Options		o;

	c	= (Clock) xv_get (me, XV_KEY_DATA, key, 0);
	o	= c-> options;
	closed  = (int)xv_get(me, FRAME_CLOSED, 0);
	if (closed)  return (icon_timer_expired (me, which));
	switch (o-> face) {
	case digital:
	  return (dig_timer_expired (me, which,FALSE));
	  break;
	case analog:
	  return (analog_timer_expired (me, which));
	  break;
	}
}

static void
center (cwidth, cheight, x, y, w, h)
        int cwidth, cheight;
        int *x, *y;
        int w, h;
{
        *x = (cwidth-w)/2;
        *y = (cheight-h)/2;
	/*fprintf(stderr, "centerX=%d, centerY=%d\n", cwidth, cheight);*/
}


static void
analog_repaint (canvas, pw, area)
	Canvas canvas;
	Pixwin * pw;
	Rectlist *area;
{
	int w, h, x, y, prw, prh, now;
	struct tm *tm;
	Clock c;

	c = (Clock) xv_get (canvas, XV_KEY_DATA, key, 0);
	now = time(0);
	tm = localtime (&now);
	w = (int) xv_get (canvas, XV_WIDTH, 0);
	h = (int) xv_get (canvas, XV_HEIGHT, 0);
	prw = (int) xv_get (handspr, XV_WIDTH, 0);
	prh = (int) xv_get (handspr, XV_HEIGHT, 0);
	pw_write (handspr, 0, 0, prw, prh, PIX_CLR, 0, 0, 0);
	paint_hands (c, handspr, tm-> tm_min*6,
		tm-> tm_hour*30 + tm-> tm_min/2, prw);
	pw_write (handspr, 0, 0, prw, prh, PIX_SRC | PIX_DST, dotspr, 0, 0);  
	center (w, h, &centerX, &centerY, prw, prh);
	pw_write (pw, centerX, centerY, prw, prh, PIX_SRC, handspr, 0, 0);
	if (seconds_on (c->options)) paint_second_hand(c);
/*	c->display->secondhand.lastSecX = -1;  */
}

static void
icon_repaint (i, pw, area)
	Icon i; 
	Pixwin *pw;
	Rectlist *area;
{
	int now;
	Font_string_dims size;
	struct tm *tm;
	Clock c;

	c	= (Clock) xv_get(i, XV_KEY_DATA, key, 0);
	now	= time(0);
	tm	= localtime (&now);
	pw_write (iconhandspr, 3, 3, 58, 58, PIX_CLR, 0, 0, 0);
	paint_hands (c, iconhandspr, tm-> tm_min*6,
		tm-> tm_hour*30 + tm-> tm_min/2, 64);
	pw_write (iconhandspr, 0, 0, 64, 64, PIX_SRC | PIX_DST, icondotspr, 0, 0);

	/* draw icon border 
        pw_vector (iconhandspr, 0, 0, 63, 0, PIX_SET, 3);
        pw_vector (iconhandspr, 0, 0, 0, 63, PIX_SET, 3);
        pw_vector (iconhandspr, 63, 0, 63, 63, PIX_SET, 3);
        pw_vector (iconhandspr, 0, 63, 63, 63, PIX_SET, 3);
	*/

	pw_write (c->icon, 0, 0, 64, 64, PIX_SRC, iconhandspr, 0, 0);
	if (seconds_on (c->options)) paint_second_hand(c);
}

static Notify_value
icon_timer_expired (me, which)
{
	static int	mins, hours;
	int		now, w, h;
	struct		tm *tm;
	Font_string_dims size;
	Clock		c;
	Xv_Font		f;

	c	= (Clock) xv_get (me, XV_KEY_DATA, key, 0);
	now	= time(0);
	tm	= (struct tm *) localtime (&now);
	
	if (tm-> tm_min != mins || tm-> tm_hour != hours) {
		mins	= tm-> tm_min;
		hours	= tm-> tm_hour;
		icon_repaint (c->icon, c->iconpw, NULL);
	}
	else {
		if (seconds_on (c->options)) {
			erase_second_hand(c);
			paint_second_hand(c);
		}
	}
	if (date_on (c->options)) 
		 paint_date (c);	
	return (NOTIFY_DONE);
}
	

static Notify_value
analog_timer_expired (me, which)
	Notify_value	me;
	int		which;
{
	static int	mins, hours;
	int		now, x, y, w, h, prw, prh;
	struct		tm *tm;
	Clock		c;

	c	= (Clock) xv_get (me, XV_KEY_DATA, key, 0);
	w	= (int) xv_get (c->canvas, XV_WIDTH, 0);
	h	= (int) xv_get (c->canvas, XV_HEIGHT, 0);
	prw	= (int) xv_get (tempr, XV_WIDTH, 0);
	prh	= (int) xv_get (tempr, XV_HEIGHT, 0);
	now	= time(0);
	tm	= (struct tm *) localtime (&now);

	if (tm-> tm_min != mins || tm-> tm_hour != hours) {
		mins	= tm-> tm_min;
		hours	= tm-> tm_hour;
		analog_repaint (c->canvas, c->pw, NULL); 
	}
	else {
/*		center (w, h, &centerX, &centerY, prw, prh);
		pw_write(c->pw, x, y, prw, prh, PIX_SRC, tempr, 0, 0);
*/
		if (seconds_on (c->options)) {
			erase_second_hand(c); 
			paint_second_hand(c);
		}
	
	}
	if (date_on (c-> options)) 
		 paint_date (c);	
	return (NOTIFY_DONE);
}

static void
paint_dig_seconds (c, tm)
	Clock c; struct tm *tm;
{
	int		fontHeight, descent, y_coord;
	Xv_Font		font;
/*	struct pr_size	fontSize; */
	Font_string_dims fontSize;
	Pixwin *	pw;
	ClockDisplay		d;
	Canvas		canvas;
	
	canvas		= c-> canvas;
	d		= c-> display;
	pw		= c-> pw;
	font		= d-> font;
	fontHeight	= d-> fontHeight;
/*	fontSize	= xv_pf_textwidth (1, d-> font, "f"); */	
	xv_get(d-> font, FONT_STRING_DIMS, "f", &fontSize);
	y_coord		= ((int) xv_get (canvas, XV_HEIGHT, 0)-fontHeight)/2;

	pw_text (pw, d-> slots[5], 
		fontSize.height + y_coord, 
		PIX_SRC,
		font, 
		nums[tm-> tm_sec]
		);
	pw_text (pw, d-> slots[5],
		(2*fontSize.height) + y_coord + 3,   /* 3 = fudge factor */
		PIX_SRC,
		font,
		(tm-> tm_hour < 12) ? "am" : "pm");
	
}
 
static void
dig_repaint (canvas, pw, area)
	Canvas		canvas;
	Pixwin *	pw;
	Rectlist	*area;
{
	int		i, now, y_coord, fontHeight, fontWidth;
	struct tm	*tm;
	Clock		c;
	ClockDisplay		d;

	c		= (Clock) xv_get (canvas, XV_KEY_DATA, key, 0);
	d		= c-> display;
	fontHeight	= d-> fontHeight;
	fontWidth	= d-> fontWidth;
	now		= time(0);
	tm		= localtime (&now);
	y_coord		= ((int) xv_get (canvas, XV_HEIGHT, 0)-fontHeight)/2;
	d->y_coord	= y_coord;

	if (tm-> tm_hour == 0)
		tm-> tm_hour = 12;
	
	if (majorHour[tm-> tm_hour] == 1)
		pw_write (pw, d-> slots[0],
			y_coord, fontWidth, 
                 	fontHeight, PIX_SRC, 
                  	d-> images[1],
		  	0, 0);
	else
          	pw_write (pw, d-> slots[0],
		  	y_coord, fontWidth, 
                  	fontHeight, PIX_SRC,
                  	d-> images[11],
		  	0, 0);
		
	pw_write (pw, d-> slots[1],
		y_coord, fontWidth, 
		fontHeight, PIX_SRC,
		d-> images[minorHour[tm-> tm_hour]],
		0, 0);
	pw_write (pw, d-> slots[2],
		y_coord, fontWidth, 
		fontHeight, PIX_SRC, 
		d-> images[10], 0, 0);
	pw_write (pw, d-> slots[3],
		y_coord, fontWidth, 
		fontHeight, PIX_SRC,
		d-> images[tm-> tm_min/10], 0, 0);
	pw_write (pw, d-> slots[4],
		y_coord, fontWidth, 
		fontHeight, PIX_SRC,
		d-> images[tm-> tm_min % 10], 0, 0);
	if (seconds_on (c-> options))
		paint_dig_seconds (c, tm);
} 

static Notify_value
dig_timer_expired (me, which, invalidate)
	Notify_value	me;
	int		which, invalidate;
{
	static int	mins, hours;
	int		now;
	struct		tm *tm;
	Clock		c;

	c		= (Clock) xv_get(me, XV_KEY_DATA, key, 0);
	now		= time(0);
	tm		= (struct tm *) localtime(&now);

	if (tm->tm_min != mins || tm-> tm_hour != hours || invalidate) {
		mins	= tm-> tm_min;
		hours	= tm-> tm_hour;
		dig_repaint (c->canvas, c->pw, NULL);
	}
	else {
		if (seconds_on (c->options)) paint_dig_seconds (c, tm); 
	}
	if (date_on (c-> options))
		 paint_date (c);	
	return (NOTIFY_DONE);
}

static Notify_value
clock_reset (item, event)
	Panel_item item;
	Event *event;
{
	Clock c		= (Clock) xv_get (item, XV_KEY_DATA, key, 0);
	Options o	= c-> options;
	xv_set (o-> faceChoice, PANEL_VALUE, o-> faceBAK, 0);
	xv_set (o-> secondsToggle, PANEL_VALUE, o->secondsBAK, 0);
	xv_set (o-> dateToggle, PANEL_VALUE, o->dateBAK, 0);
}

static int
date_changed (o)
        Options o;
{
        if (o==NULL) return(0);
        return (o->date != o->dateBAK);
}

static int
face_changed (o)
	Options o;
{
	if (o==NULL) return (0);
	return (o->face != o->faceBAK);
}

static int
seconds_changed (o)
	Options o;
{
	if (o==NULL) return (0);
	return (o-> seconds != o-> secondsBAK);
}
	
static Notify_value
clock_apply (item, event)
	Panel_item item;
	Event *event;
{
	int w, h, now;
	struct tm *tm;
	Clock c		= (Clock) xv_get (item, XV_KEY_DATA, key, 0);
	ClockDisplay d	= c-> display;
	Options o	= c-> options;
	o-> face	= (Face) xv_get (o-> faceChoice, PANEL_VALUE, 0);
	o-> seconds	= (int) xv_get (o-> secondsToggle, PANEL_VALUE, 0);
	o-> date	= (int) xv_get (o-> dateToggle, PANEL_VALUE, 0);

	writerc(c->options);
	if (date_changed(o)) {
		if (date_on(o)) {
			paint_date(c);
		}
		else {
			erase_date(c);
		}
	}
	if (seconds_changed (o) || face_changed(o)) {
		now = time(0);
		tm = localtime(&now);
		if (face_changed(o)) {
			w = (int) xv_get (c->canvas, XV_WIDTH, 0);
			h = (int) xv_get (c->canvas, XV_HEIGHT, 0);
			pw_write(c->pw, 0, 0, w, h, PIX_CLR, 0, 0, 0);
		}
		if (seconds_changed(o)) {
			switch (o-> face) {
			case digital:
				if (seconds_on(o)) {
					enable_timer(c->frame, 0, 1, 0, 1);
				}
				else {
					pw_write(c->pw, d->slots[5], d->y_coord, d->fontWidth, 
                                		5000, PIX_CLR, 0, 0, 0);
					enable_timer (c->frame, 0, 60-tm->tm_sec, 0, 60);
					dig_repaint(c->canvas, c->pw, NULL); 
				}
		  		break; 
			case analog:
				if (seconds_on(o)) {
					enable_timer(c->frame, 0, 1, 0, 1);
				}
				else {
					enable_timer (c->frame, 0, 60-tm->tm_sec, 0, 60);
					if (xv_get(c->frame, FRAME_CLOSED, 0)) {
						icon_repaint(c->icon, c->iconpw, NULL);
					}
					else {
						analog_repaint(c->canvas, c->pw, NULL);
					}
					d->secondhand.lastSecX = -1;
				}
			  	break;
			}
			backup_values(o);
			xv_set(o->frame, XV_SHOW, FALSE, 0);
			return;
		}
		backup_values(o);
		clock_repaint_proc (c->canvas, c->pw, NULL); 
		xv_set(o->frame, XV_SHOW, FALSE, 0);
		return;
	}
	clock_repaint_proc (c->canvas, c->pw, NULL); 
	backup_values(o);
}

static Notify_value
clock_defaults(item, event)
	Panel_item item;
	Event *event;
{
	Clock c		= (Clock) xv_get (item, XV_KEY_DATA, key, 0);
	ClockDisplay d	= c-> display;
	Options o	= c-> options;

	xv_set(o-> faceChoice, PANEL_VALUE, 1, 0);
	o->face=digital;
	xv_set(o-> secondsToggle, PANEL_VALUE, 0, 0);
	o->seconds=0;
	xv_set(o-> dateToggle, PANEL_VALUE, 0, 0);
	o->date=0;
	writerc(c->options);
	clock_repaint_proc(c->canvas, c->pw, NULL);
	xv_set(o->frame, XV_SHOW, FALSE, 0);
}


layout_options (o)
	Options o;
{
	Font_string_dims size;
	int	wd = 128;
	char	*str = "";
	Pixfont *pf = (Pixfont *) xv_get (o->panel, XV_FONT, 0);
	
	str	= (char *) xv_get (o-> faceStr, PANEL_LABEL_STRING, 0);
	xv_get(pf, FONT_STRING_DIMS, str, &size);
	xv_set (o-> faceStr, 
		XV_X, wd - size.width,	
		XV_Y, xv_row (o-> panel, 1),
		0);
	xv_set (o-> faceChoice,
		XV_X, xv_get (o-> faceStr, XV_X, 0) + size.width + 15,
		XV_Y, xv_row (o-> panel, 1),
		0);
	str	= (char *) xv_get (o-> displayStr, PANEL_LABEL_STRING, 0);
	xv_get(pf, FONT_STRING_DIMS, str, &size);
	xv_set (o-> displayStr,
		XV_X, wd - size.width,
		XV_Y, xv_row (o->panel, 2),
		0);
	xv_set (o-> secondsToggle,
		XV_X, xv_get (o-> displayStr, XV_X, 0) + size.width + 22,
		XV_Y, xv_row(o-> panel, 2),
		0);
	xv_set (o-> dateToggle,
		XV_X, xv_get (o-> secondsToggle, XV_X, 0) + 80,
		XV_Y, xv_row (o-> panel, 2),
		0);
	xv_set (o-> apply_button,
		XV_X, xv_get(o->displayStr, XV_X, 0)+30,
		XV_Y, xv_row (o-> panel, 3),
		0);
	xv_set (o-> reset_button,
		XV_X, xv_get(o->apply_button, XV_X, 0)+60,
		XV_Y, xv_row (o-> panel, 3),
		0);
	xv_set (o-> defaults_button,
		XV_X, xv_get(o->reset_button, XV_X, 0)+60,
		XV_Y, xv_row (o-> panel, 3),
		0);
	
}
		 
static int
digital_on (o)
	Options o;
{
	int v;
	if (o==NULL) return (0);
	return (o-> face == digital);
}

static int
seconds_on (o)
	Options o;
{
	if (o==NULL) return (0);
	return (o-> seconds);
}



static int
date_on (o)
	Options o;
{
	int v;
	if (o==NULL) return (0);
	return (o-> date);
}

static void
init_options (c)
	Clock c;
{
	int wd=40;
	Options o;

	if (c == NULL) return;
	o = c-> options;

	o-> frame =
		(Frame) xv_create (c-> frame, FRAME_PROPS,
		XV_LABEL, "Clock Properties",
		XV_X, 0,
		XV_Y, 0,
		XV_SHOW, FALSE,
		FRAME_SHOW_FOOTER, TRUE,
		XV_HELP_DATA,           "clock:PropertyFrame",
		0);
	xv_set (o-> frame, XV_KEY_DATA, key, c, 0);
	o-> panel = (Panel) xv_get
		(o-> frame, FRAME_PROPS_PANEL, 0);
	xv_set (o-> panel,
		XV_X, 0, 
		XV_Y, 0,
		XV_WIDTH, 350,
		XV_HEIGHT, xv_row(o-> panel, 4),
		XV_HELP_DATA,           "clock:PropertyPanel",
		0); 
	xv_set (o-> panel, XV_KEY_DATA, key, c, 0);
	o-> apply_button =
		(Panel_item) xv_create (o->panel, PANEL_BUTTON,
		PANEL_LABEL_BOLD, TRUE,
		PANEL_LABEL_STRING, "Apply",
		PANEL_NOTIFY_PROC, clock_apply,	
		XV_KEY_DATA, key, c,
		XV_HELP_DATA,           "clock:ApplyButton",
		0);
	o-> reset_button =
		(Panel_item) xv_create (o->panel, PANEL_BUTTON,
		PANEL_LABEL_BOLD, TRUE,
		PANEL_LABEL_STRING, "Reset",
		PANEL_NOTIFY_PROC, clock_reset,
		XV_KEY_DATA, key, c,
		XV_HELP_DATA,           "clock:ResetButton",
		0);
	o-> defaults_button =
		(Panel_item) xv_create (o->panel, PANEL_BUTTON,
		PANEL_LABEL_BOLD, TRUE,
		PANEL_LABEL_STRING, "Defaults",
		PANEL_NOTIFY_PROC, clock_defaults,
		XV_KEY_DATA, key, c,
		XV_HELP_DATA,           "clock:DefaultButton",
		0);
	o-> faceStr =
		(Panel_item) xv_create (o-> panel, PANEL_MESSAGE,
		PANEL_LABEL_BOLD, TRUE,
		PANEL_LABEL_STRING, "Clock Face:",
		XV_HELP_DATA,           "clock:FaceStyle",
		0);
	o-> faceChoice =
		(Panel_item) xv_create (o-> panel, PANEL_CHOICE,
		PANEL_CHOICE_STRINGS, "digital", "analog", 0,
		PANEL_VALUE, o->face,
		XV_HELP_DATA,           "clock:FaceStyle",
		0);
	o-> displayStr =
		(Panel_item) xv_create (o-> panel, PANEL_MESSAGE,
		PANEL_LABEL_BOLD, TRUE,
		PANEL_LABEL_STRING, "Display Options:",
		XV_HELP_DATA,           "clock:DisplayStyle",
		0);
	o-> secondsToggle =
		(Panel_item) xv_create (o-> panel, PANEL_TOGGLE,
		PANEL_CHOICE_STRINGS, "Seconds", 0,
		PANEL_VALUE, o->seconds,
		XV_HELP_DATA,           "clock:DisplayStyle",
		0);
	o-> dateToggle =
		(Panel_item) xv_create (o-> panel, PANEL_TOGGLE,
		PANEL_CHOICE_STRINGS, "Date", 0,
		PANEL_VALUE, o->date,
		XV_HELP_DATA,           "clock:DisplayStyle",
		0);
	layout_options (o);
	window_fit (o-> panel);  
	window_fit (o-> frame);
#ifndef NO_LIB_DESKSET
	ds_position_popup(c->frame, o->frame, DS_POPUP_LOR);
#endif NO_LIB_DESKSET
}

static void
backup_values (o)
	Options o;
{
	if (o==NULL) return;
	o-> faceBAK		= o-> face;
	o-> secondsBAK		= o-> seconds;
	o-> dateBAK		= o-> date;
}

static void
show_props (m, mi) 
	Menu m; Menu_item mi;
{
	Clock c = (Clock) xv_get (m, XV_KEY_DATA, key, 0);
	Options o;

	init_options (c);
	o = c-> options;
	backup_values (o);
	xv_set (o->faceChoice, PANEL_VALUE, o->face, 0);
	xv_set (o->secondsToggle, PANEL_VALUE, o->seconds, 0);
	xv_set (o->dateToggle, PANEL_VALUE, o->date, 0);
	xv_set (o->frame, FRAME_PROPS_PUSHPIN_IN, TRUE, 0);
	xv_set (o->frame, XV_SHOW, TRUE, 0); 
	clock_repaint_proc(c->canvas, c->pw, NULL);
}

static void
enable_timer (clnt, v1, v2, v3, v4)
{
	timer.it_value.tv_usec		= v1;
	timer.it_value.tv_sec		= v2; 
	timer.it_interval.tv_usec	= v3;
	timer.it_interval.tv_sec	= v4;
	(void) notify_set_itimer_func (clnt, 
		timer_expired,
		ITIMER_REAL, &timer, ITIMER_NULL);  			
}

static void
disable_timer (clnt)
{
	timer.it_value.tv_usec		= 0;
	timer.it_value.tv_sec		= 0; 
	timer.it_interval.tv_usec	= 0;
	timer.it_interval.tv_sec	= 0;
	(void) notify_set_itimer_func (clnt, 
		timer_expired,
		ITIMER_REAL, &timer, ITIMER_NULL);  			
}
	

static void
init_icon (c)
	Clock c;
{
	int w		= 64;
	int h		= 64;
	int dotsize	= w/12;
	icontempr	= make_image (w, h, c);
	iconpr		= make_image (w, h, c);
	icondotspr	= make_image (w, h, c);
	iconhandspr	= make_image (w, h, c);
	iconspotpr	= make_image (dotsize,  dotsize,  c);
	
	c->icon = (Icon) xv_create (NULL, ICON, 
		ICON_IMAGE, iconpr,
/*		WIN_REPAINT, icon_repaint,   */
		XV_KEY_DATA, key, c,
		0);

	c->iconpw = (Pixwin *) xv_get(c->icon, CANVAS_NTH_PAINT_WINDOW, 0);
/*	(void) xv_set(c->iconpw, XV_KEY_DATA, key, c, 0); */

	notify_interpose_event_func(c->icon, icon_interpose, 0);

	/* draw icon border 
	pw_write  (iconpr, 0, 0, w-1, h-1, PIX_CLR, 0, 0, 0);
        pw_vector (iconpr, 0, 0, w-1, 0, PIX_SET, 3);
        pw_vector (iconpr, 0, 0, 0, h-1, PIX_SET, 3);
        pw_vector (iconpr, w-1, 0, w-1, h-1, PIX_SET, 3);
        pw_vector (iconpr, 0, h-1, w-1, h-1, PIX_SET, 3);
	*/

	/*	init round tick marks		*/
	draw_circle (iconspotpr, armwidth(w)/8);
	paint_ticks (icondotspr, w/2, iconspotpr);
	/* icon_repaint (c->icon, c->iconpw, NULL);  */
}

static void
init_display (c)
	Clock c;
{
	ClockDisplay d	= (ClockDisplay)c->display;
	d->secondhand.lastSecX	= -1;
	d->secondhand.lastSecY	= -1;
	d->secondhand.lastSecX1	= -1;
	d->secondhand.lastSecY1	= -1;
        d->hands.angle1 = -1;
        d->hands.angle2 = -1;
        d->hands.width = -1;
	d-> fontHeight	= MIN_FONT_HEIGHT;
	d-> fontWidth	= MIN_FONT_WIDTH;	
	d-> font = (Xv_Font) xv_create (NULL, FONT, 
		FONT_FAMILY, FONT_FAMILY_LUCIDA,
		FONT_SIZE, 10,
		0);
	if (d->font==NULL) {
		d->font = (Xv_Font) xv_create (NULL, FONT, 
			FONT_NAME, "fixed",
			FONT_SIZE, 10,
			0);
		if (d->font==NULL) {
			cleanup(c);
			fprintf(stderr, "%s\n", "Cannot open font");
			exit(0);
		}
	}
}

static void
init_gray_patch()
{
	gray_patch = make_image(16, 16, key);
	pw_write (gray_patch, 0, 0, 16, 16, PIX_SRC, &my_fifty_patch, 0, 0);
}


init_clck (argc, argv)
	int  argc; char **argv;
{
	int now;
	struct tm *tm;
	Menu_item       tmp_item;
	char    **argscanner = argv;
	int clock_usersetsize;
	Clock clck	= (Clock) malloc (sizeof (ClockObject)); 	
	clck-> display	= (ClockDisplay) malloc (sizeof (DisplayInfo));
	clck-> options	= (Options) malloc (sizeof (ClockOptions));
	key		= xv_unique_key();


	clock_usersetsize = FALSE;
	while (*argscanner)
	{
		if (!strcmp(*argscanner, "-Ws") || !strcmp(*argscanner, "-size") || !strcmp(*argscanner, "-geometry"))
		{
			clock_usersetsize = TRUE;
		}
		argscanner++;
	}


	xv_init (XV_INIT_ARGC_PTR_ARGV, &argc, argv, 0);
	readrc (clck->options);
	init_gray_patch();
	init_numbers ();
	init_display (clck);
	init_icon (clck);
	clck-> frame = (Frame) xv_create (XV_NULL, FRAME, 
		FRAME_ARGC_PTR_ARGV, &argc, argv,
		FRAME_SHOW_HEADER, TRUE,
		FRAME_SHOW_FOOTER, FALSE,
		FRAME_ARGS, argc, argv,
		FRAME_SHOW_LABEL, TRUE,
		FRAME_LABEL, "",
		FRAME_PROPERTIES_PROC, show_props,  
		FRAME_ICON, clck->icon,
		XV_HELP_DATA, "clock:ClockFrame",
		0);
	notify_interpose_event_func(clck->frame, frame_interpose, 0);
	xv_set (clck-> frame, XV_KEY_DATA, key, clck, 0);
        clck-> menu = xv_create(XV_NULL, MENU, 
				MENU_TITLE_ITEM, "Clock", 
				XV_KEY_DATA, key, clck,
				0);

        tmp_item = xv_create(XV_NULL, MENUITEM,
                        MENU_STRING, "Properties",
                        MENU_ACTION_PROC, show_props,
                        MENU_CLIENT_DATA, 0,
                        XV_HELP_DATA, "clock:Properties",
                        0);

        xv_set(clck-> menu,
                MENU_APPEND_ITEM, tmp_item,
                0);

	clck-> canvas = (Canvas) xv_create (clck-> frame, CANVAS,
		CANVAS_AUTO_EXPAND, TRUE, 
		CANVAS_AUTO_SHRINK, TRUE,		
		CANVAS_AUTO_CLEAR, TRUE,			
		CANVAS_RETAINED, FALSE,
		CANVAS_RESIZE_PROC, clock_resize_proc,	
		XV_KEY_DATA, key, clck,		
		XV_SHOW, TRUE,
		XV_HELP_DATA, "clock:DisplayCanvas",
		0);					
	clck-> pw = (Pixwin *)
		 xv_get (clck-> canvas, CANVAS_NTH_PAINT_WINDOW, 0);       
	(void)xv_set(clck->pw, 
		WIN_CONSUME_KBD_EVENTS, KEY_LEFT(3), WIN_MOUSE_BUTTONS, 0,
		XV_KEY_DATA, key, clck,
		WIN_MENU, clck->menu,
		WIN_BIT_GRAVITY, ForgetGravity,  /* horse shit */
		0);
	notify_interpose_event_func(clck->pw, canvas_interpose, 0); 
	init_images (clck, (int) xv_get (clck->canvas, XV_WIDTH, 0),
		(int) xv_get (clck->canvas, XV_HEIGHT, 0));
	now = time (0);
	tm = localtime (&now);

	if (!clock_usersetsize)
	{
		xv_set (clck->canvas, 
			XV_HEIGHT, 
			digital_on (clck-> options) ?
			DEF_DIG_HEIGHT : DEF_ANALOG_HEIGHT,
			0);
		xv_set (clck->canvas, 
			XV_WIDTH, 
			digital_on (clck-> options) ?
			DEF_DIG_WIDTH : DEF_ANALOG_WIDTH, 
			0);
	}
	else
	{
		if (xv_get(clck->canvas, XV_HEIGHT) < (digital_on (clck-> options) ? MIN_DIG_HEIGHT : MIN_ANALOG_HEIGHT))
			xv_set (clck->canvas, 
				XV_HEIGHT, 
/*
				digital_on (clck-> options) ?
				MIN_DIG_HEIGHT : MIN_ANALOG_HEIGHT,
*/
			MIN_WINDOW_HEIGHT,
				0);

		if (xv_get(clck->canvas, XV_WIDTH) < (digital_on (clck-> options) ? MIN_DIG_WIDTH : MIN_ANALOG_WIDTH))
			xv_set (clck->canvas, 
				XV_WIDTH, 
/*
				digital_on (clck-> options) ?
				MIN_DIG_WIDTH : MIN_ANALOG_WIDTH, 
*/
			MIN_WINDOW_WIDTH,
				0);
	}

	clock_resize_proc (clck->canvas, xv_get(clck->canvas, XV_WIDTH), xv_get(clck->canvas, XV_HEIGHT));

	if (seconds_on (clck-> options))
		enable_timer (clck-> frame, 0, 2, 0, 1);
	else {
		timer_expired(clck->frame, NULL);
		enable_timer (clck-> frame, 0, 60-tm->tm_sec, 0, 60);
	}
	if (date_on(clck->options)) {
		paint_date(clck);
	}
	window_fit (clck-> frame);

	xv_main_loop (clck-> frame);
}


update_slots (c)
	Clock c;
{
	int i, spacing, startAt, fontWidth, windowWidth;

	if (c==NULL) return;
	fontWidth	= c-> display-> fontWidth;
	windowWidth	= (int) xv_get (c->canvas, XV_WIDTH, 0);
	spacing		= (1.0/8.0) * fontWidth;
	startAt		= (windowWidth - (6 * (fontWidth + spacing)))/2;

	for (i = 0; i < 6; i++)
	  c-> display-> slots[i] = (startAt + (i*(spacing + fontWidth)));
}


build_numbers (c)
	Clock c;
{
	int i, width, height,  nbnds = 1, npts[1], npts2[1];
	ClockDisplay d;

	if (c==NULL) return;
	npts[0]		= 4;
	npts2[0]	= 6;
	d		= c-> display;
	width		= d-> fontWidth;
	height		= d-> fontHeight;

	for (i = 0; i < 12; i++)  {
		if (d-> images[i] != NULL)
		(void) xv_destroy (d-> images[i]);
		d-> images[i] = 
		  (Server_image) xv_create (NULL, SERVER_IMAGE,
			XV_WIDTH, width,
			XV_HEIGHT, height,
			SERVER_IMAGE_DEPTH, 1,
			XV_KEY_DATA, key, c,
			0);
		pw_write (d-> images[i], 0, 0, width, height, PIX_CLR, 0, 0, 0);
	}

	for (i = 0; i < 11; i++)
		switch (i) {
		case 0: 
	          pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[0], PIX_SET,
			        NULLPR, 0, 0);
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[1], PIX_SET,
				NULLPR, 0, 0);
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[2], PIX_SET,
				NULLPR, 0, 0);
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[3], PIX_SET,
				NULLPR, 0, 0);
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[4], PIX_SET,
				NULLPR, 0, 0);
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[5], PIX_SET,
				NULLPR, 0, 0);
		  break;
		case 1:
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[6], PIX_SET,
			        NULLPR, 0, 0);
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[7], PIX_SET,
				NULLPR, 0, 0);
		  break; 
		case 2:
	          pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[8], PIX_SET,
			        NULLPR, 0, 0);
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[2], PIX_SET,
				NULLPR, 0, 0);
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[9], PIX_SET,
				NULLPR, 0, 0);
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[3], PIX_SET,
				NULLPR, 0, 0);
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts2, workingFont[14], PIX_SET,
				NULLPR, 0, 0);
		  break;
		case 3:
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[8], PIX_SET,
			        NULLPR, 0, 0);
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[2], PIX_SET,
				NULLPR, 0, 0);
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[4], PIX_SET,
				NULLPR, 0, 0);
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[10], PIX_SET,
				NULLPR, 0, 0);
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts2, workingFont[14], PIX_SET,
				NULLPR, 0, 0);
		  break;
		case 4:
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[1], PIX_SET,
			        NULLPR, 0, 0);
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[2], PIX_SET,
				NULLPR, 0, 0);
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[4], PIX_SET,
				NULLPR, 0, 0);
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts2, workingFont[14], PIX_SET,
				NULLPR, 0, 0);
		  break;
		case 5:
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[11], PIX_SET,
			        NULLPR, 0, 0);
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[1], PIX_SET,
				NULLPR, 0, 0);
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[4], PIX_SET,
				NULLPR, 0, 0);
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[5], PIX_SET,
				NULLPR, 0, 0);
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts2, workingFont[14], PIX_SET,
				NULLPR, 0, 0);
		  break;
		case 6:
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[11], PIX_SET,
			        NULLPR, 0, 0);
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[1], PIX_SET,
				NULLPR, 0, 0);
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[3], PIX_SET,
				NULLPR, 0, 0);

		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[4], PIX_SET,
				NULLPR, 0, 0);
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[5], PIX_SET,
				NULLPR, 0, 0);
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts2, workingFont[14], PIX_SET,
				NULLPR, 0, 0);
		  break;
		case 7:
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[8], PIX_SET,
			        NULLPR, 0, 0);
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[2], PIX_SET,
				NULLPR, 0, 0);
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[4], PIX_SET,
				NULLPR, 0, 0);
		  break;
		case 8:
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[0], PIX_SET,
			        NULLPR, 0, 0);
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[1], PIX_SET,
				NULLPR, 0, 0);
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[2], PIX_SET,
				NULLPR, 0, 0);
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[3], PIX_SET,
				NULLPR, 0, 0);
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[4], PIX_SET,
				NULLPR, 0, 0);
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[5], PIX_SET,
				NULLPR, 0, 0);
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts2, workingFont[14], PIX_SET,
				NULLPR, 0, 0);
		  break;
		case 9:
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[0], PIX_SET,
			        NULLPR, 0, 0);
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[1], PIX_SET,
				NULLPR, 0, 0);
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[2], PIX_SET,
				NULLPR, 0, 0);
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[4], PIX_SET,
				NULLPR, 0, 0);
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[5], PIX_SET,
				NULLPR, 0, 0);
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts2, workingFont[14], PIX_SET,
				NULLPR, 0, 0);
		  break;
		case 10:
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[12], PIX_SET,
			        NULLPR, 0, 0);
		  pw_polygon_2 (d-> images[i],
				0, 0, nbnds, npts, workingFont[13], PIX_SET,
				NULLPR, 0, 0);
		  break;
		  
	  }
}
	     
main (argc, argv)
int  argc; char **argv;

{	
	initializing = 1;
	init_clck(argc, argv);
	exit(0);
}
