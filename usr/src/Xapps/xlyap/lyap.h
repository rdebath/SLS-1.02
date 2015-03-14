/*
 *	@(#) lyap.h
 */
/*
 *	Written by Ron Record (rr@sco.com) 03 Sep 1991.
 */

#include "patchlevel.h"
#include <assert.h>
#include <math.h>
#include <memory.h>
#include <stdio.h>
#include <X11/Xlib.h> 
#include <X11/StringDefs.h> 
#include <X11/keysym.h> 
#include <X11/cursorfont.h> 
#include <X11/Xutil.h>
#include <X11/Xatom.h>

#define ABS(a)	(((a)<0) ? (0-(a)) : (a) )
#define Min(x,y) ((x < y)?x:y)
#define Max(x,y) ((x > y)?x:y)

#ifdef SIXTEEN_COLORS
#define MAXPOINTS  128
#define MAXFRAMES 4
#define MAXCOLOR 16
int maxcolor=16, startcolor=0, color_offset=0, mincolindex=1;
int dwell=50, settle=25;
int width=128, height=128, xposition=128, yposition=128;
#else
#define MAXPOINTS  256
#define MAXFRAMES 8
#define MAXCOLOR 256
int maxcolor=256, startcolor=17, color_offset=96, mincolindex=33;
int dwell=100, settle=50;
int width=256, height=256, xposition=256, yposition=256;
#endif

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

int bottom, screen;
Display*	dpy;
char*		displayname = 0;

extern double log();
extern double fabs();
extern long time();
extern int optind;
extern char *optarg;

unsigned long foreground, background;

Window canvas;

void     resize();
void     redisplay();
void     quit();
void     Spin();
void     start_iterate();
void     stop_iterate();
void	 show_defaults();
void	 StartRubberBand();
void	 TrackRubberBand();
void	 EndRubberBand();
void	 CreateXorGC();
void	 InitBuffer();
void	 BufferPoint();
void	 FlushBuffer();

typedef struct {
	int x, y;
} xy_t;

typedef struct {
	int start_x, start_y;
	int last_x, last_y;
	} rubber_band_data_t;

typedef struct {
	Cursor band_cursor;
	double p_min, p_max, q_min, q_max;
	rubber_band_data_t rubber_band;
	} image_data_t;

typedef struct points_t {
	XPoint data[MAXCOLOR][MAXPOINTS];
	int npoints[MAXCOLOR];
	} points_t;

points_t Points;
image_data_t rubber_data;

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

GC Data_GC[MAXCOLOR], RubberGC;

#define MAXINDEX 64
#define FUNCMAXINDEX 16
#define MAXWHEELS 7
#define NUMMAPS 5

typedef double (*PFD)();

double logistic(), circle(), leftlog(), rightlog(), doublelog();
double dlogistic(), dcircle(), dleftlog(), drightlog(), ddoublelog();
PFD map, deriv;
PFD Maps[NUMMAPS] = { logistic, circle, leftlog, rightlog, doublelog };
PFD Derivs[NUMMAPS] = { dlogistic, dcircle, dleftlog, drightlog, ddoublelog };

int aflag=0, bflag=0, wflag=0, hflag=0, Rflag=0;
double pmins[NUMMAPS] = { 2.0, 0.0, 0.0, 0.0, 0.0 };
double pmaxs[NUMMAPS] = { 4.0, 1.0, 6.75, 6.75, 16.0 };
double amins[NUMMAPS] = { 2.0, 0.0, 0.0, 0.0, 0.0 };
double aranges[NUMMAPS] = { 2.0, 1.0, 6.75, 6.75, 16.0 };
double bmins[NUMMAPS] = { 2.0, 0.0, 0.0, 0.0, 0.0 };
double branges[NUMMAPS] = { 2.0, 1.0, 6.75, 6.75, 16.0 };

int   forcing[MAXINDEX] = { 0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,
			0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,
			0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1 };
int   Forcing[FUNCMAXINDEX] = { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 };

int   maxindex = MAXINDEX;
int   funcmaxindex = FUNCMAXINDEX;
double	min_a=2.0, min_b=2.0, a_range=2.0, b_range=2.0, minlyap=1.0;
double  max_a=4.0, max_b=4.0;
double  start_x=0.65, lyapunov, a_inc, b_inc, a, b;
int	numcolors=16, numfreecols, displayplanes, lowrange;
xy_t	point, velocity;
Pixmap  pixmap;
Colormap cmap;
XColor	Colors[MAXCOLOR];
double  *exponents[MAXFRAMES];
double  a_minimums[MAXFRAMES], b_minimums[MAXFRAMES];
double  a_maximums[MAXFRAMES], b_maximums[MAXFRAMES];
double  minexp, maxexp, prob=0.5;
int     expind[MAXFRAMES]={0}, resized[MAXFRAMES]={0};
int	numwheels=MAXWHEELS, force=0, Force=0, negative=1;
int     rgb_max=65000, nostart=1, stripe_interval=7;
int	save=1, show=0, useprod=1, spinlength=256, savefile=0;
int	maxframe=0, frame=0, dorecalc=0, mapindex=0, run=1;
char	*outname="lyap.out";
