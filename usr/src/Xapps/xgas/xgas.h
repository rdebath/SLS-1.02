/*
 * xgas.h
 *   gas: Copyright 1990 Larry Medwin: @(#)gas.h	1.6 2/9/90
 *   Larry Medwin Dec. 15, 1989, April 1991
 */

#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Scrollbar.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Toggle.h>
#include "XGas.h"
#include <X11/Xutil.h>
#include <X11/Xlib.h>
#include <math.h>

/* Force the numerical instability out into the open */
#define double float

#define NWALLS 6
#define MOLECULE_SIZE 4
#define KB 1.38062e-16		/* Boltzmann's constant in ergs/degree */
#define MASS 3.32e-24		/* H2 molecule mass in grams */
#define MAXTEMP 500		/* degrees K */
#define INITTEMP 300            /* initial value */
#define SMALL 1.0e-10		/* effectively zero */
#define TWO_PI 2.0 * 3.14159265358979323846
#ifndef M_PI_2
#define M_PI_2 1.57079632679489661923
#endif

typedef struct _Coord {
    int x, y;
} Coord;

/* Walls and Corners */
typedef struct _WallType {
    Coord wallReflect;		/* to compute reflection angle */
    int toRotate[2][2];		/* rotate to canonical orientation */
    int fromRotate[2][2];		/* rotate from canonical orientation */
} WallType;

/* Wall orientations */
#define TOP 0
#define RIGHT 1
#define BOTTOM 2
#define LEFT 3

/* Corner orientations */
#define NW 4
#define SW 5
#define SE 6
#define NE 7

extern WallType WallParam[];	/* Defined in gas.c */

typedef struct _Wall {
    Coord end[2];		/* endpoints of line (first nearer origin) */
    int type;			/* TOP, RIGHT, BOTTOM, LEFT */
} Wall;

/* Molecules */

typedef struct _Molecule {
    float temperature;		/* Kinetic energy in degrees Kelvin */
    double xCoeff[2];		/* coefficients of parametric equation */
    double yCoeff[2];		/* coefficients of parametric equation */
    Coord pos;			/* current position */
    Coord collisionPos;		/* point on wall of next collision */
    float collisionTime;        /* microseconds until next collision */
    Wall *collisionWall;	/* wall of next collision */
    int thisBox;		/* array index of box the molecule is "in" */
} Molecule;

#ifdef ENHANCEMENT
/* Enhancement: different kinds of molecules: */
    GC thisGC;			/* Graphics Context for this molecule */
    Pixmap thisPixmap;		/* Pixmap for this molecule */
    float mass;			/* Molecular mass */
#endif

/* Chamber */

typedef struct _Box {
    Wall walls[ NWALLS ];	/* walls[0] is the hole */
    Widget control;		/* scrollbar to control temperature */
    Widget display;		/* readout of temperature */
    float temperature;		/* in degrees K */
} Box;

/* Client data structure */
typedef struct _labData {
    Dimension width, height;	/* in pixels */
    float widthMM, heightMM;	/* in millimeters */
    Coord scale;
    Box chamber[2];
    int nmolecules;
    Molecule *molecules;	/* was [MAXMOLECULES] */
    XRectangle *allPos;		/* was [MAXMOLECULES][2]last & current pos */
    float time;			/* Simulated time */
    int timestep;		/* number of this timestep */

    /* Resources */
    float timestepSize;	/* resolution of simulated time in microseconds */
    int delay;		/* delay between calls to doTimestep */
    float randomBounce;	/* randomness of collision: 0 none, 1 all */
    float equilibrium;	/* approach to equilibrium: 0 never,1 immediate */
    int maxMolecules;	/* number allowed */
    Pixel background, foreground;

    /* X stuff */
    Widget lab;
    Widget clock;
    GC WallGC, MoleculeGC;
    XtIntervalId timer;
} LabData, *LabDataPtr;

/* Forward references */
/* in dynamics.c: */
	void findNextCollision();
	void dynamics();
	void doTimestep();
	void run_callback();
	void pause_callback();
	void oneTimestep();
/* in chamber.c */
	int whichCorner();
	void labInit();
	void labResize();
	void labExpose();
	void addMolecules();
	void changeTemp();
	float vEquilibrium();
	float frand();
/* in util.c */
	void quit_callback();
