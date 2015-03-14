/*
 * molecule.c
 *   xgas: Copyright 1990 Larry Medwin: @(#)molecule.c	1.1 2/9/90
 *   Larry Medwin -- Dec. 18, 1989
 *   Larry Medwin -- Jan. 22, 1991: addMolecules()
 */

#include "xgas.h"

/*
 *  ADD MOLECULES
 *
 *  Place molecules with the mouse buttons:
 *	MB 1: one molecule
 *	MB 2: as many as possible
 *  Pick a random angle to start trajectory.
 *
 *  Drawing Molecules:
 *    This routine must guarantee that:
 *        Molecules are drawn exactly once in their new position.
 *        The "current" allPos array is updated with this position.
 *        nmolecules is updated consistently.
 */
void addMolecule( w, data, event)
    Widget w;
    LabData *data;
    XEvent *event;
{
    Molecule *mol;
    float v;
    float theta;
    Coord pos;

    /* Do we still have room for this one? */
    if( data->nmolecules >= data->maxMolecules) {
	return;
    }

    /* Find a place for it in the "global data" structure */
    mol = &data->molecules[ data->nmolecules ];

    /* Get starting point from mouse position */
    /*    data->scale.x has units of pixels/mm */
    pos.x = event->xbutton.x / data->scale.x;
    pos.y = event->xbutton.y / data->scale.y;

    /* Init temperature of molecule (JUST A TEST: always set to one temp. */
    mol->temperature = data->chamber[0].temperature;

    /* Pick a random angle */
    theta = frand( 0.0, TWO_PI);

    /* Scale the velocities by the kinetic energy */
    v = vEquilibrium( mol->temperature);
    mol->yCoeff[1] = sin( theta ) * v;
    mol->xCoeff[1] = cos( theta ) * v;

    /* Form the equations of motion */
    mol->xCoeff[0] = pos.x;
    mol->yCoeff[0] = pos.y;

    /* Fake last collision: use current location as collisionPos */
    mol->collisionPos.x = pos.x;
    mol->collisionPos.y = pos.y;
    mol->collisionTime = data->time;

    /* Which box are we in? (wall[4].end[1] is far corner of box 0) */
    if( pos.x == data->chamber[0].walls[4].end[1].x) {

	/* starting in hole; which way are we moving? */
	if( mol->xCoeff[1] > 0.0) {
	    mol->thisBox = 1;
	}
	else {
	    mol->thisBox = 0;
	}
    }
    else if( pos.x < data->chamber[0].walls[4].end[1].x) {
	mol->thisBox = 0;
    }
    else {
	mol->thisBox = 1;
    }
    if (mol->thisBox != 0 && mol->thisBox != 1) {
	error("In addMolecule(): couldn't pick a box.", 0);
    }

    /* next collision? */
    mol->collisionWall = NULL;
    findNextCollision( mol, data);

    /* Add molecule to the allPos array of all molecule positions */
    data->allPos[ data->nmolecules*2 +  data->timestep % 2 ].x
	= (short) event->xbutton.x;
    data->allPos[ data->nmolecules*2 + data->timestep % 2 ].y
	= (short) event->xbutton.y;

    /* Draw it in its initial position */
    XFillRectangle( XtDisplay(w), XtWindow(w), data->MoleculeGC,
	(int) data->allPos[ data->nmolecules*2 + data->timestep % 2 ].x,
	(int) data->allPos[ data->nmolecules*2 + data->timestep % 2 ].y,
	(int) MOLECULE_SIZE, (int) MOLECULE_SIZE);

    /* Increment number of molecules */
    data->nmolecules += 1;
}

void addMolecules( w, data, event)
    Widget w;
    LabData *data;
    XEvent *event;
{
    int i;

    switch (event->xbutton.button) {
	case 1:		addMolecule( w, data, event);
			break;
	case 2:		for (i=data->nmolecules; i<data->maxMolecules; i++)
			    addMolecule( w, data, event);
			break;
	default:
			break;
    }
}

