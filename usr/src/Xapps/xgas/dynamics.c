/*
 * dynamics.c
 *   xgas: Copyright 1990 Larry Medwin: @(#)dynamics.c	1.5 2/9/90
 *   Larry Medwin -- Dec. 15, 1989
 *   fixed: lm 3-24-91
 */

#include "xgas.h"
static void inertia();
static void collide();
static double hit();
static double hitHole();

/* DYNAMICS */
void dynamics( mol, data )
    Molecule *mol;
    LabData *data;
{
    /* Move molecule */
    while( mol->collisionTime <= data->timestepSize) {
	collide( mol, data );
	findNextCollision( mol, data);
    }
    inertia( mol, data );
}

/* INERTIA() */
static void inertia( mol, data)
    Molecule *mol;
    LabData *data;
{
    /* Solve equations of motion */
    mol->pos.x = mol->xCoeff[0] += mol->xCoeff[1] * data->timestepSize;
    mol->pos.y = mol->yCoeff[0] += mol->yCoeff[1] * data->timestepSize;
    mol->collisionTime -= data->timestepSize;
}

/*
 * COLLIDE()
 * bounce a molecule off a wall with given temperature
 *   recompute its new velocity,
 *   including next collision info.
 */
static void collide( mol, data)
    Molecule *mol;
    LabData *data;
{
    float vMagnitude, hypot;
    float x, y;
    float theta, thetaRand;
    WallType *wallParams;
    int corner;


    /* Find out about the wall we're about to hit */
    wallParams = &WallParam[ mol->collisionWall->type ];

    /*
     * Are we colliding with a corner?
     */
    if ((corner = whichCorner( mol->collisionPos.x, mol->collisionPos.y,
		mol->thisBox, data)) != 0)
	wallParams = &WallParam[ corner ];

    /* Reflect the trajectory */
    mol->xCoeff[1] *= wallParams->wallReflect.x;
    mol->yCoeff[1] *= wallParams->wallReflect.y;

    /* Rotate the trajectory into the canonical orientation */
    x = mol->xCoeff[1] * wallParams->toRotate[0][0] +
	mol->yCoeff[1] * wallParams->toRotate[0][1];
    y = mol->xCoeff[1] * wallParams->toRotate[1][0] +
	mol->yCoeff[1] * wallParams->toRotate[1][1];

    /* Find reflection angle theta */
    if ( fabs ( x ) > SMALL ) theta = atan ( y / x );
    else if ( y > 0.0 ) theta = M_PI_2;
    else theta = -M_PI_2;

    /* Randomize angle by the weight "randomBounce" */
    if (corner) thetaRand = frand( 0.0, M_PI_2);
    else thetaRand = frand( -M_PI_2, M_PI_2);

    theta = (1.0 - data->randomBounce) * theta
		+ data->randomBounce * thetaRand;

    /* Molecule approaches equilibrium according to weight "equilibrium" */
    mol->temperature = (1.0 - data->equilibrium) * mol->temperature
	+ data->equilibrium * data->chamber[mol->thisBox].temperature;

    /* Find velocity vector magnitude */
    vMagnitude = vEquilibrium( mol->temperature);

    /* ... and components */
    x = vMagnitude * cos( theta );
    y = vMagnitude * sin( theta );

    /* Rotate the trajectory back to its original orientation */
    mol->xCoeff[1] = x * wallParams->fromRotate[0][0] +
			y * wallParams->fromRotate[0][1];
    mol->yCoeff[1] = x * wallParams->fromRotate[1][0] +
			y * wallParams->fromRotate[1][1];
}

/*
 * FIND TIME AND POSITION OF NEXT COLLISION
 *   given the equations of motion for the molecule
 */
void findNextCollision( mol, data)
    Molecule *mol;
    LabData *data;
{
    int i;
    int box;
    double deltaT;
    Coord lastCollision;

    /* Save position of last collision */
    lastCollision.x = mol->collisionPos.x;
    lastCollision.y = mol->collisionPos.y;

    box = mol->thisBox;

    /* Do we go through the hole? (walls[0]) */
    if( hitHole( mol, &data->chamber[box].walls[0], data) > 0.0) {

        /* Move into the other box */
        box = 1 - box;
    }

    /*
     * Now check for collisions with the walls
     * Update collisionPos in hit()
     */
    for( i=1; i<NWALLS ; i++ ) {
	if((mol->collisionWall != &data->chamber[box].walls[i])
	  && (( deltaT = hit( mol, &data->chamber[box].walls[i]))
		 > 0.0)) {
	  break;
	}
    }

    if( deltaT == -1) {
	error("In findNextCollision(): couldn't find a wall to hit.",
	    data->time);
    }

    /* Correct the equations of motion for the particle */
    mol->xCoeff[1]
	= (mol->collisionPos.x - lastCollision.x) / deltaT;
    mol->yCoeff[1]
	= (mol->collisionPos.y - lastCollision.y) / deltaT;
    mol->xCoeff[0] = lastCollision.x;
    mol->yCoeff[0] = lastCollision.y;

    /* Update collision info */
    mol->collisionTime = deltaT;
    mol->collisionWall = &data->chamber[box].walls[i];
    mol->thisBox = box;
}

/*
 * HIT
 * Return collision time if the molecule will hit this wall
 *   otherwise -1.0
 */
static double hit( mol, wall)
    Molecule *mol;
    Wall *wall;
{
    double deltaT;
    int xPos, yPos;

    /* Find intersection of trajectory and wall */

    /* Horizontal wall */
    if( wall->type == TOP || wall->type == BOTTOM) {

        if( fabs( mol->yCoeff[1]) >= SMALL) {
	    deltaT = (wall->end[0].y-mol->collisionPos.y) / mol->yCoeff[1];

	    /* Are we headed in that direction? */
	    if( deltaT < 0.0)
		return -1.0;

	    xPos = mol->collisionPos.x + mol->xCoeff[1] * deltaT;

	    /* Hits wall between endpoints */
	    if( xPos >= wall->end[0].x && xPos <= wall->end[1].x) {
		mol->collisionPos.x = xPos;
		mol->collisionPos.y = wall->end[0].y;
		return deltaT;
	    }
	    else return -1.0;
	}
	else return -1.0;
    }

    /* Vertical wall */
    else if ( wall->type == LEFT || wall->type == RIGHT) {
        if( fabs( mol->xCoeff[1]) >= SMALL) {
	    deltaT = (wall->end[0].x - mol->collisionPos.x) / mol->xCoeff[1];

	    /* Are we headed in that direction? */
	    if( deltaT < 0.0)
		return -1.0;

	    yPos = mol->collisionPos.y + mol->yCoeff[1] * deltaT;

	    /* Hits wall between endpoints */
	    if( yPos >= wall->end[0].y && yPos <= wall->end[1].y) {
		mol->collisionPos.x = wall->end[0].x;
		mol->collisionPos.y = yPos;
		return deltaT;
	    }
	    else return -1.0;
	}
	else return -1.0;
    }
    else {
	error(" In hit, illegal wall type.", 0);
	return -1.0;
    }
}

/*
 * HITHOLE
 * Return collision time if the molecule will go through hole
 *   otherwise -1.0
 */
static double hitHole( mol, wall, data)
    Molecule *mol;
    Wall *wall;
    LabData *data;
{
    double deltaT;
    int xPos, yPos;

    /* Find intersection of trajectory and hole */

    /* OK to divide? */
    if( fabs( mol->xCoeff[1]) < SMALL)
	return -1.0;

    deltaT = (wall->end[0].x - mol->collisionPos.x) / mol->xCoeff[1];

    /* Are we headed in that direction? */
    /* ... or already on wall or corner? use <=, not <   lm 4/4/91 */
    if( deltaT <= 0.0)
	return -1.0;

    /* FInd intersection with barrier wall */
    yPos = mol->collisionPos.y + mol->yCoeff[1] * deltaT;

    /* Does it go through hole between endpoints? */
    if( !( yPos > wall->end[0].y && yPos < wall->end[1].y))
	return -1.0;

    /*
     * Check that x-intersection with horizontal wall will not
     *   be truncated to corner position.   This happens once
     *   every 10^7 collisions.
     */

    /* This isn't a problem for molecules moving in -x direction */
    if( mol->xCoeff[1] < 0.0)
	return deltaT;

    /* OK to divide? */
    if( fabs( mol->yCoeff[1]) < SMALL)
	return -1.0;

    /* Moving in +y or -y direction? */
    if( mol->yCoeff[1] > 0.0)
	deltaT = (data->heightMM - mol->collisionPos.y) / mol->yCoeff[1];
    else
	deltaT = (0 - mol->collisionPos.y) / mol->yCoeff[1];

    /* Where do we hit the horizontal wall? */
    xPos = mol->collisionPos.x + mol->xCoeff[1] * deltaT;

    /*
     *  If xPos truncated so that it lies at the x-position of the corner,
     *    then the molecule really doesn't go through the hole.
     */
    if( xPos == data->chamber[0].walls[0].end[0].x)
	return -1.0;

    /* It went through! */
    return deltaT;
}
