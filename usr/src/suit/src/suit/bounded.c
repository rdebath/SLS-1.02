/* (C) 1990 Copyright Rector and Visitors of the University of Virginia */

#include <math.h>
#include "suit.h"



#define PI                  3.14159
#define ROUND(A)            ( ((A) > 0)? (int) (A + 0.5) : (int) (A - 0.5) )


#define PREVIOUS_CURRENT_VALUE "previous current value"

#define DOUBLE_EQUAL(X,Y)	( ABS((X) - (Y)) < 0.0001)
#define PERCENTAGE(VAL,MIN,MAX)	( (double) ( ((VAL)-MIN) / ((MAX)-(MIN)) ) )


PRIVATE void OptimizePaint (SUIT_object o, char *propName, char *propType, Pointer p1, Pointer p2)
{
    if (!(SUIT_stringsMatch (propName, CURRENT_VALUE) ||
	  SUIT_stringsMatch (propName, PREVIOUS_CURRENT_VALUE)))
	OBJECT_OPTIMIZED(o) = FALSE;
}



PRIVATE void ConstrainValues (SUIT_object o)
{
    double min = SUIT_getDouble (o, MINIMUM_VALUE);
    double max = SUIT_getDouble (o, MAXIMUM_VALUE);
    double curr = SUIT_getDouble (o, CURRENT_VALUE);
    double gran = SUIT_getDouble (o, GRANULARITY);
    SUIT_object elevator = SUIT_getEmployee (o, "scroll bar", 1);

    if (max < min)
	max = min;
    if (curr < min)
	curr = min;
    else if (curr > max)
	curr = max;
    if (gran > 0.0)
	curr = floor(curr/gran+0.0000000001) * gran; /* this fudge factor fixes a rounding error */
    SUIT_setDouble (o, MAXIMUM_VALUE, max);
    SUIT_setDouble (o, CURRENT_VALUE, curr);
    SUIT_setDouble (elevator, CURRENT_VALUE, (min != max)? PERCENTAGE(curr, min,max) : -1.0);
}



PRIVATE void ConstrainValuesCB (SUIT_object o, char *propName, char *propType, Pointer new, Pointer old)
{
    if (SUIT_stringsMatch (propName, MAXIMUM_VALUE) ||
	SUIT_stringsMatch (propName, MINIMUM_VALUE) ||
	SUIT_stringsMatch (propName, CURRENT_VALUE) ||
	SUIT_stringsMatch (propName, GRANULARITY)) {
	ConstrainValues (o);
    }
    else if (SUIT_stringsMatch (propName, PERCENT_FULL)) {
	SUIT_object elevator = SUIT_getEmployee (o, "scroll bar", 1);
	SUIT_setDouble (elevator, PERCENT_FULL, SUIT_getDouble (o, PERCENT_FULL));
    }
}



/*------  THERMOMETER style for Bounded Integer  -------*/

#define TICK_LENGTH  0.1


void PaintThermometer (SUIT_object o)
{
    double min, max, curr, granularity, i;
    boolean horizontal = SUIT_stringsMatch("horizontal thermometer", SUIT_getEnumString(o, ACTIVE_DISPLAY));

    max = SUIT_getDouble(o, MAXIMUM_VALUE);
    min = SUIT_getDouble(o, MINIMUM_VALUE);
    curr = SUIT_getDouble(o, CURRENT_VALUE);

    granularity = SUIT_getDouble(o,GRANULARITY);
    
    if (horizontal) 
	GP_fillRectangleCoord(0.0, 0.0, PERCENTAGE(curr, min, max), 1.0);
    else
	GP_fillRectangleCoord(0.0, 0.0, 1.0, PERCENTAGE(curr, min, max) );

    if (granularity > 0.0 && SUIT_getBoolean (o, HAS_TICK_MARKS)) {
	for (i=curr; i<=max; i+=granularity) {
	    double temp = PERCENTAGE(i,min,max);
	    if (horizontal) {
		GP_lineCoord (temp,0.0,temp,TICK_LENGTH);
		GP_lineCoord (temp,1.0-TICK_LENGTH,temp,1.0);
	    } else {
		GP_lineCoord (0.0,temp,TICK_LENGTH,temp);
		GP_lineCoord (1.0-TICK_LENGTH,temp,1.0,temp);
	    }
	}

	/* draw ticks in background color if they're in the "mercury" */
        GP_setColor(SUIT_getColor(o, BACKGROUND_COLOR) );
	for (i=min; i<curr; i+=granularity) {
	    double temp = PERCENTAGE(i,min,max);
	    if (horizontal) {
		GP_lineCoord (temp,0.0,temp,TICK_LENGTH);
		GP_lineCoord (temp,1.0-TICK_LENGTH,temp,1.0);
	    } else {
		GP_lineCoord (0.0,temp,TICK_LENGTH,temp);
		GP_lineCoord (1.0-TICK_LENGTH,temp,1.0,temp);
	    }
	}
    }
}



void HitThermometer (SUIT_object o, SUIT_event e)
{
    SUIT_callbackFunctionPtr funct;
    double min = SUIT_getDouble (o, MINIMUM_VALUE);
    double max = SUIT_getDouble (o, MAXIMUM_VALUE);
    double curr = SUIT_getDouble (o, CURRENT_VALUE);
    boolean horizontal = SUIT_stringsMatch ("horizontal thermometer",
					    SUIT_getEnumString(o, ACTIVE_DISPLAY));
    SUIT_viewport vp;
    int width, height;

    vp = OBJECT_VIEWPORT(o);
    width = vp.top_right.x - vp.bottom_left.x;
    height = vp.top_right.y - vp.bottom_left.y;

    if (horizontal)
      curr = min + (max - min) *  (double)e.relativePixelLocation.x / (double)width;
    else
      curr = min + (max - min) *  (double)e.relativePixelLocation.y / (double)height;

    SUIT_setDouble (o, CURRENT_VALUE, curr);

    funct = (SUIT_callbackFunctionPtr) SUIT_getFunctionPointer (o, CALLBACK_FUNCTION);
    if (funct != NULL) 
      funct (o);
} 



/*------  ANALOG display style for Bounded Integer  -------*/

#define ARROW_BUTTON_HEIGHT           0.25
#define NEEDLE_LENGTH                 0.75


void PaintNeedle (SUIT_object o, double min, double max, double curr)
{
    double angle = PI * (1.0 - PERCENTAGE(curr,min,max));

    if (angle > 0.0001 && angle < PI-0.0001)
	GP_lineCoord(0.0, 0.0,NEEDLE_LENGTH*cos(angle), NEEDLE_LENGTH*sin(angle));

    if (SUIT_getBoolean(o, HAS_ARROW)) {
      double arrow_angle = PI*SUIT_getInteger(o,ARROWHEAD_ANGLE)/180;
      double arrow_head_length = SUIT_getDouble(o,ARROWHEAD_LENGTH);
      double x = (NEEDLE_LENGTH-arrow_head_length*cos(arrow_angle))*cos(angle);
      double y = (NEEDLE_LENGTH-arrow_head_length*cos(arrow_angle))*sin(angle);
      double dx = arrow_head_length*sin(arrow_angle)*cos(PI/2.0-angle);
      double dy = arrow_head_length*sin(arrow_angle)*sin(PI/2.0-angle);
      if (dy<0) dy = -dy;
      if (angle <= PI/2) {
	  if (!(DOUBLE_EQUAL(angle,0))) 
	      GP_lineCoord (NEEDLE_LENGTH*cos(angle),NEEDLE_LENGTH*sin(angle),x+dx,y-dy);
	  GP_lineCoord (NEEDLE_LENGTH*cos(angle),NEEDLE_LENGTH*sin(angle),x-dx,y+dy);
      } else {
	  GP_lineCoord (NEEDLE_LENGTH*cos(angle),NEEDLE_LENGTH*sin(angle),x+dx,y+dy);
	  if (!(DOUBLE_EQUAL(angle,PI)))
	      GP_lineCoord (NEEDLE_LENGTH*cos(angle),NEEDLE_LENGTH*sin(angle),x-dx,y-dy);
      }
    }
}



void PaintAnalogDisplay (SUIT_object o)
{
    double curr, tick_gran;
    double i, angle;
    GP_point v[7];

    double max = SUIT_getDouble (o, MAXIMUM_VALUE);
    double min = SUIT_getDouble (o, MINIMUM_VALUE);

    GP_setWindow (GP_defRectangle(-1.0, -ARROW_BUTTON_HEIGHT, 1.0, 1.0));

    if (OBJECT_OPTIMIZED(o)) {
	GP_setColor (SUIT_getColor(o, BACKGROUND_COLOR));
	curr = SUIT_getDouble (o, PREVIOUS_CURRENT_VALUE);
	PaintNeedle (o, min, max, curr);
    } else {
	tick_gran = SUIT_getDouble (o, GRANULARITY);
	if (tick_gran > 0.0 && SUIT_getBoolean(o, HAS_TICK_MARKS))
	    for (i=min; i <= max; i += tick_gran) {
		angle = PI * PERCENTAGE(i,min,max);
		GP_lineCoord ((1-TICK_LENGTH)*cos(angle), (1-TICK_LENGTH)*sin(angle), 
			      cos(angle), sin(angle));
	    }

	GP_setColor (SUIT_getColor (o, BUTTON_BACKGROUND_COLOR));
	GP_fillRectangleCoord(-1.0, -ARROW_BUTTON_HEIGHT, 1.0, 0.0);
	GP_setColor (SUIT_getColor (o, BORDER_COLOR));
	GP_rectangleCoord(-1.0, -ARROW_BUTTON_HEIGHT, 0.0, 0.0);
	GP_rectangleCoord(0.0, -ARROW_BUTTON_HEIGHT, 1.0, 0.0);
	
	GP_setColor (SUIT_getColor (o, BUTTON_FOREGROUND_COLOR));
	
	GP_setLineWidth (1);
	v[0] = GP_defPoint(0.8,-ARROW_BUTTON_HEIGHT/2);
	v[1] = GP_defPoint(0.6,-ARROW_BUTTON_HEIGHT/4);
 	v[2] = GP_defPoint(0.6,-3*ARROW_BUTTON_HEIGHT/8);
 	v[3] = GP_defPoint(0.2,-3*ARROW_BUTTON_HEIGHT/8);
 	v[4] = GP_defPoint(0.2,-5*ARROW_BUTTON_HEIGHT/8);
 	v[5] = GP_defPoint(0.6,-5*ARROW_BUTTON_HEIGHT/8);
 	v[6] = GP_defPoint(0.6,-3*ARROW_BUTTON_HEIGHT/4);
 	GP_fillPolygon (7, v);
	v[0] = GP_defPoint(-0.8,-ARROW_BUTTON_HEIGHT/2);
	v[1] = GP_defPoint(-0.6,-ARROW_BUTTON_HEIGHT/4);
 	v[2] = GP_defPoint(-0.6,-3*ARROW_BUTTON_HEIGHT/8);
 	v[3] = GP_defPoint(-0.2,-3*ARROW_BUTTON_HEIGHT/8);
 	v[4] = GP_defPoint(-0.2,-5*ARROW_BUTTON_HEIGHT/8);
 	v[5] = GP_defPoint(-0.6,-5*ARROW_BUTTON_HEIGHT/8);
 	v[6] = GP_defPoint(-0.6,-3*ARROW_BUTTON_HEIGHT/4);
 	GP_fillPolygon (7, v);
    }	

    GP_setColor (SUIT_getColor(o, NEEDLE_COLOR));
    curr = SUIT_getDouble (o, CURRENT_VALUE);
    PaintNeedle (o, min, max, curr);
    OBJECT_OPTIMIZED(o) = TRUE;
    SUIT_suspendMarkingRedisplay(o);
    SUIT_setDouble (o, PREVIOUS_CURRENT_VALUE, curr);
    SUIT_makePropertyTemporary (o, PREVIOUS_CURRENT_VALUE, OBJECT);
    SUIT_resumeMarkingRedisplay(o);
}



void HitAnalogDisplay (SUIT_object o, SUIT_event e)
{
    double min, max, curr, gran;
    SUIT_callbackFunctionPtr funct;

    min = SUIT_getDouble (o, MINIMUM_VALUE);
    max = SUIT_getDouble (o, MAXIMUM_VALUE);
    curr = SUIT_getDouble (o, CURRENT_VALUE);
    gran = SUIT_getDouble (o, GRANULARITY);
    e.worldLocation.x = (e.worldLocation.x-0.5)*2.0;
    e.worldLocation.y = e.worldLocation.y*(1.0+ARROW_BUTTON_HEIGHT) - ARROW_BUTTON_HEIGHT;
    if (e.worldLocation.y > 0.0) {
	if (e.worldLocation.x == 0.0)
	    curr = (max+min)/2;
	else if (e.worldLocation.y/e.worldLocation.x < 0)
	    curr = max - (PI-atan(-e.worldLocation.y/e.worldLocation.x))/PI*(max-min);
	else
	    curr = max - atan(e.worldLocation.y/e.worldLocation.x)/PI*(max-min);
    } else if (e.worldLocation.x < 0.0) {
	curr -= gran;
	if (curr < min)
	    curr = min;
    } else {
	curr += gran;
	if (curr > max)
	    curr = max;
    }
    SUIT_setDouble (o, CURRENT_VALUE, curr);
    funct = (SUIT_callbackFunctionPtr) SUIT_getFunctionPointer (o, CALLBACK_FUNCTION);
    if (funct != NULL)
	funct (o);
}



/*------  SCROLL BAR display style for Bounded Integer  ------*/

PRIVATE void ConstrainEmployees (SUIT_object o)
{
    SUIT_object arrow1, elevator, arrow2;
    SUIT_viewport vp;
    int width, height;
    boolean horiz;

    vp = OBJECT_VIEWPORT(o);
    width = vp.top_right.x - vp.bottom_left.x;
    height = vp.top_right.y - vp.bottom_left.y;
    horiz = (width > height);

    arrow1 = SUIT_getEmployee (o, "scroll bar", 0);
    elevator = SUIT_getEmployee (o, "scroll bar", 1);
    arrow2 = SUIT_getEmployee (o, "scroll bar", 2);
    if (horiz) {
	SUIT_setEnumString (arrow1, DIRECTION, "right");
	SUIT_setEnumString (arrow2, DIRECTION, "left");
	if (width <= 2*height) {
	    SUIT_setViewport (arrow2, VIEWPORT, SRGP_defRectangle (0, 0, width/3, height));
	    SUIT_setViewport (elevator, VIEWPORT, SRGP_defRectangle (width/3, 0, 2*width/3, height));
	    SUIT_setViewport (arrow1, VIEWPORT, SRGP_defRectangle (2*width/3, 0, width, height));
	} else {
	    SUIT_setViewport (arrow2, VIEWPORT, SRGP_defRectangle (0, 0, height, height));
	    SUIT_setViewport (elevator, VIEWPORT, SRGP_defRectangle (height, 0, width-height, height));
	    SUIT_setViewport (arrow1, VIEWPORT, SRGP_defRectangle (width-height, 0, width, height));
	}
    } else {
	SUIT_setEnumString (arrow1, DIRECTION, "up");
	SUIT_setEnumString (arrow2, DIRECTION, "down");
	if (height <= 2*width)	{
	    SUIT_setViewport (arrow2, VIEWPORT, SRGP_defRectangle (0, 0, width, height/3));
	    SUIT_setViewport (elevator, VIEWPORT, SRGP_defRectangle (0, height/3, width, 2*height/3));
	    SUIT_setViewport (arrow1, VIEWPORT, SRGP_defRectangle (0, 2*height/3, width, height));
	} else {
	    SUIT_setViewport (arrow2, VIEWPORT, SRGP_defRectangle (0, 0, width, width));
	    SUIT_setViewport (elevator, VIEWPORT, SRGP_defRectangle (0, width, width, height-width));
	    SUIT_setViewport (arrow1, VIEWPORT, SRGP_defRectangle (0, height-width, width, height));
	}
    }
}



PRIVATE void ConstrainEmployeesCB (SUIT_object o, char *propName, char *propType, Pointer new, Pointer old)
{
    if (SUIT_stringsMatch (propName, VIEWPORT))
	ConstrainEmployees (o);
}



void ArrowCallBack (SUIT_object arrow)
{
    SUIT_object scrollbar = SUIT_getParent(arrow);
    double curr = SUIT_getDouble (scrollbar, CURRENT_VALUE);
    double min = SUIT_getDouble (scrollbar, MINIMUM_VALUE);
    double max = SUIT_getDouble (scrollbar, MAXIMUM_VALUE);
    double gran = SUIT_getDouble (scrollbar, GRANULARITY);
    void (*funct)(SUIT_object);

    if (SUIT_stringsMatch(SUIT_getEnumString(arrow,DIRECTION), "up") ||
        SUIT_stringsMatch(SUIT_getEnumString(arrow,DIRECTION), "left")) {
        if (curr-gran >= min)
            curr -= gran;
    } else {
        if (curr+gran <= max)
            curr += gran;
    }

    SUIT_suspendMarkingRedisplay (scrollbar);
    SUIT_setDouble (scrollbar, CURRENT_VALUE, curr);
    SUIT_resumeMarkingRedisplay (scrollbar);

    funct = (SUIT_callbackFunctionPtr) SUIT_getFunctionPointer (scrollbar, CALLBACK_FUNCTION);
    if (funct != NULL)
        funct (scrollbar);
}



void ElevatorCallBack (SUIT_object elevator)
{
    SUIT_object scrollbar = SUIT_getParent(elevator);
    double min = SUIT_getDouble (scrollbar, MINIMUM_VALUE);
    double max = SUIT_getDouble (scrollbar, MAXIMUM_VALUE);
    double percent = 1.0-SUIT_getDouble (elevator, CURRENT_VALUE);
    double curr = min + (1.0-percent)*(max-min);
    SUIT_callbackFunctionPtr funct;

    SUIT_suspendMarkingRedisplay (scrollbar);
    SUIT_setDouble (scrollbar, CURRENT_VALUE, curr);
    SUIT_resumeMarkingRedisplay (scrollbar);

    funct = (SUIT_callbackFunctionPtr) SUIT_getFunctionPointer (scrollbar, CALLBACK_FUNCTION);
    if (funct != NULL)
        funct (scrollbar);
}



/*------  PIE SLICE display style for Bounded Integer  ------*/

void HitPie (SUIT_object o, SUIT_event e)
{
    double min, max, curr, angle, start;
    SUIT_callbackFunctionPtr funct;
    
    max = SUIT_getDouble (o,MAXIMUM_VALUE);
    min = SUIT_getDouble (o,MINIMUM_VALUE);
    curr= SUIT_getDouble (o,CURRENT_VALUE);
    start=SUIT_getDouble (o,START_ANGLE);
    
    e.worldLocation.x = (e.worldLocation.x - 0.5)*2.0;
    e.worldLocation.y = (e.worldLocation.y - 0.5)*2.0;
    angle=atan2(e.worldLocation.y,e.worldLocation.x);
    
    if (e.worldLocation.y<0) angle+=(2.0*PI);
    if (SUIT_getBoolean(o,INCREASE_CLOCKWISE)) 
	angle=2.0*PI-angle+start*PI/180.0;
    else 
	angle=2.0*PI-(start)*PI/180.0+angle;
    if (angle>2.0*PI) angle-=2.0*PI;
    if (angle<0.0) angle+=2.0*PI;
    
    
    curr=angle/2.0/PI;
    
    curr = min + ( curr * (max-min) );
    
    SUIT_setDouble(o,CURRENT_VALUE,curr);
    funct = (SUIT_callbackFunctionPtr) SUIT_getFunctionPointer(o,CALLBACK_FUNCTION);    
    if (funct != NULL)
	funct(o);
}



void PaintPie (SUIT_object o)
{
    double min, max, curr, start_angle, end_angle;
    
    max = SUIT_getDouble (o,MAXIMUM_VALUE);
    min = SUIT_getDouble (o,MINIMUM_VALUE);
    curr= SUIT_getDouble (o,CURRENT_VALUE);
    
    start_angle = SUIT_getDouble(o,START_ANGLE);
    
    GP_setWindow (GP_defRectangle(-1.0, -1.0, 1.0, 1.0));
    
    end_angle = 360.0 * PERCENTAGE(curr, min, max);
    if (SUIT_getBoolean(o,INCREASE_CLOCKWISE)) {
	end_angle=360.0-end_angle+start_angle;
	if ( PERCENTAGE(curr, min, max) >= 1.0 )
	    start_angle+=360;
	GP_fillEllipseArc(GP_defRectangle(-1.0,-1.0,1.0,1.0),end_angle,start_angle);
    }
    else {
	end_angle+=start_angle;
	GP_fillEllipseArc(GP_defRectangle(-1.0,-1.0,1.0,1.0),start_angle,end_angle);
    }
}



SUIT_object SUIT_createBoundedValue (char *name, void (*callback)(SUIT_object))
{
    SUIT_object o, upArrow, downArrow, elevator;
    static boolean firstTime = TRUE;

    o = SUIT_createObject (name, "bounded value");
    
    /*------------  Scrollbar displays ------------*/
    SUIT_addDisplayToObject(o, "scroll bar", SUIT_passEventDown, SUIT_paintEmployees);
    
    upArrow = SUIT_createArrowButton (SUIT_relativeName(o, "first arrow"), ArrowCallBack);
    SUIT_setBoolean (upArrow, HAS_BORDER, FALSE);
    SUIT_addEmployeeToDisplay(o, "scroll bar", upArrow);
    
    elevator = SUIT_createElevator (SUIT_relativeName(o, "elevator"), ElevatorCallBack);
    SUIT_setBoolean (elevator, HAS_BORDER, FALSE);
    SUIT_addEmployeeToDisplay(o, "scroll bar", elevator);
    
    downArrow = SUIT_createArrowButton (SUIT_relativeName(o, "second arrow"), ArrowCallBack);
    SUIT_setBoolean (downArrow, HAS_BORDER, FALSE);
    SUIT_addEmployeeToDisplay(o, "scroll bar", downArrow);
    
    SUIT_setEnumString (upArrow, DIRECTION, "up");
    SUIT_setEnumString (downArrow, DIRECTION, "down");
    SUIT_setDouble (o, PERCENT_FULL, 0.2);
    
    /*------------ Themometer displays ------------*/
    SUIT_addDisplayToObject (o, "vertical thermometer", HitThermometer, PaintThermometer);
    SUIT_addDisplayToObject (o, "horizontal thermometer", HitThermometer, PaintThermometer);
    
    /*------------- Slice o' pie display ----------------*/
    SUIT_addDisplayToObject (o, "pie slice", HitPie, PaintPie);
    
    /* this will happen in the slice of pie display */ 
    if (firstTime) {
	SUIT_deluxeSetDouble (o, START_ANGLE, 0.0, CLASS);
	SUIT_deluxeSetBoolean (o, INCREASE_CLOCKWISE, TRUE,  CLASS);
	SUIT_deluxeSetDouble (o, ARROWHEAD_LENGTH, 0.2, CLASS);
	SUIT_deluxeSetInteger (o, ARROWHEAD_ANGLE, 10, CLASS);
    }
    
    /*------------ Analog display ------------*/
    SUIT_addDisplayToObject (o, "speedometer", HitAnalogDisplay, PaintAnalogDisplay);
    if (firstTime) {
	SUIT_deluxeSetColor (o, BUTTON_BACKGROUND_COLOR, SUIT_getColor (o, FOREGROUND_COLOR), CLASS);
	SUIT_deluxeSetColor (o, BUTTON_FOREGROUND_COLOR, SUIT_getColor (o, BACKGROUND_COLOR), CLASS);
	SUIT_deluxeSetColor (o, NEEDLE_COLOR, GP_defColor("black", BLACK_ON_MONO), CLASS);
	SUIT_deluxeSetBoolean (o, HAS_ARROW, TRUE, CLASS);
	SUIT_deluxeSetBoolean (o, HAS_TICK_MARKS, TRUE, CLASS);
	firstTime = FALSE;
    }
    
    /* ==== stuff for all displays ==== */
    SUIT_registerInterest (o, ConstrainEmployeesCB);
    SUIT_registerInterest (o, ConstrainValuesCB);
    SUIT_registerInterest (o, OptimizePaint);
    /* ConstrainEmployees (o); */
    
    SUIT_setDouble (o, MAXIMUM_VALUE, 1.0);
    SUIT_setDouble (o, MINIMUM_VALUE, 0.0);
    SUIT_setDouble (o, CURRENT_VALUE, 0.5);
    SUIT_setDouble (o, GRANULARITY, 0.1);
    
    SUIT_deluxeSetFunctionPointer (o, CALLBACK_FUNCTION, (SUIT_functionPointer) callback, OBJECT);
    
    ConstrainEmployees(o);
    return o;
}


