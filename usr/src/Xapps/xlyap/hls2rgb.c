/* From colorwheel.c which is part of color3 by Hiram Clawson (hiramc@sco.com)*/

#include	<X11/Xlib.h>
#include	<X11/Xutil.h>

#ifdef _NO_PROTO
static double hue_value();
#else
static double hue_value( double, double, double );
#endif

/***********************************************************************
 * NAME: hls2rgb() from foley and van dam, fundamentals of interactive ...
 *		page 619
 *
 * PURPOSE: Convert hls[0..3600][0..1000][0..1000] space to rgb space
 *	That is the Hue, Lightness, Saturation color model.
 *	Which is two cones, base to base, the bottom tip is black, the
 *	top tip is white, the middle (bases) around the outside is a color
 *	wheel.  The axis of this solid is Lightness.  Hue is the angular
 *	measure around the cones, and the saturation is the radius from the
 *	axis towards the surface of the cones.
 ***********************************************************************/
void hls2rgb( hue_light_sat, rgb )
int hue_light_sat[3];
int rgb[3];		/*	Each in range [0..65535]	*/
{
	double r, g, b, h, l, s;
	double m1, m2;

	h = (double) hue_light_sat[0] / 10.0;
	l = (double) hue_light_sat[1] / 1000.0;
	s = (double) hue_light_sat[2] / 1000.0;

	if ( l < 0.5 )
	{
		m2 = l * ( 1.0 + s );
	}
	else
	{
		m2 = l + s - (l * s);
	}
	m1 = (2.0 * l) - m2;
	if ( (s + 1.0) == 1.0 )
	{
		if ( (h + 1.0) < 1.0 )
		{
			r = g = b = l;
		}
		else
		{
			r = g = b = 0.0;
		}
	}
	else
	{
		r = hue_value( m1, m2, h + 120.0 );
		g = hue_value( m1, m2, h );
		b = hue_value( m1, m2, h - 120.0 );
	}
	rgb[0] = 65535.0 * r;
	rgb[1] = 65535.0 * g;
	rgb[2] = 65535.0 * b;
	if ( rgb[0] > 65535 )
		rgb[0] = 65535;
	else if ( rgb[0] < 0 )
		rgb[0] = 0;

	if ( rgb[1] > 65535 )
		rgb[1] = 65535;
	else if ( rgb[1] < 0 )
		rgb[1] = 0;

	if ( rgb[2] > 65535 )
		rgb[2] = 65535;
	else if ( rgb[2] < 0 )
		rgb[2] = 0;

	return;
}	/* end of void hls2rgb( hue_light_sat, rgb )	*/

static double hue_value( n1, n2, hue )
double n1;
double n2;
double hue;
{
	if ( hue > 360.0 )
		hue -= 360.0;
	if ( hue < 0.0 )
		hue += 360.0;
	if ( hue < 60.0 )
		return  ( n1 + ((n2 - n1)*hue/60.0));
	else if ( hue < 180.0 )
		return ( n2 );
	else if ( hue < 240.0 )
		return ( n1 + ((n2 - n1)*(240.0 - hue)/60.0));
	else
		return( n1 );
}	/* end of double hue_value( n1, n2, hue )	*/
