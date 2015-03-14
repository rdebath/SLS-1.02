/* $XConsortium: cbrt.c,v 1.0 90/09/29 10:24:02 rws Exp $ */

/* simple cbrt, in case your math library doesn't have a good one */

double pow();

double
cbrt(x)
    double x;
{
    return pow(x, 1.0/3.0);
}
