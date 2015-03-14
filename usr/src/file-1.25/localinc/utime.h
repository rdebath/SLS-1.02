/*
 * Normally you should use your system's utime.h; this is for 
 * crufty old antique compilers/systems that don't provide one.
 * $Id: utime.h,v 1.1 92/09/09 16:21:29 ian Exp $
 */
struct utimbuf {
	long actime;
	long modtime;
};
