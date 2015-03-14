/*
 * Header file for bit-mapped device drivers.
 * By Harry C. Pulley, IV.  Started 24NOV92.
 */
  
/* iBCS ioctl() codes */

#define KDMAPDISP 0
#define KDUNMAPDISP 1
#define KDENABIO 2
#define KDDISABIO 3
#define KDADDIO 4
#define KDDELIO 5 

/* ioctl() communication structure (vec argument) */

struct bmform 
{
	union bm_arg
	{
		struct bm_io
		{
			int val, offset;
		} io;
	
		struct bm_map
		{
			int userAddr, videoAddr, videoLen;
		} map;
	} arg;
};
