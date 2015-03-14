/* fdomain.h -- Header for Future Domain TMC-1660/TMC-1680 driver
 * Created: Sun May  3 18:47:33 1992
 * Revised: Sun Jan 10 00:54:29 1993 by root
 * Author: Rickard E. Faith, faith@cs.unc.edu
 * Copyright 1992, 1993 Rickard E. Faith
 * This program comes with ABSOLUTELY NO WARRANTY.
 *
 * $Log$
 */

#ifndef _FDOMAIN_H
#define _FDOMAIN_H

int  fdomain_16x0_detect( int );
int  fdomain_16x0_command( Scsi_Cmnd * );
int  fdomain_16x0_abort( Scsi_Cmnd *, int );
const char *fdomain_16x0_info( void );
int  fdomain_16x0_reset( void ); 
int  fdomain_16x0_queue( Scsi_Cmnd *, void (*done)(Scsi_Cmnd *) );
int  fdomain_16x0_biosparam( int, int, int * );

#define FDOMAIN_16X0  { "Future Domain TMC-16x0",          \
			 fdomain_16x0_detect,              \
			 fdomain_16x0_info,                \
			 fdomain_16x0_command,             \
			 fdomain_16x0_queue,               \
		         fdomain_16x0_abort,               \
			 fdomain_16x0_reset,               \
			 NULL,                             \
			 fdomain_16x0_biosparam,           \
			 1, 6, 64 /* SG_NONE */, 1 ,0, 0}
#endif
