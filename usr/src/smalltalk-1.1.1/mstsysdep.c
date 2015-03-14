/***********************************************************************
 *
 *	System specific implementation module.
 *
 *	This module contains implementations of various operating system
 *	specific routines.  This module should encapsulate most of these OS
 *	specific calls so that the rest of the code is portable.
 *
 ***********************************************************************/

/***********************************************************************
 *
 * Copyright (C) 1990, 1991 Free Software Foundation, Inc.
 * Written by Steve Byrne.
 *
 * This file is part of GNU Smalltalk.
 *
 * GNU Smalltalk is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 1, or (at your option) any later 
 * version.
 * 
 * GNU Smalltalk is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 * 
 * You should have received a copy of the GNU General Public License along with
 * GNU Smalltalk; see the file COPYING.  If not, write to the Free Software
 * Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  
 *
 ***********************************************************************/


/*
 *    Change Log
 * ============================================================================
 * Author      Date       Change 
 * sbyrne    17 May 90	  Added enableInterrupts and disableInterrupts.  System
 *			  V.3 code signal support from Doug McCallum (thanks,
 *			  Doug!).
 *
 * sbyrne    16 May 90	  Created.
 *
 */

#include "mst.h"
#include "mstsysdep.h"
#include <signal.h>

#ifdef SYSV_3_SIGNALS
static unsigned long __cursigmask; /* keep track of signal mask status */
#endif

/*
 *	IntState disableInterrupts()
 *
 * Description
 *
 *	Saves and returns the current state of the software interrupt system.
 *	Disables all interrupts.
 *
 * Outputs
 *
 *	The old state of the interrupt system (presumably for saving for a
 *	later call to enableInterrupts).
 */
IntState disableInterrupts()
{
#ifdef BSD_SIGNALS
  return (sigsetmask(-1));
#endif
#ifdef SYSV_3_SIGNALS
  unsigned long oldmask = __cursigmask;
  register int i;

  __cursigmask = -1;
  for (i=1; i <= 32; i++) {
    sighold(i);		/* want it blocked - ok if it already is */
  }
  return oldmask;
#endif
}


/*
 *	void enableInterrupts(mask)
 *
 * Description
 *
 *	Restores the state of the interrupt system to that which it had when
 *	"mask" was created. 
 *
 * Inputs
 *
 *	mask  : An interrupt state...should have been returned at some point
 *		from a call to disableInterrupts.
 *
 */
void enableInterrupts(mask)
IntState mask;
{
#ifdef BSD_SIGNALS
  sigsetmask(mask);
#endif
#ifdef SYSV_3_SIGNALS
  unsigned long oldmask = __cursigmask;
  register int i;

  __cursigmask = mask;
  for (i=1; mask != 0; i++, mask >>= 1) { 
    if (oldmask & (0x1 << (i-1))) {
      sigrelse(i);	/* want it unblocked and it is blocked */
    }
  }
#endif
}





