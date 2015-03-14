/***********************************************************************
 *
 *	Declarations for the byte code compiler.
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
 * sbyrne     2 Sep 89	  added descriptor support
 *
 * sbyrne     2 Sep 89	  Moved common compiled method structure definition
 *			  here, so that the interpeter could share.
 *
 * sbyrne     1 Jan 89	  Created.
 *
 */

#ifndef __MSTCOMP__
#define __MSTCOMP__

#ifndef __MSTINTERP__
#include "mstinterp.h"
#endif

struct CompiledMethodStruct {
  OBJ_HEADER;
  OOP		descriptor;
  MethodHeader	header;
  OOP		literals[1];	/* actually, literals followed by bytecodes */
};

extern OOP		thisClass;
extern Boolean		declareTracing;
extern Boolean		emacsProcess;

extern void		compileMethod(), executeStatements(),
			installInitialMethods(), setCompilationClass(),
			invokeInitBlocks(), compiledMethodAtPut(),
			setMethodDescriptor(), 
			setCompilationCategory(), copyCompileContext(),
			initDefaultCompilationEnvironment();
extern OOP		compiledMethodAt(), getMethodDescriptor(),
			methodNewOOP();
extern Boolean		validMethodIndex();

#endif /* __MSTCOMP__ */
