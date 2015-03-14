"======================================================================
|
|   SystemDictionary Method Definitions
|
 ======================================================================"


"======================================================================
|
| Copyright (C) 1990, 1991 Free Software Foundation, Inc.
| Written by Steve Byrne.
|
| This file is part of GNU Smalltalk.
|
| GNU Smalltalk is free software; you can redistribute it and/or modify it
| under the terms of the GNU General Public License as published by the Free
| Software Foundation; either version 1, or (at your option) any later version.
| 
| GNU Smalltalk is distributed in the hope that it will be useful, but WITHOUT
| ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
| FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
| details.
| 
| You should have received a copy of the GNU General Public License along with
| GNU Smalltalk; see the file COPYING.  If not, write to the Free Software
| Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  
|
 ======================================================================"


"
|     Change Log
| ============================================================================
| Author       Date       Change 
| sbyrne      8 Jul 89	  Created.
|
"

Object variableSubclass: #CFunctionDescriptor
       instanceVariableNames: 'cFunction cFunctionName returnType numFixedArgs'
       classVariableNames: ''
       poolDictionaries: ''
       category: nil !

CFunctionDescriptor comment: 
'I am not part of the Smalltalk definition.  My instances contain information
about C functions that can be called from within Smalltalk, such as number
and type of parameters.  This information is used by the C callout mechanism
to perform the actual call-out to C routines.' !


"A couple of simple, but useful callout functions, as examples."

Behavior defineCFunc: 'system'
         withSelectorArgs: 'system: aString'
	 forClass: SystemDictionary
	 returning: #int
	 args: #(string)!

Behavior defineCFunc: 'getenv'
         withSelectorArgs: 'getenv: aString'
	 forClass: SystemDictionary
	 returning: #string
	 args: #(string)!
