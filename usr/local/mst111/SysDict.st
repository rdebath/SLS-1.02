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
| sbyrne     22 Apr 90	  Fixed Dependencies to be an IdentityDictionary
|			  instead of a regular Dictionary.  This has better
|			  semantics and is faster.
|
| sbyrne      4 Jul 89	  added initBlocks methods.
|
| sbyrne     25 Apr 89	  created.
|
"

Dictionary variableSubclass: #SystemDictionary
	   instanceVariableNames: ''
	   classVariableNames: ''
	   poolDictionaries: ''
	   category: nil.

SystemDictionary comment: 
'I am a special form of dictionary.  Typically I only have one instance,
called "Smalltalk", which is known to the Smalltalk interpreter.  I define
several methods that are "system" related, such as #quitPrimitive.
My instance also helps keep track of dependencies between objects.' !


!SystemDictionary methodsFor: 'basic'!

initialize
    InitBlocks _ OrderedCollection new.
    self at: #Dependencies put: (IdentityDictionary new) 
	    	    	    	    	    	 "### I don't think this is
    	    	    	    	    	    	  the right way to do this"
!

addInit: aBlock
    "Adds 'aBlock' to the array of blocks to be invoked after every start
     of the system."
    InitBlocks add: aBlock
!

doInits
    "Called after the system has loaded the image, this will invoke any
     init blocks that have been installed."
    InitBlocks do: [ :aBlock | aBlock value ]
!

dependenciesAt: anObject
    ^(Smalltalk at: #Dependencies) at: anObject
!!
