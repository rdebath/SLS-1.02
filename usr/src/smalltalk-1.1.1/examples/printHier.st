"Print out the class inheritance hierarchy."

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
| GNU Smalltalk; see the file LICENSE.  If not, write to the Free Software
| Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  
|
 ======================================================================"



!Behavior methodsFor: 'demo'!

printHierarchy
    "I my entire subclassclass hierarchy on the terminal."
    self printSubclasses: 0    
!!

!Behavior methodsFor: 'private'!

printSubclasses: level
    "I print my name, and then all my subclasses, each indented according
     to its position in the hierarchy."
    | mySubclasses |
    self indentToLevel: level.
    self name printNl.
    mySubclasses _ self subclasses asSortedCollection:
    	    	        [ :a :b | (a name isNil or: [ b name isNil ])
    	    	    	    	      ifTrue: [ true ]
		    	    	      ifFalse: [ a name <= b name ] ].
    mySubclasses do:
    	[ :subclass | subclass class ~~ Metaclass
	    	    	ifTrue: [ subclass printSubclasses: level + 1 ] ]
!

indentToLevel: level
    level timesRepeat:
    	[ stdout next: (self hierarchyIndent) put: Character space ]
!

hierarchyIndent
    ^4
!!

Object printHierarchy!
