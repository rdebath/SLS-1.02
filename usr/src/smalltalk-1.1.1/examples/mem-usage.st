"Compute the amount of memory and the number of instances for each
 Class in the GNU Smalltalk system."


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



!Object methodsFor: 'demo'!

showMemUsage
    | t numObjs classObjs totalInstanceSize instVarSize |
    numObjs _ t _ 0.
    Object withAllSubclasses do: 
	[ :class | class printNl.
		   totalInstanceSize _ classObjs _ 0.
		   instVarSize _ class instSize.
    	    	   (class == MethodContext or: [ class == BlockContext ])
		       ifFalse: [ class allInstancesDo:
				      [ :inst | totalInstanceSize _ totalInstanceSize +
						    ((WordMemory at: (Memory addressOf: inst)) * 4).
						classObjs _ classObjs + 1 ] 
				      ].
						    
		   '  Instances: ' print. classObjs printNl.
		   '  Size: ' print. totalInstanceSize printNl.
		   stdout nl.
		   t _ t + totalInstanceSize.
		   numObjs _ numObjs + classObjs ].

    numObjs printNl.
    t printNl
!!

Object showMemUsage!
