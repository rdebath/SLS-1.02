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
| sbyrne     24 May 90	  created.
|
"

Object subclass: #XEvent
       instanceVariableNames: 'sequenceNumber'
       classVariableNames: 'EventMap'
       poolDictionaries: 'XGlobals'
       category: 'X hacking'
!

!XEvent methodsFor: 'accessing'!

sequenceNumber
    ^sequenceNumber
!

mappedId: anId
    ^Registry at: anId
	      ifAbsent: [ nil ]
!!


FileStream fileIn: 'Xevt.st'!

!XEvent class methodsFor: 'subinstance creation'!

initialize
    | namePrefixes i sym |
    namePrefixes _ #(
KeyPress			"2"
KeyRelease			"3"
ButtonPress			"4"
ButtonRelease			"5"
MotionNotify			"6"
EnterNotify			"7"
LeaveNotify			"8"
FocusIn				"9"
FocusOut			"10"
KeymapNotify			"11"
Expose				"12"
GraphicsExposure		"13"
NoExposure			"14"
VisibilityNotify		"15"
CreateNotify			"16"
DestroyNotify			"17"
UnmapNotify			"18"
MapNotify			"19"
MapRequest			"20"
ReparentNotify			"21"
ConfigureNotify			"22"
ConfigureRequest		"23"
GravityNotify			"24"
ResizeRequest			"25"
CirculateNotify			"26"
CirculateRequest		"27"
PropertyNotify			"28"
SelectionClear			"29"
SelectionRequest		"30"
SelectionNotify			"31"
ColormapNotify			"32"
ClientMessage			"33"
MappingNotify			"34"
).
    EventMap _ Array new: namePrefixes size.
    i _ 2.
    namePrefixes do: 
	[ :prefix | sym _ (prefix, 'Event') asSymbol.
		    EventMap at: i put: (Smalltalk at: sym 
						   ifAbsent: [ nil ]).
		    i _ i + 1 ]
!

newFrom: aStream type: type
    | class | 
    class _ EventMap at: type.
    ^class newFrom: aStream
!!

XEvent initialize!
