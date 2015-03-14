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

Object subclass: #XError
       instanceVariableNames: 'sequenceNumber majorOp minorOp'
       classVariableNames: 'ErrorMap' "should this create this variable?"
       poolDictionaries: ''
       category: 'X hacking'
!
    
FileStream fileIn: 'Xerr.st'!


!XError class methodsFor: 'subinstance creation'!

initialize
    | namePrefixes i |
    namePrefixes _ #(Request Value Window Pixmap Atom Cursor Font 
			 Match Drawable Access Alloc Colormap GContext
			 IDChoice Name Length Implementation).
    ErrorMap _ Array new: namePrefixes size.
    i _ 1.
    namePrefixes do: 
	[ :prefix | ErrorMap at: i put: (Smalltalk at: (prefix, 'Error') asSymbol).
		    i _ i + 1 ]
!

newFrom: aStream
    | type class | 
    type _ aStream byte.
    class _ ErrorMap at: type.
    ^class newFrom: aStream
!!

XError initialize!

