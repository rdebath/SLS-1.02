"======================================================================
|
|   Boolean Method Definitions
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
| sbyrne     11 Feb 90	  Converted to printOn: representation.
|
| sbyrne     25 Apr 89	  created.
|
"

Object subclass: #Boolean
       instanceVariableNames: ''
       classVariableNames: ''
       poolDictionaries: ''
       category: nil.

Boolean comment:
'I have two instances in the Smalltalk system: true and false.  I provide
methods that are conditional on boolean values, such as conditional
execution and loops, and conditional testing, such as conditional and and
conditional or.  I should say that I appear to provide those operations; 
my subclasses True and False actually provide those operations.' !

!Boolean methodsFor: 'storing'!

storeOn: aStream
    self printOn: aStream		"representation is the same"

!!

