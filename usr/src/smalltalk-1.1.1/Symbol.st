"======================================================================
|
|   Symbol Method Definitions
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
| sbyrne     19 Sep 89	  Changed to use real method categories.
|
| sbyrne     25 Apr 89	  created.
|
"

String variableByteSubclass: #Symbol
       instanceVariableNames: ''
       classVariableNames: ''
       poolDictionaries: ''
       category: nil.

Symbol comment: 
'My instances are unique throughout the Smalltalk system.  My instances 
behave for the most part like strings, except that they print differently,
and I guarantee that any two instances that have the same printed 
representation are in fact the same instance.' !

!Symbol class methodsFor: 'instance creation'!

internCharacter: aCharacter
    | s |
    s _ String new: 1.
    s at: 1 put: aCharacter.
    ^self intern: s
!!



!Symbol methodsFor: 'converting'!

asSymbol
    ^self
!!



!Symbol methodsFor: 'misc'!

species
    ^String
!!



!Symbol methodsFor: 'storing'!

storeOn: aStream
    ('#' , self) printOn: aStream
!!

