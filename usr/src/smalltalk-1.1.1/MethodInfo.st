"======================================================================
|
|   MethodInfo Method Definitions
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
| sbyrne      3 Sep 89	  created.
|
"

Object subclass: #MethodInfo
       instanceVariableNames: 'sourceCode category'
       classVariableNames: ''
       poolDictionaries: ''
       category: nil.

MethodInfo comment: 
'I provide information about particular methods.  I can produce the category
that a method was filed under, and can be used to access the source
code of the method.' !

!MethodInfo methodsFor: 'basic'!

category
    ^category
!

category: aCategory
    category _ aCategory
!

sourceCode
    ^sourceCode
!

sourceString
    ^sourceCode asString
!

sourceFile
    ^sourceCode fileName
!

sourcePos
    ^sourceCode filePos
!!
