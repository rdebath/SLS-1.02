"======================================================================
|
|   Message Method Definitions
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
| sbyrne     25 Apr 89	  created.
|
"

Object subclass: #Message
       instanceVariableNames: 'selector args'
       classVariableNames: ''
       poolDictionaries: ''
       category: nil.

Message comment: 
'I am a virtually existent class.  By that I mean that logically instances of 
my class are created whenever a message is sent to an object, but in reality
my instances are only created to hold a message that has failed, so that
error reporting methods can examine the sender and arguments.' !
	   

!Message methodsFor: 'basic'!

reinvokeFor: aReceiver
    ^aReceiver perform: selector withArguments: args
!!
