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

XEvent subclass: #KeyPressEvent
	instanceVariableNames: 'detail time root event child rootX rootY eventX eventY state sameScreen '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!KeyPressEvent class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!KeyPressEvent methodsFor: 'accessing'!

detail
    ^detail
!

sequenceNumber
    ^sequenceNumber
!

time
    ^time
!

root
    ^self mappedId:  root
!

event
    ^self mappedId:  event
!

child
    ^self mappedId:  child
!

rootX
    ^rootX
!

rootY
    ^rootY
!

eventX
    ^eventX
!

eventY
    ^eventY
!

state
    ^state
!

sameScreen
    ^sameScreen
!

!

!KeyPressEvent methodsFor: 'private'!

initFrom: aStream
    detail _ aStream byte. 
    sequenceNumber _ aStream uword. 
    time _ aStream long. 
    root _ aStream long. 
    event _ aStream long. 
    child _ aStream long. 
    rootX _ aStream word. 
    rootY _ aStream word. 
    eventX _ aStream word. 
    eventY _ aStream word. 
    state _ aStream word. 
    sameScreen _ aStream byte. 
    aStream skipBytes: 1
!!


XEvent subclass: #KeyReleaseEvent
	instanceVariableNames: 'detail time root event child rootX rootY eventX eventY state sameScreen '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!KeyReleaseEvent class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!KeyReleaseEvent methodsFor: 'accessing'!

detail
    ^detail
!

sequenceNumber
    ^sequenceNumber
!

time
    ^time
!

root
    ^self mappedId:  root
!

event
    ^self mappedId:  event
!

child
    ^self mappedId:  child
!

rootX
    ^rootX
!

rootY
    ^rootY
!

eventX
    ^eventX
!

eventY
    ^eventY
!

state
    ^state
!

sameScreen
    ^sameScreen
!

!

!KeyReleaseEvent methodsFor: 'private'!

initFrom: aStream
    detail _ aStream byte. 
    sequenceNumber _ aStream uword. 
    time _ aStream long. 
    root _ aStream long. 
    event _ aStream long. 
    child _ aStream long. 
    rootX _ aStream word. 
    rootY _ aStream word. 
    eventX _ aStream word. 
    eventY _ aStream word. 
    state _ aStream word. 
    sameScreen _ aStream byte. 
    aStream skipBytes: 1
!!


XEvent subclass: #ButtonPressEvent
	instanceVariableNames: 'detail time root event child rootX rootY eventX eventY state sameScreen '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!ButtonPressEvent class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!ButtonPressEvent methodsFor: 'accessing'!

detail
    ^detail
!

sequenceNumber
    ^sequenceNumber
!

time
    ^time
!

root
    ^self mappedId:  root
!

event
    ^self mappedId:  event
!

child
    ^self mappedId:  child
!

rootX
    ^rootX
!

rootY
    ^rootY
!

eventX
    ^eventX
!

eventY
    ^eventY
!

state
    ^state
!

sameScreen
    ^sameScreen
!

!

!ButtonPressEvent methodsFor: 'private'!

initFrom: aStream
    detail _ aStream byte. 
    sequenceNumber _ aStream uword. 
    time _ aStream long. 
    root _ aStream long. 
    event _ aStream long. 
    child _ aStream long. 
    rootX _ aStream word. 
    rootY _ aStream word. 
    eventX _ aStream word. 
    eventY _ aStream word. 
    state _ aStream word. 
    sameScreen _ aStream byte. 
    aStream skipBytes: 1
!!


XEvent subclass: #ButtonReleaseEvent
	instanceVariableNames: 'detail time root event child rootX rootY eventX eventY state sameScreen '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!ButtonReleaseEvent class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!ButtonReleaseEvent methodsFor: 'accessing'!

detail
    ^detail
!

sequenceNumber
    ^sequenceNumber
!

time
    ^time
!

root
    ^self mappedId:  root
!

event
    ^self mappedId:  event
!

child
    ^self mappedId:  child
!

rootX
    ^rootX
!

rootY
    ^rootY
!

eventX
    ^eventX
!

eventY
    ^eventY
!

state
    ^state
!

sameScreen
    ^sameScreen
!

!

!ButtonReleaseEvent methodsFor: 'private'!

initFrom: aStream
    detail _ aStream byte. 
    sequenceNumber _ aStream uword. 
    time _ aStream long. 
    root _ aStream long. 
    event _ aStream long. 
    child _ aStream long. 
    rootX _ aStream word. 
    rootY _ aStream word. 
    eventX _ aStream word. 
    eventY _ aStream word. 
    state _ aStream word. 
    sameScreen _ aStream byte. 
    aStream skipBytes: 1
!!


XEvent subclass: #MotionNotifyEvent
	instanceVariableNames: 'detail time root event child rootX rootY eventX eventY state sameScreen '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!MotionNotifyEvent class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!MotionNotifyEvent methodsFor: 'accessing'!

detail
    ^detail
!

sequenceNumber
    ^sequenceNumber
!

time
    ^time
!

root
    ^self mappedId:  root
!

event
    ^self mappedId:  event
!

child
    ^self mappedId:  child
!

rootX
    ^rootX
!

rootY
    ^rootY
!

eventX
    ^eventX
!

eventY
    ^eventY
!

state
    ^state
!

sameScreen
    ^sameScreen
!

!

!MotionNotifyEvent methodsFor: 'private'!

initFrom: aStream
    detail _ aStream ubyte. 
    sequenceNumber _ aStream uword. 
    time _ aStream long. 
    root _ aStream long. 
    event _ aStream long. 
    child _ aStream long. 
    rootX _ aStream word. 
    rootY _ aStream word. 
    eventX _ aStream word. 
    eventY _ aStream word. 
    state _ aStream word. 
    sameScreen _ aStream byte. 
    aStream skipBytes: 1
!!


XEvent subclass: #EnterNotifyEvent
	instanceVariableNames: 'detail time root event child rootX rootY eventX eventY state mode focus '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!EnterNotifyEvent class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!EnterNotifyEvent methodsFor: 'accessing'!

detail
    ^detail
!

sequenceNumber
    ^sequenceNumber
!

time
    ^time
!

root
    ^self mappedId:  root
!

event
    ^self mappedId:  event
!

child
    ^self mappedId:  child
!

rootX
    ^rootX
!

rootY
    ^rootY
!

eventX
    ^eventX
!

eventY
    ^eventY
!

state
    ^state
!

mode
    ^mode
!

focus
    ^focus
!

!

!EnterNotifyEvent methodsFor: 'private'!

initFrom: aStream
    detail _ aStream ubyte. 
    sequenceNumber _ aStream uword. 
    time _ aStream long. 
    root _ aStream long. 
    event _ aStream long. 
    child _ aStream long. 
    rootX _ aStream word. 
    rootY _ aStream word. 
    eventX _ aStream word. 
    eventY _ aStream word. 
    state _ aStream word. 
    mode _ aStream ubyte. 
    focus _ aStream ubyte. 
    aStream skipBytes: 0
!!


XEvent subclass: #LeaveNotifyEvent
	instanceVariableNames: 'detail time root event child rootX rootY eventX eventY state mode focus '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!LeaveNotifyEvent class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!LeaveNotifyEvent methodsFor: 'accessing'!

detail
    ^detail
!

sequenceNumber
    ^sequenceNumber
!

time
    ^time
!

root
    ^self mappedId:  root
!

event
    ^self mappedId:  event
!

child
    ^self mappedId:  child
!

rootX
    ^rootX
!

rootY
    ^rootY
!

eventX
    ^eventX
!

eventY
    ^eventY
!

state
    ^state
!

mode
    ^mode
!

focus
    ^focus
!

!

!LeaveNotifyEvent methodsFor: 'private'!

initFrom: aStream
    detail _ aStream ubyte. 
    sequenceNumber _ aStream uword. 
    time _ aStream long. 
    root _ aStream long. 
    event _ aStream long. 
    child _ aStream long. 
    rootX _ aStream word. 
    rootY _ aStream word. 
    eventX _ aStream word. 
    eventY _ aStream word. 
    state _ aStream word. 
    mode _ aStream ubyte. 
    focus _ aStream ubyte. 
    aStream skipBytes: 0
!!


XEvent subclass: #FocusInEvent
	instanceVariableNames: 'detail event mode '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!FocusInEvent class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!FocusInEvent methodsFor: 'accessing'!

detail
    ^detail
!

sequenceNumber
    ^sequenceNumber
!

event
    ^self mappedId:  event
!

mode
    ^mode
!

!

!FocusInEvent methodsFor: 'private'!

initFrom: aStream
    detail _ aStream ubyte. 
    sequenceNumber _ aStream uword. 
    event _ aStream long. 
    mode _ aStream ubyte. 
    aStream skipBytes: 23
!!


XEvent subclass: #FocusOutEvent
	instanceVariableNames: 'detail event mode '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!FocusOutEvent class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!FocusOutEvent methodsFor: 'accessing'!

detail
    ^detail
!

sequenceNumber
    ^sequenceNumber
!

event
    ^self mappedId:  event
!

mode
    ^mode
!

!

!FocusOutEvent methodsFor: 'private'!

initFrom: aStream
    detail _ aStream ubyte. 
    sequenceNumber _ aStream uword. 
    event _ aStream long. 
    mode _ aStream ubyte. 
    aStream skipBytes: 23
!!


XEvent subclass: #KeymapNotifyEvent
	instanceVariableNames: 'keys '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!KeymapNotifyEvent class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!KeymapNotifyEvent methodsFor: 'accessing'!

keys
    ^keys
!

!

!KeymapNotifyEvent methodsFor: 'private'!

initFrom: aStream
    keys _ aStream getUnpaddedString: 31. 
    aStream skipBytes: 0
!!


XEvent subclass: #ExposeEvent
	instanceVariableNames: 'window x y width height count '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!ExposeEvent class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!ExposeEvent methodsFor: 'accessing'!

sequenceNumber
    ^sequenceNumber
!

window
    ^self mappedId:  window
!

x
    ^x
!

y
    ^y
!

width
    ^width
!

height
    ^height
!

count
    ^count
!

!

!ExposeEvent methodsFor: 'private'!

initFrom: aStream
    aStream ubyte. 
    sequenceNumber _ aStream uword. 
    window _ aStream long. 
    x _ aStream uword. 
    y _ aStream uword. 
    width _ aStream uword. 
    height _ aStream uword. 
    count _ aStream uword. 
    aStream skipBytes: 14
!!


XEvent subclass: #GraphicsExposureEvent
	instanceVariableNames: 'drawable x y width height minorOp count majorOp '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!GraphicsExposureEvent class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!GraphicsExposureEvent methodsFor: 'accessing'!

sequenceNumber
    ^sequenceNumber
!

drawable
    ^self mappedId:  drawable
!

x
    ^x
!

y
    ^y
!

width
    ^width
!

height
    ^height
!

minorOp
    ^minorOp
!

count
    ^count
!

majorOp
    ^majorOp
!

!

!GraphicsExposureEvent methodsFor: 'private'!

initFrom: aStream
    aStream ubyte. 
    sequenceNumber _ aStream uword. 
    drawable _ aStream long. 
    x _ aStream uword. 
    y _ aStream uword. 
    width _ aStream uword. 
    height _ aStream uword. 
    minorOp _ aStream uword. 
    count _ aStream uword. 
    majorOp _ aStream ubyte. 
    aStream skipBytes: 11
!!


XEvent subclass: #NoExposureEvent
	instanceVariableNames: 'drawable minorOp majorOp '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!NoExposureEvent class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!NoExposureEvent methodsFor: 'accessing'!

sequenceNumber
    ^sequenceNumber
!

drawable
    ^self mappedId:  drawable
!

minorOp
    ^minorOp
!

majorOp
    ^majorOp
!

!

!NoExposureEvent methodsFor: 'private'!

initFrom: aStream
    aStream ubyte. 
    sequenceNumber _ aStream uword. 
    drawable _ aStream long. 
    minorOp _ aStream uword. 
    majorOp _ aStream ubyte. 
    aStream skipBytes: 21
!!


XEvent subclass: #VisibilityNotifyEvent
	instanceVariableNames: 'window state '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!VisibilityNotifyEvent class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!VisibilityNotifyEvent methodsFor: 'accessing'!

sequenceNumber
    ^sequenceNumber
!

window
    ^self mappedId:  window
!

state
    ^state
!

!

!VisibilityNotifyEvent methodsFor: 'private'!

initFrom: aStream
    aStream ubyte. 
    sequenceNumber _ aStream uword. 
    window _ aStream long. 
    state _ aStream ubyte. 
    aStream skipBytes: 23
!!


XEvent subclass: #CreateNotifyEvent
	instanceVariableNames: 'parent window x y width height borderWidth overrideRedirect '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!CreateNotifyEvent class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!CreateNotifyEvent methodsFor: 'accessing'!

sequenceNumber
    ^sequenceNumber
!

parent
    ^self mappedId:  parent
!

window
    ^self mappedId:  window
!

x
    ^x
!

y
    ^y
!

width
    ^width
!

height
    ^height
!

borderWidth
    ^borderWidth
!

overrideRedirect
    ^overrideRedirect
!

!

!CreateNotifyEvent methodsFor: 'private'!

initFrom: aStream
    aStream ubyte. 
    sequenceNumber _ aStream uword. 
    parent _ aStream long. 
    window _ aStream long. 
    x _ aStream word. 
    y _ aStream word. 
    width _ aStream uword. 
    height _ aStream uword. 
    borderWidth _ aStream uword. 
    overrideRedirect _ aStream byte. 
    aStream skipBytes: 9
!!


XEvent subclass: #DestroyNotifyEvent
	instanceVariableNames: 'event window '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!DestroyNotifyEvent class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!DestroyNotifyEvent methodsFor: 'accessing'!

sequenceNumber
    ^sequenceNumber
!

event
    ^self mappedId:  event
!

window
    ^self mappedId:  window
!

!

!DestroyNotifyEvent methodsFor: 'private'!

initFrom: aStream
    aStream ubyte. 
    sequenceNumber _ aStream uword. 
    event _ aStream long. 
    window _ aStream long. 
    aStream skipBytes: 20
!!


XEvent subclass: #UnmapNotifyEvent
	instanceVariableNames: 'event window fromConfigure '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!UnmapNotifyEvent class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!UnmapNotifyEvent methodsFor: 'accessing'!

sequenceNumber
    ^sequenceNumber
!

event
    ^self mappedId:  event
!

window
    ^self mappedId:  window
!

fromConfigure
    ^fromConfigure
!

!

!UnmapNotifyEvent methodsFor: 'private'!

initFrom: aStream
    aStream ubyte. 
    sequenceNumber _ aStream uword. 
    event _ aStream long. 
    window _ aStream long. 
    fromConfigure _ aStream byte. 
    aStream skipBytes: 19
!!


XEvent subclass: #MapNotifyEvent
	instanceVariableNames: 'event window overrideRedirect '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!MapNotifyEvent class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!MapNotifyEvent methodsFor: 'accessing'!

sequenceNumber
    ^sequenceNumber
!

event
    ^self mappedId:  event
!

window
    ^self mappedId:  window
!

overrideRedirect
    ^overrideRedirect
!

!

!MapNotifyEvent methodsFor: 'private'!

initFrom: aStream
    aStream ubyte. 
    sequenceNumber _ aStream uword. 
    event _ aStream long. 
    window _ aStream long. 
    overrideRedirect _ aStream byte. 
    aStream skipBytes: 19
!!


XEvent subclass: #MapRequestEvent
	instanceVariableNames: 'parent window '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!MapRequestEvent class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!MapRequestEvent methodsFor: 'accessing'!

sequenceNumber
    ^sequenceNumber
!

parent
    ^self mappedId:  parent
!

window
    ^self mappedId:  window
!

!

!MapRequestEvent methodsFor: 'private'!

initFrom: aStream
    aStream ubyte. 
    sequenceNumber _ aStream uword. 
    parent _ aStream long. 
    window _ aStream long. 
    aStream skipBytes: 20
!!


XEvent subclass: #ReparentNotifyEvent
	instanceVariableNames: 'event window parent x y overrideRedirect '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!ReparentNotifyEvent class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!ReparentNotifyEvent methodsFor: 'accessing'!

sequenceNumber
    ^sequenceNumber
!

event
    ^self mappedId:  event
!

window
    ^self mappedId:  window
!

parent
    ^self mappedId:  parent
!

x
    ^x
!

y
    ^y
!

overrideRedirect
    ^overrideRedirect
!

!

!ReparentNotifyEvent methodsFor: 'private'!

initFrom: aStream
    aStream ubyte. 
    sequenceNumber _ aStream uword. 
    event _ aStream long. 
    window _ aStream long. 
    parent _ aStream long. 
    x _ aStream word. 
    y _ aStream word. 
    overrideRedirect _ aStream byte. 
    aStream skipBytes: 11
!!


XEvent subclass: #ConfigureNotifyEvent
	instanceVariableNames: 'event window aboveSibling x y width height borderWidth overrideRedirect '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!ConfigureNotifyEvent class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!ConfigureNotifyEvent methodsFor: 'accessing'!

sequenceNumber
    ^sequenceNumber
!

event
    ^self mappedId:  event
!

window
    ^self mappedId:  window
!

aboveSibling
    ^self mappedId:  aboveSibling
!

x
    ^x
!

y
    ^y
!

width
    ^width
!

height
    ^height
!

borderWidth
    ^borderWidth
!

overrideRedirect
    ^overrideRedirect
!

!

!ConfigureNotifyEvent methodsFor: 'private'!

initFrom: aStream
    aStream ubyte. 
    sequenceNumber _ aStream uword. 
    event _ aStream long. 
    window _ aStream long. 
    aboveSibling _ aStream long. 
    x _ aStream word. 
    y _ aStream word. 
    width _ aStream uword. 
    height _ aStream uword. 
    borderWidth _ aStream uword. 
    overrideRedirect _ aStream byte. 
    aStream skipBytes: 5
!!


XEvent subclass: #ConfigureRequestEvent
	instanceVariableNames: 'stackMode parent window sibling x y width height borderWidth valueMask '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!ConfigureRequestEvent class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!ConfigureRequestEvent methodsFor: 'accessing'!

stackMode
    ^stackMode
!

sequenceNumber
    ^sequenceNumber
!

parent
    ^self mappedId:  parent
!

window
    ^self mappedId:  window
!

sibling
    ^self mappedId:  sibling
!

x
    ^x
!

y
    ^y
!

width
    ^width
!

height
    ^height
!

borderWidth
    ^borderWidth
!

valueMask
    ^valueMask
!

!

!ConfigureRequestEvent methodsFor: 'private'!

initFrom: aStream
    stackMode _ aStream ubyte. 
    sequenceNumber _ aStream uword. 
    parent _ aStream long. 
    window _ aStream long. 
    sibling _ aStream long. 
    x _ aStream word. 
    y _ aStream word. 
    width _ aStream uword. 
    height _ aStream uword. 
    borderWidth _ aStream uword. 
    valueMask _ aStream uword. 
    aStream skipBytes: 4
!!


XEvent subclass: #GravityNotifyEvent
	instanceVariableNames: 'event window x y '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!GravityNotifyEvent class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!GravityNotifyEvent methodsFor: 'accessing'!

sequenceNumber
    ^sequenceNumber
!

event
    ^self mappedId:  event
!

window
    ^self mappedId:  window
!

x
    ^x
!

y
    ^y
!

!

!GravityNotifyEvent methodsFor: 'private'!

initFrom: aStream
    aStream ubyte. 
    sequenceNumber _ aStream uword. 
    event _ aStream long. 
    window _ aStream long. 
    x _ aStream word. 
    y _ aStream word. 
    aStream skipBytes: 16
!!


XEvent subclass: #ResizeRequestEvent
	instanceVariableNames: 'window width height '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!ResizeRequestEvent class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!ResizeRequestEvent methodsFor: 'accessing'!

sequenceNumber
    ^sequenceNumber
!

window
    ^self mappedId:  window
!

width
    ^width
!

height
    ^height
!

!

!ResizeRequestEvent methodsFor: 'private'!

initFrom: aStream
    aStream ubyte. 
    sequenceNumber _ aStream uword. 
    window _ aStream long. 
    width _ aStream uword. 
    height _ aStream uword. 
    aStream skipBytes: 20
!!


XEvent subclass: #CirculateNotifyEvent
	instanceVariableNames: 'event window place '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!CirculateNotifyEvent class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!CirculateNotifyEvent methodsFor: 'accessing'!

sequenceNumber
    ^sequenceNumber
!

event
    ^self mappedId:  event
!

window
    ^self mappedId:  window
!

place
    ^place
!

!

!CirculateNotifyEvent methodsFor: 'private'!

initFrom: aStream
    aStream ubyte. 
    sequenceNumber _ aStream uword. 
    event _ aStream long. 
    window _ aStream long. 
    aStream long. 
    place _ aStream ubyte. 
    aStream skipBytes: 15
!!


XEvent subclass: #CirculateRequestEvent
	instanceVariableNames: 'parent window place '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!CirculateRequestEvent class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!CirculateRequestEvent methodsFor: 'accessing'!

sequenceNumber
    ^sequenceNumber
!

parent
    ^self mappedId:  parent
!

window
    ^self mappedId:  window
!

place
    ^place
!

!

!CirculateRequestEvent methodsFor: 'private'!

initFrom: aStream
    aStream ubyte. 
    sequenceNumber _ aStream uword. 
    parent _ aStream long. 
    window _ aStream long. 
    aStream ulong. 
    place _ aStream ubyte. 
    aStream skipBytes: 15
!!


XEvent subclass: #PropertyNotifyEvent
	instanceVariableNames: 'window atom time state '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!PropertyNotifyEvent class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!PropertyNotifyEvent methodsFor: 'accessing'!

sequenceNumber
    ^sequenceNumber
!

window
    ^self mappedId:  window
!

atom
    ^self mappedId:  atom
!

time
    ^time
!

state
    ^state
!

!

!PropertyNotifyEvent methodsFor: 'private'!

initFrom: aStream
    aStream ubyte. 
    sequenceNumber _ aStream uword. 
    window _ aStream long. 
    atom _ aStream long. 
    time _ aStream long. 
    state _ aStream ubyte. 
    aStream skipBytes: 15
!!


XEvent subclass: #SelectionClearEvent
	instanceVariableNames: 'time owner selection '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!SelectionClearEvent class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!SelectionClearEvent methodsFor: 'accessing'!

sequenceNumber
    ^sequenceNumber
!

time
    ^time
!

owner
    ^self mappedId:  owner
!

selection
    ^self mappedId:  selection
!

!

!SelectionClearEvent methodsFor: 'private'!

initFrom: aStream
    aStream ubyte. 
    sequenceNumber _ aStream uword. 
    time _ aStream long. 
    owner _ aStream long. 
    selection _ aStream long. 
    aStream skipBytes: 16
!!


XEvent subclass: #SelectionRequestEvent
	instanceVariableNames: 'time owner requestor selection target property '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!SelectionRequestEvent class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!SelectionRequestEvent methodsFor: 'accessing'!

sequenceNumber
    ^sequenceNumber
!

time
    ^time
!

owner
    ^self mappedId:  owner
!

requestor
    ^self mappedId:  requestor
!

selection
    ^self mappedId:  selection
!

target
    ^self mappedId:  target
!

property
    ^self mappedId:  property
!

!

!SelectionRequestEvent methodsFor: 'private'!

initFrom: aStream
    aStream ubyte. 
    sequenceNumber _ aStream uword. 
    time _ aStream long. 
    owner _ aStream long. 
    requestor _ aStream long. 
    selection _ aStream long. 
    target _ aStream long. 
    property _ aStream long. 
    aStream skipBytes: 4
!!


XEvent subclass: #SelectionNotifyEvent
	instanceVariableNames: 'time requestor selection target property '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!SelectionNotifyEvent class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!SelectionNotifyEvent methodsFor: 'accessing'!

sequenceNumber
    ^sequenceNumber
!

time
    ^time
!

requestor
    ^self mappedId:  requestor
!

selection
    ^self mappedId:  selection
!

target
    ^self mappedId:  target
!

property
    ^self mappedId:  property
!

!

!SelectionNotifyEvent methodsFor: 'private'!

initFrom: aStream
    aStream ubyte. 
    sequenceNumber _ aStream uword. 
    time _ aStream long. 
    requestor _ aStream long. 
    selection _ aStream long. 
    target _ aStream long. 
    property _ aStream long. 
    aStream skipBytes: 8
!!


XEvent subclass: #ColormapNotifyEvent
	instanceVariableNames: 'window colormap new state '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!ColormapNotifyEvent class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!ColormapNotifyEvent methodsFor: 'accessing'!

sequenceNumber
    ^sequenceNumber
!

window
    ^self mappedId:  window
!

colormap
    ^self mappedId:  colormap
!

new
    ^new
!

state
    ^state
!

!

!ColormapNotifyEvent methodsFor: 'private'!

initFrom: aStream
    aStream ubyte. 
    sequenceNumber _ aStream uword. 
    window _ aStream long. 
    colormap _ aStream long. 
    new _ aStream byte. 
    state _ aStream ubyte. 
    aStream skipBytes: 18
!!


XEvent subclass: #ClientMessageEvent
	instanceVariableNames: 'format window type '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!ClientMessageEvent class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!ClientMessageEvent methodsFor: 'accessing'!

format
    ^format
!

sequenceNumber
    ^sequenceNumber
!

window
    ^self mappedId:  window
!

type
    ^self mappedId:  type
!

!

!ClientMessageEvent methodsFor: 'private'!

initFrom: aStream
    format _ aStream ubyte. 
    sequenceNumber _ aStream uword. 
    window _ aStream long. 
    type _ aStream long. 
    aStream skipBytes: 20
!!


XEvent subclass: #MappingNotifyEvent
	instanceVariableNames: 'request keycode count '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X hacking'
!

!MappingNotifyEvent class methodsFor: 'instance creation'!

newFrom: aStream
    ^self new initFrom: aStream
!!

!MappingNotifyEvent methodsFor: 'accessing'!

sequenceNumber
    ^sequenceNumber
!

request
    ^request
!

keycode
    ^keycode
!

count
    ^count
!

!

!MappingNotifyEvent methodsFor: 'private'!

initFrom: aStream
    aStream ubyte. 
    sequenceNumber _ aStream uword. 
    request _ aStream ubyte. 
    keycode _ aStream byte. 
    count _ aStream ubyte. 
    aStream skipBytes: 25
!!


