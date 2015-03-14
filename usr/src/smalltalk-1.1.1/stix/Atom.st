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

Object subclass: #Atom
	instanceVariableNames: 'id'
	classVariableNames: 'AtomNames'
	poolDictionaries: ''
	category: 'X window attributes'
!
    
!Symbol methodsFor: 'Atom mapping'!

mapToId
    ^(Atom symbolMap: self) mapToId

!!

!Atom class methodsFor: 'initialization'!

initialize
    | names i |
    AtomNames _ Dictionary new.    
    names _ #(Primary Secondary Arc Atom Bitmap Cardinal Colormap Cursor CutBuffer0
		  CutBuffer1 CutBuffer2 CutBuffer3 CutBuffer4 CutBuffer5 CutBuffer6
		  CutBuffer7 Drawable Font Integer Pixmap Point Rectangle ResourceManager
		  RGBColorMap RGBBestMap RGBBlueMap RGBDefaultMap RGBGrayMap RGBGreenMap
		  RGBRedMap String Visualid Window WmCommand WmHints WmClientMachine
		  WmIconName WmIconSize WmName WmNormalHints WmSizeHints WmZoomHints
		  MemSpace NormSpace MaxSpace EndSpace SuperscriptX SuperscriptY
		  SubstriptX SubscriptY UnderlinePosition UnderlineThickness
		  StrikeoutAscent StrikeoutDescent ItalicAngle XHeight QuadWidth
		  Weight PointSize Resolution Copyright Notice FontName FamilyName
		  FullName CapHeight WmClass WmTransientFor).
    i _ 1.
    names do: 
	[ :name | AtomNames at: name put: (Atom new: i).
		  i _ i + 1 ].
!

new: anId
    ^self new init: anId
!!
    
!Atom class methodsFor: 'accessing'!

symbolMap: anAtomId
    ^AtomNames at: anAtomId
!!


!Atom methodsFor: 'accessing'!

mapToId
    ^id
!!

!Atom methodsFor: 'private'!

init: anId
    id _ anId
!!

Atom initialize!


