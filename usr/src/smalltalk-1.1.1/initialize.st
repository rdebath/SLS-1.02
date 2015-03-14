"======================================================================
|
|   Initialize various Smalltalk classes
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
| sbyrne     24 May 89	  created.
|
"

"This file is loaded after all the kernel Smalltalk classes have been loaded.
It generally performs any initializations that might depend on the full
Smalltalk class hierarchy being defined. After this is file is loaded, 
a couple of random class definitions are loaded (currently CFunc and Autoload)
and then the binary image is saved."


Date initialize!


Smalltalk initialize!

KernelInitialized _ true!

"
""Smalltalk addInit: [ 'Welcome to Smalltalk!!!' print.
""    	    	     Character nl print ]!
""Smalltalk addInit: [ 'I hope you like your stay' print.
""    	    	     Character nl print ]!
"
