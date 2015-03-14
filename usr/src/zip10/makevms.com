$ !
$ !     "Makefile" for VMS versions of Zip, ZipNote,
$ !      ZipSplit, Ship and UnShip (stolen from Unzip)
$ !
$ set verify    ! like "echo on", eh?
$ !
$ !------------------------------- Zip section --------------------------------
$ !
$ cc /def=EXPORT zip,zipfile,zipup,fileio,util,tempf,shrink,globals,implode,im_lmat,im_ctree,im_bits
$ link zip,zipfile,zipup,fileio,util,tempf,shrink,globals,implode,im_lmat,im_ctree,im_bits,sys$input:/opt
sys$share:vaxcrtl.exe/shareable
$ !
$ ! If you have problems with implode, compile with /define=noimplode
$ ! and remove all the im* files from the above lines.
$ !
$ !-------------------------- Zip utilities section ---------------------------
$ !
$ ren zipfile.c zipfile_.c;*
$ ren zipup.c zipup_.c;*
$ ren fileio.c fileio_.c;*
$ ren util.c util_.c;*
$ cc /def=EXPORT zipnote, zipsplit
$ cc /def=EXPORT /def=UTIL zipfile_, zipup_, fileio_, util_
$ ren zipfile_.c zipfile.c;*
$ ren zipup_.c zipup.c;*
$ ren fileio_.c fileio.c;*
$ ren util_.c util.c;*
$ link zipnote, zipfile_, zipup_, fileio_, globals, sys$input:/opt
sys$share:vaxcrtl.exe/shareable
$ link zipsplit, zipfile_, zipup_, fileio_, globals, sys$input:/opt
sys$share:vaxcrtl.exe/shareable
$ !
$ !--------------------------- Ship/UnShip section ----------------------------
$ !
$ cc ship
$ link ship,sys$input:/opt
sys$share:vaxcrtl.exe/shareable
$ !
$ ! Create a hard link.  (To remove both files, delete the copy FIRST, then
$ ! the original.  Otherwise, if original deleted first [copy says "no such
$ ! file"], must use "set file/remove unship.exe;#" to get rid of the copy.
$ ! Unlike in Unix, deleting the original ALWAYS destroys the data--but not
$ ! the directory entry of the copy.)  Using a hard link saves disk space, by
$ ! the way.  Note, however, that copying a hard link copies the data, not
$ ! just the link.  Therefore, set up the link in the directory in which the
$ ! executable is to reside, or else rename (move) the executables into the
$ ! directory.
$ !
$ set file/enter=unship.exe ship.exe
$ !
$ !----------------------------- Symbols section ------------------------------
$ !
$ ! Set up symbols for the various executables.  Edit the example below,
$ ! changing "pc" to "disk:[directory]" as appropriate, and uncomment
$ ! (remove the exclamation marks).
$ !
$ ! zip		== "$pc:zip.exe"
$ ! zipnote	== "$pc:zipnote.exe"
$ ! zipsplit	== "$pc:zipsplit.exe"
$ ! ship		== "$pc:ship.exe"
$ ! unship	== "$pc:unship.exe"
$ !
$ set noverify
