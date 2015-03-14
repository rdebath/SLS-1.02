$!
$!    Command file to build CLISP on VMS
$!    Run this as     @MAKE-CLISP
$!
$!    Written by Bruno Haible with the help of David Link 4.3.1993
$!
$! Set the default directory to proper place for use in batch.
$! Works for interactive too.
$ flnm = f$enviroment("PROCEDURE")     ! get current procedure name
$ set default 'f$parse(flnm,,,"DEVICE")''f$parse(flnm,,,"DIRECTORY")'
$ set default [.-]
$!
$! 
$!                      Character set conversions
$!
$! Not done here. We assume the sources were already converted by
$! "all-to-ascii" on a Unix machine and then transferred to VMS using
$! FTP in ASCII mode.
$!
$! set default [.unix]
$! cc /optimize cv-to-ascii.c
$! link cv-to-ascii
$! set default [.-]
$! convert ANNOUNCE, COPYRIGHT, GNU-GPL, INSTALL, SUMMARY,
$!         src/*.*, utils/*.*, unix/*.*
$!
$!
$ create /directory [.build]
$ set default [.build]
$!
$!
$!                      Target "make init"
$!
$! Build ccpaux.exe
$ copy [.-.utils]ccpaux.c ccpaux.c
$ cc /optimize ccpaux.c
$ link ccpaux
$!delete ccpaux.c;*
$ delete ccpaux.obj;*
$!
$! Build comment5.exe
$ spawn /input=[.-.utils]comment5.c /output=comment5.c /nolog run ccpaux
$ cc /optimize comment5.c
$ link comment5
$!delete comment5.c;*
$ delete comment5.obj;*
$!
$! Build deerror.exe
$ copy [.-.utils]deerror.c deerror.c
$ cc /optimize deerror.c
$ link deerror
$!delete deerror.c;*
$ delete deerror.obj;*
$!
$! Build unixconf.h
$ copy [.-.vms]unixconf.h unixconf.h
$!
$! Build machine.exe
$ copy [.-.src]machine.d machine.d
$ mc []comment5 machine
$ cc /optimize machine.c
$ link machine
$ delete machine.d;*
$!delete machine.c;*
$ delete machine.obj;*
$!
$! Build machine.h
$ spawn /output=machine.h /nolog run machine
$!
$! Build traddecl.exe
$ copy [.-.utils]traddecl.d traddecl.d
$ mc []comment5 traddecl
$ cc /optimize /define=("CUT_U_AND_L") traddecl.c
$ link traddecl
$ delete traddecl.d;*
$!delete traddecl.c;*
$ delete traddecl.obj;*
$!
$! Build mergestrings.exe
$ copy [.-.utils]mergestrings.d mergestrings.d
$ mc []comment5 mergestrings
$ spawn /input=mergestrings.c /output=mergestrings.c1 /nolog run ccpaux
$ spawn /input=mergestrings.c1 /output=mergestrings.c /nolog run traddecl
$ cc /optimize mergestrings.c
$ link mergestrings
$ delete mergestrings.d;*
$ delete mergestrings.c1;*
$!delete mergestrings.c;*
$ delete mergestrings.obj;*
$!
$! Build txt2c.exe
$ copy [.-.utils]txt2c.c txt2c.c
$ cc /optimize txt2c.c
$ link txt2c
$!delete txt2c.c;*
$ delete txt2c.obj;*
$!
$! Build generrors.exe
$ copy [.-.utils]generrors.c generrors.c
$ spawn /input=generrors.c /output=generrors.c1 /nolog run ccpaux
$ spawn /input=generrors.c1 /output=generrors.c /nolog run traddecl
$ cc /optimize generrors.c
$ link generrors
$ delete generrors.c1;*
$!delete generrors.c;*
$ delete generrors.obj;*
$!
$! Build errors.c
$ spawn /output=errors.c /nolog run generrors
$!
$!
$!                      Target "make allc"
$!
$! Build spvw.c
$ copy [.-.src]spvw.d spvw.d
$ mc []comment5 spvw
$ delete spvw.d;*
$ spawn /input=spvw.c /output=spvw.c1 /nolog run ccpaux
$ spawn /input=spvw.c1 /output=spvw.c2 /nolog run deerror
$ spawn /input=spvw.c2 /output=spvw.c /nolog run traddecl
$ delete spvw.c1;*
$ delete spvw.c2;*
$!
$! Build spvwtabf.c
$ copy [.-.src]spvwtabf.d spvwtabf.d
$ mc []comment5 spvwtabf
$ delete spvwtabf.d;*
$ spawn /input=spvwtabf.c /output=spvwtabf.c1 /nolog run ccpaux
$ spawn /input=spvwtabf.c1 /output=spvwtabf.c2 /nolog run deerror
$ spawn /input=spvwtabf.c2 /output=spvwtabf.c /nolog run traddecl
$ delete spvwtabf.c1;*
$ delete spvwtabf.c2;*
$!
$! Build spvwtabs.c
$ copy [.-.src]spvwtabs.d spvwtabs.d
$ mc []comment5 spvwtabs
$ delete spvwtabs.d;*
$ spawn /input=spvwtabs.c /output=spvwtabs.c1 /nolog run ccpaux
$ spawn /input=spvwtabs.c1 /output=spvwtabs.c2 /nolog run deerror
$ spawn /input=spvwtabs.c2 /output=spvwtabs.c /nolog run traddecl
$ delete spvwtabs.c1;*
$ delete spvwtabs.c2;*
$!
$! Build spvwtabo.c
$ copy [.-.src]spvwtabo.d spvwtabo.d
$ mc []comment5 spvwtabo
$ delete spvwtabo.d;*
$ spawn /input=spvwtabo.c /output=spvwtabo.c1 /nolog run ccpaux
$ spawn /input=spvwtabo.c1 /output=spvwtabo.c2 /nolog run deerror
$ spawn /input=spvwtabo.c2 /output=spvwtabo.c /nolog run traddecl
$ delete spvwtabo.c1;*
$ delete spvwtabo.c2;*
$!
$! Build eval.c
$ copy [.-.src]eval.d eval.d
$ mc []comment5 eval
$ delete eval.d;*
$ spawn /input=eval.c /output=eval.c1 /nolog run ccpaux
$ spawn /input=eval.c1 /output=eval.c2 /nolog run deerror
$ spawn /input=eval.c2 /output=eval.c /nolog run traddecl
$ delete eval.c1;*
$ delete eval.c2;*
$!
$! Build control.c
$ copy [.-.src]control.d control.d
$ mc []comment5 control
$ delete control.d;*
$ spawn /input=control.c /output=control.c1 /nolog run ccpaux
$ spawn /input=control.c1 /output=control.c2 /nolog run deerror
$ spawn /input=control.c2 /output=control.c /nolog run traddecl
$ delete control.c1;*
$ delete control.c2;*
$!
$! Build pathname.c
$ copy [.-.src]pathname.d pathname.d
$ mc []comment5 pathname
$ delete pathname.d;*
$ spawn /input=pathname.c /output=pathname.c1 /nolog run ccpaux
$ spawn /input=pathname.c1 /output=pathname.c2 /nolog run deerror
$ spawn /input=pathname.c2 /output=pathname.c /nolog run traddecl
$ delete pathname.c1;*
$ delete pathname.c2;*
$!
$! Build stream.c
$ copy [.-.src]stream.d stream.d
$ mc []comment5 stream
$ delete stream.d;*
$ spawn /input=stream.c /output=stream.c1 /nolog run ccpaux
$ spawn /input=stream.c1 /output=stream.c2 /nolog run deerror
$ spawn /input=stream.c2 /output=stream.c /nolog run traddecl
$ delete stream.c1;*
$ delete stream.c2;*
$!
$! Build socket.c
$ copy [.-.src]socket.d socket.d
$ mc []comment5 socket
$ delete socket.d;*
$ spawn /input=socket.c /output=socket.c1 /nolog run ccpaux
$ spawn /input=socket.c1 /output=socket.c2 /nolog run deerror
$ spawn /input=socket.c2 /output=socket.c /nolog run traddecl
$ delete socket.c1;*
$ delete socket.c2;*
$!
$! Build io.c
$ copy [.-.src]io.d io.d
$ mc []comment5 io
$ delete io.d;*
$ spawn /input=io.c /output=io.c1 /nolog run ccpaux
$ spawn /input=io.c1 /output=io.c2 /nolog run deerror
$ spawn /input=io.c2 /output=io.c /nolog run traddecl
$ delete io.c1;*
$ delete io.c2;*
$!
$! Build array.c
$ copy [.-.src]array.d array.d
$ mc []comment5 array
$ delete array.d;*
$ spawn /input=array.c /output=array.c1 /nolog run ccpaux
$ spawn /input=array.c1 /output=array.c2 /nolog run deerror
$ spawn /input=array.c2 /output=array.c /nolog run traddecl
$ delete array.c1;*
$ delete array.c2;*
$!
$! Build hashtabl.c
$ copy [.-.src]hashtabl.d hashtabl.d
$ mc []comment5 hashtabl
$ delete hashtabl.d;*
$ spawn /input=hashtabl.c /output=hashtabl.c1 /nolog run ccpaux
$ spawn /input=hashtabl.c1 /output=hashtabl.c2 /nolog run deerror
$ spawn /input=hashtabl.c2 /output=hashtabl.c /nolog run traddecl
$ delete hashtabl.c1;*
$ delete hashtabl.c2;*
$!
$! Build list.c
$ copy [.-.src]list.d list.d
$ mc []comment5 list
$ delete list.d;*
$ spawn /input=list.c /output=list.c1 /nolog run ccpaux
$ spawn /input=list.c1 /output=list.c2 /nolog run deerror
$ spawn /input=list.c2 /output=list.c /nolog run traddecl
$ delete list.c1;*
$ delete list.c2;*
$!
$! Build package.c
$ copy [.-.src]package.d package.d
$ mc []comment5 package
$ delete package.d;*
$ spawn /input=package.c /output=package.c1 /nolog run ccpaux
$ spawn /input=package.c1 /output=package.c2 /nolog run deerror
$ spawn /input=package.c2 /output=package.c /nolog run traddecl
$ delete package.c1;*
$ delete package.c2;*
$!
$! Build record.c
$ copy [.-.src]record.d record.d
$ mc []comment5 record
$ delete record.d;*
$ spawn /input=record.c /output=record.c1 /nolog run ccpaux
$ spawn /input=record.c1 /output=record.c2 /nolog run deerror
$ spawn /input=record.c2 /output=record.c /nolog run traddecl
$ delete record.c1;*
$ delete record.c2;*
$!
$! Build sequence.c
$ copy [.-.src]sequence.d sequence.d
$ mc []comment5 sequence
$ delete sequence.d;*
$ spawn /input=sequence.c /output=sequence.c1 /nolog run ccpaux
$ spawn /input=sequence.c1 /output=sequence.c2 /nolog run deerror
$ spawn /input=sequence.c2 /output=sequence.c /nolog run traddecl
$ delete sequence.c1;*
$ delete sequence.c2;*
$!
$! Build charstrg.c
$ copy [.-.src]charstrg.d charstrg.d
$ mc []comment5 charstrg
$ delete charstrg.d;*
$ spawn /input=charstrg.c /output=charstrg.c1 /nolog run ccpaux
$ spawn /input=charstrg.c1 /output=charstrg.c2 /nolog run deerror
$ spawn /input=charstrg.c2 /output=charstrg.c /nolog run traddecl
$ delete charstrg.c1;*
$ delete charstrg.c2;*
$!
$! Build debug.c
$ copy [.-.src]debug.d debug.d
$ mc []comment5 debug
$ delete debug.d;*
$ spawn /input=debug.c /output=debug.c1 /nolog run ccpaux
$ spawn /input=debug.c1 /output=debug.c2 /nolog run deerror
$ spawn /input=debug.c2 /output=debug.c /nolog run traddecl
$ delete debug.c1;*
$ delete debug.c2;*
$!
$! Build misc.c
$ copy [.-.src]misc.d misc.d
$ mc []comment5 misc
$ delete misc.d;*
$ spawn /input=misc.c /output=misc.c1 /nolog run ccpaux
$ spawn /input=misc.c1 /output=misc.c2 /nolog run deerror
$ spawn /input=misc.c2 /output=misc.c /nolog run traddecl
$ delete misc.c1;*
$ delete misc.c2;*
$!
$! Build predtype.c
$ copy [.-.src]predtype.d predtype.d
$ mc []comment5 predtype
$ delete predtype.d;*
$ spawn /input=predtype.c /output=predtype.c1 /nolog run ccpaux
$ spawn /input=predtype.c1 /output=predtype.c2 /nolog run deerror
$ spawn /input=predtype.c2 /output=predtype.c /nolog run traddecl
$ delete predtype.c1;*
$ delete predtype.c2;*
$!
$! Build symbol.c
$ copy [.-.src]symbol.d symbol.d
$ mc []comment5 symbol
$ delete symbol.d;*
$ spawn /input=symbol.c /output=symbol.c1 /nolog run ccpaux
$ spawn /input=symbol.c1 /output=symbol.c2 /nolog run deerror
$ spawn /input=symbol.c2 /output=symbol.c /nolog run traddecl
$ delete symbol.c1;*
$ delete symbol.c2;*
$!
$! Build lisparit.c
$ copy [.-.src]lisparit.d lisparit.d
$ mc []comment5 lisparit
$ delete lisparit.d;*
$ spawn /input=lisparit.c /output=lisparit.c1 /nolog run ccpaux
$ spawn /input=lisparit.c1 /output=lisparit.c2 /nolog run deerror
$ spawn /input=lisparit.c2 /output=lisparit.c /nolog run traddecl
$ delete lisparit.c1;*
$ delete lisparit.c2;*
$!
$! Build lispbibl.c
$ copy [.-.src]lispbibl.d lispbibl.d
$ mc []comment5 lispbibl
$ delete lispbibl.d;*
$ spawn /input=lispbibl.c /output=lispbibl.c1 /nolog run ccpaux
$ spawn /input=lispbibl.c1 /output=lispbibl.c2 /nolog run deerror
$ spawn /input=lispbibl.c2 /output=lispbibl.c /nolog run traddecl
$ delete lispbibl.c1;*
$ delete lispbibl.c2;*
$!
$! Build fsubr.c
$ copy [.-.src]fsubr.d fsubr.d
$ mc []comment5 fsubr
$ delete fsubr.d;*
$ spawn /input=fsubr.c /output=fsubr.c1 /nolog run ccpaux
$ spawn /input=fsubr.c1 /output=fsubr.c2 /nolog run deerror
$ spawn /input=fsubr.c2 /output=fsubr.c /nolog run traddecl
$ delete fsubr.c1;*
$ delete fsubr.c2;*
$!
$! Build subr.c
$ copy [.-.src]subr.d subr.d
$ mc []comment5 subr
$ delete subr.d;*
$ spawn /input=subr.c /output=subr.c1 /nolog run ccpaux
$ spawn /input=subr.c1 /output=subr.c2 /nolog run deerror
$ spawn /input=subr.c2 /output=subr.c /nolog run traddecl
$ delete subr.c1;*
$ delete subr.c2;*
$!
$! Build pseudofun.c
$ copy [.-.src]pseudofun.d pseudofun.d
$ mc []comment5 pseudofun
$ delete pseudofun.d;*
$ spawn /input=pseudofun.c /output=pseudofun.c1 /nolog run ccpaux
$ spawn /input=pseudofun.c1 /output=pseudofun.c2 /nolog run deerror
$ spawn /input=pseudofun.c2 /output=pseudofun.c /nolog run traddecl
$ delete pseudofun.c1;*
$ delete pseudofun.c2;*
$!
$! Build constsym.c
$ copy [.-.src]constsym.d constsym.d
$ mc []comment5 constsym
$ delete constsym.d;*
$ spawn /input=constsym.c /output=constsym.c1 /nolog run ccpaux
$ spawn /input=constsym.c1 /output=constsym.c2 /nolog run deerror
$ spawn /input=constsym.c2 /output=constsym.c /nolog run traddecl
$ delete constsym.c1;*
$ delete constsym.c2;*
$!
$! Build constobj.c
$ copy [.-.src]constobj.d constobj.d
$ mc []comment5 constobj
$ delete constobj.d;*
$ spawn /input=constobj.c /output=constobj.c1 /nolog run ccpaux
$ spawn /input=constobj.c1 /output=constobj.c2 /nolog run deerror
$ spawn /input=constobj.c2 /output=constobj.c /nolog run traddecl
$ delete constobj.c1;*
$ delete constobj.c2;*
$!
$! Build vms.c
$ copy [.-.src]vms.d vms.d
$ mc []comment5 vms
$ delete vms.d;*
$ spawn /input=vms.c /output=vms.c1 /nolog run ccpaux
$ spawn /input=vms.c1 /output=vms.c2 /nolog run deerror
$ spawn /input=vms.c2 /output=vms.c /nolog run traddecl
$ delete vms.c1;*
$ delete vms.c2;*
$!
$! Build avl.c
$ copy [.-.src]avl.d avl.d
$ mc []comment5 avl
$ delete avl.d;*
$ spawn /input=avl.c /output=avl.c1 /nolog run ccpaux
$ spawn /input=avl.c1 /output=avl.c2 /nolog run deerror
$ spawn /input=avl.c2 /output=avl.c /nolog run traddecl
$ delete avl.c1;*
$ delete avl.c2;*
$!
$! Build sort.c
$ copy [.-.src]sort.d sort.d
$ mc []comment5 sort
$ delete sort.d;*
$ spawn /input=sort.c /output=sort.c1 /nolog run ccpaux
$ spawn /input=sort.c1 /output=sort.c2 /nolog run deerror
$ spawn /input=sort.c2 /output=sort.c /nolog run traddecl
$ delete sort.c1;*
$ delete sort.c2;*
$!
$! Build aridecl.c
$ copy [.-.src]aridecl.d aridecl.d
$ mc []comment5 aridecl
$ delete aridecl.d;*
$ spawn /input=aridecl.c /output=aridecl.c1 /nolog run ccpaux
$ spawn /input=aridecl.c1 /output=aridecl.c2 /nolog run deerror
$ spawn /input=aridecl.c2 /output=aridecl.c /nolog run traddecl
$ delete aridecl.c1;*
$ delete aridecl.c2;*
$!
$! Build arilev0.c
$ copy [.-.src]arilev0.d arilev0.d
$ mc []comment5 arilev0
$ delete arilev0.d;*
$ spawn /input=arilev0.c /output=arilev0.c1 /nolog run ccpaux
$ spawn /input=arilev0.c1 /output=arilev0.c2 /nolog run deerror
$ spawn /input=arilev0.c2 /output=arilev0.c /nolog run traddecl
$ delete arilev0.c1;*
$ delete arilev0.c2;*
$!
$! Build arilev1.c
$ copy [.-.src]arilev1.d arilev1.d
$ mc []comment5 arilev1
$ delete arilev1.d;*
$ spawn /input=arilev1.c /output=arilev1.c1 /nolog run ccpaux
$ spawn /input=arilev1.c1 /output=arilev1.c2 /nolog run deerror
$ spawn /input=arilev1.c2 /output=arilev1.c /nolog run traddecl
$ delete arilev1.c1;*
$ delete arilev1.c2;*
$!
$! Build arilev1c.c
$ copy [.-.src]arilev1c.d arilev1c.d
$ mc []comment5 arilev1c
$ delete arilev1c.d;*
$ spawn /input=arilev1c.c /output=arilev1c.c1 /nolog run ccpaux
$ spawn /input=arilev1c.c1 /output=arilev1c.c2 /nolog run deerror
$ spawn /input=arilev1c.c2 /output=arilev1c.c /nolog run traddecl
$ delete arilev1c.c1;*
$ delete arilev1c.c2;*
$!
$! Build arilev1e.c
$ copy [.-.src]arilev1e.d arilev1e.d
$ mc []comment5 arilev1e
$ delete arilev1e.d;*
$ spawn /input=arilev1e.c /output=arilev1e.c1 /nolog run ccpaux
$ spawn /input=arilev1e.c1 /output=arilev1e.c2 /nolog run deerror
$ spawn /input=arilev1e.c2 /output=arilev1e.c /nolog run traddecl
$ delete arilev1e.c1;*
$ delete arilev1e.c2;*
$!
$! Build arilev1i.c
$ copy [.-.src]arilev1i.d arilev1i.d
$ mc []comment5 arilev1i
$ delete arilev1i.d;*
$ spawn /input=arilev1i.c /output=arilev1i.c1 /nolog run ccpaux
$ spawn /input=arilev1i.c1 /output=arilev1i.c2 /nolog run deerror
$ spawn /input=arilev1i.c2 /output=arilev1i.c /nolog run traddecl
$ delete arilev1i.c1;*
$ delete arilev1i.c2;*
$!
$! Build intelem.c
$ copy [.-.src]intelem.d intelem.d
$ mc []comment5 intelem
$ delete intelem.d;*
$ spawn /input=intelem.c /output=intelem.c1 /nolog run ccpaux
$ spawn /input=intelem.c1 /output=intelem.c2 /nolog run deerror
$ spawn /input=intelem.c2 /output=intelem.c /nolog run traddecl
$ delete intelem.c1;*
$ delete intelem.c2;*
$!
$! Build intlog.c
$ copy [.-.src]intlog.d intlog.d
$ mc []comment5 intlog
$ delete intlog.d;*
$ spawn /input=intlog.c /output=intlog.c1 /nolog run ccpaux
$ spawn /input=intlog.c1 /output=intlog.c2 /nolog run deerror
$ spawn /input=intlog.c2 /output=intlog.c /nolog run traddecl
$ delete intlog.c1;*
$ delete intlog.c2;*
$!
$! Build intplus.c
$ copy [.-.src]intplus.d intplus.d
$ mc []comment5 intplus
$ delete intplus.d;*
$ spawn /input=intplus.c /output=intplus.c1 /nolog run ccpaux
$ spawn /input=intplus.c1 /output=intplus.c2 /nolog run deerror
$ spawn /input=intplus.c2 /output=intplus.c /nolog run traddecl
$ delete intplus.c1;*
$ delete intplus.c2;*
$!
$! Build intcomp.c
$ copy [.-.src]intcomp.d intcomp.d
$ mc []comment5 intcomp
$ delete intcomp.d;*
$ spawn /input=intcomp.c /output=intcomp.c1 /nolog run ccpaux
$ spawn /input=intcomp.c1 /output=intcomp.c2 /nolog run deerror
$ spawn /input=intcomp.c2 /output=intcomp.c /nolog run traddecl
$ delete intcomp.c1;*
$ delete intcomp.c2;*
$!
$! Build intbyte.c
$ copy [.-.src]intbyte.d intbyte.d
$ mc []comment5 intbyte
$ delete intbyte.d;*
$ spawn /input=intbyte.c /output=intbyte.c1 /nolog run ccpaux
$ spawn /input=intbyte.c1 /output=intbyte.c2 /nolog run deerror
$ spawn /input=intbyte.c2 /output=intbyte.c /nolog run traddecl
$ delete intbyte.c1;*
$ delete intbyte.c2;*
$!
$! Build intmal.c
$ copy [.-.src]intmal.d intmal.d
$ mc []comment5 intmal
$ delete intmal.d;*
$ spawn /input=intmal.c /output=intmal.c1 /nolog run ccpaux
$ spawn /input=intmal.c1 /output=intmal.c2 /nolog run deerror
$ spawn /input=intmal.c2 /output=intmal.c /nolog run traddecl
$ delete intmal.c1;*
$ delete intmal.c2;*
$!
$! Build intdiv.c
$ copy [.-.src]intdiv.d intdiv.d
$ mc []comment5 intdiv
$ delete intdiv.d;*
$ spawn /input=intdiv.c /output=intdiv.c1 /nolog run ccpaux
$ spawn /input=intdiv.c1 /output=intdiv.c2 /nolog run deerror
$ spawn /input=intdiv.c2 /output=intdiv.c /nolog run traddecl
$ delete intdiv.c1;*
$ delete intdiv.c2;*
$!
$! Build intgcd.c
$ copy [.-.src]intgcd.d intgcd.d
$ mc []comment5 intgcd
$ delete intgcd.d;*
$ spawn /input=intgcd.c /output=intgcd.c1 /nolog run ccpaux
$ spawn /input=intgcd.c1 /output=intgcd.c2 /nolog run deerror
$ spawn /input=intgcd.c2 /output=intgcd.c /nolog run traddecl
$ delete intgcd.c1;*
$ delete intgcd.c2;*
$!
$! Build int2adic.c
$ copy [.-.src]int2adic.d int2adic.d
$ mc []comment5 int2adic
$ delete int2adic.d;*
$ spawn /input=int2adic.c /output=int2adic.c1 /nolog run ccpaux
$ spawn /input=int2adic.c1 /output=int2adic.c2 /nolog run deerror
$ spawn /input=int2adic.c2 /output=int2adic.c /nolog run traddecl
$ delete int2adic.c1;*
$ delete int2adic.c2;*
$!
$! Build intsqrt.c
$ copy [.-.src]intsqrt.d intsqrt.d
$ mc []comment5 intsqrt
$ delete intsqrt.d;*
$ spawn /input=intsqrt.c /output=intsqrt.c1 /nolog run ccpaux
$ spawn /input=intsqrt.c1 /output=intsqrt.c2 /nolog run deerror
$ spawn /input=intsqrt.c2 /output=intsqrt.c /nolog run traddecl
$ delete intsqrt.c1;*
$ delete intsqrt.c2;*
$!
$! Build intprint.c
$ copy [.-.src]intprint.d intprint.d
$ mc []comment5 intprint
$ delete intprint.d;*
$ spawn /input=intprint.c /output=intprint.c1 /nolog run ccpaux
$ spawn /input=intprint.c1 /output=intprint.c2 /nolog run deerror
$ spawn /input=intprint.c2 /output=intprint.c /nolog run traddecl
$ delete intprint.c1;*
$ delete intprint.c2;*
$!
$! Build intread.c
$ copy [.-.src]intread.d intread.d
$ mc []comment5 intread
$ delete intread.d;*
$ spawn /input=intread.c /output=intread.c1 /nolog run ccpaux
$ spawn /input=intread.c1 /output=intread.c2 /nolog run deerror
$ spawn /input=intread.c2 /output=intread.c /nolog run traddecl
$ delete intread.c1;*
$ delete intread.c2;*
$!
$! Build rational.c
$ copy [.-.src]rational.d rational.d
$ mc []comment5 rational
$ delete rational.d;*
$ spawn /input=rational.c /output=rational.c1 /nolog run ccpaux
$ spawn /input=rational.c1 /output=rational.c2 /nolog run deerror
$ spawn /input=rational.c2 /output=rational.c /nolog run traddecl
$ delete rational.c1;*
$ delete rational.c2;*
$!
$! Build sfloat.c
$ copy [.-.src]sfloat.d sfloat.d
$ mc []comment5 sfloat
$ delete sfloat.d;*
$ spawn /input=sfloat.c /output=sfloat.c1 /nolog run ccpaux
$ spawn /input=sfloat.c1 /output=sfloat.c2 /nolog run deerror
$ spawn /input=sfloat.c2 /output=sfloat.c /nolog run traddecl
$ delete sfloat.c1;*
$ delete sfloat.c2;*
$!
$! Build ffloat.c
$ copy [.-.src]ffloat.d ffloat.d
$ mc []comment5 ffloat
$ delete ffloat.d;*
$ spawn /input=ffloat.c /output=ffloat.c1 /nolog run ccpaux
$ spawn /input=ffloat.c1 /output=ffloat.c2 /nolog run deerror
$ spawn /input=ffloat.c2 /output=ffloat.c /nolog run traddecl
$ delete ffloat.c1;*
$ delete ffloat.c2;*
$!
$! Build dfloat.c
$ copy [.-.src]dfloat.d dfloat.d
$ mc []comment5 dfloat
$ delete dfloat.d;*
$ spawn /input=dfloat.c /output=dfloat.c1 /nolog run ccpaux
$ spawn /input=dfloat.c1 /output=dfloat.c2 /nolog run deerror
$ spawn /input=dfloat.c2 /output=dfloat.c /nolog run traddecl
$ delete dfloat.c1;*
$ delete dfloat.c2;*
$!
$! Build lfloat.c
$ copy [.-.src]lfloat.d lfloat.d
$ mc []comment5 lfloat
$ delete lfloat.d;*
$ spawn /input=lfloat.c /output=lfloat.c1 /nolog run ccpaux
$ spawn /input=lfloat.c1 /output=lfloat.c2 /nolog run deerror
$ spawn /input=lfloat.c2 /output=lfloat.c /nolog run traddecl
$ delete lfloat.c1;*
$ delete lfloat.c2;*
$!
$! Build flo_konv.c
$ copy [.-.src]flo_konv.d flo_konv.d
$ mc []comment5 flo_konv
$ delete flo_konv.d;*
$ spawn /input=flo_konv.c /output=flo_konv.c1 /nolog run ccpaux
$ spawn /input=flo_konv.c1 /output=flo_konv.c2 /nolog run deerror
$ spawn /input=flo_konv.c2 /output=flo_konv.c /nolog run traddecl
$ delete flo_konv.c1;*
$ delete flo_konv.c2;*
$!
$! Build flo_rest.c
$ copy [.-.src]flo_rest.d flo_rest.d
$ mc []comment5 flo_rest
$ delete flo_rest.d;*
$ spawn /input=flo_rest.c /output=flo_rest.c1 /nolog run ccpaux
$ spawn /input=flo_rest.c1 /output=flo_rest.c2 /nolog run deerror
$ spawn /input=flo_rest.c2 /output=flo_rest.c /nolog run traddecl
$ delete flo_rest.c1;*
$ delete flo_rest.c2;*
$!
$! Build realelem.c
$ copy [.-.src]realelem.d realelem.d
$ mc []comment5 realelem
$ delete realelem.d;*
$ spawn /input=realelem.c /output=realelem.c1 /nolog run ccpaux
$ spawn /input=realelem.c1 /output=realelem.c2 /nolog run deerror
$ spawn /input=realelem.c2 /output=realelem.c /nolog run traddecl
$ delete realelem.c1;*
$ delete realelem.c2;*
$!
$! Build realrand.c
$ copy [.-.src]realrand.d realrand.d
$ mc []comment5 realrand
$ delete realrand.d;*
$ spawn /input=realrand.c /output=realrand.c1 /nolog run ccpaux
$ spawn /input=realrand.c1 /output=realrand.c2 /nolog run deerror
$ spawn /input=realrand.c2 /output=realrand.c /nolog run traddecl
$ delete realrand.c1;*
$ delete realrand.c2;*
$!
$! Build realtran.c
$ copy [.-.src]realtran.d realtran.d
$ mc []comment5 realtran
$ delete realtran.d;*
$ spawn /input=realtran.c /output=realtran.c1 /nolog run ccpaux
$ spawn /input=realtran.c1 /output=realtran.c2 /nolog run deerror
$ spawn /input=realtran.c2 /output=realtran.c /nolog run traddecl
$ delete realtran.c1;*
$ delete realtran.c2;*
$!
$! Build compelem.c
$ copy [.-.src]compelem.d compelem.d
$ mc []comment5 compelem
$ delete compelem.d;*
$ spawn /input=compelem.c /output=compelem.c1 /nolog run ccpaux
$ spawn /input=compelem.c1 /output=compelem.c2 /nolog run deerror
$ spawn /input=compelem.c2 /output=compelem.c /nolog run traddecl
$ delete compelem.c1;*
$ delete compelem.c2;*
$!
$! Build comptran.c
$ copy [.-.src]comptran.d comptran.d
$ mc []comment5 comptran
$ delete comptran.d;*
$ spawn /input=comptran.c /output=comptran.c1 /nolog run ccpaux
$ spawn /input=comptran.c1 /output=comptran.c2 /nolog run deerror
$ spawn /input=comptran.c2 /output=comptran.c /nolog run traddecl
$ delete comptran.c1;*
$ delete comptran.c2;*
$!
$! Build arivax.c
$ copy [.-.src]arivax.d arivax.d
$ mc []comment5 arivax
$ delete arivax.d;*
$ spawn /input=arivax.c /output=arivax.c1 /nolog run ccpaux
$ spawn /input=arivax.c1 /output=arivax.c /nolog run deerror
$ delete arivax.c1;*
$!
$!
$!                      Target "make allo"
$!
$! Build spvw.i
$ cc /preprocess_only=spvw.i1 spvw.c
$ spawn /input=spvw.i1 /output=spvw.i /nolog run mergestrings
$ delete spvw.i1;*
$!
$! Build spvwtabf.i
$ cc /preprocess_only=spvwtabf.i1 spvwtabf.c
$ spawn /input=spvwtabf.i1 /output=spvwtabf.i /nolog run mergestrings
$ delete spvwtabf.i1;*
$!
$! Build spvwtabs.i
$ cc /preprocess_only=spvwtabs.i1 spvwtabs.c
$ spawn /input=spvwtabs.i1 /output=spvwtabs.i /nolog run mergestrings
$ delete spvwtabs.i1;*
$!
$! Build spvwtabo.i
$ cc /preprocess_only=spvwtabo.i1 spvwtabo.c
$ spawn /input=spvwtabo.i1 /output=spvwtabo.i /nolog run mergestrings
$ delete spvwtabo.i1;*
$!
$! Build eval.i
$ cc /preprocess_only=eval.i1 eval.c
$ spawn /input=eval.i1 /output=eval.i /nolog run mergestrings
$ delete eval.i1;*
$!
$! Build control.i
$ cc /preprocess_only=control.i1 control.c
$ spawn /input=control.i1 /output=control.i /nolog run mergestrings
$ delete control.i1;*
$!
$! Build pathname.i
$ cc /preprocess_only=pathname.i1 pathname.c
$ spawn /input=pathname.i1 /output=pathname.i /nolog run mergestrings
$ delete pathname.i1;*
$!
$! Build stream.i
$ cc /preprocess_only=stream.i1 stream.c
$ spawn /input=stream.i1 /output=stream.i /nolog run mergestrings
$ delete stream.i1;*
$!
$! Build socket.i
$ cc /preprocess_only=socket.i1 socket.c
$ spawn /input=socket.i1 /output=socket.i /nolog run mergestrings
$ delete socket.i1;*
$!
$! Build io.i
$ cc /preprocess_only=io.i1 io.c
$ spawn /input=io.i1 /output=io.i /nolog run mergestrings
$ delete io.i1;*
$!
$! Build array.i
$ cc /preprocess_only=array.i1 array.c
$ spawn /input=array.i1 /output=array.i /nolog run mergestrings
$ delete array.i1;*
$!
$! Build hashtabl.i
$ cc /preprocess_only=hashtabl.i1 hashtabl.c
$ spawn /input=hashtabl.i1 /output=hashtabl.i /nolog run mergestrings
$ delete hashtabl.i1;*
$!
$! Build list.i
$ cc /preprocess_only=list.i1 list.c
$ spawn /input=list.i1 /output=list.i /nolog run mergestrings
$ delete list.i1;*
$!
$! Build package.i
$ cc /preprocess_only=package.i1 package.c
$ spawn /input=package.i1 /output=package.i /nolog run mergestrings
$ delete package.i1;*
$!
$! Build record.i
$ cc /preprocess_only=record.i1 record.c
$ spawn /input=record.i1 /output=record.i /nolog run mergestrings
$ delete record.i1;*
$!
$! Build sequence.i
$ cc /preprocess_only=sequence.i1 sequence.c
$ spawn /input=sequence.i1 /output=sequence.i /nolog run mergestrings
$ delete sequence.i1;*
$!
$! Build charstrg.i
$ cc /preprocess_only=charstrg.i1 charstrg.c
$ spawn /input=charstrg.i1 /output=charstrg.i /nolog run mergestrings
$ delete charstrg.i1;*
$!
$! Build debug.i
$ cc /preprocess_only=debug.i1 debug.c
$ spawn /input=debug.i1 /output=debug.i /nolog run mergestrings
$ delete debug.i1;*
$!
$! Build misc.i
$ cc /preprocess_only=misc.i1 misc.c
$ spawn /input=misc.i1 /output=misc.i /nolog run mergestrings
$ delete misc.i1;*
$!
$! Build predtype.i
$ cc /preprocess_only=predtype.i1 predtype.c
$ spawn /input=predtype.i1 /output=predtype.i /nolog run mergestrings
$ delete predtype.i1;*
$!
$! Build symbol.i
$ cc /preprocess_only=symbol.i1 symbol.c
$ spawn /input=symbol.i1 /output=symbol.i /nolog run mergestrings
$ delete symbol.i1;*
$!
$! Build lisparit.i
$ cc /preprocess_only=lisparit.i1 lisparit.c
$ spawn /input=lisparit.i1 /output=lisparit.i /nolog run mergestrings
$ delete lisparit.i1;*
$!
$! Build spvw.obj
$ cc /optimize spvw.i
$!
$! Build spvwtabf.obj
$ cc /optimize spvwtabf.i
$!
$! Build spvwtabs.obj
$ cc /optimize spvwtabs.i
$!
$! Build spvwtabo.obj
$ cc /optimize spvwtabo.i
$!
$! Build eval.obj
$ cc /optimize eval.i
$!
$! Build control.obj
$ cc /optimize control.i
$!
$! Build pathname.obj
$ cc /optimize pathname.i
$!
$! Build stream.obj
$ cc /optimize stream.i
$!
$! Build socket.obj
$ cc /optimize socket.i
$!
$! Build io.obj
$ cc /optimize io.i
$!
$! Build array.obj
$ cc /optimize array.i
$!
$! Build hashtabl.obj
$ cc /optimize hashtabl.i
$!
$! Build list.obj
$ cc /optimize list.i
$!
$! Build package.obj
$ cc /optimize package.i
$!
$! Build record.obj
$ cc /optimize record.i
$!
$! Build sequence.obj
$ cc /optimize sequence.i
$!
$! Build charstrg.obj
$ cc /optimize charstrg.i
$!
$! Build debug.obj
$ cc /optimize debug.i
$!
$! Build misc.obj
$ cc /optimize misc.i
$!
$! Build predtype.obj
$ cc /optimize predtype.i
$!
$! Build symbol.obj
$ cc /optimize symbol.i
$!
$! Build lisparit.obj
$ cc /optimize lisparit.i
$!
$! Build arivax.s
$ cc /preprocess_only=arivax.s arivax.c
$!
$! Build arivax.obj
$ macro arivax.s
$!
$!
$!                      Target "make lisp.exe"
$!
$! ??
$ link /output=lisp.exe -
_$spvw.obj,spvwtabf.obj,spvwtabs.obj,spvwtabo.obj,-
_$eval.obj,control.obj,pathname.obj,stream.obj,socket.obj,io.obj,-
_$array.obj,hashtabl.obj,list.obj,package.obj,record.obj,sequence.obj,-
_$charstrg.obj,debug.obj,misc.obj,predtype.obj,symbol.obj,-
_$lisparit.obj,arivax.obj
$!
$!
$!                      Target "make alllsp"
$!
$! Build init.lsp
$ copy [.-.src]init.lsp init.lsp
$!
$! Build defseq.lsp
$ copy [.-.src]defseq.lsp defseq.lsp
$!
$! Build backquot.lsp
$ copy [.-.src]backquot.lsp backquot.lsp
$!
$! Build defmacro.lsp
$ copy [.-.src]defmacro.lsp defmacro.lsp
$!
$! Build macros1.lsp
$ copy [.-.src]macros1.lsp macros1.lsp
$!
$! Build macros2.lsp
$ copy [.-.src]macros2.lsp macros2.lsp
$!
$! Build defs1.lsp
$ copy [.-.src]defs1.lsp defs1.lsp
$!
$! Build places.lsp
$ copy [.-.src]places.lsp places.lsp
$!
$! Build floatpri.lsp
$ copy [.-.src]floatpri.lsp floatpri.lsp
$!
$! Build type.lsp
$ copy [.-.src]type.lsp type.lsp
$!
$! Build defstruc.lsp
$ copy [.-.src]defstruc.lsp defstruc.lsp
$!
$! Build format.lsp
$ copy [.-.src]format.lsp format.lsp
$!
$! Build user1.lsp
$ copy [.-.src]user1.lsp user1.lsp
$!
$! Build user2.lsp
$ copy [.-.src]user2.lsp user2.lsp
$!
$! Build trace.lsp
$ copy [.-.src]trace.lsp trace.lsp
$!
$! Build macros3.lsp
$ copy [.-.src]macros3.lsp macros3.lsp
$!
$! Build config.lsp
$ copy [.-.src]cfgvms.lsp config.lsp
$!
$! Build compiler.lsp
$ copy [.-.src]compiler.lsp compiler.lsp
$!
$! Build rexx.lsp
$ copy [.-.src]rexx.lsp rexx.lsp
$!
$! Build editor.lsp
$ copy [.-.src]editor.lsp editor.lsp
$!
$!
$!                      Target "make interpreted.mem"
$!
$! Build interpreted.mem
$ spawn /input=[.-.vms]interpreted.in /output=lisp.out /nolog run lisp.exe
$ rename lispinit.mem interpreted.mem
$!delete lisp.out;*
$!
$!
$!                      Target "make compiled.mem"
$!
$! Build init.fas
$ mc []lisp.exe -M interpreted.mem -q -c init.lsp
$!
$! Build defseq.fas
$ mc []lisp.exe -M interpreted.mem -q -c defseq.lsp
$!
$! Build backquot.fas
$ mc []lisp.exe -M interpreted.mem -q -c backquot.lsp
$!
$! Build defmacro.fas
$ mc []lisp.exe -M interpreted.mem -q -c defmacro.lsp
$!
$! Build macros1.fas
$ mc []lisp.exe -M interpreted.mem -q -c macros1.lsp
$!
$! Build macros2.fas
$ mc []lisp.exe -M interpreted.mem -q -c macros2.lsp
$!
$! Build defs1.fas
$ mc []lisp.exe -M interpreted.mem -q -c defs1.lsp
$!
$! Build places.fas
$ mc []lisp.exe -M interpreted.mem -q -c places.lsp
$!
$! Build floatpri.fas
$ mc []lisp.exe -M interpreted.mem -q -c floatpri.lsp
$!
$! Build type.fas
$ mc []lisp.exe -M interpreted.mem -q -c type.lsp
$!
$! Build defstruc.fas
$ mc []lisp.exe -M interpreted.mem -q -c defstruc.lsp
$!
$! Build format.fas
$ mc []lisp.exe -M interpreted.mem -q -c format.lsp
$!
$! Build user1.fas
$ mc []lisp.exe -M interpreted.mem -q -c user1.lsp
$!
$! Build user2.fas
$ mc []lisp.exe -M interpreted.mem -q -c user2.lsp
$!
$! Build trace.fas
$ mc []lisp.exe -M interpreted.mem -q -c trace.lsp
$!
$! Build macros3.fas
$ mc []lisp.exe -M interpreted.mem -q -c macros3.lsp
$!
$! Build config.fas
$ mc []lisp.exe -M interpreted.mem -q -c config.lsp
$!
$! Build compiler.fas
$ mc []lisp.exe -M interpreted.mem -q -c compiler.lsp
$!
$! Build rexx.fas
$ mc []lisp.exe -M interpreted.mem -q -c rexx.lsp
$!
$! Build editor.fas
$ mc []lisp.exe -M interpreted.mem -q -c editor.lsp
$!
$! Build compiled.mem
$ spawn /input=[.-.vms]compiled.in /output=lisp.out /nolog run lisp.exe
$ rename lispinit.mem compiled.mem
$!delete lisp.out;*
$!
$!
$!                      Target "make manual"
$!
$! Build ANNOUNCE
$ copy [.-]ANNOUNCE ANNOUNCE
$!
$! Build COPYRIGHT
$ copy [.-]COPYRIGHT COPYRIGHT
$!
$! Build GNU-GPL
$ copy [.-]GNU-GPL GNU-GPL
$!
$! Build SUMMARY
$ copy [.-]SUMMARY SUMMARY
$!
$! Build README
$ spawn /input=[.-.src]_README /output=txt.c /nolog run txt2c
$ cc txt.c
$ link txt
$ spawn /output=README /nolog run txt
$ delete txt.*;*
$!
$! Build impnotes.txt
$ spawn /input=[.-.src]_impnotes.txt /output=txt.c /nolog run txt2c
$ cc txt.c
$ link txt
$ spawn /output=impnotes.txt /nolog run txt
$ delete txt.*;*
$!
$! Build clisp.1
$ spawn /input=[.-.src]_clisp.1 /output=txt.c /nolog run txt2c
$ cc txt.c
$ link txt
$ spawn /output=clisp.1 /nolog run txt
$! delete empty lines from clisp.1. How??
$ delete txt.*;*
$!
#! Build clisp.man
$ spawn /input=clisp.1 /output=clisp.man #! nroff! How??
$!
$!
$!                      Target "make test"
$!
$ create /directory [.stage]
$ copy init.lsp [.stage]init.lsp
$ copy defseq.lsp [.stage]defseq.lsp
$ copy backquot.lsp [.stage]backquot.lsp
$ copy defmacro.lsp [.stage]defmacro.lsp
$ copy macros1.lsp [.stage]macros1.lsp
$ copy macros2.lsp [.stage]macros2.lsp
$ copy defs1.lsp [.stage]defs1.lsp
$ copy places.lsp [.stage]places.lsp
$ copy floatpri.lsp [.stage]floatpri.lsp
$ copy type.lsp [.stage]type.lsp
$ copy defstruc.lsp [.stage]defstruc.lsp
$ copy format.lsp [.stage]format.lsp
$ copy user1.lsp [.stage]user1.lsp
$ copy user2.lsp [.stage]user2.lsp
$ copy trace.lsp [.stage]trace.lsp
$ copy macros3.lsp [.stage]macros3.lsp
$ copy config.lsp [.stage]config.lsp
$ copy compiler.lsp [.stage]compiler.lsp
$ copy rexx.lsp [.stage]rexx.lsp
$ copy editor.lsp [.stage]editor.lsp
$ mc []lisp.exe -M compiled.mem -q -c [.stage]init.lsp
$ mc []lisp.exe -M compiled.mem -q -c [.stage]defseq.lsp
$ mc []lisp.exe -M compiled.mem -q -c [.stage]backquot.lsp
$ mc []lisp.exe -M compiled.mem -q -c [.stage]defmacro.lsp
$ mc []lisp.exe -M compiled.mem -q -c [.stage]macros1.lsp
$ mc []lisp.exe -M compiled.mem -q -c [.stage]macros2.lsp
$ mc []lisp.exe -M compiled.mem -q -c [.stage]defs1.lsp
$ mc []lisp.exe -M compiled.mem -q -c [.stage]places.lsp
$ mc []lisp.exe -M compiled.mem -q -c [.stage]floatpri.lsp
$ mc []lisp.exe -M compiled.mem -q -c [.stage]type.lsp
$ mc []lisp.exe -M compiled.mem -q -c [.stage]defstruc.lsp
$ mc []lisp.exe -M compiled.mem -q -c [.stage]format.lsp
$ mc []lisp.exe -M compiled.mem -q -c [.stage]user1.lsp
$ mc []lisp.exe -M compiled.mem -q -c [.stage]user2.lsp
$ mc []lisp.exe -M compiled.mem -q -c [.stage]trace.lsp
$ mc []lisp.exe -M compiled.mem -q -c [.stage]macros3.lsp
$ mc []lisp.exe -M compiled.mem -q -c [.stage]config.lsp
$ mc []lisp.exe -M compiled.mem -q -c [.stage]compiler.lsp
$ mc []lisp.exe -M compiled.mem -q -c [.stage]rexx.lsp
$ mc []lisp.exe -M compiled.mem -q -c [.stage]editor.lsp
$! How to compare files??
$ compare init.fas [.stage]init.fas
$ compare defseq.fas [.stage]defseq.fas
$ compare backquot.fas [.stage]backquot.fas
$ compare defmacro.fas [.stage]defmacro.fas
$ compare macros1.fas [.stage]macros1.fas
$ compare macros2.fas [.stage]macros2.fas
$ compare defs1.fas [.stage]defs1.fas
$ compare places.fas [.stage]places.fas
$ compare floatpri.fas [.stage]floatpri.fas
$ compare type.fas [.stage]type.fas
$ compare defstruc.fas [.stage]defstruc.fas
$ compare format.fas [.stage]format.fas
$ compare user1.fas [.stage]user1.fas
$ compare user2.fas [.stage]user2.fas
$ compare trace.fas [.stage]trace.fas
$ compare macros3.fas [.stage]macros3.fas
$ compare config.fas [.stage]config.fas
$ compare compiler.fas [.stage]compiler.fas
$ compare rexx.fas [.stage]rexx.fas
$!
$!
$!                      Target "make testsuite"
$!
$ create /directory [.suite]
$ copy [.-.tests]*.lsp [.suite]
$ copy [.-.tests]*.tst [.suite]
$ set default [.suite]
$ run [.-]lisp.exe -M [.-]compiled.mem -i tests.lsp -x "(run-all-tests)"
$! The test passed if no .erg files were created.
$ set default [.-]
$!
$!
$!                      That's it.
