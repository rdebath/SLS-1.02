# Makefile for the CLISP binaries
# DO NOT EDIT! GENERATED AUTOMATICALLY!

# -------------- Start of configurable section --------------------

# Directories used by "make install":
prefix = 
exec_prefix = $(prefix)
bindir = $(exec_prefix)\bin
mandir = $(exec_prefix)\man
libdir = $(exec_prefix)\lib
lisplibdir = $(libdir)\lisp

# Programs used by "make":
CC = gcc
CFLAGS = -fomit-frame-pointer -O6 -DOS2
CPP = gcc -E
LIBS = -lreadline  -ltermcap -lvideo
LD = ld

RM = del
CP = copy
LN = copy
MV = ren
CAT = type
RANLIB = ranlib

# Programs used by "make install":
INSTALL = copy
INSTALL_PROGRAM = $(INSTALL)
INSTALL_DATA = $(INSTALL)

# Programs used by "make distrib":

# formatter for manual page
ROFF_MAN = nroff -man # try "groff -Tascii -mandoc" or "nroff -man"
ROFF_DVI =  # try "groff -Tdvi -mandoc"

# -------------- End of configurable section --------------------

COMMENT5 = comment5
ANSIDECL = ansidecl
TXT2C = txt2c

DFILES = \
spvw.d \
spvwtabf.d \
spvwtabs.d \
spvwtabo.d \
eval.d \
control.d \
pathname.d \
stream.d \
io.d \
array.d \
hashtabl.d \
list.d \
package.d \
record.d \
sequence.d \
charstrg.d \
debug.d \
misc.d \
predtype.d \
symbol.d \
lisparit.d \
lispbibl.d \
fsubr.d \
subr.d \
pseudofun.d \
constsym.d \
constobj.d \
msdos.d \
avl.d \
sort.d \
aridecl.d \
arilev0.d \
arilev1.d \
intelem.d \
intlog.d \
intplus.d \
intcomp.d \
intbyte.d \
intmal.d \
intdiv.d \
intgcd.d \
int2adic.d \
intsqrt.d \
intprint.d \
intread.d \
rational.d \
sfloat.d \
ffloat.d \
dfloat.d \
lfloat.d \
flo_konv.d \
flo_rest.d \
realelem.d \
realrand.d \
realtran.d \
compelem.d \
comptran.d \
arilev1c.d \
arilev1e.d \
arilev1i.d \
noreadline.d \
main.d \
ari80386.d

CFILES = \
spvw.c \
spvwtabf.c \
spvwtabs.c \
spvwtabo.c \
eval.c \
control.c \
pathname.c \
stream.c \
io.c \
array.c \
hashtabl.c \
list.c \
package.c \
record.c \
sequence.c \
charstrg.c \
debug.c \
misc.c \
predtype.c \
symbol.c \
lisparit.c \
lispbibl.c \
fsubr.c \
subr.c \
pseudofun.c \
constsym.c \
constobj.c \
msdos.c \
avl.c \
sort.c \
aridecl.c \
arilev0.c \
arilev1.c \
intelem.c \
intlog.c \
intplus.c \
intcomp.c \
intbyte.c \
intmal.c \
intdiv.c \
intgcd.c \
int2adic.c \
intsqrt.c \
intprint.c \
intread.c \
rational.c \
sfloat.c \
ffloat.c \
dfloat.c \
lfloat.c \
flo_konv.c \
flo_rest.c \
realelem.c \
realrand.c \
realtran.c \
compelem.c \
comptran.c \
arilev1c.c \
arilev1e.c \
arilev1i.c \
noreadline.c \
main.c \
ari80386.c

OBJECTS = \
spvw.o \
spvwtabf.o \
spvwtabs.o \
spvwtabo.o \
eval.o \
control.o \
pathname.o \
stream.o \
io.o \
array.o \
hashtabl.o \
list.o \
package.o \
record.o \
sequence.o \
charstrg.o \
debug.o \
misc.o \
predtype.o \
symbol.o \
lisparit.o \
ari80386.o

LSPFILES = \
init.lsp \
defseq.lsp \
backquot.lsp \
defmacro.lsp \
macros1.lsp \
macros2.lsp \
defs1.lsp \
places.lsp \
floatpri.lsp \
type.lsp \
defstruc.lsp \
format.lsp \
user1.lsp \
user2.lsp \
trace.lsp \
macros3.lsp \
config.lsp \
compiler.lsp \
rexx.lsp \
editor.lsp

FASFILES = \
init.fas \
defseq.fas \
backquot.fas \
defmacro.fas \
macros1.fas \
macros2.fas \
defs1.fas \
places.fas \
floatpri.fas \
type.fas \
defstruc.fas \
format.fas \
user1.fas \
user2.fas \
trace.fas \
macros3.fas \
config.fas \
compiler.fas \
rexx.fas \
editor.fas

TESTLSPFILES = \
stage/init.lsp \
stage/defseq.lsp \
stage/backquot.lsp \
stage/defmacro.lsp \
stage/macros1.lsp \
stage/macros2.lsp \
stage/defs1.lsp \
stage/places.lsp \
stage/floatpri.lsp \
stage/type.lsp \
stage/defstruc.lsp \
stage/format.lsp \
stage/user1.lsp \
stage/user2.lsp \
stage/trace.lsp \
stage/macros3.lsp \
stage/config.lsp \
stage/compiler.lsp \
stage/rexx.lsp \
stage/editor.lsp

TESTFASFILES = \
stage/init.fas \
stage/defseq.fas \
stage/backquot.fas \
stage/defmacro.fas \
stage/macros1.fas \
stage/macros2.fas \
stage/defs1.fas \
stage/places.fas \
stage/floatpri.fas \
stage/type.fas \
stage/defstruc.fas \
stage/format.fas \
stage/user1.fas \
stage/user2.fas \
stage/trace.fas \
stage/macros3.fas \
stage/config.fas \
stage/compiler.fas \
stage/rexx.fas \
stage/editor.fas


all : init allc allo lisp.exe interpreted.mem compiled.mem manual


init : comment5.exe ansidecl.exe txt2c.exe $(DFILES) errors.c

comment5.exe : ..\utils\comment5.c
	$(CC) $(CFLAGS) ../utils/comment5.c -o comment5.exe

ansidecl.exe : ..\utils\ansidecl.d comment5.exe
	$(LN) ..\utils\ansidecl.d ansidecl.d
	$(COMMENT5) ansidecl.d ansidecl.c
	$(RM) ansidecl.d
	$(CC) $(CFLAGS) ansidecl.c -o ansidecl.exe
	$(RM) ansidecl.c

txt2c.exe : ..\utils\txt2c.c
	$(CC) $(CFLAGS) ../utils/txt2c.c -o txt2c.exe

generrors.exe : ..\utils\generrors.c
	$(LN) ..\utils\generrors.c generrors.c
	$(CC) $(CFLAGS) generrors.c -o generrors.exe
	$(RM) generrors.c

errors.c : generrors.exe
	generrors > errors.c


allc : init $(CFILES)

spvw.c : spvw.d
	$(COMMENT5) spvw.d | $(ANSIDECL) > spvw.c

spvwtabf.c : spvwtabf.d
	$(COMMENT5) spvwtabf.d | $(ANSIDECL) > spvwtabf.c

spvwtabs.c : spvwtabs.d
	$(COMMENT5) spvwtabs.d | $(ANSIDECL) > spvwtabs.c

spvwtabo.c : spvwtabo.d
	$(COMMENT5) spvwtabo.d | $(ANSIDECL) > spvwtabo.c

eval.c : eval.d
	$(COMMENT5) eval.d | $(ANSIDECL) > eval.c

control.c : control.d
	$(COMMENT5) control.d | $(ANSIDECL) > control.c

pathname.c : pathname.d
	$(COMMENT5) pathname.d | $(ANSIDECL) > pathname.c

stream.c : stream.d
	$(COMMENT5) stream.d | $(ANSIDECL) > stream.c

io.c : io.d
	$(COMMENT5) io.d | $(ANSIDECL) > io.c

array.c : array.d
	$(COMMENT5) array.d | $(ANSIDECL) > array.c

hashtabl.c : hashtabl.d
	$(COMMENT5) hashtabl.d | $(ANSIDECL) > hashtabl.c

list.c : list.d
	$(COMMENT5) list.d | $(ANSIDECL) > list.c

package.c : package.d
	$(COMMENT5) package.d | $(ANSIDECL) > package.c

record.c : record.d
	$(COMMENT5) record.d | $(ANSIDECL) > record.c

sequence.c : sequence.d
	$(COMMENT5) sequence.d | $(ANSIDECL) > sequence.c

charstrg.c : charstrg.d
	$(COMMENT5) charstrg.d | $(ANSIDECL) > charstrg.c

debug.c : debug.d
	$(COMMENT5) debug.d | $(ANSIDECL) > debug.c

misc.c : misc.d
	$(COMMENT5) misc.d | $(ANSIDECL) > misc.c

predtype.c : predtype.d
	$(COMMENT5) predtype.d | $(ANSIDECL) > predtype.c

symbol.c : symbol.d
	$(COMMENT5) symbol.d | $(ANSIDECL) > symbol.c

lisparit.c : lisparit.d
	$(COMMENT5) lisparit.d | $(ANSIDECL) > lisparit.c

lispbibl.c : lispbibl.d
	$(COMMENT5) lispbibl.d | $(ANSIDECL) > lispbibl.c

fsubr.c : fsubr.d
	$(COMMENT5) fsubr.d | $(ANSIDECL) > fsubr.c

subr.c : subr.d
	$(COMMENT5) subr.d | $(ANSIDECL) > subr.c

pseudofun.c : pseudofun.d
	$(COMMENT5) pseudofun.d | $(ANSIDECL) > pseudofun.c

constsym.c : constsym.d
	$(COMMENT5) constsym.d | $(ANSIDECL) > constsym.c

constobj.c : constobj.d
	$(COMMENT5) constobj.d | $(ANSIDECL) > constobj.c

msdos.c : msdos.d
	$(COMMENT5) msdos.d | $(ANSIDECL) > msdos.c

avl.c : avl.d
	$(COMMENT5) avl.d | $(ANSIDECL) > avl.c

sort.c : sort.d
	$(COMMENT5) sort.d | $(ANSIDECL) > sort.c

aridecl.c : aridecl.d
	$(COMMENT5) aridecl.d | $(ANSIDECL) > aridecl.c

arilev0.c : arilev0.d
	$(COMMENT5) arilev0.d | $(ANSIDECL) > arilev0.c

arilev1.c : arilev1.d
	$(COMMENT5) arilev1.d | $(ANSIDECL) > arilev1.c

intelem.c : intelem.d
	$(COMMENT5) intelem.d | $(ANSIDECL) > intelem.c

intlog.c : intlog.d
	$(COMMENT5) intlog.d | $(ANSIDECL) > intlog.c

intplus.c : intplus.d
	$(COMMENT5) intplus.d | $(ANSIDECL) > intplus.c

intcomp.c : intcomp.d
	$(COMMENT5) intcomp.d | $(ANSIDECL) > intcomp.c

intbyte.c : intbyte.d
	$(COMMENT5) intbyte.d | $(ANSIDECL) > intbyte.c

intmal.c : intmal.d
	$(COMMENT5) intmal.d | $(ANSIDECL) > intmal.c

intdiv.c : intdiv.d
	$(COMMENT5) intdiv.d | $(ANSIDECL) > intdiv.c

intgcd.c : intgcd.d
	$(COMMENT5) intgcd.d | $(ANSIDECL) > intgcd.c

int2adic.c : int2adic.d
	$(COMMENT5) int2adic.d | $(ANSIDECL) > int2adic.c

intsqrt.c : intsqrt.d
	$(COMMENT5) intsqrt.d | $(ANSIDECL) > intsqrt.c

intprint.c : intprint.d
	$(COMMENT5) intprint.d | $(ANSIDECL) > intprint.c

intread.c : intread.d
	$(COMMENT5) intread.d | $(ANSIDECL) > intread.c

rational.c : rational.d
	$(COMMENT5) rational.d | $(ANSIDECL) > rational.c

sfloat.c : sfloat.d
	$(COMMENT5) sfloat.d | $(ANSIDECL) > sfloat.c

ffloat.c : ffloat.d
	$(COMMENT5) ffloat.d | $(ANSIDECL) > ffloat.c

dfloat.c : dfloat.d
	$(COMMENT5) dfloat.d | $(ANSIDECL) > dfloat.c

lfloat.c : lfloat.d
	$(COMMENT5) lfloat.d | $(ANSIDECL) > lfloat.c

flo_konv.c : flo_konv.d
	$(COMMENT5) flo_konv.d | $(ANSIDECL) > flo_konv.c

flo_rest.c : flo_rest.d
	$(COMMENT5) flo_rest.d | $(ANSIDECL) > flo_rest.c

realelem.c : realelem.d
	$(COMMENT5) realelem.d | $(ANSIDECL) > realelem.c

realrand.c : realrand.d
	$(COMMENT5) realrand.d | $(ANSIDECL) > realrand.c

realtran.c : realtran.d
	$(COMMENT5) realtran.d | $(ANSIDECL) > realtran.c

compelem.c : compelem.d
	$(COMMENT5) compelem.d | $(ANSIDECL) > compelem.c

comptran.c : comptran.d
	$(COMMENT5) comptran.d | $(ANSIDECL) > comptran.c

arilev1c.c : arilev1c.d
	$(COMMENT5) arilev1c.d | $(ANSIDECL) > arilev1c.c

arilev1e.c : arilev1e.d
	$(COMMENT5) arilev1e.d | $(ANSIDECL) > arilev1e.c

arilev1i.c : arilev1i.d
	$(COMMENT5) arilev1i.d | $(ANSIDECL) > arilev1i.c

noreadline.c : noreadline.d
	$(COMMENT5) noreadline.d | $(ANSIDECL) > noreadline.c

main.c : main.d
	$(COMMENT5) main.d | $(ANSIDECL) > main.c

ari80386.c : ari80386.d
	$(COMMENT5) ari80386.d > ari80386.c

ari80386.s : ari80386.c
	$(CPP) -DUNDERSCORE ari80386.c > ari80386.s

allo : allc spvw.o spvwtabf.o spvwtabs.o spvwtabo.o eval.o control.o pathname.o stream.o io.o array.o hashtabl.o list.o package.o record.o sequence.o charstrg.o debug.o misc.o predtype.o symbol.o lisparit.o

alls : allc spvw.s spvwtabf.s spvwtabs.s spvwtabo.s eval.s control.s pathname.s stream.s io.s array.s hashtabl.s list.s package.s record.s sequence.s charstrg.s debug.s misc.s predtype.s symbol.s lisparit.s

spvw.i : spvw.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c avl.c sort.c
	$(CPP) $(CFLAGS) -USINGLE_PASS_COMPILER spvw.c > spvw.i

spvw.s : spvw.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c avl.c sort.c
	$(CC) $(CFLAGS) -S spvw.c

spvw.o : spvw.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c avl.c sort.c
	$(CC) $(CFLAGS) -c spvw.c

spvwtabf.i : spvwtabf.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CPP) $(CFLAGS) -USINGLE_PASS_COMPILER spvwtabf.c > spvwtabf.i

spvwtabf.s : spvwtabf.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CC) $(CFLAGS) -S spvwtabf.c

spvwtabf.o : spvwtabf.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CC) $(CFLAGS) -c spvwtabf.c

spvwtabs.i : spvwtabs.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CPP) $(CFLAGS) -USINGLE_PASS_COMPILER spvwtabs.c > spvwtabs.i

spvwtabs.s : spvwtabs.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CC) $(CFLAGS) -S spvwtabs.c

spvwtabs.o : spvwtabs.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CC) $(CFLAGS) -c spvwtabs.c

spvwtabo.i : spvwtabo.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CPP) $(CFLAGS) -USINGLE_PASS_COMPILER spvwtabo.c > spvwtabo.i

spvwtabo.s : spvwtabo.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CC) $(CFLAGS) -S spvwtabo.c

spvwtabo.o : spvwtabo.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CC) $(CFLAGS) -c spvwtabo.c

eval.i : eval.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CPP) $(CFLAGS) -USINGLE_PASS_COMPILER eval.c > eval.i

eval.s : eval.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CC) $(CFLAGS) -S eval.c

eval.o : eval.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CC) $(CFLAGS) -c eval.c

control.i : control.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CPP) $(CFLAGS) -USINGLE_PASS_COMPILER control.c > control.i

control.s : control.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CC) $(CFLAGS) -S control.c

control.o : control.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CC) $(CFLAGS) -c control.c

pathname.i : pathname.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CPP) $(CFLAGS) -USINGLE_PASS_COMPILER pathname.c > pathname.i

pathname.s : pathname.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CC) $(CFLAGS) -S pathname.c

pathname.o : pathname.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CC) $(CFLAGS) -c pathname.c

stream.i : stream.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CPP) $(CFLAGS) -Ireadline -USINGLE_PASS_COMPILER stream.c > stream.i

stream.s : stream.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CC) $(CFLAGS) -Ireadline -S stream.c

stream.o : stream.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CC) $(CFLAGS) -Ireadline -c stream.c

io.i : io.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c arilev0.c
	$(CPP) $(CFLAGS) -USINGLE_PASS_COMPILER io.c > io.i

io.s : io.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c arilev0.c
	$(CC) $(CFLAGS) -S io.c

io.o : io.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c arilev0.c
	$(CC) $(CFLAGS) -c io.c

array.i : array.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c arilev0.c
	$(CPP) $(CFLAGS) -USINGLE_PASS_COMPILER array.c > array.i

array.s : array.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c arilev0.c
	$(CC) $(CFLAGS) -S array.c

array.o : array.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c arilev0.c
	$(CC) $(CFLAGS) -c array.c

hashtabl.i : hashtabl.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c arilev0.c aridecl.c
	$(CPP) $(CFLAGS) -USINGLE_PASS_COMPILER hashtabl.c > hashtabl.i

hashtabl.s : hashtabl.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c arilev0.c aridecl.c
	$(CC) $(CFLAGS) -S hashtabl.c

hashtabl.o : hashtabl.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c arilev0.c aridecl.c
	$(CC) $(CFLAGS) -c hashtabl.c

list.i : list.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CPP) $(CFLAGS) -USINGLE_PASS_COMPILER list.c > list.i

list.s : list.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CC) $(CFLAGS) -S list.c

list.o : list.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CC) $(CFLAGS) -c list.c

package.i : package.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c arilev0.c
	$(CPP) $(CFLAGS) -USINGLE_PASS_COMPILER package.c > package.i

package.s : package.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c arilev0.c
	$(CC) $(CFLAGS) -S package.c

package.o : package.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c arilev0.c
	$(CC) $(CFLAGS) -c package.c

record.i : record.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CPP) $(CFLAGS) -USINGLE_PASS_COMPILER record.c > record.i

record.s : record.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CC) $(CFLAGS) -S record.c

record.o : record.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CC) $(CFLAGS) -c record.c

sequence.i : sequence.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CPP) $(CFLAGS) -USINGLE_PASS_COMPILER sequence.c > sequence.i

sequence.s : sequence.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CC) $(CFLAGS) -S sequence.c

sequence.o : sequence.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CC) $(CFLAGS) -c sequence.c

charstrg.i : charstrg.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CPP) $(CFLAGS) -USINGLE_PASS_COMPILER charstrg.c > charstrg.i

charstrg.s : charstrg.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CC) $(CFLAGS) -S charstrg.c

charstrg.o : charstrg.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CC) $(CFLAGS) -c charstrg.c

debug.i : debug.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CPP) $(CFLAGS) -USINGLE_PASS_COMPILER debug.c > debug.i

debug.s : debug.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CC) $(CFLAGS) -S debug.c

debug.o : debug.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CC) $(CFLAGS) -c debug.c

misc.i : misc.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c arilev0.c errors.c
	$(CPP) $(CFLAGS) -USINGLE_PASS_COMPILER misc.c > misc.i

misc.s : misc.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c arilev0.c errors.c
	$(CC) $(CFLAGS) -S misc.c

misc.o : misc.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c arilev0.c errors.c
	$(CC) $(CFLAGS) -c misc.c

predtype.i : predtype.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CPP) $(CFLAGS) -USINGLE_PASS_COMPILER predtype.c > predtype.i

predtype.s : predtype.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CC) $(CFLAGS) -S predtype.c

predtype.o : predtype.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CC) $(CFLAGS) -c predtype.c

symbol.i : symbol.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CPP) $(CFLAGS) -USINGLE_PASS_COMPILER symbol.c > symbol.i

symbol.s : symbol.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CC) $(CFLAGS) -S symbol.c

symbol.o : symbol.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CC) $(CFLAGS) -c symbol.c

lisparit.i : lisparit.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c aridecl.c arilev0.c arilev1.c intelem.c intlog.c intplus.c intcomp.c intbyte.c intmal.c intdiv.c intgcd.c int2adic.c intsqrt.c intprint.c intread.c rational.c sfloat.c ffloat.c dfloat.c lfloat.c flo_konv.c flo_rest.c realelem.c realrand.c realtran.c compelem.c comptran.c arilev1c.c arilev1e.c arilev1i.c
	$(CPP) $(CFLAGS) -USINGLE_PASS_COMPILER lisparit.c > lisparit.i

lisparit.s : lisparit.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c aridecl.c arilev0.c arilev1.c intelem.c intlog.c intplus.c intcomp.c intbyte.c intmal.c intdiv.c intgcd.c int2adic.c intsqrt.c intprint.c intread.c rational.c sfloat.c ffloat.c dfloat.c lfloat.c flo_konv.c flo_rest.c realelem.c realrand.c realtran.c compelem.c comptran.c arilev1c.c arilev1e.c arilev1i.c
	$(CC) $(CFLAGS) -S lisparit.c

lisparit.o : lisparit.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c aridecl.c arilev0.c arilev1.c intelem.c intlog.c intplus.c intcomp.c intbyte.c intmal.c intdiv.c intgcd.c int2adic.c intsqrt.c intprint.c intread.c rational.c sfloat.c ffloat.c dfloat.c lfloat.c flo_konv.c flo_rest.c realelem.c realrand.c realtran.c compelem.c comptran.c arilev1c.c arilev1e.c arilev1i.c
	$(CC) $(CFLAGS) -c lisparit.c

noreadline.i : noreadline.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CPP) $(CFLAGS) -Ireadline -USINGLE_PASS_COMPILER noreadline.c > noreadline.i

noreadline.s : noreadline.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CC) $(CFLAGS) -Ireadline -S noreadline.c

noreadline.o : noreadline.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CC) $(CFLAGS) -Ireadline -c noreadline.c

main.i : main.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CPP) $(CFLAGS) -USINGLE_PASS_COMPILER main.c > main.i

main.s : main.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CC) $(CFLAGS) -S main.c

main.o : main.c lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(CC) $(CFLAGS) -c main.c

ari80386.o : ari80386.s
	$(CC) $(CFLAGS) -c ari80386.s

lisp.exe : $(OBJECTS) readline/libreadline.a
	$(RM) lisp
	$(RM) lisp.exe
	$(CC) $(CFLAGS) -o lisp @gcclink.in  -Lreadline $(LIBS)
	emxbind -q c:\emx\bin\emx.exe lisp lisp.exe -p -acm


alllsp : $(LSPFILES)

config.lsp : cfgdos.lsp
	$(CP) cfgdos.lsp config.lsp

interpreted.mem : lisp.exe $(LSPFILES)
	lisp.exe < interpreted.in
	-$(RM) interpreted.mem
	$(MV) lispinit.mem interpreted.mem

init.fas : init.lsp lisp.exe interpreted.mem
	lisp.exe -M interpreted.mem -q -c init.lsp

defseq.fas : defseq.lsp lisp.exe interpreted.mem
	lisp.exe -M interpreted.mem -q -c defseq.lsp

backquot.fas : backquot.lsp lisp.exe interpreted.mem
	lisp.exe -M interpreted.mem -q -c backquot.lsp

defmacro.fas : defmacro.lsp lisp.exe interpreted.mem
	lisp.exe -M interpreted.mem -q -c defmacro.lsp

macros1.fas : macros1.lsp lisp.exe interpreted.mem
	lisp.exe -M interpreted.mem -q -c macros1.lsp

macros2.fas : macros2.lsp lisp.exe interpreted.mem
	lisp.exe -M interpreted.mem -q -c macros2.lsp

defs1.fas : defs1.lsp lisp.exe interpreted.mem
	lisp.exe -M interpreted.mem -q -c defs1.lsp

places.fas : places.lsp lisp.exe interpreted.mem
	lisp.exe -M interpreted.mem -q -c places.lsp

floatpri.fas : floatpri.lsp lisp.exe interpreted.mem
	lisp.exe -M interpreted.mem -q -c floatpri.lsp

type.fas : type.lsp lisp.exe interpreted.mem
	lisp.exe -M interpreted.mem -q -c type.lsp

defstruc.fas : defstruc.lsp lisp.exe interpreted.mem
	lisp.exe -M interpreted.mem -q -c defstruc.lsp

format.fas : format.lsp lisp.exe interpreted.mem
	lisp.exe -M interpreted.mem -q -c format.lsp

user1.fas : user1.lsp lisp.exe interpreted.mem
	lisp.exe -M interpreted.mem -q -c user1.lsp

user2.fas : user2.lsp lisp.exe interpreted.mem
	lisp.exe -M interpreted.mem -q -c user2.lsp

trace.fas : trace.lsp lisp.exe interpreted.mem
	lisp.exe -M interpreted.mem -q -c trace.lsp

macros3.fas : macros3.lsp lisp.exe interpreted.mem
	lisp.exe -M interpreted.mem -q -c macros3.lsp

config.fas : config.lsp lisp.exe interpreted.mem
	lisp.exe -M interpreted.mem -q -c config.lsp

compiler.fas : compiler.lsp lisp.exe interpreted.mem
	lisp.exe -M interpreted.mem -q -c compiler.lsp

rexx.fas : rexx.lsp lisp.exe interpreted.mem
	lisp.exe -M interpreted.mem -q -c rexx.lsp

editor.fas : editor.lsp lisp.exe interpreted.mem
	lisp.exe -M interpreted.mem -q -c editor.lsp

compiled.mem : lisp.exe $(FASFILES)
	lisp.exe < compiled.in
	-$(RM) compiled.mem
	$(MV) lispinit.mem compiled.mem


# Test: recompile $(LSPFILES) and check if the resulting .mem file has the same length.
test : compiled.mem stage $(TESTLSPFILES) $(TESTFASFILES)
	-$(RM) stage\editor.*
	comp *.fas stage
	REM The test passed if no differences were found.

stage :
	-mkdir stage

stage/init.lsp : init.lsp
	$(LN) init.lsp stage

stage/defseq.lsp : defseq.lsp
	$(LN) defseq.lsp stage

stage/backquot.lsp : backquot.lsp
	$(LN) backquot.lsp stage

stage/defmacro.lsp : defmacro.lsp
	$(LN) defmacro.lsp stage

stage/macros1.lsp : macros1.lsp
	$(LN) macros1.lsp stage

stage/macros2.lsp : macros2.lsp
	$(LN) macros2.lsp stage

stage/defs1.lsp : defs1.lsp
	$(LN) defs1.lsp stage

stage/places.lsp : places.lsp
	$(LN) places.lsp stage

stage/floatpri.lsp : floatpri.lsp
	$(LN) floatpri.lsp stage

stage/type.lsp : type.lsp
	$(LN) type.lsp stage

stage/defstruc.lsp : defstruc.lsp
	$(LN) defstruc.lsp stage

stage/format.lsp : format.lsp
	$(LN) format.lsp stage

stage/user1.lsp : user1.lsp
	$(LN) user1.lsp stage

stage/user2.lsp : user2.lsp
	$(LN) user2.lsp stage

stage/trace.lsp : trace.lsp
	$(LN) trace.lsp stage

stage/macros3.lsp : macros3.lsp
	$(LN) macros3.lsp stage

stage/config.lsp : config.lsp
	$(LN) config.lsp stage

stage/compiler.lsp : compiler.lsp
	$(LN) compiler.lsp stage

stage/rexx.lsp : rexx.lsp
	$(LN) rexx.lsp stage

stage/editor.lsp : editor.lsp
	$(LN) editor.lsp stage

stage/init.fas : stage/init.lsp lisp.exe compiled.mem
	lisp.exe -M compiled.mem -q -c stage/init.lsp

stage/defseq.fas : stage/defseq.lsp lisp.exe compiled.mem
	lisp.exe -M compiled.mem -q -c stage/defseq.lsp

stage/backquot.fas : stage/backquot.lsp lisp.exe compiled.mem
	lisp.exe -M compiled.mem -q -c stage/backquot.lsp

stage/defmacro.fas : stage/defmacro.lsp lisp.exe compiled.mem
	lisp.exe -M compiled.mem -q -c stage/defmacro.lsp

stage/macros1.fas : stage/macros1.lsp lisp.exe compiled.mem
	lisp.exe -M compiled.mem -q -c stage/macros1.lsp

stage/macros2.fas : stage/macros2.lsp lisp.exe compiled.mem
	lisp.exe -M compiled.mem -q -c stage/macros2.lsp

stage/defs1.fas : stage/defs1.lsp lisp.exe compiled.mem
	lisp.exe -M compiled.mem -q -c stage/defs1.lsp

stage/places.fas : stage/places.lsp lisp.exe compiled.mem
	lisp.exe -M compiled.mem -q -c stage/places.lsp

stage/floatpri.fas : stage/floatpri.lsp lisp.exe compiled.mem
	lisp.exe -M compiled.mem -q -c stage/floatpri.lsp

stage/type.fas : stage/type.lsp lisp.exe compiled.mem
	lisp.exe -M compiled.mem -q -c stage/type.lsp

stage/defstruc.fas : stage/defstruc.lsp lisp.exe compiled.mem
	lisp.exe -M compiled.mem -q -c stage/defstruc.lsp

stage/format.fas : stage/format.lsp lisp.exe compiled.mem
	lisp.exe -M compiled.mem -q -c stage/format.lsp

stage/user1.fas : stage/user1.lsp lisp.exe compiled.mem
	lisp.exe -M compiled.mem -q -c stage/user1.lsp

stage/user2.fas : stage/user2.lsp lisp.exe compiled.mem
	lisp.exe -M compiled.mem -q -c stage/user2.lsp

stage/trace.fas : stage/trace.lsp lisp.exe compiled.mem
	lisp.exe -M compiled.mem -q -c stage/trace.lsp

stage/macros3.fas : stage/macros3.lsp lisp.exe compiled.mem
	lisp.exe -M compiled.mem -q -c stage/macros3.lsp

stage/config.fas : stage/config.lsp lisp.exe compiled.mem
	lisp.exe -M compiled.mem -q -c stage/config.lsp

stage/compiler.fas : stage/compiler.lsp lisp.exe compiled.mem
	lisp.exe -M compiled.mem -q -c stage/compiler.lsp

stage/rexx.fas : stage/rexx.lsp lisp.exe compiled.mem
	lisp.exe -M compiled.mem -q -c stage/rexx.lsp

stage/editor.fas : stage/editor.lsp lisp.exe compiled.mem
	lisp.exe -M compiled.mem -q -c stage/editor.lsp

compiled2.mem : lisp.exe $(TESTFASFILES)
	lisp.exe -x "(cd \"stage/\") (load \"init.fas\") (cd \"..\\\") (saveinitmem) (exit)"
	-$(RM) compiled2.mem
	$(MV) lispinit.mem compiled2.mem


manual : ANNOUNCE COPYRIGHT GNU-GPL SUMMARY README clisp.1 impnotes.txt

manualx : manual clisp.man

ANNOUNCE : ../ANNOUNCE
	-$(LN) ..\ANNOUNCE ANNOUNCE

COPYRIGHT : ../COPYRIGHT
	-$(LN) ..\COPYRIGHT COPYRIGHT

GNU-GPL : ../GNU-GPL
	-$(LN) ..\GNU-GPL GNU-GPL

SUMMARY : ../SUMMARY
	-$(LN) ..\SUMMARY SUMMARY

README : _README lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(TXT2C) < _README > txt.c
	$(CC) $(CFLAGS) txt.c -o txt.exe
	txt > README
	$(RM) txt.c
	$(RM) txt.i
	$(RM) txt.exe

clisp.1 : _clisp.1 lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(TXT2C) < _clisp.1 > txt.c
	$(CC) $(CFLAGS) txt.c -o txt.exe
	txt | grep -v -f grep.in > clisp.1
	$(RM) txt.c
	$(RM) txt.i
	$(RM) txt.exe

impnotes.txt : _impnotes.txt lispbibl.c fsubr.c subr.c pseudofun.c constsym.c constobj.c msdos.c
	$(TXT2C) < _impnotes.txt > txt.c
	$(CC) $(CFLAGS) txt.c -o txt.exe
	txt > impnotes.txt
	$(RM) txt.c
	$(RM) txt.i
	$(RM) txt.exe

clisp.man : clisp.1
	$(ROFF_MAN) clisp.1 > clisp.man

clisp.dvi : clisp.1
	$(ROFF_DVI) clisp.1 > clisp.dvi


install : install-bin

install-bin : lisp.exe compiled.mem force
	-mkdir $(libdir)
	-mkdir $(lisplibdir)
	$(INSTALL_PROGRAM) lisp.exe $(lisplibdir)\lisp.exe
	$(INSTALL_DATA) compiled.mem $(lisplibdir)\lispinit.mem
	-mkdir $(bindir)
	echo $(lisplibdir)\lisp.exe -M $(lisplibdir)\lispinit.mem %1 %2 %3 %4 %5 %6 %7 %8 %9 > $(bindir)\clisp.bat

# clean0 is harmless: removes only superfluous things: temporary files.
clean0 : force
	-$(RM) core ansidecl.c generrors.c lisparit.tmp txt.c txt.i txt.exe

# clean1 removes everything that becomes obsolete once lisp.run is recompiled
# without changing the bytecode format and the tables in
# constobj.d, constsym.d, subr.d, fsubr.d, pseudofun.d.
clean1 : clean0
	-$(RM) clisp.h *.i *.s *.o *.a lisp.exe stage/* suite/*
	-rmdir stage
	-rmdir suite
	-$(RM) README clisp.1 impnotes.txt

# clean2 removes everything that becomes obsolete once lisp.run is recompiled
# without changing the bytecode format.
clean2 : clean1
	-$(RM) interpreted.mem compiled.mem compiled2.mem

# clean3 removes everything that becomes obsolete once lisp.run is recompiled.
# It leaves everything generated by "make allc".
clean3 : clean2
	-$(RM) $(FASFILES) *.lib

# clean4 removes everything that becomes obsolete
# when switching to a new version of the C compiler.
# It leaves everything generated by "make init".
clean4 : clean3
	-$(RM) $(CFILES)

# clean5 even undoes "make init".
clean5 : clean4
	-$(RM) config.lsp
	-$(RM) comment5.exe ansidecl.exe ccpaux.exe deelif.exe deerror.exe traddecl.exe mergestrings.exe txt2c.exe
	-$(RM) generrors.exe errors.c

# clean6 lets us go back to "makemake > makefile".
clean6 : clean5
	-$(RM) makefile

# clean7 lets us go back to "target".
clean7 : clean6
	-$(RM) config.status machine.h unixconf.h makemake
	-$(RM) readline.tar.Z
	cd readline ; make clean

# clean8 moreover cleans up the SRCDIR.
clean8 : clean7
	-$(RM) lisparit0.d

# The normal "start over" command:
clean : clean4

# Make "clear for distribution":
distclean : clean7

# Remove everything that can be generated otherwise:
realclean : clean8


force:

