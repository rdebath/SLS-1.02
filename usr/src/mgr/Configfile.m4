dnl{{{}}}
dnl{{{  m4 rules
changequote([,])dnl
define([check],[ifdef([$1],[define([$1],[changequote({,})]{$[]1}[changequote([,])])],[define([$1],)])dnl])dnl
dnl}}}
dnl{{{  checks
check([coherent])
check([linux])
check([vga])
check([hgc])
check([ega1])
check([ega2])
check([svga1])
check([svga2])
check([sunos])
check([movie])
check([hpux])
check([gropbm])
dnl}}}
dnl{{{  portable definitions
[VERSION=	0.54]

[INCLUDEDIR=	]WHEREHOME[/include]
[BINDIR=	]WHEREHOME[/bin$(ARCHITECTURE)]
[FONTDIR=	]WHEREHOME[/font]
[HFONTDIR=	]WHEREHOME[/hfont]
[ICONDIR=	]WHEREHOME[/icon]
[LIBDIR=	]WHEREHOME[/lib$(ARCHITECTURE)]
[MANDIR=	]WHEREHOME[/man]

[DEFAULT_FONT=	$(FONTDIR)/default]
[MOVIECLIENTS=	]movie([play_data])
[GROPBM=	]gropbm([gropbm])
[GROFFFONTDIR=	]WHEREGROFFFONT
dnl}}}
dnl{{{  linux
linux([
LEX=		flex
CC=		gcc
FLAGS=		-Wall -O2 -I$(INCLUDEDIR) ]movie([-DMOVIE])[

AWK=		gawk
ROFF=		groff
ROFFLAGS=	-Tascii
TBL=		tbl

MOUSE_DEV=	/dev/mouse
MGRFLAGS=	-DWHO -DVI -DBUCKEY -DKILL -DCUT -DALIGN -DMOUSE=]MOUSE[ -DTERMNAME=\"linux-mgr\"
BITBLIT=	linux

SERVER=		mgr
SERVER_PERM=	4755
]
dnl{{{  hercules flags
hgc([
SCREEN=		hgc
SCREEN_DEV=	720x348
])
dnl}}}
dnl{{{  vga flags
vga([
SCREEN=		vga
SCREEN_DEV=	640x480
])
dnl}}}
)
dnl}}}
dnl{{{  coherent
coherent([
SHELL=		/usr/bin/ksh
LEX=		lex
CC=		cc
EMULIB=		libcoh
LIBEMU=		$(LIBDIR)/libcoh.a
FLAGS=		-O -I$(INCLUDEDIR) -I$(INCLUDEDIR)/coherent ]movie([-DMOVIE])[

AWK=		awk
ROFF=		nroff
ROFFLAGS=
TBL=		

MOUSE_DEV=	/dev/mouse
MGRFLAGS=	-DVI -DBUCKEY -DKILL -DCUT -DALIGN ]mousesystems([-DMOUSESYSTEMS_MOUSE]) microsoft([-DMICROSOFT_MOUSE])[ -DTERMNAME=\"mgr\"
BITBLIT=	coherent

SERVER=		mgr
SERVER_PERM=	755
]
dnl{{{  hercules flags
hgc([
SCREEN=		hgc
SCREEN_DEV=	720x348
])
dnl}}}
dnl{{{  vga flags
vga([
SCREEN=		vga
SCREEN_DEV=	640x480
])
dnl}}}
dnl{{{  ega1 flags
ega1([
SCREEN=		vga
SCREEN_DEV=	640x350
])
dnl}}}
dnl{{{  ega2 flags
ega2([
SCREEN=		vga
SCREEN_DEV=	640x200
])
dnl}}}
dnl{{{  svga1 flags
svga1([
SCREEN=		vga
SCREEN_DEV=	800x600
])
dnl}}}
dnl{{{  svga2 flags
svga2([
SCREEN=		vga
SCREEN_DEV=	1024x768
])
dnl}}}
)
dnl}}}
dnl{{{  sunos
sunos([
LEX=		lex
LEXLIB=		-ll
CC=		gcc
FLAGS=		-Wall -O -I$(INCLUDEDIR) ]movie([-DMOVIE])[
CPP=		/usr/lib/cpp

AWK=		awk
ROFF=		nroff
TBL=		tbl

MOUSE_DEV=	/dev/mouse
MGRFLAGS=	-DVI -DBUCKEY -DKBD -DKILL -DCUT -DALIGN -DMOUSE=1 -DTERMNAME=\"mgr\"

BITBLIT=	sunmono
BLITLIBFLAGS=	-DUNROLL
BLITLIBOPT=	0

SCREEN=		sun
SCREEN_DEV=	/dev/fb

SERVER=		mgr
SERVER_PERM=	755
])
dnl}}}
dnl{{{  hp-ux
hpux([
LEX=		lex
LEXLIB=		-ll
CC=		gcc
FLAGS=		-Wall -O -I$(INCLUDEDIR) -Dsrandom=srand -Drandom=rand]movie([-DMOVIE])[
CPP=		/lib/cpp

AWK=		awk
ROFF=		nroff
TBL=		tbl

SERVER=
BITBLIT=
])
dnl}}}
