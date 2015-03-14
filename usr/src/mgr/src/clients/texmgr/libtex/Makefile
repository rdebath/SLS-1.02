include ../../../../Configfile
include ../Configfile

CFLAGS=		$(FLAGS) -DFONTDESC=\"$(FONTDESC)\"

OBJS=		conv.o dviclass.o error.o findpost.o fio.o font.o font_subr.o \
		gfclass.o gripes.o magfactor.o rotate.o scaletfm.o scanpost.o \
		search.o seek.o split.o strsave.o tfm.o gffont.o pkfont.o \
		pxlfont.o tfmfont.o dvi_draw.o dvistuff.o

all:		libtex.a

libtex.a:	$(OBJS)
		rm -f $@
		ar cq $@ $(OBJS)
		ranlib libtex.a
                
clean:
		rm -f $(OBJS)
                
clobber:	clean
		rm -f libtex.a
