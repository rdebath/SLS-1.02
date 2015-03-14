#
# Makefile for the global data
#

include ../Makeconfig

include ../Makerules

ifndef OPTFLAGS
OPTFLAGS= -O6 -fomit-frame-pointer
endif

CFLAGS = $(WFLAGS) $(OPTFLAGS) $(INC) $(XCFLAGS)

DOSUBDIRS=lib

DIRS	=rpc

LIBC_DATA2S=rpc/rpc_comdata.o

LIBRPC_DATA1S=rpc/rpc_comdata.o

LIBS= libc.data2.o

lib:
	$(MAKE) dosubdirs DOSUBDIRS=library

dep:
	$(MAKE) dosubdirs DOSUBDIRS=dep

asm:
	$(MAKE) dosubdirs DOSUBDIRS=asm


jump: data

data: libc.data2.o # librpc.data1.o 

text: librpc.text1.o

libm.data2.o: $(LIBM_DATA2S)
	$(LD) -x -r -o $@ $?

libc.data2.o: $(LIBC_DATA2S)
	$(LD) -x -r -o $@ $?

librpc.data1.o: $(LIBRPC_DATA1S)
	$(LD) -x -r -o $@ $?

dosubdirs:
	for i in $(DIRS); do \
	  echo making $(DOSUBDIRS) in $$i; \
	  (cd $$i; $(MAKE) $(DOSUBDIRS)); \
	done

clean:
	$(RM) -f $(LIBS)
	for i in $(DIRS); \
	do \
		echo cleaning $$i; \
		(cd $$i; $(MAKE) clean); \
	done
