#
# Makefile for crt0.o of Linux
#
#

include ../Makeconfig

ifndef OPTFLAGS
OPTFLAGS= -O6 -fomit-frame-pointer
endif

CFLAGS = $(WFLAGS) $(OPTFLAGS) $(INC) $(XCFLAGS)

ifdef PROFILE

OBJS  = gcrt0.o

TARGET=profile

else

OBJS  = crt0.o

TARGET=normal

endif

.SUFFIXES:
.SUFFIXES: .S .o .c .s

.s.o :
	$(CC) $(CFLAGS) -c $<
#	$(LD) $(LDFLAGS) $*.o
#	$(MV) a.out $*.o

.S.o :
	$(CC) $(CFLAGS) -c $<
#	$(LD) $(LDFLAGS) $*.o
#	$(MV) a.out $*.o

.c.o :
	$(CC) $(CFLAGS) -c $<
#	$(LD) $(LDFLAGS) $*.o
#	$(MV) a.out $*.o

.c.s :
	$(CC) $(CFLAGS) -S $<

all: $(OBJS)


install: $(OBJS) $(TARGET)

normal:
	cp $(OBJS) $(TARGET_LIB_DIR)

profile:
	cp $(OBJS) $(TARGET_LIBEXTRA_DIR)

clean:
	$(RM) -f core *.o *.a tmp_make foo
	-for i in *.c;do \
	  if [ -f $$i ]; then \
	    $(RM) -f `basename $$i .c`.s; \
	  fi; \
	done

dep:
	@echo No dependency in ./crt.
