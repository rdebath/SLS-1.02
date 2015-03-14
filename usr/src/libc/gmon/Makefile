#
# This is Makefile of gmon for Linux
#
#

include ../Makeconfig

LIB=../libgmon.a

PGCFLAGS = $(WFLAGS) -O6 $(INC) $(XCFLAGS)

.SUFFIXES:
.SUFFIXES: .S .o .c .s .i

.s.o :
	$(CC) $(PGCFLAGS) -c $<
	$(LD) $(LDFLAGS) $*.o
	$(MV) a.out $*.o

.S.o :
	$(CC) $(PGCFLAGS) -c $<
	$(LD) $(LDFLAGS) $*.o
	$(MV) a.out $*.o

.c.o :
	$(CC) $(PGCFLAGS) -c $<
	$(LD) $(LDFLAGS) $*.o
	$(MV) a.out $*.o

.c.s :
	$(CC) $(PGCFLAGS) -S $<

.c.i :
	$(CC) $(PGCFLAGS) -E $< -o $*.i


OBJS = gmon.o profil.o

library: $(OBJS)
	$(AR) uvc $(LIB) $(OBJS)


clean:
	$(RM) -f core *.o *.a tmp_make foo
	-for i in *.c;do \
	  if [ -f $$i ]; then \
	    $(RM) -f `basename $$i .c`.s; \
	  fi; \
	done

dep:
	sed '/\#\#\# Dependencies/q' < Makefile > tmp_make
	(for i in *.c;do echo -n `echo $$i | sed 's,\.c,\.s,'`" "; \
	$(CPP) -M $$i;done) >> tmp_make
	cp tmp_make Makefile

### Dependencies:
