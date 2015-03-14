CC = gcc
CFLAGS = -O2 -include ../bsd/bsd.h -I../bsd
BSDOBJS = daemon.o logwtmp.o logout.o pty.o login_tty.o # syslog.o hostalias.o 
GNUOBJS = # mkstemp.o
OBJS = $(BSDOBJS) $(GNUOBJS) \
	sgtty.o cfsetspeed.o revoke.o \
	usershell.o setpassent.o \
	snprintf.o roundup.o checkrhosts.o \
	fsync.o
# strsep.o p_class.o fp_query.o p_time.o
# p_type.o dn_skipname.o seteuid.o setegid.o utimes.o
LIB = libbsd.a

all: $(LIB)

$(LIB): $(OBJS)
	ar rcs $(LIB) $(OBJS)

clean:
	rm -f *.o *~

clobber: clean
	rm -f $(LIB)

