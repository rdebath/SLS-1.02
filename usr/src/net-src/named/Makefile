CC = gcc
LDFLAGS =
DEFINES = -include ../bsd/bsd.h -I../bsd
STDDEF = -DDEBUG -DSTATS
CFLAGS = -O2 $(STDDEF) $(DEFINES)
OBJS =	db_dump.o db_load.o db_lookup.o db_reload.o db_save.o db_update.o \
	ns_forw.o ns_init.o ns_main.o ns_maint.o ns_req.o ns_resp.o \
	ns_sort.o ns_stats.o db_glue.o version.o
LDLIBS = ../libbsd/libbsd.a
SUBDIRS = xfer tools

all: named subdirs

subdirs:
	@for i in $(SUBDIRS); do (cd $$i && echo $$i: && $(MAKE) all) || exit 1; done

named: $(OBJS)
	$(CC) $(LDFLAGS) -o named $(OBJS) $(LDLIBS)

version.c: Version.c
	(u=$${USER-root} d=`pwd` h=`hostname` t=`date`; \
	sed -e "s|%WHEN%|$${t}|" \
	    -e "s|%WHOANDWHERE%|$${u}@$${h}:$${d}|" \
	    < Version.c > version.c)

clean:
	rm -f *.o *~ core version.c
	@for i in $(SUBDIRS); do (cd $$i && echo $$i: && $(MAKE) clean) || exit 1; done

clobber: clean
	rm -f named
	@for i in $(SUBDIRS); do (cd $$i && echo $$i: && $(MAKE) clobber) || exit 1; done

