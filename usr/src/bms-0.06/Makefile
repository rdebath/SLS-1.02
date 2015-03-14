#
# Makefile for bms, the batch-mode MandelSpawn client program
#
# This doesn't use Imake because it isn't an X program.
#

# uncomment this if you have GCC (not nearly as important as for mslaved)
# CC=gcc

LD=$(CC)

# You may need to:
#  * add -DNO_MALLOC_H to DEFINES if you don't have <malloc.h>
#  * add -DNEED_SYS_SELECT_H to DEFINES if you need to include <sys/select.h>
#     to define getdtablesize() or the fd_set type
DEFINES=

CFLAGS= -O $(DEFINES)

LDFLAGS=

OBJS=bms.o work.o mspawn.o

bms: $(OBJS)
	$(LD) $(LDFLAGS) $(OBJS) -o bms

bms.o: bms.c work.h mspawn.h ms_job.h ms_real.h
work.o: work.c work.h ms_ipc.h ms_real.h
mspawn.o: mspawn.c mspawn.h ms_ipc.h ms_job.h ms_real.h

clean:
	rm -f core bms *.o *~

