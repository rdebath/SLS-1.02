#
# Makefile for kernel/blk_drv/scsi
#
# Note! Dependencies are done automagically by 'make dep', which also
# removes any old dependencies. DONT put your own dependencies here
# unless its something special (ie not a .c file).
#

SCSI_HOSTS := 0
SCSI_OBJS :=

ifdef CONFIG_SCSI

SCSI_OBJS := hosts.o scsi.o scsi_ioctl.o

ifdef CONFIG_BLK_DEV_ST
SCSI_OBJS := $(SCSI_OBJS) st.o
endif

ifdef CONFIG_BLK_DEV_SD
SCSI_OBJS := $(SCSI_OBJS) sd.o sd_ioctl.o
endif

ifdef CONFIG_BLK_DEV_SR
SCSI_OBJS := $(SCSI_OBJS) sr.o sr_ioctl.o
endif

ifdef CONFIG_SCSI_AHA1542
SCSI_OBJS := $(SCSI_OBJS) aha1542.o
SCSI_HOSTS := 1+$(SCSI_HOSTS)
endif

ifdef CONFIG_SCSI_AHA1740
SCSI_OBJS := $(SCSI_OBJS) aha1740.o
SCSI_HOSTS := 1+$(SCSI_HOSTS)
endif

ifdef CONFIG_SCSI_FUTURE_DOMAIN
SCSI_OBJS := $(SCSI_OBJS) fdomain.o
SCSI_HOSTS := 1+$(SCSI_HOSTS)
endif

ifdef CONFIG_SCSI_ULTRASTOR
SCSI_OBJS := $(SCSI_OBJS) ultrastor.o
SCSI_HOSTS := 1+$(SCSI_HOSTS)
endif

ifdef CONFIG_SCSI_7000FASST
SCSI_OBJS := $(SCSI_OBJS) wd7000.o
SCSI_HOSTS := 1+$(SCSI_HOSTS)
endif

ifdef CONFIG_SCSI_SEAGATE
SCSI_OBJS := $(SCSI_OBJS) seagate.o
SCSI_HOSTS := 1+$(SCSI_HOSTS)
else
ifdef CONFIG_SCSI_FD_88x
SCSI_OBJS := $(SCSI_OBJS) seagate.o
SCSI_HOSTS := 1+$(SCSI_HOSTS)
endif
endif

ifdef CONFIG_SCSI_DEBUG
SCSI_OBJS := $(SCSI_OBJS) scsi_debug.o
SCSI_HOSTS := 1+$(SCSI_HOSTS)
endif

scsi.a: $(SCSI_OBJS)
	rm -f scsi.a
	$(AR) rcs scsi.a $(SCSI_OBJS)
	sync

else

scsi.a:
	rm -f scsi.a
	@echo No SCSI drivers configured
	$(AR) rcs scsi.a

endif

CFLAGS := $(CFLAGS) "-DMAX_SCSI_HOSTS=($(SCSI_HOSTS))"

seagate.o: seagate.c
	$(CC) -Wall -I$(KERNELHDRS) -c seagate.c 

clean:
	rm -f core *.o *.a *.s

dep:
	$(CPP) -M *.c > .depend

#
# include a dependency file if one exists
#
ifeq (.depend,$(wildcard .depend))
include .depend
endif
