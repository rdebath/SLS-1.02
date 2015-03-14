/* Get the system load averages.
   Copyright (C) 1985, 86, 87, 88, 89, 91, 92 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Compile-time symbols that this file uses:

   FIXUP_KERNEL_SYMBOL_ADDR()	Adjust address in returned struct nlist.
   KERNEL_FILE			Pathname of the kernel to nlist.
   LDAV_CVT()			Scale the load average from the kernel.
				Returns a double.
   LDAV_SYMBOL			Name of kernel symbol giving load average.
   LOAD_AVE_TYPE		Type of the load average array in the kernel.
				Must be defined unless one of
				apollo, DGUX, NeXT, or UMAX is defined;
				otherwise, no load average is available.
   NLIST_STRUCT			Include nlist.h, not a.out.h, and
				the nlist n_name element is a pointer,
				not an array.
   NLIST_NAME_UNION		struct nlist has an n_un member, not n_name.

   Specific system predefines this file uses, aside from setting
   default values if not emacs:

   apollo
   BSD				Real BSD, not just BSD-like.
   DGUX
   eunice			UNIX emulator under VMS.
   NeXT
   sgi
   sony_news                    NEWS-OS (works at least for 4.1C)
   UMAX
   UMAX4_3
   VMS

   In addition, to avoid nesting many #ifdefs, we internally set
   LDAV_DONE to indicate that the load average has been computed.  */

#ifdef emacs
#include "config.h"

/* The existing Emacs configuration files define a macro called
   LOAD_AVE_CVT, which accepts a value of type LOAD_AVE_TYPE, and
   returns the load average multiplied by 100.  What we actually want
   is a macro called LDAV_CVT, which returns the load average as an
   unmultiplied double.

   For backwards compatibility, we'll define LDAV_CVT in terms of
   LOAD_AVE_CVT, but future machine config files should just define
   LDAV_CVT directly.  */

#if !defined(LDAV_CVT) && defined(LOAD_AVE_CVT)
#define LDAV_CVT(n) (LOAD_AVE_CVT (n) / 100.0)
#endif

#else /* not emacs */

#ifdef unix
#include <sys/param.h>
#endif

#ifdef NeXT
/* NeXT in the 2.{0,1,2} releases defines BSD in <sys/param.h>, which
   conflicts with the definition understood in this file, that this
   really is BSD. */
#undef BSD
#endif

/* Set values that are different from the defaults, which are
   set a little farther down with #ifndef.  */


/* Some shorthands.  */
#if defined(hp300) && !defined(hpux)
#define MORE_BSD
#endif

#if defined(ultrix) && defined(mips)
#define decstation
#endif

#if defined(sun) && defined(SVR4)
#define SUNOS_5
#endif


#if defined(MORE_BSD) || defined(sun) || defined(decstation) || defined(_SEQUENT_) || defined(sgi) || defined(SVR4) || defined(sony_news)
#define LOAD_AVE_TYPE long
#endif


#ifndef	FSCALE

/* SunOS and some others define FSCALE in sys/param.h.  */

#ifdef MORE_BSD
#define FSCALE 2048.0
#endif

#if defined(MIPS) || defined(SVR4) || defined(decstation)
#define FSCALE 256
#endif

#ifdef sgi
#define	FSCALE 1000.0
#endif

#ifdef FSCALE
#define	LDAV_CVT(n) (((double) (n)) / FSCALE)
#endif

#endif	/* Not FSCALE.  */


#if !defined(NLIST_STRUCT) && (defined(MORE_BSD) || defined(sun) || defined(decstation) || defined(hpux) || defined(_SEQUENT_) || defined(sequent) || defined(sgi) || defined(SVR4)) || defined(sony_news)
#define NLIST_STRUCT
#endif


#if defined(sgi) || (defined(mips) && !defined(BSD))
#define FIXUP_KERNEL_SYMBOL_ADDR(nl) ((nl)[0].n_value &= ~(1 << 31))
#endif


#ifdef sequent
#define KERNEL_FILE "/dynix"
#endif

#ifdef hpux
#define KERNEL_FILE "/hp-ux"
#endif

#if !defined(KERNEL_FILE) && (defined(_SEQUENT_) || defined(MIPS) || defined(SVR4) || defined(ISC) || defined (sgi) || defined(SVR4))
#define KERNEL_FILE "/unix"
#endif


#ifdef alliant
#define LDAV_SYMBOL "_Loadavg"
#endif

#if !defined(LDAV_SYMBOL) && (defined(hpux) || defined(_SEQUENT_) || defined(SVR4) || defined(ISC) || defined(sgi))
#define LDAV_SYMBOL "avenrun"
#endif

#endif /* not emacs */

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#if defined(USG) || defined(SYSV) || defined(_POSIX_VERSION)
#include <fcntl.h>
#else
#include <sys/file.h>
#endif

#ifdef TEST
#include <stdio.h>
#include <errno.h>

#ifndef errno
extern int errno;
#endif
#endif /* TEST */

/* LOAD_AVE_TYPE should only get defined if we're going to use the
   nlist method.  */
#if !defined(LOAD_AVE_TYPE) && (defined(BSD) || defined(LDAV_CVT) || defined(KERNEL_FILE) || defined(LDAV_SYMBOL))
#define LOAD_AVE_TYPE double
#endif

#ifdef LOAD_AVE_TYPE

#ifndef VMS
#ifndef NLIST_STRUCT
#include <a.out.h>
#else /* NLIST_STRUCT */
#include <nlist.h>
#endif /* NLIST_STRUCT */

#ifdef SUNOS_5
#include <fcntl.h>
#include <kvm.h>
#endif

#ifndef KERNEL_FILE
#define KERNEL_FILE "/vmunix"
#endif /* KERNEL_FILE */

#ifndef LDAV_SYMBOL
#define LDAV_SYMBOL "_avenrun"
#endif /* LDAV_SYMBOL */

#else /* VMS */

#ifndef eunice
#include <iodef.h>
#include <descrip.h>
#else /* eunice */
#include <vms/iodef.h>
#endif /* eunice */
#endif /* VMS */

#ifndef LDAV_CVT
#define LDAV_CVT(n) ((double) (n))
#endif /* !LDAV_CVT */

#endif /* LOAD_AVE_TYPE */

#ifdef NeXT
#include <mach.h>
#endif /* NeXT */

#ifdef sgi
#include <sys/types.h>
#include <sys/sysmp.h>
#endif /* sgi */

#ifdef UMAX
#include <stdio.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/syscall.h>

#ifdef UMAX_43
#include <machine/cpu.h>
#include <inq_stats/statistics.h>
#include <inq_stats/sysstats.h>
#include <inq_stats/cpustats.h>
#include <inq_stats/procstats.h>
#else /* Not UMAX_43.  */
#include <sys/sysdefs.h>
#include <sys/statistics.h>
#include <sys/sysstats.h>
#include <sys/cpudefs.h>
#include <sys/cpustats.h>
#include <sys/procstats.h>
#endif /* Not UMAX_43.  */
#endif /* UMAX */

#ifdef DGUX
#include <sys/dg_sys_info.h>
#endif

/* Avoid static vars inside a function since in HPUX they dump as pure.  */

#ifdef NeXT
static processor_set_t default_set;
static int initialized;
#endif /* NeXT */

#ifdef UMAX
static unsigned int cpus = 0;
static unsigned int samples;
#endif /* UMAX */

#ifdef DGUX
static struct dg_sys_info_load_info load_info;	/* what-a-mouthful! */
#endif /* DGUX */

#ifdef LOAD_AVE_TYPE
/* File descriptor open to /dev/kmem or VMS load ave driver.  */
static int channel;
/* Nonzero iff channel is valid.  */
static int initialized;
/* Offset in kmem to seek to read load average, or 0 means invalid.  */
static long offset;

#if !defined(VMS) && !defined(sgi)
static struct nlist nl[2];
#endif /* Not VMS or sgi */

#ifdef SUNOS_5
static kvm_t *kd;
#endif /* SUNOS_5 */

#endif /* LOAD_AVE_TYPE */

/* Put the 1 minute, 5 minute and 15 minute load averages
   into the first NELEM elements of LOADAVG.
   Return the number written (never more than 3),
   or -1 if an error occurred.  */

int
getloadavg (loadavg, nelem)
     double loadavg[];
     int nelem;
{
  int elem = 0;			/* Return value.  */

#if defined(NeXT)
#define LDAV_DONE
  /* The NeXT code was adapted from iscreen 3.2.
     We only know how to get the 1-minute average for this system.  */

  host_t host;
  struct processor_set_basic_info info;
  unsigned info_count;

  if (!initialized)
    {
      if (processor_set_default (host_self (), &default_set) == KERN_SUCCESS)
	initialized = 1;
    }

  if (initialized)
    {
      info_count = PROCESSOR_SET_BASIC_INFO_COUNT;
      if (processor_set_info (default_set, PROCESSOR_SET_BASIC_INFO, &host,
			     (processor_set_info_t) &info, &info_count)
	  != KERN_SUCCESS)
	initialized = 0;
      else
	{
	  if (nelem > 0)
	    loadavg[elem++] = (double) info.load_average / LOAD_SCALE;
	}
    }

  if (!initialized)
    return -1;
#endif /* NeXT */

#if defined(UMAX)
#define LDAV_DONE
/* UMAX 4.2, which runs on the Encore Multimax multiprocessor, does not
   have a /dev/kmem.  Information about the workings of the running kernel
   can be gathered with inq_stats system calls.
   We only know how to get the 1-minute average for this system.  */

  struct proc_summary proc_sum_data;
  struct stat_descr proc_info;
  double load;
  register unsigned int i, j;

  if (cpus == 0)
    {
      register unsigned int c, i;
      struct cpu_config conf;
      struct stat_descr desc;

      desc.sd_next = 0;
      desc.sd_subsys = SUBSYS_CPU;
      desc.sd_type = CPUTYPE_CONFIG;
      desc.sd_addr = (char *) &conf;
      desc.sd_size = sizeof conf;

      if (inq_stats (1, &desc))
	return -1;

      c = 0;
      for (i = 0; i < conf.config_maxclass; ++i)
	{
	  struct class_stats stats;
	  bzero ((char *) &stats, sizeof stats);

	  desc.sd_type = CPUTYPE_CLASS;
	  desc.sd_objid = i;
	  desc.sd_addr = (char *) &stats;
	  desc.sd_size = sizeof stats;

	  if (inq_stats (1, &desc))
	    return -1;

	  c += stats.class_numcpus;
	}
      cpus = c;
      samples = cpus < 2 ? 3 : (2 * cpus / 3);
    }

  proc_info.sd_next = 0;
  proc_info.sd_subsys = SUBSYS_PROC;
  proc_info.sd_type = PROCTYPE_SUMMARY;
  proc_info.sd_addr = (char *) &proc_sum_data;
  proc_info.sd_size = sizeof (struct proc_summary);
  proc_info.sd_sizeused = 0;

  if (inq_stats (1, &proc_info) != 0)
    return -1;

  load = proc_sum_data.ps_nrunnable;
  j = 0;
  for (i = samples - 1; i > 0; --i)
    {
      load += proc_sum_data.ps_nrun[j];
      if (j++ == PS_NRUNSIZE)
	j = 0;
    }

  if (nelem > 0)
    loadavg[elem++] = load / samples / cpus;
#endif /* UMAX */

#if defined(DGUX)
#define LDAV_DONE
  /* This call can return -1 for an error, but with good args
     it's not supposed to fail.  The first argument is for no
     apparent reason of type `long int *'.  */
  dg_sys_info ((long int *) &load_info,
	       DG_SYS_INFO_LOAD_INFO_TYPE,
	       DG_SYS_INFO_LOAD_VERSION_0);

  if (nelem > 0)
    loadavg[elem++] = load_info.one_minute;
  if (nelem > 1)
    loadavg[elem++] = load_info.five_minute;
  if (nelem > 2)
    loadavg[elem++] = load_info.fifteen_minute;
#endif /* DGUX */

#ifdef apollo
#define LDAV_DONE
/* Apollo code from lisch@mentorg.com (Ray Lischner).

   This system call is not documented.  The load average is obtained as
   three long integers, for the load average over the past minute,
   five minutes, and fifteen minutes.  Each value is a scaled integer,
   with 16 bits of integer part and 16 bits of fraction part.

   I'm not sure which operating system first supported this system call,
   but I know that SR10.2 supports it.  */

  extern void proc1_$get_loadav ();
  unsigned long load_ave[3];

  proc1_$get_loadav (load_ave);

  if (nelem > 0)
    loadavg[elem++] = load_ave[0] / 65536.0;
  if (nelem > 1)
    loadavg[elem++] = load_ave[1] / 65536.0;
  if (nelem > 2)
    loadavg[elem++] = load_ave[2] / 65536.0;
#endif /* apollo */

#if defined(VMS)
#define LDAV_DONE
  /* VMS specific code -- read from the Load Ave driver.  */

  LOAD_AVE_TYPE load_ave[3];
#ifdef eunice
  struct
  {
    int dsc$w_length;
    char *dsc$a_pointer;
  } descriptor;
#endif

  /* Ensure that there is a channel open to the load ave device.  */
  if (!initialized)
    {
      /* Attempt to open the channel.  */
#ifdef eunice
      descriptor.size = 18;
      descriptor.ptr = "$$VMS_LOAD_AVERAGE";
#else
      $DESCRIPTOR (descriptor, "LAV0:");
#endif
      if (sys$assign (&descriptor, &channel, 0, 0) & 1)
	initialized = 1;
    }

  /* Read the load average vector.  */
  if (initialized && !(sys$qiow (0, channel, IO$_READVBLK, 0, 0, 0,
				 load_ave, 12, 0, 0, 0, 0) & 1))
    {
      sys$dassgn (channel);
      initialized = 0;
    }

  if (!initialized)
    return -1;
#endif /* VMS */

#if defined(LOAD_AVE_TYPE) && !defined(VMS)
#define LDAV_DONE
  /* UNIX-specific code -- read the average from /dev/kmem.  */

  LOAD_AVE_TYPE load_ave[3];

  /* Get the address of LDAV_SYMBOL.  */
  if (offset == 0)
    {
#ifndef SUNOS_5
#ifndef sgi
#ifndef NLIST_STRUCT
      strcpy (nl[0].n_name, LDAV_SYMBOL);
      strcpy (nl[1].n_name, "");
#else /* NLIST_STRUCT */
#ifdef NLIST_NAME_UNION
      nl[0].n_un.n_name = LDAV_SYMBOL;
      nl[1].n_un.n_name = 0;
#else /* not NLIST_NAME_UNION */
      nl[0].n_name = LDAV_SYMBOL;
      nl[1].n_name = 0;
#endif /* not NLIST_NAME_UNION */
#endif /* NLIST_STRUCT */

      if (nlist (KERNEL_FILE, nl) >= 0)
	/* Omit "&& nl[0].n_type != 0 " -- it breaks on Sun386i.  */
	{
#ifdef FIXUP_KERNEL_SYMBOL_ADDR
	  FIXUP_KERNEL_SYMBOL_ADDR (nl);
#endif
	  offset = nl[0].n_value;
	}
#else /* sgi */
      int ldav_off;

      ldav_off = sysmp (MP_KERNADDR, MPKA_AVENRUN);
      if (ldav_off != -1)
	offset = (long) ldav_off & 0x7fffffff;
#endif /* sgi */
#endif  /* !SUNOS_5 */
    }

  /* Make sure we have /dev/kmem open.  */
  if (!initialized)
    {
#ifndef SUNOS_5
      channel = open ("/dev/kmem", 0);
      if (channel >= 0)
	initialized = 1;
#else /* SUNOS_5 */
      kd = kvm_open (0, 0, 0, O_RDONLY, 0);
      if (kd != 0) 
	{
	  kvm_nlist (kd, nl);
	  initialized = 1;
	}
#endif /* SUNOS_5 */
    }

  /* If we can, get the load average values.  */
  if (offset && initialized)
    {
      /* Try to read the load.  */
#ifndef SUNOS_5
      if (lseek (channel, offset, 0) == -1L
	  || read (channel, (char *) load_ave, sizeof (load_ave))
	  != sizeof (load_ave))
	{
	  close (channel);
	  initialized = 0;
	}
#else  /* SUNOS_5 */
      if (kvm_read (kd, offset, (char *) load_ave, sizeof (load_ave))
	  != sizeof (load_ave))
        {
          kvm_close (kd);
          initialized = 0;
	}
#endif /* SUNOS_5 */
    }

  if (offset == 0 || !initialized)
    return -1;
#endif /* LOAD_AVE_TYPE and not VMS */

#ifdef LOAD_AVE_TYPE		/* Including VMS.  */
  if (nelem > 0)
    loadavg[elem++] = LDAV_CVT (load_ave[0]);
  if (nelem > 1)
    loadavg[elem++] = LDAV_CVT (load_ave[1]);
  if (nelem > 2)
    loadavg[elem++] = LDAV_CVT (load_ave[2]);
#endif /* LOAD_AVE_TYPE */

#ifdef LDAV_DONE
  return elem;
#else
  return -1;
#endif
}

#ifdef TEST
void
main (argc, argv)
     int argc;
     char **argv;
{
  int naptime = 0;

  if (argc > 1)
    naptime = atoi (argv[1]);

  if (naptime == 0)
    naptime = 5;

  while (1)
    {
      double avg[3];
      int loads;

      errno = 0;		/* Don't be misled if it doesn't set errno.  */
      loads = getloadavg (avg, 3);
      if (loads == -1)
	{
	  perror ("Error getting load average");
	  exit (1);
	}
      if (loads > 0)
	printf ("1-minute: %f  ", avg[0]);
      if (loads > 1)
	printf ("5-minute: %f  ", avg[1]);
      if (loads > 2)
	printf ("15-minute: %f  ", avg[2]);
      if (loads > 0)
	putchar ('\n');
      sleep (naptime);
    }
}
#endif /* TEST */
