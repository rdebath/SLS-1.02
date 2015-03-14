/*
 * get_load - get system load
 *
 * $Header: /home/x_cvs/mit/clients/xload/get_load.c,v 1.4 1992/09/26 09:29:53 dawes Exp $
 * $XConsortium: get_load.c,v 1.26 91/07/25 14:20:25 rws Exp $
 *
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Authors:  Many and varied...
 *
 * Call InitLoadPoint() to initialize.
 * GetLoadPoint() is a callback for the StripChart widget.
 */

#include <X11/Xos.h>
#include <X11/Intrinsic.h>
#include <stdio.h>

#ifdef att
#define LOADSTUB
#endif

#ifndef macII
#ifndef apollo
#ifndef LOADSTUB
#include <nlist.h>
#endif /* LOADSTUB */
#endif /* apollo */
#endif /* macII */

#if defined(MOTOROLA) && defined(SYSV)
#include <sys/sysinfo.h>
#endif

#ifdef sun
#    include <sys/param.h>
#    ifdef i386
#        include <kvm.h>
#        define	KVM_ROUTINES
#    endif /* i386 */
#endif

#if defined(umips) || (defined(ultrix) && defined(mips))
#include <sys/fixpoint.h>
#endif

#if  defined(CRAY) || defined(AIXV3)
#include <sys/param.h>
#define word word_t
#include <sys/sysinfo.h>
#undef word
#undef n_type
#define n_type n_value
#endif	/* CRAY */

#ifdef sequent
#include <sys/vm.h>
#endif /* sequent */

#ifdef macII
#include <a.out.h>
#include <sys/var.h>
#define X_AVENRUN 0
#define fxtod(i) (vec[i].high+(vec[i].low/65536.0))
struct lavnum {
    unsigned short high;
    unsigned short low;
};
#endif /* macII */

#ifdef hcx
#include <sys/param.h>
#endif /* hcx */

#ifdef __386BSD__
#include <sys/param.h>
#endif /* __386BSD__ */

#if defined(UTEK) || defined(alliant) || (defined(MOTOROLA) && defined(SVR4))
#define FSCALE	100.0
#endif

#ifdef sequent
#define FSCALE	1000.0
#endif

#ifdef sgi
#define FSCALE	1024.0
#endif

#if defined(sony) && OSMAJORVERSION == 4
#ifdef mips
#include <sys/fixpoint.h>
#else
#include <sys/param.h>
#endif
#endif

#ifdef SVR4
#ifndef FSCALE
#define FSCALE	(1 << 8)
#endif
#endif

#ifdef X_NOT_POSIX
extern long lseek();
#endif
extern void exit();

static xload_error();


#ifdef apollo
#include <apollo/base.h>
#include <apollo/time.h>
typedef struct {
	short		state;		/* ready, waiting, etc. */
	pinteger	usr;		/* user sr */
	linteger	upc;		/* user pc */
	linteger	usp;		/* user stack pointer */
	linteger	usb;		/* user sb ptr (A6) */
	time_$clock_t	cpu_total;	/* cumulative cpu used by process */
	unsigned short	priority;	/* process priority */
    } proc1_$info_t;

void proc1_$get_cput(
	time_$clock_t	*cput
);

void proc1_$get_info(
	short		&pid,
	proc1_$info_t	*info,
	status_$t	*sts
);

static int     lastNullCpu;
static int     lastClock;

void InitLoadPoint()				/* Apollo version */
{
     time_$clock_t  timeNow;
     proc1_$info_t  info;
     status_$t      st;

     proc1_$get_info( (short) 2, &info, &st );
     time_$clock( &timeNow );

     lastClock = timeNow.low32;
     lastNullCpu = info.cpu_total.low32;
}

/* ARGSUSED */
void GetLoadPoint( w, closure, call_data ) 	/* Apollo version */
     Widget	w;		/* unused */
     caddr_t	closure;	/* unused */
     caddr_t	call_data;	/* pointer to (double) return value */
{
     time_$clock_t  timeNow;
     double         temp;
     proc1_$info_t  info;
     status_$t      st;

     proc1_$get_info( (short) 2, &info, &st );
     time_$clock( &timeNow );

     temp = info.cpu_total.low32 - lastNullCpu;
     *(double *)call_data = 1.0 - temp / (timeNow.low32 - lastClock);

     lastClock = timeNow.low32;
     lastNullCpu = info.cpu_total.low32;
}
#else /* not apollo */
#if defined(SYSV) && defined(SYSV386)
/*
 * inspired by 'avgload' by John F. Haugh II
 */
#include <sys/param.h>
#include <sys/buf.h>
#include <sys/immu.h>
#include <sys/region.h>
#include <sys/var.h>
#include <sys/proc.h>
#define KERNEL_FILE "/unix"
#define KMEM_FILE "/dev/kmem"
#define VAR_NAME "v"
#define PROC_NAME "proc"
#define BUF_NAME "buf"
#define DECAY 0.8
struct nlist namelist[] = {
  {VAR_NAME},
  {PROC_NAME},
  {BUF_NAME},
  {0},
};

static int kmem;
static struct var v;
static struct proc *p;
static caddr_t first_buf, last_buf;

void InitLoadPoint()				/* SYSV386 version */
{
    int i;

    nlist( KERNEL_FILE, namelist);

    for (i=0; namelist[i].n_name; i++) 
	if (namelist[i].n_value == 0)
	    xload_error("cannot get name list from", KERNEL_FILE);

    if ((kmem = open(KMEM_FILE, O_RDONLY)) < 0)
	xload_error("cannot open", KMEM_FILE);

    if (lseek(kmem, namelist[0].n_value, 0) == -1)
	xload_error("cannot seek", VAR_NAME);

    if (read(kmem, &v, sizeof(v)) != sizeof(v))
	xload_error("cannot read", VAR_NAME);

    if ((p=(struct proc *)malloc(v.v_proc*sizeof(*p))) == NULL)
	xload_error("cannot allocat space for", PROC_NAME);
	  
    first_buf = (caddr_t) namelist[2].n_value;
    last_buf  = first_buf + v.v_buf * sizeof(struct buf);
}
	
/* ARGSUSED */
void GetLoadPoint( w, closure, call_data )	/* SYSV386 version */
Widget	w;		/* unused */
caddr_t	closure;	/* unused */
caddr_t	call_data;	/* pointer to (double) return value */
{
    double *loadavg = (double *)call_data;
    static double avenrun = 0.0;
    int i, nproc, size;
	
    (void) lseek(kmem, namelist[0].n_value, 0);
    (void) read(kmem, &v, sizeof(v));

    size = (struct proc *)v.ve_proc - (struct proc *)namelist[1].n_value;

    (void) lseek(kmem, namelist[1].n_value, 0);
    (void) read(kmem, p, size * sizeof(struct proc));

    for (nproc = 0, i=0; i<size; i++) 
	  if ((p[i].p_stat == SRUN) ||
	      (p[i].p_stat == SIDL) ||
	      (p[i].p_stat == SXBRK) ||
	      (p[i].p_stat == SSLEEP && (p[i].p_pri < PZERO) &&
	       (p[i].p_wchan >= first_buf) && (p[i].p_wchan < last_buf)))
	    nproc++;

    /* update the load average using a decay filter */
    avenrun = DECAY * avenrun + nproc * (1.0 - DECAY);
    *loadavg = avenrun;

    return;
}
#else /* not (SYSV && SYSV386) */
#ifdef KVM_ROUTINES
/*
 *	Sun 386i Code - abstracted to see the wood for the trees
 */

static struct nlist nl[2];
static kvm_t *kd;

void
InitLoadPoint()					/* Sun 386i version */
{
    kd = kvm_open("/vmunix", NULL, NULL, O_RDONLY, "Load Widget");
    if (kd == (kvm_t *)0) {
	xload_error("cannot get access to kernel address space");
    }
	
    nl[0].n_name = "avenrun";
    nl[1].n_name = NULL;
	
    if (kvm_nlist(kd, nl) != 0) {
	xload_error("cannot get name list");
    }
    
    if (nl[0].n_value == 0) {
	xload_error("Cannot find address for avenrun in the kernel\n");
    }
}

/* ARGSUSED */
void 
GetLoadPoint( w, closure, call_data ) 		/* Sun 386i version */
Widget	w;		/* unused */
XtPointer closure;	/* unused */
XtPointer call_data;	/* pointer to (double) return value */
{
    double *loadavg = (double *)call_data;
    long	temp;

    if (kvm_read(kd, nl[0].n_value, (char *)&temp, sizeof (temp)) != 
	sizeof (temp)) {
	xload_error("Kernel read error");
    }
    *loadavg = (double)temp/FSCALE;
}
#else /* not KVM_ROUTINES */
#ifdef LOADSTUB

void InitLoadPoint()
{
}

/* ARGSUSED */
void GetLoadPoint( w, closure, call_data )
     Widget	w;		/* unused */
     caddr_t	closure;	/* unused */
     caddr_t	call_data;	/* pointer to (double) return value */
{
	*(double *)call_data = 1.0;
}

#else /* not LOADSTUB */

#ifndef KMEM_FILE
#define KMEM_FILE "/dev/kmem"
#endif

#ifndef KERNEL_FILE

#ifdef __386BSD__
#define KERNEL_FILE "/386bsd"
#endif /* __386BSD__ */

#ifdef alliant
#define KERNEL_FILE "/vmunix"
#endif /* alliant */

#ifdef CRAY
#define KERNEL_FILE "/unicos"
#endif /* CRAY */

#ifdef hpux
#define KERNEL_FILE "/hp-ux"
#endif /* hpux */

#ifdef macII
#define KERNEL_FILE "/unix"
#endif /* macII */

#ifdef umips
# ifdef SYSTYPE_SYSV
# define KERNEL_FILE "/unix"
# else
# define KERNEL_FILE "/vmunix"
# endif /* SYSTYPE_SYSV */
#endif /* umips */

#ifdef sequent
#define KERNEL_FILE "/dynix"
#endif /* sequent */

#ifdef hcx
#define KERNEL_FILE "/unix"
#endif /* hcx */

#ifdef MOTOROLA
#if defined(SYSV) && defined(m68k)
#define KERNEL_FILE "/sysV68"
#endif
#if defined(SYSV) && defined(m88k)
#define KERNEL_FILE "/unix"
#endif
#ifdef SVR4
#define KERNEL_FILE "/unix"
#endif
#endif /* MOTOROLA */

/*
 * provide default for everyone else
 */
#ifndef KERNEL_FILE
#ifdef SVR4
#define KERNEL_FILE "/stand/unix"
#else
#ifdef SYSV
#define KERNEL_FILE "/unix"
#else
#define KERNEL_FILE "/vmunix"
#endif /* SYSV */
#endif /* SVR4 */
#endif /* KERNEL_FILE */
#endif /* KERNEL_FILE */


#ifndef KERNEL_LOAD_VARIABLE
#    ifdef __386BSD__
#        define KERNEL_LOAD_VARIABLE "_averunnable"
#    endif /* __386BSD__ */

#    ifdef alliant
#        define KERNEL_LOAD_VARIABLE "_Loadavg"
#    endif /* alliant */

#    ifdef CRAY
#        if defined(CRAY2) && OSMAJORVERSION == 4
#            define KERNEL_LOAD_VARIABLE "avenrun"
#        else
#            define KERNEL_LOAD_VARIABLE "sysinfo"
#            define SYSINFO
#        endif /* defined(CRAY2) && OSMAJORVERSION == 4 */
#    endif /* CRAY */

#    ifdef hpux
#        ifdef hp9000s800
#            define KERNEL_LOAD_VARIABLE "avenrun"
#        endif /* hp9000s800 */
#    endif /* hpux */

#    ifdef umips
#        ifdef SYSTYPE_SYSV
#            define KERNEL_LOAD_VARIABLE "avenrun"
#        else
#            define KERNEL_LOAD_VARIABLE "_avenrun"
#        endif /* SYSTYPE_SYSV */
#    endif /* umips */

#    ifdef sgi
#	 define KERNEL_LOAD_VARIABLE "avenrun"
#    endif /* sgi */

#    ifdef AIXV3
#        define KERNEL_LOAD_VARIABLE "sysinfo"
#    endif /* AIXV3 */

#    ifdef MOTOROLA
#        if defined(SYSV) && defined(m68k)
#            define KERNEL_LOAD_VARIABLE "sysinfo"
#        endif
#        if defined(SYSV) && defined(m88k)
#            define KERNEL_LOAD_VARIABLE "_sysinfo"
#        endif
#        ifdef SVR4
#            define KERNEL_LOAD_VARIABLE "avenrun"
#        endif
#    endif /* MOTOROLA */

#endif /* KERNEL_LOAD_VARIABLE */

/*
 * provide default for everyone else
 */

#ifndef KERNEL_LOAD_VARIABLE
#    ifdef USG
#        define KERNEL_LOAD_VARIABLE "sysinfo"
#        define SYSINFO
#    else
#    ifdef SVR4
#        define KERNEL_LOAD_VARIABLE "avenrun"
#    else
#        define KERNEL_LOAD_VARIABLE "_avenrun"
#    endif
#    endif
#endif /* KERNEL_LOAD_VARIABLE */

#ifdef macII
static struct var v;
static int pad[2];	/* This padding is needed if xload compiled on */
			/* a/ux 1.1 is executed on a/ux 1.0, because */
			/* the var structure had too much padding in 1.0, */
			/* so the 1.0 kernel writes past the end of the 1.1 */
			/* var structure in the uvar() call. */
static struct nlist nl[2];
static struct lavnum vec[3];
#else /* not macII */
static struct nlist namelist[] = {	    /* namelist for vmunix grubbing */
#define LOADAV 0
    {KERNEL_LOAD_VARIABLE},
    {0}
};
#endif /* macII */

static kmem;
static long loadavg_seek;

InitLoadPoint()
{
#ifdef macII
    extern nlist();

    int i;

    strcpy(nl[0].n_name, "avenrun");
    nl[1].n_name[0] = '\0';

    kmem = open(KMEM_FILE, O_RDONLY);
    if (kmem < 0) {
	xload_error("cannot open", KMEM_FILE);
    }

    uvar(&v);

    if (nlist( KERNEL_FILE, nl) != 0) {
	xload_error("cannot get name list from", KERNEL_FILE);
    }
    for (i = 0; i < 2; i++) {
	nl[i].n_value = (int)nl[i].n_value - v.v_kvoffset;
    }
#else /* not macII */
#if (!defined(SVR4) || !defined(__STDC__)) && !defined(sgi) && !defined(MOTOROLA) && !defined(__386BSD__)
    extern void nlist();
#endif

#ifdef AIXV3
    knlist( namelist, 1, sizeof(struct nlist));
#else	
    nlist( KERNEL_FILE, namelist);
#endif
    /*
     * Some systems appear to set only one of these to Zero if the entry could
     * not be found, I hope no_one returns Zero as a good value, or bad things
     * will happen to you.  (I have a hard time believing the value will
     * ever really be zero anyway).   CDP 5/17/89.
     */
#ifdef hcx
    if (namelist[LOADAV].n_type == 0 &&
#else
    if (namelist[LOADAV].n_type == 0 ||
#endif /* hcx */
	namelist[LOADAV].n_value == 0) {
	xload_error("cannot get name list from", KERNEL_FILE);
	exit(-1);
    }
    loadavg_seek = namelist[LOADAV].n_value;
#if defined(umips) && defined(SYSTYPE_SYSV)
    loadavg_seek &= 0x7fffffff;
#endif /* umips && SYSTYPE_SYSV */
#if (defined(CRAY) && defined(SYSINFO))
    loadavg_seek += ((char *) (((struct sysinfo *)NULL)->avenrun)) -
	((char *) NULL);
#endif /* CRAY && SYSINFO */
  
    kmem = open(KMEM_FILE, O_RDONLY);
    if (kmem < 0) xload_error("cannot open", KMEM_FILE);
#endif /* macII else */
}

/* ARGSUSED */
void GetLoadPoint( w, closure, call_data )
     Widget	w;		/* unused */
     caddr_t	closure;	/* unused */
     caddr_t	call_data;	/* pointer to (double) return value */
{
  	double *loadavg = (double *)call_data;

#ifdef macII
	lseek(kmem, (long)nl[X_AVENRUN].n_value, 0);
#else
	(void) lseek(kmem, loadavg_seek, 0);
#endif

#if defined(sun) || defined (UTEK) || defined(sequent) || defined(alliant) || defined(SVR4) || defined(sgi) || defined(hcx) || defined(__386BSD__)
	{
		long temp;
		(void) read(kmem, (char *)&temp, sizeof(long));
		*loadavg = (double)temp/FSCALE;
	}
#else /* else not sun or UTEK or sequent or alliant or SVR4 or sgi or hcx */
# ifdef macII
        {
                read(kmem, vec, 3*sizeof(struct lavnum));
                *loadavg = fxtod(0);
        }
# else /* else not macII */
#  if defined(umips) || (defined(ultrix) && defined(mips))
	{
		fix temp;
		(void) read(kmem, (char *)&temp, sizeof(fix));
		*loadavg = FIX_TO_DBL(temp);
	}
#  else /* not umips or ultrix risc */
#    ifdef AIXV3
        {
          struct sysinfo sysinfo_now;
          struct sysinfo sysinfo_last;
          static firsttime = TRUE;
          static double runavg = 0.0, swpavg = 0.0;

          (void) lseek(kmem, loadavg_seek, 0);
          (void) read(kmem, (char *)&sysinfo_last, sizeof(struct sysinfo));
          if (firsttime)
            {
              *loadavg = 0.0;
              firsttime = FALSE;
            }
          else
            {
              sleep(1);
              (void) lseek(kmem, loadavg_seek, 0);
              (void) read(kmem, (char *)&sysinfo_now, sizeof(struct sysinfo));
              runavg *= 0.8; swpavg *= 0.8;
              if (sysinfo_now.runocc != sysinfo_last.runocc)
                runavg += 0.2*((sysinfo_now.runque - sysinfo_last.runque - 1)
                          /(double)(sysinfo_now.runocc - sysinfo_last.runocc));
              if (sysinfo_now.swpocc != sysinfo_last.swpocc)
                swpavg += 0.2*((sysinfo_now.swpque - sysinfo_last.swpque)
                          /(double)(sysinfo_now.swpocc - sysinfo_last.swpocc));
              *loadavg = runavg + swpavg;
              sysinfo_last = sysinfo_now;
            }
          /* otherwise we leave load alone. */
        }
#    else /* not AIXV3 */
#      if defined(MOTOROLA) && defined(SYSV)
	{
        static int init = 0;
        static kmem;
        static long loadavg_seek;
        static xload_error();

#define CEXP    0.25            /* Constant used for load averaging */

        struct sysinfo sysinfod;
        static double oldloadavg;
        static double cexp = CEXP;
        static long sv_rq, sv_oc;   /* save old values */
        double rq, oc;              /* amount values have changed */

        if (!init)
        {
            if (nlist(KERNEL_FILE,namelist) == -1)
            {
                perror("xload: nlist()");
                xload_error("cannot get name list from", KERNEL_FILE);
            }
            loadavg_seek = namelist[0].n_value;

            kmem = open(KMEM_FILE, O_RDONLY);
            if (kmem < 0)
            {
                perror("xload: open()");
                xload_error("cannot open", KMEM_FILE);
            }
        }

        lseek(kmem, loadavg_seek, 0);
        if (read(kmem, &sysinfod, (int) sizeof (struct sysinfo)) == -1)
        {
             perror("xload: read() SYSINFONL");
             xload_error("read failed from", KMEM_FILE);
        }

        if (!init)
        {
            init = 1;
            sv_rq = sysinfod.runque;
            sv_oc = sysinfod.runocc;
            oldloadavg = *loadavg = 0.0;
            return;
        }
        /*
         * calculate the amount the values have
         * changed since last update
         */
        rq = (double) sysinfod.runque - sv_rq;
        oc = (double) sysinfod.runocc - sv_oc;

        /*
         * save old values for next time
         */
        sv_rq = sysinfod.runque;
        sv_oc = sysinfod.runocc;

        if (oc == 0.0)          /* avoid divide by zero  */
        {
                *loadavg = (1.0 - cexp) * oldloadavg;

        }
        else
        {
                *loadavg = ((1.0 - cexp) * oldloadavg) ((rq / oc) * cexp);
        }
        oldloadavg = *loadavg;
	}
#      else /* not MOTOROLA */
#     if defined(sony) && OSMAJORVERSION == 4
#      ifdef mips
	{
		fix temp;
		(void) read(kmem, (char *)&temp, sizeof(fix));
		*loadavg = FIX_TO_DBL(temp);
	}
#      else /* not mips */
	{
		long temp;
		(void) read(kmem, (char *)&temp, sizeof(long));
		*loadavg = (double)temp/FSCALE;
	}
#      endif /* mips */
#     else /* not sony NEWSOS4 */
	(void) read(kmem, (char *)loadavg, sizeof(double));
#        endif /* sony NEWOS4 */
#      endif /* MOTOROLA else */
#    endif /* AIXV3 else */
#  endif /* umips else */
# endif /* macII else */
#endif /* sun else */
	return;
}
#endif /* LOADSTUB else */
#endif /* KVM_ROUTINES else */
#endif /* SYSV && SYSV386 else */

static xload_error(str1, str2)
char *str1, *str2;
{
    (void) fprintf(stderr,"xload: %s %s\n", str1, str2);
    exit(-1);
}

#endif /* apollo else */

