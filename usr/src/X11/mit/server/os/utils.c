/***********************************************************
Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/
/* $Header: /home/x_cvs/mit/server/os/utils.c,v 1.11 1992/09/16 15:10:44 dawes Exp $ */
/* $XConsortium: utils.c,v 1.109 92/02/24 19:03:14 keith Exp $ */
#include "Xos.h"
#include <stdio.h>
#include "misc.h"
#include "X.h"
#include "input.h"
#include "opaque.h"
#include <signal.h>
#ifndef SYSV
#include <sys/resource.h>
#endif

#ifdef SIGNALRETURNSINT
#define SIGVAL int
#else
#define SIGVAL void
#endif

extern char *display;

extern long defaultScreenSaverTime;	/* for parsing command line */
extern long defaultScreenSaverInterval;
extern int defaultScreenSaverBlanking;
extern int defaultBackingStore;
extern Bool disableBackingStore;
extern Bool disableSaveUnders;
extern Bool PartialNetwork;
#ifndef NOLOGOHACK
extern int logoScreenSaver;
#endif
#ifdef RLIMIT_DATA
extern int limitDataSpace;
#endif
#ifdef RLIMIT_STACK
extern int limitStackSpace;
#endif
#ifdef RLIMIT_NOFILE
extern int limitNoFile;
#endif
extern int defaultColorVisualClass;
extern long ScreenSaverTime;		/* for forcing reset */
extern Bool permitOldBugs;
extern int monitorResolution;
extern Bool defeatAccessControl;

Bool CoreDump;

void ddxUseMsg();

#if !defined(SVR4) && !defined(linux)
extern char *sbrk();
#endif

#ifdef AIXV3
#define AIXFILE "/tmp/aixfile"
FILE *aixfd;
int SyncOn  = 0;
extern int SelectWaitTime;
#endif

#ifdef DEBUG
#ifndef SPECIAL_MALLOC
#define MEMBUG
#endif
#endif

#ifdef MEMBUG
#define MEM_FAIL_SCALE 100000
long Memory_fail = 0;
static pointer minfree = NULL;
static void CheckNode();
#endif

Bool Must_have_memory = FALSE;

char *dev_tty_from_init = NULL;		/* since we need to parse it anyway */

/* Force connections to close on SIGHUP from init */

SIGVAL
AutoResetServer ()
{
    dispatchException |= DE_RESET;
    isItTimeToYield = TRUE;
#ifdef GPROF
    chdir ("/tmp");
    exit (0);
#endif
#if defined(SYSV) || defined (SVR4)
    signal (SIGHUP, AutoResetServer);
#endif
}

/* Force connections to close and then exit on SIGTERM, SIGINT */

SIGVAL
GiveUp()
{

#if defined(SYSV) || defined(SVR4)
    /*
     * Don't let any additional occurrances of these signals cause
     * premature termination (xdm seems to send two SIGTERMs).
     * DHD Dec 1991
     */
    signal(SIGTERM,SIG_IGN);
    signal(SIGINT,SIG_IGN);
#endif

    dispatchException |= DE_TERMINATE;
    isItTimeToYield = TRUE;
}


static void
AbortServer()
{
    extern void AbortDDX();

    AbortDDX();
    fflush(stderr);
    if (CoreDump)
	abort();
    exit (1);
}

void
Error(str)
    char *str;
{
    perror(str);
}

#if defined (UTEK) || defined (UTEKV) || defined(sgi)
/*
 * Tektronix has a shared-memory time value which doesn't
 * match gettimeofday at all, but it is only accessible
 * inside the driver.  SGI has their own GetTimeInMillis.
 */
#else
long
GetTimeInMillis()
{
    struct timeval  tp;

    gettimeofday(&tp, 0);
    return(tp.tv_sec * 1000) + (tp.tv_usec / 1000);
}
#endif

AdjustWaitForDelay (waitTime, newdelay)
    pointer	    waitTime;
    unsigned long   newdelay;
{
    static struct timeval   delay_val;
    struct timeval	    **wt = (struct timeval **) waitTime;
    unsigned long	    olddelay;

    if (*wt == NULL)
    {
	delay_val.tv_sec = newdelay / 1000;
	delay_val.tv_usec = 1000 * (newdelay % 1000);
	*wt = &delay_val;
    }
    else
    {
	olddelay = (*wt)->tv_sec * 1000 + (*wt)->tv_usec / 1000;
	if (newdelay < olddelay)
	{
	    (*wt)->tv_sec = newdelay / 1000;
	    (*wt)->tv_usec = 1000 * (newdelay % 1000);
	}
    }
}

void UseMsg()
{
#if !defined(AIXrt) && !defined(AIX386)
    ErrorF("use: X [:<display>] [option]\n");
    ErrorF("-a #                   mouse acceleration (pixels)\n");
    ErrorF("-ac                    disable access control restrictions\n");
#ifdef MEMBUG
    ErrorF("-alloc int             chance alloc should fail\n");
#endif
    ErrorF("-auth string           select authorization file\n");	
    ErrorF("bc                     enable bug compatibility\n");
    ErrorF("-bs                    disable any backing store support\n");
    ErrorF("-c                     turns off key-click\n");
    ErrorF("c #                    key-click volume (0-100)\n");
    ErrorF("-cc int                default color visual class\n");
    ErrorF("-co string             color database file\n");
    ErrorF("-dpi int               screen resolution in dots per inch\n");
    ErrorF("-f #                   bell base (0-100)\n");
    ErrorF("-fc string             cursor font\n");
    ErrorF("-fn string             default font name\n");
    ErrorF("-fp string             default font path\n");
    ErrorF("-help                  prints message with these options\n");
    ErrorF("-I                     ignore all remaining arguments\n");
#ifdef RLIMIT_DATA
    ErrorF("-ld int                limit data space to N Kb\n");
#endif
#ifdef RLIMIT_NOFILE
    ErrorF("-lf int                limit number of open files to N\n");
#endif
#ifdef RLIMIT_STACK
    ErrorF("-ls int                limit stack space to N Kb\n");
#endif
#ifndef NOLOGOHACK
    ErrorF("-logo                  enable logo in screen saver\n");
    ErrorF("nologo                 disable logo in screen saver\n");
#endif
    ErrorF("-p #                   screen-saver pattern duration (minutes)\n");
    ErrorF("-r                     turns off auto-repeat\n");
    ErrorF("r                      turns on auto-repeat \n");
    ErrorF("-s #                   screen-saver timeout (minutes)\n");
    ErrorF("-su                    disable any save under support\n");
    ErrorF("-t #                   mouse threshold (pixels)\n");
    ErrorF("-to #                  connection time out\n");
    ErrorF("ttyxx                  server started from init on /dev/ttyxx\n");
    ErrorF("v                      video blanking for screen-saver\n");
    ErrorF("-v                     screen-saver without video blanking\n");
    ErrorF("-wm                    WhenMapped default backing-store\n");
    ErrorF("-x string              loads named extension at init time \n");
#ifdef XDMCP
    XdmcpUseMsg();
#endif
#endif /* !AIXrt && ! AIX386 */
    ddxUseMsg();
}

/*
 * This function parses the command line. Handles device-independent fields
 * and allows ddx to handle additional fields.  It is not allowed to modify
 * argc or any of the strings pointed to by argv.
 */
void
ProcessCommandLine ( argc, argv )
int	argc;
char	*argv[];

{
    int i, skip;

#ifdef MEMBUG
#ifndef AIXV3
    if (!minfree)
	minfree = (pointer)sbrk(0);
#else
    /* segment 2 is user data space */
    minfree = (pointer) 0x20000000;
#endif
#endif
    defaultKeyboardControl.autoRepeat = TRUE;

#ifdef AIXV3
    OpenDebug();
#endif
    for ( i = 1; i < argc; i++ )
    {
	/* call ddx first, so it can peek/override if it wants */
        if(skip = ddxProcessArgument(argc, argv, i))
	{
	    i += (skip - 1);
	}
	else if(argv[i][0] ==  ':')  
	{
	    /* initialize display */
	    display = argv[i];
	    display++;
	}
	else if ( strcmp( argv[i], "-a") == 0)
	{
	    if(++i < argc)
	        defaultPointerControl.num = atoi(argv[i]);
	    else
		UseMsg();
	}
	else if ( strcmp( argv[i], "-ac") == 0)
	{
	    defeatAccessControl = TRUE;
	}
#ifdef MEMBUG
	else if ( strcmp( argv[i], "-alloc") == 0)
	{
	    if(++i < argc)
	        Memory_fail = atoi(argv[i]);
	    else
		UseMsg();
	}
#endif
	else if ( strcmp( argv[i], "-auth") == 0)
	{
	    if(++i < argc)
	        InitAuthorization (argv[i]);
	    else
		UseMsg();
	}
	else if ( strcmp( argv[i], "bc") == 0)
	    permitOldBugs = TRUE;
	else if ( strcmp( argv[i], "-bs") == 0)
	    disableBackingStore = TRUE;
	else if ( strcmp( argv[i], "c") == 0)
	{
	    if(++i < argc)
	        defaultKeyboardControl.click = atoi(argv[i]);
	    else
		UseMsg();
	}
	else if ( strcmp( argv[i], "-c") == 0)
	{
	    defaultKeyboardControl.click = 0;
	}
	else if ( strcmp( argv[i], "-cc") == 0)
	{
	    if(++i < argc)
	        defaultColorVisualClass = atoi(argv[i]);
	    else
		UseMsg();
	}
	else if ( strcmp( argv[i], "-co") == 0)
	{
	    if(++i < argc)
	        rgbPath = argv[i];
	    else
		UseMsg();
	}
	else if ( strcmp( argv[i], "-core") == 0)
	    CoreDump = TRUE;
	else if ( strcmp( argv[i], "-dpi") == 0)
	{
	    if(++i < argc)
	        monitorResolution = atoi(argv[i]);
	    else
		UseMsg();
	}
	else if ( strcmp( argv[i], "-f") == 0)
	{
	    if(++i < argc)
	        defaultKeyboardControl.bell = atoi(argv[i]);
	    else
		UseMsg();
	}
	else if ( strcmp( argv[i], "-fc") == 0)
	{
	    if(++i < argc)
	        defaultCursorFont = argv[i];
	    else
		UseMsg();
	}
	else if ( strcmp( argv[i], "-fn") == 0)
	{
	    if(++i < argc)
	        defaultTextFont = argv[i];
	    else
		UseMsg();
	}
	else if ( strcmp( argv[i], "-fp") == 0)
	{
	    if(++i < argc)
	        defaultFontPath = argv[i];
	    else
		UseMsg();
	}
	else if ( strcmp( argv[i], "-help") == 0)
	{
	    UseMsg();
	    exit(0);
	}
#ifdef RLIMIT_DATA
	else if ( strcmp( argv[i], "-ld") == 0)
	{
	    if(++i < argc)
	    {
	        limitDataSpace = atoi(argv[i]);
		if (limitDataSpace > 0)
		    limitDataSpace *= 1024;
	    }
	    else
		UseMsg();
	}
#endif
#ifdef RLIMIT_NOFILE
	else if ( strcmp( argv[i], "-lf") == 0)
	{
	    if(++i < argc)
	        limitNoFile = atoi(argv[i]);
	    else
		UseMsg();
	}
#endif
#ifdef RLIMIT_STACK
	else if ( strcmp( argv[i], "-ls") == 0)
	{
	    if(++i < argc)
	    {
	        limitStackSpace = atoi(argv[i]);
		if (limitStackSpace > 0)
		    limitStackSpace *= 1024;
	    }
	    else
		UseMsg();
	}
#endif
#ifndef NOLOGOHACK
	else if ( strcmp( argv[i], "-logo") == 0)
	{
	    logoScreenSaver = 1;
	}
	else if ( strcmp( argv[i], "nologo") == 0)
	{
	    logoScreenSaver = 0;
	}
#endif
	else if ( strcmp( argv[i], "-p") == 0)
	{
	    if(++i < argc)
	        defaultScreenSaverInterval = ((long)atoi(argv[i])) *
					     MILLI_PER_MIN;
	    else
		UseMsg();
	}
	else if ( strcmp( argv[i], "-pn") == 0)
	    PartialNetwork = TRUE;
	else if ( strcmp( argv[i], "r") == 0)
	    defaultKeyboardControl.autoRepeat = TRUE;
	else if ( strcmp( argv[i], "-r") == 0)
	    defaultKeyboardControl.autoRepeat = FALSE;
	else if ( strcmp( argv[i], "-s") == 0)
	{
	    if(++i < argc)
	        defaultScreenSaverTime = ((long)atoi(argv[i])) * MILLI_PER_MIN;
	    else
		UseMsg();
	}
	else if ( strcmp( argv[i], "-su") == 0)
	    disableSaveUnders = TRUE;
	else if ( strcmp( argv[i], "-t") == 0)
	{
	    if(++i < argc)
	        defaultPointerControl.threshold = atoi(argv[i]);
	    else
		UseMsg();
	}
	else if ( strcmp( argv[i], "-to") == 0)
	{
	    if(++i < argc)
		TimeOutValue = ((long)atoi(argv[i])) * MILLI_PER_SECOND;
	    else
		UseMsg();
	}
	else if ( strcmp( argv[i], "-terminate") == 0)
	{
	    extern Bool terminateAtReset;
	    
	    terminateAtReset = TRUE;
	}
	else if ( strcmp( argv[i], "v") == 0)
	    defaultScreenSaverBlanking = PreferBlanking;
	else if ( strcmp( argv[i], "-v") == 0)
	    defaultScreenSaverBlanking = DontPreferBlanking;
#ifdef MEMBUG
	else if ( strcmp ( argv[i], "validateMemory") == 0)
	{
	    extern unsigned long MemoryValidate;
	    MemoryValidate = 1;
	}
	else if ( strcmp ( argv[i], "neverFreeMemory") == 0)
	{
	    extern unsigned long MemoryNeverFree;
	    MemoryNeverFree = 1;
	}
#endif
	else if ( strcmp( argv[i], "-wm") == 0)
	    defaultBackingStore = WhenMapped;
	else if ( strcmp( argv[i], "-x") == 0)
	{
	    if(++i >= argc)
		UseMsg();
	    /* For U**x, which doesn't support dynamic loading, there's nothing
	     * to do when we see a -x.  Either the extension is linked in or
	     * it isn't */
	}
	else if ( strcmp( argv[i], "-I") == 0)
	{
	    /* ignore all remaining arguments */
	    break;
	}
	else if (strncmp (argv[i], "tty", 3) == 0)
	{
	    /* just in case any body is interested */
	    dev_tty_from_init = argv[i];
	}
#ifdef XDMCP
	else if ((skip = XdmcpOptions(argc, argv, i)) != i)
	{
	    i = skip - 1;
	}
#endif
#ifdef AIXV3
        else if ( strcmp( argv[i], "-timeout") == 0)
        {
            if(++i < argc)
                SelectWaitTime = atoi(argv[i]);
            else
                UseMsg();
        }
        else if ( strcmp( argv[i], "-sync") == 0)
        {
            SyncOn++;
        }
#endif
 	else
 	{
	    UseMsg();
	    exit (1);
        }
    }
}

#ifndef SPECIAL_MALLOC

#ifdef MEMBUG
#define FIRSTMAGIC 0x11aaaa11
#define SECONDMAGIC 0x22aaaa22
#define FREEDMAGIC  0x33aaaa33
#define BLANKMAGIC  0x44aaaa44
#define ALLOCMAGIC  0x55aaaa55

typedef struct _MallocHeader	*MallocHeaderPtr;

typedef struct _MallocHeader {
	unsigned long	amount;
	unsigned long	time;
	MallocHeaderPtr	prev;
	MallocHeaderPtr	next;
	unsigned long	magic;
} MallocHeaderRec;

typedef struct _MallocTrailer {
	unsigned long	magic;
} MallocTrailerRec, *MallocTrailerPtr;

unsigned long	MemoryAllocTime;
unsigned long	MemoryAllocBreakpoint = ~0;
unsigned long	MemoryFreeBreakpoint = ~0;
unsigned long	MemoryActive = 0;
unsigned long	MemoryValidate;
unsigned long	MemoryNeverFree;

MallocHeaderPtr	MemoryInUse;
MallocHeaderPtr	MemoryFreed;

#define request(amount)	((amount) + sizeof (MallocHeaderRec) + sizeof (MallocTrailerRec))
#define Header(ptr)	((MallocHeaderPtr) (((char *) ptr) - sizeof (MallocHeaderRec)))
#define Trailer(ptr)	((MallocTrailerPtr) (((char *) ptr) + Header(ptr)->amount))

static unsigned long *
SetupBlock(ptr, amount)
    unsigned long   *ptr;
{
    MallocHeaderPtr	head = (MallocHeaderPtr) ptr;
    MallocTrailerPtr	tail = (MallocTrailerPtr) (((char *) ptr) + amount + sizeof (MallocHeaderRec));

    MemoryActive += amount;
    head->magic = FIRSTMAGIC;
    head->amount = amount;
    if (MemoryAllocTime == MemoryAllocBreakpoint)
	head->amount = amount;
    head->time = MemoryAllocTime++;
    head->next = MemoryInUse;
    head->prev = 0;
    if (MemoryInUse)
	MemoryInUse->prev = head;
    MemoryInUse = head;

    tail->magic = SECONDMAGIC;
    
    return (unsigned long *)(((char *) ptr) + sizeof (MallocHeaderRec));
}

ValidateAllActiveMemory ()
{
    MallocHeaderPtr	head;
    MallocTrailerPtr	tail;

    for (head = MemoryInUse; head; head = head->next)
    {
	tail = (MallocTrailerPtr) (((char *) (head + 1)) + head->amount);
    	if (head->magic == FREEDMAGIC)
	    FatalError("Free data on active list");
    	if(head->magic != FIRSTMAGIC || tail->magic != SECONDMAGIC)
	    FatalError("Garbage object on active list");
    }
    for (head = MemoryFreed; head; head = head->next)
    {
	tail = (MallocTrailerPtr) (((char *) (head + 1)) + head->amount);
	if (head->magic != FREEDMAGIC || tail->magic != FREEDMAGIC)
	    FatalError("Non free data on free list");
	if (!CheckMemoryContents (head, BLANKMAGIC))
	    FatalError("Freed data reused");
    }
}

FillMemoryContents (head, value)
    MallocHeaderPtr head;
    long	    value;
{
    int		    count;
    long	    *store;

    count = head->amount / sizeof (long);
    store = (long *) (head + 1);
    while (count--)
	*store++ = value;
}

CheckMemoryContents (head, value)
    MallocHeaderPtr head;
    long	    value;
{
    int		    count;
    long	    *check;

    count = head->amount / sizeof (long);
    check = (long *) (head + 1);
    while (count--)
	if (*check++ != value)
	    return FALSE;
    return TRUE;
}

#endif

/* XALLOC -- X's internal memory allocator.  Why does it return unsigned
 * int * instead of the more common char *?  Well, if you read K&R you'll
 * see they say that alloc must return a pointer "suitable for conversion"
 * to whatever type you really want.  In a full-blown generic allocator
 * there's no way to solve the alignment problems without potentially
 * wasting lots of space.  But we have a more limited problem. We know
 * we're only ever returning pointers to structures which will have to
 * be long word aligned.  So we are making a stronger guarantee.  It might
 * have made sense to make Xalloc return char * to conform with people's
 * expectations of malloc, but this makes lint happier.
 */

unsigned long * 
Xalloc (amount)
    unsigned long amount;
{
    char		*malloc();
    register pointer  ptr;
	
    if ((long)amount <= 0)
	return (unsigned long *)NULL;
    /* aligned extra on long word boundary */
    amount = (amount + 3) & ~3;
#ifdef MEMBUG
    if (MemoryValidate)
	ValidateAllActiveMemory ();
    if (!Must_have_memory && Memory_fail &&
	((random() % MEM_FAIL_SCALE) < Memory_fail))
	return (unsigned long *)NULL;
    if (ptr = (pointer)malloc(request(amount)))
    {
	unsigned long	*ret;
	ret = SetupBlock (ptr, amount);
	FillMemoryContents ((MallocHeaderPtr) ptr, ALLOCMAGIC);
	return ret;
    }
#else
    if (ptr = (pointer)malloc(amount))
	return (unsigned long *)ptr;
#endif
    if (Must_have_memory)
	FatalError("Out of memory");
    return (unsigned long *)NULL;
}

/*****************
 * Xcalloc
 *****************/

unsigned long *
Xcalloc (amount)
    unsigned long   amount;
{
    unsigned long   *ret;

    ret = Xalloc (amount);
    if (ret)
	bzero ((char *) ret, (int) amount);
    return ret;
}

/*****************
 * Xrealloc
 *****************/

unsigned long *
Xrealloc (ptr, amount)
    register pointer ptr;
    unsigned long amount;
{
    char *malloc();
    char *realloc();

#ifdef MEMBUG
    if (ptr)
    {
    	if (MemoryValidate)
	    ValidateAllActiveMemory ();
    	if ((long)amount <= 0)
    	{
	    if (!amount)
	    	Xfree(ptr);
	    return (unsigned long *)NULL;
    	}
    	if (!Must_have_memory && Memory_fail &&
	    ((random() % MEM_FAIL_SCALE) < Memory_fail))
	    return (unsigned long *)NULL;
    	amount = (amount + 3) & ~3;
	CheckNode(ptr);
	ptr = (pointer)realloc((char *) Header (ptr), request(amount));
	if (ptr)
	    return SetupBlock (ptr, amount);
    }
    else
    {
	return Xalloc (amount);
    }
#else
    if ((long)amount <= 0)
    {
	if (ptr && !amount)
	    free(ptr);
	return (unsigned long *)NULL;
    }
    amount = (amount + 3) & ~3;
    if (ptr)
        ptr = (pointer)realloc((char *)ptr, amount);
    else
	ptr = (pointer)malloc(amount);
    if (ptr)
        return (unsigned long *)ptr;
#endif
    if (Must_have_memory)
	FatalError("Out of memory");
    return (unsigned long *)NULL;
}
                    
/*****************
 *  Xfree
 *    calls free 
 *****************/    

void
Xfree(ptr)
    register pointer ptr;
{
#ifdef MEMBUG
    if (MemoryValidate)
	ValidateAllActiveMemory ();
    if (ptr)
    {
	MallocHeaderPtr	    head;
	MallocTrailerPtr    trail;

	CheckNode(ptr);
	head = Header(ptr);
	trail = Trailer(ptr);
	if (head->time == MemoryFreeBreakpoint)
	    head->magic = FIRSTMAGIC;
	head->magic = FREEDMAGIC;
	trail->magic = FREEDMAGIC;
	FillMemoryContents (head, BLANKMAGIC);
	if (MemoryNeverFree)
	{
	    head->prev = 0;
	    head->next = MemoryFreed;
	    MemoryFreed = head;
	}
	else
	    free ((char *) head);
    }
#else
    if (ptr)
	free((char *)ptr); 
#endif
}

#ifdef MEMBUG
static void
CheckNode(ptr)
    pointer ptr;
{
    MallocHeaderPtr	head;
    MallocHeaderPtr	f, prev;

    if (ptr < minfree)
	FatalError("Trying to free static storage");
    head = Header(ptr);
    if (((pointer) head) < minfree)
	FatalError("Trying to free static storage");
    if (head->magic == FREEDMAGIC)
	FatalError("Freeing something already freed");
    if(head->magic != FIRSTMAGIC || Trailer(ptr)->magic != SECONDMAGIC)
	FatalError("Freeing a garbage object");
    if(head->prev)
	head->prev->next = head->next;
    else
	MemoryInUse = head->next;
    if (head->next)
	head->next->prev = head->prev;
    MemoryActive -= head->amount;
}

DumpMemoryInUse (time)
    unsigned long   time;
{
    MallocHeaderPtr	head;

    for (head = MemoryInUse; head; head = head->next)
	if (head->time >= time)
	    fprintf (stderr, "0x%08x %5d %6d\n", head,
					head->amount,
					head->time);
}

static unsigned long	MarkedTime;

MarkMemoryTime ()
{
    MarkedTime = MemoryAllocTime;
}

DumpMemorySince ()
{
    DumpMemoryInUse (MarkedTime);
}
#endif
#else  /* SPECIAL_MALLOC */
#ifdef CAHILL_MALLOC
#include <malloc.h>

unsigned long * 
debug_Xalloc (file, line, amount)
    char *file;
    int line;
    unsigned long amount;
{
    register pointer  ptr;
	
    if ((long)amount <= 0)
	return (unsigned long *)NULL;
    /* aligned extra on long word boundary */
    amount = (amount + 3) & ~3;
    if (ptr = (pointer)debug_malloc(file, line, amount))
	return (unsigned long *)ptr;
    if (Must_have_memory)
	FatalError("Out of memory");
    return (unsigned long *)NULL;
}

/*****************
 * Xcalloc
 *****************/

unsigned long *
debug_Xcalloc (file, line, amount)
    char *file;
    int line;
    unsigned long   amount;
{
    unsigned long   *ret;

    ret = debug_Xalloc (file, line, amount);
    if (ret)
	bzero ((char *) ret, (int) amount);
    return ret;
}

/*****************
 * Xrealloc
 *****************/

unsigned long *
debug_Xrealloc (file, line, ptr, amount)
    char *file;
    int line;
    register pointer ptr;
    unsigned long amount;
{
    if ((long)amount <= 0)
    {
	if (ptr && !amount)
	    debug_free(file, line, ptr);
	return (unsigned long *)NULL;
    }
    amount = (amount + 3) & ~3;
    if (ptr)
        ptr = (pointer)debug_realloc(file, line, (char *)ptr, amount);
    else
	ptr = (pointer)debug_malloc(file, line, amount);
    if (ptr)
        return (unsigned long *)ptr;
    if (Must_have_memory)
	FatalError("Out of memory");
    return (unsigned long *)NULL;
}
                    
/*****************
 *  Xfree
 *    calls free 
 *****************/    

void
debug_Xfree(file, line, ptr)
    char *file;
    int line;
    register pointer ptr;
{
    if (ptr)
	debug_free(file, line, (char *)ptr); 
}
#endif
#endif /* SPECIAL_MALLOC */

/*VARARGS1*/
void
FatalError(f, s0, s1, s2, s3, s4, s5, s6, s7, s8, s9) /* limit of ten args */
    char *f;
    char *s0, *s1, *s2, *s3, *s4, *s5, *s6, *s7, *s8, *s9;
{
    ErrorF("\nFatal server error:\n");
    ErrorF(f, s0, s1, s2, s3, s4, s5, s6, s7, s8, s9);
    ErrorF("\n");
    AbortServer();
    /*NOTREACHED*/
}

/*VARARGS1*/
void
ErrorF( f, s0, s1, s2, s3, s4, s5, s6, s7, s8, s9) /* limit of ten args */
    char *f;
    char *s0, *s1, *s2, *s3, *s4, *s5, *s6, *s7, *s8, *s9;
{
#ifdef AIXV3
    fprintf(aixfd, f, s0, s1, s2, s3, s4, s5, s6, s7, s8, s9);
    fflush (aixfd);

    if (SyncOn)
        sync();
#else
    fprintf( stderr, f, s0, s1, s2, s3, s4, s5, s6, s7, s8, s9);
#endif
}

#ifdef AIXV3
OpenDebug()
{
        if((aixfd = fopen(AIXFILE,"w")) == NULL )
        {
                fprintf(stderr,"open aixfile failed\n");
                exit(-1);
        }
        chmod(AIXFILE,00777);
}
#endif
