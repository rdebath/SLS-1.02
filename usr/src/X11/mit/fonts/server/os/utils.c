/* $XConsortium: utils.c,v 1.9 92/01/31 17:45:40 eswu Exp $ */
/*
 * misc os utilities
 */
/*
 * Copyright 1990, 1991 Network Computing Devices;
 * Portions Copyright 1987 by Digital Equipment Corporation and the
 * Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this protoype software
 * and its documentation to Members and Affiliates of the MIT X Consortium
 * any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the names of Network Computing Devices, Digital or
 * MIT not be used in advertising or publicity pertaining to distribution of
 * the software without specific, written prior permission.
 *
 * NETWORK COMPUTING DEVICES, DIGITAL AND MIT DISCLAIM ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS, IN NO EVENT SHALL NETWORK COMPUTING DEVICES, DIGITAL OR MIT BE
 * LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * $NCDId: @(#)utils.c,v 4.9 1991/07/09 14:08:13 lemke Exp $
 *
 */

#include	<stdio.h>
#include	<X11/Xos.h>
#include	"misc.h"
#include	"globals.h"
#include	<signal.h>

#ifndef X_NOT_POSIX
#ifdef _POSIX_SOURCE
#include <limits.h>
#else
#define _POSIX_SOURCE
#include <limits.h>
#undef _POSIX_SOURCE
#endif
#endif /* X_NOT_POSIX */
#ifndef PATH_MAX
#include <sys/param.h>
#ifndef PATH_MAX
#ifdef MAXPATHLEN
#define PATH_MAX MAXPATHLEN
#else
#define PATH_MAX 1024
#endif
#endif
#endif /* PATH_MAX */

#ifdef SIGNALRETURNSINT
#define SIGVAL int
#else
#define SIGVAL void
#endif

extern char *configfilename;
char       *progname;
Bool        CloneSelf;
int         ListenSock = -1;
extern int  ListenPort;

SIGVAL
AutoResetServer()
{

#ifdef DEBUG
    fprintf(stderr, "got a reset signal\n");
#endif

    dispatchException |= DE_RESET;
    isItTimeToYield = TRUE;

#ifdef SYSV
    signal(SIGHUP, AutoResetServer);
#endif
}

SIGVAL
GiveUp()
{

#ifdef DEBUG
    fprintf(stderr, "got a TERM signal\n");
#endif

    dispatchException |= DE_TERMINATE;
    isItTimeToYield = TRUE;
}

SIGVAL
ServerReconfig()
{

#ifdef DEBUG
    fprintf(stderr, "got a re-config signal\n");
#endif

    dispatchException |= DE_RECONFIG;
    isItTimeToYield = TRUE;

#ifdef SYSV
    signal(SIGUSR1, ServerReconfig);
#endif
}

SIGVAL
ServerCacheFlush()
{

#ifdef DEBUG
    fprintf(stderr, "got a flush signal\n");
#endif

    dispatchException |= DE_FLUSH;
    isItTimeToYield = TRUE;

#ifdef SYSV
    signal(SIGUSR2, ServerCacheFlush);
#endif
}

long
GetTimeInMillis()
{
    struct timeval tp;

    gettimeofday(&tp, 0);
    return ((tp.tv_sec * 1000) + (tp.tv_usec / 1000));
}

static void
usage()
{
    fprintf(stderr, "%s: [-cf config-file] [-p tcp_port] [-s server_number]\n", progname);
    exit(-1);
}

/* ARGSUSED */
void
ProcessCmdLine(argc, argv)
    int         argc;
    char      **argv;
{
    int         i;

#ifdef MEMBUG
    {
	extern pointer MemoryAllocationBase;

	if (!MemoryAllocationBase)
#ifndef AIXV3
	    MemoryAllocationBase = (pointer) sbrk(0);
#else
	    MemoryAllocationBase = (pointer) 0x20000000;
#endif
    }
#endif

    progname = argv[0];
    for (i = 1; i < argc; i++) {
	if (!strcmp(argv[i], "-port")) {
	    if (argv[i + 1])
		ListenPort = atoi(argv[++i]);
	    else
		usage();
	} else if (!strcmp(argv[i], "-ls")) {
	    if (argv[i + 1])
		ListenSock = atoi(argv[++i]);
	    else
		usage();
	} else if (!strcmp(argv[i], "-cf") || !strcmp(argv[i], "-config")) {
	    if (argv[i + 1])
		configfilename = argv[++i];
	    else
		usage();
	}
#ifdef MEMBUG
	else if ( strcmp( argv[i], "-alloc") == 0)
	{
	    extern unsigned long    MemoryFail;
	    if(++i < argc)
	        MemoryFail = atoi(argv[i]);
	    else
		usage ();
	}
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
	else
	    usage();
    }
}


#ifndef SPECIAL_MALLOC

unsigned long	Must_have_memory;

#ifdef MEMBUG
#define FIRSTMAGIC 0x11aaaa11
#define SECONDMAGIC 0x22aaaa22
#define FREEDMAGIC  0x33aaaa33
#define BLANKMAGIC  0x44aaaa44
#define ALLOCMAGIC  0x55aaaa55
#define MEM_FAIL_SCALE	100000

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
unsigned long	MemoryFail;
pointer		MemoryAllocationBase = NULL;

static void	CheckNode ();

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

/* FSalloc -- FS's internal memory allocator.  Why does it return unsigned
 * int * instead of the more common char *?  Well, if you read K&R you'll
 * see they say that alloc must return a pointer "suitable for conversion"
 * to whatever type you really want.  In a full-blown generic allocator
 * there's no way to solve the alignment problems without potentially
 * wasting lots of space.  But we have a more limited problem. We know
 * we're only ever returning pointers to structures which will have to
 * be long word aligned.  So we are making a stronger guarantee.  It might
 * have made sense to make FSalloc return char * to conform with people's
 * expectations of malloc, but this makes lint happier.
 */

unsigned long * 
FSalloc (amount)
    unsigned long amount;
{
    char		*malloc();
    register pointer  ptr;
	
    if ((long)amount < 0)
	return (unsigned long *)NULL;
    if (amount == 0)
	amount++;
    /* aligned extra on long word boundary */
    amount = (amount + 3) & ~3;
#ifdef MEMBUG
    if (MemoryValidate)
	ValidateAllActiveMemory ();
    if (!Must_have_memory && MemoryFail &&
	((random() % MEM_FAIL_SCALE) < MemoryFail))
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
 * FScalloc
 *****************/

unsigned long *
FScalloc (amount)
    unsigned long   amount;
{
    unsigned long   *ret;

    ret = FSalloc (amount);
    if (ret)
	bzero ((char *) ret, (int) amount);
    return ret;
}

/*****************
 * FSrealloc
 *****************/

unsigned long *
FSrealloc (ptr, amount)
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
	    	FSfree(ptr);
	    return (unsigned long *)NULL;
    	}
    	if (!Must_have_memory && MemoryFail &&
	    ((random() % MEM_FAIL_SCALE) < MemoryFail))
	    return (unsigned long *)NULL;
    	amount = (amount + 3) & ~3;
	CheckNode(ptr);
	ptr = (pointer)realloc((char *) Header (ptr), request(amount));
	if (ptr)
	    return SetupBlock (ptr, amount);
    }
    else
    {
	return FSalloc (amount);
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
 *  FSfree
 *    calls free 
 *****************/    

void
FSfree(ptr)
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

    if (ptr < MemoryAllocationBase)
	FatalError("Trying to free static storage");
    head = Header(ptr);
    if (((pointer) head) < MemoryAllocationBase)
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
#endif /* SPECIAL_MALLOC */
