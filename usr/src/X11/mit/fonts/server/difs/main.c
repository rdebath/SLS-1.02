/* $XConsortium: main.c,v 1.9 91/07/25 12:25:41 keith Exp $ */
/*
 * Font server main routine
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
 * $NCDId: @(#)main.c,v 4.9 1991/07/09 14:08:55 lemke Exp $
 *
 */

#include	"FS.h"
#include	"FSproto.h"
#include	"clientstr.h"
#include	"resource.h"
#include	"misc.h"
#include	"globals.h"
#include	"servermd.h"
#include	"cache.h"
#include	"site.h"

char       *ConnectionInfo;
int         ConnInfoLen;

Cache       serverCache;

#ifndef DEFAULT_CONFIG_FILE
#define DEFAULT_CONFIG_FILE "/usr/lib/X11/fs/config"
#endif

#define	SERVER_CACHE_SIZE	10000	/* for random server cacheables */

extern void InitProcVectors();
extern void InitFonts();
extern void InitAtoms();
extern void InitExtensions();
extern void ProcessCmdLine();
static Bool create_connection_block();

extern int  ListenSock;
extern ClientPtr currentClient;
char       *configfilename;
extern Bool drone_server;

main(argc, argv)
    int         argc;
    char      **argv;
{
    int         i;

    argcGlobal = argc;
    argvGlobal = argv;

    configfilename = DEFAULT_CONFIG_FILE;

    /* init stuff */
    ProcessCmdLine(argc, argv);
    InitErrors();
    /*
     * do this first thing, to get any options that only take effect at
     * startup time.  it is erad again each time the server resets
     */
    if (ReadConfigFile(configfilename) != FSSuccess)
	FatalError("couldn't parse config file");

    while (1) {
	serverGeneration++;
	OsInit();
	if (serverGeneration == 1) {
	    /* do first time init */
	    serverCache = CacheInit(SERVER_CACHE_SIZE);
	    CreateSockets(ListenSock);
	    InitProcVectors();
	    clients = (ClientPtr *) fsalloc(MAXCLIENTS * sizeof(ClientPtr));
	    if (!clients)
		FatalError("couldn't create client array");
	    for (i = MINCLIENT; i < MAXCLIENTS; i++)
		clients[i] = NullClient;
	    /* make serverClient */
	    serverClient = (ClientPtr) fsalloc(sizeof(ClientRec));
	    if (!serverClient)
		FatalError("couldn't create server client");
	}
	ResetSockets();

	/* init per-cycle stuff */
	InitClient(serverClient, SERVER_CLIENT, (pointer) 0);

	clients[SERVER_CLIENT] = serverClient;
	currentMaxClients = MINCLIENT;
	currentClient = serverClient;

	if (!InitClientResources(serverClient))
	    FatalError("couldn't init server resources");

	InitExtensions();
	InitAtoms();
	InitFonts();
	SetConfigValues();
	if (!create_connection_block())
	    FatalError("couldn't create connection block");

#ifdef DEBUG
	fprintf(stderr, "Entering Dispatch loop\n");
#endif

	Dispatch();

#ifdef DEBUG
	fprintf(stderr, "Leaving Dispatch loop\n");
#endif

	/* clean up per-cycle stuff */
	CacheReset();
	CloseDownExtensions();
	if ((dispatchException & DE_TERMINATE) || drone_server)
	    break;
	fsfree(ConnectionInfo);
	/* note that we're parsing it again, for each time the server resets */
	if (ReadConfigFile(configfilename) != FSSuccess)
	    FatalError("couldn't parse config file");
    }

    CloseErrors();
    exit(0);
}

void
NotImplemented()
{
    NoopDDA();			/* dummy to get difsutils.o to link */
    FatalError("Not implemented");
}

static Bool
create_connection_block()
{
    fsConnSetupAccept setup;
    char       *pBuf;

    setup.release_number = VENDOR_RELEASE;
    setup.vendor_len = strlen(VENDOR_STRING);
    setup.max_request_len = MAX_REQUEST_SIZE;
    setup.length = (sizeof(fsConnSetupAccept) + setup.vendor_len + 3) >> 2;

    ConnInfoLen = sizeof(fsConnSetupAccept) + ((setup.vendor_len + 3) & ~3);
    ConnectionInfo = (char *) fsalloc(ConnInfoLen);
    if (!ConnectionInfo)
	return FALSE;

    bcopy((char *) &setup, ConnectionInfo, sizeof(fsConnSetupAccept));
    pBuf = ConnectionInfo + sizeof(fsConnSetupAccept);
    bcopy(VENDOR_STRING, pBuf, (int) setup.vendor_len);

    return TRUE;
}
