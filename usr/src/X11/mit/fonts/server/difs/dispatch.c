/* $XConsortium: dispatch.c,v 1.15 92/06/02 14:15:53 gildea Exp $ */
/*
 * protocol dispatcher
 */
/*
 * Copyright 1990, 1991 Network Computing Devices;
 * Portions Copyright 1987 by Digital Equipment Corporation and the
 * Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and
 * its documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the names of Network Computing Devices, Digital or
 * M.I.T. not be used in advertising or publicity pertaining to distribution
 * of the software without specific, written prior permission.
 *
 * NETWORK COMPUTING DEVICES, DIGITAL AND M.I.T. DISCLAIM ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL NETWORK COMPUTING DEVICES,
 * DIGITAL OR M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
 * DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
 * PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
 * THIS SOFTWARE.
 */

#include	"FS.h"
#include	"FSproto.h"
#include	"clientstr.h"
#include	"authstr.h"
#include	"misc.h"
#include	"osstruct.h"
#include	"extentst.h"
#include	"globals.h"
#include	"resource.h"
#include	"difsfontst.h"
#include	"fontstruct.h"
#include	"site.h"
#include	"events.h"
#include	"cache.h"

static void kill_all_clients();

char        dispatchException = 0;
char        isItTimeToYield;

ClientPtr   currentClient;

static int  nClients = 0;
static int  nextFreeClientID;

extern char *ConnectionInfo;
extern int  ConnInfoLen;

extern char *configfilename;

extern Bool drone_server;

extern void NotImplemented();

extern int  (*InitialVector[3]) ();
extern int  (*ProcVector[NUM_PROC_VECTORS]) ();
extern int  (*SwappedProcVector[NUM_PROC_VECTORS]) ();
extern void (*EventSwapVector[NUM_EVENT_VECTORS]) ();
extern void (*ReplySwapVector[NUM_PROC_VECTORS]) ();

extern void Swap32Write(), Swap16Write(), CopySwap16Write();

#define	MAJOROP	((fsReq *)client->requestBuffer)->reqType

#define	ALL_FORMAT_BITS	(BitmapFormatByteOrderMask | \
			 BitmapFormatBitOrderMask | \
			 BitmapFormatScanlineUnitMask | \
			 BitmapFormatScanlinePadMask | \
			 BitmapFormatImageRectMask)

#define	ALL_FORMAT_MASK_BITS	(BitmapFormatMaskByte | \
				 BitmapFormatMaskBit | \
				 BitmapFormatMaskImageRectangle | \
				 BitmapFormatMaskScanLinePad | \
				 BitmapFormatMaskScanLineUnit)

Dispatch()
{
    int         nready,
                result;
    int        *clientReady;
    ClientPtr   client;

    nextFreeClientID = MINCLIENT;
    nClients = 0;

    clientReady = (int *) ALLOCATE_LOCAL(sizeof(int) * MaxClients);
    if (!clientReady)
	return;

    while (1) {
	/* wait for something */
	nready = WaitForSomething(clientReady);

	while (!dispatchException && (--nready >= 0)) {
	    client = currentClient = clients[clientReady[nready]];

	    isItTimeToYield = FALSE;

	    while (!isItTimeToYield) {
		result = ReadRequest(client);
		if (result <= 0) {
		    if (result < 0)
			CloseDownClient(client);
		    break;
		}
		client->sequence++;

		if (result > (MAX_REQUEST_SIZE << 2))
		    result = FSBadLength;
		else
		    result = (*client->requestVector[MAJOROP]) (client);
		if (result != FSSuccess) {
		    if (client->noClientException != FSSuccess)
			CloseDownClient(client);
		    break;
		}
	    }
	    FlushAllOutput ();
	}
	/* reset if server is a drone and has run out of clients */
	if (drone_server && nClients == 0) {
	    dispatchException |= DE_RESET;
	}
	if (dispatchException) {
	    /* re-read the config file */
	    if (dispatchException & DE_RECONFIG) {
		NoticeF("Re-reading config file\n");
		if (ReadConfigFile(configfilename) != FSSuccess)
		    ErrorF("couldn't parse config file");
		SetConfigValues();
		dispatchException &= ~DE_RECONFIG;
	    }
	    /* flush all the caches */
	    if (dispatchException & DE_FLUSH) {
		NoticeF("flushing all caches\n");
		CacheReset();
		dispatchException &= ~DE_FLUSH;
	    }
	    /* reset */
	    if (dispatchException & DE_RESET) {
		NoticeF("resetting\n");
		break;
	    }
	    /* die *now* */
	    if (dispatchException & DE_TERMINATE) {
		NoticeF("terminating\n");
		kill_all_clients();
		exit(0);
		break;
	    }
	}
    }
    kill_all_clients();
    dispatchException = 0;
}

int
ProcInitialConnection(client)
    ClientPtr   client;
{
    REQUEST(fsReq);
    fsConnClientPrefix *prefix;
    int         whichbyte = 1;

    nClients++;
    prefix = (fsConnClientPrefix *) ((char *) stuff + sz_fsReq);
    if ((prefix->byteOrder != 'l') && (prefix->byteOrder != 'B'))
	return (client->noClientException = -2);
    if (((*(char *) &whichbyte) && (prefix->byteOrder == 'B')) ||
	    (!(*(char *) &whichbyte) && (prefix->byteOrder == 'l'))) {
	client->swapped = TRUE;
	SwapConnClientPrefix(prefix);
    }
    client->major_version = prefix->major_version;
    client->minor_version = prefix->minor_version;
    stuff->reqType = 2;
    stuff->length += prefix->auth_len;
    if (client->swapped) {
	swaps(&stuff->length, whichbyte);
    }
    ResetCurrentRequest(client);
    return client->noClientException;
}

int
ProcEstablishConnection(client)
    ClientPtr   client;
{
    fsConnClientPrefix *prefix;
    fsConnSetup csp;
    int         ret;
    pointer     auth_data,
                ad;
    char       *server_auth_data;
    AuthPtr     client_auth;
    int         i,
                num_alts,
                altlen,
                auth_accept,
                auth_index,
                auth_len;
    AlternateServerPtr altservers;

    REQUEST(fsReq);

    prefix = (fsConnClientPrefix *) ((char *) stuff + sz_fsReq);
    auth_data = (pointer) prefix + sz_fsConnClientPrefix;
    client_auth = (AuthPtr) ALLOCATE_LOCAL(prefix->num_auths * sizeof(AuthRec));
    if (!client_auth) {
	SendErrToClient(client, FSBadAlloc, (pointer) 0);
	return FSBadAlloc;
    }
/* XXXX -- this needs work for multiple auth replies */

    /* build up a list of the stuff */
    for (i = 0, ad = auth_data; i < (int)prefix->num_auths; i++) {
	client_auth[i].namelen = *(short *) ad;
	ad += 2;
	client_auth[i].datalen = *(short *) ad;
	ad += 2;
	client_auth[i].name = (char *) ad;
	ad += client_auth[i].namelen;
	client_auth[i].data = (char *) ad;
	ad += client_auth[i].datalen;
    }
    num_alts = ListAlternateServers(&altservers);
    for (i = 0, altlen = 0; i < num_alts; i++) {
	/* subset + len + namelen + pad */
	altlen += (2 + altservers[i].namelen + 3) >> 2;
    }

    auth_index = prefix->num_auths;
    ret = CheckClientAuthorization(client, client_auth,
		    &auth_accept, &auth_index, &auth_len, &server_auth_data);

    DEALLOCATE_LOCAL(client_auth);

    if (ret != FSSuccess) {
	SendErrToClient(client, FSBadAlloc, (pointer) 0);
	return FSBadAlloc;
    }
    csp.status = auth_accept;
    if (client->major_version == 1)
	/* we implement backwards compatibility for version 1.0 */
	csp.major_version = client->major_version;
    else
	csp.major_version = FS_PROTOCOL;
    csp.minor_version = FS_PROTOCOL_MINOR;
    csp.num_alternates = num_alts;
    csp.alternate_len = altlen;
    csp.auth_len = auth_len >> 2;
    csp.auth_index = auth_index;
    if (client->swapped) {
	WriteSConnSetup(client, &csp);
    } else {
	(void) WriteToClient(client, sizeof(fsConnSetup), (char *) &csp);
    }

    /* send the alternates info */
    for (i = 0; i < num_alts; i++) {
	char        tmp[258];

	/* WriteToClient pads, so we have to fake some things */
	tmp[0] = altservers[i].subset;
	tmp[1] = altservers[i].namelen;
	bcopy(altservers[i].name, (char *) &tmp[2], altservers[i].namelen);
	(void) WriteToClient(client, altservers[i].namelen + 2, tmp);
    }

    if (auth_len)
	(void) WriteToClient(client, auth_len, (char *) server_auth_data);

    if (auth_accept != AuthSuccess) {
	nClients--;
	return (client->noClientException = -2);
    }
    client->requestVector = client->swapped ? SwappedProcVector : ProcVector;
    client->sequence = 0;
    if (client->swapped)
	(void) WriteSConnectionInfo(client, ConnInfoLen, ConnectionInfo);
    else
	(void) WriteToClient(client, ConnInfoLen, ConnectionInfo);

#ifdef DEBUG
    fprintf(stderr, "Establishing new connection\n");
#endif

    return client->noClientException;
}

/*
 * NOTE -- the incoming data may be mangled
 */

void
SendErrToClient(client, error, data)
    ClientPtr   client;
    int         error;
    pointer     data;		/* resource id, format, resolution, etc */
{
    fsError     rep;
    int         extralen = 0;

    switch (error) {
    case FSBadFormat:
	extralen = sizeof(fsBitmapFormat);
	break;
    case FSBadFont:
    case FSBadAccessContext:
    case FSBadIDChoice:
    case FSBadEventMask:
	if (client->swapped)
	    SwapLongs((long *) data, 1);
	extralen = sizeof(Font);
	break;
    case FSBadRange:
	extralen = sizeof(fsRange);
	break;
    case FSBadResolution:
	if (client->swapped)
	    SwapShorts((short *) data, 1);
	/* note sneaky hack */
	rep.pad = *(CARD16 *) data;
	data += 2;
	extralen = 4;
	break;
    case FSBadLength:
	if (client->swapped)
	    SwapLongs((long *) data, 1);
	extralen = sizeof(long);
	break;
    default:
	/* nothing else to send */
	break;
    }

    rep.type = FS_Error;
    rep.sequenceNumber = client->sequence;
    rep.request = error;
    rep.major_opcode = ((fsReq *) client->requestBuffer)->reqType;
    rep.minor_opcode = MinorOpcodeOfRequest(client),
	rep.timestamp = GetTimeInMillis();
    rep.length = (sizeof(fsError) + extralen) >> 2;

    WriteErrorToClient(client, &rep);

    if (extralen)
	WriteToClient(client, extralen, (char *) data);
}

/* ARGSUSED */
int
ProcBadRequest(client)
    ClientPtr   client;
{
    SendErrToClient(client, FSBadRequest, NULL);
    return FSBadRequest;
}

int
ProcNoop(client)
    ClientPtr   client;
{
    REQUEST(fsReq);
    REQUEST_AT_LEAST_SIZE(fsReq);

    return client->noClientException;
}

int
ProcListCatalogues(client)
    ClientPtr   client;
{
    int         len,
                num;
    char       *catalogues;
    fsListCataloguesReply rep;

    REQUEST(fsListCataloguesReq);
    REQUEST_AT_LEAST_SIZE(fsListCataloguesReq);

    num = ListCatalogues((char *) &stuff[1], stuff->nbytes, stuff->maxNames,
			 &catalogues, &len);
    rep.type = FS_Reply;
    rep.num_replies = 0;
    rep.num_catalogues = num;
    rep.sequenceNumber = client->sequence;
    rep.length = (sizeof(fsListCataloguesReply) + len + 3) >> 2;

    WriteReplyToClient(client, sizeof(fsListCataloguesReply), &rep);
    (void) WriteToClient(client, len, (char *) catalogues);
    fsfree((char *) catalogues);
    return client->noClientException;
}

int
ProcSetCatalogues(client)
    ClientPtr   client;
{
    char       *new_cat;
    int         err,
                len;
    int         num;

    REQUEST(fsSetCataloguesReq);
    REQUEST_AT_LEAST_SIZE(fsSetCataloguesReq);

    if (stuff->num_catalogues == 0) {
	/* use the default */
	num = ListCatalogues("*", 1, 10000, &new_cat, &len);
    } else {
	num = stuff->num_catalogues;
	err = ValidateCatalogues(&num, (char *) &stuff[1]);
	if (err == FSSuccess) {
	    len = (stuff->length << 2) - sizeof(fsSetCataloguesReq);
	    new_cat = (char *) fsalloc(len);
	    if (!new_cat)
		return FSBadAlloc;
	    bcopy((char *) &stuff[1], new_cat, len);
	} else {
	    SendErrToClient(client, err, (pointer) &num);
	    return err;
	}
    }
    if (client->catalogues)
	fsfree((char *) client->catalogues);
    client->catalogues = new_cat;
    client->num_catalogues = num;
    return client->noClientException;
}

int
ProcGetCatalogues(client)
    ClientPtr   client;
{
    int         len,
                i,
                size;
    char       *cp;
    fsGetCataloguesReply rep;

    REQUEST(fsGetCataloguesReq);
    REQUEST_AT_LEAST_SIZE(fsGetCataloguesReq);

    for (i = 0, len = 0, cp = client->catalogues;
	    i < client->num_catalogues; i++) {
	size = *cp++;
	len += size + 1;	/* str length + size byte */
	cp += size;
    }

    rep.type = FS_Reply;
    rep.num_catalogues = client->num_catalogues;
    rep.sequenceNumber = client->sequence;
    rep.length = (sizeof(fsGetCataloguesReply) + len + 3) >> 2;

    WriteReplyToClient(client, sizeof(fsGetCataloguesReply), &rep);
    (void) WriteToClient(client, len, client->catalogues);

    return client->noClientException;
}

int
ProcCreateAC(client)
    ClientPtr   client;
{
    fsCreateACReply rep;
    AuthPtr     acp;
    AuthContextPtr authp;
    int         accept,
                i,
                err,
                index,
                size;
    pointer     ad;
    char       *auth_data;

    REQUEST(fsCreateACReq);
    REQUEST_AT_LEAST_SIZE(fsCreateACReq);

    authp = (AuthContextPtr) LookupIDByType(client->index, stuff->acid,
					    RT_AUTHCONT);
    if (authp) {
	SendErrToClient(client, FSBadIDChoice, (pointer) &stuff->acid);
	return FSBadIDChoice;
    }
    acp = 0;
    if (stuff->num_auths)
    {
    	acp = (AuthPtr) ALLOCATE_LOCAL(stuff->num_auths * sizeof(AuthRec));
    	if (!acp) {
	    SendErrToClient(client, FSBadAlloc, (pointer) 0);
	    return FSBadAlloc;
    	}
    }
    /* build up a list of the stuff */
    for (i = 0, ad = (pointer) &stuff[1]; i < (int)stuff->num_auths; i++) {
	acp[i].namelen = *(short *) ad;
	ad += 2;
	acp[i].datalen = *(short *) ad;
	ad += 2;
	acp[i].name = (char *) ad;
	ad += acp[i].namelen;
	acp[i].data = (char *) ad;
	ad += acp[i].datalen;
    }

/* XXX needs work for AuthContinue */
    index = stuff->num_auths;
    err = CheckClientAuthorization(client, acp, &accept, &index, &size,
				   &auth_data);

    if (err != FSSuccess) {
	SendErrToClient(client, err, (pointer) 0);
	if (acp)
	    DEALLOCATE_LOCAL(acp);
	return err;
    }
    authp = (AuthContextPtr) fsalloc(sizeof(AuthContextRec));
    if (!authp) {
	goto alloc_failure;
    }
    authp->authname = 0;
    authp->authdata = 0;
    if (index > 0)
    {
	authp->authname = (char *) fsalloc(acp[index - 1].namelen + 1);
	authp->authdata = (char *) fsalloc(acp[index - 1].datalen + 1);
	if (!authp->authname || !authp->authdata) {
	    fsfree((char *) authp->authname);
	    fsfree((char *) authp->authdata);
	    fsfree((char *) authp);
	    goto alloc_failure;
	}
	bcopy(acp[index - 1].name, authp->authname, acp[index - 1].namelen);
	bcopy(acp[index - 1].data, authp->authdata, acp[index - 1].datalen);
    }
    else
	size = 0;
    authp->acid = stuff->acid;
    if (!AddResource(client->index, stuff->acid, RT_AUTHCONT,(pointer) authp)) 
    {
alloc_failure:
	SendErrToClient(client, FSBadAlloc, (pointer) 0);
	if (acp)
	    DEALLOCATE_LOCAL(acp);
	return FSBadAlloc;
    }
    DEALLOCATE_LOCAL(acp);
    rep.type = FS_Reply;
    rep.status = accept;
    rep.auth_index = index;
    rep.sequenceNumber = client->sequence;
    rep.length = (sizeof(fsCreateACReply) + size) >> 2;
    rep.status = AuthSuccess;

    WriteReplyToClient(client, sizeof(fsCreateACReply), &rep);
    if (size)
	(void) WriteToClient(client, size, auth_data);

    return client->noClientException;
}

/* ARGSUSED */
int
DeleteAuthCont (value, id)
    pointer value;
    FSID    id;
{
    AuthContextPtr  authp = (AuthContextPtr) value;

    if (authp->authname)
	fsfree (authp->authname);
    if (authp->authdata)
	fsfree (authp->authdata);
    fsfree (authp);
    return 1;
}

int
ProcFreeAC(client)
    ClientPtr   client;
{
    AuthContextPtr authp;

    REQUEST(fsFreeACReq);
    REQUEST_AT_LEAST_SIZE(fsFreeACReq);
    authp = (AuthContextPtr) LookupIDByType(client->index, stuff->id,
					  RT_AUTHCONT);
    if (!authp) {
	SendErrToClient(client, FSBadIDChoice, (pointer) &stuff->id);
	return FSBadIDChoice;
    }
    FreeResource(client->index, stuff->id, RT_NONE);
    return client->noClientException;
}

int
ProcSetAuthorization(client)
    ClientPtr   client;
{
    AuthContextPtr acp;

    REQUEST(fsSetAuthorizationReq);
    REQUEST_AT_LEAST_SIZE(fsSetAuthorizationReq);
    acp = (AuthContextPtr) LookupIDByType(client->index, stuff->id,
					  RT_AUTHCONT);
    if (!acp) {
	SendErrToClient(client, FSBadIDChoice, (pointer) &stuff->id);
	return FSBadIDChoice;
    }
    client->auth = acp;		/* XXX does this need a refcount? */
    return client->noClientException;
}

int
ProcSetResolution(client)
    ClientPtr   client;
{
    fsResolution *new_res;

    REQUEST(fsSetResolutionReq);
    REQUEST_AT_LEAST_SIZE(fsSetResolutionReq);

    new_res = (fsResolution *)
	fsalloc(sizeof(fsResolution) * stuff->num_resolutions);
    if (!new_res) {
	SendErrToClient(client, FSBadAlloc, NULL);
	return FSBadAlloc;
    }
    fsfree((char *) client->resolutions);
    bcopy((char *) &stuff[1], (char *) new_res,
	  (stuff->num_resolutions * sizeof(fsResolution)));
    client->resolutions = new_res;
    client->num_resolutions = stuff->num_resolutions;

    return client->noClientException;
}

int
ProcGetResolution(client)
    ClientPtr   client;
{
    fsGetResolutionReply reply;

    REQUEST(fsReq);
    REQUEST_AT_LEAST_SIZE(fsReq);

    reply.type = FS_Reply;
    reply.num_resolutions = client->num_resolutions;
    reply.sequenceNumber = client->sequence;
    reply.length = (sizeof(fsGetResolutionReply) +
		    client->num_resolutions * sizeof(fsResolution)) >> 2;

    WriteReplyToClient(client, sizeof(fsGetResolutionReply), &reply);
    if (client->swapped)
	client->pSwapReplyFunc = CopySwap16Write;

    WriteSwappedDataToClient(client,
       (client->num_resolutions * sizeof(fsResolution)), client->resolutions);

    return client->noClientException;
}

int
ProcListFonts(client)
    ClientPtr   client;
{
    REQUEST(fsListFontsReq);
    REQUEST_FIXED_SIZE(fsListFontsReq, stuff->nbytes);

    return ListFonts(client, stuff->nbytes, (unsigned char *) &stuff[1],
		     stuff->maxNames);
}

int
ProcListFontsWithXInfo(client)
    ClientPtr   client;
{
    REQUEST(fsListFontsWithXInfoReq);
    REQUEST_FIXED_SIZE(fsListFontsWithXInfoReq, stuff->nbytes);

    return StartListFontsWithInfo(client, stuff->nbytes,
			       (unsigned char *) &stuff[1], stuff->maxNames);
}

int
ProcOpenBitmapFont(client)
    ClientPtr   client;
{
    FontPtr     pfont;
    int         nbytes,
                err;
    unsigned char *fname;

    REQUEST(fsOpenBitmapFontReq);
    fname = (unsigned char *) &stuff[1];
    nbytes = *fname++;

    REQUEST_FIXED_SIZE(fsOpenBitmapFontReq, (nbytes + 1));

    pfont = (FontPtr) LookupIDByType(client->index, stuff->fid, RT_FONT);
    if (pfont) {
	SendErrToClient(client, FSBadIDChoice, (pointer) &stuff->fid);
	return FSBadIDChoice;
    }
    if (stuff->format_hint != 0 &&
	    stuff->format_hint & ~ALL_FORMAT_BITS) {
	SendErrToClient(client, FSBadFormat, (pointer) &stuff->format_hint);
	return FSBadFormat;
    }
    if (stuff->format_mask & ~ALL_FORMAT_MASK_BITS) {
	SendErrToClient(client, FSBadFormat, (pointer) &stuff->format_mask);
	return FSBadFormat;
    }
    err = OpenFont(client, stuff->fid, stuff->format_hint, stuff->format_mask,
		   nbytes, (char *) fname);

    if (err == FSSuccess) {
	return client->noClientException;
    } else {
	return err;
    }
}
int
ProcQueryXInfo(client)
    ClientPtr   client;
{
    ClientFontPtr cfp;
    int         err,
                lendata;
    fsQueryXInfoReply reply;
    fsPropInfo *prop_info;

    REQUEST(fsQueryXInfoReq);

    REQUEST_AT_LEAST_SIZE(fsQueryXInfoReq);

    cfp = (ClientFontPtr) LookupIDByType(client->index, stuff->id, RT_FONT);
    if (!cfp) {
	SendErrToClient(client, FSBadFont, (pointer) &stuff->id);
	return FSBadFont;
    }
    reply.type = FS_Reply;
    reply.sequenceNumber = client->sequence;

    /* get the header */
    err = LoadXFontInfo(client, &cfp->font->info, &reply.header, &prop_info);

    switch (err)
    {
    case Successful:
	break;
    case AllocError:
	SendErrToClient(client, FSBadAlloc, (pointer) 0);
	return err;
    default:
	ErrorF("ProcQueryXInfo: unexpected return val %d from LoadXFontInfo",
	       err);
	SendErrToClient(client, FSBadImplementation, (pointer) 0);
	return err;
    }
    lendata = sizeof(fsPropInfo) +
	prop_info->num_offsets * sizeof(fsPropOffset) +
	prop_info->data_len;

    reply.length = (sizeof(fsQueryXInfoReply) + lendata + 3) >> 2;
    WriteReplyToClient(client, sizeof(fsQueryXInfoReply), &reply);

    if (client->swapped)
	SwapPropInfo(prop_info);
    (void) WriteToClient(client, lendata, (char *) prop_info);

    fsfree((char *) prop_info);
    return client->noClientException;
}

int
ProcQueryXExtents(client)
    ClientPtr   client;
{
    ClientFontPtr cfp;
    int         err;
    int         item_size;

    REQUEST(fsQueryXExtents8Req);

    REQUEST_AT_LEAST_SIZE(fsQueryXExtents8Req);

    cfp = (ClientFontPtr) LookupIDByType(client->index, stuff->fid, RT_FONT);
    if (!cfp) {
	SendErrToClient(client, FSBadFont, (pointer) &stuff->fid);
	return FSBadFont;
    }
    item_size = (stuff->reqType == FS_QueryXExtents8) ? 1 : 2;

    /* get the extents */
    err = QueryExtents(client, cfp, item_size,
		       stuff->num_ranges, stuff->range, (pointer) &stuff[1]);

    if (err != FSSuccess) {
	return err;
    } else
	return client->noClientException;
}

int
ProcQueryXBitmaps(client)
    ClientPtr   client;
{
    ClientFontPtr cfp;
    int         err;
    int         item_size;

    REQUEST(fsQueryXBitmaps8Req);

    REQUEST_AT_LEAST_SIZE(fsQueryXBitmaps8Req);

    cfp = (ClientFontPtr) LookupIDByType(client->index, stuff->fid, RT_FONT);
    if (!cfp) {
	SendErrToClient(client, FSBadFont, (pointer) &stuff->fid);
	return FSBadFont;
    }
    if (stuff->format & ~ALL_FORMAT_BITS) {
	SendErrToClient(client, FSBadFormat, (pointer) &stuff->format);
	return FSBadFormat;
    }
    assert((stuff->reqType == FS_QueryXBitmaps8) || (stuff->reqType == FS_QueryXBitmaps16));
    item_size = (stuff->reqType == FS_QueryXBitmaps8) ? 1 : 2;

    /* get the glyphs */
    err = QueryBitmaps(client, cfp, item_size, stuff->format,
		       stuff->num_ranges, stuff->range, (pointer) &stuff[1]);

    if (err != FSSuccess) {
	return err;
    } else {
	return client->noClientException;
    }
}

int
ProcCloseFont(client)
    ClientPtr   client;
{
    ClientFontPtr cfp;

    REQUEST(fsResourceReq);

    REQUEST_SIZE_MATCH(fsResourceReq);
    cfp = (ClientFontPtr) LookupIDByType(client->index, stuff->id, RT_FONT);
    if (cfp) {
	FreeResource(client->index, stuff->id, RT_NONE);
	return client->noClientException;
    } else {
	SendErrToClient(client, FSBadFont, (pointer) &stuff->id);
	return FSBadFont;
    }
}

void
CloseDownClient(client)
    ClientPtr   client;
{
    int client_alive = client->clientGone == CLIENT_ALIVE;

    if (client_alive) {
	client->clientGone = CLIENT_GONE;
	CloseDownConnection(client);
    }
    FreeClientResources(client);
    if (ClientIsAsleep(client))
	ClientSignal(client);
    if (client->index < nextFreeClientID)
	nextFreeClientID = client->index;
    clients[client->index] = NullClient;

    if (client_alive) {
	--nClients;
#ifdef NOTYET
	/* reset server when last client goes away */
	if (client->requestVector != InitialVector && nClients == 0)
	    dispatchException |= DE_RESET;
#endif
    } 
    if (currentClient == client)
	currentClient = serverClient;
    fsfree(client);

#ifdef DEBUG
    fprintf(stderr, "Shut down client\n");
#endif

    while (!clients[currentMaxClients - 1])
	currentMaxClients--;
}

static void
kill_all_clients()
{
    int         i;

    for (i = MINCLIENT; i < currentMaxClients; i++) {
	if (clients[i])
	    CloseDownClient(clients[i]);
    }
}

void
InitProcVectors()
{
    int         i;

    for (i = 0; i < NUM_PROC_VECTORS; i++) {
	if (!ProcVector[i]) {
	    ProcVector[i] = SwappedProcVector[i] = ProcBadRequest;
	    ReplySwapVector[i] = NotImplemented;
	}
    }
    for (i = FSLASTEvent; i < NUM_EVENT_VECTORS; i++) {
	EventSwapVector[i] = NotImplemented;
    }
}

InitClient(client, i, ospriv)
    ClientPtr   client;
    int         i;
    pointer     ospriv;
{
    client->index = i;
    client->sequence = 0;
    client->last_request_time = GetTimeInMillis();
    client->clientGone = CLIENT_ALIVE;
    client->noClientException = FSSuccess;
    client->requestVector = InitialVector;
    client->osPrivate = ospriv;
    client->swapped = FALSE;

    client->auth = (AuthContextPtr) 0;
    client->catalogues = NULL;
    client->num_catalogues = 0;
    client->num_resolutions = 0;
    client->resolutions = (fsResolution *) 0;
    client->eventmask = (Mask) 0;
}

ClientPtr
NextAvailableClient(ospriv)
    pointer     ospriv;
{
    int         i;
    ClientPtr   client;
    fsReq       data;
    extern long MaxClients;

    i = nextFreeClientID;
    if (i == MaxClients)
	return NullClient;

    clients[i] = client = (ClientPtr) fsalloc(sizeof(ClientRec));
    if (!client)
	return NullClient;

    InitClient(client, i, ospriv);

    if (!InitClientResources(client)) {
	fsfree(client);
	return NullClient;
    }
    data.reqType = 1;
    data.length = (sz_fsReq + sz_fsConnClientPrefix) >> 2;
    if (!InsertFakeRequest(client, (char *) &data, sz_fsReq)) {
	FreeClientResources(client);
	fsfree(client);
	return NullClient;
    }
    if (i == currentMaxClients)
	currentMaxClients++;
    while ((nextFreeClientID < MAXCLIENTS) && clients[nextFreeClientID])
	nextFreeClientID++;

    /* if we've maxed out, try to clone */
    if (nextFreeClientID == MaxClients) {
	CloneMyself();
    }
    return client;
}

MarkClientException(client)
    ClientPtr   client;
{
    client->noClientException = -2;
}
