/*
Copyright (c) 1991 Bell Communications Research, Inc. (Bellcore)

Permission to use, copy, modify, and distribute this material 
for any purpose and without fee is hereby granted, provided 
that the above copyright notice and this permission notice 
appear in all copies, and that the name of Bellcore not be 
used in advertising or publicity pertaining to this 
material without the specific, prior written permission 
of an authorized representative of Bellcore.  BELLCORE 
MAKES NO REPRESENTATIONS ABOUT THE ACCURACY OR SUITABILITY 
OF THIS MATERIAL FOR ANY PURPOSE.  IT IS PROVIDED "AS IS", 
WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES.
*/
/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1989 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
/*
	Include file for /usr/andrew/lib/libmail.a.
*/

extern char *ams_genid();	/* Generates a world-unique identifier; Boolean parameter specifies whether or not to generate a long name */
extern char *convlongto64();	/* converts a long and padding to 6 bytes base 64 */
extern unsigned long conv64tolong(); /* reverses the above conversion, basically */
extern char *arpadate();	/* Returns pointer to current ASCII date string in RFC821 format */
extern char *EX_Messages[];	/* Text messages for sysexits.h */
extern int EX_Nerr;	/* Number of error messages in above table */
extern char *UnixSysExits();	/* Pass it a sysexits value and it returns a static description */

/* From vmail.c: */
extern char VM_text[];	/* loaded with readable message for all vmail ops */
extern int VM_errordetail;	/* loaded with a more specific error code from vmail ops */
extern int VM_open();	/* pass User, Mailbox, ReturnPath, For, and Auth (all char*) */
extern int VM_write();	/* pass addr of characters and number of chars */
extern int VM_printf();	/* pass format string and parameters */
extern int VM_close();	/* no parameters necessary */
extern int VM_SetTiming();	/* pass Boolean for whether to do timing--flag cleared after VM_close */
/* Values for VM_errordetail */
#define vm_ViceDown 1
#define vm_NotADir 2
#define vm_NotOnVice 3
#define vm_InternalError 4
#define vm_NoSuchUser 5
#define vm_AlreadyOpen 6
#define vm_NotOpen 7
#define vm_BadParameters 8
#define vm_OverQuota 9
#define vm_OutOfRetries 10
#define vm_OutOfMemory 11
#define vm_DirMayHaveMoved 12
#define vm_SystemErrorOffset 100	/* added to errno values */


/* extern char *LocalMailHostname;	Name of this host for mail purposes */
extern int IsLocalAlias();	/* Says whether a name is a nickname for the local mail hostname */

extern void CanonicalizePersonName();	/* Interprets its char* argument as a person's
			   name and canonicalizes it, handling embedded spaces and dots.
			   Overwrites its argument.
			   The process turns ``a.b'' into ``a b'' and ``a..b'' into ``a. b''. */

struct ScribeState {
    int	statecode;	/* current state */
    int	previous_state;	/* last state, before \ processing */
    int	writecount;

    /* The following are only used by Version 10 and higher */
    int	begindatacount;	/* depth in dataobject tree. */
    int	keywordpos;	/* position in keyword buffer. */
    char *linefrag;	/* Line buffer. */
    char *keyword;	/* Keyword buffer. */
    char *stylebuf;	/* Buffer for {} contents. Used to name views */
    short lmargin;	/* left margin of current line */
    short rmargin;	/* right margin of current line */
    short justify;	/* justification for this line */
    short indent;	/* indentation for this line */
    short specials;	/* count of extra characters needed at end of line */
    short face;		/* face state from previous line */
    short newpara;    /* flag: indicates that the first line of the paragraph is yet to be printed */
    struct StateVector *vector;	/* Stack of style states kept as a linked list */
};

extern int UnScribeInit();
		/* int UnScribeInit(fieldvalue, refstate)
			char *fieldvalue;
			struct ScribeState **refstate;
		Pass it the value of the X-Andrew-ScribeFormat: header
		to see if the package can handle this format.
		Returns a code value >= 0 for OK, < 0 for error.
		Remember this code value to pass to UnScribe.
		In addition, pass the address of a variable to hold the
		UnScribe state between calls.  This variable will be
		initialized (space malloced, too) with the UnScribeInit call.
		Error value of -1 means that the field value wasn't recognized;
		error value of -2 means that a malloc failed.
		*/

extern int UnScribe();
		/* int UnScribe(code, refstate, text, textlen, fileptr)
			int code, textlen;
			struct ScribeState **refstate;
			char *text;
			FILE *fileptr;
		Pass it the initial UnScribeInit return value and the address
		of the integer state variable.  Also pass the address of the
		text to be unscribed, and the number of bytes at that address.
		The unscribed text will be written onto stdio file *fileptr.
		Return values are >= 0 for OK (the number of characters
		written), < 0 for errors.  Error value -1 means
		that errno holds a useful code.	*/
extern int UnScribeFlush();
		/* int UnScribeFlush(code, refstate, fileptr)
			int code;
			struct ScribeState **refstate;
			FILE *fileptr;
		Call this to unbuffer an UnScribe sequence.  If the UnScribe
		routine has buffered any data, this call cues it to un-buffer it.
		Returns zero for OK, non-zero for failures.
		This routine also frees up the refstate.
		*/
extern int UnScribeAbort();
		/* int UnScribeAbort(code, refstate)
			int code;
			struct ScribeState **refstate;
		Call this to undo (abortively) an UnScribeInit without needing
		an active stdio file to which to write buffered data.
		Returns 0 for all OK, non-zero for (presumably ignorable) errors.
		*/
extern int PrintQuotingFormatting();
		/* extern int PrintQuotingFormatting(fp, text, format, len)
			FILE *fp;
			char *text, *format;
			int len;
		Call this to write LEN chars from TEXT to file FP, assuming that
		you want it encoded in format FORMAT (e.g., ``yes'', ``2'', or ``10'').
		Returns the number of characters written, or 0 or -1 for errors (like fwrite).
		*/
extern int PrintMaybeFoldQuotingFormatting();
		/* extern int PrintMaybeFoldQuotingFormatting(fp, text, format, len, DoFold)
			FILE *fp;
			char *text, *format;
			int len, DoFold;
		Call this to write LEN chars from TEXT to file FP, assuming that
		you want it encoded in format FORMAT (e.g., ``yes'', ``2'', or ``10'').
		Returns the number of characters written, or 0 or -1 for errors (like fwrite).
		Make DoFold non-zero to allow line folding.
		*/

enum MailHostQuality {mailhost_good, mailhost_bad, mailhost_indeterminate};

extern enum MailHostQuality ValidateMailHostName();
extern enum MailHostQuality ValidateDomainMail();

/* Stuff for interpretation of local addresses (spec in la.spec) */
struct MailDom {	/* pointed to by parsed addresses; result of valhost; managed in locaddr */
	struct MailDom	*Next, *Prev;
	int		Refs;		/* Reference count */
	char		*Orig, *Final;
	enum MailHostQuality	Qual;
	unsigned long int	DomainAddress;
	unsigned int	NumFwds;
	unsigned short int	*FwdPrefs;
	char		**Fwds;
};

#define latype_Remote	0	/* addr->MD->Qual might say if good or bad host name */
#define latype_LocalID	1
#define latype_LocalName	2
#define latype_DistList	3
#define latype_DirInsert	4
#define latype_FSMembers	5
/* and more to follow.. */

#define laerr_NoError	0	/* no problem */
#define laerr_OutOfMemory	1	/* a malloc() failed */
#define laerr_SyntaxError	2	/* the local part had bad quoting syntax */
#define laerr_UnrecSpecial	3	/* local part ``+foo+bar'' for unrecognized ``foo'' */
#define laerr_WPerror	4	/* la_Resolve returns this sometimes */
#define laerr_BadSecond	5	/* local address is foo+bar, where bar has white space. */

extern int la_Kind();	/* la_Kind(Addr, outType, outPrime, outSecond)
			PARSED_ADDRESS *Addr;
			int *outType; -- returns one of the latype_XXX codes
			char **outPrime, **outSecond;	*/
extern int la_KindDomain();	/* la_KindDomain(Addr, outType, outPrime, outSecond, Domain)
			PARSED_ADDRESS *Addr;
			int *outType; -- returns one of the latype_XXX codes
			char **outPrime, **outSecond; char *Domain	*/
extern char *la_ErrorString();	/* given a laerr_XXX code, tell you what the code means */

extern int la_Resolve();	/* la_Resolve(Addr, PrimePart, outSearchToken, outMinFound,
				MaxQuality, outMatchQuality, outPrimeKey)
			PARSED_ADDRESS *Addr; char *PrimePart;
			wp_SearchToken *outSearchToken;
			int *outMinFound, MaxQuality, *outMatchQuality;
			wp_PrimeKey *outPrimeKey;	*/

extern int BracketField();	/* Searches for the contents of the named field. */

extern int IsOK822Atom();	/* TRUE iff the char argument could be part of an RFC822 Atom */
#define is822Atom 1	/* Return codes from NextWord */
#define is822QuotedString 2
#define is822Special 3
#define is822End 4
extern int Next822Word();	/* next word */
extern char *Next822LPart();	/* next local-part */
extern char *Next822Phrase();	/* next phrase */
extern char *Quote822LPart();	/* quote a local-part */
extern char *Quote822Phrase();	/* quote a phrase */

extern int CheckAMSDelivery();	/* Hand it a cell name.  Returns >0 if that cell is running AMS delivery, <0 if they're not, 0 if it can't tell. */
extern int CheckAMSNameSep();	/* Hand it a cell name; it determines what that cell uses as its space-substitution in validated mail names.  Returns -1 if no, 0 if you can't tell, and >0 if it does.  If the value is >0, it's the separator character itself. */
extern char *CheckAMSMBName();	/* Hand it a cell name.  Returns what that cell uses as the name of its mail in-box directory, that accounts use to receive mail.  Returns NULL if it can't tell. */
extern char *CheckAMSPMName();	/* Hand it a cell name.  Returns what that cell uses as the username of its distinguished delivery agent.  Returns NULL if it can't tell. */
extern char *CheckAMSWPIAddr();	/* Hand it a cell name.  Returns what that cell uses as the address for WPI update requests.  Returns NULL if it can't tell. */
extern int CheckAMSFmtOK(); /* Hand it a domain name.  Returns >0 if the domain accepts ATK-formatted mail, <0 if it doesn't, and 0 if we can't tell. */
extern int CheckAMSUUCPSupp(); /* Hand it a domain name.  Returns >0 if the domain thinks a!b is a remote address, <0 if it doesn't, and 0 if we can't tell. */
#define	vld_WPValid	0x1	/* AMS_WPValidation: 1 */
#define	vld_PasswdValid	0x2	/* AMS_PasswdValidation: 1 */
#define	vld_LocalDBValid	0x4	/* AMS_LocalDatabaseValidation: 1 */
#define	vld_AliasesValid    0x8	/* AMS_AliasesValidation: 1 */
extern int CheckAMSValidationMask(); /* Hand it a cell name.  Returns a mask for how that cell does its name validation.  Returns an int <0 if it can't find out. */
/* Structure to describe a site's default MSPATH. */
struct cell_msPath {
    char    *Abbrev;	/* e.g. "local" or "external" */
    char    *RootDir;	/* the root directory of that mspath element. */
    char    *Decorate;	/* any square-bracketed decoration on the mspath element */
    char    *RootCell;	/* the AFS cell in which this root sits */
    int	Validated:1;	/* whether this one has been checked */
    int	CkMailbox:1;	/* whether we check this one's mailbox */
};
extern int CheckAMSDfMSPath();	/* Given a cell name as its first parameter, it returns a count of cell_msPaths returned as an array via its second parameter.  Returns 0 if there are none.  Returns <0 if the domain name isn't running AMS delivery or there's some problem in finding the list. */

/* Functions to describe a user's authentication in multiple cells */
extern char AMSHome_errmsg[];	/* Text describing the result of the FindAMSHomeCell call. */
extern void ForgetAMSHome();	/* Forget the AMS home cell and everything else. */
extern int FindAMSHomeCell();	/* int FindAMSHomeCell(ppCellAuth)
				struct CellAuth **ppCellAuth;
		Returns a pointer to what the AMS thinks its home cell should be, if there is one.
		Return 1 if there's no such cell, or 2 if there's no authentication at all. */

extern int SetAMSHomeCell();	/* int SetAMSHomeCell(cellAuth)
				struct CellAuth *cellAuth;
		Declare that the given cellAuth should be marked as being the AMS home cell.
		Can fail if the cell isn't the workstation's cell and the given cell isn't running AMS delivery. */


/**** from fwdvalid.c ****/
/*  fwdvalid_msgbuf:
    ----------------
    The char array where ValidateFwdAddr will put diagnostics.
    */
extern char fwdvalid_msgbuf[];


/* fwdvalid_SetTildeUser:
   ----------------------
   Takes a string, and uses that to resolve addresses of the form
   "+dir-insert+~/..." in later calls to ValidateFwdAddr. */
extern void fwdvalid_SetTildeUser(/* [IN] char *s */);

/* ValidateFwdAddr:
   ----------------
   Takes a string (NewAddr), and checks it for validity as a (forwarding)
   mail address.  Returns 0 if successful.  The "canonicalized"
   form of NewAddr will be returned in FixedAddr (malloc'd).  Diagnostics
   can be found in fwdvalid_msgbuf.  */
extern int ValidateFwdAddr(/* [IN]  char *NewAddr, 
			      [OUT] char **FixedAddr */);
