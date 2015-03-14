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
/****************************************************** 
    Metamail -- A tool to help diverse mail readers 
                cope with diverse multimedia mail formats.

    Author:  Nathaniel S. Borenstein, Bellcore

 ******************************************************* */
#include <stdio.h>
#include <ctype.h>
#include <config.h>

#ifdef __MSDOS__
extern unsigned _stklen = 16384;
extern char *mktemp(char *);

#else /* __MSDOS__ */

#include <pwd.h>
#include <sys/time.h>
#include <signal.h>

#ifndef AMIGA
#ifdef SYSV
#include <termio.h>
#else /* SYSV */
#include <sgtty.h>
#endif /* SYSV */
#endif /* AMIGA */
#endif /* __MSDOS__ */

#ifdef AMIGA
extern char *MkRmScript();
#define CATCOMMAND  "Type"
#define CATTEMPLATE "Type %s"
#define METAMAIL    "metamail <*"
#define TMPFILE_NAME_SIZE     50
#define MAX_FILE_NAME_SIZE 256
#else
extern char **environ, *gets();
#define CATCOMMAND  "cat"
#define CATTEMPLATE "cat %s"
#define METAMAIL    "metamail"
#define TMPFILE_NAME_SIZE   1000
#define MAX_FILE_NAME_SIZE 1000
#endif

#ifndef NO_RLIMITS
#include <sys/resource.h>
#endif

#define CMDSIZE 1200 /* Maximum size of command to execute */

#define LINE_BUF_SIZE       2000
extern char *malloc();
extern char *realloc();
extern char *getenv();
extern char *index();
extern char *rindex();
char fileToDelete[MAX_FILE_NAME_SIZE];

char *FindParam();
extern FILE *popen();
static char *nomem = "Out of memory!";
static char *mmversion = "2.3a";
static char *NoAskDefault = "text,text/plain,text/richtext";
static char *QuietDefault = CATCOMMAND;

struct MailcapEntry {
    char *contenttype;
    char *command;
    char *testcommand;
    int needsterminal;
    int copiousoutput;
    int needtofree;
};

FILE *InputFP = NULL;

int MightAskBeforeExecuting = 1,
    DefinitelyNotTty = 0,
    MustNotBeTty = 0,
    MaybePageOutput = 0,
    MustPageOutput = 0,
    EatLeadingNewlines = 0,
    PrintSomeHeaders = 1,
    DoInBackground = 0,
    Quiet = 0,
    TransparentMode = 0,
    DeleteSourceFileWhenDone = 0,
    Is822Format = 1,
    DoDebug = 0,
    CParamsAlloced = 0,
    CParamsUsed = 0,
    YankMode = 0,
    UsingStandardInput = 0;

char *ContentType = NULL,
    *ContentEncoding = NULL,
    *MailerName = "unknown",
    *MailSubject = "Mail message",
    *MailFrom = "unknown sender",
    *MailSummary = "non-text mail message",
    *mailheaders = NULL,
    **CParams = NULL,
    **CParamValues = NULL,
    *JunkParameter = NULL;

#define ENCODING_NONE 0
#define ENCODING_BASE64 1
#define ENCODING_QUOTEDPRINTABLE 2
#define ENCODING_8BIT 3
#define ENCODING_UUENCODE -1	/* non-standard */
int EncodingCode = ENCODING_NONE;

struct NoAskItem {
    char *type;
    struct NoAskItem *next;
} *FirstNoAskItem = NULL,
  *FirstQuietItem = NULL;

sigtype cleanup();

char *Cleanse(s) /* no leading or trailing space, all lower case */
char *s;
{
    char *tmp, *news;
    
    /* strip leading white space */
    while (*s && isspace((unsigned char) *s)) ++s;
    news = s;
    /* put in lower case */
    for (tmp=s; *tmp; ++tmp) {
        if (isupper((unsigned char) *tmp)) *tmp = tolower((unsigned char) *tmp);
    }
    /* strip trailing white space */
    while (*--tmp && isspace((unsigned char) *tmp)) *tmp = 0;
    return(news);
}

char *UnquoteString(s)
char *s;
{
    char *ans, *t;

    if (*s != '"') return(s);
    ans = malloc(1+strlen(s));
    if (!ans) ExitWithError(nomem);
    ++s;
    t = ans;
    while (*s) {
        if (*s == '\\') {
            *t++ = *++s;
        } else if (*s == '"') {
            break;
        } else {
            *t++ = *s;
        }
        ++s;
    }
    *t = 0;
    return(ans);
}

sigtype
cleanup(signum) 
int signum;
{
#ifdef __MSDOS__
    exit(signum);
#else
#ifdef AMIGA
    exit(signum);
#else
    signal(signum, SIG_DFL);
    kill(getpid(), signum);
#endif
#endif
}

char **Boundaries = NULL;
int BoundaryCt = 0, BoundaryAlloc = 0;
struct nextfile {
    char *filename;
    struct nextfile *next;
} *FileQueue=NULL, *LastInQueue = NULL;


main(argc, argv)
int argc;
char **argv;
{
    int retcode;

#ifndef __MSDOS__
    signal(SIGINT, cleanup);
#ifndef AMIGA
    signal(SIGILL, cleanup);
    signal(SIGTRAP, cleanup);
    signal(SIGIOT, cleanup);
    signal(SIGEMT, cleanup);
    signal(SIGFPE, cleanup);
    signal(SIGBUS, cleanup);
    signal(SIGSEGV, cleanup);
    signal(SIGTERM, cleanup);
#endif
#endif
#ifdef SIGXCPU
    signal(SIGXCPU, cleanup);
#endif
    mailheaders = getenv("MM_HEADERS");
    if (mailheaders) {
        char *s;
        s = malloc(15+strlen(mailheaders));
        if (!s) ExitWithError(nomem);
        sprintf(s, "MM_HEADERS=%s", mailheaders);
        mailheaders = s;
    }
    fileToDelete[0] = 0;
    ProcessArguments(argc, argv); /* calls ExitWithError on error */
    retcode = HandleMessage(NULL, 0);
    if (! UsingStandardInput) {
        fclose(InputFP);
    }
    if (fileToDelete[0]) {
        unlink(fileToDelete);
        fileToDelete[0] = 0;
    }
    while (FileQueue) {
        InputFP = fopen(FileQueue->filename, "r");
        if (!InputFP) ExitWithError("Can't read input file");
        if (DeleteSourceFileWhenDone) {
            strcpy(fileToDelete, FileQueue->filename);
        }
        ResetGlobals();
        retcode |= HandleMessage(NULL, 0);
        if (! UsingStandardInput) {
            fclose(InputFP);
        }
        if (fileToDelete[0]) {
            unlink(fileToDelete);
            fileToDelete[0] = 0;
        }
        FileQueue = FileQueue->next;
    }
    if (MustPageOutput) {
        char AnsBuf[100];
        printf("Press RETURN to go on.\n");
        gets(AnsBuf);
    }
    if (DoDebug) printf("Exiting with status: %d\n", retcode);
    exit(retcode);
}

QueueNextFile(fname)
char *fname;
{
    struct nextfile *tmp = (struct nextfile *) malloc(sizeof (struct nextfile));
    if (!tmp) ExitWithError(nomem);
    tmp->filename = fname;
    tmp->next = NULL;
    if (FileQueue) {
        LastInQueue->next = tmp;
        LastInQueue = tmp;
    } else {
        FileQueue = tmp;
        LastInQueue = tmp;
    }
}

ResetGlobals() {
    CParamsAlloced = 0;
    CParamsUsed = 0;

    ContentType = NULL;
    ContentEncoding = NULL;
    MailSubject = "Mail message";
    MailFrom = "unknown sender";
    MailSummary = "non-text mail message";
    mailheaders = getenv("MM_HEADERS");
    if (mailheaders) {
        char *s;
        s = malloc(15+strlen(mailheaders));
        if (!s) ExitWithError(nomem);
        sprintf(s, "MM_HEADERS=%s", mailheaders);
        mailheaders = s;
    }
    CParams = NULL;
    CParamValues = NULL;
    JunkParameter = NULL;
}

HandleMessage(SquirrelFile, nestingdepth)
char *SquirrelFile;
/* SquirrelFile, if non-NULL, is a place to save a recognized body instead of executing it. */
int nestingdepth;
{
    if (Is822Format) {
        Read822Prefix(SquirrelFile?0:1, nestingdepth);
    } else Is822Format = 1; /* this property is not recursive for multipart or message */
    PrepareMessage();
    if (!ProcessMailcapFiles(SquirrelFile)) return(0);
    if (!lc2strcmp(ContentType, "message")
         || !lc2strcmp(ContentType, "message/rfc822")) {
        if (SquirrelFile) return(SaveSquirrelFile(SquirrelFile));
        ContentType = NULL; /* reset default */
        ContentEncoding = NULL; /* reset default */
        return(HandleMessage(NULL, nestingdepth+1)); /* simple recursion */
    }
    if (!lc2strncmp(ContentType, "multipart", 9)) {
        char *boundary, *LineBuf, NewSquirrelFile[TMPFILE_NAME_SIZE];
        char *subtype = NULL;
        int currct, result, IsAlternative, WroteSquirrelFile;

        if (SquirrelFile) return(SaveSquirrelFile(SquirrelFile));
        boundary = FindParam("boundary");
        if (!boundary) boundary =JunkParameter; /* backward compatibility hack */
        if (!boundary) {
            ExitWithError("Bad message format -- multipart message has no boundary parameter!");
        }
        if (boundary[0] == '"') {
            boundary=UnquoteString(boundary);
        }
        subtype = index(ContentType, '/');
        if (subtype) {
            ++subtype;
            subtype = Cleanse(subtype);
        } else subtype = "mixed";
#ifndef __MSDOS__
        DoInBackground = strcmp(subtype, "parallel") ? 0 : 1;
#endif

        IsAlternative = strcmp(subtype, "alternative") ? 0 : 1;
        if (IsAlternative) {
            MkTmpFileName(NewSquirrelFile);
            WroteSquirrelFile = 0;
        }
        LineBuf = malloc(LINE_BUF_SIZE);
        if (!LineBuf) ExitWithError(nomem);
        sprintf(LineBuf, "--%s", boundary);
        strcpy(boundary, LineBuf);
        if (BoundaryCt >= BoundaryAlloc) {
            BoundaryAlloc += 5;
            if (Boundaries) {
                Boundaries = (char **) realloc(Boundaries, BoundaryAlloc*sizeof(char *));
            } else {
                Boundaries = (char **) malloc(BoundaryAlloc*sizeof(char *));
            }
            if (!Boundaries) ExitWithError(nomem);
        }
        Boundaries[BoundaryCt++] = boundary;
        if (DoDebug) printf("Handling multipart as built-in here.  Boundary: %s\n", boundary);
        while (fgets(LineBuf, LINE_BUF_SIZE, InputFP)) { /* find start */
            if (!strncmp(LineBuf, boundary, strlen(boundary))) break;
        }
        free(LineBuf);
        currct = BoundaryCt;
        while(currct == BoundaryCt) {
            if (!strcmp(subtype, "digest")) {
                ContentType = "message/rfc822";
            } else {
                ContentType = NULL; /* reset default */
            }
            ContentEncoding = NULL; /* reset default */
            if (IsAlternative) {
                result = HandleMessage(NewSquirrelFile, nestingdepth+1);
            } else{
                result = HandleMessage(NULL, nestingdepth+1);
            }
            if (result) {
                /* Need to consume the rest of the part */
                ConsumeRestOfPart(NULL);
            } else {
                ++WroteSquirrelFile;
            }
        }
        /* Now we've seen the last encapsulation boundary, but if there is a "postfix"
            we must throw it away.*/
        if (BoundaryCt > 0) {
            ConsumeRestOfPart(NULL);
        }
        if (IsAlternative) {
            if (WroteSquirrelFile) {
                int retcode; 
                char Cmd[TMPFILE_NAME_SIZE + 15];
                sprintf(Cmd, "%s %s", METAMAIL, NewSquirrelFile);
                fflush(stdout); fflush(stderr);
                retcode = system(Cmd);
                unlink(NewSquirrelFile);
                return(retcode);
            } else {
                printf("Cannot handle any part of multipart/alternative message\n");
            }
        }
        return(0);
    }
    if (!TryBuiltIns(SquirrelFile)) return(0);
    if (!SquirrelFile) { /* Very last resort -- unrecognized types */
        char Fname[MAX_FILE_NAME_SIZE];
        FILE *fp;
        int ans = 0;
        if (MightAskBeforeExecuting
             && !DefinitelyNotTty) {
            printf("\nThis message contains data in an unrecognized format, %s,\nwhich can either be viewed as text or written to a file.\n", ContentType);
            while (!ans) {
                printf("\nWhat do you want to do with the '%s'-format data?\n1 -- See it as text\n2 -- Write it to a file\n3 -- Just skip it\n\n", ContentType);
                fgets(Fname, sizeof(Fname), stdin);
                ans = atoi(Fname);
                switch(ans) {
                    case 1:
                        TranslateInputToOutput(InputFP, stdout, EncodingCode);
                        return(0);
                    case 2:
                        printf("Please enter the name of a file to which the data should be written:\n> ");
                        fflush(stdout);
                        fgets(Fname, sizeof(Fname), stdin);
                        Fname[strlen(Fname) - 1] = '\0'; /* bogus newline */
#ifndef AMIGA
#ifndef __MSDOS__
                        if (Fname[0] == '~' && Fname[1] == '/') {
                            char Fname2[1000];
                            int uid = getuid();
                            struct passwd *p;
                            p = getpwuid(uid);
                            if (!p) {
                                MkTmpFileName(Fname);
                                printf("Cannot figure out what ~ means, using temporary file %s instead\n", Fname);
                            } else {
                                strcpy(Fname2, p->pw_dir);
                                strcat(Fname2, "/");
                                strcat(Fname2, Fname + 2);
                                strcpy(Fname, Fname2);
                            }
                        }
#endif
#endif
                        break;
                    case 3:
                        return(1);
                        break;
                    default:
                        ans = 0;
                        break;
                }
            }
        } else {
            MkTmpFileName(Fname);
            printf("\nThis message contains data in an unrecognized format, %s,\nwhich is being decoded and written to the file named %s.\nIf you do not want this data, you probably should delete that file.\n", ContentType, Fname);
        }
        if (Fname[0] == 0 || Fname[0] == '\n') {
            ConsumeRestOfPart(NULL);
            return(0);
        } else {
            fp = fopen(Fname, "w");
            if (!fp) ExitWithError("Cannot open temporary file");
            TranslateInputToOutput(InputFP, fp, EncodingCode);
            return(fclose(fp));
        }
    }
    return(-1); /* Unrecognized, really */
}

ProcessArguments(argc, argv)
int argc;
char **argv;
{
    int i, RunAsRootOK = 0;
    char *SourceFileName = NULL, *NoAskStr, *QuietStr;

    QuietStr = getenv("MM_QUIET");
    if (!QuietStr) {
        QuietStr=QuietDefault;
    }
    if (!strcmp(QuietStr, "1")) {
        Quiet = 1;
    } else {
        struct NoAskItem *qitem;
        char *s, *tmp;
        char *QuietCopy;

        Quiet = 0;
        QuietCopy = malloc(1+strlen(QuietStr));
        if (!QuietCopy) ExitWithError(nomem);
        strcpy(QuietCopy, QuietStr);
        for (tmp=QuietCopy; *tmp; ++tmp) {
            if (isupper((unsigned char) *tmp)) *tmp = tolower((unsigned char) *tmp);
        }
        do {
            s = index(QuietCopy, ',');
            if (s) *s++ = 0;
            qitem = (struct NoAskItem *) malloc(sizeof (struct NoAskItem));
            if (!qitem) ExitWithError(nomem);
            qitem->next = FirstQuietItem;
            qitem->type = QuietCopy;
            FirstQuietItem = qitem;
            QuietCopy = s;
        } while (QuietCopy);
    }
    if (getenv("MM_TRANSPARENT")) {
        TransparentMode = atoi(getenv("MM_TRANSPARENT")); /* Will not propogate recursively */
    }
    if (getenv("MM_YANKMODE")) {
        YankMode = atoi(getenv("MM_YANKMODE")); /* Will not propogate recursively */
    }
    if (getenv("MM_DEBUG")) {
        DoDebug = atoi(getenv("MM_DEBUG"));
    }
    if (DoDebug) printf("Metamail Version %s, debugging turned on.\n", mmversion);
    NoAskStr = getenv("MM_NOASK");
    if (!NoAskStr) NoAskStr = NoAskDefault;
    if (!strcmp(NoAskStr, "1")) {
        MightAskBeforeExecuting = 0;
    } else {
        struct NoAskItem *nai;
        char *s, *tmp;
        char *NoAskCopy;

        NoAskCopy = malloc(1+strlen(NoAskStr));
        if (!NoAskCopy) ExitWithError(nomem);
        strcpy(NoAskCopy, NoAskStr);
        for (tmp=NoAskCopy; *tmp; ++tmp) {
            if (isupper((unsigned char) *tmp)) *tmp = tolower((unsigned char) *tmp);
        }
        do {
            s = index(NoAskCopy, ',');
            if (s) *s++ = 0;
            nai = (struct NoAskItem *) malloc(sizeof (struct NoAskItem));
            if (!nai) ExitWithError(nomem);
            nai->next = FirstNoAskItem;
            nai->type = NoAskCopy;
            FirstNoAskItem = nai;
            NoAskCopy = s;
        } while (NoAskCopy);
    }
    MailerName = getenv("MM_MAILER");
    if (!MailerName) MailerName = "unknown";
    if (getenv("MM_USEPAGER")) {
        MaybePageOutput = atoi(getenv("MM_USEPAGER"));
    }
    if ((getenv("MM_NOTTTY") && ((atoi(getenv("MM_NOTTTY"))) != 0))) {
        MustNotBeTty = 1;
    }
    if (MustNotBeTty
         || !isatty(0)
         || !isatty(1)) {
        DefinitelyNotTty = 1;
    }
    for (i=1; i<argc; ++i) {
	if (argv[i][0] == '-') {
	    switch (argv[i][1]) {
		case 'b':
		    Is822Format = 0;
		    break;
		case 'B':
#ifdef __MSDOS__
                    fprintf(stderr, "metamail warning: -B flag not supported on this system\n");
#else
		    DoInBackground = 1;
#endif
		    break;
		case 'c':
		    if (++i >= argc) usage();
		    ContentType = argv[i];
                    ParseContentParameters(ContentType);
		    break;
		case 'd':
		    MightAskBeforeExecuting = 0;
		    break;
		case 'e':
		    EatLeadingNewlines = 1;
		    break;
		case 'E':
		    if (++i >= argc) usage();
		    ContentEncoding = argv[i];
		    break;
		case 'f':
		    if (++i >= argc) usage();
		    MailFrom = argv[i];
		    break;
		case 'm':
		    if (++i >= argc) usage();
		    MailerName = argv[i];
		    break;
		case 'p':
		    MaybePageOutput = 1;
		    break;
		case 'P':
                    MaybePageOutput = 1;
                    MustPageOutput = 1;
		    break;
		case 'r':
#ifdef __MSDOS__
                    fprintf(stderr, "metamail warning: -r flag not supported on this system\n");
#else
		    RunAsRootOK = 1;
#endif
                    break;
                case 'R':
#ifdef RESET_PROGRAM
                    system(RESET_PROGRAM);
                    if (DoDebug) printf("Executed reset\n");
#else
                    fprintf(stderr, "metamail warning: -R flag not supported on this system\n");
#endif
                    break;
		case 's':
		    if (++i >= argc) usage();
		    MailSubject = argv[i];
		    break;
                case 'T':
                    TransparentMode = 0;
                    break;
                case 'q':
                    Quiet = 1;
		    PrintSomeHeaders = 0;
		    break;
		case 'x':
                    DefinitelyNotTty = 1;
                    MustNotBeTty = 1;
		    break;
                case 'y':
                    YankMode = 1;
                    break;
		case 'z':
		    DeleteSourceFileWhenDone = 1;
		    break;
		default:
		    usage();
	    }
	} else {
	    if (SourceFileName) {
                QueueNextFile(argv[i]);
	    } else {
		SourceFileName = argv[i];
	    }
	}
    }
    if (TransparentMode) {
        RunInNewWindow(argv, argc, &SourceFileName);
    }
    if (!Is822Format && !ContentType) {
	fprintf(stderr, "metamail:  -b requires -c.\n");
	usage();
    }
    if (DeleteSourceFileWhenDone && !SourceFileName) {
	fprintf(stderr, "metamail:  -z requires -f.\n");
	usage();
    }
#ifndef __MSDOS__
#ifndef AMIGA
    if (!RunAsRootOK && (getuid() == 0 || geteuid() == 0)) {
	fprintf(stderr, "You can not run MetaMail as root unless you use -r.");
	usage();
    }
#endif
#endif
    if (SourceFileName) {
        InputFP = fopen(SourceFileName, "r");
        if (!InputFP) ExitWithError("Can't read input file");
        if (DeleteSourceFileWhenDone) {
            strcpy(fileToDelete, SourceFileName);
        }
    } else {  /* input on stdin */
        UsingStandardInput = 1;
        if (MustNotBeTty) {
            InputFP = stdin;
            MaybePageOutput = 1;
            MightAskBeforeExecuting = 0;
        } else {
#if defined(__MSDOS__) || defined(AMIGA)
            InputFP = stdin;
            DefinitelyNotTty = 1;
            MaybePageOutput = 0;
#else
            int newfd = dup(0);
            FILE *newfp;
            if (newfd > 0) {
                InputFP = fdopen(newfd, "r");
                if (InputFP 
                     && ((newfp = fopen("/dev/tty", "r")) != NULL)
                     && !dup2(fileno(newfp), 0)) {
                    DefinitelyNotTty = 0;
                } else {
                    InputFP = stdin;
                    DefinitelyNotTty = 1;
                    MaybePageOutput = 0;
                }
            }
#endif
        }
    }
    if (DefinitelyNotTty && MaybePageOutput) {
        RunInNewWindow(argv, argc, &SourceFileName);
    }
    if (DefinitelyNotTty) {
        MaybePageOutput = 0;	/* Disable pager if I/O has been redirected */
    }
    return(0);
}

usage() {
    fprintf(stderr, "Usage:  metamail [-b] [-B] [-d] [-e] [-r] [-R] [-p]  [-P] [-x] [-y] [-z] [-c content-type] [-E content-transfer-encoding] [-f from-name] [-m mailername] [-s subject] [message-file-name]\n");
    ExitWithError(NULL);
}

RunInNewWindow(argv, argc, SourceFileNamePtr)
char **argv, **SourceFileNamePtr;
int argc;
{
    char *FullCmd, TmpName[TMPFILE_NAME_SIZE];
    int i, createdfile=0;
    if (!*SourceFileNamePtr) {
        char *LineBuf;
        FILE *fptmp;

        LineBuf = malloc(LINE_BUF_SIZE);
        if (!LineBuf) ExitWithError(nomem);
        /* Create it, ugh.  Also needs to affect later command. */
        MkTmpFileName(TmpName);
        DeleteSourceFileWhenDone = 1;
        fptmp = fopen(TmpName, "w");
        if (!fptmp) ExitWithError("Can't open temporary file\n");
        while (fgets(LineBuf, LINE_BUF_SIZE, stdin)) {
            fputs(LineBuf, fptmp);
        }
        free(LineBuf);
        fclose(fptmp);
        *SourceFileNamePtr = TmpName;
        createdfile = 1;
    }
    FullCmd = malloc(CMDSIZE);
    if (!FullCmd) ExitWithError(nomem);
    if (TransparentMode) {
        /* In transparent mode, we want to produce stdout that is what we get in, and do EVERYTHING externally in a terminal window.  This is to make the truly brain-dead mailers like mailtool happy. I am NOT happy about having to do this.  */
        /* So, first we copy everything to stdout */
        sprintf(FullCmd, CATTEMPLATE, *SourceFileNamePtr);
        system(FullCmd); /* Cheesy way to do it */
        fflush(stdout); fflush(stderr);
    }
    /* Then we run ourselves in a terminal window */
    MailSummary = "Metamail"; /* for window label */
    CreateNewWindowPrefix(FullCmd);
    strcat(FullCmd, METAMAIL);
    strcat(FullCmd, " -P ");
    if (TransparentMode) strcat(FullCmd, "-T ");
    for (i=1; i<argc; ++i) {
        if (strncmp(argv[i], "-x", 2)
             && strncmp(argv[i], "-d", 2)) {
            strcat(FullCmd, argv[i]);
            strcat(FullCmd, " ");
        }
    }
    if (createdfile) {
        strcat(FullCmd, "-z ");
        strcat(FullCmd, *SourceFileNamePtr);
    }
    if (!MightAskBeforeExecuting) {
        strcat(FullCmd, " -d ");
        /* The special hack for -d is HORRIBLE, but xterm screws up with the -d option in the middle of the command line! */
    }
    DefinitelyNotTty = 0;
    SetUpEnvironment();
    if (DoDebug) fprintf(stderr, "Executing %s\n", FullCmd);
    fflush(stdout); fflush(stderr);
    exit(system(FullCmd));
}

/* Only one or the other set up builtins gets used,
  depending on whether or not we're in the middle of 
          a multipart/alternative body part */
struct MailcapEntry BuiltIns[] = {
    {"text/*", CATTEMPLATE, NULL, 0, 1, 0},
    {NULL, NULL, NULL, 0, 0, 0}};

struct MailcapEntry BuiltInsAlternative[] = {
    {"text/plain", CATTEMPLATE, NULL, 0, 1, 0},
    {NULL, NULL, NULL, 0, 0, 0}};

ProcessMailcapFiles(SquirrelFile) 
char *SquirrelFile;
{
    char *s, *pathcopy = NULL;
#ifdef __MSDOS__
    char *path = STDPATH;
#else
#ifdef AMIGA
    char *path = STDPATH;
#else
    char *path = getenv("MAILCAPS");
    if (!path) {
        int uid = getuid();
        struct passwd *p;
        p = getpwuid(uid);
        if (p) path = malloc(5+strlen(p->pw_dir) + sizeof(STDPATH));
        if (!p || !path) ExitWithError(nomem);
        strcpy(path, p->pw_dir);
        strcat(path, STDPATH);
        pathcopy = path;
    } else 
#endif
#endif
    {
        pathcopy = malloc(1+strlen(path));
        if (!pathcopy) ExitWithError(nomem);
        strcpy(pathcopy, path);
        path = pathcopy;
    }
    while(path) {
        s = index(path, PATH_SEPARATOR);
        if (s) *s++ = 0;
        if (!ProcessMailcapFile(path, SquirrelFile)) return(0);
        path = s;
    }
    if (pathcopy) free(pathcopy);
    return(-1);
}

TryBuiltIns(SquirrelFile) 
char *SquirrelFile;
{
    int i;
    /* Last resort -- for sites that didn't bother putting a "text" line in their mailcap files... */
    if (DoDebug) fprintf(stderr, "Looking for '%s' in built-in content-type handling settings.\n", ContentType);
    for (i=0; BuiltIns[i].contenttype; ++i) {
        if (!TryMailcapEntry(SquirrelFile ? BuiltInsAlternative[i] : BuiltIns[i], SquirrelFile))    return(0);
    }
    return(-1);
}

ProcessMailcapFile(file, SquirrelFile)
char *file, *SquirrelFile;
{
    struct MailcapEntry mc;
    FILE *fp = fopen(file, "r");

    if (DoDebug) fprintf(stderr, "Looking for '%s' in mailcap file '%s'.\n", ContentType, file);
    while (fp && !feof(fp)) {
	if (GetMailcapEntry(fp, &mc)) {
            if (!TryMailcapEntry(mc, SquirrelFile)) return(0);
	}
    }
    if (fp) fclose(fp);
    return(-1);
}

char *ShortCommand(progname)
char *progname;
{
    char *s;
    static char FullProgName[500];
    while (*progname && (*progname == '(' || isspace((unsigned char) *progname))) ++progname;
    strcpy(FullProgName, progname);
    s = index(FullProgName, ' ');
    if (s) *s = 0;
    s = rindex(FullProgName, '/');
    if (s) {
	return(s+1);
    } else {
	return(FullProgName);
    }
}

TryMailcapEntry(mc, SquirrelFile)
struct MailcapEntry mc;
char *SquirrelFile;
{
    StripTrailingSpace(mc.contenttype);
    if (DoDebug) fprintf(stderr, "Trying mailcap entry for '%s'.\n", mc.contenttype);
    if (CtypeMatch(ContentType, mc.contenttype) && PassesTest(&mc)) {
        if (SquirrelFile) {
            return(SaveSquirrelFile(SquirrelFile));
        } else {
            char TmpFileName[TMPFILE_NAME_SIZE];
            MkTmpFileName(TmpFileName);
            return(ExecuteMailcapEntry(mc, TmpFileName, ContentType, 1));
        }
    }
    if (mc.needtofree) {
        free(mc.contenttype);
        free(mc.command);
    }
    return(-1);
}

SaveSquirrelFile(SquirrelFile)
char *SquirrelFile;
{
    int j;
    FILE *outfp;
    outfp = fopen(SquirrelFile, "w");
    if (!outfp) {
        fprintf(stderr, "Cannot open %s to squirrel away a portion of a multipart/alternative\n", SquirrelFile);
        return(-1);
    }
    fprintf(outfp, "Content-type: %s", ContentType);
    for (j=0; j<CParamsUsed; ++j) {
        fprintf(outfp, " ; ");
        fprintf(outfp, CParams[j]);
        fprintf(outfp, " = ");
        fprintf(outfp, CParamValues[j]);
    }
    fprintf(outfp, "\n\n"); 
    TranslateInputToOutput(InputFP, outfp, EncodingCode);
    if (fclose(outfp)) {
        ExitWithError("fclose failed");
    }
    return(0);
}

ExecuteMailcapEntry(mc, TmpFileName, ThisContentType, NeedToWriteTmpFile)
char *TmpFileName, *ThisContentType;
struct MailcapEntry mc;
int NeedToWriteTmpFile;
{
    int resultcode=0, DidExecute, UsedTmpFileName;
    char *s, *cmd;

    cmd = malloc(CMDSIZE);
    if (!cmd) ExitWithError(nomem);
    BuildCommand(cmd, mc.command, TmpFileName, &UsedTmpFileName);
    if (DoDebug) fprintf(stderr, "Match!  Built command %s.\n", cmd);
    if (mc.copiousoutput && MaybePageOutput) {
        strcat(cmd, " | ");
        s = getenv("METAMAIL_PAGER");
        if (s && strncmp(s, "metamail", 8)) {
            /* If METAMAIL_PAGER is set to "metamail" we override it */
            strcat(cmd, s);
#ifndef AMIGA
            if (!strncmp(s, "less", 4)) {
                fprintf(stderr, "Warning:  'less' behaves badly for some mail types, notably richtext.\n");
            }
#endif
        } else {
            strcat(cmd, "more");
        }
    }
    if (!DefinitelyNotTty) {
        SaveTtyState();
    }
    if (!NeedToAskBeforeExecuting(ThisContentType)
         || OKToRun(ThisContentType, cmd)) {
        char *FullCmd;
        int ReallyNotTty;
#ifndef NO_RLIMITS
        /* Limit size of core dumps */
        struct rlimit rlp;

        rlp.rlim_cur = 0;
        rlp.rlim_max = 0;
        setrlimit(RLIMIT_CORE, &rlp); 
#endif
        FullCmd = malloc(CMDSIZE);
        if (!FullCmd) ExitWithError(nomem);
        ReallyNotTty = DefinitelyNotTty;
        if (mc.needsterminal
             && DefinitelyNotTty) {
            int j;
            sprintf(cmd, " %s -P -b -c '%s", METAMAIL, ThisContentType);
            for (j=0; j<CParamsUsed; ++j) {
                strcat(cmd, " ; ");
                strcatquoting(cmd, CParams[j]);
                strcat(cmd, " = ");
                strcatquoting(cmd, CParamValues[j]);
            }
            strcat(cmd, "' ");
            strcat(cmd, TmpFileName);
            CreateNewWindowPrefix(FullCmd);
            strcat(FullCmd, cmd);
            DefinitelyNotTty = 0; /* For recursive call */
        } else {
            strcpy(FullCmd, cmd);
        }
        DidExecute = 0;
        if (UsedTmpFileName) {
            int isempty;
            if (NeedToWriteTmpFile) {
                isempty = WriteTmpFile(TmpFileName);
            } else isempty = 0;
            if (!isempty || strncmp(ThisContentType, "text", 4)) {
                if (DoInBackground && !mc.needsterminal) {
#ifdef AMIGA
                    char TmpCmd[80], TmpScriptName[40];
                    sprintf(TmpCmd, "execRmScript %s &",
                            MkRmScript(FullCmd, TmpFileName, TmpScriptName));
#else
                    char TmpCmd[CMDSIZE];
                    sprintf(TmpCmd, "(%s; rm %s) &", FullCmd, TmpFileName);
#endif
                    DefinitelyNotTty = 1; /* in background */
                    SetUpEnvironment();
                    resultcode = ExecuteCommand(TmpCmd, 1);
                    ++DidExecute;
                } else {
                    SetUpEnvironment();
                    resultcode = ExecuteCommand(FullCmd, 1);
                    unlink(TmpFileName);
                    ++DidExecute;
                }
            } else { /* empty text part, hack to not say "more" */
                unlink(TmpFileName);
            }
        } else {
            FILE *tmpfp;
            SetUpEnvironment();
            ExecuteCommand(FullCmd, 0);
            tmpfp = popen(FullCmd, "w");
            if (!NeedToWriteTmpFile) {
                FILE *tmpfp2;
                int c;

                tmpfp2 = fopen(TmpFileName, "r");
                if (!tmpfp2) ExitWithError("Cannot read tmp file");
                while ((c=getc(tmpfp2)) != EOF) {
                    putc(c, tmpfp);
                }
                fclose(tmpfp2);
            } else {
                TranslateInputToOutput(InputFP, tmpfp, EncodingCode);
            }
            resultcode = tmpfp ? pclose(tmpfp) : -1;
            ++DidExecute;
        }
        DefinitelyNotTty = ReallyNotTty;
        if (!DefinitelyNotTty && DidExecute) {
            RestoreTtyState();
            if (mc.copiousoutput && MaybePageOutput && BoundaryCt > 0) {
                char AnsBuf[100];
                printf("Press RETURN to go on.\n");
                gets(AnsBuf);
            }
        }
        if (!resultcode) {
            free(cmd);
            free(FullCmd);
            return(0);
        } else {
            fprintf(stderr, "Command failed: %s\n", FullCmd);
            ExitWithError(NULL);
        }
    } else {
        /* user does not want to execute command */
        if (!DefinitelyNotTty) {
            RestoreTtyState();
        }
        if (DoDebug) fprintf(stderr, "Not executing command.\n");
        ConsumeRestOfPart(NULL);
        free(cmd);
        return(0); /* Did as requested, after all */
    }
    if (!DefinitelyNotTty) {
        RestoreTtyState();
    }
    free(cmd);
    return(0);
}

PassesTest(mc)
struct MailcapEntry *mc;
{
    int result;
    char *cmd, TmpFileName[TMPFILE_NAME_SIZE];

    if (!mc->testcommand) return(1);
    MkTmpFileName(TmpFileName);
    cmd = malloc(CMDSIZE);
    if (!cmd) ExitWithError(nomem);
    BuildCommand(cmd, mc->testcommand, TmpFileName, NULL);
    result = system(cmd);
    free(cmd);
    return(!result);
}

char *
GetCommand(s, t)
char *s, **t;
{
    char *s2;
    int quoted = 0;
    s2 = malloc(strlen(s)*2); /* absolute max, if all % signs */
    if (!s2) ExitWithError(nomem);
    *t = s2;
    while (s && *s) {
	if (quoted) {
            if (*s == '%') *s2++ = '%'; /* Quote through next level, ugh! */

            *s2++ = *s++;
	    quoted = 0;
	} else {
	    if (*s == ';') {
                *s2 = 0;
		return(++s);
	    }
	    if (*s == '\\') {
		quoted = 1;
		++s;
	    } else {
		*s2++ = *s++;
	    }
	}
    }
    *s2 = 0;
    return(NULL);
}	

GetMailcapEntry(fp, mc)
FILE *fp;
struct MailcapEntry *mc;
{
    int rawentryalloc = 2000, len;
    char *rawentry, *s, *t, *LineBuf;


    LineBuf = malloc(LINE_BUF_SIZE);
    if (!LineBuf) ExitWithError(nomem);
    rawentry = malloc(1 + rawentryalloc);
    if (!rawentry) ExitWithError(nomem);
    *rawentry = 0;
    while (fgets(LineBuf, LINE_BUF_SIZE, fp)) {
	if (LineBuf[0] == '#') continue;
	len = strlen(LineBuf);
        if (LineBuf[len-1] == '\n') LineBuf[--len] = 0;
	if ((len + strlen(rawentry)) > rawentryalloc) {
	    rawentryalloc += 2000;
	    rawentry = realloc(rawentry, rawentryalloc+1);
	    if (!rawentry) ExitWithError(nomem);
	}
	if (LineBuf[len-1] == '\\') {
            LineBuf[len-1] = 0;
	    strcat(rawentry, LineBuf);
	} else {
	    strcat(rawentry, LineBuf);
	    break;
	}
    }
    free(LineBuf);
    for (s=rawentry; *s && isspace((unsigned char) *s); ++s) ;
    if (!*s) {
	/* totally blank entry -- quietly ignore */
	free(rawentry);
	return(0);
    }
    s = index(rawentry, ';');
    if (!s) {
	fprintf(stderr, "metamail: Ignoring invalid mailcap entry: %s\n", rawentry);
	free(rawentry);
	return(0);
    }
    *s++ = 0;
    mc->needsterminal = 0;
    mc->copiousoutput = 0;
    mc->needtofree = 1;
    mc->testcommand = NULL;
    mc->contenttype = malloc(1+strlen(rawentry));
    if (!mc->contenttype) ExitWithError(nomem);
    strcpy(mc->contenttype, rawentry);
    t = GetCommand(s, &mc->command);
    if (!t) {
        free(rawentry);
        return(1);
    }
    while (s && *s && isspace((unsigned char) *s)) ++s;
    s = t;
    while (s) {
	char *arg, *eq;

        t = GetCommand(s, &arg);
        if (t) *t++ = 0;
        eq = index(arg, '=');
        if (eq) *eq++ = 0;
        arg = Cleanse(arg);
	if (!strcmp(arg, "needsterminal")) {
	    mc->needsterminal = 1;
	} else if (!strcmp(arg, "copiousoutput")) {
	    mc->copiousoutput = 1;
        } else if (eq && !strcmp(arg, "test")) {
            mc->testcommand = eq;
	} else if (strcmp(arg, "notes")) { /* IGNORE notes field */
	    if (*arg && DoDebug) fprintf(stderr, "metamail: Ignoring mailcap flag: %s\n", arg);
	}
	s = t;
    }
    free(rawentry);
    return(1);
}

ExitWithError(txt)
char *txt;
{
    if (txt) fprintf(stderr, "metamail: %s\n", txt);
    exit(-1);
}

char *
FreshHeaderCopy(s)
char *s;
{
    char *t, *newcopy;
    int len;

    while (s && *s && isspace((unsigned char) *s) && *s != '\n') ++s;
    t = index(s, '\n');
    while (t && (*(t+1) == ' ' || *(t+1) == '\t')) {
        t = index(t+1, '\n');
    }
    len = t ? (t-s+1) : (strlen(s)+1);
    newcopy = malloc(len+1);
    if (!newcopy) ExitWithError(nomem);
    strncpy(newcopy, s, len);
    newcopy[len] = 0;
    return(newcopy);
}

Read822Prefix(PrintHeads, nestingdepth)
int PrintHeads, nestingdepth;
{
    int SawNewline = 1, bytes = 0, alloced = 1000, HasEncodedChars=0;
    int c, oldbytes;
    char *s, *t, *tmp;

    if (!PrintSomeHeaders) PrintHeads = 0;
    mailheaders = malloc(alloced+1);
    if (!mailheaders) ExitWithError(nomem);
    strcpy(mailheaders, "MM_HEADERS=\n");
    bytes = 12;
yankagain:
    t = mailheaders + bytes;
    oldbytes = bytes-1; /* a hack for YankMode */
    while ((c = getc(InputFP)) != EOF) {
        if (++bytes >= alloced) {
            alloced += 1000;
            mailheaders = realloc(mailheaders, alloced);
            if (!mailheaders) ExitWithError(nomem);
            t = mailheaders + bytes - 1;
        }
        if (c == '\n') {
            if (SawNewline) break;
            SawNewline = 1;
        } else SawNewline = 0;
        *t++ = c;
    }
    *t = 0;
    --bytes;
    if (c == EOF) {
        if (nestingdepth) {
            exit(0);
        } else {
            if (YankMode) {
                ExitWithError("Could not extract a MIME message from the body\n");
            } else {
                ExitWithError("Could not find end of mail headers");
            }
        }
    }
    for (s=mailheaders+oldbytes; *s; ++s) {
        if (*s == '\n' && (*(s+1) != ' ') && (*(s+1) != '\t')) {
            if (!ContentType && !lc2strncmp(s, "\ncontent-type:", 14)) {
                ContentType = FreshHeaderCopy(s+14);
                StripTrailingSpace(ContentType);
                ParseContentParameters(ContentType);
                if (PrintHeads) maybephead(s+1);
            } else if (!ContentEncoding && !lc2strncmp(s, "\ncontent-transfer-encoding:", 27)) {
                ContentEncoding = FreshHeaderCopy(s+27);
                if (PrintHeads) maybephead(s+1);
            } else if (!lc2strncmp(s, "\nsubject:", 9)) {
                if (PrintHeads) maybephead(s+1);
                MailSubject = FreshHeaderCopy(s+9);
            } else if (!lc2strncmp(s, "\nfrom:", 6)) {
                if (PrintHeads) maybephead(s+1);
                MailFrom = FreshHeaderCopy(s+6);
            } else if (!lc2strncmp(s, "\ncontent-description:", 4)) {
                if(PrintHeads) maybephead(s+1);
                MailSubject = FreshHeaderCopy(s+21);
            } else {
                /* Print any with encoded variables */
                char *dum = s;
                while (dum) {
                    dum = index(dum, '?');
                    if (dum && *++dum == '=') break;
                }
                if (dum) {
                    char *nl = s+1;
                    while (nl) {
                        nl = index(nl, '\n');
                        if (nl && !isspace((unsigned char) *++nl)) break;
                    }
                    if (nl && nl > dum) ++HasEncodedChars;
                }
                if (HasEncodedChars) {
                    phead(s+1);
                } else if (PrintHeads) {
                    maybephead(s+1);
                }
            }
        }
    }
    /* Ugly, but effective */
    if (YankMode && !ContentType) {
        goto yankagain;
    }
    if (PrintHeads) printf("\n");
    if (!ContentType) ContentType = "text/plain";
    for (tmp=ContentType; *tmp; ++tmp) {
        if (isupper((unsigned char) *tmp)) *tmp = tolower((unsigned char) *tmp);
    }
}

PrepareMessage() {
    int c;

    EncodingCode = ENCODING_NONE;
    if (ContentEncoding) {
        /* strip leading white space */
        while (*ContentEncoding && isspace((unsigned char) *ContentEncoding)) ++ContentEncoding;
        StripTrailingSpace(ContentEncoding);
        if (!lc2strcmp(ContentEncoding, "base64")) {
            EncodingCode = ENCODING_BASE64;
        } else if (!lc2strcmp(ContentEncoding, "quoted-printable")) {
            EncodingCode = ENCODING_QUOTEDPRINTABLE;
        } else if (!lc2strncmp (ContentEncoding, "x-uue", 5)) {
            fprintf (stderr, "WARNING:  Using nonstandard %s encoding, trying uuencode algorithm.\n", ContentEncoding);
 	    EncodingCode = ENCODING_UUENCODE;
        } else {
            if (lc2strcmp(ContentEncoding, "none")
                 && !lc2strcmp(ContentEncoding, "8bit")
                 && !lc2strcmp(ContentEncoding, "7bit")) {
                fprintf(stderr, "Ignoring unrecognized Content-Transfer-Encoding value: %s\n", ContentEncoding);
            }
        }
    }
    if (EatLeadingNewlines) {
        while ((c = getc(InputFP)) != EOF) {
            if (c != '\n') {
                ungetc(c, InputFP);
                break;
            }
        }
    }
    SetUpEnvironment();  
}

SetUpEnvironment() { 
    int i, j, environsize;
    char **newenviron, *mailervar, *summaryvar, *ctypevar, *s;
    static char ttyenv[15], debugenv[15], *noaskenv, pagerenv[15], *quietenv;

#ifndef __MSDOS__
#ifndef AMIGA
    /* Hack to make the code look similar for unix & dos */
#define putenv(var)        newenviron[i++] = var;
    for (environsize=0; environ[environsize]; ++environsize) {
	;
    }
    newenviron = (char **) malloc(sizeof(char *) * (16+environsize));
    if (!newenviron) ExitWithError(nomem);
#endif
#endif
    mailervar = malloc(13+strlen(MailerName));
    if (!mailervar) ExitWithError(nomem);
    sprintf(mailervar, "MM_MAILER=%s", MailerName);
    summaryvar = malloc(26 + strlen(MailFrom) + strlen(MailSubject));
    if (!summaryvar) ExitWithError(nomem);
    sprintf(summaryvar, "MM_SUMMARY=%s (from %s)", MailSubject, MailFrom);
    MailSummary = summaryvar+11;
    EliminateNastyChars(MailSummary);
    i = 0;
    if (ContentType) {
        int ctypelen = 22+strlen(ContentType);
        for (j=0; j<CParamsUsed; ++j) {
            ctypelen += 6 + strlen(CParams[j]) + strlen(CParamValues[j]);
        }
        ctypevar = malloc(ctypelen);
        if (!ctypevar) ExitWithError(nomem);
        for (s=ContentType; *s; ++s) {
            if (isupper((unsigned char) *s)) *s = tolower((unsigned char) *s);
        }
        while (isspace((unsigned char) *--s)) *s = 0;
        sprintf(ctypevar, "MM_CONTENTTYPE=%s", ContentType);
        for (j=0; j<CParamsUsed; ++j) {
            strcat(ctypevar, " ; ");
            strcat(ctypevar, CParams[j]);
            strcat(ctypevar, " = ");
            strcat(ctypevar, CParamValues[j]);
        }
        putenv(ctypevar);
    }
    putenv(mailheaders ? mailheaders : "MM_HEADERS=unknown");
    putenv(mailervar);
    putenv(summaryvar);
    sprintf(ttyenv, "MM_NOTTTY=%d", DefinitelyNotTty);
    putenv(ttyenv);
    sprintf(debugenv, "MM_DEBUG=%d", DoDebug);
    putenv(debugenv);
    s = getenv("MM_QUIET");
    if (!s) s = QuietDefault;
    quietenv = malloc(15 + strlen(s));
    if (!quietenv) ExitWithError(nomem);
    if (Quiet) {
        strcpy(quietenv, "MM_QUIET=1");
    } else {
        sprintf(quietenv, "MM_QUIET=%s", s);
    }
    putenv(quietenv);
    s = getenv("MM_NOASK");
    if (!s) s = NoAskDefault;
    noaskenv = malloc(15 + strlen(s));
    if (!noaskenv) ExitWithError(nomem);
    if (MightAskBeforeExecuting) {
        sprintf(noaskenv, "MM_NOASK=%s", s);
    } else {
        strcpy(noaskenv, "MM_NOASK=1");
    }
    putenv(noaskenv);
    sprintf(pagerenv, "MM_USEPAGER=%d", MaybePageOutput);
    putenv(pagerenv);
#ifndef __MSDOS__
#ifndef AMIGA
    for (j=0; j<environsize; ++j) {
        if (strncmp(environ[j], "MM_", 3) || !strncmp(environ[j], "MM_CHARSET", 10)) {
            putenv(environ[j]);
        }
    }
    newenviron[i] = NULL;
    environ = newenviron;
    if (DoDebug) {
        printf("Here is the environment:\n\n");
        system("printenv");
    }
#endif
#endif
}


#ifdef AMIGA
int
putenv(def)
char *def;
{
    char *cp;
    char nameBuf[100];
    FILE *envFile;

    if ((cp = index(def, '=')) == NULL || def == cp) {
        return(1);
    }

    strcpy(nameBuf, "ENV:");
    strncat(nameBuf, def, cp - def);
    nameBuf[(cp - def) + 4] = 0;
    cp++;               /* Now points to value part of environment string. */

    if ((envFile = fopen(nameBuf, "w")) == NULL) {
        return(2);
    }

    fputs(cp, envFile);
    fclose(envFile);

    return(0);
}
#endif


lc2strncmp(s1, s2, len)
char *s1, *s2;
int len;
{
    if (!s1 || !s2) return (-1);
    while (*s1 && *s2 && len > 0) {
	if (*s1 != *s2 && (tolower(*s1) != *s2)) return(-1);
	++s1; ++s2; --len;
    }
    if (len <= 0) return(0);
    return((*s1 == *s2) ? 0 : -1);
}

lc2strcmp(s1, s2)
char *s1, *s2;
{
    if (!s1 || !s2) return (-1);
    while (*s1 && *s2) {
	if (*s1 != *s2 && (tolower(*s1) != *s2)) return(-1);
	++s1; ++s2;
    }
    return((*s1 == *s2) ? 0 : -1);
}

OKToRun(ctype, progname)
char *ctype, *progname;
{
    char AnsBuf[100], *s;

    if (DoInBackground) return(1);
    while (1) {
        printf("\n");
	printf("This message contains '%s'-format data.\nDo you want to view it using the '%s' command (y/n) [y] ? ", ctype, ShortCommand(progname));
        s = gets(AnsBuf);
        if (!s) return(0); /* EOF */
	while (s && *s && isspace((unsigned char) *s)) ++s;
	if (*s == 'y' || *s == 'Y' || !*s || *s == '\n') return(1);
	if (*s == 'n' || *s == 'N' || *s == 'q' || *s || 'Q') {
	    return(0);
	}
	printf("Please answer yes or no.\n");
    }
}

EliminateNastyChars(s)
char *s;
{
    if (s) for( ; *s ;++s) {
        if (isalnum((unsigned char) *s)) continue;
	if (index(" ,.;:/?\\|[]{}()*&^%#@-_=+~<>\"", *s)) continue;
	if (*s == '\'' || *s == '`') {
	    *s = '"';
	} else {
	    *s = ' ';
	}
    }
}

StripTrailingSpace(s)
char *s;
{
    char *t = s+strlen(s) -1;
    while (isspace((unsigned char) *t)) *t-- = 0;
}

maybephead(hdr)
char *hdr;
{
    static char *KeyHeads=NULL;
    static char **KeyHeadList;
    char *s;
    int numkeys=0, len;

    if (!KeyHeads) {
        KeyHeads = getenv("KEYHEADS");
        if (KeyHeads) {
            for (s=KeyHeads;*s;++s) if (isupper((unsigned char) *s)) *s=tolower((unsigned char) *s);
        } else {
            char *khtmp; /* to avoid writing into a string constant later */
            khtmp = "to:cc:subject:from:content-description:date";
            KeyHeads = malloc(1+strlen(khtmp));
            if (!KeyHeads) ExitWithError(nomem);
            strcpy(KeyHeads, khtmp);
        }
        for (s=KeyHeads; *s; ++s) if (*s == ':') ++numkeys;
        numkeys += 2;
        KeyHeadList = (char **) malloc((numkeys) * sizeof(char *));
        if (!KeyHeadList) ExitWithError(nomem);
        numkeys = 0;
        KeyHeadList[0] = KeyHeads;
        for(s=KeyHeads; *s; ++s) {
            if (*s == ':') {
                *s = '\0';
                KeyHeadList[++numkeys] = s+1;
            }
        }
        KeyHeadList[++numkeys] = NULL;
    }
    s = index(hdr, ':');
    if (s) {
        len = s - hdr;
        for (numkeys=0; KeyHeadList[numkeys]; ++numkeys) {
            if (!strcmp(KeyHeadList[numkeys], "*")
                 || !lc2strncmp(hdr, KeyHeadList[numkeys], len)) {
                phead(hdr);
                break;
            }
        }
    }
}

/* This next routine prints out a mail header, and needs to deal with the new extended charset headers. */
phead(s)
char *s;
{
    char *t = s;

    while (1) {
	t = index(t, '\n');
	if (!t) break;
        if (!isspace((unsigned char) *(t+1))) {
            *t = 0;
	    break;
	} else ++t;
    }
    PrintHeader(s, 1);
    printf("\n");
    if (t) *t = '\n';
}

static char PrevCharset[100] = "us-ascii";

/* This is the part that actually handles the charset issues */
PrintHeader(s, ShowLeadingWhitespace)
char *s;
int ShowLeadingWhitespace;
{
    char *charset, *encoding, *txt, *txtend, TmpFile[TMPFILE_NAME_SIZE];
    int ecode = ENCODING_NONE, CorrectedCharset = 0;
    FILE *fp;

    while (*s && (*s != '=')) {
        if (isspace((unsigned char) *s)) {
            if (ShowLeadingWhitespace) {
                putchar(' ');
            }
        } else {
            if (!ShowLeadingWhitespace) {
                /* Not another encoded word, not leading any more */
                ShowLeadingWhitespace = 1;
                putchar(' ');
            }
            putchar(*s);
            if (!CorrectedCharset) {
                CorrectedCharset = 1;
                strcpy(PrevCharset, "us-ascii");
            }
        }
        ++s;
    }
    if (!*s) return;
    if (*(s+1) != '?') {
        putchar('=');
        PrintHeader(++s, 1);
        return;
    }
    charset = s+2;
    encoding = index(charset, '?');
    if (!encoding) {
        putchar('=');
        PrintHeader(++s,1);
        return;
    }
    txt = index(encoding+1, '?');
    if (!txt) {
        putchar('=');
        PrintHeader(++s, 1);
        return;
    }
    txtend = txt;
    do {
        txtend = index(txtend+1, '?');
    } while(txtend && (*(txtend+1) != '='));
    if (!txtend) {
        putchar('=');
        PrintHeader(++s, 1);
    }
    /* Proper parse! Ready to dissect... */
    *encoding = 0;
    *txt = 0;
    *txtend = 0;
    if ((*(encoding+1) == 'q') || (*(encoding+1) == 'Q')) {
        ecode = ENCODING_QUOTEDPRINTABLE;
    } else if ((*(encoding+1) == 'b') || (*(encoding+1) == 'B')) {
        ecode = ENCODING_BASE64;
    } else {
        fprintf(stderr, "Bad encoding value in non-ASCII header string: %s\n", encoding+1);
    }
    if (lc2strcmp(charset, PrevCharset)) {
        char *s2, *charsetinuse;

        strcpy(PrevCharset, charset);
        for (s2=PrevCharset; *s2; ++s2) {
            if (isupper((unsigned char) *s2)) *s2 = tolower((unsigned char) *s2);
        }
        charsetinuse = getenv("MM_CHARSET");
        if (!charsetinuse || lc2strcmp(charsetinuse, PrevCharset)) {
            printf("[** %s charset **] ", charset);
        }
    }
    if (ecode == ENCODING_NONE) {
        printf(txt+1);
    } else {
        /* What follows is REALLY bogus, but all my encoding stuff is pipe-oriented right now... */
        MkTmpFileName(TmpFile);
        fp = fopen(TmpFile, "w");
        if (!fp) {
            fprintf(stderr, "Could not open temporary file\n");
        } else {
            char *t;
            for (t=txt+1; *t; ++t) {
                if (*t == '_') {
                    putc(' ', fp);
                } else if (*t == '\n') {
                    putc(' ', fp);
                } else {
                    putc(*t, fp);
                }
            }
            fclose(fp);
            fp = fopen(TmpFile, "r");
            if (!fp) {
                fprintf(stderr, "Could not open temporary file\n");
            } else {
                TranslateInputToOutput(fp, stdout, ecode);
                fclose(fp);
            }
            unlink(TmpFile);
        }
    }
    *encoding = '?';
    *txt = '?';
    *txtend = '?';
    PrintHeader(txtend + 2, 0);
}

BuildCommand(Buf, controlstring, TmpFileName, UsedTmpFileName)
char *Buf, *controlstring, *TmpFileName;
int *UsedTmpFileName;
{
    char *from, *to, *s, *p, *tmp;
    int prefixed = 0;

    if (UsedTmpFileName) *UsedTmpFileName = 0;
    for (from=controlstring, to=Buf; *from; ++from) {
        if (prefixed) {
            prefixed = 0;
            switch(*from) {
                case '%':
                    *to++ = '%';
                    break;
                case 's':
                    if (TmpFileName) {
                        strcpy(to, TmpFileName);
                        to += strlen(TmpFileName);
                        if (UsedTmpFileName) ++(*UsedTmpFileName);
                    }
                    break;
                case '{':
                    s = index(from, '}');
                    if (!s) {
                        fprintf(stderr, "Ignoring ill-formed parameter reference in mailcap file: %s\n", from);
                        break;
                    }
                    ++from;
                    *s = 0;
                    /* put in lower case */
                    for (tmp=from; *tmp; ++tmp) {
                        if (isupper((unsigned char) *tmp)) *tmp = tolower((unsigned char) *tmp);
                    }
                    p = FindParam(from);
                    if (!p) p = "\"\"";
                    strcpy(to, p);
                    to += strlen(p);
                    *s = '}'; /* restore */
                    from = s;
                    break;
                case 't':
                    /* type/subtype */
                    strcpy(to, ContentType);
                    to += strlen(ContentType);
                    break;
                default:
                    fprintf(stderr, "Ignoring unrecognized format code in mailcap file: %%<%c\n", *from);
                    break;
            }
        } else if (*from == '%') {
            prefixed = 1;
        } else {
            *to++ = *from;
        }
    }
    *to = 0;
}

WriteTmpFile(fname)
char *fname;
{
    FILE *fpout;
    int retval = 0;

    fpout = fopen(fname, "w");
    if (!fpout) ExitWithError("Can't create temporary file");
    TranslateInputToOutput(InputFP, fpout, EncodingCode);
    if (ftell(fpout) == 0) retval = 1;
    if (fclose(fpout)) ExitWithError("Can't write temporary file");
    return(retval);
}

TranslateInputToOutput(InputFP, OutputFP, Ecode)
FILE *InputFP, *OutputFP;
int Ecode;
{
    int InMultipart = BoundaryCt > 0 ? 1 : 0;

    switch(Ecode) {
        case ENCODING_BASE64:
            from64(InputFP, OutputFP, InMultipart ? Boundaries : NULL, &BoundaryCt);
            break;
        case ENCODING_QUOTEDPRINTABLE:
            fromqp(InputFP, OutputFP, InMultipart ? Boundaries : NULL, &BoundaryCt);
            break;
 	case ENCODING_UUENCODE:
 	    fromuue(InputFP, OutputFP, InMultipart ? Boundaries: NULL, &BoundaryCt);
 	    break;
        default:
            ConsumeRestOfPart(OutputFP);
    }
#ifndef __MSDOS__
#ifndef AMIGA
    if (UsingStandardInput && feof(InputFP) && !freopen("/dev/tty", "r", stdin)) {
        fprintf(stderr, "Warning: Cannot freopen /dev/tty to stdin");
    } else InputFP = stdin;
#endif
#endif
}

CreateNewWindowPrefix(Prefix)
char *Prefix;
{
    char *override = getenv("TERMINAL_CMD");
    if (override) {
        strcpy(Prefix, override);
#ifdef AMIGA
    } else {
        /* The window should *not* run in background. We are thus unable
         * to use NewWsh or NewCLI.
         */
        /* strcpy(Prefix, "newwsh CMD "); */
        Prefix[0] = 0;
#else
    } else if (getenv("DISPLAY")) {
        /* X11 */
        strcpy(Prefix, "xterm -title '");
        strcat(Prefix, MailSummary);
        strcat(Prefix, "' -e ");
    } else if (getenv("WINDOW_PARENT")) {
        /* SunView */
        strcpy(Prefix, "shelltool ");
    } else if (getenv("WMHOST")) {
        /* old Andrew WM */
        strcpy(Prefix, "h19 ");
    } else {
        /* last resort is to look for /dev/tty */
        if (!freopen("/dev/tty", "r", stdin)){
            ExitWithError("Don't know how to create a terminal window");
        }
        InputFP = stdin;
        fprintf(stderr, "Warning, reopened /dev/tty, could be strange.\n");
        Prefix[0] = 0;
#endif
    }
}

#ifndef __MSDOS__
#ifndef AMIGA
#ifdef SYSV
static struct termio MyTtyStateIn, MyTtyStateOut;
#else
static struct sgttyb MyTtyStateIn, MyTtyStateOut;
#endif
#endif
#endif

SaveTtyState() {
    /* Bogus -- would like a good portable way to reset the terminal state here */
#ifndef __MSDOS__
#ifndef AMIGA
#ifdef SYSV
    ioctl(fileno(stdin), TCGETA, &MyTtyStateIn);
    ioctl(fileno(stdout), TCGETA, &MyTtyStateOut);
#else
    gtty(fileno(stdin), &MyTtyStateIn);
    gtty(fileno(stdout), &MyTtyStateOut);
#endif
#endif
#endif
}

RestoreTtyState() {
#ifndef __MSDOS__
#ifndef AMIGA
#ifdef SYSV
    ioctl(fileno(stdout), TCSETA, &MyTtyStateOut);
    ioctl(fileno(stdin), TCSETA, &MyTtyStateIn);
#else
    stty(fileno(stdout), &MyTtyStateOut);
    stty(fileno(stdin), &MyTtyStateIn);
#endif
#endif
#endif
}

NeedToAskBeforeExecuting(type)
char *type;
{
    struct NoAskItem *nai;
    if (!MightAskBeforeExecuting || DoInBackground) return(0);
    for (nai = FirstNoAskItem; nai; nai = nai->next) {
        if (CtypeMatch(type, nai->type)) return(0);
    }
    return(1);
}

NeedToBeQuiet(cmd)
char *cmd;
{
    struct NoAskItem *nai;
    for (nai = FirstQuietItem; nai; nai = nai->next) {
        if (!lc2strcmp(nai->type, cmd)) return(1);
    }
    return(0);
}

CtypeMatch(ctype, pat)
char *ctype, *pat;
{
    int len;
    char pat2[200];

    if (!lc2strcmp(ctype, pat)) {
        return(1); /* exact match, case-insensitive */
    }
    if (index(pat, '/') == NULL) {
        /* implicit wildcard */
        strcpy(pat2, pat);
        strcat(pat2, "/*");
        pat = pat2;
    }
    len = strlen(pat);
    if ((pat[--len] == '*')
         && (pat[--len] == '/')
         && (!lc2strncmp(ctype, pat, len))
         && ((ctype[len] == '/') || (ctype[len] == '\0'))){
        /* wildcard match */
        return(1);
    }
    return(0);
}

ExecuteCommand(cmd, really)
char *cmd;
int really;
{
    int code;
    if (!Quiet || DoDebug) {
        if (!NeedToBeQuiet(ShortCommand(cmd))) {
            printf("---Executing: %s\n", DoDebug ? cmd : ShortCommand(cmd));
        } else if (EatLeadingNewlines) {
            printf("\n");
        } 
        fflush(stdout);
    }
    if (really) {
        fflush(stdout); fflush(stderr);
        code = system(cmd);
        if (DoDebug) printf("Command exit status: %d\n", code);
        return(code);
    }
    return(0);
}

MkTmpFileName(name)
char *name;
{
#ifdef AMIGA
    strcpy(name, "T:mmXXXXXX");
    mktemp(name);
#else
#ifndef __MSDOS__
    static int ctr = 0;

    sprintf(name, "/tmp/mm.%d.%d.%d", getuid(), getpid(), ctr++);
#else
     strcpy(name, "TXXXXXX");
     if (!mktemp(name))
         name[0] = 0;
     else
         printf("temp name = \"%s\"\n", name);
#endif
#endif
}

#ifdef AMIGA
/* We need to execute a command and then remove a file "fileToRemove".
 * MkRmScript() creates a shell script that accomplishes this. The script
 * is written to a temporary file. The name of the script is returned.
 */
char *
MkRmScript(command, fileToRemove, nameBuf)
char *command;
char *fileToRemove;
char *nameBuf;
{
    FILE *script;

    MkTmpFileName(nameBuf);
    if ((script = fopen(nameBuf, "w")) == NULL) {
        fprintf(stderr, "Unable to open %s for writing\n", nameBuf);
        exit(1);
    }
    fprintf(script, ".BRA {\n.KET }\n%s\nDelete %s\n", command, fileToRemove);
    fclose(script);
    return(nameBuf);
}
#endif

ConsumeRestOfPart(outfp)
FILE *outfp;
{
    char *Buf;
    int c;

    if (BoundaryCt <= 0) {
        while ((c=getc(InputFP)) != EOF) {
            if (outfp) putc(c, outfp);
        }
        return;
    }
    Buf = malloc(LINE_BUF_SIZE);
    if (!Buf) ExitWithError(nomem);
    while (fgets(Buf, LINE_BUF_SIZE, InputFP)) {
        if ((BoundaryCt > 0)
             && (Buf[0] == '-')
             && (Buf[1] == '-')
             && PendingBoundary(Buf, Boundaries, &BoundaryCt)) {
            break;
        }
        if (outfp) fputs(Buf, outfp);
    }
    free(Buf);
}

char *paramend(s)
char *s;
{
    int inquotes=0;
    while (*s) {
        if (inquotes) {
            if (*s == '"') {
                inquotes = 0;
            } else if (*s == '\\') {
                ++s; /* skip a char */
            }
        } else if (*s == ';') {
            return(s);
        } else if (*s == '"') {
            inquotes = 1;
        }
        ++s;
    }
    return(NULL);
}        

ParseContentParameters(ct)
char *ct;
{
    char *s, *t, *eq;

    CParamsUsed = 0;
    s = index(ct, ';');
    if (!s) return;
    *s++ = 0;
    do {
        t = paramend(s);
        if (t) *t++ = 0;
        eq = index(s, '=');
        if (!eq) {
            fprintf(stderr, "Ignoring unparsable content-type parameter: '%s'\n", s);
            JunkParameter=Cleanse(s);
        } else {
            if (CParamsUsed >= CParamsAlloced) {
                CParamsAlloced += 10;
                if (CParams) {
                    CParams = (char **) realloc(CParams, (1+CParamsAlloced) * sizeof (char *));
                    CParamValues = (char **) realloc(CParamValues, (1+CParamsAlloced) * sizeof (char *));
                } else {
                    CParams = (char **) malloc((1+CParamsAlloced) * sizeof (char *));
                    CParamValues = (char **) malloc((1+CParamsAlloced) * sizeof (char *));
                }
                if (!CParams || !CParamValues) ExitWithError(nomem);
            }
            *eq++ = 0;
            s = Cleanse(s);
            CParams[CParamsUsed] = s;
            /* strip leading white space */
            while (*eq && isspace((unsigned char) *eq)) ++eq;
            /* strip trailing white space */
            StripTrailingSpace(eq);
            CParamValues[CParamsUsed++] = eq; 
            if (DoDebug) printf("NEW PARAMETER: %s VALUE: %s\n", s, eq);
        }
        s = t;
    } while (t);
}

char *FindParam(s)
char *s;
{
    int i;
    for (i=0; i<CParamsUsed; ++i) {
        if (!strcmp(s, CParams[i])) {
            return(CParamValues[i]);
        }
    }
    return(NULL);
}

#ifdef __MSDOS__
system2(s)
char *s;
{
    printf("system2: \"%s\"\n", s);
    return(0);
}
#endif

strcatquoting(s1, s2)
char *s1;
char *s2;
{
    strcat(s1, s2);
#ifdef NOTDEF
    while (*s1) ++s1;
    while (*s2) {
        if (*s2 == '\"' || *s2 == '\\') *s1++ = '\\';
        *s1++ = *s2++;
    }
    *s1 = '\0';
#endif
}
