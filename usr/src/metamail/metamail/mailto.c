/*
Copyright (c) 1992 Bell Communications Research, Inc. (Bellcore)

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

/* 
Program: mailto
Author: Nathaniel S. Borenstein

   This is a program that allows users to send MIME-format mail with much 
   the same user interface as the Berkeley mail program.  

   However, it should be stressed that this program includes
   ABSOLUTELY NO CODE taken from Berkeley mail -- it is a 
   "clean" reimplementation.

   The user interface is directly modelled on Berkeley mail,
   and the desiderata for its design are taken from the man
   page for that program.

   For information on using this program, consult the man page.

STILL NEED TO DO/SUPPORT:

-- Replace the main fgets call with something that allows in-line escape codes to do the equivalent of the tilde escapes.  Handle margins, centering, excerpts, EightBitMode, and RightToLeft mode properly.
-- Replace EditString with something that does the right thing in-line.
*/

#include <stdio.h>
#include <ctype.h>
#include <config.h>
#include <pwd.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#ifdef AMIGA
#define Prototype   extern
  
#include <getfiles.h>
#include <time.h>
#include <lib_protos.h>
#else
extern char *getenv();
#endif

#ifdef SYSV
/* Different people say different things about whether unistd.h lives in sys/ */
/* #include <sys/unistd.h> */
#include <unistd.h>
#endif

extern char *malloc(), *realloc(), *index();
struct mailpart *CreateNewPart();

/* The main data structure for the multiple parts of the mail */

struct mailpart {
    int istext;
    int isrich;
    char *content_type;
    int encoding_type_needed;
    char *filename;
    struct MailcapEntry *mc;
    struct mailpart *next, *prev;
};

/* Some globals */
struct mailpart *FirstPart = NULL;
static char *Subject = NULL, *ToList = NULL, *CCList= NULL, *InReplyTo = NULL;
FILE *fpout = NULL;
static char *CharacterSet = NULL;
static int PartEndsWithNewline=1;
static int SplitSize=DEFAULT_SPLIT_SIZE;
#define MINCHUNKSIZE 20000 /* Better be enough to hold the headers, or we die! */

#define CMDSIZE 1200 /* Maximum size of command to execute */
#define MAX_LINELENGTH  2000

#ifdef AMIGA
char userName[50];
char myAddress[100];
char deadLetter[60];
char mailRC[60];
char signature[60];
char mimeSignature[60];

#define TMPFILE_NAME_SIZE     50
#define FILE_NAME_SIZE       200

#define DEFAULT_EDITOR      "ed"

#define SYSTEM(command)     systemWithStdin(command)
#else
char *deadLetter = "dead.letter";
char *signature = ".signature";
char *mimeSignature = ".SIGNATURE";
#define TMPFILE_NAME_SIZE   1000
#define FILE_NAME_SIZE      1000

#define DEFAULT_EDITOR      "vi"

#define SYSTEM(command)     system(command);
#endif

/* The following are the globals that can be set via .mailrc */
int  V_askcc=0, /* To ask about the Cc field */
     V_dot=0, /* To take a period as the end of input */
     V_ignore=0, /* To ignore interrupts */
     V_verbose=0, /* To run sendmail with -v */
     V_quiet=0, /* To suppress various informational output */
     V_keepblind=0; /* To receive a blind copy of outgoing mail. */

/* Codes for encoding_type_needed */
#define ENC_NONE 0
#define ENC_QP 1 /* quoted-printable */
#define ENC_B64 2 /* base64 */

/* Codes for JustificationState */
#define JUST_LEFT 0
#define JUST_RIGHT 1
#define JUST_CENTER 2
int JustificationState = JUST_LEFT;


EightBitCharHelp() {
    if (!CharacterSet || !strcmp(CharacterSet, "us-ascii")) {
        printf("There are no extended characters available for your US-ASCII terminal.\n\n");
        printf("If you are actually using a terminal or terminal emulator with a richer\ncharacter set, you must use the '-a' option or the 'MM_CHARSET' environment\nvariable to inform this program of that fact.\n");
    } else {
        static char *kbd1[] =
          {
            "1234567890-=",
            "qwertyuiop[]",
            "asdfghjkl;'`",
            "zxcvbnm,./\\",
            NULL
        };
        static char *kbd2[] = /* MUST have same # of entries as kbd1 */
          {
            "!@#$%^&*()_+",
            "QWERTYUIOP{}",
            "ASDFGHJKL:\"~",
            "ZXCVBNM<>?|",
            NULL
        };
        char *s;
        int i, ct;

        printf("Here is the keyboard map for the character set %s\n.If your terminal does not really use this character set, this may look strange.\n\n", CharacterSet);
        for (i=0; kbd1[i]; ++i) {
            for (ct=0, s=kbd1[i]; *s; ++s) {
                printf("%c%c ", *s, (*s) + 128);
                ct+=3;
            }
            while (ct++<40) printf(" ");
            for (s=kbd2[i]; *s; ++s) {
                printf("%c%c ", *s, (*s) + 128);
            }
            printf("\n");
        }
    }
}            

char *
tmpname() {
    static int ctr = 0;
    char *s = malloc(30);
    if (!s) nomemabort();
#ifdef AMIGA
    strcpy(s, "T:mmXXXXXX");
    mktemp(s);
#else
    sprintf(s, "/tmp/mt.%d.%d.%d", getuid(), getpid(), ctr++);
#endif
    return(s);
}

TildeHelp() {
    char *pager = getenv("PAGER");
    char TmpName[100], CmdBuf[150];
    FILE *fp;

    strcpy(TmpName, tmpname());
    fp = fopen(TmpName, "w");
    if (!fp) fp = stdout;
    fprintf(fp, "The following tilde escapes are BSD-mail-compatible:\n");
    fprintf(fp, "~? Show help on tilde escapes\n");
    fprintf(fp, "~| RESERVED FOR BSD MAIL COMPATIBILITY\n");
    fprintf(fp, "~f RESERVED FOR BSD MAIL COMPATIBILITY\n");
    fprintf(fp, "~m RESERVED FOR BSD MAIL COMPATIBILITY\n");
    fprintf(fp, "~! Shell escape\n");
    fprintf(fp, "~~ Enter text line starting with a tilde\n");
    fprintf(fp, "~. Send the mail and exit\n");
    fprintf(fp, "~c Add to Cc list\n");
    fprintf(fp, "~d Read from %s (or named file, ~d filename)\n", deadLetter);
    fprintf(fp, "~e Edit message being composed\n");
    fprintf(fp, "~h Edit the headers\n");
    fprintf(fp, "~p Print out the message so far\n");
    fprintf(fp, "~q Quit, copying to %s\n", deadLetter);
    fprintf(fp, "~r Read the named text file into the message\n");
    fprintf(fp, "~s Reset the subject\n");
    fprintf(fp, "~t Add to To list\n");
    fprintf(fp, "~v Edit using VISUAL editor\n");
    fprintf(fp, "~w Write message to named file\n");
    fprintf(fp, "\n");
    fprintf(fp, "The following tilde escapes are unique to this program:\n");
    fprintf(fp, "~/<number> Set maximum size before message is split into multiple parts\n");
    fprintf(fp, "~?+ Show help on extended (eight-bit) characters\n");
    fprintf(fp, "~> Indent Left Margin\n");
    fprintf(fp, "~< Unindent Left Margin\n");
    fprintf(fp, "~<R Indent Right Margin\n");
    fprintf(fp, "~>R Unindent Right Margin\n");
    fprintf(fp, "~+ Enter 8-bit mode for non-ASCII characters\n");
    fprintf(fp, "~- Leave 8-bit mode (return to ASCII)\n");
    fprintf(fp, "~^ Toggle \"Upside-down\" (right-to-left) mode.\n");
    fprintf(fp, "~* Add non-text data (pictures, sounds, etc.) as a new MIME part (try it!)\n");
    fprintf(fp, "~b Toggle bold mode\n");
    fprintf(fp, "~i Toggle italic mode\n");
    fprintf(fp, "~j Alter Justification (~jc = center, ~jl = flushleft, ~jr = flushright.)\n");
    fprintf(fp, "~k Toggles whether or not to keep a 'blind' copy of your mail\n");
    fprintf(fp, "~n Force newline (hard line break)\n");
    fprintf(fp, "~Q Toggle quotation (excerpt) mode\n");
    fprintf(fp, "~S Toggle Semitic text mode (combines 8-bit and left-to-right)\n");
    fprintf(fp, "~u Toggle underline mode\n");
    fprintf(fp, "~z Add the contents of %s as a TEXT signature.\n", signature);
    fprintf(fp, "~Z Add the contents of %s as a NON-TEXT (MIME-format) signature.\n", mimeSignature);
    fprintf(fp, "For further information, read the man page.\n");
    fprintf(fp, "\n");
    if (fp != stdout) {
        fclose(fp);
        sprintf(CmdBuf, "%s %s", pager ? pager : "more", TmpName);
        SYSTEM(CmdBuf);
        unlink(TmpName);
    }
}

#ifdef AMIGA
InitUserName()
{
    char *cp;

    if ((cp = GetUserName()) == NULL) {
        fprintf(stderr, "mailto: Unable to find UserName in environment or UUCP Config file\n");
        exit(10);
    }
    strcpy(userName, cp);
    if ((cp = FindConfig("NodeName")) == NULL) {
        fprintf(stderr, "mailto: Unable to find NodeName in UUCP Config file\n");
        exit(10);
    }
    strcpy(myAddress, cp);
    if ((cp = FindConfig("DomainName")) == NULL) {
        fprintf(stderr, "mailto: Unable to find DomainName in UUCP Config file\n");
        exit(10);
    }
    strcat(myAddress, cp);
    sprintf(deadLetter, "UUMAIL:%s.dead.letter", userName);
    sprintf(mailRC, "UULIB:%s.mailtorc", userName);
    sprintf(signature, "UULIB:%s.signature.rt", userName);
    sprintf(mimeSignature, "UULIB:%s.signature.MIME", userName);
}
#else 
char *gethome() {
    struct passwd *p = getpwuid(getuid());
    if (!p || !p->pw_dir) {
        perror("Cannot find your home directory, using /tmp");
        return("/tmp");
    }
    return (p->pw_dir);
}
#endif


static char standoutbuf[50], standendbuf[50], StartUnderline[50], StopUnderline[50], BoldOn[50], BoldOff[50], KS[50], KE[50];
static int termcolumns, termrows;

InitTerminal() {
#ifdef AMIGA
    strcpy(standoutbuf, "\x9b\x37m");   /* Enter standout (highlighted) mode */
    strcpy(standendbuf, "\x9b\x30m");   /* Exit standout mode */
    strcpy(BoldOn,      "\x9b\x31m");   /* Enter bold mode */
    strcpy(BoldOff,     "\x9b\x30m");   /* Exit bold mode */
    strcpy(StartUnderline,"\x9b\x34m"); /* Enter underline mode */
    strcpy(StopUnderline,"\x9b\x30m");  /* Exit underline mode */
#else
    char tbuf[1024], *term, *dum;

    term = getenv("TERM");
    if (term && tgetent(tbuf, term) != 1) {
        term = NULL;
    }
    if (term) {
        dum = KS;
        if (tgetstr("ks", &dum)) *dum = NULL; else KS[0] = '\0';
        dum = KE;
        if (tgetstr("ke", &dum)) *dum = NULL; else KE[0] = '\0';
        dum = standoutbuf;
        if (tgetstr("so", &dum)) *dum = NULL; else standoutbuf[0] = '\0';
        dum = standendbuf;
        if (tgetstr("se", &dum)) *dum = NULL; else standendbuf[0] = '\0';
        dum = BoldOn;
        if (tgetstr("md", &dum)) *dum = NULL; else strcpy(BoldOn, standoutbuf);
        dum = BoldOff;
        if (tgetstr("me", &dum)) *dum = NULL; else strcpy(BoldOff, standendbuf);
        dum = StartUnderline;
        if (tgetstr("us", &dum)) *dum = NULL; else StartUnderline[0] = '\0';
        dum = StopUnderline;
        if (tgetstr("ue", &dum)) *dum = NULL; else StopUnderline[0] = '\0';
        termcolumns = tgetnum("co");
        if (termcolumns <= 0) termcolumns = 80;
        termrows = tgetnum("li");
        if (termrows <= 0) termrows = 23;
    } else {
        KS[0] = NULL;
        KE[0] = NULL;
        standoutbuf[0] = NULL;
        standendbuf[0] = NULL;
        BoldOn[0] = NULL;
        BoldOff[0] = NULL;
        StartUnderline[0] = NULL;
        StopUnderline[0] = NULL;
        termcolumns = 80;
        termrows = 23;
    }
    fputs(KS, stdout);
#endif
}

FinalizeTerminal() {
    tfputs(standendbuf);
    tfputs(BoldOff);
    tfputs(StopUnderline);
#ifndef AMIGA
    fputs(KE, stdout);
#endif
}

struct mailpart *
NewPart() {
    struct mailpart *p;
    p = (struct mailpart *) malloc(sizeof(struct mailpart));
    if (!p) nomemabort();
    p->istext = 1;
    p->isrich = 0;
    p->content_type = "text/plain";
    p->encoding_type_needed = ENC_NONE;
    p->filename = tmpname();
    p->mc = NULL;
    p->next = NULL;
    p->prev = NULL;
    return(p);
}

nomemabort() {
    fprintf(stderr, "mailto: Out of memory\n");
    cleanexit(-1);
}

char *
freshcopy(s)
char *s;
{
    char *t = malloc(1+strlen(s));
    if (!t) nomemabort();
    strcpy(t, s);
    return(t);
}

char *
GetLineMalloced(prompt, def)
char *prompt, *def;
{
    char Sbuf[1000], *ans;
    printf("%s: ", prompt);
    fflush(stdout);
    ans = fgets(Sbuf, sizeof(Sbuf), stdin);
    if (ans) {
        /* Need a fresh copy */
        ans = malloc(1+strlen(Sbuf));
        if (!ans) nomemabort();
        strcpy(ans, Sbuf);
        ans[strlen(ans) - 1] = '\0'; /* trash newline */
    } else ans = def;
    return(ans);
}

char *
EditString(prompt, s)
char *prompt, *s;
{
    char *ans;
    char NewPrompt[500];
    if (!s) s = "";
    sprintf(NewPrompt, "%s [%s]", prompt, s);
    ans = GetLineMalloced(NewPrompt, s);
    while (*ans && isspace((unsigned char) *ans)) ++ans;
    if (!*ans) return(s);
    return(ans);
}

char *
AddCommasToAddressList(s)
char *s;
{
    int spaces = 0;
    char *t, *ans, *ansptr;

    for (t=s; *t; ++t) {
        if (*t == ' ') ++spaces;
    }
    /* That provides a max size for the mallocs */
    ans = malloc(1+strlen(s) +spaces);
    if (!ans) nomemabort();
    ansptr = ans;
    for (t=s; *t; ++t) {
        if (*t == ' ' && *(t-1) != ',') {
            *ansptr++ = ',';
        }
        *ansptr++ = *t;
    }
    *ansptr = '\0';
    while (--ansptr > ans && (isspace((unsigned char) *ansptr) || *ansptr == ',')) {
        *ansptr = '\0';
    }
    return(ans);
}

char *
AddToList(List, entry)
char *List, *entry;
{
    int len;
    char *end = entry+strlen(entry);

    while (--end >= entry && isspace((unsigned char) *end)) *end = '\0';
    if (! *entry) return(List);
    if (List && *List) {
        List = realloc(List, strlen(List) + strlen(entry) + 5);
        if (!List) nomemabort();
        strcat(List, ", ");
        strcat(List, entry);
    } else {
        List = malloc(strlen(entry) + 2);
        if (!List) nomemabort();
        strcpy(List, entry);
    }
    len = strlen(List);
    if (List[--len] == ',') List[len] = '\0';
    return(List);
}

main(argc, argv)
char **argv;
{
    char *sdum, *LineBuf, CmdBuf[100];
    int i, EightBitMode = 0, RightToLeftMode = 0, AllDone=0, EightBitSeen=0;
    FILE *fpin;
    struct mailpart *CurrentPart=NULL;

#ifdef AMIGA
    InitUserName();
#endif
    if ((LineBuf = malloc(MAX_LINELENGTH)) == NULL) {
        fprintf(stderr, "mailto: Unable to allocate memory\n");
        exit(10);
    }
    ProcessInitFiles();
    InitSignals();
    InitTerminal();
    sdum = getenv("SPLITSIZE");
    if (sdum) {
        i = atoi(sdum);
        if (i < MINCHUNKSIZE) {
            fprintf(stderr, "Ignoring SPLITSIZE environment variable of %d -- the minimum value is %d\n", i, MINCHUNKSIZE);
        } else {
            SplitSize = i;
        }
    }

    CharacterSet = getenv("MM_CHARSET");
    for (i=1; i<argc; ++i) {
        if (argv[i][0] == '-') {
            switch (argv[i][1]) {
                case 'a':
                    if (argv[i][2]) {
                        CharacterSet = &argv[i][2];
                    } else if (++i < argc) {
                        CharacterSet = argv[i];
                    } else {
                        fprintf(stderr, "mailto: -a requires a character set specification to follow\n");
                        cleanexit(-1);
                    }
                    break;
                case 'c':
                    if (argv[i][2]) {
                        CCList = &argv[i][2];
                    } else if (++i < argc) {
                        CCList = argv[i];
                    } else {
                        CCList = "";
                    }
                    break;
                case 'r':
                    if (argv[i][2]) {
                        InReplyTo = &argv[i][2];
                    } else if (++i < argc) {
                        InReplyTo = argv[i];
                    } else {
                        fprintf(stderr, "mailto: -r requires a Message-ID to follow\n");
                        cleanexit(-1);
                    }
                    break;
                case 's':
                    if (argv[i][2]) {
                        Subject = &argv[i][2];
                    } else if (++i < argc) {
                        Subject = argv[i];
                    } else {
                        fprintf(stderr, "mailto: -s requires a subject specification to follow\n");
                        cleanexit(-1);
                    }
                    break;
                default:
                    fprintf(stderr, "mailto:  Unrecognized option %s\n", argv[i]);
                    cleanexit(-1);
            }
        } else {
            /* It's an address, I guess */
            ToList = AddToList(ToList, argv[i]);
        }
    }
    if (!CharacterSet) CharacterSet = "us-ascii";
    for (sdum = CharacterSet; *sdum; ++sdum) {
        if (isupper(*sdum)) *sdum = tolower(*sdum);
    }
    if (strcmp(CharacterSet, "us-ascii")
         && strncmp(CharacterSet, "iso-8859-", 9)) {
        fprintf(stderr, "mailto:  Unsupported character set: %s\n", CharacterSet);
        exit(-1);
    }
    if (strcmp(CharacterSet, "us-ascii")) {
        printf("Composing mail in character set %s\n", CharacterSet);
    }
    ProcessMailcapFiles();
    if (!ToList) {
        sdum=GetLineMalloced("To", "");
        ToList=AddCommasToAddressList(sdum);
        free(sdum);
    }
    if (!Subject) Subject=GetLineMalloced("Subject", "<NO SUBJECT>");
    if (!CCList && V_askcc) {
        sdum = GetLineMalloced("Cc", "");
        CCList = AddCommasToAddressList(sdum);
        free(sdum);
    }
    FirstPart = NewPart();
    CurrentPart = FirstPart;
    fpout = fopen(CurrentPart->filename, "w");
    if (!fpout) {
        fprintf(stderr, "mailto:  Can't open temporary file %s\n", CurrentPart->filename);
        cleanexit(-1);
    }
    while (!AllDone && fgets(LineBuf, MAX_LINELENGTH, stdin) != NULL) {
        if (V_dot && LineBuf[0] == '.' && LineBuf[1] == '\n') {
            AllDone = 1;
        } else if (LineBuf[0] == '~') {
            /* Clean up argument */
            char *start = LineBuf+2;
            char *end = start + strlen(start);
            while (--end > start && isspace((unsigned char) *end)) *end = '\0';
            while (isspace((unsigned char) *start)) ++start;
            switch (LineBuf[1]) {
                case '.':
                    AllDone = 1;
                    break;
                case '?':
                    if (*start == '+') {
                        EightBitCharHelp();
                    } else {
                        TildeHelp();
                    }
                    break;
                case '~':
                    FputsQuotingLT(LineBuf+1, fpout, CurrentPart, EightBitMode, RightToLeftMode);
                    break;
                case '!':
                    if (!V_quiet) printf("Executing: %s\n", start);
                    SYSTEM(start);
                    break;
                case '>':
                    CurrentPart->isrich = 1;
                    fputs("<nl>", fpout);
                    if (*start=='r' || *start == 'R') {
                        TryClosingStyle("indentright", fpout, CurrentPart);
                    } else {
                        TryOpeningStyle("indent", fpout, CurrentPart, NULL);
                    }
                    break;
                case '<':
                    CurrentPart->isrich = 1;
                    fputs("<nl>", fpout);
                    if (*start == 'R' || *start == 'r') {
                        TryOpeningStyle("indentright", fpout, CurrentPart, NULL);
                    } else {
                        TryClosingStyle("indent", fpout, CurrentPart);
                    }
                    break;
                case '/':
                    i = atoi(start);
                    if (i < MINCHUNKSIZE) {
                        fprintf(stderr, "Ignoring splitsize setting of %d -- the minimum value is %d\n", i, MINCHUNKSIZE);
                    } else {
                        SplitSize = i;
                        if (!V_quiet) printf("Set splitsize to %d\n", i);
                    }
                    break;
                case '+':
                    if (!strcmp(CharacterSet, "us-ascii")) {
                        fprintf(stderr, "mailto:  No 8-bit characters allowed in ASCII mail\n");
                    } else {
                        EightBitMode = 1;
                        if (!V_quiet) printf("Entering text in eight-bit mode\n");
                    }
                    break;
                case '-':
                    EightBitMode = 0;
                    if (!V_quiet) printf("Entering text in seven-bit (normal) mode\n");
                    break;
                case '^':
                    RightToLeftMode = ! RightToLeftMode;
                    if (!V_quiet) printf("%s right-to-left mode\n", RightToLeftMode ? "Entering" : "Exiting");
                    break;
                case '*':
                    {
                    struct mailpart *p = CreateNewPart();
                    if (!p) break;
                    TempCloseStyles(fpout);
                    fclose(fpout);
                    CurrentPart->next = p;
                    CurrentPart->next->prev = CurrentPart;
                    CurrentPart = CurrentPart->next;
                    CurrentPart->next = NewPart();
                    CurrentPart->next->prev = CurrentPart;
                    CurrentPart = CurrentPart->next;
                    fpout = fopen(CurrentPart->filename, "w");
                    if (!fpout) {
                        fprintf(stderr, "mailto:  Can't open temporary file %s\n", CurrentPart->filename);
                        cleanexit(-1);
                    }
                    ReopenStyles(fpout, CurrentPart);
                    break;
                    }
                case 'b':
                    ToggleStyle("bold", fpout, CurrentPart, BoldOn, BoldOff);
                    break;
                case 'c':
                    CCList = AddToList(CCList, start);
                    if (!V_quiet) printf("Cc field is now: %s\n", CCList);
                    break;
                case 'd':
                    {
                    char fnam[FILE_NAME_SIZE];
                    if (! *start) {
#ifdef AMIGA
                        strcpy(fnam, deadLetter);
#else
                        sprintf(fnam, "%s/dead.letter", gethome());
#endif
                    } else {
                        strcpy(fnam, start);
                    }
                    fpin = fopen(fnam, "r");
                    if (!fpin) {
                        fprintf(stderr, "mailto: Cannot open file %s\n", fnam);
                        break;
                    }
                    TempCloseStyles(fpout);
                    fclose(fpout);
                    CurrentPart->next = NewPart();
                    CurrentPart->next->prev = CurrentPart;
                    CurrentPart = CurrentPart->next;
                    CurrentPart->istext = 0;
                    CurrentPart->content_type = "message/rfc822";
                    fpout = fopen(CurrentPart->filename, "w");
                    if (!fpout) {
                        fprintf(stderr, "mailto: Cannot open temporary file %s\n", CurrentPart->filename);
                        break;
                    }
                    TranslateInputToEncodedOutput(fpin, fpout, ENC_NONE);
                    fclose(fpin);
                    fclose(fpout);
                    printf("Included contents of %s\n", fnam);
                    CurrentPart->next = NewPart();
                    CurrentPart->next->prev = CurrentPart;
                    CurrentPart = CurrentPart->next;
                    fpout = fopen(CurrentPart->filename, "w");
                    if (!fpout) {
                        fprintf(stderr, "mailto:  Can't open temporary file %s\n", CurrentPart->filename);
                        cleanexit(-1);
                    }
                    ReopenStyles(fpout, CurrentPart);
                    break;
                    }
                case 'e':
                    EditCurrentMessage(0);
                    break;
                case 'h':
                    ToList = AddCommasToAddressList(EditString("To", ToList));
                    Subject = EditString("Subject", Subject);
                    CCList = AddCommasToAddressList(EditString("Cc", CCList));
                    break;
                case 'i':
                    ToggleStyle("italic", fpout, CurrentPart, standoutbuf, standendbuf);
                    break;
                case 'j':
                    {
                    int NewJustificationState;
                    switch (*start) {
                        case 'c':
                        case 'C':
                            NewJustificationState = JUST_CENTER;
                            break;
                        case 'r':
                        case 'R':
                            NewJustificationState = JUST_RIGHT;
                            break;
                        case 'l':
                        case 'L':
                            NewJustificationState = JUST_LEFT;
                            break;
                        default:
                            printf("mailto: Unrecognized justification state: %c\n", *start);
                            NewJustificationState = JustificationState;
                    }
                    if (JustificationState == NewJustificationState) {
                        if (!V_quiet) printf("mailto:  No change in justificiation\n");
                    } else {
                        if (CurrentPart->isrich) {
                            if (JustificationState == JUST_CENTER) {
                                EndStyle(fpout, "center");
                            } else if (JustificationState == JUST_RIGHT) {
                                EndStyle(fpout, "flushright");
                            } else {
                                EndStyle(fpout, "flushleft");
                            }
                        }
                        CurrentPart->isrich = 1;
                        fputs("<nl>", fpout);
                        JustificationState = NewJustificationState;
                        if (JustificationState == JUST_CENTER) {
                            StartStyle(fpout, "center", NULL);
                        } else if (JustificationState == JUST_RIGHT) {
                            StartStyle(fpout, "flushright", NULL);
                        } else {
                            StartStyle(fpout, "flushleft", NULL);
                        }
                    }
                    break;
                    }
                case 'k':
                    V_keepblind = ! V_keepblind;
                    if (!V_quiet) printf("%s keep a blind copy of this message\n", V_keepblind ? "Will" : "Won't");
                    break;
                case 'n':
                    CurrentPart->isrich = 1;
                    fputs("<nl>", fpout);
                    if (!V_quiet) printf("Inserted line break\n");
                    break;
                case 'p':
                    {
                    FILE *fptmp;
                    char Cmd[TMPFILE_NAME_SIZE + 15];
                    char *s=tmpname();
                    fclose(fpout);
                    fptmp = fopen(s, "w");
                    WriteOutMessage(fptmp, ToList, Subject, CCList, FirstPart);
                    TempCloseStyles(fptmp);
                    fclose(fptmp);
                    fpout = fopen(CurrentPart->filename, "a");
                    if (!fpout) {
                        fprintf(stderr, "mailto:  Can't open temporary file %s\n", CurrentPart->filename);
                        cleanexit(-1);
                    }
                    sprintf(Cmd, "metamail -z %s", s);
                    SYSTEM(Cmd);
                    free(s);
                    RestoreCurrentStyles();
                    break;
                    }
                case 'q':
                    TempCloseStyles(fpout);
                    fclose(fpout);
                    if (!WriteDeadLetter()) cleanexit(0);
                    break;
                case 'Q':
                    ToggleStyle("excerpt", fpout, CurrentPart, NULL, NULL);
                    break;
                case 'r':
                    if (! *start) {
                        printf("mailto: No file name given.\n");
                        break;
                    }
                    fpin = fopen(start, "r");
                    if (!fpin) {
                        fprintf(stderr, "mailto: Cannot open file %s\n", start);
                        break;
                    }
                    EightBitSeen = TranslateInputToEncodedOutput(fpin, fpout, ENC_NONE);
                    if (EightBitSeen 
                         && (CurrentPart->istext || CurrentPart->isrich)) {
                        if (CurrentPart->encoding_type_needed == ENC_NONE) {
                            CurrentPart->encoding_type_needed = ENC_QP;
                        }
                        if (!strcmp(CharacterSet, "us-ascii")) {
                            SwitchToEuropean();
                        }
                    }
                    fclose(fpin);
                    if (!V_quiet) printf("Included contents of %s\n", start);
                    break;
                case 'S':
                    /* Semitic language mode toggle */
                    if (EightBitMode) {
                        EightBitMode = 0;
                        RightToLeftMode = 0;
                    } else {
                        if (!strcmp(CharacterSet, "us-ascii")) {
                            fprintf(stderr, "mailto:  No 8-bit characters allowed in ASCII mail\n");
                            break;
                        }
                        EightBitMode = 1;
                        RightToLeftMode = 1;
                    }
                    if (!V_quiet) printf("%s Eight-bit and right-to-left modes\n", RightToLeftMode ? "Entering" : "Exiting");
                    break;
                case 's':
                    {
                    char *subj=start;
                    if (*subj) {
                        Subject = freshcopy(subj);
                    } else {
                        Subject=GetLineMalloced("Subject", "<NO SUBJECT>");
                    }
                    break;
                    }
                case 't':
                    ToList = AddToList(ToList, start);
                    if (!V_quiet) printf("To field is now: %s\n", ToList);
                    break;
                case 'u':
                    ToggleStyle("underline", fpout, CurrentPart, StartUnderline, StopUnderline);
                    break;
                case 'v':
                    EditCurrentMessage(1);
                    break;
                case 'w':
                    {
                    FILE *fptmp;
                    char *fname;
                    fclose(fpout);
                    fname = start;
                    fptmp = fopen(fname, "w");
                    WriteOutMessage(fptmp, ToList, Subject, CCList, FirstPart);
                    TempCloseStyles(fptmp);
                    if(fclose(fptmp)) {
                        fprintf(stderr, "Could not write file %s\n", fname);
                    } else {
                        printf("Wrote draft to %s\n", fname);
                    }
                    fpout = fopen(CurrentPart->filename, "a");
                    if (!fpout) {
                        fprintf(stderr, "mailto:  Can't open temporary file %s\n", CurrentPart->filename);
                        cleanexit(-1);
                    }
                    break;
                    }
                case 'z':
                    {
                    char FBuf[FILE_NAME_SIZE];
                    if (! *start) {
#ifdef AMIGA
                        strcpy(FBuf, signature);
#else
                        sprintf(FBuf, "%s/.signature", gethome());
#endif
                    } else {
                        strcpy(FBuf, start);
                    }
                    fpin = fopen(FBuf, "r");
                    if (!fpin) {
                        fprintf(stderr, "mailto: Cannot open file %s\n", FBuf);
                        break;
                    }
                    CurrentPart->isrich = 1;
                    StartStyle(fpout, "signature", NULL);
                    EightBitSeen = TranslateInputToEncodedOutput(fpin, fpout, ENC_NONE);
                    if (EightBitSeen) {
                        if (CurrentPart->encoding_type_needed == ENC_NONE) {
                            CurrentPart->encoding_type_needed = ENC_QP;
                        }
                        if ((CurrentPart->istext || CurrentPart->isrich) && !strcmp(CharacterSet, "us-ascii")) {
                            SwitchToEuropean();
                        }
                    }
                    fclose(fpin);
                    EndStyle(fpout, "signature");
                    if (!V_quiet) printf("Included contents of %s\n", FBuf);
                    break;
                    }
                case 'Z':
                    {
                    char fnam[FILE_NAME_SIZE];
                    if (! *start) {
#ifdef AMIGA
                        strcpy(fnam, mimeSignature);
#else
                        sprintf(fnam, "%s/.SIGNATURE", gethome());
#endif
                    } else {
                        strcpy(fnam, start);
                    }
                    fpin = fopen(fnam, "r");
                    if (!fpin) {
                        fprintf(stderr, "mailto: Cannot open file %s\n", fnam);
                        break;
                    }
                    TempCloseStyles(fpout);
                    fclose(fpout);
                    CurrentPart->next = NewPart();
                    CurrentPart->next->prev = CurrentPart;
                    CurrentPart = CurrentPart->next;
                    CurrentPart->istext = 0;
                    CurrentPart->content_type = "message/rfc822";
                    fpout = fopen(CurrentPart->filename, "w");
                    if (!fpout) {
                        fprintf(stderr, "mailto: Cannot open temporary file %s\n", CurrentPart->filename);
                        break;
                    }
                    EightBitSeen = TranslateInputToEncodedOutput(fpin, fpout, ENC_NONE);
                    if (EightBitSeen &&
                        (CurrentPart->istext || CurrentPart->isrich)) {
                        if (CurrentPart->encoding_type_needed == ENC_NONE) {
                            CurrentPart->encoding_type_needed = ENC_QP;
                        }
                        if (!strcmp(CharacterSet, "us-ascii")) {
                            SwitchToEuropean();
                        }
                    }
                    fclose(fpin);
                    fclose(fpout);
                    printf("Included contents of %s\n", fnam);
                    CurrentPart->next = NewPart();
                    CurrentPart->next->prev = CurrentPart;
                    CurrentPart = CurrentPart->next;
                    fpout = fopen(CurrentPart->filename, "w");
                    if (!fpout) {
                        fprintf(stderr, "mailto:  Can't open temporary file %s\n", CurrentPart->filename);
                        cleanexit(-1);
                    }
                    ReopenStyles(fpout, CurrentPart);
                    break;
                    }
                default:
                    printf("Unrecognized tilde escape: %c\n", LineBuf[1]);
                    break;
            }
        } else {
            FputsQuotingLT(LineBuf, fpout, CurrentPart, EightBitMode, RightToLeftMode);
        }
    }
    TempCloseStyles(fpout);
    fclose(fpout);
    printf("EOT\n");
    if (V_verbose) {
        sprintf(CmdBuf, "splitmail -d -v -s %d", SplitSize);
    } else {
        sprintf(CmdBuf, "splitmail -d -s %d", SplitSize);
    }
    fpout = popen(CmdBuf, "w");
    if (!fpout) {
        fprintf(stderr, "mailto: Can't write to splitmail\n");
        cleanexit(-1);
    }
    WriteOutMessage(fpout, ToList, Subject, CCList, FirstPart);
    if (pclose(fpout)) {
        perror("mailto: Error writing to splitmail");
        cleanexit(-1);
    }
    return(cleanexit(0));
}

WriteOutMessage(fp, ToList, Subject, CCList, FirstPart)
FILE *fp;
char *ToList, *Subject, *CCList;
struct mailpart *FirstPart;
{
    FILE *fpin;
    static int ctr = 0;
    fprintf(fp, "MIME-Version: 1.0\n");
    if (InReplyTo != NULL) {
        fprintf(fp, "In-Reply-To: %s\n", InReplyTo);
    }
    EmitHeaderWithAliases(fp, "To", ToList);

    fprintf(fp, "Subject: %s\n", Subject ? Subject : "");
    if (CCList && *CCList) EmitHeaderWithAliases(fp, "Cc", CCList);
    if (V_keepblind) {
#ifdef AMIGA
        fprintf(fp, "Bcc: %s@%s\n", userName, myAddress);
#else
        struct passwd *p = getpwuid(getuid());
        if (!p) {
            fprintf(stderr, "Can't find your user id to keep a blind copy\n");
        } else {
            fprintf(fp, "Bcc: %s\n", p->pw_name);
        }
#endif
    }
    if (!FirstPart) return; /* empty body */
    if (FirstPart->next) {
        char boundary[120], hostname[60];
#ifdef AMIGA
        sprintf(boundary, "PART.BOUNDARY.%d.%s@%s", time(0), SeqToName(GetSequence(4)),
                myAddress);
#else
#ifdef SYSV
	uname(hostname);
#else
	gethostname(hostname, sizeof(hostname));
#endif /* SYSV */
        sprintf(boundary, "PART.BOUNDARY.%d.%d.%s.%d.%d",
                 getuid(), getpid(), hostname, time(0), ++ctr);
#endif /* AMIGA */
        fprintf(fp, "Content-type: multipart/mixed;\n\tboundary=\"%s\"\n\n", boundary);
        fprintf(fp, "> THIS IS A MESSAGE IN 'MIME' FORMAT.  Your mail reader does not support MIME.\n> Some parts of this will be readable as plain text.\n> To see the rest, you will need to upgrade your mail reader.\n");
        while(FirstPart) {
            /* First check to see if it is empty & text, in which case skip it */
            if (FirstPart->istext) {
                struct stat stbuf;
                if (!stat(FirstPart->filename, &stbuf)) {
                    if (stbuf.st_size == 0) {
                        FirstPart = FirstPart->next;
                        continue;
                    }
                }
            }
            fprintf(fp, "\n--%s\n", boundary);
            WriteContentTypeAndEncoding(fp, FirstPart);
            fprintf(fp, "\n");
            fpin = fopen(FirstPart->filename, "r");
            if (!fpin) {
                fprintf(stderr, "Can't read temporary file %s\n", FirstPart->filename);
                cleanexit(-1);
            }
            TranslateInputToEncodedOutput(fpin, fp, FirstPart->encoding_type_needed);
            fclose(fpin);
            FirstPart = FirstPart->next;
        }
        fprintf(fp, "\n--%s--\n", boundary);
    } else {
        WriteContentTypeAndEncoding(fp, FirstPart);
        fprintf(fp, "\n");
        fpin = fopen(FirstPart->filename, "r");
        if (!fpin) {
            fprintf(stderr, "Can't read temporary file %s\n", FirstPart->filename);
            cleanexit(-1);
        }
        TranslateInputToEncodedOutput(fpin, fp, FirstPart->encoding_type_needed);
        fclose(fpin);
    }
}

WriteContentTypeAndEncoding(fp, part)
FILE *fp;
struct mailpart *part;
{
    if (part->istext) {
        if (part->isrich) {
            if (strcmp(CharacterSet, "us-ascii")
                 && (strncmp(CharacterSet, "iso-8859-", 9) 
                      || part->encoding_type_needed != ENC_NONE)) {
                fprintf(fp, "Content-type: text/richtext; charset=%s\n", CharacterSet);
            } else {
                fprintf(fp, "Content-type: text/richtext\n");
            }
        } else {
            if (strcmp(CharacterSet, "us-ascii")
                 && (strncmp(CharacterSet, "iso-8859-", 9) 
                      || part->encoding_type_needed != ENC_NONE)) {
                fprintf(fp, "Content-type: text/plain; charset=%s\n", CharacterSet);
            } else {
                fprintf(fp, "Content-type: text/plain\n");
            }
        }
    } else {
        fprintf(fp, "Content-type: ");
        WriteCtypeNicely(fp, part->content_type); /* fixes some syntactic junk */
    }
    if (part->istext && part->isrich && part->encoding_type_needed == ENC_NONE) {
        /* Don't you believe it... */
        part->encoding_type_needed = ENC_QP;
    }
    if (part->encoding_type_needed == ENC_B64) {
        fprintf(fp, "Content-Transfer-Encoding: base64\n");
    } else if (part->encoding_type_needed == ENC_QP) {
        fprintf(fp, "Content-Transfer-Encoding: quoted-printable\n");
    }
}

TranslateInputToEncodedOutput(InputFP, OutputFP, Ecode)
FILE *InputFP, *OutputFP;
int Ecode;
{
    int c, EightBitSeen = 0;

    switch(Ecode) {
        case ENC_B64:
            to64(InputFP, OutputFP);
            break;
        case ENC_QP:
            toqp(InputFP, OutputFP);
            break;
        default:
            while ((c = getc(InputFP)) != EOF){
                if (c > 127) EightBitSeen = 1;
                putc(c, OutputFP);
            }
    }
    return(EightBitSeen);
}

#define	MAX_STACK_SIZE	500
static int StackSize=0;
static char *Stack[MAX_STACK_SIZE];
static char *EnvStartStack[MAX_STACK_SIZE];

/* The following two routines are used when richtext styles need to be 
   kept open across an inserted object -- i.e. the richtext is split into 
   two parts of a multipart message */

TempCloseStyles(fp)
FILE *fp;
{
    int i = StackSize;
    while(--i>=0) {
        fprintf(fp, "</%s>", Stack[i]);
    }
    tfputs(StopUnderline);
    tfputs(standendbuf);
    tfputs(BoldOff);
    fflush(stdout);
}

RestoreCurrentStyles() {
    int i=0;
    while (i<StackSize) {
        if (EnvStartStack[i]) {
            tfputs(EnvStartStack[i]);
        }
        ++i;
    }
}

ReopenStyles(fp, part)
FILE *fp;
struct mailpart *part;
{
    int i=0;
    RestoreCurrentStyles();
    if (StackSize > 0) part->isrich = 1;
    while (i < StackSize) {
        fprintf(fp, "<%s>", Stack[i++]);
    }
}

richtextreset()
{
    StackSize = 0;
}

AlreadyInStyle(s)
char *s;
{
    int i;
    for (i=0; i<StackSize; ++i) {
        if (!strcmp(Stack[i], s)) return(1);
    }
    return(0);
}

StartStyle(fp, s, envstartstr)
FILE *fp;
char *s;
char *envstartstr;
{
    char *t;
    fprintf(fp, "<%s>", s);
    PartEndsWithNewline=0;
    t = malloc(1+strlen(s));
    if (!t) nomemabort();
    strcpy(t, s);
    EnvStartStack[StackSize] = envstartstr;
    Stack[StackSize++] = t;
    if (!V_quiet) printf("Beginning: %s\n", t);
}

EndStyle(fp, s)
FILE *fp;
char *s;
{
    int i = StackSize, j;
    while(--i>=0) {
        fprintf(fp, "</%s>", Stack[i]);
        if (!strcmp(s, Stack[i])) break;
    }
    if (i>=0) {
        free(Stack[i]);
        --StackSize;
        for (j=i; j<StackSize; ++j) {
            Stack[j] = Stack[j+1];
            EnvStartStack[j] = EnvStartStack[j+1];
        }
    } else i=0;
    while (i<StackSize) {
        fprintf(fp, "<%s>", Stack[i++]);
    }
    PartEndsWithNewline=0;
    if (!V_quiet) printf("Ending: %s\n", s);
}

ToggleStyle(name, fp, part, turnonstr, turnoffstr)
char *name;
FILE *fp;
struct mailpart *part;
char *turnonstr, *turnoffstr;
{
    part->isrich = 1;
    if (AlreadyInStyle(name)) {
        EndStyle(fp, name);
        if (turnoffstr) tfputs(turnoffstr);
        RestoreCurrentStyles(); /* because on some terminals,
                                  the same thing turns
                                  off underlining AND bold */
    } else {
        StartStyle(fp, name, turnonstr);
        if (turnonstr) tfputs(turnonstr);
    }
}

TryOpeningStyle(name, fp, part, envstartstr)
char *name;
FILE *fp;
struct mailpart *part;
char *envstartstr;
{
    if (AlreadyInStyle(name)) {
        printf("mailto: Already in %s style\n", name);
    } else {
        part->isrich = 1;
        StartStyle(fp, name, envstartstr);
    }
}

TryClosingStyle(name, fp, part)
char *name;
FILE *fp;
struct mailpart *part;
{
    if (AlreadyInStyle(name)) {
        part->isrich = 1;
        EndStyle(fp, name);
    } else {
        printf("mailto: You aren't in a %s style\n", name);
    }
}

FputsQuotingLT(s, fp, part, EightBitMode, RightToLeftMode)
char *s;
FILE *fp;
struct mailpart *part;
int EightBitMode, RightToLeftMode;
{
    static int InNewLineSequence=1;
    int c=0;
    char LBuf[1000], *sdum;
    if (*s == '\n') {
        if (InNewLineSequence) {
            fputs("<nl>\n", fp);
        } else {
            fputs("<nl><nl>\n\n", fp);
            InNewLineSequence = 1;
        }
        part->isrich = 1;
        PartEndsWithNewline=1;
        return;
    }
    InNewLineSequence = 0;
    if (RightToLeftMode) {
        sdum = s+strlen(s)-1;
        if (*sdum == '\n') --sdum;
        while (sdum >= s) {
            LBuf[c++] = *sdum--;
        }
        LBuf[c++] = '\n';
        LBuf[c] = '\0';
        s = LBuf;
    }
    if (isspace((unsigned char) *s)) {
        /* Lines that start with spaces should not be folded! */
        fputs("<nl>", fp);
        part->isrich = 1;
    }
    if (s) {
        if (EightBitMode) part->encoding_type_needed = ENC_QP;
        while (*s) {
            c = (unsigned char) *s;
            if (EightBitMode) {
                if (isprint(c) && ! isspace(c)) {
                    c = c+128;
                }
                putc(c, fp);
                putc(c, stdout);
            } else if (c == '<') {
                part->isrich = 1;
                fputs("<lt>", fp);
            } else {
                if (c > 127 || c < 0) {
                    part->encoding_type_needed = ENC_QP;
                    if (!strcmp(CharacterSet, "us-ascii")) {
                        SwitchToEuropean();
                    }
                }
                putc(c, fp);
                if (RightToLeftMode) putc(c, stdout);
            }
            ++s;
        }
        PartEndsWithNewline = (c == '\n') ? 1 : 0;
    }
}

cleanexit(code)
int code;
{
    finalize();
    exit(code);
}

finalize() {
    while (FirstPart) {
        unlink(FirstPart->filename);
        FirstPart = FirstPart->next;
    }
    FinalizeTerminal();
}

void
cleanup(signum)
int signum;
{
    if (fpout) {
        TempCloseStyles(fpout);
        fclose(fpout);
    }
    if (signum == SIGINT) {
        if (V_ignore) {
            printf("Interrupt ignored because 'ignore' is set.  Use ~q if you want to quit.\n");
            return;
        } else {
            WriteDeadLetter();
        }
    }
    finalize();
#ifdef AMIGA
    exit(signum);
#else
    signal(signum, SIG_DFL);
    kill(getpid(), signum);
#endif
}

InitSignals() {
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
#ifdef SIGXCPU
    signal(SIGXCPU, cleanup);
#endif
#endif
}

WriteDeadLetter()
{
    FILE *fp;
#ifdef AMIGA
    fp = fopen(deadLetter, "w");
    WriteOutMessage(fp, ToList, Subject, CCList, FirstPart);
    if(fclose(fp)) {
        fprintf(stderr, "Could not write %s\n", deadLetter);
        return(-1);
    } else {
        printf("Wrote draft to %s\n", deadLetter);
        return(0);
    }
#else
    char DeadFile[1000];

    sprintf(DeadFile, "%s/dead.letter", gethome());
    fp = fopen(DeadFile, "w");
    WriteOutMessage(fp, ToList, Subject, CCList, FirstPart);
    if(fclose(fp)) {
        perror("Could not write ~/dead.letter\n");
        return(-1);
    } else {
        printf("Wrote draft to ~/dead.letter\n");
        return(0);
    }
#endif
}

struct MailcapEntry {
    char *contenttype;
    char *command;
    char *testcommand;
    char *editcommand;
    char *composecommand;
    char *label;
    int needsterminal;
    int copiousoutput;
    struct MailcapEntry *next;
} *FirstMailcapEntry = NULL;

/* There are a fair number of core leaks in what follows.  That should matter little -- the mailcap files are only parsed once, and are usually pretty small anyway. */

char *
GetCommand(s, t)
char *s, **t;
{
    char *s2;
    int quoted = 0;
    s2 = malloc(strlen(s)*2); /* absolute max, if all % signs */
    if (!s2) nomemabort();
    *t = s2;
    while (s && *s) {
	if (quoted) {
            if (*s == '%') *s2++ = '%'; /* Quote through next level, ugh! */

            *s2++ = *s++;
	    quoted = 0;
	} else {
	    if (*s == ';') {
                *s2 = '\0';
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
    *s2 = '\0';
    return(NULL);
}	

char *Cleanse(s, dolc) /* no leading or trailing space, all lower case */
char *s;
int dolc;
{
    char *tmp, *news;
    
    /* strip leading white space */
    while (*s && isspace((unsigned char) *s)) ++s;
    news = s;
    /* put in lower case, find end */
    for (tmp=s; *tmp; ++tmp) {
        if (dolc && isupper((unsigned char) *tmp)) *tmp = tolower((unsigned char) *tmp);
    }
    /* strip trailing white space */
    while (--tmp && *tmp && isspace((unsigned char) *tmp)) *tmp = '\0';
    return(news);
}

char *DeQuote(s)
char *s;
{
    char *retval;
    s = Cleanse(s, 0);
    if (*s != '"') return(s);
    retval = ++s;
    while (s && *s) {
        s = index(s, '\"');
        if (!s) return(retval); /* but it's a bad parse */
        if (*(s-1) != '\\') {
            *s = '\0';
            return(retval);
        }
        ++s;
    }
    return(retval); /* also a bad parse */
}    

struct MailcapEntry *
GetMailcapEntry(fp)
FILE *fp;
{
    int rawentryalloc = MAX_LINELENGTH, len;
    char *rawentry, *s, *t, *LineBuf;
    struct MailcapEntry *mc;

    LineBuf = malloc(MAX_LINELENGTH);
    if (!LineBuf) nomemabort();
    rawentry = malloc(1 + rawentryalloc);
    mc = (struct MailcapEntry *) malloc(sizeof (struct MailcapEntry));
    if (!rawentry || !mc) nomemabort();
    *rawentry = '\0';
    while (fgets(LineBuf, MAX_LINELENGTH, fp)) {
	if (LineBuf[0] == '#') continue;
	len = strlen(LineBuf);
        if (LineBuf[len-1] == '\n') LineBuf[--len] = '\0';
	if ((len + strlen(rawentry)) > rawentryalloc) {
            rawentryalloc += MAX_LINELENGTH;
	    rawentry = realloc(rawentry, rawentryalloc+1);
	    if (!rawentry) nomemabort();
	}
	if (LineBuf[len-1] == '\\') {
            LineBuf[len-1] = '\0';
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
	return(NULL);
    }
    s = index(rawentry, ';');
    if (!s) {
	fprintf(stderr, "mailto: Ignoring invalid mailcap entry: %s\n", rawentry);
	free(rawentry);
	return(NULL);
    }
    *s++ = '\0';
    mc->needsterminal = 0;
    mc->copiousoutput = 0;
    mc->testcommand = NULL;
    mc->composecommand = NULL;
    mc->editcommand = NULL;
    mc->label = NULL;
    mc->contenttype = malloc(1+strlen(rawentry));
    mc->next = NULL;
    if (!mc->contenttype) nomemabort();
    strcpy(mc->contenttype, rawentry);
    t = GetCommand(s, &mc->command);
    s = t;
    while (s) {
	char *arg, *eq;

        t = GetCommand(s, &arg);
        if (t) *t++ = '\0';
        eq = index(arg, '=');
        if (eq) *eq++ = '\0';
        arg = Cleanse(arg, 1);
	if (!strcmp(arg, "needsterminal")) {
	    mc->needsterminal = 1;
	} else if (!strcmp(arg, "copiousoutput")) {
	    mc->copiousoutput = 1;
        } else if (eq && !strcmp(arg, "test")) {
            mc->testcommand = DeQuote(eq);
        } else if (eq && !strcmp(arg, "edit")) {
            mc->editcommand = DeQuote(eq);
        } else if (eq && !strcmp(arg, "compose")) {
            mc->composecommand = DeQuote(eq);
        } else if (eq && !strcmp(arg, "label")) {
            mc->label = DeQuote(eq);
	} else if (strcmp(arg, "notes")) { /* IGNORE notes field */
/*	    if (*arg) fprintf(stderr, "mailto: Ignoring invalid mailcap flag: %s\n", arg); */
	}
	s = t;
    }
    free(rawentry);
    return(mc);
}

ProcessMailcapFiles() 
{
    char *s, *path = getenv("MAILCAPS"), *origpath;
    static char *stdpath = STDPATH;
    struct MailcapEntry *mc, *CurrentMailcapEntry = NULL;
    FILE *fp;

    if (!path) {
#ifdef AMIGA
        path = malloc(5 + strlen(stdpath));
        if (!path) nomemabort();
        strcpy(path, stdpath);
#else
        int uid = getuid();
        struct passwd *p;
        p = getpwuid(uid);
        if (p) path = malloc(5+strlen(p->pw_dir) + strlen(stdpath));
        if (!p || !path) nomemabort();
        strcpy(path, p->pw_dir);
        strcat(path, stdpath);
#endif
    } else {
        char *pathcopy;
        pathcopy = malloc(1+strlen(path));
        if (!pathcopy) nomemabort();
        strcpy(pathcopy, path);
        path = pathcopy;
    }
    origpath = path;
    while(path) {
        s = index(path, PATH_SEPARATOR);
        if (s) *s++ = '\0';
        fp = fopen(path, "r");
        while (fp && !feof(fp)) {
            mc = GetMailcapEntry(fp);
            if (!mc) continue;
            if ((mc->composecommand || mc->editcommand) && index(mc->contenttype, '*')) {
                printf("\nWARNING:  a mailcap file is configured to allow the generation\n");
                printf("of a content-type with a wildcard.  This is probably a mistake.\n");
                printf("The relevant mailcap entry is in the file %s\n", path);
                printf("and is for content-type %s.\n\n", mc->contenttype);
            }
            if (!FirstMailcapEntry) {
                FirstMailcapEntry = mc;
                CurrentMailcapEntry = mc;
            } else {
                CurrentMailcapEntry->next = mc;
                CurrentMailcapEntry = mc;
            }
	}
        if (fp) fclose(fp);
        path = s;
    }
    free(origpath);
    return(-1);
}

struct mailpart *
CreateNewPart() {
    struct mailpart *mp;
    struct MailcapEntry *mc = FirstMailcapEntry;
    int i, ans, resultcode;
    char LineBuf[100], *CmdBuf;

    mp = NewPart();
    if (!mp) return(NULL);
    if (!mc) {
        printf("There is no part-composing software installed in your site's mailcap files.\n");
        printf("So, The only way to include non-text data is via a file containing such data.\n");
        ans = 0;
    } else {
        printf("Please choose which kind of data you wish to insert:\n\n");
        printf("0: Raw data from a file, with you specifying the content-type by hand.\n");
        i = 0;
        while (mc) {
            if (mc->composecommand) {
                ++i;
                if (mc->label) {
                    printf("%d: %s\n", i, mc->label);
                } else {
                    printf("%d: data in '%s' format\n", i, mc->contenttype);
                }
            }
            mc = mc->next;
        }
        printf("\n\nEnter your choice as a number from 0 to %d: ", i);
        fflush(stdout);
        fgets(LineBuf, sizeof(LineBuf), stdin);
        ans = atoi(LineBuf);
    }
    if (ans == 0) {
        char *sdum;
        FILE *fpi, *fpo;
        printf("\nIf you want to include non-textual data from a file, enter the file name.\nIf not, just press ENTER (RETURN): ");
        fflush(stdout);
        fgets(LineBuf, sizeof(LineBuf), stdin);
        sdum = LineBuf+strlen(LineBuf) -1;
        while (sdum >= LineBuf && isspace((unsigned char) *sdum)) {
            *sdum = '\0';
            --sdum;
        }
        sdum=LineBuf;
        while (*sdum && isspace((unsigned char) *sdum)) ++sdum;
        if (! *sdum) {
            printf("Data insertion cancelled\n");
            return(NULL);
        }
        fpi = fopen(sdum, "r");
        if (!fpi) {
            printf("Cannot read %s, data insertion cancelled\n", sdum);
            return(NULL);
        }
        fpo = fopen(mp->filename, "w");
        if (!fpo) {
            printf("Cannot open temporary file, data insertion cancelled\n");
            return(NULL);
        }
        TranslateInputToEncodedOutput(fpi, fpo, ENC_NONE);
        fclose(fpi);
        fclose(fpo);
        mp->istext = 0;
        while (1) {
            int ct;
            printf("\nEnter the MIME Content-type value for this data type ('?' for a list): ");
            fflush(stdout);
            gets(LineBuf);
            if (index(LineBuf, '/')) break;
            printf("\nMIME content-type values are type/format pairs, and always include a '/'.\nThe types supported at your site include, but are not limited to:\n\n");
            mc=FirstMailcapEntry;
            ct = 0;
            while (mc) {
                if (mc->contenttype && index(mc->contenttype, '/') && !index(mc->contenttype, '*')) {
                    if (ct++) fputs(", ", stdout);
                    fputs(mc->contenttype, stdout);
                }
                mc = mc->next;
            }
            fputs("\n\nThe MIME content-type for file inclusion is 'application/octet-stream'.\n", stdout);
        }
        mp->content_type = freshcopy(LineBuf);
        mp->encoding_type_needed = WhichEncodingForFile(mp->filename, mp->content_type);
        printf("Included data in '%s' format\n", mp->content_type);
        SetTextFlags(mp);
        return(mp);
    } else if (ans<0 || ans > i) {
        printf("Data insertion cancelled\n");
        return(NULL);
    }
    i=1;
    mc=FirstMailcapEntry;
    while (mc) {
        if (mc->composecommand) {
            if (i == ans) break;
            ++i;
        }
        mc = mc->next;
    }
    CmdBuf = malloc(CMDSIZE);
    if (!CmdBuf) nomemabort();
    BuildCommand(CmdBuf, mc->composecommand, mp->filename);
    printf("Executing: %s\n", CmdBuf);
    resultcode = SYSTEM(CmdBuf);
    free(CmdBuf);
    if (resultcode) {
        printf("Command execution failed, nothing included\n");
        return(NULL);
    }
    if (access(mp->filename, R_OK)) {
        printf("No file was created, nothing included!\n");
        return(NULL);
    }
    printf("Included data in '%s' format\n", mc->contenttype);
    mp->istext = 0;
    mp->content_type = mc->contenttype;
    mp->encoding_type_needed = WhichEncodingForFile(mp->filename, mp->content_type);
    SetTextFlags(mp);
    return(mp);
}

SetTextFlags(mp)
struct mailpart *mp;
{
    char *s;
    for (s=mp->content_type; *s; ++s) if (isupper((unsigned char) *s)) *s = tolower((unsigned char) *s);
    if (!strncmp(mp->content_type, "text/", 5)) {
        mp->istext = 1;
        if (!strncmp(mp->content_type, "text/richtext", 13)) {
            mp->isrich = 1;
        }
    }
}

WhichEncodingForFile(fname, ctype)
char *fname, *ctype;
{
    int c, linesize=0, total=0, unsafechars=0, longlines=0;
    char *s;
    FILE *fp = fopen(fname, "r");
    if (!fp) {
        /* If the stupid editing program forks, this actually will do a 
              reasonable thing as long as it saves the file before the mail is sent */
        return(ENC_B64); /* safest */
    }
    while ((c = getc(fp)) != EOF) {
        if (c>127) ++unsafechars;
        ++total;
        if (c == '\n') {
            if (linesize > 79) ++longlines;
            linesize = 0;
        } else ++linesize;
        if (total>1000 && (longlines || unsafechars)) break;
    }
    fclose(fp);
    if (longlines || unsafechars) {
        for (s=ctype; *s; ++s) if (isupper((unsigned char) *s)) *s = tolower((unsigned char) *s);
        if (!strncmp(ctype, "image/", 6)
             || !strncmp(ctype, "audio/", 6)
             || !strncmp(ctype, "application/octet-stream", 24)
             || !strncmp(ctype, "video/", 6)) {
            return(ENC_B64);
        }
        if (!unsafechars) return(ENC_QP);

        return((total/unsafechars < 16) ? ENC_B64 : ENC_QP);
    }
    return(ENC_NONE);
}

BuildCommand(Buf, controlstring, TmpFileName)
char *Buf, *controlstring, *TmpFileName;
{
    char *from, *to;
    int prefixed = 0;
    int NamedFile=0;

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
                        ++NamedFile;
                    }
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
    *to = '\0';
    if (!NamedFile) {
        strcat(Buf, " > ");
        strcat(Buf, TmpFileName);
    }
}

CtypeMatch(ctype, pat)
char *ctype, *pat;
{
    int len;
    char *s;
    for (s=ctype; *s; ++s) if (isupper((unsigned char) *s)) *s = tolower((unsigned char) *s);
    for (s=pat; *s; ++s) if (isupper((unsigned char) *s)) *s = tolower((unsigned char) *s);
    if (!strcmp(ctype, pat)) {
        return(1); /* exact match, case-insensitive */
    }
    len = strlen(pat);
    if ((pat[--len] == '*')
         && (pat[--len] == '/')
         && (!strncmp(ctype, pat, len))
         && (ctype[len] == '/')){
        /* wildcard match */
        return(1);
    }
    return(0);
}

EditCurrentMessage(UseVisual)
int UseVisual;
{
    char *editor = NULL;
    char *CmdBuf, *CmdBuf2, LineBuf[100];
    struct mailpart *mp, *lastmp;
    struct MailcapEntry *mc;
    int partct=1, ans;

    CmdBuf = malloc(CMDSIZE);
    CmdBuf2 = malloc(CMDSIZE);
    if (!CmdBuf || !CmdBuf2) nomemabort();
    if (!PartEndsWithNewline) {
        putc('\n', fpout);
        PartEndsWithNewline=1;
    }
    fclose(fpout);
    if (UseVisual) editor = getenv("VISUAL");
    if (!editor) editor = getenv("EDITOR");
#ifdef AMIGA
    if (!editor) editor = FindConfig("MailEditor");
#endif
    if (!editor) editor = DEFAULT_EDITOR;
    if (!FirstPart->next) {
        /* Only one part */
        sprintf(CmdBuf, "%s %s", editor, FirstPart->filename);
        printf("Executing: %s\n", CmdBuf);
        SYSTEM(CmdBuf);
        if ((FirstPart->istext || FirstPart->isrich) &&
             FirstPart->encoding_type_needed == ENC_NONE &&
             ContainsEightBitChar(FirstPart->filename)) {
            FirstPart->encoding_type_needed = ENC_QP;
        }
        fpout = fopen(FirstPart->filename, "a");
        free(CmdBuf);
        free(CmdBuf2);
        return;
    }
    mp = FirstPart;
    while (mp) {
        printf("Part #%d is of type '%s'.\n", partct, mp->content_type);
        sprintf(CmdBuf, "%s %s", editor, mp->filename);
        if (mp->mc && mp->mc->editcommand) {
            BuildCommand(CmdBuf2, mp->mc->editcommand, mp->filename);
        } else {
            mc = FirstMailcapEntry;
            while (mc) {
                if (mc->editcommand && CtypeMatch(mp->content_type, mc->contenttype)) {
                    break;
                }
                mc = mc->next;
            }
            if (mc) {
                BuildCommand(CmdBuf2, mc->editcommand, mp->filename);
            } else {
                CmdBuf2[0] = '\0';
            }
        }
        if (CmdBuf2[0]) {
            printf("Which command do you want to use to edit it?\n\n");
            printf("1: %s\n", CmdBuf2);
            printf("2: %s\n", CmdBuf);
            printf("\n\nEnter 1 or 2, or 0 to not edit it: ");
            fflush(stdout);
            fgets(LineBuf, sizeof(LineBuf), stdin);
            ans = atoi(LineBuf);
        } else ans = 2;
        if (ans == 1) {
            printf("Executing: %s\n", CmdBuf2);
            SYSTEM(CmdBuf2);
        } else if (ans == 2) {
            printf("Executing: %s\n", CmdBuf);
            SYSTEM(CmdBuf);
        }
        if ((mp->istext || mp->isrich) &&
             mp->encoding_type_needed == ENC_NONE &&
             ContainsEightBitChar(mp->filename)) {
            mp->encoding_type_needed = ENC_QP;
        }
        ++partct;
        lastmp = mp;
        mp = mp->next;
    }
    fpout = fopen(lastmp->filename, "a");
    free(CmdBuf);
    free(CmdBuf2);
}


ProcessInitFiles() {
#ifdef AMIGA
    ProcessOneMailRC(mailRC, 0);
#else
    char fname[FILE_NAME_SIZE];

    ProcessOneMailRC("/usr/lib/Mail.rc", 0);
    sprintf(fname, "%s/.mailrc", gethome());
    ProcessOneMailRC(fname, 0);
    sprintf(fname, "%s/.AMS_aliases", gethome());
    ProcessOneMailRC(fname, 1);
#endif
}


ProcessOneMailRC(fname, IsAndrew)
char *fname;
int IsAndrew;
{
    FILE *fp;
    char *LineBuf, *sdum;

    fp = fopen(fname, "r");
    if (!fp) return;
    LineBuf = malloc(MAX_LINELENGTH);
    if (!LineBuf) nomemabort();
    while (fgets(LineBuf, MAX_LINELENGTH, fp)) {
        if (IsAndrew && LineBuf[0] == '#') continue;
        for (sdum = LineBuf; *sdum; ++sdum) {
            if (isupper((unsigned char) *sdum)) *sdum = tolower((unsigned char) *sdum);
        }
        if (IsAndrew) {
            HandleAliasCommand(LineBuf);
        } else if (!strncmp(LineBuf, "set ", 4)) {
            HandleSetCommand(LineBuf+4, 1);
        } else if (!strncmp(LineBuf, "unset ", 6)) {
            HandleSetCommand(LineBuf+6, 0);
        } else if (!strncmp(LineBuf, "alias ", 6)) {
            HandleAliasCommand(LineBuf+6);
        } else {
            /* ignore */
        }
    }
    free(LineBuf);
    fclose(fp);
}

HandleSetCommand(cmd, DoSet)
char *cmd;
int DoSet;
{
    char *s;
    int i;

    while (*cmd) {
        while (*cmd && isspace((unsigned char) *cmd)) ++cmd;
        for (s=cmd; *s && !isspace((unsigned char) *s); ++s) {;}
        *s++ = '\0';
        if (!strcmp(cmd, "askcc")) {
            V_askcc = DoSet;
        } else if (!strcmp(cmd, "dot")) {
            V_dot = DoSet;
        } else if (!strcmp(cmd, "ignore")) {
            V_ignore = DoSet;
        } else if (!strcmp(cmd, "verbose")) {
            V_verbose = DoSet;
        } else if (!strcmp(cmd, "quiet")) {
            V_quiet = DoSet;
        } else if (!strcmp(cmd, "keepblind")) {
            V_keepblind = DoSet;
        } else if (!strncmp(cmd, "splitsize ", 9)) {
            i = atoi(cmd+10);
            if (i < MINCHUNKSIZE) {
                fprintf(stderr, "Ignoring splitsize mailrc setting of %d -- the minimum value is %d\n", i, MINCHUNKSIZE);
            } else {
                SplitSize = i;
            }
        } else {
            /* ignore */
        }
        cmd = s;;
    }
}

struct alias {
    char *shortname, *longname;
    struct alias *next;
} *FirstAlias = NULL;

HandleAliasCommand(aliasline)
char *aliasline;
{
    struct alias *tmpalias;
    char *s, *s2;
    int len = strlen(aliasline);

    if (aliasline[len - 1] == '\n') {
        aliasline[len - 1] = '\0';
    }
    tmpalias = (struct alias *) malloc(sizeof (struct alias));
    s = malloc(len + 1);
    if (!s || !tmpalias) nomemabort();
    strcpy(s, aliasline);
    while (*s && isspace((unsigned char) *s)) { ++s;}
    for (s2=s; *s2 && !isspace((unsigned char) *s2); ++s2) {;}
    if (!*s2) {
        if (s != s2) printf("mailto: ignoring bad alias line in init file: %s\n", aliasline);
        free(s);
        free(tmpalias);
        return;
    }
    *s2++ = '\0';
    tmpalias->shortname = s;
    tmpalias->longname = s2;
    tmpalias->next = FirstAlias;
    FirstAlias = tmpalias;
}

EmitHeaderWithAliases(fp, hdr, names)
FILE *fp;
char *hdr;
char *names;
{
    fputs(hdr, fp);
    fputs(": ", fp);
    EmitAddresses(fp, names);
    fputs("\n", fp);
}

EmitAddresses(fp, names)
FILE *fp;
char *names;
{
    char *s;
    while (names) {
        s= index(names, ',');
        if (s) *s = '\0';
        DeAlias(names, fp);
        if (s) {
            *s++ = ',';
            fputs(",\n\t", fp);
        }
        names = s;
    }
}

DeAlias(name, fp)
char *name;
FILE *fp;
{
    struct alias *tmpalias;
    char *end, savechar;

    while (isspace((unsigned char) *name)) ++name;
    end = name+strlen(name)-1;
    while (isspace((unsigned char) *end)) --end;
    ++end;
    savechar = *end;
    for (tmpalias=FirstAlias; tmpalias; tmpalias = tmpalias->next) {
        if (!strcmp(tmpalias->shortname, name)) {
            *end = savechar;
            EmitAddresses(fp, tmpalias->longname);
            return;
        }
    }
    *end = savechar;
    fputs(name, fp);
}


WriteCtypeNicely(fp, ct)
FILE *fp;
char *ct;
{
    char *semi, *slash, *eq, *s;

    for (s = ct; *s; ++s) {
        if (*s == '\n') *s = ' ';
    }
    semi = (char *) index(ct, ';');
    if (semi) *semi = '\0';
    slash = (char *) index(ct, '/');
    fputs(ct, fp);
    if (!slash) fputs("/unknown", fp);
    while (semi) {
        ct = semi + 1;
        *semi = ';';
        semi = (char *) index(ct, ';');
        if (semi) *semi = '\0';
        eq = (char *) index(ct, '=');
        if (eq) *eq = '\0';
        fputs(";\n\t", fp);
        while (isspace((unsigned char) *ct)) ++ct;
        fputs(ct, fp);
        if (eq) {
            s = eq;
            fputs("=", fp);
            ++s;
            while (isspace((unsigned char) *s)) ++s;
            fputsquoting(s, fp);
            *eq = '=';
        }
    }
    fputs("\n", fp);
}

fputsquoting(s, fp)
char *s;
FILE *fp;
{
    char *end = s + strlen(s) - 1;
    while (isspace((unsigned char) *end) && end > s) --end;
    if (*s == '\"') {
        putc(*s, fp);
        while (*++s) {
            if (*s == '\"') break; /* MAY TERMINATE EARLY! */
            if (*s == '\\') {
                putc(*s, fp);
                ++s; /* Don't check this next char */
                if (!*s) break;
            }
            putc(*s, fp);
        }
        putc('\"', fp);
    } else {
        putc('\"', fp);
        putc(*s, fp);
        while (*++s) {
            if (*s == '\"' || *s == '\\') {
                putc('\\', fp);
            }
            putc(*s, fp);
        }
        putc('\"', fp);
    }
}
        
        
#ifdef AMIGA
/*
 * The DICE system() routine does not connect stdin to the terminal.
 * We tell the shell to open stdin from the tty by redirecting the
 * command input to '*'.
 */
int
systemWithStdin(cmd)
char *cmd;
{
    char *cmdBuf;
    char *cp, *to;
    int result;
  
    cmdBuf = malloc(CMDSIZE);
    if (!cmdBuf) nomemabort();
    if (!index(cmd, '<')) {
        for (cp = cmd; *cp == ' ' || *cp == '\t'; cp++)
            ;
        if (*cp == 0) {
            return (0);
        }
        to = cmdBuf;
        while (*cp && *cp != ' ' && *cp != '\t') {
            *to++ = *cp++;
        }
        strcpy(to, " <*");
        strcat(to, cp);
        result = system(cmdBuf);
    } else {
        result = system(cmd);
    }
    free(cmdBuf);
    return(result);
}
#endif

controlputc(c)
char c;
{
    fputc(c, stdout);
}

/* Do the equivalent of an fputs for the terminal escape stuff */
#ifdef AMIGA
tfputs(s)
char *s;
{
    fputs(s, stdout);
    return (0);
}
#else
tfputs(s)
char *s;
{
    tputs(s, 1, controlputc);
}
#endif
ContainsEightBitChar(fname)
char *fname;
{
    int c, eightBitSeen = 0;
    FILE *fp = fopen(fname, "r");
    if (!fp) {
        /* If the stupid editing program forks, this actually will do a
              reasonable thing as long as it saves the file before the mail is sent */
        return(1); /* safest */
    }
    while ((c = getc(fp)) != EOF) {
        if (c>127) {
            eightBitSeen = 1;
            break;
        }
    }
    fclose(fp);
    return(eightBitSeen);
}

SwitchToEuropean() {
    printf("WARNING:  You have entered 8-bit characters in what is supposed to be\n");
    printf("plain ASCII text.  If you are using a non-ASCII character set, you should\n");
    printf("declare this to be the case with the MM_CHARSET environment variable.\n");
    printf("For now, I am assuming you are using the iso-8859-1 character set,\n");
    printf("but this may be false.\n");
    CharacterSet = "iso-8859-1";
}
