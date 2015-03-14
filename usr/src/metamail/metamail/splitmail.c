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
#include <sys/types.h>
#include <sys/stat.h>
#include <config.h>

#define MINCHUNKSIZE 20000 /* Better be enough to hold the headers, or we die! */
extern char *malloc(), *index();

#ifdef AMIGA
#define Prototype   extern

#include <getfiles.h>
#include <time.h>
#include <lib_protos.h>

#define NORMALDELIVERYCMD NormalDeliveryCmd
#define VERBOSEDELIVERYCMD VerboseDeliveryCmd
#else
extern char *getenv();
#define NORMALDELIVERYCMD "/usr/lib/sendmail -t -oi"
#define VERBOSEDELIVERYCMD "/usr/lib/sendmail -t -v -oi"
#endif

usageexit() {
    fprintf(stderr, "Usage:  splitmail [-d] [-v] [-s splitsize] [-i id-suffix] [-p prefix] [file-name]\n");
    exit(-1);
}

char *
endofheader(s)
char *s;
{
    char *orgs = s, c;
    while (1) {
	s = index(s, '\n');
	if (!s) return(orgs+strlen(orgs));
	c = *(s+1);
        if (c != ' ' && c != '\t') return(s);
        ++s;
    }
}

main(argc, argv)
char **argv;
{
    int i, DoDeliver=0, SplitSize=DEFAULT_SPLIT_SIZE, dum, InNewline=1, bytesread, whichpart=1, Verbose=0, numparts = -1, c;
    char *fname = NULL, *bigbuf, *s, *SharedHeaders, *headend, *from, id[100], hostname[60], *deliverycmd, *prefix = "/tmp/split.", SubjectBuf[250];
    char *MessageID = 0;
    FILE *fp;
#ifdef AMIGA
     char *NormalDeliveryCmd;
     char VerboseDeliveryCmd[100];
#endif    

    s = getenv("SPLITSIZE");
    if (s) {
        dum = atoi(s);
        if (dum < MINCHUNKSIZE) {
            fprintf(stderr, "Ignoring SPLITSIZE environment variable of %d -- the minimum value is %d\n", dum, MINCHUNKSIZE);
        } else {
            SplitSize = dum;
        }
    }
#ifdef AMIGA
    NormalDeliveryCmd = GetConfigProgram("Sendmail");
    strcpy(VerboseDeliveryCmd, NormalDeliveryCmd);
    strcat(VerboseDeliveryCmd, "-v");
#endif
    for (i=1; i<argc; ++i) {
	if (argv[i][0] == '-') {
	    switch (argv[i][1]) {
		case 's':
		    if (++i >= argc) usageexit();
		    dum = atoi(argv[i]);
		    if (dum < MINCHUNKSIZE && dum >= 0) {
			fprintf(stderr, "splitmail: Using minimum splitsize of %d\n", MINCHUNKSIZE);
			dum = MINCHUNKSIZE;
			
		    }
		    SplitSize = dum;
		    break;
		case 'd':
		    DoDeliver = 1;
                    break;
                case 'p':
		    if (++i >= argc) usageexit();
                    prefix = argv[i];
                    break;
 		case 'i':
 		    if( ++i >= argc) usageexit();
 		    MessageID = argv[i];
 		    break;
		case 'v':
		    Verbose = 1;
		    break;
		default:
		    usageexit();
	    }
	} else {
	    if (fname) usageexit();
	    fname = argv[i];
	}
    }
    bigbuf = malloc(1+SplitSize);
    if (!bigbuf) {
	fprintf(stderr, "splitmail:  Not enough memory for %d-byte chunks\n", SplitSize);
	exit(-1);
    }
    SplitSize -= 1000; /* gives fudge factor for headers, line endings */
    if (fname) {
        struct stat stbuf;
        if (!stat(fname, &stbuf)) {
            /* Note:  this will sometimes be 1 too high when it is a very close call,
              because of the desire to have complete lines.  In such cases, we send
              a null final part */
            numparts = 1 + (stbuf.st_size / SplitSize);
        }
	fp = fopen(fname, "r");
	if (!fp) {
	    fprintf(stderr, "splitmail: Cannot read file %s\n", fname);
	    exit(-1);
	}
    } else fp = stdin;
    headend = bigbuf;
    while((c=getc(fp)) != EOF) {
        if (headend >= bigbuf + SplitSize) {
            fprintf(stderr, "splitmail: Could not find the end of the headers!\n");
            exit(-1);
        }
	*headend++ = c;
	if (c == '\n') {
	    if (InNewline) break; /* end of headers */
	    InNewline = 1;
	} else {
	    InNewline = 0;
	}
    }
    if (c == EOF) {
	fprintf(stderr, "splitmail: Could not find the end of the headers!\n");
	exit(-1);
    }
    *headend = '\0';
    SharedHeaders = malloc(1+strlen(bigbuf)); /* maximum size needed */
    if (!SharedHeaders) {
	fprintf(stderr, "splitmail: Not enough memory\n");
	exit(-1);
    }
    from = bigbuf;
    *SharedHeaders = '\0';
    strcpy(SubjectBuf, "Partial Message");
    while (from < headend) {
	s = endofheader(from);  /* would be index(from, '\n'),
				 but need to check for continuation lines */
        *s = '\0';
	if (ShareThisHeader(from, SubjectBuf)) {
	    strcat(SharedHeaders, from);
	    strcat(SharedHeaders, "\n");
	}
	*s = '\n';
	from = ++s;
    }
#ifdef AMIGA
    sprintf(id, "%d.%s@%s%s", time(0), SeqToName(GetSequence(4)), FindConfig("NodeName"),
            FindConfig("DomainName"));
#else
#ifdef SYSV
    uname(hostname);
#else
    gethostname(hostname, sizeof(hostname));
#endif /* SYSV */
    sprintf(id, "%d.%d.%d.%s", getuid(), getpid(), time(0), hostname);
#endif /* AMIGA */
    bytesread = headend - bigbuf;
    deliverycmd = Verbose ? VERBOSEDELIVERYCMD : NORMALDELIVERYCMD;
    while (!feof(fp)) {
	while (SplitSize > bytesread && !feof(fp)) {
	    /* Need to loop because fread is weird */
	    bytesread += fread(bigbuf + bytesread, sizeof(char), SplitSize - bytesread, fp);
	}
	/* Now complete the line */
	while((c=getc(fp)) != EOF) {
	    bigbuf[bytesread++] = c;
	    if (c=='\n') break;
	}
        bigbuf[bytesread] = '\0';
        if (feof(fp) && numparts <= 0) numparts = whichpart;
        HandleOnePart(DoDeliver, deliverycmd, prefix, numparts, whichpart, SharedHeaders, SubjectBuf, id, MessageID, bigbuf);
	bytesread = 0;
	++whichpart;
    }
    while (whichpart <= numparts) {
        /* Our guess as to how many parts was OFF, hopefully only by one */
        *bigbuf = '\0'; /* have to deliver an empty part, ugh! */
        HandleOnePart(DoDeliver, deliverycmd, prefix, numparts, whichpart, SharedHeaders, SubjectBuf, id, MessageID, bigbuf);
        ++whichpart;
    }
    return(0);
}

HandleOnePart(DoDeliver, deliverycmd, prefix, numparts, whichpart, SharedHeaders, SubjectBuf, id, MessageID, bigbuf)
int DoDeliver, numparts, whichpart;
char *deliverycmd, *prefix, *SharedHeaders, *SubjectBuf, *id, *MessageID, *bigbuf;
{
    FILE *fpout;
    char OutputFile[1000];
    int code;

    if (DoDeliver) {
        fpout = popen(deliverycmd, "w");
    } else {
        sprintf(OutputFile, "%s%d", prefix, whichpart);
        fpout = fopen(OutputFile, "w");
    }
    if (!fpout) {
        fprintf(stderr, "splitmail: Can't open %s for writing\n", deliverycmd);
        exit(-1);
    }
    if (numparts != 1) { /* one-parters end up not changed at all! */
        fputs(SharedHeaders, fpout);
 	if( MessageID) {
            fprintf(fpout, "Message-Id: <%d.%s.%s>\n",whichpart,id,MessageID);
        }
        fprintf(fpout, "Subject: %s (part %d of ", SubjectBuf, whichpart);
        if (numparts > 0) {
            fprintf(fpout, "%d)\n", numparts);
        } else {
            fprintf(fpout, "several)\n");
        }
        fprintf(fpout, "MIME-Version: 1.0\nContent-type:message/partial; id=%s; number=%d", id, whichpart);
        if (numparts > 0) fprintf(fpout, "; total=%d", numparts);
        fputs("\n\n", fpout);
    }
    fputs(bigbuf, fpout);
    code = DoDeliver ? pclose(fpout) : fclose(fpout);
    if (code) {
        fprintf(stderr, "splitmail: %s of part %d failed\n", DoDeliver ? "Delivery" : "Writing", whichpart);
        if (whichpart > 1) fprintf(stderr, "  (previous %d parts may have succeeded)\n", whichpart -1);
        exit(-1);
    }
}

static char *SharedHeads[] = {
    "from",
    "to",
    "cc",
    "bcc",
    NULL
};

ShareThisHeader(s, SubjectBuf)
char *s;
char *SubjectBuf;
{
    int i;
    char *colon = index(s, ':');
    if (!colon) return(0); /* don't share it in all parts */
    *colon = '\0';
    if (!ULstrcmp(s, "subject")) {
        *colon = ':';
        strcpy(SubjectBuf, ++colon);
        return(0);
    }
    for (i=0; SharedHeads[i]; ++i) {
	if (!ULstrcmp(s, SharedHeads[i])) break;
    }
    *colon = ':';
    return(SharedHeads[i] ? 1 : 0);
}

int ULstrcmp(s1, s2)
register char *s1, *s2;
{
    char c1,c2;

    for(;;) {
	c1 = *s1++; if (c1 <= 'Z') if (c1 >= 'A') c1 += 040;
	c2 = *s2++; if (c2 <= 'Z') if (c2 >= 'A') c2 += 040;
	if (c1 != c2) break;
	if (c1 == '\0') return(0);
    }
    return(c1 - c2);
}

/* STILL TO DO:  
  Get number of parts right when possible 
*/
