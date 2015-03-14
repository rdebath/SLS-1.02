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
#include <stdio.h>
#include <mail.h> /* from andrew distribution */
#ifdef	SYSV
#include <sys/types.h>
/* Different people say different things about whether unistd.h lives in sys/ */
/* #include <sys/unistd.h> */
#include <unistd.h>
#include <sys/fcntl.h>
#include <sys/termio.h>
#endif
#include <sys/file.h>
#include <config.h>

#ifdef AMIGA
#define BUFFER_SIZE (BUFSIZ+1)
#else
#define BUFFER_SIZE 5000
extern char **environ;
#endif

main(argc, argv)
int argc;
char **argv;
{
    struct ScribeState ScribeState;
    FILE *infp;
    int bytes, code;
    char *prog = "ez", *fname;
    char TmpName[100];
    char BigBuf[BUFFER_SIZE];
#ifndef AMIGA
    char adir[1200], *s;
#endif
    
    if (argc < 2) exit(-1);
    if (argc > 2) {
	prog = argv[1];
	fname = argv[2];
    } else {
	fname = argv[1];
    }
    TmpName[0] = 0;
    if (!strcmp(fname, "-")) {
        FILE *fp;
        int c;

#ifdef AMIGA
        strcpy(TmpName, "T:mmXXXXXX");
        mktemp(TmpName);
#else
        sprintf(TmpName, "/tmp/ezview.%d", getpid());
#endif
        fp = fopen(TmpName, "w");
        if (!fp) exit(-1);
        while ((c = getc(stdin)) != EOF) putc(c, fp);
        fclose(fp);
        fname = TmpName;
    }
#ifndef AMIGA
    sprintf(BigBuf, "%s -geometry +0+0 -d %s", prog, fname);
    if (!getenv("ANDREWDIR")) {
        /* Add *some* ANDREWDIR variable, to help with the fonts */
        if (argc >2) {
            sprintf(adir, "ANDREWDIR=%s", argv[1]);
            s = (char *) rindex(adir, '/');
            if (s) {
                *s = NULL;
                s = (char *) rindex(adir, '/');
                if (s) *s = NULL;
            }
        } else {
            if (!access("/usr/andrew", R_OK)) {
                sprintf(adir, "ANDREWDIR=%s", "/usr/andrew");
            } else if (!access("/usr/local/pkg/X11/andrew", R_OK)) {
                sprintf(adir, "ANDREWDIR=%s", "/usr/local/pkg/X11/andrew");
            } else {
                adir[0] = NULL; /* Hope for compiled-in default! */
            }
        }
        if (adir[0]) {
            /* Add to the environment here */
            char **newenviron;
            int environsize;
            for (environsize=0; environ[environsize]; ++environsize) {;}
            newenviron = (char **) malloc(sizeof(char *) * (2+environsize));
            newenviron[environsize+1] = NULL;
            newenviron[environsize] = adir;
            while (environsize--) {
                newenviron[environsize] = environ[environsize];
            }
            environ = newenviron;
        }
    }
    if (!getenv("DISPLAY") || RunCmd(BigBuf))
#endif
    {
        infp = fopen(fname, "r");
        if (infp == NULL) {
            fprintf(stderr, "%s: Unable to open \"%s\" for input.\n", argv[0],
                    fname);
            if (TmpName[0]) unlink(TmpName);
            exit(10);
        }
	code = UnScribeInit("12", &ScribeState);
        while ((bytes = fread(BigBuf, 1, sizeof(BigBuf) - 1, infp)) > 0) {
	    UnScribe(code, &ScribeState, BigBuf, bytes, stdout);
	}
	UnScribeFlush(code, &ScribeState, stdout);
        fclose(infp);
    }
    if (TmpName[0]) unlink(TmpName);
    exit(0);
}

/* The following returns static data which is overwritten with each call; it must therefore be used only once in a sprintf, for example. */
static char *RepresentChar(c)
char c;
{
    static char ans[20];

    if (c == '\015') {
	strcpy(ans, "RETURN");
    } else if (c <= '\032') {
	sprintf(ans, "CTL-%c", c+'A'-1);
    } else if (c == '\033') {
	strcpy(ans, "ESC");
    } else if (c == '\177') {
	strcpy(ans, "DELETE/RUBOUT");
    } else {
	ans[0] = c;
	ans[1] = '\0';
    }
    return(ans);
}

#ifndef AMIGA
#ifdef __hpux
#include <termio.h>
#else
#include <sys/ioctl.h>
#endif

RunCmd(BigBuf)
char *BigBuf;
{
    char abort = '\177';
#ifdef __hpux
#define SYSV    /* Is this necessary? */
#endif
#ifdef SYSV
    struct termio blob;

    if (!ioctl(0, TCGETA, &blob)) {
        abort = blob.c_cc[VINTR];
    }
#else
    struct tchars blob;

    if (!ioctl(0, TIOCGETC, &blob)) {
        abort = blob.t_intrc;
    }
#endif
    fprintf(stderr, "Now executing the EZ program -- if an EZ window appears, you can quit it by\n    holding down the middle mouse button and selecting the 'Quit' menu.\n\nYou can prevent the window from appearing by INTERRUPTING (%s) now,\n    in which case you should see a text-only version of the data.\n", RepresentChar(abort));
    return(system(BigBuf));
}
#endif
