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
#include <ctype.h>
#include "richlex.h"

char *translate(t)
char *t;
{
    if (!strcmp(t, "fixed")) return("typewriter");
    if (!strcmp(t, "excerpt")) return("quotation");
    /* Really ought to handle ISO-10646 and ISO-8859-X somehow */
    return(t);
}

main() {
    int c, i, JustDidNewline = 0;
    char tok[MAX_TOKEN_SIZE + 1],*token;

    fputs("\\begindata{text, 42}\n\\template{messages}\n", stdout);
    while((c = richtextlex(stdin,tok + 1)) != EOF) {
	if (c == RICHTEXT_COMMAND || c == RICHTEXT_NEG_COMMAND) {
	    if (c == RICHTEXT_NEG_COMMAND) {
		tok[0] = '/';
		token = tok;
	    } else
		token = tok + 1;
            if (!strcmp(token, "lt")) {
                putc('<', stdout);
                JustDidNewline = 0;
            } else if (!strcmp(token, "nl")) {
                fputs(JustDidNewline ? "\n" : "\n\n", stdout);
                JustDidNewline = 1;
            } else if (!strcmp(token, "/paragraph")) {
                fputs(JustDidNewline ? "\n\n" : "\n\n\n", stdout);
                JustDidNewline = 1;
            } else if (!strcmp(token, "comment")) {
                while (strcmp(token, "/comment")) {
                    while ((c = getc(stdin)) != '<') ;
                    for (i=0; (c = getc(stdin)) != '>'; ++i) {
                        token[i] = isupper(c) ? tolower(c) : c;
                    }
                    token[i] = NULL;
                }
            } else if (!ignoretoken(token)) {
                if (token[0] == '/') {
                    putc('}', stdout);
                } else {
                    fprintf(stdout, "\\%s{", translate(token));
                    JustDidNewline = 0;
                }
            }
        } else if (c == '\n') {
            putc(' ', stdout);
            JustDidNewline = 0;
        } else {
            putc(c, stdout);
            JustDidNewline = 0;
        }
    }
    fputs("\n \n\\enddata{text, 42}\n", stdout);
}

ignoretoken(t)
char *t;
{
    if (*t == '/') ++t;
    if (!strcmp(t, "us-ascii")) return(1);
    if (!strcmp(t, "paragraph")) return(1); /* handled otherwise */
    if (!strcmp(t, "no-op")) return(1);
    return(0);
}
