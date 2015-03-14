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

#define BASE64 1
#define QP 2 /* quoted-printable */

main(argc, argv)
int argc;
char **argv;
{
    int encode = 1, which = BASE64, i;
    FILE *fp = stdin;

    for (i=1; i<argc; ++i) {
        if (argv[i][0] == '-') {
            switch (argv[i][1]) {
                case 'u':
                    encode = 0;
                    break;
                case 'q':
                    which = QP;
                    break;
                case 'b':
                    which = BASE64;
                    break;
		default:
                    fprintf(stderr,
                       "Usage: mmencode [-u] [-q] [-b] [file name]\n");
                    exit(-1);
            }
        } else {
            fp = fopen(argv[i], "r");
            if (!fp) {
                perror(argv[i]);
                exit(-1);
            }
        }
    }
    if (which == BASE64) {
        if (encode) to64(fp, stdout); else from64(fp, stdout, NULL, 0);
    } else {
        if (encode) toqp(fp, stdout); else fromqp(fp, stdout, NULL, 0);
    }
    return(0);
}

