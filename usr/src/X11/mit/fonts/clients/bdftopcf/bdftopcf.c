/* $Header: /home/x_cvs/mit/fonts/clients/bdftopcf/bdftopcf.c,v 1.3 1992/05/23 12:42:08 dawes Exp $ */
/*
 * $XConsortium: bdftopcf.c,v 1.4 91/05/13 15:25:06 gildea Exp $
 * 
 * Copyright 1991 by the Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided 
 * that the above copyright notice appear in all copies and that both that 
 * copyright notice and this permission notice appear in supporting 
 * documentation, and that the name of M.I.T. not be used in advertising
 * or publicity pertaining to distribution of the software without specific, 
 * written prior permission.  M.I.T. makes no representations about the 
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * The X Window System is a Trademark of MIT.
 */
#include    <X11/X.h>
#include    <X11/Xproto.h>
#include    "fontmisc.h"
#include    "fontstruct.h"
#include    "bitmap.h"
#include    <stdio.h>

main (argc, argv)
    char	**argv;
{
    FontRec	font;
    FontFilePtr input, output;
    char	*input_name = 0, *output_name = 0;
    char	*program_name;
    int		bit, byte, glyph, scan;

    FontDefaultFormat (&bit, &byte, &glyph, &scan);
    program_name = argv[0];
    argc--, argv++;
    while (argc-- > 0) {
	if (argv[0][0] == '-') {
	    switch (argv[0][1]) {
	    case 'p':
		switch (argv[0][2]) {
		case '1':
		case '2':
		case '4':
		case '8':
		    if (argv[0][3] != '\0')
			goto usage;
		    glyph = argv[0][2] - '0';
		    break;
		default:
		    goto usage;
		}
		break;

	    case 'u':
		switch (argv[0][2]) {
		case '1':
		case '2':
		case '4':
		    if (argv[0][3] != '\0')
			goto usage;
		    scan = argv[0][2] - '0';
		    break;
		default:
		    goto usage;
		}
		break;

	    case 'm':
		if (argv[0][2] != '\0')
		    goto usage;
		bit = MSBFirst;
		break;

	    case 'l':
		if (argv[0][2] != '\0')
		    goto usage;
		bit = LSBFirst;
		break;

	    case 'M':
		if (argv[0][2] != '\0')
		    goto usage;
		byte = MSBFirst;
		break;

	    case 'L':
		if (argv[0][2] != '\0')
		    goto usage;
		byte = LSBFirst;
		break;

	    case 't':	/* attempt to make terminal fonts if possible */
		if (argv[0][2] != '\0')
		    goto usage;
		break;

	    case 'i':	/* inhibit ink metric computation */
		if (argv[0][2] != '\0')
		    goto usage;
		break;
	    case 'o':
		if (argv[0][2])
		    output_name = argv[0] + 2;
		else
		{
		    if (!argv[1])
			goto usage;
		    argv++;
		    argc--;
		    output_name = argv[0];
		}
		break;
	    default:
		goto usage;
	    }
	} else {
	    if (input_name)
	    {
	usage:
		fprintf(stderr,
	"usage: %s [-p#] [-u#] [-m] [-l] [-M] [-L] [-t] [-i] [-o pcf file] [bdf file]\n",
			program_name);
		fprintf(stderr,
			"       where # for -p is 1, 2, 4, or 8\n");
		fprintf(stderr,
			"       and   # for -s is 1, 2, or 4\n");
		exit(1);
	    }
	    input_name = argv[0];
	}
	argv++;
    }
    input = FontFileOpen (input_name, "r");
    if (!input)
    {
	fprintf (stderr, "%s: can't open bdf source file %s\n",
		 program_name, input_name ? input_name : "<stdin>");
	exit (1);
    }
    if (bdfReadFont (&font, input, bit, byte, glyph, scan) != Successful)
    {
	fprintf (stderr, "%s: bdf input, %s, corrupt\n",
		 program_name, input_name);
	exit (1);
    }
    output = FontFileOpen (output_name, "w");
    if (!output)
    {
	fprintf (stderr, "%s: can't open pcf sink file %s\n",
		 program_name, output_name ? output_name : "<stdout>");
	exit (1);
    } 
    if (pcfWriteFont (&font, output) != Successful)
    {
	fprintf (stderr, "%s: can't write pcf file %s\n",
		 program_name, output_name ? output_name : "<stdout>");
	if (output_name)
	    unlink (output_name);
	exit (1);
    }
    else
	FontFileClose (output);
    exit (0);
}
