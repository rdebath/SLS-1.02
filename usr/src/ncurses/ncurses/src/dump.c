
/* This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for   *
*  details. If they are missing then this copy is in violation of    *
*  the copyright conditions.                                        */

/*
 *	dump.c - dump the contents of a compiled terminfo file in a
 *		 human-readable format.
 *
 */

#include "compiler.h"
#include "terminfo.h"
#include <stdlib.h>

static void expand(unsigned char *);
static void usage(void);

void main(argc, argv)
int	argc;
char	*argv[];
{
	int		i, j;
	int		cur_column;
	char		buffer[1024];
	TERMINAL	current_term;
	char		*terminal;
	char		*terminfo;

	cur_term = &current_term;
	
	if((terminal = getenv("TERM")) == NULL)
	{
		fprintf(stderr,"untic: environment variable TERM not set\n");
		exit(1);
	}
	
	if((terminfo = getenv("TERMINFO")) == NULL)
	{
		terminfo = SRCDIR;
	}
	
	switch(argc) {
	case 1:
		/* no parameters */
		sprintf(buffer, "%s/%c/%s", terminfo, *terminal, terminal);
		break;
	case 2:
		/* parameter is terminal name */
		if(argv[1][0] == '-' && (argv[1][1] == '?' || argv[1][1] == 'h'))
			usage();
		else
			sprintf(buffer, "%s/%c/%s", terminfo, *argv[1], argv[1]);
		break;
	case 3:
		/* parameter is "-f filename" */
		if(argv[1][0] == '-' && argv[1][1] == 'f')
			sprintf(buffer, "%s", &argv[1][3]);
		else
			usage();
		break;
	default:
		usage();
	}
	
	if (read_entry(buffer, cur_term) < 0) {
		fprintf(stderr, "file %s may not be a terminfo entry\n", argv[j]);
		exit(1);
	}

	printf("%s,\n\t", cur_term->term_names);
	cur_column = 9;

	for (i=0; i < BOOLCOUNT; i++)
	{
		if (cur_term->Booleans[i] == TRUE)
		{
			if (cur_column > 9 &&  cur_column + strlen(boolnames[i]) + 2 > 79)
			{
				printf("\n\t");
				cur_column = 9;
			}
			printf("%s, ", boolnames[i]);
			cur_column += strlen(boolnames[i]) + 2;
		}
	}

	for (i=0; i < NUMCOUNT; i++)
	{
		if (cur_term->Numbers[i] != -1)
		{
			if (cur_column > 9 &&  cur_column + strlen(numnames[i]) + 5 > 79)
			{
				printf("\n\t");
				cur_column = 9;
		 	}
			printf("%s#%d, ", numnames[i], cur_term->Numbers[i]);
			cur_column += strlen(numnames[i]) + 5;
		}
	}

	for (i=0; i < STRCOUNT; i++)
	{
		if (cur_term->Strings[i])
		{
			sprintf(buffer, "%s=%s, ", strnames[i], cur_term->Strings[i]);
			expand(buffer);
		 	if (cur_column > 9  &&  cur_column + strlen(buffer) > 79)
			{
				printf("\n\t");
				cur_column = 9;
		 	}
			printf("%s", buffer);
			cur_column += strlen(buffer);
		}
	}
	putchar('\n');
}


void expand(unsigned char *str)
{
char	buffer[1024];
int	bufp;
unsigned char	*ptr;

	bufp = 0;
	ptr = str;
	while (*str) {
	    if (*str < ' ') {
		buffer[bufp++] = '^';
		buffer[bufp++] = *str + '@';
	    }
	    else if (*str < '\177')
		buffer[bufp++] = *str;
	    else {
		sprintf(&buffer[bufp], "\\%03o", *str);
		bufp += 4;
	    }

	    str++;
	}

	buffer[bufp] = '\0';
	strcpy(ptr, buffer);
}

void
usage()
{
	fprintf(stderr,"\nusage: untic [term] [-f file] - decompile terminfo database entry\n");
	fprintf(stderr,"  term       - terminal name to be decompiled\n");
	fprintf(stderr,"  -f file    - filename to be used to decompile\n");
	fprintf(stderr,"  no options - terminal name and terminfo path got from TERM/TERMINFO\n\n");
	exit(1);
}

