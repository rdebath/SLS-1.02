
/* This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for   *
*  details. If they are missing then this copy is in violation of    *
*  the copyright conditions.                                        */

/*
 *	comp_main.c --- Main program for terminfo compiler
 *
 */

#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "compiler.h"

char	*source_file = "terminfo";
char	*destination = SRCDIR;
char	*usage_string = "\ttic [-v[n]] source-file\n";
char	check_only = 0;

extern void init(char *);
extern void make_hash_table(void);
extern void compile(void);

int main (argc, argv)
int	argc;
char	*argv[];
{
	int	i;
	int	argflag = FALSE;

	debug_level = 0;

	for (i=1; i < argc; i++)
	{
	    if (argv[i][0] == '-')
	    {
		switch (argv[i][1])
		{
		    case 'c':
			check_only = 1;
			break;

		    case 'v':
			debug_level = argv[i][2]  ?  atoi(&argv[i][2])  :  1;
			break;

		    default:
			fprintf(stderr, "%s: Unknown option. Usage is:\n\t%s\n",
						       argv[0], usage_string);
			exit(1);
		}
	    }
	    else if (argflag)
	    {
		fprintf(stderr, "%s: Too many file names.  Usage is:\n\t%s\n",
							argv[0], usage_string);
		exit(1);
	    }
	    else
	    {
		argflag = TRUE;
		source_file = argv[i];
	    }
	}

	init(argv[0]);
	make_hash_table();
	compile();

	exit(0);
}


/*
 *	init(progname)
 *
 *	Miscelaneous initialisations
 *
 *	Open source file as standard input
 *	Check for access rights to destination directories
 *	Create any directories which don't exist.
 *
 */

void init(progname)
char	*progname;
{
	struct stat	statbuf;
	char		*dirnames = "abcdefghijklmnopqrstuvwxyz0123456789";
	char		*getenv();
	char		dir[2];

	start_time = time(0);

	curr_line = 0;

	if (freopen(source_file, "r", stdin) == NULL)
	{
	    fprintf(stderr, "%s: Can't open %s\n", progname, source_file);
	    exit(1);
	}

	if (getenv("TERMINFO") != NULL)
	    destination = getenv("TERMINFO");

	if (access(destination, 7) < 0)
	{
	    fprintf(stderr, "%s: %s non-existant or permission denied\n",
							progname, destination);
	    exit(1);
	}

	if (chdir(destination) < 0)
	{
	    fprintf(stderr, "%s: %s is not a directory\n",
							progname, destination);
	    exit(1);
	}
	
	dir[1] = '\0';
	for (dir[0] = *dirnames; *dirnames != '\0'; dir[0] = *(++dirnames))
	{
	    if (stat(dir, &statbuf) < 0)
	    {
		mkdir(dir, 0755);
	    }
	    else if (access(dir, 7) < 0)
	    {
		fprintf(stderr, "%s: %s/%s: Permission denied\n",
						    progname, destination, dir);
		exit(1);
	    }
	    else if ((statbuf.st_mode & S_IFMT) != S_IFDIR)
	    {
		fprintf(stderr, "%s: %s/%s: Not a directory\n",
						    progname, destination, dir);
		exit(1);
	    }
	}
}


