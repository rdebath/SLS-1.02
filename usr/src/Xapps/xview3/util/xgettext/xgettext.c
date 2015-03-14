#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)xgettext.c 1.10 91/09/14";
#endif
#endif

#include "xgettext.h"

/*
 * gettext - extract strings from gettext() calls in a C language
 *           source file and create a psffm:
 *           "Portable Source File Format for Messages"
 *           file template.
 */

int	write_args(), get_arguments(),  sort_message(),
	write_sorted_psffm();
char	*calloc(), *malloc(), *realloc();
char	*strcat(), *strcpy(), *strdup();

void	process(), print_usage(), init_lists(), add_comment_to_domain(),
	read_psffm();
char	*consume_whitespace(), *consume_symbol(), *consume_comment(),
	    *consume_quoted_string(), *consume_delim();
char	*get_nextline(), *write_comment(), *sort_comment();

FILE 	*defaultfp = NULL;
FILE	*currentfp = NULL;
FILE	*exclude_stream = NULL;

struct  list_head *get_list();
struct  list_head lists, *last_list;

char	*comment_cookie;
char	*message_string = "";
char	*default_domain = "messages";
char	*file_path = ".";
char	*currentf = "psffm";
char	*exclude_file;

int	readstd = 0;
int	textdomain_call = 0;
int	dgettext_call = 0;
int	gettext_call = 0;
int	verbose = 0;
int	exclude = 0;
int	number_lines = 0;
int	use_pp = 0;
int	use_msg_ids = 0;
int	extract_comments = 0;
int     linenum = 0;
int	sorted_output = 1;
int	all_strings = 0;
int	update = 0;
/*
int	overwrite = 0;
*/


char	cfilename [MAXPATHLEN+1];

char	linebuf[LINEBUF_SIZ];
char	default_filepath[MAXPATHLEN+1];

main(argc, argv, envp)
    int argc;
    char **argv, **envp;
{

    (void) get_arguments(&argc, &argv, envp);
    

    if (readstd) {
	strcpy(cfilename, "stdin");
	linenum = 0;
	process();
        readstd = 0;
    }

    while (argc > 0) {
	if (verbose)
	    fprintf(stderr, "scanning file # %d = %s\n", argc, *argv);

        if (freopen(*argv, "r", stdin) == NULL) {
            perror(*argv);
	    exit(2);
	}

	strcpy(cfilename, *argv);
	linenum = 0;
        process(*argv);

        argc--; argv++;
    }

    if (sorted_output || all_strings) {
	write_sorted_psffm();
    }

    exit(0);
}

void
print_usage()
{
    fprintf(stderr,
	"\nusage: xgettext [-adnuv] [-c<flag>] [-m<tag>] [-o<default>]\n");
    fprintf(stderr,
	"            ... [-p<path>] [-x<xlist>] - | file\n");
    fprintf(stderr,
	"       xgettext -h (HELP)\n");
}

print_help()
{
    fprintf(stderr, "\n");
    fprintf(stderr,
	"-a\t\tfind ALL strings\n");
    fprintf(stderr,
	"-c<flag>\tget comments beginning with <flag>\n");
    fprintf(stderr,
	"-d\t\tgenerate duplicates\n");
    fprintf(stderr,
	"-h\t\tHELP !!!\n");
    fprintf(stderr,
	"-m<tag>\t\tfill in msgstr with msgid<tag>\n");
    fprintf(stderr,
	"-n\t\tline# comments in output\n");
    fprintf(stderr,
	"-o<default>\tuse <default> for default file name\n");
    fprintf(stderr,
	"-p<path>\tuse <path> for file path\n");
    fprintf(stderr,
	"-u\t\tgenerate list of updated messages in <domain>.new\n");
    fprintf(stderr,
	"-v\t\tverbose mode on, lists xgettext's actions\n");
/*
    fprintf(stderr,
	"-w\t\toverwrite existing \".po\" files\n");
*/
    fprintf(stderr,
	"-x<xlist>\texclude strings in file <xlist> from output\n");
    fprintf(stderr,
	"-\t\tread stdin, use as a filter (input only)\n");
}

void
init_lists() {
    lists.domain = strdup(default_domain);
    last_list = &lists;
    read_psffm(default_domain);
}

char *
build_path(domain, pathbuf)
    char *domain, pathbuf[];
{
    strcpy(pathbuf, file_path);
    strcat(pathbuf, "/");
    strcat(pathbuf, domain);
    strcat(pathbuf, ".po");
    return (pathbuf);
}

int
get_arguments(argc, argv, envp)
    int *argc;
    char ***argv, **envp;
{
/*
 * Process input arguments of the form:
 *		
 *	-a	emit ALL strings
 *	-c	output comments beginning with <tag> <filename>
 *	-d	dups mode
 *	-h	HELP !!!
 *	-m	output message string using message id
 *	-n	insert line and file # comments in output
 *	-o 	specify default file name
 *	-p	file path
 *	-P	use preprocessor directives
 *	-u	create update list, new messages only
 *	-v	verbose mode on, outputs info on symbols encountered, etc.
 *	-w	overwrite existing .po files
 *	-x	exclude strings in exclude list <file>
 * 	 -	read stdin, aka use as an input only filter
 *	
*/
    (*argc)--, (*argv)++;
    while (*argc > 0 && *argv[0][0] == '-') {
	register char *cp = &(*(*argv)++)[1];
        (*argc)--;
	
	switch (*cp) {

	case NULL:
            readstd++;
            break;

	case 'a':
	    all_strings = 1;
	    if (verbose)
		printf("placing all strings in .po file\n");
	    break;

	case 'c':
	    extract_comments = 1;
	    if (*argc < 1) {
		fprintf(stderr,
		  "NULL comment flag, try -c<flag>, exiting ...\n");
		exit (1);
	    }

	    if (*(++cp) != NULL) {
	    	comment_cookie = strdup(cp);
	    } else {
		comment_cookie = strdup(**argv);
		(*argv)++; (*argc)--;
	    }
	    if (verbose)
		printf("Placing comments in \".po\" files ...\n");
	    break;

	case 'd':
	    sorted_output = 0;
	    if (verbose)
		printf("Writing ALL msgid's to \".po\" files\n");
	    break;

	case 'm':
	    use_msg_ids = 1;
	    if (*argc < 1) {
		fprintf(stderr, 
		  "NULL message tag, try -m<tag>, exiting ...\n");
		exit (1);
	    }
	    if (*(++cp) != NULL) {
	    	message_string = strdup(cp);
	    } else {
		message_string = strdup(**argv);
		(*argv)++; (*argc)--;
	    }
	    if (verbose)
		printf("Using \"<msgid> %s\" for msgstr\n", message_string);
	    break;


	case 'n':
	    number_lines = 1;
	    if (verbose)
		printf("Numbering lines in \"psffm\" files ...\n");
	    break;

	case 'o':
	    if (*argc < 1) {
		fprintf(stderr, 
		  "NULL output file, try -o<file>, exiting ...\n");
		exit (1);
	    }

	    if (*(++cp) != NULL) {
	    	default_domain = strdup(cp);
	    } else {
		default_domain = strdup(**argv);
		(*argv)++; (*argc)--;
	    }
	    break;

	case'P':
	    use_pp = 1;
	    break;

	case 'p':
	    if (*argc < 1) {
		fprintf(stderr, 
		  "NULL path, try -p<path>, exiting ...\n");
		exit (1);
	    }

	    if (*(++cp) != NULL) {
	    	file_path = strdup(cp);
	    } else {
		file_path = strdup(**argv);
		(*argv)++; (*argc)--;
	    }

	    break;

	case 'u':
	    update = 1;
	    break;

	case 'v':
	    verbose = 1;
	    fprintf(stderr, "VERBOSE ON\n");
	    break;

/*
	case 'w':
	    overwrite = 1;
	    break;
*/

	case 'x':
	    exclude = 1;
	    if (*argc < 1) {
		fprintf(stderr, 
		  "NULL exclude file, try -x<xfile>, exiting ...\n");
		exit (1);
	    }
	    if (*(++cp) != NULL) {
	    	exclude_file = strdup(cp);
	    } else {
		exclude_file = strdup(**argv);
		(*argv)++; (*argc)--;
	    }

            if ((exclude_stream = fopen(exclude_file, "r")) == NULL) {
		fprintf(stderr, "Exclude list file: %s not found,",
		    exclude_file);
		fprintf(stderr, " -x option reset, continuing ...\n");
		exclude = 0;
	    }

	    if (verbose)
		printf("Using \"<msgid>%s\" for msgstr\n", message_string);

	    break;

	default:
    	    fprintf(stderr, "Unrecognized option: %c\n", *cp);
	    print_usage();
	    break;

	case 'h':
	    print_usage();
	    print_help();
	    exit(0);
	}
	
    }

    if (*argc == 0 && !readstd )  {
	fprintf(stderr,
	    "No input files specified, exiting ...\n");
	print_usage();
	exit (1);
    }
	
    if (sorted_output) {
	init_lists();
    }

    build_path(default_domain, default_filepath);
    return (1);
}

void
process()
{
    char *cp;
    unsigned char c;
    int ret;
    int delim = 1;

    /*
     * Outer loop scans for the beginning of label, beginning of an
     * argument list, and the beginning of comments, and calls the
     * appropriate function to process them. It ignores all lines
     * where linebuf [0] == '#'.
     * 
     * Labels are detected by looking for an appropriate delimiter,
     * then calling is_symbol() to verify the label. If valid, the type of
     * call is noted in a global variable, then the loop scans
     * for the beginning of the argument list, calling write_args()
     * if a quoted argument is found.
     *
     * Comments are detected by looking for comment delimiters. If a
     * comment is found, write_comment() is called.
     */

    while (1) {
	
	if (!(cp = get_nextline())) {
	    return;
	}

	while (c = *cp++)
	switch (c) {
	    case 't':
		if (!delim) {
		    break;
		}
		delim = 0;
		if (is_symbol(&cp, "extdomain")) {
		    if (!(cp = consume_whitespace(cp))) {
			goto out;
		    }
		    if (textdomain_call = (*cp == '(')) {
			if (!(cp = consume_whitespace(++cp))) {
			    goto out;
			}
			if (verbose)
			    printf("textdomain call, arg = %s\n", cp);
		    }
		}

		break;

	    case 'd':
		if (!delim) {
		    break;
		}
		delim = 0;
		if (is_symbol(&cp, "gettext")) {
		    if(!(cp = consume_whitespace(cp))) {
			goto out;
		    }
		    if (dgettext_call = (*cp == '(')) {
			if (!(cp = consume_whitespace(++cp))) {
			    goto out;
			}
			if (verbose)
			    printf("dgettext call, arg = %s\n", cp);
		    }
		}
		break;

	    case 'g': 
		if (!delim) {
		    break;
		}
		delim = 0;
		if (is_symbol(&cp, "ettext")) {
		    if (!(cp = consume_whitespace(cp))) {
			goto out;
		    }
		    if (gettext_call = (*cp == '(')) {
			if (!(cp = consume_whitespace(++cp))) {
			    goto out;
			}
			if (verbose)
			    printf("gettext call, arg = %s\n", cp);
		    }
		}
		break;

	   case '"':
		delim = 0;

		if (cp[-2] == '\\') {
 		    break;
		}
		if(cp[-2] == '\'') {
		    break;
		}

		if (!(gettext_call || dgettext_call || textdomain_call
			|| all_strings)) {
		    if (!(cp = consume_quoted_string(cp))) {
			goto out;
		    }
		    break;
		}

		if ((ret = (int) write_args(&cp)) == -1) {
		    goto out;
		}

		textdomain_call = 0;
		dgettext_call = 0;
		gettext_call = 0;
		break;

	    case '/':
		delim = 1;
		if (*cp != '*') {
		    break;
		}
		if (extract_comments) {
		    cp++;
		    if (sorted_output) {
			cp = sort_comment(cp);
		    } else {
			cp = write_comment(cp);
		    }
		} else {
		    cp = consume_comment(cp);
		}
		if (!cp)
		    goto out;
		break;


            default:
		textdomain_call = 0;
		dgettext_call = 0;
		gettext_call = 0;
		delim = !(SYMBOL_CHAR(c));
		break;

        }	/* switch (c) */
	 
    }	    /* for (cp = ... */

out:

    if (ferror(stdout)) {
	printf("Oops\n");
        perror(cfilename);
	exit(0);
    }
}

char *
consume_comment(buf)
    char *buf;
{
    register char *bufptr = buf;
    register char c;

    while (c = *bufptr++) {
	switch (c) {
            case '*':
                if (*bufptr != '/') {
                    break;
                }
                return (bufptr++);
 
            case '\n':
		if (!(bufptr = get_nextline())) {
		    fprintf(stderr, 
			"==>ERROR, end of comment never reached\n");
		    return (0);
		}
		break;

            default:
                break;
	}
    }
}


char *
consume_symbol(buf)
    char *buf;
{
    register char *bufptr = buf;
    register char c;

    while (c = *bufptr) {
	if (SYMBOL_CHAR(c)) {
	    bufptr++;
	    continue;
	}
	break;
    }    
    return (bufptr);
}

char *
consume_quoted_string(buf)
    char *buf;
{
    register char *bufptr = buf;
    register char c;

    while (c = *bufptr++) {
        switch (c) {

	case '\n':
	    fprintf(stderr,
		"\"%s\", line %d: newline in string or char constant\n",
		    cfilename, linenum);
	    if (!(bufptr = get_nextline())) {
		fprintf(stderr, 
		"\"%s\", line %d: end of string never reached\n",
		    cfilename, linenum);
		return (0);
	    } 

	case '"':
	    if (!(bufptr = consume_whitespace(bufptr)))
		return (0);
	    if (bufptr[0] != '"')
		return (bufptr);
	    bufptr++;

	case '\\':
            if (*bufptr == '\n') {
		if (!(bufptr = get_nextline())) {
		    fprintf(stderr, 
			"\"%s\", line %d: end of string never reached\n",
			cfilename, linenum);
		    return (0);
		}
	    } else {
		bufptr++;
	    }
            break;

	default:
	    break;
	}
    }
/* 
 * SHOULDN"T REACH HERE
*/
    fprintf(stderr, "ERROR ==> end of quoted string never reached\n");
    return (--bufptr);
}

char *
consume_blankspace(buf)
    char *buf;
{
    register char *bufptr = buf;
    register char c;

    while (c = *bufptr) {
        switch (c) {
        case '\t' :
        case ' ':
	    bufptr++;
	    break;

	case '\\':
	    if (bufptr[1] != '\n')
		return (bufptr);

        default:
            return (bufptr);
        }
    }    
    return (0);
}

char *
consume_whitespace(buf)
    char *buf;
{
    register char *bufptr = buf;
    register char c;

    while (c = *bufptr) {
        switch (c) {
        case '\t' :
        case ' ':
	    bufptr++;
	    break;

	case '\\':
	    if (bufptr[1] != '\n')
		return (bufptr);
	case '\n':
	    if (!(bufptr = get_nextline()))
		return (0);
	    break;

        default:
            return (bufptr);
        }
    }    
    return (0);
}

char *
consume_delim(buf)
    char *buf;
{
    register char *bufptr = buf;
    register char c;

    while (c = *bufptr) {
	if (c == '\n') {
	    if (!(bufptr = get_nextline()))
		return (0);
        } else if (SYMBOL_CHAR(c)) {
	    return (bufptr);
	} else {
	    bufptr++;
	}
    }    
    return (0);
}

int
is_symbol(lptr, label_id)
    char **lptr, *label_id;
{
    int len = 0;
    if (strncmp(label_id, *lptr, len = strlen(label_id))) {
	*lptr = consume_symbol(*lptr);
	return (0);
    }

    *lptr += len;

    switch (**lptr) {
	case ' ':
	case '\t':
	case '\(':
	case '\n':
	    return (1);

	default:
	    *lptr = consume_symbol(*lptr);
	    return (0);
    }
}


/*
 *	FIX ME !!!!!!!!
*/
int
exclude_string(test_string)
    char *test_string;
{
        char file_string[BUFSIZ];
        fseek(exclude_stream, 0, 0); /* start from begining */
        for (;;) {
            if (fgets(file_string, sizeof file_string, exclude_stream) == NULL) {
                break; /* reached EOF */
            }
            file_string[strlen(file_string)-1] = '\0';
            if (!strcmp(test_string, file_string)) {
                return (1);
            }
        }
 
        return (0);
}

char *
get_nextline()
{

    char *bptr;
    do {
	if (fgets(linebuf, sizeof (linebuf), stdin) == NULL) {
	    if (ferror(stdin)) {
		perror("xgettext");
		exit(2);
	    }
	    return (0);
	}
	linenum ++;
	if (use_pp) {
	    break;
	}
	if (readstd && linebuf[0] == '#') {
	    sscanf(linebuf, "#%d%s", &linenum, cfilename);
	    linenum--;
	    if (verbose) {
		printf("file: %s seen at stdin, linenum: %d\n",
		    cfilename, linenum);
	    }
	}
    } while (linebuf[0] == '#');

    return (linebuf);
}

char *
extract_comment(cpp)
    char **cpp;
{

    register char c, *dp, *cp = *cpp;
    static char *commentbuf = NULL; 
    static bufsiz = 0; 
    int offset;

    if (!commentbuf) {
    	commentbuf = malloc(COMMENTBUF_SIZ);
	bufsiz = COMMENTBUF_SIZ;
    }

    memset(commentbuf, 0, bufsiz);
    commentbuf[0] = '#';
    dp = commentbuf + 1;
    offset = 1;
    
    while (c = *cp++) {
 	if (offset >= (bufsiz-3)) {
	    commentbuf = realloc(commentbuf, bufsiz += COMMENTBUF_SIZ);
	    dp = commentbuf + offset;
	}
	
	switch(c) {

	    case '\n':
		*dp++ = c;
		offset++;
		*dp++ = '#';
		offset++;
                if (!(cp = get_nextline())) {
		    fprintf(stderr,
			"==> ERROR, end of comment never reached\n");
		    return (0);
		}
		break;
		
	    case '*': 
		if (*cp == '/') {
		    *dp = '\n';
		    return (commentbuf);
		}
		break;

	    default:
		*dp++ = c;
		offset++;
		break;
	}
    }
    /*
     * ERROR, SHOULDN'T EVER GET HERE ...
     */
	
    fprintf(stderr, "ERROR, end of comment never reached\n");
    return (cp);
}

char *
sort_comment(cp)
    char *cp;
{

    char isitme[BUFSIZ];
    char domain[MAXNAMLEN+1]; 	/* MAXNAMLEN from sys/dir.h	*/
    char *comment;

    (void) sscanf(cp, "%s%s", isitme, domain);
    if (strcmp(isitme, comment_cookie)) {
	return (consume_comment(cp));
    }
    if (!(comment = extract_comment(&cp))) {
	return (0);
    }
    add_comment_to_domain(domain, comment);
    return (cp);
}

void
add_comment_to_domain(domain, commentbuf)
	char *domain, *commentbuf;
{
    struct list_head *head;
    if (!strcmp(domain, "-")) {
	head = get_list(default_domain);
    } else {
	head = get_list(domain);
    }

    if (!head->comments) {
	head->comments = head->last_comment =
		(struct comment_str *) calloc(1, sizeof(struct comment_str));
	if (!head->comments) {
	    perror("malloc error");
	    exit(3);
	}
    } else {
	head->last_comment = head->last_comment->next =
		(struct comment_str *) calloc(1, sizeof(struct comment_str));
	if (!head->last_comment) {
	    perror("malloc error");
	    exit(3);
	}
    }
    head->last_comment->str = strdup(commentbuf);
}

char *
write_comment (cp)
    char *cp;
{

    char isitme[BUFSIZ];
    char domain[MAXNAMLEN+1]; 	/* MAXNAMLEN from sys/dir.h	*/
    char comment_path[MAXPATHLEN+1];	/* MAXPATHLEN from sys/param.h	*/
    FILE *commentfp;
    char *comment;


    (void) sscanf(cp, "%s%s", isitme, domain);
    if (strcmp(isitme, comment_cookie)) {
	return (consume_comment(cp));
    }

    if (!defaultfp) {
        if ((defaultfp = fopen(default_filepath, "a+")) == NULL) {
            perror("xgettext");
            fprintf(stderr,
		"FATAL ERROR, can't open output file: %s\n", default_filepath);
            exit(2);
        }
    }

    if (strcmp(domain, "-")) {
	memset(comment_path, 0, sizeof(comment_path));
	build_path(domain, comment_path);
	if ((commentfp = fopen(comment_path, "a+")) == NULL) {
	    perror("xgettext");
            fprintf(stderr,
                "FATAL ERROR, can't open comment file: %s\n", commentfp);
            exit(2);
        }  
    } else {
	strcpy(domain, default_domain);
	commentfp = defaultfp;
    }

    comment = extract_comment(&cp);
    fprintf(commentfp, "%s\n", comment);
    close (commentfp);
    return (cp);
}

int
write_args(cpp)
    char **cpp;
{
    register char *cp = *cpp;
    register int c;
    register char *dp;

    char msgkey[MAX_MSGID_LEN+1];
    char domain[MAX_MSGSTR_LEN+1];

    int secondarg = 0;

    memset(msgkey, 0, sizeof(msgkey));
    memset(domain, 0, sizeof(domain));

    if (gettext_call || all_strings) {
	dp = msgkey;
    } else {
	dp = domain;
    }

    while (c = *cp++) {

	switch (c) {

	    case '"':
		if (!(cp = consume_whitespace(cp))) {
		    return (0);
		}
		if (cp[0] == '"') {
		    cp++;
		    continue; 
		}

		if (dgettext_call && !secondarg) {
		    if (cp[0] == ',') {
			if(!(cp = consume_whitespace(++cp)))
			    return (0);
			if (cp[0] == '"') {
			    secondarg++;
			    dp = msgkey;
			    cp++;
			    continue;
			} else {
			    *cpp = cp;
			    return (1);
			}
		    } else {
			fprintf(stderr, "Syntax error\n");
			*cpp = cp;
			return (1);
		    }
		}
		cp++;
		goto out;

	    case '\\':
		if (*cp == '\n') {
		    if (!(cp = get_nextline())) {
			fprintf(stderr, 
			"==>ERROR, end of string never reached\n");
			return (0);
		    }
		    break;
		}
		*dp++ = c;
		c = *cp++;

	    default:
		*dp++ = c;
		break;
	}
nextarg:
	continue;
    }

out:

    *cpp = --cp;

    if (exclude && exclude_string(msgkey)) {
	return (1);
    }

    if (sorted_output) {
	if (textdomain_call) {
	    char linebuf[MAXPATHLEN+100];
	    sprintf(linebuf,
		"#\n# file: %s, line number: %d, textdomain(\"%s\")\n",
		cfilename, linenum, domain);
	    add_comment_to_domain(domain, linebuf);
	    return (1);
	} else {
	    return (sort_message(domain, msgkey, "", 1));
	}
    }

    if (all_strings) {
	return (write_psffm(msgkey, NULL));
    } else if (gettext_call) {
	return (write_psffm(msgkey, NULL));
    } else if (dgettext_call) {
	return (write_psffm(msgkey, domain));
    } else if (textdomain_call) {
	return (write_psffm(NULL, domain));
    }
}

int
write_psffm(msgkey, domain)
char *msgkey, *domain;
{
    static char saved_domain[MAXNAMLEN+1];
    static char domain_path[MAXPATHLEN+1];
    struct stat psffm_stat;

    FILE *psffmfp;

    if (!defaultfp) {
	if ((defaultfp = fopen(default_filepath, "a+")) == NULL) {
	    perror("xgettext");
	    fprintf(stderr,
		"FATAL ERROR, can't open output file: %s\n",
		    default_filepath);
	    exit(2);
	}
    }


    if (domain && dgettext_call) {
	if (strcmp(saved_domain, domain)) {

	    if (verbose) {
		printf("entering new domain, ");
		printf("old domain: %s  new domain: %s\n",
		    saved_domain, domain);
	    }

	    memset(saved_domain, 0, sizeof(saved_domain));
	    strcpy(saved_domain, domain);

	    memset(domain_path, 0, sizeof(domain_path));
	    build_path(domain, domain_path);

	    if (currentfp) {
		fclose(currentfp);
	    }
	    
	    if ((currentfp = fopen(domain_path, "a+")) == NULL) {
		perror("xgettext");
		fprintf(stderr, "FATAL ERROR, can't open output file: %s\n",
		    domain_path);
		exit(2);
	    }

	    if (stat(domain_path, &psffm_stat) == -1) {
		perror("xgettext");
		fprintf(stderr, "FATAL ERROR, can't stat output file: %s\n",
		    domain_path);
		exit(2);
	    }

	    if (psffm_stat.st_size < sizeof("domain")) {
		if (verbose) 
		    printf("adding domain token: %s to file: %s\n",
			domain, domain_path);
		fprintf(currentfp, "domain ");
		fprintf(currentfp, "\"%s\"\n", domain);
		
		/*
		 * Shouldn't happen ...
		 */
		if (msgkey == NULL) {
		    return (ferror(currentfp));
		}
	    }

	}
	psffmfp = currentfp;
    } else {
	psffmfp = defaultfp;
    }

    fseek(psffmfp, 0, 2);

    if (textdomain_call) {
	fprintf(psffmfp, "#\n# %s: textdomain(), line number: %d, domain: %s\n",
			cfilename, linenum, domain);
    }

    if (msgkey) {
	if (verbose) {
	    printf("writing message key: %s to file: %s\n",
		msgkey, domain_path);
	    if (use_msg_ids)
	    	printf("using message id as messsage string\n");
	}

	if (number_lines) {
		fprintf(psffmfp, "#\n# %s: line number: %d\n",
			cfilename, linenum);
	}
	fprintf(psffmfp, "msgid ");
	fprintf(psffmfp, "\"%s\"\n", msgkey);
	fprintf(psffmfp, "msgstr ");
	if (use_msg_ids) {
	    fprintf(psffmfp, "\"%s%s\"\n", msgkey, message_string);
	} else {
	    fprintf(psffmfp, "\n");
	}
    }
    fflush(psffmfp);
    return (ferror(psffmfp));
}

struct list_head *
get_list(domain) 
    char *domain;
{

/*
 * Find sorted list for a given domain
 * make new list if domain not found
*/
 
    if (domain[0] != NULL) {
        register struct list_head *head = &lists;
	register int found;

	while (head) {
	    if (!strcmp(head->domain, domain)) {
		return (head);
	    }
	    head = head->next_list;
	}

	last_list = last_list->next_list =
	    (struct list_head *) calloc(1, sizeof(struct list_head));
	if (!last_list) {
	    perror("malloc error");
	    exit(3);
	}
	last_list->domain = strdup(domain);
	read_psffm(domain);
	return (last_list);
    } else {
	return (&lists);
    }
}

int
sort_message(domain, msgid, msgstr, new)
    char *domain, *msgid, *msgstr;
    short   new;
{
    register struct list_element *lp, *lplt, *lpgt;
    int             i, lessthan;
    struct list_head *head;

/*
 * Find correct list to add output to
*/
 
    head = get_list(domain);
    lp = head->first_element;
    lplt = lpgt = 0;
    while (lp) {
        i = strcmp(msgid, lp->msgid);
        if (i < 0) {
            lpgt = lp;
            break;
        } else if (i == 0) {           /* found! */
            goto add_comments;
        }
        lplt = lp;
        lp = lp->next;
    }

    if (!(lp = (struct list_element *)calloc(1, sizeof(struct list_element)))){
        perror("xgettext malloc error");
        exit(3);
    }
    lp->msgid = strdup(msgid);
    lp->msgstr = strdup(msgstr);
    lp->new = new;

    if (lplt == 0) {
	head->first_element = lp;
	lp->next = lpgt;
    } else if (lpgt == 0) {
	lplt->next = lp;
    } else {
	lp->next = lpgt;
	lplt->next = lp;
    }

add_comments:

    if (head->comments) {
	if (!lp->comments) {
	    lp->comments = head->comments;
	} else {
	    lp->last_comment->next = head->comments;
	}
	lp->last_comment = head->last_comment;
	head->comments = head->last_comment = 0;
    }

    if (number_lines) {
	char linebuf[MAXPATHLEN+100];
	sprintf(linebuf, "#\n# file: %s, line number: %d\n",
			cfilename, linenum);
	if (!lp->comments) {
	    lp->comments = lp->last_comment =
		(struct comment_str *) calloc(1, sizeof(struct comment_str));
	    if (!lp->comments) {
		perror("malloc error");
		exit(3);
	    }
	} else {
	    lp->last_comment = lp->last_comment->next =
		(struct comment_str *) calloc(1, sizeof(struct comment_str));
	    if (!lp->last_comment) {
		perror("malloc error");
		exit(3);
	    }
	}
	lp->last_comment->str = strdup(linebuf);
    }

    return (0);
}

int
write_sorted_psffm()
{
    FILE			*fp;
    register struct list_head	*list;
    struct list_element		*lp;
    register struct comment_str *cp;
    char			output_filename[MAXPATHLEN];

    list = &lists;
    while (list) {
	strcpy(output_filename, file_path);
	strcat(output_filename, "/");
	strcat(output_filename, list->domain);
	if (update) {
	    strcat(output_filename, ".update");
	} else {
	    strcat(output_filename, ".po");
	}
	fp = fopen(output_filename, "w");
	fprintf(fp, "domain ");
	fprintf(fp, "\"%s\"\n", list->domain);
	
	if (fp == NULL) {
	    perror(default_filepath);
	    exit(2);
	}
	
	lp = list->first_element;
	while (lp) {
	    if (update && !lp->new) {
		lp = lp->next;
		continue;
	    }
	    cp = lp->comments;
	    while (cp) {
		fprintf(fp, "%s", cp->str);
		cp = cp->next;
	    }
	    fprintf(fp, "msgid \"%s\"\n", lp->msgid);
	    if (use_msg_ids) {
		fprintf(fp, "msgstr \"%s%s\"\n",
		    lp->msgid, message_string);
	    } else {
		if (lp->msgstr[0] == '\0') {
		    fprintf(fp, "msgstr\n");
		} else {
		    fprintf(fp, "msgstr \"%s\"\n",
			lp->msgstr);
		}
	    }
	    lp = lp->next;
	}

	cp = list->comments;
	while (cp) {
	    fprintf(fp, "%s", cp->str);
	    cp = cp->next;
	}

	(void) fclose(fp);
	list = list->next_list;
    }
}

void
read_psffm(domain)
    char *domain;
{
    FILE *msgfileptr = NULL;

    char inbuf[MAX_VALUE_LEN+32];
    char *inbufptr;
    char msgfile[MAXPATHLEN];
    char current_domain[MAX_DOMAIN_LENGTH];
    char msgid[MAX_VALUE_LEN+1];
    char msgstr[MAX_VALUE_LEN+1];
    register char *bufptr;

    int inlinenum = 0;
    int	domain_set = 0;
    int whitespace = 0;
    int quotefound = 0;
    int ignored = 0;
    int indomain = 0;
    int inmsgid = 0;
    int inmsgstr = 0;
    char c;
    int number_lines_state = number_lines;
    number_lines = 0;

    build_path(domain, msgfile);
    if (!(msgfileptr = fopen(msgfile, "r"))) {
	number_lines = number_lines_state;
	return;
    }

    memset(msgid, 0, sizeof(msgid));
    memset(msgstr, 0, sizeof(msgstr));

    while (1) {
	memset(inbuf, 0, sizeof(inbuf));
        if (fgets(inbuf, sizeof(inbuf), msgfileptr) == NULL) {
            break;
        }
	inlinenum ++;

	if (verbose) {
	    printf("Scanning: %s, line number %d:\n%s\n",
		msgfile, inlinenum, inbuf);
	}

	inbufptr = inbuf;
	whitespace = 0;

        switch (*inbufptr) {
	    case '#': 	/*   comment    */
		if (inmsgstr) {
		    sort_message(domain, msgid, msgstr, 0);
		    memset(msgid, 0, sizeof(msgid));
		    memset(msgstr, 0, sizeof(msgstr));
		    inmsgid = 0;
		    inmsgstr = 0;
		    indomain = 0;
		}
		add_comment_to_domain(domain, inbufptr);
	    case '\n':
		ignored = 1;
		break;

	    case '\"':
	    case ' ':
	    case '\t':
		whitespace = 1;
	    default:
		ignored = 0;
		break;
	}

	if (ignored) {
	    continue;
	}

	/*
	 * Process MSGID Tokens, must not have just seen one unless
	 * this line begins with whitespace -or- "
	 */

	if ((!strncmp(MSGID_TOKEN, inbuf, sizeof(MSGID_TOKEN)-1))
	    || (whitespace && inmsgid)) {
	    if (inmsgid && !whitespace) {
		fprintf(stderr, "Consecutive MSGID tokens, file: %s", msgfile);
		fprintf(stderr, "line number: %d\n", inlinenum);
	    }
	    if (inmsgstr) {
		sort_message(domain, msgid, msgstr, 0);

		memset(msgid, 0, sizeof(msgid));
		memset(msgstr, 0, sizeof(msgstr));
	    }


	    if (inmsgid) {
	        inbufptr = consume_blankspace(inbuf);
	    } else {
	        inbufptr = consume_blankspace(inbuf +
		    sizeof(MSGID_TOKEN)-1);
		bufptr = msgid;
	    }

	    if (!inbufptr)
		return;
	    inmsgid = 1;
	    inmsgstr = 0;
	    indomain = 0;
	    goto load_buffer;
	}

	/*
	 * Process MSGSTR Tokens, must not have just seen one unless
	 * this line begins with whitespace -or- "
	 */

	if ((!strncmp(MSGSTR_TOKEN, inbuf, sizeof(MSGSTR_TOKEN)-1))
	    || (whitespace && inmsgstr)) {
	    if (inmsgstr && !whitespace) {
		fprintf(stderr, "Consecutive MSGSTR tokens, file: %s", msgfile);
		fprintf(stderr, "line number: %d\n", inlinenum);
	    }

	    if (inmsgstr) {
	        inbufptr = consume_blankspace(inbuf);
	    } else {
	        inbufptr = consume_blankspace(inbuf +
		    sizeof(MSGSTR_TOKEN)-1);
		bufptr = msgstr;
	    }

	    if (!inbufptr)
		return;
	    inmsgstr = 1;
	    inmsgid = 0;
	    indomain = 0;
	    goto load_buffer;
	}

	/*
	 * Process DOMAIN Tokens, add message id and message string to
	 * sorted list if msgstr was being processed, 
	*/

	if (!strncmp(DOMAIN_TOKEN, inbuf, sizeof(DOMAIN_TOKEN)-1)) {
	    if (inmsgstr) {
		sort_message(domain, msgid, msgstr, 0);
		memset(msgid, 0, sizeof(msgid));
		memset(msgstr, 0, sizeof(msgstr));
	    }

	    domain_set = 1;
	    indomain = 1;
	    inmsgid = 0;
	    inmsgstr = 0;
		
	    inbufptr = consume_blankspace(inbuf+ sizeof(DOMAIN_TOKEN));
	    memset(current_domain, 0, sizeof(current_domain));
	    bufptr = domain = current_domain;
	    if (!inbufptr)
		return;
	}

load_buffer:

	quotefound = 0;

	while (c=*inbufptr++) {
	    switch (c) {

	    case '\n':
	    case '"':
	        break;

	    default:
	        *bufptr++ = c;
	    }
	}

    }

    if (inmsgstr) {
	sort_message(domain, msgid, msgstr, 0);
    }
    number_lines = number_lines_state;
    return;
}
