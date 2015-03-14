
# line 27 "awk.y"
#ifdef DEBUG
#define YYDEBUG 12
#endif

#define	YYMAXDEPTH	300
#define	YYSSIZE	YYMAXDEPTH

#include "awk.h"

static void yyerror (); /* va_alist */
static char *get_src_buf P((void));
static int yylex P((void));
static NODE *node_common P((NODETYPE op));
static NODE *snode P((NODE *subn, NODETYPE op, int sindex));
static NODE *mkrangenode P((NODE *cpair));
static NODE *make_for_loop P((NODE *init, NODE *cond, NODE *incr));
static NODE *append_right P((NODE *list, NODE *new));
static void func_install P((NODE *params, NODE *def));
static void pop_var P((NODE *np, int freeit));
static void pop_params P((NODE *params));
static NODE *make_param P((char *name));
static NODE *mk_rexp P((NODE *exp));

static int want_assign;		/* lexical scanning kludge */
static int want_regexp;		/* lexical scanning kludge */
static int can_return;		/* lexical scanning kludge */
static int io_allowed = 1;	/* lexical scanning kludge */
static char *lexptr;		/* pointer to next char during parsing */
static char *lexend;
static char *lexptr_begin;	/* keep track of where we were for error msgs */
static char *lexeme;		/* beginning of lexeme for debugging */
static char *thisline = NULL;
#define YYDEBUG_LEXER_TEXT (lexeme)
static int param_counter;
static char *tokstart = NULL;
static char *token = NULL;
static char *tokend;

NODE *variables[HASHSIZE];

extern char *source;
extern int sourceline;
extern char *cmdline_src;
extern char **srcfiles;
extern int errcount;
extern NODE *begin_block;
extern NODE *end_block;

# line 76 "awk.y"
typedef union  {
	long lval;
	AWKNUM fval;
	NODE *nodeval;
	NODETYPE nodetypeval;
	char *sval;
	NODE *(*ptrval)();
} YYSTYPE;
# define FUNC_CALL 257
# define NAME 258
# define REGEXP 259
# define ERROR 260
# define YNUMBER 261
# define YSTRING 262
# define RELOP 263
# define APPEND_OP 264
# define ASSIGNOP 265
# define MATCHOP 266
# define NEWLINE 267
# define CONCAT_OP 268
# define LEX_BEGIN 269
# define LEX_END 270
# define LEX_IF 271
# define LEX_ELSE 272
# define LEX_RETURN 273
# define LEX_DELETE 274
# define LEX_WHILE 275
# define LEX_DO 276
# define LEX_FOR 277
# define LEX_BREAK 278
# define LEX_CONTINUE 279
# define LEX_PRINT 280
# define LEX_PRINTF 281
# define LEX_NEXT 282
# define LEX_EXIT 283
# define LEX_FUNCTION 284
# define LEX_GETLINE 285
# define LEX_IN 286
# define LEX_AND 287
# define LEX_OR 288
# define INCREMENT 289
# define DECREMENT 290
# define LEX_BUILTIN 291
# define LEX_LENGTH 292
# define UNARY 293
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern int yyerrflag;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
YYSTYPE yylval, yyval;
# define YYERRCODE 256

# line 751 "awk.y"


struct token {
	char *operator;		/* text to match */
	NODETYPE value;		/* node type */
	int class;		/* lexical class */
	unsigned flags;		/* # of args. allowed and compatability */
#	define	ARGS	0xFF	/* 0, 1, 2, 3 args allowed (any combination */
#	define	A(n)	(1<<(n))
#	define	VERSION	0xFF00	/* old awk is zero */
#	define	NOT_OLD		0x0100	/* feature not in old awk */
#	define	NOT_POSIX	0x0200	/* feature not in POSIX */
#	define	GAWKX		0x0400	/* gawk extension */
	NODE *(*ptr) ();	/* function that implements this keyword */
};

extern NODE
	*do_exp(),	*do_getline(),	*do_index(),	*do_length(),
	*do_sqrt(),	*do_log(),	*do_sprintf(),	*do_substr(),
	*do_split(),	*do_system(),	*do_int(),	*do_close(),
	*do_atan2(),	*do_sin(),	*do_cos(),	*do_rand(),
	*do_srand(),	*do_match(),	*do_tolower(),	*do_toupper(),
	*do_sub(),	*do_gsub(),	*do_strftime(),	*do_systime();

/* Tokentab is sorted ascii ascending order, so it can be binary searched. */

static struct token tokentab[] = {
{"BEGIN",	Node_illegal,	 LEX_BEGIN,	0,		0},
{"END",		Node_illegal,	 LEX_END,	0,		0},
{"atan2",	Node_builtin,	 LEX_BUILTIN,	NOT_OLD|A(2),	do_atan2},
{"break",	Node_K_break,	 LEX_BREAK,	0,		0},
{"close",	Node_builtin,	 LEX_BUILTIN,	NOT_OLD|A(1),	do_close},
{"continue",	Node_K_continue, LEX_CONTINUE,	0,		0},
{"cos",		Node_builtin,	 LEX_BUILTIN,	NOT_OLD|A(1),	do_cos},
{"delete",	Node_K_delete,	 LEX_DELETE,	NOT_OLD,	0},
{"do",		Node_K_do,	 LEX_DO,	NOT_OLD,	0},
{"else",	Node_illegal,	 LEX_ELSE,	0,		0},
{"exit",	Node_K_exit,	 LEX_EXIT,	0,		0},
{"exp",		Node_builtin,	 LEX_BUILTIN,	A(1),		do_exp},
{"for",		Node_K_for,	 LEX_FOR,	0,		0},
{"func",	Node_K_function, LEX_FUNCTION,	NOT_POSIX|NOT_OLD,	0},
{"function",	Node_K_function, LEX_FUNCTION,	NOT_OLD,	0},
{"getline",	Node_K_getline,	 LEX_GETLINE,	NOT_OLD,	0},
{"gsub",	Node_builtin,	 LEX_BUILTIN,	NOT_OLD|A(2)|A(3), do_gsub},
{"if",		Node_K_if,	 LEX_IF,	0,		0},
{"in",		Node_illegal,	 LEX_IN,	0,		0},
{"index",	Node_builtin,	 LEX_BUILTIN,	A(2),		do_index},
{"int",		Node_builtin,	 LEX_BUILTIN,	A(1),		do_int},
{"length",	Node_builtin,	 LEX_LENGTH,	A(0)|A(1),	do_length},
{"log",		Node_builtin,	 LEX_BUILTIN,	A(1),		do_log},
{"match",	Node_builtin,	 LEX_BUILTIN,	NOT_OLD|A(2),	do_match},
{"next",	Node_K_next,	 LEX_NEXT,	0,		0},
{"print",	Node_K_print,	 LEX_PRINT,	0,		0},
{"printf",	Node_K_printf,	 LEX_PRINTF,	0,		0},
{"rand",	Node_builtin,	 LEX_BUILTIN,	NOT_OLD|A(0),	do_rand},
{"return",	Node_K_return,	 LEX_RETURN,	NOT_OLD,	0},
{"sin",		Node_builtin,	 LEX_BUILTIN,	NOT_OLD|A(1),	do_sin},
{"split",	Node_builtin,	 LEX_BUILTIN,	A(2)|A(3),	do_split},
{"sprintf",	Node_builtin,	 LEX_BUILTIN,	0,		do_sprintf},
{"sqrt",	Node_builtin,	 LEX_BUILTIN,	A(1),		do_sqrt},
{"srand",	Node_builtin,	 LEX_BUILTIN,	NOT_OLD|A(0)|A(1), do_srand},
{"strftime",	Node_builtin,	 LEX_BUILTIN,	GAWKX|A(1)|A(2), do_strftime},
{"sub",		Node_builtin,	 LEX_BUILTIN,	NOT_OLD|A(2)|A(3), do_sub},
{"substr",	Node_builtin,	 LEX_BUILTIN,	A(2)|A(3),	do_substr},
{"system",	Node_builtin,	 LEX_BUILTIN,	NOT_OLD|A(1),	do_system},
{"systime",	Node_builtin,	 LEX_BUILTIN,	GAWKX|A(0),	do_systime},
{"tolower",	Node_builtin,	 LEX_BUILTIN,	NOT_OLD|A(1),	do_tolower},
{"toupper",	Node_builtin,	 LEX_BUILTIN,	NOT_OLD|A(1),	do_toupper},
{"while",	Node_K_while,	 LEX_WHILE,	0,		0},
};

/* VARARGS0 */
static void
yyerror(va_alist)
va_dcl
{
	va_list args;
	char *mesg;
	register char *bp, *cp;
	char *scan;
	char buf[120];

	errcount++;
	/* Find the current line in the input file */
	if (lexptr) {
		if (!thisline) {
			for (cp=lexeme; cp != lexptr_begin && *cp != '\n'; --cp)
				;
			if (*cp == '\n')
				cp++;
			thisline = cp;
		}
		/* NL isn't guaranteed */
		bp = lexeme;
		while (bp < lexend && *bp && *bp != '\n')
			bp++;
	} else {
		thisline = "(END OF FILE)";
		bp = thisline + 13;
	}
	msg("%.*s", (int) (bp - thisline), thisline);
	bp = buf;
	cp = buf + sizeof(buf) - 24;	/* 24 more than longest msg. input */
	if (lexptr) {
		scan = thisline;
		while (bp < cp && scan < lexeme)
			if (*scan++ == '\t')
				*bp++ = '\t';
			else
				*bp++ = ' ';
		*bp++ = '^';
		*bp++ = ' ';
	}
	va_start(args);
	mesg = va_arg(args, char *);
	strcpy(bp, mesg);
	err("", buf, args);
	va_end(args);
	exit(2);
}

static char *
get_src_buf()
{
	static int samefile = 0;
	static int nextfile = 0;
	static char *buf = NULL;
	static int fd;
	int n;
	register char *scan;
	static int len = 0;
	static int did_newline = 0;
#	define	SLOP	128	/* enough space to hold most source lines */

	if (cmdline_src) {
		if (len == 0) {
			len = strlen(cmdline_src);
			if (len == 0)
				cmdline_src = NULL;
			sourceline = 1;
			lexptr = lexptr_begin = cmdline_src;
			lexend = lexptr + len;
		} else if (!did_newline && *(lexptr-1) != '\n') {
			/*
			 * The following goop is to ensure that the source
			 * ends with a newline and that the entire current
			 * line is available for error messages.
			 */
			int offset;

			did_newline = 1;
			offset = lexptr - lexeme;
			for (scan = lexeme; scan > lexptr_begin; scan--)
				if (*scan == '\n') {
					scan++;
					break;
				}
			len = lexptr - scan;
			emalloc(buf, char *, len+1, "get_src_buf");
			memcpy(buf, scan, len);
			thisline = buf;
			lexptr = buf + len;
			*lexptr = '\n';
			lexeme = lexptr - offset;
			lexptr_begin = buf;
			lexend = lexptr + 1;
		} else
			lexeme = lexptr = lexptr_begin = NULL;
		return lexptr;
	}
	if (!samefile) {
		source = srcfiles[nextfile];
		if (source == NULL) {
			if (buf)
				free(buf);
			return lexeme = lexptr = lexptr_begin = NULL;
		}
		fd = pathopen(source);
		if (fd == -1)
			fatal("can't open source file \"%s\" for reading (%s)",
				source, strerror(errno));
		len = optimal_bufsize(fd);
		if (buf)
			free(buf);
		emalloc(buf, char *, len + SLOP, "get_src_buf");
		lexptr_begin = buf + SLOP;
		samefile = 1;
		sourceline = 1;
	} else {
		/*
		 * Here, we retain the current source line (up to length SLOP)
		 * in the beginning of the buffer that was overallocated above
		 */
		int offset;
		int linelen;

		offset = lexptr - lexeme;
		for (scan = lexeme; scan > lexptr_begin; scan--)
			if (*scan == '\n') {
				scan++;
				break;
			}
		linelen = lexptr - scan;
		if (linelen > SLOP)
			len = SLOP;
		thisline = buf + SLOP - linelen;
		memcpy(thisline, scan, linelen);
		lexeme = buf + SLOP - offset;
		lexptr_begin = thisline;
	}
	n = read(fd, buf + SLOP, len);
	if (n == -1)
		fatal("can't read sourcefile \"%s\" (%s)",
			source, strerror(errno));
	if (n == 0) {
		samefile = 0;
		nextfile++;
		return get_src_buf();
	}
	lexptr = buf + SLOP;
	lexend = lexptr + n;
	return buf;
}

#define	tokadd(x) (*token++ = (x), token == tokend ? tokexpand() : token)

char *
tokexpand()
{
	static int toksize = 60;
	int tokoffset;

	tokoffset = token - tokstart;
	toksize *= 2;
	if (tokstart)
		erealloc(tokstart, char *, toksize, "tokexpand");
	else
		emalloc(tokstart, char *, toksize, "tokexpand");
	tokend = tokstart + toksize;
	token = tokstart + tokoffset;
	return token;
}

#ifdef DEBUG
char
nextc() {
	if (lexptr && lexptr < lexend)
		return *lexptr++;
	if (get_src_buf())
		return *lexptr++;
	return '\0';
}
#else
#define	nextc()	((lexptr && lexptr < lexend) ? \
			*lexptr++ : \
			(get_src_buf() ? *lexptr++ : '\0') \
		)
#endif
#define pushback() (lexptr && lexptr > lexptr_begin ? lexptr-- : lexptr)

/*
 * Read the input and turn it into tokens.
 */

static int
yylex()
{
	register int c;
	int seen_e = 0;		/* These are for numbers */
	int seen_point = 0;
	int esc_seen;		/* for literal strings */
	int low, mid, high;
	static int did_newline = 0;
	char *tokkey;

	if (!nextc())
		return 0;
	pushback();
	lexeme = lexptr;
	thisline = NULL;
	if (want_regexp) {
		int in_brack = 0;

		want_regexp = 0;
		token = tokstart;
		while ((c = nextc()) != 0) {
			switch (c) {
			case '[':
				in_brack = 1;
				break;
			case ']':
				in_brack = 0;
				break;
			case '\\':
				if ((c = nextc()) == '\0') {
					yyerror("unterminated regexp ends with \\ at end of file");
				} else if (c == '\n') {
					sourceline++;
					continue;
				} else
					tokadd('\\');
				break;
			case '/':	/* end of the regexp */
				if (in_brack)
					break;

				pushback();
				tokadd('\0');
				yylval.sval = tokstart;
				return REGEXP;
			case '\n':
				pushback();
				yyerror("unterminated regexp");
			case '\0':
				yyerror("unterminated regexp at end of file");
			}
			tokadd(c);
		}
	}
retry:
	while ((c = nextc()) == ' ' || c == '\t')
		;

	lexeme = lexptr-1;
	thisline = NULL;
	token = tokstart;
	yylval.nodetypeval = Node_illegal;

	switch (c) {
	case 0:
		return 0;

	case '\n':
		sourceline++;
		return NEWLINE;

	case '#':		/* it's a comment */
		while ((c = nextc()) != '\n') {
			if (c == '\0')
				return 0;
		}
		sourceline++;
		return NEWLINE;

	case '\\':
#ifdef RELAXED_CONTINUATION
		if (!strict) {	/* strip trailing white-space and/or comment */
			while ((c = nextc()) == ' ' || c == '\t') continue;
			if (c == '#')
				while ((c = nextc()) != '\n') if (!c) break;
			pushback();
		}
#endif /*RELAXED_CONTINUATION*/
		if (nextc() == '\n') {
			sourceline++;
			goto retry;
		} else
			yyerror("inappropriate use of backslash");
		break;

	case '$':
		want_assign = 1;
		return '$';

	case ')':
	case ']':
	case '(':	
	case '[':
	case ';':
	case ':':
	case '?':
	case '{':
	case ',':
		return c;

	case '*':
		if ((c = nextc()) == '=') {
			yylval.nodetypeval = Node_assign_times;
			return ASSIGNOP;
		} else if (do_posix) {
			pushback();
			return '*';
		} else if (c == '*') {
			/* make ** and **= aliases for ^ and ^= */
			static int did_warn_op = 0, did_warn_assgn = 0;

			if (nextc() == '=') {
				if (do_lint && ! did_warn_assgn) {
					did_warn_assgn = 1;
					warning("**= is not allowed by POSIX");
				}
				yylval.nodetypeval = Node_assign_exp;
				return ASSIGNOP;
			} else {
				pushback();
				if (do_lint && ! did_warn_op) {
					did_warn_op = 1;
					warning("** is not allowed by POSIX");
				}
				return '^';
			}
		}
		pushback();
		return '*';

	case '/':
		if (want_assign) {
			if (nextc() == '=') {
				yylval.nodetypeval = Node_assign_quotient;
				return ASSIGNOP;
			}
			pushback();
		}
		return '/';

	case '%':
		if (nextc() == '=') {
			yylval.nodetypeval = Node_assign_mod;
			return ASSIGNOP;
		}
		pushback();
		return '%';

	case '^':
	{
		static int did_warn_op = 0, did_warn_assgn = 0;

		if (nextc() == '=') {

			if (do_lint && ! did_warn_assgn) {
				did_warn_assgn = 1;
				warning("operator `^=' is not supported in old awk");
			}
			yylval.nodetypeval = Node_assign_exp;
			return ASSIGNOP;
		}
		pushback();
		if (do_lint && ! did_warn_op) {
			did_warn_op = 1;
			warning("operator `^' is not supported in old awk");
		}
		return '^';
	}

	case '+':
		if ((c = nextc()) == '=') {
			yylval.nodetypeval = Node_assign_plus;
			return ASSIGNOP;
		}
		if (c == '+')
			return INCREMENT;
		pushback();
		return '+';

	case '!':
		if ((c = nextc()) == '=') {
			yylval.nodetypeval = Node_notequal;
			return RELOP;
		}
		if (c == '~') {
			yylval.nodetypeval = Node_nomatch;
			want_assign = 0;
			return MATCHOP;
		}
		pushback();
		return '!';

	case '<':
		if (nextc() == '=') {
			yylval.nodetypeval = Node_leq;
			return RELOP;
		}
		yylval.nodetypeval = Node_less;
		pushback();
		return '<';

	case '=':
		if (nextc() == '=') {
			yylval.nodetypeval = Node_equal;
			return RELOP;
		}
		yylval.nodetypeval = Node_assign;
		pushback();
		return ASSIGNOP;

	case '>':
		if ((c = nextc()) == '=') {
			yylval.nodetypeval = Node_geq;
			return RELOP;
		} else if (c == '>') {
			yylval.nodetypeval = Node_redirect_append;
			return APPEND_OP;
		}
		yylval.nodetypeval = Node_greater;
		pushback();
		return '>';

	case '~':
		yylval.nodetypeval = Node_match;
		want_assign = 0;
		return MATCHOP;

	case '}':
		/*
		 * Added did newline stuff.  Easier than
		 * hacking the grammar
		 */
		if (did_newline) {
			did_newline = 0;
			return c;
		}
		did_newline++;
		--lexptr;	/* pick up } next time */
		return NEWLINE;

	case '"':
		esc_seen = 0;
		while ((c = nextc()) != '"') {
			if (c == '\n') {
				pushback();
				yyerror("unterminated string");
			}
			if (c == '\\') {
				c = nextc();
				if (c == '\n') {
					sourceline++;
					continue;
				}
				esc_seen = 1;
				tokadd('\\');
			}
			if (c == '\0') {
				pushback();
				yyerror("unterminated string");
			}
			tokadd(c);
		}
		yylval.nodeval = make_str_node(tokstart,
					token - tokstart, esc_seen ? SCAN : 0);
		yylval.nodeval->flags |= PERM;
		return YSTRING;

	case '-':
		if ((c = nextc()) == '=') {
			yylval.nodetypeval = Node_assign_minus;
			return ASSIGNOP;
		}
		if (c == '-')
			return DECREMENT;
		pushback();
		return '-';

	case '.':
		c = nextc();
		pushback();
		if (!isdigit(c))
			return '.';
		else
			c = '.';	/* FALL THROUGH */
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
		/* It's a number */
		for (;;) {
			int gotnumber = 0;

			tokadd(c);
			switch (c) {
			case '.':
				if (seen_point) {
					gotnumber++;
					break;
				}
				++seen_point;
				break;
			case 'e':
			case 'E':
				if (seen_e) {
					gotnumber++;
					break;
				}
				++seen_e;
				if ((c = nextc()) == '-' || c == '+')
					tokadd(c);
				else
					pushback();
				break;
			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
			case '9':
				break;
			default:
				gotnumber++;
			}
			if (gotnumber)
				break;
			c = nextc();
		}
		pushback();
		yylval.nodeval = make_number(atof(tokstart));
		yylval.nodeval->flags |= PERM;
		return YNUMBER;

	case '&':
		if ((c = nextc()) == '&') {
			yylval.nodetypeval = Node_and;
			for (;;) {
				c = nextc();
				if (c == '\0')
					break;
				if (c == '#') {
					while ((c = nextc()) != '\n' && c != '\0')
						;
					if (c == '\0')
						break;
				}
				if (c == '\n')
					sourceline++;
				if (! isspace(c)) {
					pushback();
					break;
				}
			}
			want_assign = 0;
			return LEX_AND;
		}
		pushback();
		return '&';

	case '|':
		if ((c = nextc()) == '|') {
			yylval.nodetypeval = Node_or;
			for (;;) {
				c = nextc();
				if (c == '\0')
					break;
				if (c == '#') {
					while ((c = nextc()) != '\n' && c != '\0')
						;
					if (c == '\0')
						break;
				}
				if (c == '\n')
					sourceline++;
				if (! isspace(c)) {
					pushback();
					break;
				}
			}
			want_assign = 0;
			return LEX_OR;
		}
		pushback();
		return '|';
	}

	if (c != '_' && ! isalpha(c))
		yyerror("Invalid char '%c' in expression\n", c);

	/* it's some type of name-type-thing.  Find its length */
	token = tokstart;
	while (is_identchar(c)) {
		tokadd(c);
		c = nextc();
	}
	tokadd('\0');
	emalloc(tokkey, char *, token - tokstart, "yylex");
	memcpy(tokkey, tokstart, token - tokstart);
	pushback();

	/* See if it is a special token.  */
	low = 0;
	high = (sizeof (tokentab) / sizeof (tokentab[0])) - 1;
	while (low <= high) {
		int i/* , c */;

		mid = (low + high) / 2;
		c = *tokstart - tokentab[mid].operator[0];
		i = c ? c : strcmp (tokstart, tokentab[mid].operator);

		if (i < 0) {		/* token < mid */
			high = mid - 1;
		} else if (i > 0) {	/* token > mid */
			low = mid + 1;
		} else {
			if (do_lint) {
				if (tokentab[mid].flags & GAWKX)
					warning("%s() is a gawk extension",
						tokentab[mid].operator);
				if (tokentab[mid].flags & NOT_POSIX)
					warning("POSIX does not allow %s",
						tokentab[mid].operator);
				if (tokentab[mid].flags & NOT_OLD)
					warning("%s is not supported in old awk",
						tokentab[mid].operator);
			}
			if ((strict && (tokentab[mid].flags & GAWKX))
			    || (do_posix && (tokentab[mid].flags & NOT_POSIX)))
				break;
			if (tokentab[mid].class == LEX_BUILTIN
			    || tokentab[mid].class == LEX_LENGTH
			   )
				yylval.lval = mid;
			else
				yylval.nodetypeval = tokentab[mid].value;

			return tokentab[mid].class;
		}
	}

	yylval.sval = tokkey;
	if (*lexptr == '(')
		return FUNC_CALL;
	else {
		want_assign = 1;
		return NAME;
	}
}

static NODE *
node_common(op)
NODETYPE op;
{
	register NODE *r;

	getnode(r);
	r->type = op;
	r->flags = MALLOC;
	/* if lookahead is NL, lineno is 1 too high */
	if (lexeme && *lexeme == '\n')
		r->source_line = sourceline - 1;
	else
		r->source_line = sourceline;
	r->source_file = source;
	return r;
}

/*
 * This allocates a node with defined lnode and rnode. 
 */
NODE *
node(left, op, right)
NODE *left, *right;
NODETYPE op;
{
	register NODE *r;

	r = node_common(op);
	r->lnode = left;
	r->rnode = right;
	return r;
}

/*
 * This allocates a node with defined subnode and proc for builtin functions
 * Checks for arg. count and supplies defaults where possible.
 */
static NODE *
snode(subn, op, idx)
NODETYPE op;
int idx;
NODE *subn;
{
	register NODE *r;
	register NODE *n;
	int nexp = 0;
	int args_allowed;

	r = node_common(op);

	/* traverse expression list to see how many args. given */
	for (n= subn; n; n= n->rnode) {
		nexp++;
		if (nexp > 3)
			break;
	}

	/* check against how many args. are allowed for this builtin */
	args_allowed = tokentab[idx].flags & ARGS;
	if (args_allowed && !(args_allowed & A(nexp)))
		fatal("%s() cannot have %d argument%c",
			tokentab[idx].operator, nexp, nexp == 1 ? ' ' : 's');

	r->proc = tokentab[idx].ptr;

	/* special case processing for a few builtins */
	if (nexp == 0 && r->proc == do_length) {
		subn = node(node(make_number(0.0),Node_field_spec,(NODE *)NULL),
		            Node_expression_list,
			    (NODE *) NULL);
	} else if (r->proc == do_match) {
		if (subn->rnode->lnode->type != Node_regex)
			subn->rnode->lnode = mk_rexp(subn->rnode->lnode);
	} else if (r->proc == do_sub || r->proc == do_gsub) {
		if (subn->lnode->type != Node_regex)
			subn->lnode = mk_rexp(subn->lnode);
		if (nexp == 2)
			append_right(subn, node(node(make_number(0.0),
						     Node_field_spec,
						     (NODE *) NULL),
					        Node_expression_list,
						(NODE *) NULL));
		else if (do_lint && subn->rnode->rnode->lnode->type == Node_val)
			warning("string literal as last arg of substitute");
	} else if (r->proc == do_split) {
		if (nexp == 2)
			append_right(subn,
			    node(FS_node, Node_expression_list, (NODE *) NULL));
		n = subn->rnode->rnode->lnode;
		if (n->type != Node_regex)
			subn->rnode->rnode->lnode = mk_rexp(n);
		if (nexp == 2)
			subn->rnode->rnode->lnode->re_flags |= FS_DFLT;
	}

	r->subnode = subn;
	return r;
}

/*
 * This allocates a Node_line_range node with defined condpair and
 * zeroes the trigger word to avoid the temptation of assuming that calling
 * 'node( foo, Node_line_range, 0)' will properly initialize 'triggered'. 
 */
/* Otherwise like node() */
static NODE *
mkrangenode(cpair)
NODE *cpair;
{
	register NODE *r;

	getnode(r);
	r->type = Node_line_range;
	r->condpair = cpair;
	r->triggered = 0;
	return r;
}

/* Build a for loop */
static NODE *
make_for_loop(init, cond, incr)
NODE *init, *cond, *incr;
{
	register FOR_LOOP_HEADER *r;
	NODE *n;

	emalloc(r, FOR_LOOP_HEADER *, sizeof(FOR_LOOP_HEADER), "make_for_loop");
	getnode(n);
	n->type = Node_illegal;
	r->init = init;
	r->cond = cond;
	r->incr = incr;
	n->sub.nodep.r.hd = r;
	return n;
}

/*
 * Install a name in the symbol table, even if it is already there.
 * Caller must check against redefinition if that is desired. 
 */
NODE *
install(name, value)
char *name;
NODE *value;
{
	register NODE *hp;
	register int len, bucket;

	len = strlen(name);
	bucket = hash(name, len);
	getnode(hp);
	hp->type = Node_hashnode;
	hp->hnext = variables[bucket];
	variables[bucket] = hp;
	hp->hlength = len;
	hp->hvalue = value;
	hp->hname = name;
	return hp->hvalue;
}

/* find the most recent hash node for name installed by install */
NODE *
lookup(name)
char *name;
{
	register NODE *bucket;
	register int len;

	len = strlen(name);
	bucket = variables[hash(name, len)];
	while (bucket) {
		if (bucket->hlength == len && STREQN(bucket->hname, name, len))
			return bucket->hvalue;
		bucket = bucket->hnext;
	}
	return NULL;
}

/*
 * Add new to the rightmost branch of LIST.  This uses n^2 time, so we make
 * a simple attempt at optimizing it.
 */
static NODE *
append_right(list, new)
NODE *list, *new;
{
	register NODE *oldlist;
	static NODE *savefront = NULL, *savetail = NULL;

	oldlist = list;
	if (savefront == oldlist) {
		savetail = savetail->rnode = new;
		return oldlist;
	} else
		savefront = oldlist;
	while (list->rnode != NULL)
		list = list->rnode;
	savetail = list->rnode = new;
	return oldlist;
}

/*
 * check if name is already installed;  if so, it had better have Null value,
 * in which case def is added as the value. Otherwise, install name with def
 * as value. 
 */
static void
func_install(params, def)
NODE *params;
NODE *def;
{
	NODE *r;

	pop_params(params->rnode);
	pop_var(params, 0);
	r = lookup(params->param);
	if (r != NULL) {
		fatal("function name `%s' previously defined", params->param);
	} else
		(void) install(params->param, node(params, Node_func, def));
}

static void
pop_var(np, freeit)
NODE *np;
int freeit;
{
	register NODE *bucket, **save;
	register int len;
	char *name;

	name = np->param;
	len = strlen(name);
	save = &(variables[hash(name, len)]);
	for (bucket = *save; bucket; bucket = bucket->hnext) {
		if (len == bucket->hlength && STREQN(bucket->hname, name, len)) {
			*save = bucket->hnext;
			freenode(bucket);
			if (freeit)
				free(np->param);
			return;
		}
		save = &(bucket->hnext);
	}
}

static void
pop_params(params)
NODE *params;
{
	register NODE *np;

	for (np = params; np != NULL; np = np->rnode)
		pop_var(np, 1);
}

static NODE *
make_param(name)
char *name;
{
	NODE *r;

	getnode(r);
	r->type = Node_param_list;
	r->rnode = NULL;
	r->param = name;
	r->param_cnt = param_counter++;
	return (install(name, r));
}

/* Name points to a variable name.  Make sure its in the symbol table */
NODE *
variable(name, can_free)
char *name;
int can_free;
{
	register NODE *r;
	static int env_loaded = 0;

	if (!env_loaded && STREQ(name, "ENVIRON")) {
		load_environ();
		env_loaded = 1;
	}
	if ((r = lookup(name)) == NULL)
		r = install(name, node(Nnull_string, Node_var, (NODE *) NULL));
	else if (can_free)
		free(name);
	return r;
}

static NODE *
mk_rexp(exp)
NODE *exp;
{
	if (exp->type == Node_regex)
		return exp;
	else {
		NODE *n;

		getnode(n);
		n->type = Node_regex;
		n->re_exp = exp;
		n->re_text = NULL;
		n->re_reg = NULL;
		n->re_flags = 0;
		n->re_cnt = 1;
		return n;
	}
}
int yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
-1, 5,
	0, 61,
	-2, 0,
-1, 77,
	264, 78,
	267, 78,
	62, 78,
	124, 78,
	59, 78,
	-2, 0,
-1, 112,
	41, 86,
	-2, 0,
-1, 113,
	41, 86,
	-2, 0,
-1, 126,
	266, 0,
	-2, 101,
-1, 128,
	263, 0,
	60, 0,
	62, 0,
	124, 0,
	-2, 105,
-1, 129,
	263, 0,
	60, 0,
	62, 0,
	124, 0,
	-2, 106,
-1, 130,
	263, 0,
	60, 0,
	62, 0,
	124, 0,
	-2, 107,
-1, 149,
	264, 79,
	267, 79,
	62, 79,
	124, 79,
	59, 79,
	-2, 0,
-1, 188,
	41, 86,
	-2, 0,
-1, 190,
	41, 87,
	-2, 0,
-1, 224,
	41, 69,
	-2, 0,
-1, 253,
	266, 0,
	-2, 118,
-1, 255,
	263, 0,
	-2, 120,
-1, 263,
	41, 70,
	-2, 0,
	};
# define YYNPROD 161
# define YYLAST 1998
int yyact[]={

    62,   216,   107,    13,    87,   229,    13,    88,    89,   166,
   165,   123,   240,   297,     4,    82,    45,    37,    91,    45,
   292,   265,   291,   264,   204,    88,    89,   187,    88,    89,
   270,    24,   171,   266,   254,   155,    63,   161,    24,   127,
   176,    65,    98,   168,   169,    36,    52,    35,   157,    25,
    63,   174,    82,   122,    63,   124,   125,   126,    22,   128,
   129,   130,   131,    93,   136,    82,   189,   203,    66,    63,
   100,    82,   103,   222,   107,    64,   177,   101,    63,   158,
   158,    45,   102,    22,   103,   276,   206,    63,   232,   101,
   104,   174,   105,   269,   102,   224,   188,   163,    16,   178,
   144,   142,   113,   112,     6,    11,   133,    26,   186,   111,
    39,   257,   140,   186,   186,   170,    48,    94,   231,    99,
   160,   110,    82,   109,    86,    46,    41,   121,   159,   100,
   114,   115,   108,   134,    99,    99,   167,   164,    77,   262,
    71,   100,    82,   198,    82,   158,   141,   118,    91,   148,
   149,   263,   120,    10,    27,    20,     5,     1,    24,    50,
    12,   158,    17,   192,   223,    36,     0,    35,     0,    25,
     0,   225,   226,   228,     0,     0,   152,     0,     0,     0,
   191,    45,   207,   197,     0,     0,     0,   193,     0,   186,
   134,     0,     0,   201,     0,   237,    99,   241,   195,    99,
    99,    99,    99,    99,    99,   245,   246,   247,     0,   221,
   134,     0,   199,     0,     0,     0,   190,   217,     0,     0,
     0,     0,    94,   186,     4,    20,   205,     4,    24,     0,
     0,     0,    17,     0,    90,    36,     0,    35,     0,    25,
     0,     0,     0,   271,     0,    22,     0,    68,   172,   152,
     0,    45,    94,    23,    99,   233,     0,   236,   258,    30,
    23,     0,   280,    33,    34,   213,   172,     0,   211,    82,
   282,     0,    20,   158,     0,    24,    99,     0,    82,    17,
     0,   172,    36,     0,    35,   158,    25,   153,   212,   209,
   172,    31,    32,    28,    29,    82,   279,     0,    45,   208,
     0,     0,    82,    82,    82,     0,   152,   152,   152,   152,
   152,   273,   152,   152,   152,    22,    20,    68,     0,    24,
     0,   285,   155,    17,   185,    24,    36,     0,    35,    98,
    25,     0,    36,   295,    35,     0,    25,   281,     0,     0,
     0,     0,    45,     0,     0,     0,   289,   152,     0,   152,
   152,   152,   152,     0,   152,   152,   152,     0,     0,     0,
     0,     0,    22,   299,   202,     0,     0,     0,     0,     0,
   304,   305,   306,   152,   152,     0,     0,     0,   135,    30,
    23,     0,     0,    33,    34,     0,   152,     0,     0,     0,
     0,     0,     0,    83,     0,    80,    81,    72,    73,    74,
    75,    76,    84,    85,    78,    79,    22,    18,     0,     0,
     0,    31,    32,    28,    29,    20,     0,     0,    24,     0,
   215,     0,    17,     0,     0,    36,     0,    35,     0,    25,
     0,     0,     0,     0,     0,     0,     0,     0,     0,   260,
     0,    45,    59,     0,    60,    61,     0,     0,    67,    30,
    23,     0,     0,    33,    34,     0,     0,     0,     0,     0,
     0,     0,     0,    83,     0,    80,    81,    72,    73,    74,
    75,    76,    84,    85,    78,    79,     0,    18,     0,     0,
     0,    31,    32,    28,    29,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,    67,    30,    23,   150,     0,
    33,    34,    21,     0,     0,     0,    53,     0,     0,     0,
    83,     0,    80,    81,    72,    73,    74,    75,    76,    84,
    85,    78,    79,    96,    18,     0,     0,     0,    31,    32,
    28,    29,     0,     0,     0,     0,     0,     0,   116,   117,
    30,    23,     0,     0,    33,    34,     2,    23,     0,     0,
    33,    34,    38,     0,    83,     0,    80,    81,    72,    73,
    74,    75,    76,    84,    85,    78,    79,     0,    18,   106,
     0,     0,    31,    32,    28,    29,     0,     0,    31,    32,
   156,     0,     0,     0,     0,    20,     0,     0,    24,     0,
     0,   119,    17,     0,     0,    36,     0,    35,     0,    25,
    96,     0,     0,   179,   180,   181,   182,   183,   184,     0,
   132,     0,    59,     0,    60,   138,   139,     0,     0,     0,
   143,     0,     0,     0,     0,     0,     0,     0,   248,   250,
   251,   252,   253,     0,   255,   256,     0,     0,     0,    30,
    23,     0,     0,    33,    34,    58,     0,     0,    56,     4,
     0,     0,    20,   156,     0,    24,     0,     0,    96,    17,
   174,     0,    36,     0,    35,     0,    25,    18,    57,    54,
    55,    31,    32,    28,    29,     0,    53,     0,   275,    59,
   230,    60,    61,   196,     0,     0,    70,     0,     0,    14,
     0,     0,    14,    20,     0,   287,    24,    14,     0,    51,
    17,     0,     0,    36,     0,    35,     0,    25,     0,     0,
   156,   156,   156,   156,   156,     0,   156,   156,   156,    45,
    59,     0,    60,    61,     0,     0,     0,     0,    14,     0,
     0,     0,     0,    14,     0,     0,     0,     0,    20,     0,
     0,    24,   238,    53,     0,    17,     0,     0,    36,     0,
    35,   156,    25,   156,   156,   156,   156,     0,   156,   156,
   156,     0,     0,   194,     0,    59,     0,    60,    61,     0,
     0,     0,     0,     0,     0,     0,     0,   156,   156,     0,
     0,     0,    20,     0,    53,    24,   268,     0,     0,    17,
   156,     0,    36,    63,    35,     0,    25,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,   277,    59,
    23,    60,    61,    33,    34,    58,     0,     0,     0,     0,
     0,    20,     0,     0,    24,   290,     0,     0,    17,    53,
   294,    36,     0,    35,     0,    25,     0,     0,     0,     0,
   298,    31,    32,   301,   302,     0,     0,   303,    59,     0,
    60,    61,     0,     0,     0,     0,     0,     0,   155,     0,
     0,    24,     0,     0,     0,    98,     0,     0,    36,     0,
    35,     0,    25,    53,     0,     0,    30,    23,     0,     0,
    33,    34,    58,   274,     0,    56,     0,     0,   214,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,    18,    57,    54,    55,    31,    32,
    28,    29,    53,     0,     0,     0,     0,    30,    23,     0,
     0,    33,    34,    58,     0,     0,    56,    20,     0,     0,
    24,     0,     0,     0,    17,     0,     0,    36,     0,    35,
     0,    25,     0,     0,     0,    18,    57,    54,    55,    31,
    32,    28,    29,     0,    59,     0,    60,     0,     0,     0,
     0,     0,    30,    23,     0,     0,    33,    34,    58,   155,
     0,    56,    24,     0,     0,   155,    98,     0,    24,    36,
     0,    35,    98,    25,     0,    36,     0,    35,     0,    25,
    18,    57,    54,    55,    31,    32,    28,    29,     0,   214,
     0,     0,     0,    19,     0,     0,    30,    23,     0,     0,
    33,    34,    58,    20,     0,    56,    24,     0,    53,     0,
    17,     0,     0,    36,    95,    35,     0,    25,     0,     0,
     0,     0,     0,     0,    18,    57,    54,    55,    31,    32,
    28,    29,     0,     0,     0,    30,    23,     0,     0,    33,
    34,    58,    20,     0,    56,    24,     0,     0,     0,    17,
     0,     0,    36,     0,    35,     0,    25,     0,     0,     0,
     0,     0,     0,    18,    57,    54,    55,    31,    32,    28,
    29,   154,    30,    23,     0,     0,    33,    34,   213,     0,
     0,   211,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,    22,     0,     0,     0,     0,     0,     0,
   153,   212,   209,   210,    31,    32,    28,    29,    20,     0,
     0,    24,     0,     0,     0,    17,     0,     0,    36,     0,
    35,     0,    25,     0,   155,     0,     0,    24,     0,     0,
     0,    98,    22,     0,    36,    59,    35,    60,    25,     0,
     0,    30,    23,     0,   154,    33,    34,    58,     0,   218,
    56,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,    18,
    57,    54,     0,    31,    32,    28,    29,     0,     0,     0,
     0,     0,     0,    30,    23,     0,     0,    33,    34,   213,
    23,     0,   211,    33,    34,   213,     0,     0,     0,    53,
     0,   154,   154,   154,   154,   154,     0,   154,   154,   154,
     0,   153,   212,   209,   210,    31,    32,    28,    29,     0,
     0,    31,    32,     0,     0,     0,    40,    30,    23,     0,
     0,    33,    34,     0,     0,     0,     0,     4,     0,     8,
     9,     0,   154,     0,   154,   154,   154,   154,     0,   154,
   154,   154,     0,     0,    15,    18,     0,     0,     0,    31,
    32,    28,    29,     0,     0,     7,    30,    23,   154,   154,
    33,    34,     0,     0,     0,     0,     0,    20,     8,     9,
    24,   154,     0,     0,    17,     0,     0,    36,     0,    35,
     0,    25,     0,    15,    18,     0,     0,     0,    31,    32,
    28,    29,    20,    45,     0,    24,     0,     0,     0,    17,
     0,     0,    36,     0,    35,   155,    25,     0,    24,     0,
     0,     0,    98,     0,     0,    36,     0,    35,     0,    25,
     0,     0,    30,    23,     0,     0,    33,    34,    58,     0,
     0,    56,     0,     0,     0,     0,     0,     0,    30,    23,
     0,     0,    33,    34,   213,     0,     0,   211,     0,     0,
    18,    57,     0,     0,    31,    32,    28,    29,     0,     0,
     0,     0,     0,     0,     0,     0,   153,   212,     0,     0,
    31,    32,    28,    29,    20,     0,     0,    24,     0,     0,
   175,    17,   155,     0,    36,    24,    35,     0,    25,   147,
     0,     0,    36,     0,    35,     0,    25,     0,     0,     0,
    20,     0,     0,    24,     0,     0,     0,    17,     0,     0,
    36,     0,    35,     0,    25,     0,     0,     0,     0,     0,
     0,   155,     0,     0,    24,     0,     0,     0,    98,     0,
     0,    36,     0,    35,     0,    25,    20,     0,     0,    24,
     0,     0,     0,    17,     0,     0,    36,     0,    35,     0,
    25,     0,     0,     0,     0,     0,     0,     0,    97,     0,
     0,    24,     0,     0,     0,    98,     0,     0,    36,     0,
    35,   173,    25,    97,     0,     0,    24,     0,     0,     0,
    98,     0,     0,    36,     0,    35,     0,     0,     0,     0,
     0,    30,    23,     0,    20,    33,    34,    24,     0,     0,
     0,    17,     0,     0,    36,     0,    35,     0,    25,     0,
     0,     0,     0,     0,     0,    92,    30,    23,     0,    18,
    33,    34,     0,    31,    32,    28,    29,     0,   249,    30,
    23,     0,     0,    33,    34,     0,     0,     0,     0,     0,
     0,     0,     0,     0,    18,     0,     0,     0,    31,    32,
    28,    29,     0,     0,     0,     0,     0,   153,     0,     0,
     0,    31,    32,    28,    29,     0,     0,     0,     0,     0,
   234,     0,   235,     0,     0,     0,     0,     0,     0,   239,
     0,     0,     0,   243,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,   227,    30,    23,
     0,     0,    33,    34,   261,   151,    30,    23,    69,     0,
    33,    34,     0,     0,   267,     0,     0,    44,    44,    44,
     0,     0,     0,     0,    30,    23,    18,     0,    33,    34,
    31,    32,    28,    29,   153,     0,     0,     0,    31,    32,
    28,    29,     0,   278,     3,    30,    23,     0,     0,    33,
    34,   283,    18,    43,    43,    43,    31,    32,    28,    29,
    30,   200,     0,   293,    33,    34,   296,     0,     0,     0,
     0,     0,     0,   153,   137,     0,   300,    31,    32,    28,
    29,     0,    30,    23,    44,    44,    33,    34,    18,     0,
     0,    44,    31,    32,    28,    29,     0,    30,    23,     0,
    42,    33,    34,     0,     0,     0,     0,     0,     0,     0,
    47,    49,     0,     0,    31,    32,    28,    29,     0,    23,
    43,    43,    33,    34,     0,     0,     0,    43,     0,    31,
    32,    28,    29,     0,     0,     0,     0,     0,     0,     0,
     0,     0,   137,     0,     0,     0,     0,     0,     0,     0,
    31,    32,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,    44,     0,    44,     0,
     0,     0,     0,     0,     0,     0,   145,   146,     0,     0,
     0,     0,     0,   162,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,   137,    43,     0,    43,     0,     0,     0,     0,     0,
   242,     0,    44,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    44,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,    43,     0,
     0,   272,     0,     0,     0,     0,     0,     0,   219,     0,
   220,     0,     0,     0,     0,     0,    43,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
   284,     0,    44,     0,     0,    44,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,   244,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,    43,     0,
     0,    43,   259,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,   286,     0,     0,   288 };
int yypact[]={

  -253, -1000,  1019,  -250, -1000,   980, -1000, -1000,   -43,   -43,
   -40, -1000,   -65,   749,   192, -1000,  -261,  1279,    -5, -1000,
  1445,    47,  -253,   -17,  1460, -1000, -1000, -1000, -1000,    63,
    62,    -5,    -5, -1000, -1000,  1460,  1460, -1000, -1000, -1000,
 -1000,   -65, -1000,  -250,  -253, -1000,   -65, -1000, -1000, -1000,
 -1000,   239,  1387,  -274,  1387,  1387,  1387,  -219,  1387,  1387,
  1387,  1387,  1481,  -253,   122,    22, -1000, -1000,  -253,  -253,
   192, -1000,    61,  -253,    60,   -43,   -43,  1369,  1387,  1387,
 -1000,  -221,   382,    57, -1000, -1000,  -248, -1000, -1000, -1000,
    10,   619, -1000,    16, -1000, -1000,   -24,  1460,  1387,  -282,
  1460,  1460,  1460,  1460,  1460,  1460, -1000,  1279, -1000, -1000,
  -232,    56,  1279,  1279, -1000, -1000,   -24,   -24, -1000, -1000,
 -1000,   122,   788,    -5,  1085,   894,   552, -1000,  1481,  1481,
  1481,   705, -1000,    22, -1000, -1000,  -253, -1000, -1000, -1000,
 -1000,   122,  1387,   283,  1423, -1000, -1000,  1279,   -38,    43,
   936, -1000,  -264,    -5, -1000,  1445,    47,   -43,   788,   -43,
  1387,   -18, -1000,  1387,    55, -1000, -1000, -1000, -1000, -1000,
  1387,  1361,  1387,  -281, -1000, -1000, -1000,  1460,   619,   -24,
   -24,   -24,   -24,    35,    35,    25,   788,    41,  1279,    50,
    34,    50,    22, -1000,  1387,  -253, -1000, -1000,   619,  -263,
   -89,    22,    10,   -43,  1387,  1387,  1387,  1292,  1408,  1408,
  1408,  1408,  -224,  1408,  1408,   289, -1000,    16, -1000, -1000,
 -1000,   -43,  1279,   619,  -235,   788,   788, -1000,   788,  -225,
    47, -1000, -1000,    50, -1000, -1000, -1000,   788, -1000,  -253,
    53,  -228,  1254,   -38, -1000,   788,   788,   788,   936, -1000,
   936,  1101,     2,   942, -1000,   289,   825,  1408, -1000, -1000,
    -8,  -253,    50,     6, -1000, -1000, -1000, -1000,   283,  1387,
    50,   660,  1387,   -43,  1408,   936,   -43,   283,  -253,  -236,
 -1000, -1000,   619,  -253,  1387,    50, -1000,   936, -1000,  -259,
 -1000, -1000, -1000,  -253,   283,    50,  -253,  -253, -1000, -1000,
  -253,   283,   283,   283, -1000, -1000, -1000 };
int yypgo[]={

     0,   160,   159,   420,     0,   157,   156,   104,   502,   107,
   154,   153,   105,    98,   151,   150,   149,   216,    66,    75,
    68,   140,   139,    48,    63,  1003,    40,    67,   138,   137,
   136,   546,   126,   125,  1720,   124,  1400,   686,    41,    64,
    32,   121,  1664,  1628,   120,   115,   111,   109 };
int yyr1[]={

     0,     5,     6,     6,     6,     6,    32,     7,    33,     7,
     7,     7,     7,     7,     7,     7,    29,    29,    29,    30,
    30,    35,     1,     2,    11,    11,    41,    25,    12,    12,
    19,    19,    19,    19,    34,    34,    20,    20,    20,    20,
    20,    20,    20,    20,    20,    20,    20,    20,    20,    20,
    20,    44,    20,    20,    20,    28,    28,    21,    21,    42,
    42,    31,    31,    26,    26,    27,    27,    27,    27,    22,
    22,    14,    14,    14,    14,    14,    23,    23,    16,    16,
    15,    15,    15,    15,    15,    15,    18,    18,    17,    17,
    17,    17,    17,    17,    45,     4,     4,     4,     4,     4,
     4,     4,     4,     4,     4,     4,     4,     4,     4,     4,
     4,    46,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     8,     8,     8,     8,     8,     8,
     8,     8,     9,     9,    47,     9,     9,     9,     9,     9,
     9,     9,     9,     9,     9,    10,    10,    10,    24,    24,
    13,    13,    13,    13,    37,    38,    36,    39,    39,    43,
    40 };
int yyr2[]={

     0,     7,     3,     5,     3,     5,     1,     7,     1,     7,
     5,     5,     5,     3,     5,     5,     3,     3,     3,     2,
     2,     1,    15,     9,     3,     7,     1,     9,    11,     9,
     3,     5,     3,     5,     2,     4,     5,     5,     7,     3,
    13,    17,    17,    21,    19,     5,     5,    13,     9,     7,
     7,     1,     9,    13,     5,     3,     3,    13,    19,     3,
     4,     0,     2,     1,     5,     1,     5,     5,     5,     1,
     3,     3,     7,     3,     5,     7,     1,     3,     1,     3,
     3,     7,     3,     5,     7,     7,     1,     3,     3,     7,
     3,     5,     7,     7,     1,     9,    11,     9,     7,     7,
     7,     7,     3,     5,     7,     7,     7,     7,    11,     3,
     5,     1,     9,     7,     7,     7,     3,     5,     7,     7,
     7,    11,     3,     5,     2,     2,     7,     7,     7,     7,
     7,     7,     5,     7,     1,    11,     9,     3,     9,     5,
     5,     3,     3,     5,     5,     5,     5,     2,     1,     3,
     3,     9,     5,     5,     4,     5,     3,     0,     2,     3,
     5 };
int yychk[]={

 -1000,    -5,   -31,   -42,   267,    -6,    -7,   256,   269,   270,
   -11,   -12,    -1,    -4,   -37,   284,   -13,    40,   285,   -25,
    33,    -8,   123,   258,    36,    47,    -9,   -10,   291,   292,
   257,   289,   290,   261,   262,    45,    43,   267,   -31,    -7,
   256,   -32,   -34,   -42,   -43,    59,   -33,   -34,   -12,   -34,
    -2,   -37,   -40,   124,   287,   288,   266,   286,   263,    60,
    62,    63,    -4,    44,   -19,   -38,   -20,   256,   125,   -43,
   -37,   -21,   275,   276,   277,   278,   279,   -28,   282,   283,
   273,   274,    -4,   271,   280,   281,   -35,   265,   289,   290,
   -17,    -4,   256,   -24,   -13,   -25,    -8,    33,    40,   -13,
    94,    42,    47,    37,    43,    45,   -31,    91,    -9,   -13,
   -41,   -47,    40,    40,   -13,   -13,    -8,    -8,   -12,   -31,
   -12,   -19,    -4,   285,    -4,    -4,    -4,   258,    -4,    -4,
    -4,    -4,   -31,   -38,   -20,   256,   -39,   -43,   -31,   -31,
   -38,   -19,    40,   -31,    40,   -34,   -34,    40,   -16,   -15,
    -3,   256,   -13,   285,   -25,    33,    -8,   -23,    -4,   -23,
   -44,   258,   -34,    40,   -29,   258,   257,   -30,   291,   292,
   -45,   -40,   256,   -36,    41,   -36,   -26,    60,    -4,    -8,
    -8,    -8,    -8,    -8,    -8,   -17,    -4,   259,    40,   -18,
   -17,   -18,   -38,   -24,    58,   -39,   -31,   -38,    -4,   -20,
   258,   -23,   -17,   -27,    62,   264,   124,   -40,   256,   287,
   288,   266,   286,   263,    63,    -3,   265,   -24,   -25,   -34,
   -34,   -23,    91,    -4,    40,    -4,    -4,   256,    -4,   286,
    -8,    93,    47,   -18,   -36,   -36,   -39,    -4,   -31,   -36,
   275,   286,   -43,   -36,   -34,    -4,    -4,    -4,    -3,   256,
    -3,    -3,    -3,    -3,   258,    -3,    -3,   -46,   -26,   -34,
   -17,   -36,   -22,   -14,   258,   256,   258,   -36,   -31,    40,
   258,    -4,   -43,   -27,    58,    -3,    93,   -31,   -36,   -40,
   256,   -20,    -4,   -36,   -43,   -23,   -34,    -3,   -34,   -20,
   -31,   258,   256,   -36,   -31,   -23,   -36,   272,   -31,   -20,
   -36,   -31,   -31,   -31,   -20,   -20,   -20 };
int yydef[]={

    61,    -2,     0,    62,    59,    -2,     2,     4,     6,     8,
     0,    13,     0,    24,     0,    21,   147,     0,   148,   102,
     0,   109,    61,   150,     0,    26,   124,   125,   134,   137,
     0,     0,     0,   141,   142,     0,     0,    60,     1,     3,
     5,     0,    10,    34,    61,   159,     0,    11,    12,    14,
    15,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,   110,    61,     0,   157,    30,    32,    61,    61,
     0,    39,     0,    61,     0,     0,     0,    -2,    76,    76,
    51,     0,     0,     0,    55,    56,     0,    94,   145,   146,
     0,    88,    90,    63,   149,   103,   132,     0,     0,   147,
     0,     0,     0,     0,     0,     0,   154,     0,   152,   153,
     0,     0,    -2,    -2,   139,   140,   143,   144,     7,    35,
     9,     0,    25,   148,    99,   100,    -2,   104,    -2,    -2,
    -2,     0,   160,   157,    31,    33,    61,   158,   155,    36,
    37,     0,     0,     0,    76,    45,    46,     0,    65,    -2,
    80,    82,   147,   148,   116,     0,   122,     0,    77,     0,
    76,     0,    54,     0,     0,    16,    17,    18,    19,    20,
     0,     0,    91,     0,   156,   133,    98,     0,     0,   126,
   127,   128,   129,   130,   131,     0,    88,     0,    -2,     0,
    -2,     0,   157,    97,     0,    61,    29,    38,     0,     0,
   150,     0,     0,     0,     0,     0,     0,     0,    83,     0,
     0,     0,     0,     0,     0,   123,   111,    63,   117,    49,
    50,     0,     0,     0,    -2,    95,    89,    93,    92,     0,
    64,   151,    27,     0,   136,   138,    23,   108,    28,    61,
     0,     0,     0,    65,    48,    66,    67,    68,    81,    85,
    84,   113,   114,    -2,   119,    -2,     0,     0,   115,    52,
     0,    61,     0,    -2,    71,    73,    96,   135,     0,     0,
     0,     0,    76,     0,     0,   112,     0,     0,    61,     0,
    74,    40,     0,    61,    76,     0,    47,   121,    53,    57,
    22,    72,    75,    61,     0,     0,    61,    61,    41,    42,
    61,     0,     0,     0,    44,    58,    43 };
typedef struct { char *t_name; int t_val; } yytoktype;
#ifndef YYDEBUG
#	define YYDEBUG	0	/* don't allow debugging */
#endif

#if YYDEBUG

yytoktype yytoks[] =
{
	"FUNC_CALL",	257,
	"NAME",	258,
	"REGEXP",	259,
	"ERROR",	260,
	"YNUMBER",	261,
	"YSTRING",	262,
	"RELOP",	263,
	"APPEND_OP",	264,
	"ASSIGNOP",	265,
	"MATCHOP",	266,
	"NEWLINE",	267,
	"CONCAT_OP",	268,
	"LEX_BEGIN",	269,
	"LEX_END",	270,
	"LEX_IF",	271,
	"LEX_ELSE",	272,
	"LEX_RETURN",	273,
	"LEX_DELETE",	274,
	"LEX_WHILE",	275,
	"LEX_DO",	276,
	"LEX_FOR",	277,
	"LEX_BREAK",	278,
	"LEX_CONTINUE",	279,
	"LEX_PRINT",	280,
	"LEX_PRINTF",	281,
	"LEX_NEXT",	282,
	"LEX_EXIT",	283,
	"LEX_FUNCTION",	284,
	"LEX_GETLINE",	285,
	"LEX_IN",	286,
	"LEX_AND",	287,
	"LEX_OR",	288,
	"INCREMENT",	289,
	"DECREMENT",	290,
	"LEX_BUILTIN",	291,
	"LEX_LENGTH",	292,
	"?",	63,
	":",	58,
	"<",	60,
	">",	62,
	"|",	124,
	"+",	43,
	"-",	45,
	"*",	42,
	"/",	47,
	"%",	37,
	"!",	33,
	"UNARY",	293,
	"^",	94,
	"$",	36,
	"(",	40,
	")",	41,
	"-unknown-",	-1	/* ends search */
};

char * yyreds[] =
{
	"-no such reduction-",
	"start : opt_nls program opt_nls",
	"program : rule",
	"program : program rule",
	"program : error",
	"program : program error",
	"rule : LEX_BEGIN",
	"rule : LEX_BEGIN action",
	"rule : LEX_END",
	"rule : LEX_END action",
	"rule : LEX_BEGIN statement_term",
	"rule : LEX_END statement_term",
	"rule : pattern action",
	"rule : action",
	"rule : pattern statement_term",
	"rule : function_prologue function_body",
	"func_name : NAME",
	"func_name : FUNC_CALL",
	"func_name : lex_builtin",
	"lex_builtin : LEX_BUILTIN",
	"lex_builtin : LEX_LENGTH",
	"function_prologue : LEX_FUNCTION",
	"function_prologue : LEX_FUNCTION func_name '(' opt_param_list r_paren opt_nls",
	"function_body : l_brace statements r_brace opt_semi",
	"pattern : exp",
	"pattern : exp comma exp",
	"regexp : '/'",
	"regexp : '/' REGEXP '/'",
	"action : l_brace statements r_brace opt_semi opt_nls",
	"action : l_brace r_brace opt_semi opt_nls",
	"statements : statement",
	"statements : statements statement",
	"statements : error",
	"statements : statements error",
	"statement_term : nls",
	"statement_term : semi opt_nls",
	"statement : semi opt_nls",
	"statement : l_brace r_brace",
	"statement : l_brace statements r_brace",
	"statement : if_statement",
	"statement : LEX_WHILE '(' exp r_paren opt_nls statement",
	"statement : LEX_DO opt_nls statement LEX_WHILE '(' exp r_paren opt_nls",
	"statement : LEX_FOR '(' NAME LEX_IN NAME r_paren opt_nls statement",
	"statement : LEX_FOR '(' opt_exp semi exp semi opt_exp r_paren opt_nls statement",
	"statement : LEX_FOR '(' opt_exp semi semi opt_exp r_paren opt_nls statement",
	"statement : LEX_BREAK statement_term",
	"statement : LEX_CONTINUE statement_term",
	"statement : print '(' expression_list r_paren output_redir statement_term",
	"statement : print opt_rexpression_list output_redir statement_term",
	"statement : LEX_NEXT opt_exp statement_term",
	"statement : LEX_EXIT opt_exp statement_term",
	"statement : LEX_RETURN",
	"statement : LEX_RETURN opt_exp statement_term",
	"statement : LEX_DELETE NAME '[' expression_list ']' statement_term",
	"statement : exp statement_term",
	"print : LEX_PRINT",
	"print : LEX_PRINTF",
	"if_statement : LEX_IF '(' exp r_paren opt_nls statement",
	"if_statement : LEX_IF '(' exp r_paren opt_nls statement LEX_ELSE opt_nls statement",
	"nls : NEWLINE",
	"nls : nls NEWLINE",
	"opt_nls : /* empty */",
	"opt_nls : nls",
	"input_redir : /* empty */",
	"input_redir : '<' simp_exp",
	"output_redir : /* empty */",
	"output_redir : '>' exp",
	"output_redir : APPEND_OP exp",
	"output_redir : '|' exp",
	"opt_param_list : /* empty */",
	"opt_param_list : param_list",
	"param_list : NAME",
	"param_list : param_list comma NAME",
	"param_list : error",
	"param_list : param_list error",
	"param_list : param_list comma error",
	"opt_exp : /* empty */",
	"opt_exp : exp",
	"opt_rexpression_list : /* empty */",
	"opt_rexpression_list : rexpression_list",
	"rexpression_list : rexp",
	"rexpression_list : rexpression_list comma rexp",
	"rexpression_list : error",
	"rexpression_list : rexpression_list error",
	"rexpression_list : rexpression_list error rexp",
	"rexpression_list : rexpression_list comma error",
	"opt_expression_list : /* empty */",
	"opt_expression_list : expression_list",
	"expression_list : exp",
	"expression_list : expression_list comma exp",
	"expression_list : error",
	"expression_list : expression_list error",
	"expression_list : expression_list error exp",
	"expression_list : expression_list comma error",
	"exp : variable ASSIGNOP",
	"exp : variable ASSIGNOP exp",
	"exp : '(' expression_list r_paren LEX_IN NAME",
	"exp : exp '|' LEX_GETLINE opt_variable",
	"exp : LEX_GETLINE opt_variable input_redir",
	"exp : exp LEX_AND exp",
	"exp : exp LEX_OR exp",
	"exp : exp MATCHOP exp",
	"exp : regexp",
	"exp : '!' regexp",
	"exp : exp LEX_IN NAME",
	"exp : exp RELOP exp",
	"exp : exp '<' exp",
	"exp : exp '>' exp",
	"exp : exp '?' exp ':' exp",
	"exp : simp_exp",
	"exp : exp exp",
	"rexp : variable ASSIGNOP",
	"rexp : variable ASSIGNOP rexp",
	"rexp : rexp LEX_AND rexp",
	"rexp : rexp LEX_OR rexp",
	"rexp : LEX_GETLINE opt_variable input_redir",
	"rexp : regexp",
	"rexp : '!' regexp",
	"rexp : rexp MATCHOP rexp",
	"rexp : rexp LEX_IN NAME",
	"rexp : rexp RELOP rexp",
	"rexp : rexp '?' rexp ':' rexp",
	"rexp : simp_exp",
	"rexp : rexp rexp",
	"simp_exp : non_post_simp_exp",
	"simp_exp : post_inc_dec_exp",
	"simp_exp : simp_exp '^' simp_exp",
	"simp_exp : simp_exp '*' simp_exp",
	"simp_exp : simp_exp '/' simp_exp",
	"simp_exp : simp_exp '%' simp_exp",
	"simp_exp : simp_exp '+' simp_exp",
	"simp_exp : simp_exp '-' simp_exp",
	"non_post_simp_exp : '!' simp_exp",
	"non_post_simp_exp : '(' exp r_paren",
	"non_post_simp_exp : LEX_BUILTIN",
	"non_post_simp_exp : LEX_BUILTIN '(' opt_expression_list r_paren",
	"non_post_simp_exp : LEX_LENGTH '(' opt_expression_list r_paren",
	"non_post_simp_exp : LEX_LENGTH",
	"non_post_simp_exp : FUNC_CALL '(' opt_expression_list r_paren",
	"non_post_simp_exp : INCREMENT variable",
	"non_post_simp_exp : DECREMENT variable",
	"non_post_simp_exp : YNUMBER",
	"non_post_simp_exp : YSTRING",
	"non_post_simp_exp : '-' simp_exp",
	"non_post_simp_exp : '+' simp_exp",
	"post_inc_dec_exp : variable INCREMENT",
	"post_inc_dec_exp : variable DECREMENT",
	"post_inc_dec_exp : variable",
	"opt_variable : /* empty */",
	"opt_variable : variable",
	"variable : NAME",
	"variable : NAME '[' expression_list ']'",
	"variable : '$' non_post_simp_exp",
	"variable : '$' variable",
	"l_brace : '{' opt_nls",
	"r_brace : '}' opt_nls",
	"r_paren : ')'",
	"opt_semi : /* empty */",
	"opt_semi : semi",
	"semi : ';'",
	"comma : ',' opt_nls",
};
#endif /* YYDEBUG */
#line 1 "/usr/lib/yaccpar"
/*	@(#)yaccpar 1.10 89/04/04 SMI; from S5R3 1.10	*/

/*
** Skeleton parser driver for yacc output
*/

/*
** yacc user known macros and defines
*/
#define YYERROR		goto yyerrlab
#define YYACCEPT	{ free(yys); free(yyv); return(0); }
#define YYABORT		{ free(yys); free(yyv); return(1); }
#define YYBACKUP( newtoken, newvalue )\
{\
	if ( yychar >= 0 || ( yyr2[ yytmp ] >> 1 ) != 1 )\
	{\
		yyerror( "syntax error - cannot backup" );\
		goto yyerrlab;\
	}\
	yychar = newtoken;\
	yystate = *yyps;\
	yylval = newvalue;\
	goto yynewstate;\
}
#define YYRECOVERING()	(!!yyerrflag)
#ifndef YYDEBUG
#	define YYDEBUG	1	/* make debugging available */
#endif

/*
** user known globals
*/
int yydebug;			/* set to 1 to get debugging */

/*
** driver internal defines
*/
#define YYFLAG		(-1000)

/*
** static variables used by the parser
*/
static YYSTYPE *yyv;			/* value stack */
static int *yys;			/* state stack */

static YYSTYPE *yypv;			/* top of value stack */
static int *yyps;			/* top of state stack */

static int yystate;			/* current state */
static int yytmp;			/* extra var (lasts between blocks) */

int yynerrs;			/* number of errors */

int yyerrflag;			/* error recovery flag */
int yychar;			/* current input token number */


/*
** yyparse - return 0 if worked, 1 if syntax error not recovered from
*/
int
yyparse()
{
	register YYSTYPE *yypvt;	/* top of value stack for $vars */
	unsigned yymaxdepth = YYMAXDEPTH;

	/*
	** Initialize externals - yyparse may be called more than once
	*/
	yyv = (YYSTYPE*)malloc(yymaxdepth*sizeof(YYSTYPE));
	yys = (int*)malloc(yymaxdepth*sizeof(int));
	if (!yyv || !yys)
	{
		yyerror( "out of memory" );
		return(1);
	}
	yypv = &yyv[-1];
	yyps = &yys[-1];
	yystate = 0;
	yytmp = 0;
	yynerrs = 0;
	yyerrflag = 0;
	yychar = -1;

	goto yystack;
	{
		register YYSTYPE *yy_pv;	/* top of value stack */
		register int *yy_ps;		/* top of state stack */
		register int yy_state;		/* current state */
		register int  yy_n;		/* internal state number info */

		/*
		** get globals into registers.
		** branch to here only if YYBACKUP was called.
		*/
	yynewstate:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;
		goto yy_newstate;

		/*
		** get globals into registers.
		** either we just started, or we just finished a reduction
		*/
	yystack:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;

		/*
		** top of for (;;) loop while no reductions done
		*/
	yy_stack:
		/*
		** put a state and value onto the stacks
		*/
#if YYDEBUG
		/*
		** if debugging, look up token value in list of value vs.
		** name pairs.  0 and negative (-1) are special values.
		** Note: linear search is used since time is not a real
		** consideration while debugging.
		*/
		if ( yydebug )
		{
			register int yy_i;

			(void)printf( "State %d, token ", yy_state );
			if ( yychar == 0 )
				(void)printf( "end-of-file\n" );
			else if ( yychar < 0 )
				(void)printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				(void)printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ++yy_ps >= &yys[ yymaxdepth ] )	/* room on stack? */
		{
			/*
			** reallocate and recover.  Note that pointers
			** have to be reset, or bad things will happen
			*/
			int yyps_index = (yy_ps - yys);
			int yypv_index = (yy_pv - yyv);
			int yypvt_index = (yypvt - yyv);
			yymaxdepth += YYMAXDEPTH;
			yyv = (YYSTYPE*)realloc((char*)yyv,
				yymaxdepth * sizeof(YYSTYPE));
			yys = (int*)realloc((char*)yys,
				yymaxdepth * sizeof(int));
			if (!yyv || !yys)
			{
				yyerror( "yacc stack overflow" );
				return(1);
			}
			yy_ps = yys + yyps_index;
			yy_pv = yyv + yypv_index;
			yypvt = yyv + yypvt_index;
		}
		*yy_ps = yy_state;
		*++yy_pv = yyval;

		/*
		** we have a new state - find out what to do
		*/
	yy_newstate:
		if ( ( yy_n = yypact[ yy_state ] ) <= YYFLAG )
			goto yydefault;		/* simple state */
#if YYDEBUG
		/*
		** if debugging, need to mark whether new token grabbed
		*/
		yytmp = yychar < 0;
#endif
		if ( ( yychar < 0 ) && ( ( yychar = yylex() ) < 0 ) )
			yychar = 0;		/* reached EOF */
#if YYDEBUG
		if ( yydebug && yytmp )
		{
			register int yy_i;

			(void)printf( "Received token " );
			if ( yychar == 0 )
				(void)printf( "end-of-file\n" );
			else if ( yychar < 0 )
				(void)printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				(void)printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ( ( yy_n += yychar ) < 0 ) || ( yy_n >= YYLAST ) )
			goto yydefault;
		if ( yychk[ yy_n = yyact[ yy_n ] ] == yychar )	/*valid shift*/
		{
			yychar = -1;
			yyval = yylval;
			yy_state = yy_n;
			if ( yyerrflag > 0 )
				yyerrflag--;
			goto yy_stack;
		}

	yydefault:
		if ( ( yy_n = yydef[ yy_state ] ) == -2 )
		{
#if YYDEBUG
			yytmp = yychar < 0;
#endif
			if ( ( yychar < 0 ) && ( ( yychar = yylex() ) < 0 ) )
				yychar = 0;		/* reached EOF */
#if YYDEBUG
			if ( yydebug && yytmp )
			{
				register int yy_i;

				(void)printf( "Received token " );
				if ( yychar == 0 )
					(void)printf( "end-of-file\n" );
				else if ( yychar < 0 )
					(void)printf( "-none-\n" );
				else
				{
					for ( yy_i = 0;
						yytoks[yy_i].t_val >= 0;
						yy_i++ )
					{
						if ( yytoks[yy_i].t_val
							== yychar )
						{
							break;
						}
					}
					(void)printf( "%s\n", yytoks[yy_i].t_name );
				}
			}
#endif /* YYDEBUG */
			/*
			** look through exception table
			*/
			{
				register int *yyxi = yyexca;

				while ( ( *yyxi != -1 ) ||
					( yyxi[1] != yy_state ) )
				{
					yyxi += 2;
				}
				while ( ( *(yyxi += 2) >= 0 ) &&
					( *yyxi != yychar ) )
					;
				if ( ( yy_n = yyxi[1] ) < 0 )
					YYACCEPT;
			}
		}

		/*
		** check for syntax error
		*/
		if ( yy_n == 0 )	/* have an error */
		{
			/* no worry about speed here! */
			switch ( yyerrflag )
			{
			case 0:		/* new error */
				yyerror( "syntax error" );
				goto skip_init;
			yyerrlab:
				/*
				** get globals into registers.
				** we have a user generated syntax type error
				*/
				yy_pv = yypv;
				yy_ps = yyps;
				yy_state = yystate;
				yynerrs++;
			skip_init:
			case 1:
			case 2:		/* incompletely recovered error */
					/* try again... */
				yyerrflag = 3;
				/*
				** find state where "error" is a legal
				** shift action
				*/
				while ( yy_ps >= yys )
				{
					yy_n = yypact[ *yy_ps ] + YYERRCODE;
					if ( yy_n >= 0 && yy_n < YYLAST &&
						yychk[yyact[yy_n]] == YYERRCODE)					{
						/*
						** simulate shift of "error"
						*/
						yy_state = yyact[ yy_n ];
						goto yy_stack;
					}
					/*
					** current state has no shift on
					** "error", pop stack
					*/
#if YYDEBUG
#	define _POP_ "Error recovery pops state %d, uncovers state %d\n"
					if ( yydebug )
						(void)printf( _POP_, *yy_ps,
							yy_ps[-1] );
#	undef _POP_
#endif
					yy_ps--;
					yy_pv--;
				}
				/*
				** there is no state on stack with "error" as
				** a valid shift.  give up.
				*/
				YYABORT;
			case 3:		/* no shift yet; eat a token */
#if YYDEBUG
				/*
				** if debugging, look up token in list of
				** pairs.  0 and negative shouldn't occur,
				** but since timing doesn't matter when
				** debugging, it doesn't hurt to leave the
				** tests here.
				*/
				if ( yydebug )
				{
					register int yy_i;

					(void)printf( "Error recovery discards " );
					if ( yychar == 0 )
						(void)printf( "token end-of-file\n" );
					else if ( yychar < 0 )
						(void)printf( "token -none-\n" );
					else
					{
						for ( yy_i = 0;
							yytoks[yy_i].t_val >= 0;
							yy_i++ )
						{
							if ( yytoks[yy_i].t_val
								== yychar )
							{
								break;
							}
						}
						(void)printf( "token %s\n",
							yytoks[yy_i].t_name );
					}
				}
#endif /* YYDEBUG */
				if ( yychar == 0 )	/* reached EOF. quit */
					YYABORT;
				yychar = -1;
				goto yy_newstate;
			}
		}/* end if ( yy_n == 0 ) */
		/*
		** reduction by production yy_n
		** put stack tops, etc. so things right after switch
		*/
#if YYDEBUG
		/*
		** if debugging, print the string that is the user's
		** specification of the reduction which is just about
		** to be done.
		*/
		if ( yydebug )
			(void)printf( "Reduce by (%d) \"%s\"\n",
				yy_n, yyreds[ yy_n ] );
#endif
		yytmp = yy_n;			/* value to switch over */
		yypvt = yy_pv;			/* $vars top of value stack */
		/*
		** Look in goto table for next state
		** Sorry about using yy_state here as temporary
		** register variable, but why not, if it works...
		** If yyr2[ yy_n ] doesn't have the low order bit
		** set, then there is no action to be done for
		** this reduction.  So, no saving & unsaving of
		** registers done.  The only difference between the
		** code just after the if and the body of the if is
		** the goto yy_stack in the body.  This way the test
		** can be made before the choice of what to do is needed.
		*/
		{
			/* length of production doubled with extra bit */
			register int yy_len = yyr2[ yy_n ];

			if ( !( yy_len & 01 ) )
			{
				yy_len >>= 1;
				yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
				yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
					*( yy_ps -= yy_len ) + 1;
				if ( yy_state >= YYLAST ||
					yychk[ yy_state =
					yyact[ yy_state ] ] != -yy_n )
				{
					yy_state = yyact[ yypgo[ yy_n ] ];
				}
				goto yy_stack;
			}
			yy_len >>= 1;
			yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
			yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
				*( yy_ps -= yy_len ) + 1;
			if ( yy_state >= YYLAST ||
				yychk[ yy_state = yyact[ yy_state ] ] != -yy_n )
			{
				yy_state = yyact[ yypgo[ yy_n ] ];
			}
		}
					/* save until reenter driver code */
		yystate = yy_state;
		yyps = yy_ps;
		yypv = yy_pv;
	}
	/*
	** code supplied by user is placed in this switch
	*/
	switch( yytmp )
	{
		
case 1:
# line 137 "awk.y"
{ expression_value = yypvt[-1].nodeval; } break;
case 2:
# line 142 "awk.y"
{ 
			if (yypvt[-0].nodeval != NULL)
				yyval.nodeval = yypvt[-0].nodeval;
			else
				yyval.nodeval = NULL;
			yyerrok;
		} break;
case 3:
# line 151 "awk.y"
{
			if (yypvt[-0].nodeval == NULL)
				yyval.nodeval = yypvt[-1].nodeval;
			else if (yypvt[-1].nodeval == NULL)
				yyval.nodeval = yypvt[-0].nodeval;
			else {
				if (yypvt[-1].nodeval->type != Node_rule_list)
					yypvt[-1].nodeval = node(yypvt[-1].nodeval, Node_rule_list,
						(NODE*)NULL);
				yyval.nodeval = append_right (yypvt[-1].nodeval,
				   node(yypvt[-0].nodeval, Node_rule_list,(NODE *) NULL));
			}
			yyerrok;
		} break;
case 4:
# line 165 "awk.y"
{ yyval.nodeval = NULL; } break;
case 5:
# line 166 "awk.y"
{ yyval.nodeval = NULL; } break;
case 6:
# line 170 "awk.y"
{ io_allowed = 0; } break;
case 7:
# line 172 "awk.y"
{
		if (begin_block) {
			if (begin_block->type != Node_rule_list)
				begin_block = node(begin_block, Node_rule_list,
					(NODE *)NULL);
			(void) append_right (begin_block, node(
			    node((NODE *)NULL, Node_rule_node, yypvt[-0].nodeval),
			    Node_rule_list, (NODE *)NULL) );
		} else
			begin_block = node((NODE *)NULL, Node_rule_node, yypvt[-0].nodeval);
		yyval.nodeval = NULL;
		io_allowed = 1;
		yyerrok;
	  } break;
case 8:
# line 186 "awk.y"
{ io_allowed = 0; } break;
case 9:
# line 188 "awk.y"
{
		if (end_block) {
			if (end_block->type != Node_rule_list)
				end_block = node(end_block, Node_rule_list,
					(NODE *)NULL);
			(void) append_right (end_block, node(
			    node((NODE *)NULL, Node_rule_node, yypvt[-0].nodeval),
			    Node_rule_list, (NODE *)NULL));
		} else
			end_block = node((NODE *)NULL, Node_rule_node, yypvt[-0].nodeval);
		yyval.nodeval = NULL;
		io_allowed = 1;
		yyerrok;
	  } break;
case 10:
# line 203 "awk.y"
{
		warning("BEGIN blocks must have an action part");
		errcount++;
		yyerrok;
	  } break;
case 11:
# line 209 "awk.y"
{
		warning("END blocks must have an action part");
		errcount++;
		yyerrok;
	  } break;
case 12:
# line 215 "awk.y"
{ yyval.nodeval = node (yypvt[-1].nodeval, Node_rule_node, yypvt[-0].nodeval); yyerrok; } break;
case 13:
# line 217 "awk.y"
{ yyval.nodeval = node ((NODE *)NULL, Node_rule_node, yypvt[-0].nodeval); yyerrok; } break;
case 14:
# line 219 "awk.y"
{
		  yyval.nodeval = node (yypvt[-1].nodeval,
			     Node_rule_node,
			     node(node(node(make_number(0.0),
					    Node_field_spec,
					    (NODE *) NULL),
					Node_expression_list,
					(NODE *) NULL),
				  Node_K_print,
				  (NODE *) NULL));
		  yyerrok;
		} break;
case 15:
# line 232 "awk.y"
{
			func_install(yypvt[-1].nodeval, yypvt[-0].nodeval);
			yyval.nodeval = NULL;
			yyerrok;
		} break;
case 16:
# line 241 "awk.y"
{ yyval.sval = yypvt[-0].sval; } break;
case 17:
# line 243 "awk.y"
{ yyval.sval = yypvt[-0].sval; } break;
case 18:
# line 245 "awk.y"
{
		yyerror("%s() is a built-in function, it cannot be redefined",
			tokstart);
		errcount++;
		/* yyerrok; */
	  } break;
case 21:
# line 260 "awk.y"
{
			param_counter = 0;
		} break;
case 22:
# line 264 "awk.y"
{
			yyval.nodeval = append_right(make_param(yypvt[-4].sval), yypvt[-2].nodeval);
			can_return = 1;
		} break;
case 23:
# line 272 "awk.y"
{
		yyval.nodeval = yypvt[-2].nodeval;
		can_return = 0;
	  } break;
case 24:
# line 281 "awk.y"
{ yyval.nodeval = yypvt[-0].nodeval; } break;
case 25:
# line 283 "awk.y"
{ yyval.nodeval = mkrangenode ( node(yypvt[-2].nodeval, Node_cond_pair, yypvt[-0].nodeval) ); } break;
case 26:
# line 292 "awk.y"
{ ++want_regexp; } break;
case 27:
# line 294 "awk.y"
{
		  NODE *n;

		  getnode(n);
		  n->type = Node_regex;
		  n->re_exp = make_string(yypvt[-1].sval, strlen(yypvt[-1].sval));
		  n->re_reg = mk_re_parse(yypvt[-1].sval, 0);
		  n->re_text = NULL;
		  n->re_flags = CONST;
		  n->re_cnt = 1;
		  yyval.nodeval = n;
		} break;
case 28:
# line 310 "awk.y"
{ yyval.nodeval = yypvt[-3].nodeval ; } break;
case 29:
# line 312 "awk.y"
{ yyval.nodeval = NULL; } break;
case 30:
# line 317 "awk.y"
{ yyval.nodeval = yypvt[-0].nodeval; } break;
case 31:
# line 319 "awk.y"
{
			if (yypvt[-1].nodeval == NULL || yypvt[-1].nodeval->type != Node_statement_list)
				yypvt[-1].nodeval = node(yypvt[-1].nodeval, Node_statement_list,(NODE *)NULL);
	    		yyval.nodeval = append_right(yypvt[-1].nodeval,
				node( yypvt[-0].nodeval, Node_statement_list, (NODE *)NULL));
	    		yyerrok;
		} break;
case 32:
# line 327 "awk.y"
{ yyval.nodeval = NULL; } break;
case 33:
# line 329 "awk.y"
{ yyval.nodeval = NULL; } break;
case 36:
# line 339 "awk.y"
{ yyval.nodeval = NULL; } break;
case 37:
# line 341 "awk.y"
{ yyval.nodeval = NULL; } break;
case 38:
# line 343 "awk.y"
{ yyval.nodeval = yypvt[-1].nodeval; } break;
case 39:
# line 345 "awk.y"
{ yyval.nodeval = yypvt[-0].nodeval; } break;
case 40:
# line 347 "awk.y"
{ yyval.nodeval = node (yypvt[-3].nodeval, Node_K_while, yypvt[-0].nodeval); } break;
case 41:
# line 349 "awk.y"
{ yyval.nodeval = node (yypvt[-2].nodeval, Node_K_do, yypvt[-5].nodeval); } break;
case 42:
# line 351 "awk.y"
{
		yyval.nodeval = node (yypvt[-0].nodeval, Node_K_arrayfor, make_for_loop(variable(yypvt[-5].sval,1),
			(NODE *)NULL, variable(yypvt[-3].sval,1)));
	  } break;
case 43:
# line 356 "awk.y"
{
		yyval.nodeval = node(yypvt[-0].nodeval, Node_K_for, (NODE *)make_for_loop(yypvt[-7].nodeval, yypvt[-5].nodeval, yypvt[-3].nodeval));
	  } break;
case 44:
# line 360 "awk.y"
{
		yyval.nodeval = node (yypvt[-0].nodeval, Node_K_for,
			(NODE *)make_for_loop(yypvt[-6].nodeval, (NODE *)NULL, yypvt[-3].nodeval));
	  } break;
case 45:
# line 366 "awk.y"
{ yyval.nodeval = node ((NODE *)NULL, Node_K_break, (NODE *)NULL); } break;
case 46:
# line 369 "awk.y"
{ yyval.nodeval = node ((NODE *)NULL, Node_K_continue, (NODE *)NULL); } break;
case 47:
# line 371 "awk.y"
{ yyval.nodeval = node (yypvt[-3].nodeval, yypvt[-5].nodetypeval, yypvt[-1].nodeval); } break;
case 48:
# line 373 "awk.y"
{
			if (yypvt[-3].nodetypeval == Node_K_print && yypvt[-2].nodeval == NULL)
				yypvt[-2].nodeval = node(node(make_number(0.0),
					       Node_field_spec,
					       (NODE *) NULL),
					  Node_expression_list,
					  (NODE *) NULL);

			yyval.nodeval = node (yypvt[-2].nodeval, yypvt[-3].nodetypeval, yypvt[-1].nodeval);
		} break;
case 49:
# line 384 "awk.y"
{ NODETYPE type;

		  if (! io_allowed) yyerror("next used in BEGIN or END action");
		  if (yypvt[-1].nodeval && yypvt[-1].nodeval == lookup("file")) {
			if (do_lint)
				warning("`next file' is a gawk extension");
			else if (strict || do_posix)
				yyerror("`next file' is a gawk extension");
			type = Node_K_nextfile;
		  } else type = Node_K_next;
		  yyval.nodeval = node ((NODE *)NULL, type, (NODE *)NULL);
		} break;
case 50:
# line 397 "awk.y"
{ yyval.nodeval = node (yypvt[-1].nodeval, Node_K_exit, (NODE *)NULL); } break;
case 51:
# line 399 "awk.y"
{ if (! can_return) yyerror("return used outside function context"); } break;
case 52:
# line 401 "awk.y"
{ yyval.nodeval = node (yypvt[-1].nodeval, Node_K_return, (NODE *)NULL); } break;
case 53:
# line 403 "awk.y"
{ yyval.nodeval = node (variable(yypvt[-4].sval,1), Node_K_delete, yypvt[-2].nodeval); } break;
case 54:
# line 405 "awk.y"
{ yyval.nodeval = yypvt[-1].nodeval; } break;
case 55:
# line 410 "awk.y"
{ yyval.nodetypeval = yypvt[-0].nodetypeval; } break;
case 56:
# line 412 "awk.y"
{ yyval.nodetypeval = yypvt[-0].nodetypeval; } break;
case 57:
# line 417 "awk.y"
{
		yyval.nodeval = node(yypvt[-3].nodeval, Node_K_if, 
			node(yypvt[-0].nodeval, Node_if_branches, (NODE *)NULL));
	  } break;
case 58:
# line 423 "awk.y"
{ yyval.nodeval = node (yypvt[-6].nodeval, Node_K_if,
				node (yypvt[-3].nodeval, Node_if_branches, yypvt[-0].nodeval)); } break;
case 59:
# line 429 "awk.y"
{ want_assign = 0; } break;
case 63:
# line 440 "awk.y"
{ yyval.nodeval = NULL; } break;
case 64:
# line 442 "awk.y"
{ yyval.nodeval = node (yypvt[-0].nodeval, Node_redirect_input, (NODE *)NULL); } break;
case 65:
# line 447 "awk.y"
{ yyval.nodeval = NULL; } break;
case 66:
# line 449 "awk.y"
{ yyval.nodeval = node (yypvt[-0].nodeval, Node_redirect_output, (NODE *)NULL); } break;
case 67:
# line 451 "awk.y"
{ yyval.nodeval = node (yypvt[-0].nodeval, Node_redirect_append, (NODE *)NULL); } break;
case 68:
# line 453 "awk.y"
{ yyval.nodeval = node (yypvt[-0].nodeval, Node_redirect_pipe, (NODE *)NULL); } break;
case 69:
# line 458 "awk.y"
{ yyval.nodeval = NULL; } break;
case 70:
# line 460 "awk.y"
{ yyval.nodeval = yypvt[-0].nodeval; } break;
case 71:
# line 465 "awk.y"
{ yyval.nodeval = make_param(yypvt[-0].sval); } break;
case 72:
# line 467 "awk.y"
{ yyval.nodeval = append_right(yypvt[-2].nodeval, make_param(yypvt[-0].sval)); yyerrok; } break;
case 73:
# line 469 "awk.y"
{ yyval.nodeval = NULL; } break;
case 74:
# line 471 "awk.y"
{ yyval.nodeval = NULL; } break;
case 75:
# line 473 "awk.y"
{ yyval.nodeval = NULL; } break;
case 76:
# line 479 "awk.y"
{ yyval.nodeval = NULL; } break;
case 77:
# line 481 "awk.y"
{ yyval.nodeval = yypvt[-0].nodeval; } break;
case 78:
# line 486 "awk.y"
{ yyval.nodeval = NULL; } break;
case 79:
# line 488 "awk.y"
{ yyval.nodeval = yypvt[-0].nodeval; } break;
case 80:
# line 493 "awk.y"
{ yyval.nodeval = node (yypvt[-0].nodeval, Node_expression_list, (NODE *)NULL); } break;
case 81:
# line 495 "awk.y"
{
		yyval.nodeval = append_right(yypvt[-2].nodeval,
			node( yypvt[-0].nodeval, Node_expression_list, (NODE *)NULL));
		yyerrok;
	  } break;
case 82:
# line 501 "awk.y"
{ yyval.nodeval = NULL; } break;
case 83:
# line 503 "awk.y"
{ yyval.nodeval = NULL; } break;
case 84:
# line 505 "awk.y"
{ yyval.nodeval = NULL; } break;
case 85:
# line 507 "awk.y"
{ yyval.nodeval = NULL; } break;
case 86:
# line 512 "awk.y"
{ yyval.nodeval = NULL; } break;
case 87:
# line 514 "awk.y"
{ yyval.nodeval = yypvt[-0].nodeval; } break;
case 88:
# line 519 "awk.y"
{ yyval.nodeval = node (yypvt[-0].nodeval, Node_expression_list, (NODE *)NULL); } break;
case 89:
# line 521 "awk.y"
{
			yyval.nodeval = append_right(yypvt[-2].nodeval,
				node( yypvt[-0].nodeval, Node_expression_list, (NODE *)NULL));
			yyerrok;
		} break;
case 90:
# line 527 "awk.y"
{ yyval.nodeval = NULL; } break;
case 91:
# line 529 "awk.y"
{ yyval.nodeval = NULL; } break;
case 92:
# line 531 "awk.y"
{ yyval.nodeval = NULL; } break;
case 93:
# line 533 "awk.y"
{ yyval.nodeval = NULL; } break;
case 94:
# line 538 "awk.y"
{ want_assign = 0; } break;
case 95:
# line 540 "awk.y"
{
		  if (do_lint && yypvt[-0].nodeval->type == Node_regex)
			warning("Regular expression on left of assignment.");
		  yyval.nodeval = node (yypvt[-3].nodeval, yypvt[-2].nodetypeval, yypvt[-0].nodeval);
		} break;
case 96:
# line 546 "awk.y"
{ yyval.nodeval = node (variable(yypvt[-0].sval,1), Node_in_array, yypvt[-3].nodeval); } break;
case 97:
# line 548 "awk.y"
{
		  yyval.nodeval = node (yypvt[-0].nodeval, Node_K_getline,
			 node (yypvt[-3].nodeval, Node_redirect_pipein, (NODE *)NULL));
		} break;
case 98:
# line 553 "awk.y"
{
		  if (do_lint && ! io_allowed && yypvt[-0].nodeval == NULL)
			warning("non-redirected getline undefined inside BEGIN or END action");
		  yyval.nodeval = node (yypvt[-1].nodeval, Node_K_getline, yypvt[-0].nodeval);
		} break;
case 99:
# line 559 "awk.y"
{ yyval.nodeval = node (yypvt[-2].nodeval, Node_and, yypvt[-0].nodeval); } break;
case 100:
# line 561 "awk.y"
{ yyval.nodeval = node (yypvt[-2].nodeval, Node_or, yypvt[-0].nodeval); } break;
case 101:
# line 563 "awk.y"
{
		  if (yypvt[-2].nodeval->type == Node_regex)
			warning("Regular expression on left of MATCH operator.");
		  yyval.nodeval = node (yypvt[-2].nodeval, yypvt[-1].nodetypeval, mk_rexp(yypvt[-0].nodeval));
		} break;
case 102:
# line 569 "awk.y"
{ yyval.nodeval = yypvt[-0].nodeval; } break;
case 103:
# line 571 "awk.y"
{
		  yyval.nodeval = node(node(make_number(0.0),
				 Node_field_spec,
				 (NODE *) NULL),
		            Node_nomatch,
			    yypvt[-0].nodeval);
		} break;
case 104:
# line 579 "awk.y"
{ yyval.nodeval = node (variable(yypvt[-0].sval,1), Node_in_array, yypvt[-2].nodeval); } break;
case 105:
# line 581 "awk.y"
{
		  if (do_lint && yypvt[-0].nodeval->type == Node_regex)
			warning("Regular expression on left of comparison.");
		  yyval.nodeval = node (yypvt[-2].nodeval, yypvt[-1].nodetypeval, yypvt[-0].nodeval);
		} break;
case 106:
# line 587 "awk.y"
{ yyval.nodeval = node (yypvt[-2].nodeval, Node_less, yypvt[-0].nodeval); } break;
case 107:
# line 589 "awk.y"
{ yyval.nodeval = node (yypvt[-2].nodeval, Node_greater, yypvt[-0].nodeval); } break;
case 108:
# line 591 "awk.y"
{ yyval.nodeval = node(yypvt[-4].nodeval, Node_cond_exp, node(yypvt[-2].nodeval, Node_if_branches, yypvt[-0].nodeval));} break;
case 109:
# line 593 "awk.y"
{ yyval.nodeval = yypvt[-0].nodeval; } break;
case 110:
# line 595 "awk.y"
{ yyval.nodeval = node (yypvt[-1].nodeval, Node_concat, yypvt[-0].nodeval); } break;
case 111:
# line 600 "awk.y"
{ want_assign = 0; } break;
case 112:
# line 602 "awk.y"
{ yyval.nodeval = node (yypvt[-3].nodeval, yypvt[-2].nodetypeval, yypvt[-0].nodeval); } break;
case 113:
# line 604 "awk.y"
{ yyval.nodeval = node (yypvt[-2].nodeval, Node_and, yypvt[-0].nodeval); } break;
case 114:
# line 606 "awk.y"
{ yyval.nodeval = node (yypvt[-2].nodeval, Node_or, yypvt[-0].nodeval); } break;
case 115:
# line 608 "awk.y"
{
		  if (do_lint && ! io_allowed && yypvt[-0].nodeval == NULL)
			warning("non-redirected getline undefined inside BEGIN or END action");
		  yyval.nodeval = node (yypvt[-1].nodeval, Node_K_getline, yypvt[-0].nodeval);
		} break;
case 116:
# line 614 "awk.y"
{ yyval.nodeval = yypvt[-0].nodeval; } break;
case 117:
# line 616 "awk.y"
{ yyval.nodeval = node((NODE *) NULL, Node_nomatch, yypvt[-0].nodeval); } break;
case 118:
# line 618 "awk.y"
{ yyval.nodeval = node (yypvt[-2].nodeval, yypvt[-1].nodetypeval, mk_rexp(yypvt[-0].nodeval)); } break;
case 119:
# line 620 "awk.y"
{ yyval.nodeval = node (variable(yypvt[-0].sval,1), Node_in_array, yypvt[-2].nodeval); } break;
case 120:
# line 622 "awk.y"
{ yyval.nodeval = node (yypvt[-2].nodeval, yypvt[-1].nodetypeval, yypvt[-0].nodeval); } break;
case 121:
# line 624 "awk.y"
{ yyval.nodeval = node(yypvt[-4].nodeval, Node_cond_exp, node(yypvt[-2].nodeval, Node_if_branches, yypvt[-0].nodeval));} break;
case 122:
# line 626 "awk.y"
{ yyval.nodeval = yypvt[-0].nodeval; } break;
case 123:
# line 628 "awk.y"
{ yyval.nodeval = node (yypvt[-1].nodeval, Node_concat, yypvt[-0].nodeval); } break;
case 126:
# line 636 "awk.y"
{ yyval.nodeval = node (yypvt[-2].nodeval, Node_exp, yypvt[-0].nodeval); } break;
case 127:
# line 638 "awk.y"
{ yyval.nodeval = node (yypvt[-2].nodeval, Node_times, yypvt[-0].nodeval); } break;
case 128:
# line 640 "awk.y"
{ yyval.nodeval = node (yypvt[-2].nodeval, Node_quotient, yypvt[-0].nodeval); } break;
case 129:
# line 642 "awk.y"
{ yyval.nodeval = node (yypvt[-2].nodeval, Node_mod, yypvt[-0].nodeval); } break;
case 130:
# line 644 "awk.y"
{ yyval.nodeval = node (yypvt[-2].nodeval, Node_plus, yypvt[-0].nodeval); } break;
case 131:
# line 646 "awk.y"
{ yyval.nodeval = node (yypvt[-2].nodeval, Node_minus, yypvt[-0].nodeval); } break;
case 132:
# line 651 "awk.y"
{ yyval.nodeval = node (yypvt[-0].nodeval, Node_not,(NODE *) NULL); } break;
case 133:
# line 653 "awk.y"
{ yyval.nodeval = yypvt[-1].nodeval; } break;
case 134:
# line 655 "awk.y"
{ 
		if (! io_allowed && strcmp(tokstart, "nextfile") == 0)
			yyerror("nextfile() is illegal in BEGIN and END");
		} break;
case 135:
# line 660 "awk.y"
{ yyval.nodeval = snode (yypvt[-1].nodeval, Node_builtin, (int) yypvt[-4].lval); } break;
case 136:
# line 662 "awk.y"
{ yyval.nodeval = snode (yypvt[-1].nodeval, Node_builtin, (int) yypvt[-3].lval); } break;
case 137:
# line 664 "awk.y"
{
		if (do_lint)
			warning("call of `length' without parentheses is not portable");
		yyval.nodeval = snode ((NODE *)NULL, Node_builtin, (int) yypvt[-0].lval);
		if (do_posix)
			warning( "call of `length' without parentheses is deprecated by POSIX");
	  } break;
case 138:
# line 672 "awk.y"
{
		yyval.nodeval = node (yypvt[-1].nodeval, Node_func_call, make_string(yypvt[-3].sval, strlen(yypvt[-3].sval)));
	  } break;
case 139:
# line 676 "awk.y"
{ yyval.nodeval = node (yypvt[-0].nodeval, Node_preincrement, (NODE *)NULL); } break;
case 140:
# line 678 "awk.y"
{ yyval.nodeval = node (yypvt[-0].nodeval, Node_predecrement, (NODE *)NULL); } break;
case 141:
# line 680 "awk.y"
{ yyval.nodeval = yypvt[-0].nodeval; } break;
case 142:
# line 682 "awk.y"
{ yyval.nodeval = yypvt[-0].nodeval; } break;
case 143:
# line 685 "awk.y"
{ if (yypvt[-0].nodeval->type == Node_val) {
			yypvt[-0].nodeval->numbr = -(force_number(yypvt[-0].nodeval));
			yyval.nodeval = yypvt[-0].nodeval;
		  } else
			yyval.nodeval = node (yypvt[-0].nodeval, Node_unary_minus, (NODE *)NULL);
		} break;
case 144:
# line 692 "awk.y"
{ yyval.nodeval = yypvt[-0].nodeval; } break;
case 145:
# line 697 "awk.y"
{ yyval.nodeval = node (yypvt[-1].nodeval, Node_postincrement, (NODE *)NULL); } break;
case 146:
# line 699 "awk.y"
{ yyval.nodeval = node (yypvt[-1].nodeval, Node_postdecrement, (NODE *)NULL); } break;
case 148:
# line 705 "awk.y"
{ yyval.nodeval = NULL; } break;
case 149:
# line 707 "awk.y"
{ yyval.nodeval = yypvt[-0].nodeval; } break;
case 150:
# line 712 "awk.y"
{ yyval.nodeval = variable(yypvt[-0].sval,1); } break;
case 151:
# line 714 "awk.y"
{
		if (yypvt[-1].nodeval->rnode == NULL) {
			yyval.nodeval = node (variable(yypvt[-3].sval,1), Node_subscript, yypvt[-1].nodeval->lnode);
			freenode(yypvt[-1].nodeval);
		} else
			yyval.nodeval = node (variable(yypvt[-3].sval,1), Node_subscript, yypvt[-1].nodeval);
		} break;
case 152:
# line 722 "awk.y"
{ yyval.nodeval = node (yypvt[-0].nodeval, Node_field_spec, (NODE *)NULL); } break;
case 153:
# line 724 "awk.y"
{ yyval.nodeval = node (yypvt[-0].nodeval, Node_field_spec, (NODE *)NULL); } break;
case 155:
# line 732 "awk.y"
{ yyerrok; } break;
case 156:
# line 736 "awk.y"
{ yyerrok; } break;
case 159:
# line 745 "awk.y"
{ yyerrok; want_assign = 0; } break;
case 160:
# line 748 "awk.y"
{ yyerrok; } break;
	}
	goto yystack;		/* reset registers in driver code */
}
