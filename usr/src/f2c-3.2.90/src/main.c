/****************************************************************
Copyright 1990 by AT&T Bell Laboratories and Bellcore.

Permission to use, copy, modify, and distribute this software
and its documentation for any purpose and without fee is hereby
granted, provided that the above copyright notice appear in all
copies and that both that the copyright notice and this
permission notice and warranty disclaimer appear in supporting
documentation, and that the names of AT&T Bell Laboratories or
Bellcore or any of their entities not be used in advertising or
publicity pertaining to distribution of the software without
specific, written prior permission.

AT&T and Bellcore disclaim all warranties with regard to this
software, including all implied warranties of merchantability
and fitness.  In no event shall AT&T or Bellcore be liable for
any special, indirect or consequential damages or any damages
whatsoever resulting from loss of use, data or profits, whether
in an action of contract, negligence or other tortious action,
arising out of or in connection with the use or performance of
this software.
****************************************************************/

extern char F2C_version[];

#include "defs.h"
#include "parse.h"
#include "usignal.h"

int complex_seen, dcomplex_seen;

LOCAL int Max_ftn_files;

char **ftn_files;
int current_ftn_file = 0;

flag ftn66flag = NO;
flag nowarnflag = NO;
flag noextflag = NO;
flag  no66flag = NO;		/* Must also set noextflag to this
					   same value */
flag zflag = YES;		/* recognize double complex intrinsics */
flag debugflag = NO;
flag onetripflag = NO;
flag shiftcase = YES;
flag undeftype = NO;
flag checksubs = NO;
flag r8flag = NO;
int tyreal = TYREAL;
extern void r8fix();

int maxregvar = MAXREGVAR;	/* if maxregvar > MAXREGVAR, error */
int maxequiv = MAXEQUIV;
int maxext = MAXEXT;
int maxstno = MAXSTNO;
int maxctl = MAXCTL;
int maxhash = MAXHASH;
int extcomm, ext1comm, useauto;
int can_include = YES;	/* so we can disable includes for netlib */

static char *def_i2 = "";

static int useshortints = NO;	/* YES => tyint = TYSHORT */
static int uselongints = NO;	/* YES => tyint = TYLONG */
int addftnsrc = NO;		/* Include ftn source in output */
int usedefsforcommon = NO;	/* Use #defines for common reference */
int forcedouble = YES;		/* force real functions to double */
int Ansi = NO;
int define_equivs = YES;
int tyioint = TYLONG;
int szleng = SZLENG;
int inqmask = M(TYLONG)|M(TYLOGICAL);
int wordalign = NO;
static int skipC, skipversion;
char *filename0, *parens;
int Castargs = 1;
static int typedefs = 0;
int chars_per_wd, gflag, protostatus;
int infertypes = 1;
char used_rets[TYSUBR+1];

static char *tmpdir = "/tmp";

#define f2c_entry(swit,count,type,store,size) \
	p_entry ("-", swit, 0, count, type, store, size)

arg_info table[] = {
    f2c_entry ("w66", P_NO_ARGS, P_INT, &ftn66flag, YES),
    f2c_entry ("w", P_NO_ARGS, P_INT, &nowarnflag, YES),
    f2c_entry ("66", P_NO_ARGS, P_INT, &no66flag, YES),
    f2c_entry ("d", P_ONE_ARG, P_INT, &debugflag, YES),
    f2c_entry ("1", P_NO_ARGS, P_INT, &onetripflag, YES),
    f2c_entry ("onetrip", P_NO_ARGS, P_INT, &onetripflag, YES),
    f2c_entry ("I2", P_NO_ARGS, P_INT, &useshortints, YES),
    f2c_entry ("I4", P_NO_ARGS, P_INT, &uselongints, YES),
    f2c_entry ("U", P_NO_ARGS, P_INT, &shiftcase, NO),
    f2c_entry ("u", P_NO_ARGS, P_INT, &undeftype, YES),
    f2c_entry ("O", P_ONE_ARG, P_INT, &maxregvar, 0),
    f2c_entry ("C", P_NO_ARGS, P_INT, &checksubs, YES),
    f2c_entry ("Nq", P_ONE_ARG, P_INT, &maxequiv, 0),
    f2c_entry ("Nx", P_ONE_ARG, P_INT, &maxext, 0),
    f2c_entry ("Ns", P_ONE_ARG, P_INT, &maxstno, 0),
    f2c_entry ("Nc", P_ONE_ARG, P_INT, &maxctl, 0),
    f2c_entry ("Nn", P_ONE_ARG, P_INT, &maxhash, 0),
    f2c_entry ("c", P_NO_ARGS, P_INT, &addftnsrc, YES),
    f2c_entry ("p", P_NO_ARGS, P_INT, &usedefsforcommon, YES),
    f2c_entry ("R", P_NO_ARGS, P_INT, &forcedouble, NO),
    f2c_entry ("!R", P_NO_ARGS, P_INT, &forcedouble, YES),
    f2c_entry ("A", P_NO_ARGS, P_INT, &Ansi, YES),
    f2c_entry ("ext", P_NO_ARGS, P_INT, &noextflag, YES),
    f2c_entry ("z", P_NO_ARGS, P_INT, &zflag, NO),
    f2c_entry ("a", P_NO_ARGS, P_INT, &useauto, YES),
    f2c_entry ("r8", P_NO_ARGS, P_INT, &r8flag, YES),
    f2c_entry ("i2", P_NO_ARGS, P_INT, &tyioint, NO),
    f2c_entry ("w8", P_NO_ARGS, P_INT, &wordalign, YES),
    f2c_entry ("!I", P_NO_ARGS, P_INT, &can_include, NO),
    f2c_entry ("W", P_ONE_ARG, P_INT, &chars_per_wd, 0),
    f2c_entry ("g", P_NO_ARGS, P_INT, &gflag, YES),
    f2c_entry ("T", P_ONE_ARG, P_STRING, &tmpdir, 0),
    f2c_entry ("E", P_NO_ARGS, P_INT, &extcomm, 1),
    f2c_entry ("e1c", P_NO_ARGS, P_INT, &ext1comm, 1),
    f2c_entry ("ec", P_NO_ARGS, P_INT, &ext1comm, 2),
    f2c_entry ("C++", P_NO_ARGS, P_INT, &Ansi, 2),
    f2c_entry ("P", P_NO_ARGS, P_INT, &Castargs, 3),
    f2c_entry ("Ps", P_NO_ARGS, P_INT, &protostatus, 1),
    f2c_entry ("!P", P_NO_ARGS, P_INT, &Castargs, 0),
    f2c_entry ("!c", P_NO_ARGS, P_INT, &skipC, 1),
    f2c_entry ("!it", P_NO_ARGS, P_INT, &infertypes, 0),

	/* options omitted from man pages */

	/* -ev ==> implement equivalence with initialized pointers */
    f2c_entry ("ev", P_NO_ARGS, P_INT, &define_equivs, NO),

	/* -!it used to be the default when -it was more agressive */

    f2c_entry ("it", P_NO_ARGS, P_INT, &infertypes, 1),

	/* -Pd is similar to -P, but omits :ref: lines */
    f2c_entry ("Pd", P_NO_ARGS, P_INT, &Castargs, 2),

	/* -t ==> emit typedefs (under -A or -C++) for procedure
		argument types used.  This is meant for netlib's
		f2c service, so -A and -C++ will work with older
		versions of f2c.h
		*/
    f2c_entry ("t", P_NO_ARGS, P_INT, &typedefs, 1),

	/* -!V ==> omit version msg (to facilitate using diff in
		regression testing)
		*/
    f2c_entry ("!V", P_NO_ARGS, P_INT, &skipversion, 1)

}; /* table */

char *c_functions	= "c_functions";
char *coutput		= "c_output";
char *initfname		= "raw_data";
char *blkdfname		= "block_data";
char *p1_file		= "p1_file";
char *p1_filebak	= "p1_file.BAK";
char *sortfname		= "init_file";
char *protofname	= "proto_file";
FILE *protofile;

void list_init_data();

 static void
set_tmp_names()
{
	int k, pid;
	if (debugflag == 1)
		return;
	pid = getpid();
	k = strlen(tmpdir) + 16;
	c_functions = (char *)ckalloc(6*k);
	initfname = c_functions + k;
	blkdfname = initfname + k;
	p1_file = blkdfname + k;
	p1_filebak = p1_file + k;
	sortfname = p1_filebak + k;
	sprintf(c_functions, "%s/f2c%d_func", tmpdir, pid);
	/* keep initfname short enough to append .b in backup_filename */
	sprintf(initfname, "%s/f2c%d_rd", tmpdir, pid);
	sprintf(blkdfname, "%s/f2c%d_blkd", tmpdir, pid);
	sprintf(p1_file, "%s/f2c%d_p1f", tmpdir, pid);
	sprintf(p1_filebak, "%s/f2c%d_p1fb", tmpdir, pid);
	sprintf(sortfname, "%s/f2c%d_sort", tmpdir, pid);
	if (debugflag)
		fprintf(diagfile, "%s %s %s %s %s %s\n", c_functions,
			initfname, blkdfname, p1_file, p1_filebak, sortfname);
	}

 static char *
c_name(s,ft)char *s;
{
	char *b, *s0;
	int c;

	b = s0 = s;
	while(c = *s++)
		if (c == '/')
			b = s;
	if (--s < s0 + 3 || s[-2] != '.'
			 || ((c = *--s) != 'f' && c != 'F')) {
		infname = s0;
		Fatal("file name must end in .f or .F");
		}
	*s = ft;
	b = copys(b);
	*s = c;
	return b;
	}

set_externs ()
{
/* Adjust the global flags according to the command line parameters */

    if (chars_per_wd > 0) {
	typesize[TYADDR] = typesize[TYLONG] = typesize[TYREAL] =
		typesize[TYLOGICAL] = chars_per_wd;
	typesize[TYDREAL] = typesize[TYCOMPLEX] = chars_per_wd << 1;
	typesize[TYDCOMPLEX] = chars_per_wd << 2;
	typesize[TYSHORT] = chars_per_wd >> 1;
	typesize[TYCILIST] = 5*chars_per_wd;
	typesize[TYICILIST] = 6*chars_per_wd;
	typesize[TYOLIST] = 9*chars_per_wd;
	typesize[TYCLLIST] = 3*chars_per_wd;
	typesize[TYALIST] = 2*chars_per_wd;
	typesize[TYINLIST] = 26*chars_per_wd;
	}

    if (wordalign)
	typealign[TYDREAL] = typealign[TYDCOMPLEX] = typealign[TYREAL];
    if (!tyioint) {
	tyioint = TYSHORT;
	szleng = typesize[TYSHORT];
	def_i2 = "#define f2c_i2 1\n";
	inqmask = M(TYSHORT)|M(TYLOGICAL);
	goto checklong;
	}
    else
	szleng = typesize[TYLONG];
    if (useshortints) {
	inqmask = M(TYLONG);
 checklong:
	protorettypes[TYLOGICAL] = typename[TYLOGICAL] = "shortlogical";
	typesize[TYLOGICAL] = typesize[TYSHORT];
	casttypes[TYLOGICAL] = "K_fp";
	if (uselongints)
	    err ("Can't use both long and short ints");
	else
	    tyint = tylogical = TYSHORT;
	}
    else if (uselongints)
	tyint = TYLONG;

    if (no66flag)
	noextflag = no66flag;
    if (noextflag)
	zflag = 0;

    if (r8flag) {
	tyreal = TYDREAL;
	r8fix();
	}
    if (forcedouble) {
	protorettypes[TYREAL] = "E_f";
	casttypes[TYREAL] = "E_fp";
	}

    if (maxregvar > MAXREGVAR) {
	warni("-O%d: too many register variables", maxregvar);
	maxregvar = MAXREGVAR;
    } /* if maxregvar > MAXREGVAR */

/* Check the list of input files */

    {
	int bad, i, cur_max = Max_ftn_files;

	for (i = bad = 0; i < cur_max && ftn_files[i]; i++)
	    if (ftn_files[i][0] == '-') {
		errstr ("Invalid flag '%s'", ftn_files[i]);
		bad++;
		}
	if (bad)
		exit(1);

    } /* block */
} /* set_externs */

 void
unlinkall(cdelete)
{
	if (!debugflag) {
		unlink(c_functions);
		unlink(initfname);
		unlink(p1_file);
		unlink(sortfname);
		unlink(blkdfname);
		if (cdelete && coutput)
			unlink(coutput);
		}
	}

 static void
killed()
{
	signal(SIGINT, SIG_IGN);
#ifdef SIGQUIT
	signal(SIGQUIT, SIG_IGN);
#endif
#ifdef SIGHUP
	signal(SIGHUP, SIG_IGN);
#endif
	signal(SIGTERM, SIG_IGN);
	unlinkall(1);
	exit(126);
	}

 static void
flovflo()
{
	Fatal("floating exception during constant evaluation; cannot recover");
	/* vax returns a reserved operand that generates
	   an illegal operand fault on next instruction,
	   which if ignored causes an infinite loop.
	*/
	signal(SIGFPE, flovflo);
}


 static void
sig1catch(sig) int sig;
{
	if (signal(sig, SIG_IGN) != SIG_IGN)
		signal(sig, killed);
	}

 static void
sigcatch()
{
	sig1catch(SIGINT);
#ifdef SIGQUIT
	sig1catch(SIGQUIT);
#endif
#ifdef SIGHUP
	sig1catch(SIGHUP);
#endif
	sig1catch(SIGTERM);
	signal(SIGFPE, flovflo);  /* catch overflows */
	}

 static int
comm2dcl()
{
	struct Extsym *ext;
	if (ext1comm)
		for(ext = extsymtab; ext < nextext; ext++)
			if (ext->extstg == STGCOMMON && !ext->extinit)
				return ext1comm;
	return 0;
	}

 static void
write_typedefs(outfile)
 FILE *outfile;
{
	register int i;
	register char *s, *p = 0;
	static char st[4] = { TYREAL, TYCOMPLEX, TYDCOMPLEX, TYCHAR };
	static char stl[4] = { 'E', 'C', 'Z', 'H' };

	for(i = 0; i <= TYSUBR; i++)
		if (s = usedcasts[i]) {
			if (!p) {
				p = Ansi == 1 ? "()" : "(...)";
				nice_printf(outfile,
				"/* Types for casting procedure arguments: */\
\n\n#ifndef F2C_proc_par_types\n");
				if (i == 0) {
					nice_printf(outfile,
			"typedef int /* Unknown procedure type */ (*%s)%s;\n",
						 s, p);
					continue;
					}
				}
			nice_printf(outfile, "typedef %s (*%s)%s;\n",
					c_type_decl(i,1), s, p);
			}
	for(i = !forcedouble; i < 4; i++)
		if (used_rets[st[i]])
			nice_printf(outfile,
				"typedef %s %c_f; /* %s function */\n",
				p = i ? "VOID" : "doublereal",
				stl[i], ftn_types[st[i]]);
	if (p)
		nice_printf(outfile, "#endif\n\n");
	}

 static void
commonprotos(outfile)
 register FILE *outfile;
{
	register struct Extsym *e, *ee;
	register Argtypes *at;
	Atype *a, *ae;
	int k;
	extern int proc_protochanges;

	if (!outfile)
		return;
	for (e = extsymtab, ee = nextext; e < ee; e++)
		if (e->extstg == STGCOMMON && e->allextp)
			nice_printf(outfile, "/* comlen %s %ld */\n",
				e->cextname, e->maxleng);
	if (Castargs < 3)
		return;

	/* -Pr: special comments conveying current knowledge
	    of external references */

	k = proc_protochanges;
	for (e = extsymtab, ee = nextext; e < ee; e++)
		if (e->extstg == STGEXT
		&& e->cextname != e->fextname)	/* not a library function */
		    if (at = e->arginfo) {
			if ((!e->extinit || at->changes & 1)
				/* not defined here or
					changed since definition */
			&& at->nargs >= 0) {
				nice_printf(outfile, "/*:ref: %s %d %d",
					e->cextname, e->extype, at->nargs);
				a = at->atypes;
				for(ae = a + at->nargs; a < ae; a++)
					nice_printf(outfile, " %d", a->type);
				nice_printf(outfile, " */\n");
				if (at->changes & 1)
					k++;
				}
			}
		    else if (e->extype)
			/* typyed external, never invoked */
			nice_printf(outfile, "/*:ref: %s %d :*/\n",
				e->cextname, e->extype);
	if (k) {
		nice_printf(outfile,
	"/* Rerunning f2c -P may change prototypes or declarations. */\n");
		if (nerr)
			return;
		if (protostatus)
			done(4);
		if (protofile != stdout) {
			fprintf(diagfile,
	"Rerunning \"f2c -P ... %s %s\" may change prototypes or declarations.\n",
				filename0, protofname);
			fflush(diagfile);
			}
		}
	}

 static int retcode = 0;

main(argc, argv)
int argc;
char **argv;
{
	int c2d, k, pid, status, w;
	FILE *c_output;
	char *filename, *cdfilename;
	static char stderrbuf[BUFSIZ];
	extern void define_commons();
	extern char **dfltproc, *dflt1proc[];

	setbuf(stderr, stderrbuf);	/* arrange for fast error msgs */

	Max_ftn_files = argc - 1;
	ftn_files = (char **)ckalloc((argc+1)*sizeof(char *));

	parse_args (argc, argv, table, sizeof(table)/sizeof(arg_info),
		ftn_files, Max_ftn_files);
	if (!can_include && ext1comm == 2)
		ext1comm = 1;
	if (ext1comm && !extcomm)
		extcomm = 2;
	if (protostatus)
		Castargs = 3;
	else if (Castargs == 1 && !Ansi)
		Castargs = 0;
	if (Castargs >= 2 && !Ansi)
		Ansi = 1;

	if (!Ansi)
		parens = "()";
	else if (!Castargs)
		parens = Ansi == 1 ? "()" : "(...)";
	else
		dfltproc = dflt1proc;

	set_externs();
	fileinit();
	read_Pfiles(ftn_files);

	for(k = 1; ftn_files[k]; k++) {
		if (!(pid = fork()))
			break;
		while((w = wait(&status)) != pid)
			if (w == -1)
				Fatal("bad wait code");
		retcode |= status >> 8;
		}
	filename0 = filename = ftn_files[current_ftn_file = k - 1];

	set_tmp_names();
	sigcatch();

	c_file   = opf(c_functions);
	pass1_file=opf(p1_file);
	initkey();
	if (filename && *filename) {
		if (debugflag != 1) {
			coutput = c_name(filename,'c');
			if (Castargs >= 2)
				protofname = c_name(filename,'P');
			}
		cdfilename = coutput;
		if (Castargs >= 2 && !(protofile = fopen(protofname, "w")))
			fatalstr("Can't open %.84s\n", protofname);
		}
	else {
		filename = "";
		cdfilename = "f2c_out.c";
		c_output = stdout;
		coutput = 0;
		if (Castargs >= 2) {
			protofile = stdout;
			if (!skipC)
				printf("#ifdef P_R_O_T_O_T_Y_P_E_S\n");
			}
		}

	if(inilex( copys(filename) ))
		done(1);
	if (filename0) {
		fprintf(diagfile, "%s:\n", filename);
		fflush(diagfile);
		}

	procinit();
	if(k = yyparse())
	{
		fprintf(diagfile, "Bad parse, return code %d\n", k);
		done(1);
	}

	commonprotos(protofile);
	if (protofile == stdout && !skipC)
		printf("#endif\n\n");

	if(nerr)
		done(1);

	if (skipC)
		goto C_skipped;

	if (filename0
	&& (c_output = fopen (coutput, "w")) == (FILE *) NULL)
		fatalstr("can't open %.86s");


/* Write out the declarations which are global to this file */

	if ((c2d = comm2dcl()) == 1)
		nice_printf(c_output, "/*>>>'/dev/null'<<<*/\n\n\
/* Split this into several files by piping it through\n\n\
sed \"s/^\\/\\*>>>'\\(.*\\)'<<<\\*\\/\\$/cat >'\\1' <<'\\/*<<<\\1>>>*\\/'/\" | /bin/sh\n\
 */\n\
/*<<</dev/null>>>*/\n\
/*>>>'%s'<<<*/\n", cdfilename);
	if (!skipversion) {
		nice_printf (c_output, "/* %s -- translated by f2c ", filename);
		nice_printf (c_output, "(version of %s).\n", F2C_version);
		nice_printf (c_output,
	"   You must link the resulting object file with the libraries:\n\
	-lF77 -lI77 -lm -lc   (in that order)\n*/\n\n");
		}
	if (Ansi == 2)
		nice_printf(c_output,
			"#ifdef __cplusplus\nextern \"C\" {\n#endif\n");
	nice_printf (c_output, "%s#include \"f2c.h\"\n\n", def_i2);
	if (Castargs && typedefs)
		write_typedefs(c_output);
	nice_printf (c_file, "\n");
	fclose (c_file);
	c_file = c_output;		/* HACK to get the next indenting
					   to work */
	write_common_decls (c_output);
	if (blkdfile)
		list_init_data(&blkdfile, blkdfname, c_output);
	write_globals (c_output);
	if ((c_file = fopen (c_functions, "r")) == (FILE *) NULL)
	    Fatal("main - couldn't reopen c_functions");
	ffilecopy (c_file, c_output);
	if (*main_alias) {
	    nice_printf (c_output, "/* Main program alias */ ");
	    nice_printf (c_output, "int %s () { MAIN__ (); }\n",
		    main_alias);
	    }
	if (Ansi == 2)
		nice_printf(c_output,
			"#ifdef __cplusplus\n\t}\n#endif\n");
	if (c2d) {
		if (c2d == 1)
			fprintf(c_output, "/*<<<%s>>>*/\n", cdfilename);
		else
			fclose(c_output);
		define_commons(c_output);
		}
	if (c2d != 2)
		fclose (c_output);

 C_skipped:
	if(parstate != OUTSIDE)
		{
		warn("missing END statement");
		endproc();
		}
	done(nerr ? 1 : 0);
}


FILEP opf(fn)
char *fn;
{
	FILEP fp;
	if( fp = fopen(fn, "w") )
		return(fp);

	fatalstr("cannot open intermediate file %s", fn);
	/* NOTREACHED */ return 0;
}


clf(p)
FILEP *p;
{
	if(p!=NULL && *p!=NULL && *p!=stdout)
	{
		if(ferror(*p))
			Fatal("writing error");
		fclose(*p);
	}
	*p = NULL;
}


done(k)
int k;
{
	clf(&initfile);
	clf(&c_file);
	clf(&pass1_file);
	unlinkall(k);
	exit(k|retcode);
}
