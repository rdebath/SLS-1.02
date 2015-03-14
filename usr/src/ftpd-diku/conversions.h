#define	T_REG		1		/* regular files OK */
#define	T_DIR		2		/* directories OK */
#define	T_ASCII		4		/* ASCII transfers OK */

struct	convert {
		char	*stripfix;		/* postfix to strip from real file */
		char	*postfix;		/* postfix to add to real file */
		char	*external_cmd;	/* command to do conversion */
		int		types;			/* types: {file,directory} OK to convert */
		int		options;		/* for logging: which conversion(s) used */
		char	*name;			/* description of conversion */
};

/* cannot have both a stripfix and a postfix for a single entry in cvtlist
 * right now */

static struct	convert	cvtlist[] = {
	".Z", NULL, "/bin/compress -d -c %s", T_REG|T_ASCII, O_UNCOMPRESS,
		"UNCOMPRESS",

	"-z", NULL, "/bin/compress -d -c %s", T_REG|T_ASCII, O_UNCOMPRESS,
		"UNCOMPRESS",

	NULL, ".Z", "/bin/compress -c %s", T_REG, O_COMPRESS,
		"COMPRESS",

	NULL, ".tar", "/bin/tar -c -f - %s", T_REG|T_DIR, O_TAR,
		"TAR",

	NULL, ".tar.Z", "/bin/tar -c -Z -f - %s", T_REG|T_DIR, O_COMPRESS|O_TAR,
		"TAR+COMPRESS",

/* add new conversions here */
	NULL, NULL, NULL, NULL, NULL, NULL,
};

struct	convert	*cvtptr = cvtlist;
