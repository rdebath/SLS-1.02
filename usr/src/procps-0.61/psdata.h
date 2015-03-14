/*
 * psdata.h
 *
 */

/* #define	DEBUG	1 */


#define	PSDATABASE	"/etc/psdatabase"
#define	PS_MAGIC	"PSDATA.96"
#define	SWAPPATH	"/dev/swap"


struct dbtbl_s {
    off_t		off;		/* offset in psdatabase */
    int			nsym;		/* # symbols */
    int			size;		/* size of array + strings */
};

/*
 * header of psdatabase
 */
struct psdb_hdr {
    char		magic[16];
    char		uts_release[8];
    char		uts_version[8];
    char		sys_path[128];	/* name of system binary */
    char		swap_path[128];	/* name of swap device */
    struct dbtbl_s	vars;		/* bss and data symbols */
    struct dbtbl_s	fncs;		/* list of all functions */
    struct dbtbl_s	ttys;		/* tty names */
};

struct sym_s {
    unsigned long	addr;		/* core address in kernel */
    int			name;		/* offset from strings ptr */
};

struct tbl_s {
    struct sym_s	*tbl;
    int			nsym;
    char		*strings;	/* ptr to start of strings */
};

extern struct tbl_s vars, fncs;
extern struct psdb_hdr db_hdr;
extern char *swappath[];
extern char *ttynames;


#ifdef DEBUG
extern int Debug;
#else
#define Debug 0
#endif

void *xmalloc(unsigned int);
int read_tbl(struct dbtbl_s *dbtbl, struct tbl_s *tbl);

