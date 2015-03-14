#ifndef FNM_PATHNAME
/* fnmatch function */
#define FNM_PATHNAME    0x01    /* match pathnames, not filenames */
#ifndef _POSIX_SOURCE
#define FNM_QUOTE       0x02    /* escape special chars with \ */
#endif
#endif

#define LOGIN   0
#define CWD     1
#define BANNER  2

#define ALIGN(x)        ((x) + (sizeof(long) - (x) % sizeof(long)))

#define O_COMPRESS              (1 << 0)        /* file was compressed */
#define O_UNCOMPRESS            (1 << 1)        /* file was uncompressed */
#define O_TAR                   (1 << 2)        /* file was tar'ed */

#define MAXARGS         10
#define MAXKWLEN        20

struct aclmember {
        struct  aclmember       *next;
        char                            keyword[MAXKWLEN];
        char                            *arg[MAXARGS];
};

#define MAXUSERS        256

#define ARG0    entry->arg[0]
#define ARG1    entry->arg[1]
#define ARG2    entry->arg[2]
#define ARG3    entry->arg[3]
#define ARG4    entry->arg[4]
#define ARG5    entry->arg[5]
#define ARG6    entry->arg[6]
#define ARG7    entry->arg[7]
#define ARG8    entry->arg[8]
#define ARG9    entry->arg[9]

#ifdef linux
#define seteuid(x)	setreuid(-1, (x))
#define setegid(x)	setregid(-1, (x))
#define d_namlen	d_reclen
#endif
