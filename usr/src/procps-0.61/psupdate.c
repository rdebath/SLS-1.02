/*
 * update_db.c	- create/update psdatabase
 *
 * Copyright (c) 1992 Branko Lankester
 *
 * munged into psupdate.c by Michael K. Johnson for the procps suite.
 */
#include <stdio.h>
#include <stdlib.h>
#include <a.out.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <sys/utsname.h>
#include "psdata.h"
#include "ps.h"

#define	SYS_PATH	"/usr/src/linux/tools/system"


int update_psdb(char *syspath);
int read_nlist(char *systemfile);
int write_tbl(int fd, struct dbtbl_s *dbtbl, struct tbl_s *tbl);
void make_fnctbl(void);
void make_vartbl(void);
__compar_fn_t addrcmp(struct sym_s *p1, struct sym_s *p2);
__compar_fn_t varcmp(struct sym_s *p1, struct sym_s *p2);
unsigned long k_addr(char *sym);

static struct nlist *namelist;
static int nsym;
static char *strings;
static int stringsize;

int main(int argc, char **argv) {

  char nsyspath[128];

  if(argc == 1)
    update_psdb(SYS_PATH);
  if(argc == 2) {
    sscanf(argv[1], "%s", nsyspath);
    update_psdb(nsyspath);
  }
  return 0;
}


/*
 * Open systemfile, if update is non zero the psdatabase will be
 * updated from sysfile. If sysfile is NULL the system file that
 * was used to create the existing database will be used for
 * updating.
 */

int open_sys(char *sysfile, int update)
{

    if (sysfile == NULL || *sysfile == '\0') {
	if (open_psdb() == -1) {
#if 0
	    if (!update)
		return -1;
#else
	    read_nlist(SYS_PATH);
#endif
	    sysfile = SYS_PATH;
	} else
	    sysfile = db_hdr.sys_path;
    } else
	read_nlist(sysfile);

    if (update)
	update_psdb(sysfile);

    return(0);
}


int update_psdb(char *syspath)
{
    int fd, sysfd;
    struct utsname uts;

    if (namelist == NULL)
	read_nlist(syspath);
    
    close_psdb();

    if ((fd = open(PSDATABASE, O_RDWR|O_TRUNC|O_CREAT, 0666)) == -1) {
	perror(PSDATABASE);
	exit(1);
    }
    if (*syspath != '/') {
	memset(db_hdr.sys_path, 0, sizeof db_hdr.sys_path);
	if (getcwd(db_hdr.sys_path, sizeof db_hdr.sys_path - 2) != NULL)
	    strcat(db_hdr.sys_path, "/");
	strncat(db_hdr.sys_path, syspath, sizeof db_hdr.sys_path -
				strlen(db_hdr.sys_path) - 1);
    } else
	strncpy(db_hdr.sys_path, syspath, sizeof db_hdr.sys_path);
    strncpy(db_hdr.magic, PS_MAGIC, sizeof db_hdr.magic);

/*    strncpy(db_hdr.swap_path, swappath[0], sizeof db_hdr.swap_path);*/
    strncpy(db_hdr.swap_path, "/dev/swap", sizeof db_hdr.swap_path);


    make_vartbl();
    make_fnctbl();

    write(fd, (char *) &db_hdr, sizeof db_hdr);
    write_tbl(fd, &db_hdr.vars, &vars);
    write_tbl(fd, &db_hdr.fncs, &fncs);

    if ((sysfd = open(syspath, O_RDONLY)) == -1) {
	perror(syspath);
	exit(1);
    }
    lseek(sysfd, k_addr("_system_utsname") + 1024, SEEK_SET);
    read(sysfd, (char *) &uts, sizeof(struct utsname));
    close(sysfd);
    strncpy(db_hdr.uts_release, uts.release, sizeof db_hdr.uts_release);
    strncpy(db_hdr.uts_version, uts.version, sizeof db_hdr.uts_version);

    lseek(fd, 0L, SEEK_SET);
    write(fd, (char *) &db_hdr, sizeof db_hdr);
    close(fd);

    free(namelist);
    namelist = NULL;
    return(0);
}

int read_nlist(char *systemfile)
{
    int fd;
    struct exec hdr;
    unsigned symsize, size;

    if ((fd = open(systemfile, O_RDONLY)) < 0) {
	perror(systemfile);
	exit(1);
    }
    if (read(fd, (char *) &hdr, sizeof(hdr)) != sizeof(hdr)) {
	perror(systemfile);
	exit(1);
    }
    if (N_BADMAG(hdr)) {
	fprintf(stderr, "%s: bad magic number\n", systemfile);
	exit(1);
    }
    if (N_STROFF(hdr) == 0) {
	fprintf(stderr, "%s has no symbols\n", systemfile);
	exit(1);
    }
    lseek(fd, N_STROFF(hdr), SEEK_SET);
    read(fd, (char *) &stringsize, sizeof(stringsize));
    symsize = N_STROFF(hdr) - N_SYMOFF(hdr);
    size = symsize + stringsize;
    namelist = (struct nlist *) xmalloc(size);
    lseek(fd, N_SYMOFF(hdr), SEEK_SET);
    if (read(fd, (char *) namelist, size) != size) {
	perror(systemfile);
	exit(1);
    }
    close(fd);

    strings = ((char *) namelist) + symsize;
    nsym = symsize / sizeof(struct nlist);
    if (Debug > 1)
	fprintf(stderr, "read %d symbols from %s\n", nsym, systemfile);
    return(0);
}


/*
 * make list of all text symbols, sorted on address for easy
 * lookup of wait channel.
 */
void make_fnctbl(void)
{
    int i;
    struct sym_s *fp;

    fp= fncs.tbl= (struct sym_s *) xmalloc(nsym * sizeof(struct sym_s));
    fncs.strings = strings;
    for (i = 0; i < nsym; ++i) {
	if ((namelist[i].n_type & ~N_EXT) == N_TEXT && 
		!strchr(strings + namelist[i].n_un.n_strx, '.')) {
	    fp->addr = namelist[i].n_value;
	    fp->name = namelist[i].n_un.n_strx;
	    ++fp;
	}
    }
    fncs.nsym = fp - fncs.tbl;

    if (Debug > 1)
	fprintf(stderr, "%d text symbols\n", fncs.nsym);
    qsort(fncs.tbl, fncs.nsym, sizeof(struct sym_s), (__compar_fn_t) addrcmp);
    if (Debug > 1)
	for (i = 1; i<fncs.nsym; ++i)
	    if (fncs.tbl[i].addr == fncs.tbl[i-1].addr)
		printf("text symbols %s and %s both have address %x\n",
		    	strings + fncs.tbl[i-1].name,
			strings + fncs.tbl[i].name, fncs.tbl[i].addr);
}


void make_vartbl(void)
{
    int i;
    struct sym_s *vp;

    vp= vars.tbl= (struct sym_s *) xmalloc(nsym * sizeof(struct sym_s));
    vars.strings = strings;
    for (i = 0; i < nsym; ++i) {
	int typ = namelist[i].n_type & ~N_EXT;

	if (typ == N_DATA || typ == N_BSS) {
	    vp->addr = namelist[i].n_value;
	    vp->name = namelist[i].n_un.n_strx;
	    ++vp;
	}
    }
    vars.nsym = vp - vars.tbl;

    if (Debug > 1)
	fprintf(stderr, "%d data/bss symbols\n", vars.nsym);

    qsort(vars.tbl, vars.nsym, sizeof(struct sym_s), (__compar_fn_t) varcmp);
}


/*
 * write table tbl to descriptor fd, header structure dbtdl is updated
 */
int write_tbl(int fd, struct dbtbl_s *dbtbl, struct tbl_s *tbl)
{
    int i;
    struct sym_s *p;
    char *s;
    int strsize, symsize;

    dbtbl->off = lseek(fd, 0L, SEEK_CUR);
    s= tbl->strings= xmalloc(stringsize);
    for (i = tbl->nsym, p = tbl->tbl; i--; ) {
	strcpy(s, strings + p->name);
	p->name = s - tbl->strings;
	++p;
	s += strlen(s) + 1;
    }
    symsize = tbl->nsym * sizeof(struct sym_s);
    if (write(fd, (char *) tbl->tbl, symsize) != symsize)
	return -1;
    strsize = (s - tbl->strings + 3) & ~3;
    if (write(fd, tbl->strings, strsize) != strsize)
	return -1;
    
    dbtbl->size = strsize + symsize;
    dbtbl->nsym = tbl->nsym;

    return(0);
}

/*
 * fncs are sorted on address
 */
__compar_fn_t addrcmp(struct sym_s *p1, struct sym_s *p2)
{
    return((__compar_fn_t)(p1->addr - p2->addr));
}



/*
 * vars are sorted on name
 */
__compar_fn_t varcmp(struct sym_s *p1, struct sym_s *p2)
{
    return((__compar_fn_t)strcmp(vars.strings + p1->name,
				 vars.strings + p2->name));
}



/*
 * get address of data symbol
 */
unsigned long k_addr(char *sym)
{
    struct sym_s *p, key;

    if (vars.tbl == NULL)
	    read_tbl(&db_hdr.vars, &vars);

    key.name = sym - vars.strings;
    p = (struct sym_s *) bsearch(&key, vars.tbl, vars.nsym,
				sizeof(struct sym_s), (__compar_fn_t) varcmp);
    return(p ? p->addr : -1);
}


