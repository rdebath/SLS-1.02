/*
 *      (c) Copyright 1989 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ident	"@(#)mem.c	26.9	91/09/14 SMI"

/*
 * Safe memory allocation/free routines - front-ends the C library functions
 *
 */

#include <malloc.h>
#include <memory.h>
#include <stdio.h>
#include <sys/types.h>

#include "i18n.h"

#ifdef MEMDEBUG
#include "st.h"

#define MemAlloc d_MemAlloc
#define MemFree  d_MemFree
#define MemRealloc d_MemRealloc
#define MemCalloc d_MemCalloc

int         MemAcct;
int         AcctTag;

static st_table *memHashTable;

typedef struct {
    unsigned int s;
    int         a;
    int         l;
    char       *f;
    char       *k;	
}           Mem;

static int
memCompare(g1, g2)
    char       *g1, *g2;
{
    return (g1 - g2);
}


static int
memHash(g1, modulus)
    int         g1;
    int         modulus;
{
    return g1 % modulus;
}

static void
insertAcctInfo(p, s, f, l, k)
    void *p;
    unsigned int s;
    char *f;
    int l;
    char *k;
{
    if (MemAcct) {
	Mem        *m;
	MemAcct = 0;	/*prevent nasty recursion*/

	if (memHashTable == NULL)
	    memHashTable = st_init_table(memCompare, memHash);

	m = (Mem *) malloc(sizeof(Mem));
	m->s = s;
	m->f = f;
	m->l = l;
	m->a = AcctTag;
	m->k = k;
	st_insert(memHashTable, p, m);

	MemAcct = 1;
    }
}


static enum st_retval
dodump(key, rec)
    void       *key;
    Mem        *rec;
{
    char extra[255];

    if (rec->k == NULL)
	strcpy(extra, "\n");
    else {
	if (strcmp(rec->k, "(string)") == 0)
	    sprintf(extra, ":\"%s\"\n", key);
	else 
	    sprintf(extra, ":%s\n", rec->k);
    }

    fprintf(stderr, "extant: %d) 0x%.8x, (f:%s, l:%d) (%d bytes)%s",
	    rec->a, key, rec->f, rec->l, rec->s, extra);
    return ST_DELETE;
}

void
DumpExtant()
{
    if (memHashTable)
	st_foreach(memHashTable, dodump, NULL);
    else
	fprintf(stderr, "no accounting info availiable!\n");
}

#endif

void       *
MemAlloc(sz
#ifdef MEMDEBUG
 , f, l, k
#endif
)
    unsigned int sz;
#ifdef MEMDEBUG
    char *f;
    int l;
    char *k;
#endif
{
    void       *p;

    if ((p = malloc(sz)) == NULL)
	ErrorGeneral(gettext("Memory allocation failure."));

    memset((char *) p, 0, (int) sz);

#ifdef MEMDEBUG
    insertAcctInfo(p, sz, f, l, k);
#endif

    return p;
}

void       *
MemCalloc(num, sz
#ifdef MEMDEBUG
   , f, l
#endif
)
    unsigned int num;
    unsigned int sz;
#ifdef MEMDEBUG
    char *f;
    int l;
#endif
{
    void       *p;

    if ((p = calloc(num, sz)) == NULL)
	ErrorGeneral(gettext("Memory array allocation failure."));

    memset((char *) p, 0, (int) sz * (int) num);

#ifdef MEMDEBUG
    insertAcctInfo(p, sz * num, f, l, NULL);
#endif

    return p;
}

void       *
MemRealloc(p, sz)
    void       *p;
    unsigned int sz;
{
    void       *t;

    if ((t = realloc(p, sz)) == NULL)
	ErrorGeneral(gettext("Memory array allocation failure."));

#ifdef MEMDEBUG
    if (MemAcct && memHashTable) {
	Mem        *oldM;
	MemAcct = 0;
	(void) st_delete(memHashTable, &p, (char *) &oldM);
	oldM->s = sz;
	st_insert(memHashTable, t, oldM);
	MemAcct = 1;
    }
#endif

    return t;
}


void
MemFree(p)
    void       *p;
{
    if (p != NULL) {
#ifdef MEMDEBUG
	if (MemAcct && memHashTable) {
	    Mem        *oldM;
	    MemAcct = 0;
	    (void) st_delete(memHashTable, &p, (char *) &oldM);
	    free(oldM);
	    MemAcct = 1;
	}
#endif
	free(p);
    }
}
