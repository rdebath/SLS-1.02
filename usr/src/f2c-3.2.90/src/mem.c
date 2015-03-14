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

#include <stdio.h>
#include "iob.h"
#include "string.h"

#define MEMBSIZE	32000
#define GMEMBSIZE	16000

 extern char *Malloc();

 char *
gmem(n, round)
 int n, round;
{
	static char *last, *next;
	char *rv;
	if (round)
		next =
#ifdef CRAY
		    (long)next & 0xe000000000000000
			? (char *)(((long)next & 0x1fffffffffffffff) + 1)
			: next;
#else
			 (char *)(((long)next + sizeof(char *)-1)
				& ~(sizeof(char *)-1));
#endif
	rv = next;
	if ((next += n) > last) {
		rv = Malloc(n + GMEMBSIZE);

		next = rv + n;
		last = next + GMEMBSIZE;
		}
	return rv;
	}

 struct memblock {
	struct memblock *next;
	char buf[MEMBSIZE];
	};
 typedef struct memblock memblock;

 static memblock mem0;
 memblock *curmemblock = &mem0, *firstmemblock = &mem0;

 char	*mem_first = mem0.buf,
	*mem_next  = mem0.buf,
	*mem_last  = mem0.buf + sizeof(mem0.buf),
	*mem0_last = mem0.buf + sizeof(mem0.buf);

 char *
mem(n, round)
 int n, round;
{
	memblock *b;
	register char *rv, *s;

	if (round)
		mem_next =
#ifdef CRAY
		    (long)mem_next & 0xe000000000000000
			? (char *)(((long)mem_next & 0x1fffffffffffffff) + 1)
			: mem_next;
#else
			 (char *)(((long)mem_next + sizeof(char *)-1)
				& ~(sizeof(char *)-1));
#endif
	rv = mem_next;
	s = rv + n;
	if (s >= mem_last) {
		if (n > sizeof(mem0.buf))  {
			fprintf(stderr, "mem(%d) failure!\n", n);
			exit(1);
			}
		if (!(b = curmemblock->next)) {
			b = (memblock *)Malloc(sizeof(memblock));
			curmemblock->next = b;
			b->next = 0;
			}
		curmemblock = b;
		rv = b->buf;
		mem_last = rv + sizeof(b->buf);
		s = rv + n;
		}
	mem_next = s;
	return rv;
	}

 char *
tostring(s,n)
 register char *s;
 int n;
{
	register char *s1, *se, **sf;
	extern char *str_fmt[];
	char *rv, *s0, *s2;
	register int k = n + 2, t;

	sf = str_fmt;
	sf['%'] = "%";
	s0 = s;
	se = s + n;
	for(; s < se; s++) {
		t = *(unsigned char *)s;
		s1 = sf[t < 127 ? t : 127];
		while(*++s1)
			k++;
		}
	rv = s1 = mem(k,0);
	*s1++ = '"';
	for(s = s0; s < se; s++) {
		t = *(unsigned char *)s;
		if (t < 127)
			for(s2 = sf[t]; *s1 = *s2++; s1++);
		else {
			sprintf(s1, sf[127], t);
			s1 += strlen(s1);
			}
		}
	*s1 = 0;
	sf['%'] = "%%";
	return rv;
	}

 char *
cpstring(s)
 register char *s;
{
	return strcpy(mem(strlen(s)+1,0), s);
	}

 void
new_iob_data(ios, name)
 register io_setup *ios;
 char *name;
{
	register iob_data *iod;
	register char **s, **se;

	iod = (iob_data *)
		mem(sizeof(iob_data) + ios->nelt*sizeof(char *), 1);
	iod->next = iob_list;
	iob_list = iod;
	iod->type = ios->fields[0];
	iod->name = cpstring(name);
	s = iod->fields;
	se = s + ios->nelt;
	while(s < se)
		*s++ = "0";
	*s = 0;
	}

 char *
string_num(pfx, n)
 char *pfx;
 long n;
{
	char buf[32];
	sprintf(buf, "%s%ld", pfx, n);
	/* can't trust return type of sprintf -- BSD gets it wrong */
	return strcpy(mem(strlen(buf)+1,0), buf);
	}

static defines *define_list;

 void
define_start(outfile, s1, s2, post)
 FILE *outfile;
 char *s1, *s2, *post;
{
	defines *d;
	int n, n1;

	n = n1 = strlen(s1);
	if (s2)
		n += strlen(s2);
	d = (defines *)mem(sizeof(defines)+n, 1);
	d->next = define_list;
	define_list = d;
	strcpy(d->defname, s1);
	if (s2)
		strcpy(d->defname + n1, s2);
	nice_printf(outfile, "#define %s %s", d->defname, post);
	}

 void
other_undefs(outfile)
 FILE *outfile;
{
	defines *d;
	if (d = define_list) {
		define_list = 0;
		nice_printf(outfile, "\n");
		do
			nice_printf(outfile, "#undef %s\n", d->defname);
			while(d = d->next);
		nice_printf(outfile, "\n");
		}
	}
