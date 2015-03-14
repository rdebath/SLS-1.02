#include "cscore.h"				/*			CSCORE.C	*/


 struct evlist *
lcreat(nslots)                  /* allocate array of event pointer slots */
 int nslots;
{
	struct evlist *a;

	a = (struct evlist *) calloc((unsigned)(nslots+2),sizeof(int));
	a->nslots = nslots;
	return(a);
}


 struct event *
createv(pcnt)                   /* create new event space */
 int pcnt;
{
	struct event *e;

	e = (struct event *) calloc((unsigned)(pcnt+2),sizeof(float));
	e->pcnt = pcnt;
	return(e);
}


 struct event *
copyev(e)                               /* make a new copy of an event */
 struct event *e;
{
	struct event *f;
	int  n;
	float *p, *q;

	n = e->pcnt;
	f = createv(n);
	f->op = e->op;
	p = &e->p[1];
	q = &f->p[1];
	while (n--)
		*q++ = *p++;
	return(f);
}


 struct event *
defev(s)                        /* define an event from string arg */
 char *s;
{
	struct event *e;
	float *p, *q;

	while (*s == ' ')
		s++;
	buf.op = *s++;				/* read opcode */
	while (*s == ' ')
		s++;
        p = &buf.p[1];
	q = &buf.p[PMAX];
	while (sscanf(s,"%f",p++) > 0) {		/* read pfields */
		while (*s >= '0' && *s <= '9' || *s == '.' || *s == '-')
			s++;
		while (*s == ' ')
			s++;
		if (p > q && *s != '\0')  {		/* too many ? */
			p++;
                        printf("PMAX exceeded, string event truncated.\n");
			break;
		}
	}
        buf.pcnt = p - &buf.p[1] - 1;   /* count of params recvd */
	e = copyev(&buf);               /* copy event to a new space */
	return(e);
}


 struct event *
getev()                         /* read an event from stdin score file */
{
	struct event *e;
	float *p, *q, junk;
	char c;
                     
	if (scanf("%1s",&buf.op) != EOF)  {	 /* if opcode found */
		while (buf.op == ';') {
			while ((c = getchar()) != '\n' && c != EOF)
				;
			if (scanf("%1s",&buf.op) == EOF)
				return(NULL);
		}
                p = &buf.p[1];
		q = &buf.p[PMAX];
		while (scanf("%f",p++) > 0) {	    /* read pfields into buf */
		    if (p > q && scanf("%f",&junk) > 0) {      /* too many ? */
			printf("PMAX exceeded, input event truncated.\n");
			while (scanf("%f",&junk) > 0)         /* flush extra */
				;
		    }
		}
		buf.pcnt = p - &buf.p[1] - 1;	/* count of params recvd */
		e = copyev(&buf);		/* copy event to a new space */
		return(e);
	}
	else
		return(NULL);
}


putev(e)                                /* put event to stdout */
 struct event *e;
{
	int  pcnt;
	float *q;

	printf("%c",e->op);
	q = &e->p[1];
	pcnt = e->pcnt;
	while (pcnt--)
		printf(" %g",*q++);
	printf("\n");
}


putstr(s)				/* put string to stdout */
 char *s;
{
	printf("%s\n",s);
}


lcount(a)			/* count entries in event list */
 struct evlist *a;
{
	struct event **p;
	int  n, nrem;

	n = 0;	nrem = a->nslots;
	p = &a->e[1];
	while ((nrem--) && *p++ != NULL)
		n++;
	return(n);
}


 struct evlist *
lappev(a,e)                     /* append an event to a list */
 struct evlist *a;
 struct event *e;
{
	struct event **p, **q;
	struct evlist *b;
	int  n, slots;

	n = lcount(a);
	slots = a->nslots;
	if (slots > n) {
		p = &a->e[n+1];
		*p++ = e;
		if (slots > n+1)
			*p = NULL;
	}
        else {
		b = lcreat(n + 5);
		p = &a->e[1];
                q = &b->e[1];
		while (n--)
			*q++ = *p++;
		*q++ = e;
		*q = NULL;
		cfree(a);
		a = b;
	}
	return(a);
}
         

 struct evlist *
lget()                          /* get section events from stdin scorefile */
{
	struct event *e;
	struct evlist *a;

	a = lcreat(5);
	while ((e = getev()) != NULL) {
		if (e->op == 's' || e->op == 'e')
			break;
		a = lappev(a,e);
        }
	return(a);
}


lput(a) 			/* put listed events to stdout */
 struct evlist *a;
{
	struct event **p;
	int  n;

	n = a->nslots;
	p = &a->e[1];
	while ((n--) && *p != NULL)
		putev(*p++);
}


 struct evlist *
lcopy(a)
 struct evlist *a;
{
	struct evlist *b;
	struct event **p, **q;
	int  n;

	n = lcount(a);
	b = lcreat(n);
	p = &a->e[1];
	q = &b->e[1];
	while (n--)
		*q++ = *p++;
	return(b);
}


 struct evlist *
lcopyev(a)
 struct evlist *a;
{
	struct evlist *b;
	struct event  **p, **q;
	int  n;

        n = lcount(a);
	b = lcreat(n);
	p = &a->e[1];
	q = &b->e[1];
	while (n--)
		*q++ = copyev(*p++);
	return(b);
}


 struct evlist *
lcat(a,b)
 struct evlist *a, *b;
{
	struct event **p, **q;
	int i, j, n;

	i = lcount(a);
	j = lcount(b);
	if (i + j > a->nslots) {
		struct evlist *c;

		c = lcreat(i+j);
		p = &a->e[1];
		q = &c->e[1];
		n = i;
		while (n--)
			*q++ = *p++;
		cfree(a);
		a = c;
	}
	else if (i + j < a->nslots)
		a->e[i+j+1] = NULL;
	p = &a->e[i+1];
	q = &b->e[1];
	while (j--)
		*p++ = *q++;
	return(a);
}


lsort(a)			/* evlist pointers into chronological order */
 struct evlist *a;
{
	struct event **p, **q, *e, *f;
	int  n, gap, i, j;

	n = lcount(a);
	e = a->e[n];
	if (e->op == 's' || e->op == 'e')
		--n;
	for (gap = n/2;  gap > 0;  gap /=2)
	    for (i = gap;  i < n;  i++)
		for (j = i-gap;  j >= 0;  j -= gap) {
		    p = &a->e[j+1];     e = *p;
		    q = &a->e[j+1+gap]; f = *q;
		    if (e->p[2] < f->p[2])
			break;
		    if (e->p[2] == f->p[2]) {
			if (e->op == f->op) {
			    if (e->op == 'f')
				break;
			    if (e->p[1] < f->p[1])
				break;
			    if (e->p[1] == f->p[1])
				if (e->p[3] <= f->p[3])
				    break;
			}
			else if (e->op < f->op)
			    break;
		    }
                    *p = f;  *q = e;         
		}
}


 struct evlist *
lxins(a,s)                      /* list extract by instr numbers */
 struct evlist *a;
 char *s;
{
	int  x[5], xcnt, xn, *xp, insno, n;
	struct event **p, **q, *e;
	struct evlist *b, *c;

	xcnt = sscanf(s,"%d%d%d%d%d",&x[0],&x[1],&x[2],&x[3],&x[4]);
	n = lcount(a);
	b = lcreat(n);
	p = &a->e[1];
	q = &b->e[1];
	while ((n--) && (e = *p++) != NULL) {
		if (e->op != 'i')
			*q++ = e;
		else {
			insno = e->p[1];
			xn = xcnt;  xp = x;
			while (xn--)
				if (*xp++ == insno) {
					*q++ = e;
					break;
				}
                }    
	}
	c = lcopy(b);
	cfree(b);
        return(c);
}


 struct evlist *
lxtimev(a,from,to)                      /* list extract by time */
 struct evlist *a;
 float from, to;
{
	struct event **p, **q, *e;
	struct evlist *b, *c;
	float maxp3;
	int  n;

	n = lcount(a);
	b = lcreat(n);
	p = &a->e[1];
	q = &b->e[1];
	maxp3 = to - from;
	while ((n--) && (e = *p++) != NULL)
		switch (e->op) {
		case 'f':
			if (e->p[2] < to) {
				*q++ = e = copyev(e);
				if (e->p[2] <= from)
					e->p[2] = 0;
				else
					e->p[2] -= from;
			}
			break;
		case 'i':
			if (e->p[2] < from) {
				if (e->p[2] + e->p[3] > from) {
					*q++ = e = copyev(e);
					e->p[3] -= from - e->p[2];
					e->p[2] = 0;
					if (e->p[3] > maxp3)
						e->p[3] = maxp3;
				}
			}
                        else if (e->p[2] < to) {
				*q++ = e = copyev(e);
				if (e->p[2] + e->p[3] > to)
					e->p[3] = to - e->p[2];
				e->p[2] -= from;
			}
			break;
		default:
			*q++ = copyev(e);
			break;
		}
	c = lcopy(b);
	cfree(b);
	return(c);
}


 struct evlist *
lsepf(a)                        /* separate f events from evlist */
 struct evlist *a;
{
	struct event **p, **q, **r, **s;
	struct evlist *b, *c;
	int  n;

	n = lcount(a);
	b = lcreat(n);
	p = q = &a->e[1];
	r = s = &b->e[1];
	while (n--) {
		if ((*p)->op == 'f')
			*r++ = *p++;
		else
			*q++ = *p++;
	}
	if (r > s)
		*q = NULL;
	c = lcopy(b);
	cfree(b);
	return(c);
}


lstripes(a)				/* strip evlist of s or e event */
 struct evlist *a;
{
	struct event **p;
	int  n;
	char op;

	n = lcount(a);
        p = &a->e[1];
	while (n--) {
		op = (*p++)->op;
		if (op == 's' || op == 'e') {
			*--p = NULL;
			break;
		}
	}
}


relev(e)                        /* give back event space */
 struct event *e;
{
	cfree(e);
}


lrel(a) 			/* give back list space */
 struct evlist *a;
{
	cfree(a);
}


lrelev(a)			/* give back list and its event spaces */
 struct evlist *a;
{
	struct event **p;
	int  n;

	n = lcount(a);
	p = &a->e[1];
	while (n--)
		cfree(*p++);
	cfree(a);
}
