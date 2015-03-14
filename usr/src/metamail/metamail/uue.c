#include <stdio.h>

/*
 * hack to metamail to decode uuencoded bodyparts
 * Written by Keith Moore, February 1992
 */

uueget (ptr, outfp, n)
char *ptr;
FILE *outfp;
{
    unsigned char c1, c2, c3;
    unsigned char p0, p1, p2, p3;

    p0 = (ptr[0] - ' ') & 0x3F;
    p1 = (ptr[1] - ' ') & 0x3F;
    p2 = (ptr[2] - ' ') & 0x3F;
    p3 = (ptr[3] - ' ') & 0x3F;
    
    c1 = p0 << 2 | p1 >> 4;
    c2 = p1 << 4 | p2 >> 2;
    c3 = p2 << 6 | p3;

    if (n >= 1)
	putc (c1, outfp);
    if (n >= 2)
	putc (c2, outfp);
    if (n >= 3)
	putc (c3, outfp);
}


getline (buf, size, fp)
char *buf;
int size;
FILE *fp;
{
    int c;
    char *ptr = buf;

    for (c = 0; c < size; ++c)
	buf[c] = ' ';
    do {
	c = getc (fp);
	if (c == EOF) {
	    *ptr = '\0';
	    return (ptr == buf) ? -1 : 0;
	}
	else if (c == '\n' || c == '\r') {
	    *ptr = '\0';
	    return 0;
	}
	else if (ptr == buf && c == '>') /* ">From" line hack */
	    continue;
	else if (size > 0) {
	    *ptr++ = c;
	    size--;
	}
    } while (1);
    return(0); /* shut lint up */
}


fromuue (infp, outfp, boundaries, ctptr)
FILE *infp, *outfp;
char **boundaries;
int *ctptr;
{
    char buf[63];

    while (1) {
	if (getline (buf, sizeof buf, infp) < 0) {
	    fprintf (stderr, "Premature EOF!\n");
	    return;
	}
	if (strncmp (buf, "begin", 5) == 0)
	    break;
	else if (buf[0] == '-' && buf[1] == '-') {
	    if (boundaries && PendingBoundary (buf, boundaries, ctptr))
		return;
	}
    }	
    while (1) {
	if (getline (buf, sizeof buf, infp) < 0) {
	    fprintf (stderr, "Premature EOF!\n");
	    return;
	}
	else if (strncmp (buf, "end", 5) == 0)
	    break;
	else if (buf[0] == '-' && buf[1] == '-') {
	    if (boundaries && PendingBoundary (buf, boundaries, ctptr)) {
		fprintf (stderr, "premature end of x-uue body part\n");
		return;
	    }
	    else {
		fprintf (stderr, "ignoring invalid boundary marker\n");
		continue;
	    }
	}
	else {
	    int length = (*buf - ' ');
	    if (*buf == '`')
		length = 0;
	    if (length < 0 || length > 63) {
		fprintf (stderr, "fromuue: illegal length (%d)\n",
			 length);
	    }
	    else if (length == 0)
		break;
	    else {
		char *ptr = buf + 1;
		while (length > 0) {
		    uueget (ptr, outfp, length);
		    length -= 3;
		    ptr += 4;
		}
	    }
	}
    }
}

