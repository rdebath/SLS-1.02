/* -*- Mode:Text -*- */
/*
 * hash.c - a simple hash function for ispell
 *
 * Pace Willisson, 1983
 */

hash (s, n, hashsize)
register char *s;
register n;
register hashsize;
{
	register short h = 0;

	while (n--) {
		h ^= *s++;
		if (h < 0) {
			h <<= 1;
			h++;
		} else {
			h <<= 1;
		}
	}

	h &= 077777;
	return (unsigned long) h % hashsize;
}
