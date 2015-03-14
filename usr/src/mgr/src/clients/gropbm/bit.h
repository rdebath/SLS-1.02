#ifndef _BIT_H
#define _BIT_H

#include <stdio.h>

int bitmalloc(int width, int height);
void bitfree(void);
void bitclear(void);
void bitline(unsigned int x1, unsigned int y1, unsigned int x2, unsigned int y2);
void bitellipse(int x0, int y0, int rx, int ry);
int bitpbmwrite(FILE *fp);

#ifdef MGR
int bitmgrwrite(FILE *fp);
#endif

#endif
/*{{{}}}*/
