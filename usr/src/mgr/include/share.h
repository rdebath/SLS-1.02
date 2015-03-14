#ifdef MOVIE
#include <stdio.h>

#include "bitmap.h"

/* types */

#define OP_MASK		0xF		/* opcode part of type */
#define TYPE_MASK	0xF0

#define T_NOP		0x00		/* do a nop */
#define T_BLIT		0x10		/* do a bit-blt */
#define T_WRITE		0x20		/* do a bit-blt */
#define T_LINE		0x30		/* do a line */
#define T_POINT		0x40		/* do a point */
#define T_DATA		0x50		/* send some data */
#define T_KILL		0x60		/* destroy a bitmap */
#define T_SCREEN	0x70		/* define the screen */
#define T_TIME		0x80		/* current time (100'th of a second) */
#define T_BLIT2		0x90		/* compressed bit-blit for chars */
#define T_MARK		0xa0      	/* network marks */

struct share_msg
{
	unsigned short type;		/* message type */
	unsigned short stuff[8];	/* other stuff */
};

/* stuff for getting bitmap id's */

#define MAX_MAPS	4000		/* max # bitmap id's */

extern int log_noinitial;
#ifdef __STDC__
void log_blit(BITMAP *dst_map, int xd, int yd,int w, int h, int op, BITMAP *src_map, int xs, int ys);
void log_line(BITMAP *dst, int x0, int y0, int x1, int y1, int op);
void log_point(BITMAP *dst, int x, int y, int op);
void log_open(BITMAP *bp);
void log_destroy(BITMAP *bitmap);
void log_alloc(BITMAP *bp);
void log_create(BITMAP *bp);
void send_sync(void);
void log_time(void);
int log_start(FILE *logfile);
int log_end(void);
#else
extern void log_blit();
extern void log_line();
extern void log_point();
extern void log_open();
extern void log_destroy();
extern void log_alloc();
extern void log_create();
extern void send_sync();
extern void log_time();
extern int log_start();
extern int log_end();
#endif

#endif
