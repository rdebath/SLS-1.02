/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

/* macros for determining clip rectangle for background updates */

typedef struct {		/* clipping rectangle */
  int x1,y1,x2,y2;
  } rect;

#ifdef DEBUG
#define SHOW_CLIP() \
	bit_blit(screen,W(x0)+SUM_BDR+clip.x1,W(y0)+SUM_BDR+clip.y1, \
	clip.x2-clip.x1,clip.y2-clip.y1, \
	BIT_NOT(BIT_DST),NULL,0,0);
#define SHOW() \
	if (debug && index(debug_level,'c'))  \
		{ SHOW_CLIP();getchar();SHOW_CLIP(); }
#else
#define SHOW()
#endif

#define Do_clip() \
	(W(flags)&W_BACKGROUND && !(W(flags)&W_ACTIVE))

#define Set_all() { \
	clip.x1 = 0, clip.y1=0; \
	clip.x2 = BIT_WIDE(W(border)); \
	clip.y2 = BIT_HIGH(W(border)); \
	SHOW(); \
        }

#define Set_clipall() { \
	clip.x2 = 0, clip.y2=0; \
	clip.x1 = BIT_WIDE(W(border)); \
	clip.y1 = BIT_HIGH(W(border)); \
        }

#define Set_clip(X1,Y1,X2,Y2) { \
	clip.x1 = Min(clip.x1,X1); \
	clip.y1 = Min(clip.y1,Y1); \
	clip.x2 = Max(clip.x2,X2); \
	clip.y2 = Max(clip.y2,Y2); \
	SHOW(); \
	}

#define Set_cliplow(X1,Y1) { \
	clip.x1 = Min(clip.x1,X1); \
	clip.y1 = Min(clip.y1,Y1); \
	SHOW(); \
	}

#define Set_cliphigh(X2,Y2) { \
	clip.x2 = Max(clip.x2,X2); \
	clip.y2 = Max(clip.y2,Y2); \
	SHOW(); \
	}

extern rect clip;
