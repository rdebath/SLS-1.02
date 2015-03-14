/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

#define H_SIZE	1999		/* size of hash table for cut/paste */

struct entry {
   unsigned char value;		/* character at this location */
   unsigned char type;          /* type: bold or underline */
   struct entry *next;		/* pntr to next char */
   };

#define	MAXGLYPHS	256

struct font {
   struct font_header head;	/* font header */
   BITMAP *data;		/* all the glyphs strung together */
   BITMAP **glyph;              /* pointers to individual glyphs */
   short ident;			/* font id */
   struct entry **table;	/* pointer to hash table for cut/paste */
   };

#ifdef __STDC__
struct font *open_font(char *file);
void free_font(struct font *dead_font);
#else
extern struct font *open_font();
extern void free_font();
#endif
