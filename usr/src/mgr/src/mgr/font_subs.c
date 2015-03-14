/*{{{}}}*/
/*{{{  Notes*/
/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
*/

/* font routines */
/*}}}  */

/*{{{  #includes*/
#include <limits.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "bitblit.h"
#include "font.h"

#include "defs.h"
#include "default_font.h"

#include "cut.h"
#include "font_subs.h"
/*}}}  */

/*{{{  glyph_create*/
static void glyph_create(font,glyph,offset) struct font *font; BITMAP **glyph; int offset;
{
  register int i, x;
  int first = font->head.start;
  int last = font->head.start + font->head.count;
  int wide = font->head.wide;
  int high = font->head.high;
  int nochar;

  /* Pick the character to be printed for characters not in the set.
     Normally, it is the character specified by C_NOCHAR, but it that isn't
     in the range of the set, we pick the first character (which is usually
     a space).
  */
  nochar = C_NOCHAR - font->head.start;
  if( nochar >= last ) nochar = 0;
  nochar = nochar*wide + offset;

  x = offset;
  for(i=0; i<MAXGLYPHS; i++) if (i < first || i >= last)
  glyph[i] = bit_create(font->data, nochar, 0, wide, high);
  else 
  {
    glyph[i] = bit_create(font->data, x, 0, wide, high);
    x += wide;
  }
}
/*}}}  */
/*{{{  open_sfont -- set up a static font file*/
static struct font *open_sfont(head,data) struct font_header head; BITMAP *data;
{
  struct font *font;

  if ((font=malloc(sizeof(struct font))) == (struct font *)0)
  return((struct font *)0);

  font->head = head;
  font->data = data;
  /* hack! */
  font->head.type = FONT_S;
  font->table = (struct entry **) 0;

  /* create individual characters */

  font->glyph = (BITMAP **) malloc(sizeof(BITMAP *) * MAXGLYPHS);
  glyph_create(font,font->glyph,0);

  return(font);
}
/*}}}  */
/*{{{  font_purge -- look for and remove all references to a particular font */
static int font_purge(gone) register struct font *gone;	/* invalid font pointer */
   {
   register WINDOW *win, *win2;
   register int count=0;

   /* re-reference current window font */

   for(win=active;win != (WINDOW *) 0;win=win->next) {
      if (W(font) == gone) {
         W(font) = font;
         count++;
         }

      /* now re-reference any stacked fonts */

      for(win2=W(stack);win2 != (WINDOW *) 0;win2=win2->stack)
         if (win2->font == gone) {
            win2->font = font;
            count++;
            }
      }
   return(count);
   }
/*}}}  */

/*{{{  open_font -- set up a font file*/
struct font *open_font(file) char *file;
{
  FILE *fp;
  int size;
  struct font *font;

  if (file == (char *) 0 || *file == '\0') 
  {
    return(open_sfont(default_font_head,&default_font));
  }

#ifdef DEBUG
  dprintf(f)(stderr,"Opening font file [%s]\n",file);
#endif

  if ((fp=fopen(file,"rb"))  == NULL) return((struct font *)0);

  if ((font=(struct font *) malloc(sizeof(struct font))) == (struct font *)0) 
  {
    fclose(fp);
    return((struct font *)0);
  }

  if (fread(&(font->head),sizeof(font->head),1,fp) != 1)
  {
    free((char *) font);
    fclose(fp);
    return((struct font *)0);
  }

  if (font->head.type != FONT_A)
  {
    free((char *) font);
    fclose(fp);
    return((struct font *)0);
  }
                               
  /* fonts are always 32 bit aligned */

  size = (font->head.wide*font->head.count+31)&~31;

  font->data = bit_alloc(size,font->head.high,NULL_DATA,1);
  font->table = (struct entry **) 0;

  /* read in font data */

  size = bit_size(BIT_HIGH(font->data),BIT_WIDE(font->data),BIT_DEPTH(font->data));
  fread(BIT_DATA(font->data), size, 1, fp);
  SET_DIRTY(font->data);
  /* create individual characters */

  font->glyph = malloc(sizeof(BITMAP *) * MAXGLYPHS);
  glyph_create(font,font->glyph,0);

  fclose(fp);
  return(font);
}
/*}}}  */
/*{{{  free_font -- deallocate a font*/
void free_font(dead_font) struct font *dead_font;
{
  register int i;
  int count; /* # of glyphs to trash */

  if (!dead_font) return;

         count = dead_font->head.type & 0x80 ? 4*MAXGLYPHS : MAXGLYPHS;
         dead_font->head.type &= ~0x80;


  for(i=0;i<count;i++) if (dead_font->glyph[i]) bit_destroy(dead_font->glyph[i]);
  if (dead_font->head.type != FONT_S) bit_destroy(dead_font->data);
  free(dead_font->glyph);
  zap_fhash(dead_font);		/* free up hash table space */
  i=font_purge(dead_font);	/* eliminate references to dead font */

# ifdef DEBUG
  dprintf(f)(stderr,"freeing font %d (%d references)\n",dead_font->ident,i);
# endif

  free(dead_font);
}
/*}}}  */
/*{{{  enhance_font -- add bold face and underlining to a font*/
int enhance_font(font)struct font *font;                    /* font to be enhanced */
{
  BITMAP *data;                           /* new bitmap data */
  BITMAP **glyph;                 /* new font glyphs */
  int size;                                       /* current font size */
  register int i;

  if (font->head.type&0x80)            /* already enhanced */
    return(0);

  /* make data structures larger, copy existing data */

   glyph = (BITMAP **) malloc(sizeof(BITMAP *) * MAXGLYPHS * 4);
       for(i=0;i<MAXGLYPHS;i++)
               bit_destroy(font->glyph[i]);
       free(font->glyph);
       font->glyph = glyph;

       size = BIT_WIDE(font->data);
    data = bit_alloc(size*4,font->head.high,NULL_DATA,1);
    bit_blit(data,0,0,size,font->head.high,BIT_SRC,font->data,0,0);

       if (font->head.type == FONT_S)               /* watch out for static font data */
               font->head.type = FONT_A;
       else
               bit_destroy(font->data);
       font->data = data;

/* make extra font glyph pointers */

       for(i=0;i<4;i++)
               glyph_create(font,font->glyph + i*MAXGLYPHS,i*size);

       /* make font "wider"; tack "enhancements" on to the right */

    bit_blit(data,size,0,size,font->head.high,BIT_SRC,font->data,0,0); /* copy font */
       for(i=0;i<MAXGLYPHS;i++)                        /* under line it */
               if (i != ' ')
                       bit_blit(font->glyph[MAXGLYPHS+i],0,0,font->head.wide,font->head.high,
                                               BIT_SRC|BIT_DST, font->glyph['_'],0,0);

       /* now for bold and bold_underline */

    bit_blit(data,2*size,0,2*size,font->head.high,BIT_SRC,font->data,0,0);
           for(i=0;i<MAXGLYPHS*2;i++)                      /* embolden */
                           bit_blit(font->glyph[2*MAXGLYPHS+i],1,0,font->head.wide,font->head.high,
                                                   BIT_SRC|BIT_DST, font->glyph[i],0,0);

           /* mark font "expanded" */

       font->head.type |= 0x80;
       return(1);
}
/*}}}  */
