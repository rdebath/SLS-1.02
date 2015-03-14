/*
 * util.c - C source for GNU CHESS
 *
 * Copyright (c) 1988,1989,1990 John Stanback
 * Copyright (c) 1992 Free Software Foundation
 *
 * This file is part of GNU CHESS.
 *
 * GNU Chess is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * GNU Chess is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GNU Chess; see the file COPYING.  If not, write to
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */
#include "gnuchess.h"
unsigned int TTadd = 0;
short int recycle;
extern char mvstr[4][6];
#ifdef CACHE
extern struct etable etab[2][ETABLE];
#endif

int
parse (FILE * fd, short unsigned int *mv, short int side, char *opening)
{
  register int c, i, r1, r2, c1, c2;
  char s[128];
  char *p;

  while ((c = getc (fd)) == ' ' || c == '\n') ;
  i = 0;
  s[0] = (char) c;
  if (c == '!')
    {
      p = opening;
      do
	{
	  *p++ = c;
	  c = getc (fd);
	  if (c == '\n' || c == EOF)
	    {
	      *p = '\0';
	      return 0;
	    }
      } while (true);
    }
  while (c != '?' && c != ' ' && c != '\t' && c != '\n' && c != EOF)
    s[++i] = (char) (c = getc (fd));
  s[++i] = '\0';
  if (c == EOF)
    return (-1);
  if (s[0] == '!' || s[0] == ';' || i < 3)
    {
      while (c != '\n' && c != EOF)
	c = getc (fd);
      return (0);
    }
  if (s[4] == 'o')
    *mv = ((side == black) ? LONGBLACKCASTLE : LONGWHITECASTLE);
  else if (s[0] == 'o')
    *mv = ((side == black) ? BLACKCASTLE : WHITECASTLE);
  else
    {
      c1 = s[0] - 'a';
      r1 = s[1] - '1';
      c2 = s[2] - 'a';
      r2 = s[3] - '1';
      *mv = (locn (r1, c1) << 8) | locn (r2, c2);
    }
  if (c == '?')
    {				/* Bad move, not for the program to play */
      *mv |= 0x8000;		/* Flag it ! */
      c = getc (fd);
    }
  return (1);
}


#if ttblsz

#define CB(i) (unsigned char) ((color[2 * (i)] ? 0x80 : 0)\
	       | (board[2 * (i)] << 4)\
	       | (color[2 * (i) + 1] ? 0x8 : 0)\
	       | (board[2 * (i) + 1]))

int
ProbeTTable (short int side,
	     short int depth,
	     short int ply,
	     short int *alpha,
	     short int *beta,
	     short int *score)

/*
 * Look for the current board position in the transposition table.
 */

{
  register struct hashentry *ptbl;
  register /*unsigned*/ short i;  /*to match new type of rehash --tpm*/

  ptbl = &ttable[side][hashkey & (ttblsize - 1)];

  /* rehash max rehash times */
  for (i = 0; (ptbl->depth) && (ptbl->hashbd != hashbd) && (i < rehash); i++) ptbl++;
  if ((ptbl->depth) && (ptbl->hashbd == hashbd))
/*     ^^^^^^^^^^^^   we can use some informations of the entry even
                      the depth is not large enough */
    {
#ifdef HASHTEST
      for (i = 0; i < 32; i++)
	{
	  if (ptbl->bd[i] != CB (i))
	    {
#if !defined CHESSTOOL && !defined XBOARD
	      HashCol++;
	      ShowMessage (CP[199]);	/*ttable collision detected*/
#endif
	      break;
	    }
	}
#endif /* HASHTEST */


      PV = SwagHt = ptbl->mv;
      if ((short) ptbl->depth >= depth)
	{
#if !defined CHESSTOOL && !defined XBOARD
          HashCnt++;
#endif
	  if (ptbl->flags & truescore)
	    {
	      *score = ptbl->score;
	      /* adjust *score so moves to mate is from root */
	      if (*score > 9000)
		*score -= ply;
	      else if (*score < -9000)
		*score += ply;
	      *beta = -20000;
	    }
#ifdef notdef			/* Never happens! see search */
	  else if (ptbl->flags & upperbound)
	    {
	      if (ptbl->score < *beta)
		*beta = ptbl->score + 1;
	    }
#endif
	  else if (ptbl->flags & lowerbound)
	    {
	      if (ptbl->score > *alpha)
		*alpha = ptbl->score - 1;
	    }
	  return (true);
	}
    }
  return (false);
}


int
PutInTTable (short int side,
	     short int score,
	     short int depth,
	     short int ply,
	     short int alpha,
	     short int beta,
	     short unsigned int mv)

/*
 * Store the current board position in the transposition table.
 */

{
  register struct hashentry *ptbl;
  register /*unsigned*/ short i;  /*to match new type of rehash --tpm*/

  ptbl = &ttable[side][hashkey & (ttblsize - 1)];

  /* rehash max rehash times */
  for (i = 0; ptbl->depth && ptbl->hashbd != hashbd && i < rehash; i++)
    ptbl++;
  if (i == rehash) {
#if !defined CHESSTOOL && !defined XBOARD
	THashCol++;
#endif
	ptbl -= recycle;}
  if (depth >= (short)ptbl->depth || ptbl->hashbd != hashbd)
    {
#if !defined CHESSTOOL && !defined XBOARD
      TTadd++;
      HashAdd++; 
#endif
      ptbl->hashbd = hashbd;
      ptbl->depth = (unsigned char) depth;
      /* adjust score so moves to mate is from this ply */
      if (score > 9000)
	score += ply;
      else if (score < -9000)
	score -= ply;
      ptbl->score = score;
      ptbl->mv = mv;
#ifdef DEBUG4
      if (debuglevel & 32)
	{
	  algbr (mv >> 8, mv & 0xff, 0);
	  printf ("-add-> d=%d s=%d p=%d a=%d b=%d %s\n", depth, score, ply, alpha, beta, mvstr);
	}
#endif
/*#ifdef notdef
      if (score < alpha)
	ptbl->flags = upperbound;
      else
/*#endif /* 0 */
      if (score > beta)
	{
	  ptbl->flags = lowerbound;
	  score = beta + 1;
	}
      else
	ptbl->flags = truescore;

#ifdef HASHTEST
      for (i = 0; i < 32; i++)
	{
	  ptbl->bd[i] = CB (i);
	}
#endif /* HASHTEST */
      return true;
    }
  return false;
}

   
static struct hashentry *ttagew, *ttageb;

void
ZeroTTable (void)
{
   register struct hashentry *w, *b;
  for(w=ttable[white],b=ttable[black];w<&ttable[white][ttblsize];w++,b++)
   { w->depth = 0; b->depth = 0;}
	ttagew = ttable[white]; ttageb = ttable[black];
#ifdef CACHE
   memset ((char *) etab, 0, sizeof (etab));
#endif
#ifdef notdef
  register unsigned int a;
  for (a = 0; a < ttblsize + (unsigned int)rehash; a++)
    {
      ttable[white][a].depth = 0;
      ttable[black][a].depth = 0;
    }
#endif
    TTadd = 0; 
}

#ifdef HASHFILE
int Fbdcmp(char *a,char *b)
{
	register int i;
	for(i = 0;i<32;i++)
		if(a[i] != b[i])return false;
	return true;
}
int
ProbeFTable (short int side,
	     short int depth,
	     short int ply,
	     short int *alpha,
	     short int *beta,
	     short int *score)

/*
 * Look for the current board position in the persistent transposition table.
 */

{
  register short int i;
  register unsigned long hashix;
  struct fileentry new, t;

  hashix = ((side == white) ? (hashkey & 0xFFFFFFFE) : (hashkey | 1)) & filesz;

  for (i = 0; i < 32; i++)
    new.bd[i] = CB (i);
  new.flags = 0;
  if (Mvboard[kingP[side]] == 0)
    {
      if (Mvboard[qrook[side]] == 0)
	new.flags |= queencastle;
      if (Mvboard[krook[side]] == 0)
	new.flags |= kingcastle;
    }
  for (i = 0; i < frehash; i++)
    {
      fseek (hashfile,
	     sizeof (struct fileentry) * ((hashix + 2 * i) & (filesz)),
	     SEEK_SET);
      fread (&t, sizeof (struct fileentry), 1, hashfile);
      if (!t.depth) break;
       if(!Fbdcmp(t.bd, new.bd)) continue;
      if (((short int) t.depth >= depth) 
	  && (new.flags == (unsigned short)(t.flags & (kingcastle | queencastle))))
	{
#if !defined CHESSTOOL && !defined XBOARD
	  FHashCnt++;
#endif
	  PV = (t.f << 8) | t.t;
	  *score = (t.sh << 8) | t.sl;
	  /* adjust *score so moves to mate is from root */
	  if (*score > 9000)
	    *score -= ply;
	  else if (*score < -9000)
	    *score += ply;
	  if (t.flags & truescore)
	    {
	      *beta = -20000;
	    }
	  else if (t.flags & lowerbound)
	    {
	      if (*score > *alpha)
		*alpha = *score - 1;
	    }
	  else if (t.flags & upperbound)
	    {
	      if (*score < *beta)
		*beta = *score + 1;
	    }
	  return (true);
	}
    }
  return (false);
}

void
PutInFTable (short int side,
	     short int score,
	     short int depth,
	     short int ply,
	     short int alpha,
	     short int beta,
	     short unsigned int f,
	     short unsigned int t)

/*
 * Store the current board position in the persistent transposition table.
 */

{
  register unsigned short i;
  register unsigned long hashix;
  struct fileentry new, tmp;

  hashix = ((side == white) ? (hashkey & 0xFFFFFFFE) : (hashkey | 1)) & filesz;
  for (i = 0; i < 32; i++) new.bd[i] = CB (i);
  new.f = (unsigned char) f;
  new.t = (unsigned char) t;
  if (score < alpha)
    new.flags = upperbound;
  else
    new.flags = ((score > beta) ? lowerbound : truescore);
  if (Mvboard[kingP[side]] == 0)
    {
      if (Mvboard[qrook[side]] == 0)
	new.flags |= queencastle;
      if (Mvboard[krook[side]] == 0)
	new.flags |= kingcastle;
    }
  new.depth = (unsigned char) depth;
  /* adjust *score so moves to mate is from root */
  if (score > 9000)
    score += ply;
  else if (score < -9000)
    score -= ply;


  new.sh = (unsigned char) (score >> 8);
  new.sl = (unsigned char) (score & 0xFF);

  for (i = 0; i < frehash; i++)
    {
      fseek (hashfile,
	     sizeof (struct fileentry) * ((hashix + 2 * i) & (filesz)),
	     SEEK_SET);
      if(fread (&tmp, sizeof (struct fileentry), 1, hashfile) == NULL){perror("hashfile");exit(1);}
      if (tmp.depth && !Fbdcmp(tmp.bd,new.bd))continue;
      if(tmp.depth == depth)break;
      if (!tmp.depth || (short) tmp.depth < depth)
	{
	  fseek (hashfile,
		 sizeof (struct fileentry) * ((hashix + 2 * i) & (filesz)),
		 SEEK_SET);
	  fwrite (&new, sizeof (struct fileentry), 1, hashfile);
#if !defined CHESSTOOL && !defined XBOARD
          FHashAdd++;
#endif
	  break;
	}
    }
}

#endif /* HASHFILE */
#endif /* ttblsz */

void
ZeroRPT (void)
{
#ifdef NOMEMSET
  register int side, i;
  for (side = white; side <= black; side++)
    for (i = 0; i < 256;)
      rpthash[side][i++] = 0;
#else
   memset ((char *) rpthash, 0, sizeof (rpthash));
#endif
}
