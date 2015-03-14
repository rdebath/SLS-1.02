/* $Id: Sets.c,v 1.10 1992/05/05 13:19:05 grosch rel $ */

/* $Log: Sets.c,v $
 * Revision 1.10  1992/05/05  13:19:05  grosch
 * added rcsid
 *
 * Revision 1.9  1992/03/30  09:53:37  grosch
 * fixed bug in IsSubset: removed operator !
 *
 * Revision 1.8  1992/02/06  09:29:54  grosch
 * fixed bug: stdio and ANSI C
 *
 * Revision 1.7  1992/01/31  16:31:44  grosch
 * adaption to ANSI C
 *
 * Revision 1.6  1991/11/21  14:28:16  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.5  91/07/17  17:23:36  grosch
 * introduced ARGS trick for ANSI compatibility
 * 
 * Revision 1.4  90/09/20  09:12:25  grosch
 * calmed down lint
 * 
 * Revision 1.3  90/07/04  14:34:02  grosch
 * introduced conditional include
 * 
 * Revision 1.2  89/12/08  17:25:01  grosch
 * complete redesign in order to increase efficiency
 * 
 * Revision 1.1  89/01/09  17:29:23  grosch
 * added functions Size, Minimum, and Maximum
 * 
 * Revision 1.0  88/10/04  11:44:44  grosch
 * Initial revision
 * 
 */

/* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 */

static char rcsid [] = "$Id: Sets.c,v 1.10 1992/05/05 13:19:05 grosch rel $";

# include "ratc.h"
# include "Sets.h"
# include "DynArray.h"
# include "General.h"

# define BitsPerByte		8
# define BytesPerBitset		BitsPerBitset / BitsPerByte
# define ONE			0x80000000
# define NoCard			-1

# ifdef PCS10
# define CALL(f)	(* f)
# else
# define CALL(f)	f
# endif

void MakeSet
# ifdef __STDC__
   (tSet * Set, cardinal MaxSize)
# else
   (Set, MaxSize)
   tSet *	Set	;
   cardinal	MaxSize	;
# endif
   {
      unsigned long	ElmtCount	;

      ElmtCount = (MaxSize + BitsPerBitset - (MaxSize & MaskBitsPerBitset)) >> LdBitsPerBitset;
      MakeArray ((char * *) & Set->BitsetPtr, & ElmtCount, (unsigned long) BytesPerBitset);
      Set->MaxElmt = MaxSize;
      Set->LastBitset = ElmtCount - 1;
      AssignEmpty (Set);
   }
      
void ReleaseSet (Set)
   tSet *	Set;
   {
      unsigned long	ElmtCount	;

      ElmtCount = Set->LastBitset + 1;
      ReleaseArray ((char * *) & Set->BitsetPtr, & ElmtCount, (unsigned long) BytesPerBitset);
   }

void Union (Set1, Set2)
   tSet *	Set1	;
   tSet *	Set2	;
   {
      register tSet *	rSet1	= Set1;
      register cardinal	i 	= rSet1->LastBitset + 1;
      register BITSET *	s1	= rSet1->BitsetPtr;
      register BITSET *	s2	= Set2->BitsetPtr;
      register tSet *	rSet2	= Set2;

      do {* s1 ++ |= * s2 ++;} while (-- i);
      rSet1->Card      = NoCard;
      rSet1->FirstElmt = Min (rSet1->FirstElmt, rSet2->FirstElmt);
      rSet1->LastElmt  = Max (rSet1->LastElmt , rSet2->LastElmt );
   }

void Difference (Set1, Set2)
   tSet *	Set1	;
   tSet *	Set2	;
   {
      register tSet *	rSet1	= Set1;
      register cardinal	i 	= rSet1->LastBitset + 1;
      register BITSET *	s1	= rSet1->BitsetPtr;
      register BITSET *	s2	= Set2->BitsetPtr;

      do {* s1 ++ &= ~ * s2 ++;} while (-- i);
      rSet1->Card = NoCard;
   }

void Intersection (Set1, Set2)
   tSet *	Set1	;
   tSet *	Set2	;
   {
      register tSet *	rSet1	= Set1;
      register cardinal	i 	= rSet1->LastBitset + 1;
      register BITSET *	s1	= rSet1->BitsetPtr;
      register BITSET *	s2	= Set2->BitsetPtr;
      register tSet *	rSet2	= Set2;

      do {* s1 ++ &= * s2 ++;} while (-- i);
      rSet1->Card      = NoCard;
      rSet1->FirstElmt = Max (rSet1->FirstElmt, rSet2->FirstElmt);
      rSet1->LastElmt  = Min (rSet1->LastElmt , rSet2->LastElmt);
   }

void SymDiff (Set1, Set2)
   tSet *	Set1	;
   tSet *	Set2	;
   {
      register tSet *	rSet1	= Set1;
      register cardinal	i 	= rSet1->LastBitset + 1;
      register BITSET *	s1	= rSet1->BitsetPtr;
      register BITSET *	s2	= Set2->BitsetPtr;
      register tSet *	rSet2	= Set2;

      do {* s1 ++ ^= * s2 ++;} while (-- i);
      rSet1->Card      = NoCard;
      rSet1->FirstElmt = Min (rSet1->FirstElmt, rSet2->FirstElmt);
      rSet1->LastElmt  = Max (rSet1->LastElmt , rSet2->LastElmt);
   }

void Complement (Set)
   tSet *	Set	;
   {
      register tSet *	rSet	= Set;
      register cardinal	i 	= rSet->LastBitset;
      register BITSET *	s1	= rSet->BitsetPtr;

      while (i --) {* s1 = ~ * s1; s1 ++;}
      * s1 = ((long) ONE >> (rSet->MaxElmt & MaskBitsPerBitset)) & ~ * s1;
      if (rSet->Card != NoCard) rSet->Card = (short) rSet->MaxElmt + 1 - rSet->Card;
      rSet->FirstElmt = 0;
      rSet->LastElmt  = rSet->MaxElmt;
   }

void Include
# ifdef __STDC__
   (tSet * Set, cardinal Elmt)
# else
   (Set, Elmt)
   tSet *	Set	;
   cardinal	Elmt	;
# endif
   {
      register tSet *	rSet	= Set;
      register cardinal	rElmt	= Elmt;

      rSet->BitsetPtr [rElmt >> LdBitsPerBitset] |= (unsigned long) ONE >> (rElmt & MaskBitsPerBitset);
      rSet->Card      = NoCard;
      rSet->FirstElmt = Min (rSet->FirstElmt, rElmt);
      rSet->LastElmt  = Max (rSet->LastElmt , rElmt);
   }

void Exclude
# ifdef __STDC__
   (tSet * Set, cardinal Elmt)
# else
   (Set, Elmt)
   tSet *	Set	;
   cardinal	Elmt	;
# endif
   {
      register tSet *	rSet	= Set;
      register cardinal	rElmt	= Elmt;

      rSet->BitsetPtr [rElmt >> LdBitsPerBitset] &= ~ ((unsigned long) ONE >> (rElmt & MaskBitsPerBitset));
      rSet->Card = NoCard;
      if (rElmt == rSet->FirstElmt && rElmt < rSet->MaxElmt) rSet->FirstElmt ++;
      if (rElmt == rSet->LastElmt && rElmt > 0) rSet->LastElmt --;
   }

cardinal Card (Set)
   tSet *	Set	;
   {
      register tSet * rSet = Set;

      if (rSet->Card == NoCard) {
	 register cardinal i, n;
	 register cardinal last = rSet->LastElmt;

	 n = 0;
	 for (i = rSet->FirstElmt; i <= last; i ++) {
	    if (IsElement (i, rSet)) n ++;
	 }
	 rSet->Card = n;
      }
      return rSet->Card;
   }
    
cardinal Minimum (Set)
   tSet *	Set	;
   {
      register tSet *	rSet	= Set;
      register cardinal	i;
      register cardinal	last	= rSet->LastElmt;

      for (i = rSet->FirstElmt; i <= last; i ++) {
	 if (IsElement (i, rSet)) {
	    rSet->FirstElmt = i;
	    return i;
	 }
      }
      return 0;
   }
    
cardinal Maximum (Set)
   tSet *	Set	;
   {
      register tSet *	rSet	= Set;
      register cardinal	i;
      register cardinal	first	= rSet->FirstElmt;

      for (i = rSet->LastElmt; i >= first; i --) {
	 if (IsElement (i, rSet)) {
	    rSet->LastElmt = i;
	    return i;
	 }
      }
      return 0;
   }
    
cardinal Extract (Set)
   tSet *	Set	;
   {
      register cardinal i = Minimum (Set);
      Exclude (Set, i);
      return i;
   }

bool IsSubset (Set1, Set2)
   tSet *	Set1	;
   tSet *	Set2	;
   {
      register tSet *	rSet1	= Set1;
      register cardinal	i 	= rSet1->LastBitset + 1;
      register BITSET *	s1	= rSet1->BitsetPtr;
      register BITSET *	s2	= Set2->BitsetPtr;

      do {
	 if (* s1 ++ & ~ * s2 ++) return false;
      } while (-- i);
      return true;
   }

bool IsEqual (Set1, Set2)
   tSet *	Set1	;
   tSet *	Set2	;
   {
      register tSet *	rSet1	= Set1;
      register cardinal	i 	= rSet1->LastBitset + 1;
      register BITSET *	s1	= rSet1->BitsetPtr;
      register BITSET *	s2	= Set2->BitsetPtr;

      do {
	 if (* s1 ++ != * s2 ++) return false;
      } while (-- i);
      return true;
   }
    
bool IsEmpty (Set)
   tSet *	Set	;
   {
      register tSet * rSet = Set;

      if (rSet->FirstElmt <= rSet->LastElmt) {
	 register cardinal i  = rSet->LastBitset + 1;
	 register BITSET * s1 = rSet->BitsetPtr;

	 do {
	    if (* s1 ++ != 0) return false;
	 } while (-- i);
      }
      return true;
   }
    
bool Forall (Set, Proc)
   tSet *	Set	;
   bool		(* Proc) ();
   {
      register tSet *	rSet	= Set;
      register cardinal	i;
      register cardinal	last	= rSet->LastElmt;

      for (i = rSet->FirstElmt; i <= last; i ++) {
	 if (IsElement (i, rSet) && ! CALL(Proc) (i)) return false;
      }
      return true;
   }
    
bool Exists (Set, Proc)
   tSet *	Set	;
   bool		(* Proc) ();
   {
      register tSet *	rSet	= Set;
      register cardinal	i;
      register cardinal	last	= rSet->LastElmt;

      for (i = rSet->FirstElmt; i <= last; i ++) {
	 if (IsElement (i, rSet) && CALL(Proc) (i)) return true;
      }
      return false;
   }
    
bool Exists1 (Set, Proc)
   tSet *	Set	;
   bool		(* Proc) ();
   {
      register tSet *	rSet	= Set;
      register cardinal	i, n;
      register cardinal	last	= rSet->LastElmt;

      n = 0;
      for (i = rSet->FirstElmt; i <= last; i ++) {
	 if (IsElement (i, rSet) && CALL(Proc) (i)) n ++;
      }
      return n == 1;
   }

void Assign (Set1, Set2)
   tSet *	Set1	;
   tSet *	Set2	;
   {
      register tSet *	rSet1	= Set1;
      register cardinal	i 	= rSet1->LastBitset + 1;
      register BITSET *	s1	= rSet1->BitsetPtr;
      register BITSET *	s2	= Set2->BitsetPtr;
      register tSet *	rSet2	= Set2;

      do {* s1 ++ = * s2 ++;} while (-- i);
      rSet1->Card      = rSet2->Card;
      rSet1->FirstElmt = rSet2->FirstElmt;
      rSet1->LastElmt  = rSet2->LastElmt;
   }

void AssignElmt
# ifdef __STDC__
   (tSet * Set, cardinal Elmt)
# else
   (Set, Elmt)
   tSet *	Set	;
   cardinal	Elmt	;
# endif
   {
      register tSet *	rSet	= Set;
      register cardinal	rElmt	= Elmt;

      AssignEmpty (rSet);
      Include (rSet, rElmt);
      rSet->Card      = 1;
      rSet->FirstElmt = rElmt;
      rSet->LastElmt  = rElmt;
   }

void AssignEmpty (Set)
   tSet *	Set	;
   {
      register tSet *	rSet	= Set;
      register cardinal	i 	= rSet->LastBitset + 1;
      register BITSET *	s1	= rSet->BitsetPtr;

      do {* s1 ++ = 0;} while (-- i);
      rSet->Card      = 0;
      rSet->FirstElmt = rSet->MaxElmt;
      rSet->LastElmt  = 0;
   }

void ForallDo (Set, Proc)
   tSet *	Set	;
   void		(* Proc) ();
   {
      register tSet *	rSet	= Set;
      register cardinal	i;
      register cardinal	last	= rSet->LastElmt;

      for (i = rSet->FirstElmt; i <= last; i ++) {
	 if (IsElement (i, rSet)) CALL(Proc) (i);
      }
   }

void ReadSet (File, Set)
   FILE *	File	;
   tSet *	Set	;
   {
      register tSet * rSet = Set;
      int i, card = 0;

      while (fgetc (File) != '{');
      AssignEmpty (rSet);
      for (;;) {
	 if (fgetc (File) == '}') break;
	 (void) fscanf (File, "%d", & i);
	 Include (rSet, i);
	 card ++;
      }
      rSet->Card = card;
   }

static FILE * g;

void PrintElmt (Elmt)
   cardinal	Elmt	;
   {
      (void) fprintf (g, " %d", Elmt);
   }

void WriteSet (File, Set)
   FILE *	File	;
   tSet *	Set	;
   {
      g = File;
      (void) fprintf (File, "{");
      ForallDo (Set, PrintElmt);
      (void) fprintf (File, "}");
   }

void InitSets ()
   {
      if (sizeof (BITSET) != BytesPerBitset)
	 (void) fprintf (stderr, "Sets: sizeof (BITSET) = %d\n", sizeof (BITSET));
   }
