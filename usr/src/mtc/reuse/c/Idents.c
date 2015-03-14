/* $Id: Idents.c,v 1.12 1992/05/05 13:19:05 grosch rel $ */

/* $Log: Idents.c,v $
 * Revision 1.12  1992/05/05  13:19:05  grosch
 * added rcsid
 *
 * Revision 1.11  1992/02/18  12:52:30  grosch
 * changed tString from unsigned char * to char *
 *
 * Revision 1.10  1992/01/31  16:31:44  grosch
 * adaption to ANSI C
 *
 * Revision 1.9  1992/01/30  13:13:54  grosch
 * redesign of StringMem: pointer instead of array index
 *
 * Revision 1.8  1992/01/14  15:24:35  grosch
 * introduced automatic initialization
 *
 * Revision 1.7  1991/11/21  14:28:16  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.6  91/09/18  15:18:46  grosch
 * added procedure GetStringRef
 * 
 * Revision 1.5  91/07/17  17:23:07  grosch
 * introduced ARGS trick for ANSI compatibility
 * 
 * Revision 1.4  91/01/21  12:13:20  grosch
 * some performance improvements
 * 
 * Revision 1.3  90/09/20  09:12:22  grosch
 * calmed down lint
 * 
 * Revision 1.2  90/07/04  14:33:55  grosch
 * introduced conditional include
 * 
 * Revision 1.1  89/06/06  10:28:25  grosch
 * added public variable NoIdent
 * 
 * Revision 1.0  88/10/04  11:44:38  grosch
 * Initial revision
 * 
 */

/* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 */

static char rcsid [] = "$Id: Idents.c,v 1.12 1992/05/05 13:19:05 grosch rel $";

# include "ratc.h"
# include "Idents.h"
# include "DynArray.h"

# define InitialTableSize	1024
# define HashTableSize		256
# define cNoIdent		0

tIdent	NoIdent = 1;

typedef struct {
   tStringRef	String;
   cardinal	Length;
   tIdent	Collision;
} IdentTableEntry;

static	unsigned short	Null		= 0;
static	IdentTableEntry	Table [InitialTableSize] = {{0, 0, 0}, {& Null, 0, cNoIdent}};
static	IdentTableEntry	* TablePtr	= Table;
static	unsigned long	IdentTableSize	= InitialTableSize;
static	tIdent		IdentCount	= 1;

static	tIdent		HashTable [HashTableSize] = {
   1	   , cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
   cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
   cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
   cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
   cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
   cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
   cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
   cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
   cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
   cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
   cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
   cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
   cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
   cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
   cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
   cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
   cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
   cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
   cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
   cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
   cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
   cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
   cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
   cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
   cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
   cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
   cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
   cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
   cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
   cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
   cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
   cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
};

tIdent MakeIdent
# ifdef __STDC__
   (register char * string, register cardinal length)
# else
   (string, length)
   register char *	string;
   register cardinal	length;
# endif
   {
      register	cardinal	HashTableIndex;
      register	tIdent		CurIdent;
      register	IdentTableEntry	* TablePtrReg;
   
      HashTableIndex = length == 0 ? 0 :	/* hash */
	 ((cardinal) * string + (cardinal) string [length - 1] * 11
			      + length * 26) & (HashTableSize - 1);

      CurIdent = HashTable [HashTableIndex];	/* search */
      while (CurIdent != cNoIdent) {
	 TablePtrReg = & TablePtr [CurIdent];
	 if (TablePtrReg->Length == length && IsEqualSt (TablePtrReg->String, string))
	    return CurIdent;			/* found */
	 CurIdent = TablePtrReg->Collision;
      }

      if (++ IdentCount == IdentTableSize)	/* not found: enter */
	 ExtendArray ((char * *) & TablePtr, & IdentTableSize, (long) sizeof (IdentTableEntry));
      TablePtrReg = & TablePtr [IdentCount];
      TablePtrReg->String	= PutString (string, length);
      TablePtrReg->Length	= length;
      TablePtrReg->Collision	= HashTable [HashTableIndex];
      HashTable [HashTableIndex] = IdentCount;
      return IdentCount;
   }

void GetString
# ifdef __STDC__
   (tIdent ident, char * string)
# else
   (ident, string)
   tIdent	ident;
   char *	string;
# endif
   {
      StGetString (TablePtr [ident].String, string);
   }

tStringRef GetStringRef
# ifdef __STDC__
   (tIdent ident)
# else
   (ident) tIdent ident;
# endif
   {
      return TablePtr [ident].String;
   }

tIdent MaxIdent ()
   {
      return IdentCount;
   }

void WriteIdent
# ifdef __STDC__
   (FILE * file, tIdent ident)
# else
   (file, ident)
   FILE *	file;
   tIdent	ident;
# endif
   {
      char	string [256];

      GetString (ident, string);
      (void) fputs (string, file);
   }

void WriteIdents ()
   {
      cardinal	i;

      for (i = 1; i <= IdentCount; i ++) {
	 (void) printf ("%5d ", i);
	 WriteIdent (stdout, i);
	 (void) fputc ('\n', stdout);
      }
   }

void WriteHashTable ()
   {
      tIdent	CurIdent;
      cardinal	i;
      cardinal	Count;
   
      for (i = 0; i < HashTableSize; i ++) {
	 (void) printf ("%5d", i);

	 Count = 0;
	 CurIdent = HashTable [i];
	 while (CurIdent != cNoIdent) {
	    Count ++;
	    CurIdent = TablePtr [CurIdent].Collision;
	 }
	 (void) printf ("%5d", Count);

	 CurIdent = HashTable [i];
	 while (CurIdent != cNoIdent) {
	    (void) fputc (' ', stdout);
	    WriteIdent (stdout, CurIdent);
	    CurIdent = TablePtr [CurIdent].Collision;
	 }
	 (void) fputc ('\n', stdout);
      }
      (void) printf ("\nIdents = %5d\n", IdentCount);
   }
    
void InitIdents ()
   {
      register cardinal i;

      for (i = 0; i < HashTableSize; i ++) HashTable [i] = cNoIdent;

      IdentCount	= 0;
      NoIdent		= MakeIdent ("", 0);
   }
