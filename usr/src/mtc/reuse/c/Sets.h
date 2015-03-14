# ifndef yySets
# define yySets

/* $Id: Sets.h,v 1.7 1992/08/07 14:36:33 grosch rel $ */

/* $Log: Sets.h,v $
 * Revision 1.7  1992/08/07  14:36:33  grosch
 * layout changes
 *
 * Revision 1.6  1992/02/06  09:29:54  grosch
 * fixed bug: stdio and ANSI C
 *
 * Revision 1.5  1991/11/21  14:25:34  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.4  91/07/17  17:23:38  grosch
 * introduced ARGS trick for ANSI compatibility
 * 
 * Revision 1.3  90/07/04  14:34:05  grosch
 * introduced conditional include
 * 
 * Revision 1.2  89/12/08  17:25:03  grosch
 * complete redesign in order to increase efficiency
 * 
 * Revision 1.1  89/01/09  17:29:42  grosch
 * added functions Size, Minimum, and Maximum
 * 
 * Revision 1.0  88/10/04  11:44:45  grosch
 * Initial revision
 * 
 */

/* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 */

# include "ratc.h"
# include <stdio.h>

# ifdef __STDC__
# define ARGS(parameters)	parameters
# else
# define ARGS(parameters)	()
# endif

# define BitsPerBitset		32
# define LdBitsPerBitset	5
# define MaskBitsPerBitset	0x0000001f

# define IsElement(Elmt, Set)	    ((int) ((Set)->BitsetPtr [(Elmt) >> LdBitsPerBitset] << ((Elmt) & MaskBitsPerBitset)) < 0)
# define Size(Set)		    ((Set)->MaxElmt)
# define Select(Set)		    Minimum (Set)
# define IsNotEqual(Set1, Set2)	    (! IsEqual (Set1, Set2))
# define IsStrictSubset(Set1, Set2) (IsSubset (Set1, Set2) && IsNotEqual (Set1, Set2))

typedef long	BITSET		;

typedef struct	{
      cardinal	MaxElmt		;
      cardinal	LastBitset	;
      BITSET *	BitsetPtr	;
      short	Card		;
      cardinal	FirstElmt	;
      cardinal	LastElmt	;
   } tSet;

extern void	MakeSet		ARGS((tSet * Set, cardinal MaxSize));
extern void	ReleaseSet	ARGS((tSet * Set));
extern void	Union		ARGS((tSet * Set1, tSet * Set2));
extern void	Difference	ARGS((tSet * Set1, tSet * Set2));
extern void	Intersection	ARGS((tSet * Set1, tSet * Set2));
extern void	SymDiff		ARGS((tSet * Set1, tSet * Set2));
extern void	Complement	ARGS((tSet * Set));
extern void	Include		ARGS((tSet * Set, cardinal Elmt));
extern void	Exclude		ARGS((tSet * Set, cardinal Elmt));
extern cardinal	Card		ARGS((tSet * Set));
/* extern cardinal	Size		ARGS((tSet * Set)); */
extern cardinal	Minimum		ARGS((tSet * Set));
extern cardinal	Maximum		ARGS((tSet * Set));
/* extern cardinal	Select		ARGS((tSet * Set)); */
extern cardinal	Extract		ARGS((tSet * Set));
extern bool	IsSubset	ARGS((tSet * Set1, tSet * Set2));
/* extern bool	IsStrictSubset	ARGS((tSet * Set1, tSet * Set2)); */
extern bool	IsEqual		ARGS((tSet * Set1, tSet * Set2));
/* extern bool	IsNotEqual	ARGS((tSet * Set1, tSet * Set2)); */
/* extern bool	IsElement	ARGS((cardinal Elmt, tSet * Set)); */
extern bool	IsEmpty		ARGS((tSet * Set));
extern bool	Forall		ARGS((tSet * Set, bool (* Proc) ()));
extern bool	Exists		ARGS((tSet * Set, bool (* Proc) ()));
extern bool	Exists1		ARGS((tSet * Set, bool (* Proc) ()));
extern void	Assign		ARGS((tSet * Set1, tSet * Set2));
extern void	AssignElmt	ARGS((tSet * Set, cardinal Elmt));
extern void	AssignEmpty	ARGS((tSet * Set));
extern void	ForallDo	ARGS((tSet * Set, void (* Proc) ()));
extern void	ReadSet		ARGS((FILE * File, tSet * Set));
extern void	WriteSet	ARGS((FILE * File, tSet * Set));
extern void	InitSets	();

# endif
