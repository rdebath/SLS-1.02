(* $Id: General.md,v 1.3 1992/01/30 13:23:29 grosch rel $ *)

(* $Log: General.md,v $
 * Revision 1.3  1992/01/30  13:23:29  grosch
 * redesign of interface to operating system
 *
 * Revision 1.2  1991/11/21  14:33:17  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.1  90/03/02  17:36:05  grosch
 * automized handling of machine independent alignment
 * 
 * Revision 1.0  88/10/04  11:46:53  grosch
 * Initial revision
 * 
 *)

(* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 *)

(* General Subroutines: minimum, maximum, binary logarithm, and power of 2 *)

DEFINITION MODULE General;

VAR	  MaxAlign	: INTEGER;
VAR	  AlignMasks	: ARRAY [0..8] OF BITSET;

PROCEDURE Min		(a, b: INTEGER)			: INTEGER;
			(* Returns the minimum of 'a' and 'b'.		*)

PROCEDURE Max		(a, b: INTEGER)			: INTEGER;
			(* Returns the maximum of 'a' and 'b'.		*)

PROCEDURE Log2		(x: LONGINT)			: CARDINAL;
			(* Returns the logarithm to the base 2 of 'x'.	*)

PROCEDURE Exp2		(x: CARDINAL)			: LONGINT;
			(* Returns 2 to the power of 'x'.		*)

PROCEDURE AntiLog	(x: LONGINT)			: CARDINAL;
			(* Returns the number of the lowest bit set in 'x'. *)

PROCEDURE Exp10		(x: INTEGER)			: REAL;
			(* Returns 10 to the power of 'x'.		*)

END General.
