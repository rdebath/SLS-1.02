(* $Id: Sort.md,v 1.0 1992/08/07 14:42:01 grosch rel $ *)

(* $Log: Sort.md,v $
# Revision 1.0  1992/08/07  14:42:01  grosch
# Initial revision
#
 *)

(* Ich, Doktor Josef Grosch, Informatiker, Juli 1992 *)

DEFINITION MODULE Sort;

TYPE tProcIntIntBool	= PROCEDURE (INTEGER, INTEGER): BOOLEAN;
TYPE tProcIntInt	= PROCEDURE (INTEGER, INTEGER);

PROCEDURE Sort (Lwb, Upb: INTEGER; IsLess: tProcIntIntBool; Swap: tProcIntInt);

	(* Sort data from the indices 'Lwb' to 'Upb' using quicksort.	*)
	(* The procedures 'IsLess' and 'Swap' are used to compare and	*)
	(* exchange two data elements.					*)

END Sort.
