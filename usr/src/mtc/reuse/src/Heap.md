(* $Id: Heap.md,v 1.2 1992/08/07 14:45:41 grosch rel $ *)

(* $Log: Heap.md,v $
 * Revision 1.2  1992/08/07  14:45:41  grosch
 * added comments
 *
 * Revision 1.1  1991/11/21  14:33:17  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.0  88/10/04  11:46:55  grosch
 * Initial revision
 * 
 *)

(* Ich, Doktor Josef Grosch, Informatiker, 2.9.1988 *)

DEFINITION MODULE Heap;

FROM SYSTEM IMPORT ADDRESS;

VAR	  HeapUsed	: LONGCARD;
			(* Holds the total amount of memory managed by	*)
			(* this module.					*)

PROCEDURE Alloc		(ByteCount: LONGINT) : ADDRESS;
			(* Returns a pointer to dynamically allocated	*)
			(* space of size 'ByteCount' bytes.		*)

PROCEDURE Free		;
			(* The complete space allocated for the heap	*)
			(* is released.					*)

(* PROCEDURE WriteHeap; *)

END Heap.
