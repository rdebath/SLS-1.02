(* $Id: Memory.md,v 1.2 1992/08/07 14:45:41 grosch rel $ *)

(* $Log: Memory.md,v $
 * Revision 1.2  1992/08/07  14:45:41  grosch
 * added comments
 *
 * Revision 1.1  1991/11/21  14:33:17  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.0  88/10/04  11:47:10  grosch
 * Initial revision
 * 
 *)

(* Ich, Doktor Josef Grosch, Informatiker, Juli 1986 *)

DEFINITION MODULE Memory;

FROM SYSTEM IMPORT ADDRESS;

VAR	  MemoryUsed	: LONGCARD;
			(* Holds the total amount of memory managed by	*)
			(* this module.					*)

PROCEDURE Alloc		(ByteCount: LONGINT) : ADDRESS;
			(* Returns a pointer to dynamically allocated	*)
			(* space of size 'ByteCount' bytes.		*)
			(* Returns NIL if space is exhausted.           *)


PROCEDURE Free		(ByteCount: LONGINT; a: ADDRESS);
			(* The dynamically allocated space starting at	*)
			(* address 'a' of size 'ByteCount' bytes is	*)
			(* released.					*)

(* PROCEDURE WriteMemory; *)

END Memory.
