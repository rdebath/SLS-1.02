(* $Id: Times.md,v 1.1 1991/11/21 14:33:17 grosch rel $ *)

(* $Log: Times.md,v $
 * Revision 1.1  1991/11/21  14:33:17  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.0  88/10/04  11:47:38  grosch
 * Initial revision
 * 
 *)

(* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 *)

DEFINITION MODULE Times;

PROCEDURE CpuTime	(): LONGINT;
PROCEDURE StepTime	(): LONGINT;
PROCEDURE WriteStepTime	(Text: ARRAY OF CHAR);

END Times.
