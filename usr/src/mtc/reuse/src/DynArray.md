(* $Id: DynArray.md,v 1.1 1991/11/21 14:33:17 grosch rel $ *)

(* $Log: DynArray.md,v $
 * Revision 1.1  1991/11/21  14:33:17  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.0  88/10/04  11:46:52  grosch
 * Initial revision
 * 
 *)

(* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 *)

DEFINITION MODULE DynArray;

FROM SYSTEM	IMPORT ADDRESS;

PROCEDURE MakeArray    (VAR ArrayPtr	: ADDRESS	;
			VAR ElmtCount	: LONGINT	;
			    ElmtSize	: LONGINT)	;

PROCEDURE ExtendArray  (VAR ArrayPtr	: ADDRESS	;
			VAR ElmtCount	: LONGINT	;
			    ElmtSize	: LONGINT)	;

PROCEDURE ReleaseArray (VAR ArrayPtr	: ADDRESS	;
			VAR ElmtCount	: LONGINT	;
			    ElmtSize	: LONGINT)	;

END DynArray.
