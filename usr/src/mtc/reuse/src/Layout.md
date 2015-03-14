(* $Id: Layout.md,v 1.2 1991/11/21 14:33:17 grosch rel $ *)

(* $Log: Layout.md,v $
 * Revision 1.2  1991/11/21  14:33:17  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.1  89/01/24  19:04:46  grosch
 * added procedure SkipSpaces
 * 
 * Revision 1.0  88/10/04  11:47:02  grosch
 * Initial revision
 * 
 *)

(* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 *)

DEFINITION MODULE Layout;

FROM IO	IMPORT tFile;

PROCEDURE WriteChar	(f: tFile; Ch: CHAR);
PROCEDURE WriteSpace	(f: tFile);
PROCEDURE WriteSpaces	(f: tFile; Count: INTEGER);

PROCEDURE ReadSpace	(f: tFile);
PROCEDURE ReadSpaces	(f: tFile; Count: INTEGER);
PROCEDURE SkipSpaces	(f: tFile);

END Layout.
