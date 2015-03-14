(* $Id: Texts.md,v 1.2 1992/08/07 14:43:04 grosch rel $ *)

(* $Log: Texts.md,v $
 * Revision 1.2  1992/08/07  14:43:04  grosch
 * added procedure IsEmpty
 *
 * Revision 1.1  1991/11/21  14:33:17  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.0  88/10/04  11:47:36  grosch
 * Initial revision
 * 
 *)

(* Ich, Doktor Josef Grosch, Informatiker, 31.8.1988 *)

DEFINITION MODULE Texts;

FROM IO		IMPORT tFile	;
FROM Lists	IMPORT tList	;
FROM Strings	IMPORT tString	;

TYPE tText	= tList;

PROCEDURE MakeText	(VAR Text: tText);
			(* Create an empty text.			*)

PROCEDURE Append	(VAR Text: tText; VAR String: tString);
			(* Add a line at the beginning of text 'Text'.	*)

PROCEDURE Insert	(VAR Text: tText; VAR String: tString);
			(* Add a line at the end of the text 'Text'.	*)

PROCEDURE IsEmpty	(VAR Text: tText): BOOLEAN;
			(* Test whether a text 'Text' is empty.		*)

PROCEDURE WriteText	(f: tFile; Text: tText);
			(* Print the text 'Text' on the file 'f'.	*)

END Texts.
