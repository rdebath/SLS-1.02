(* $Id: Idents.md,v 1.6 1992/08/07 14:45:41 grosch rel $ *)

(* $Log: Idents.md,v $
 * Revision 1.6  1992/08/07  14:45:41  grosch
 * added comments
 *
 * Revision 1.5  1991/11/21  14:33:17  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.4  89/06/06  10:07:56  grosch
 * changed tIdent to SHORTCARD
 * 
 * Revision 1.3  89/06/01  18:20:22  grosch
 * added predefined identifier NoIdent
 * 
 * Revision 1.2  89/01/25  12:05:29  grosch
 * added function MaxIdent
 * 
 * Revision 1.1  89/01/21  23:03:08  grosch
 * added file parameter to procedure WriteIdent
 * 
 * Revision 1.0  88/10/04  11:47:00  grosch
 * Initial revision
 * 
 *)

(* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 *)

DEFINITION MODULE Idents;

FROM IO		IMPORT tFile		;
FROM Strings	IMPORT tString		;
FROM StringMem	IMPORT tStringRef	;

TYPE	  tIdent	= SHORTCARD;

VAR	  NoIdent	: tIdent;
			(* A default identifer (empty string).		*)

PROCEDURE MakeIdent	(VAR s: tString)		: tIdent;
			(* The string 's' is mapped to a unique number	*)
			(* (an integer) which is returned.		*)

PROCEDURE GetString	(i: tIdent; VAR s: tString);
			(* Returns the string 's' whose number is 'i'.	*)

PROCEDURE GetStringRef	(i: tIdent)			: tStringRef;
			(* Returns a reference to the string whose	*)
			(* number is 'i'.				*)

PROCEDURE MaxIdent	()				: tIdent;
			(* Returns the current maximal value of the	*)
			(* type 'tIdent'.				*)

PROCEDURE WriteIdent	(f: tFile; i: tIdent);
			(* The string encoded by the ident 'i' is	*)
			(* printed on file 'f'.				*)

PROCEDURE WriteIdents	;
			(* The contents of the identifier table is	*)
			(* printed on the terminal.			*)

PROCEDURE InitIdents	;
			(* The identifier table	is initialized.		*)

PROCEDURE WriteHashTable;

END Idents.
