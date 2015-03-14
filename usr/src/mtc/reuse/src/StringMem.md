(* $Id: StringMem.md,v 1.3 1992/08/07 14:45:41 grosch rel $ *)

(* $Log: StringMem.md,v $
 * Revision 1.3  1992/08/07  14:45:41  grosch
 * added comments
 *
 * Revision 1.2  1991/11/21  14:33:17  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.1  89/01/21  23:02:41  grosch
 * added file parameter to procedure WriteString
 * 
 * Revision 1.0  88/10/04  11:47:17  grosch
 * Initial revision
 * 
 *)

(* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 *)

DEFINITION MODULE StringMem;

FROM IO		IMPORT tFile	;
FROM Strings	IMPORT tString	;

TYPE tStringRef = LONGINT	;

PROCEDURE PutString	(VAR s: tString)			: tStringRef;
			(* Stores string 's' in the string memory and	*)
			(* returns a reference to the stored string.	*)

PROCEDURE GetString	(r: tStringRef; 		   VAR s: tString);
			(* Returns the string 's' from the string	*)
			(* memory which is referenced by 'r'.		*)

PROCEDURE Length	(r: tStringRef)				: CARDINAL;
			(* Returns the length of the string 's'		*)
			(* which is referenced by 'r'.			*)

PROCEDURE IsEqual	(r: tStringRef; VAR s: tString)		: BOOLEAN;
			(* Compares the string referenced by 'r' and	*)
			(* the string 's'.				*)
			(* Returns TRUE if both are equal.		*)

PROCEDURE WriteString	(f: tFile; r: tStringRef);
			(* The string referenced by 'r' is printed on	*)
			(* file 'f'.					*)

PROCEDURE WriteStringMemory;
			(* The contents of the string memory is printed	*)
			(* on the terminal.				*)

PROCEDURE InitStringMemory;
			(* The string memory is initialized.		*)

END StringMem.
