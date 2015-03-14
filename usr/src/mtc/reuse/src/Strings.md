(* $Id: Strings.md,v 1.3 1991/11/21 14:33:17 grosch rel $ *)

(* $Log: Strings.md,v $
 * Revision 1.3  1991/11/21  14:33:17  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.2  89/06/06  10:06:58  grosch
 * changed tStringIndex to SHORTCARD
 * 
 * Revision 1.1  89/05/22  10:45:34  grosch
 * added procedure IntToString
 * 
 * Revision 1.0  88/10/04  11:47:20  grosch
 * Initial revision
 * 
 *)

(* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 *)

DEFINITION MODULE Strings;

FROM IO IMPORT tFile;

CONST	cMaxStrLength	= 255;

TYPE	tStringIndex	= SHORTCARD [0 .. cMaxStrLength];

TYPE	tString		= RECORD
		     	     Chars  : ARRAY tStringIndex OF CHAR;
		     	     Length : tStringIndex;
		  	  END;

PROCEDURE Assign	(VAR s1, s2: tString);

			(* Assigns the string 's2' to the string 's1'.	*)

PROCEDURE AssignEmpty	(VAR s: tString);

			(* Returns an empty string 's'.			*)

PROCEDURE Concatenate	(VAR s1, s2: tString);

			(* Returns in parameter 's1' the concatenation	*)
			(* of the strings 's1' and 's2'.		*)

PROCEDURE Append	(VAR s: tString; c: CHAR);

			(* The character 'c' is concatenated at the end	*)
			(* of the string 's'.				*)

PROCEDURE Length	(VAR s: tString)			: CARDINAL;

			(* Returns the length of the string 's'.	*)

PROCEDURE IsEqual	(VAR s1, s2: tString)			: BOOLEAN;

			(* Returns TRUE if the strings 's1' and 's2'	*)
			(* are equal.					*)

PROCEDURE IsInOrder	(VAR s1, s2: tString)			: BOOLEAN;

			(* Returns TRUE if the string 's1' is lexico-	*)
			(* graphically less or equal to the string 's2'.*)

PROCEDURE Exchange	(VAR s1, s2: tString);

			(* Exchanges the strings 's1' and 's2'.		*)

PROCEDURE SubString	(VAR s1: tString; from, to: tStringIndex; VAR s2: tString);

			(* Returns in 's2' the substring from 's1' com-	*)
			(* prising the characters between 'from' and 'to'. *)
			(* PRE	1 <= from <= Length (s1)		*)
			(* PRE	1 <=  to  <= Length (s1)		*)

PROCEDURE Char		(VAR s: tString; i: tStringIndex)	: CHAR;

			(* Returns the 'i'-th character of the string 's'. *)
			(* The characters are counted from 1 to Length (s). *)
			(* PRE	1 <= index <= Length (s)		*)

PROCEDURE ArrayToString	(a: ARRAY OF CHAR; VAR s: tString);

			(* An array 'a' of characters representing a	*)
			(* MODULA string is converted to a string 's'	*)
			(* of type tString.				*)

PROCEDURE StringToArray	(VAR s: tString; VAR a: ARRAY OF CHAR);

			(* A string 's' of type tString is converted to *)
			(* an array 'a' of characters representing a	*)
			(* MODULA string.				*)

PROCEDURE StringToInt	(VAR s: tString)			: INTEGER;

			(* Returns the integer value represented by 's'. *)

PROCEDURE StringToNumber(VAR s: tString; Base: CARDINAL)	: CARDINAL;

			(* Returns the integer value represented by 's'	*)
			(* to the base 'Base'.				*)

PROCEDURE StringToReal	(VAR s: tString)			: REAL;

			(* Returns the real value represented by 's'.	*)

PROCEDURE IntToString	(n: INTEGER; VAR s: tString);

			(* Returns in 's' the string representation of 'n'. *)

PROCEDURE ReadS		(f: tFile; VAR s: tString; FieldWidth: tStringIndex);

			(* Read 'FieldWidth' characters as string 's' 	*)
			(* from file 'f'.				*)

PROCEDURE ReadL		(f: tFile; VAR s: tString);

			(* Read rest of line as string 's' from file	*)
			(* 'f'.	Skip to next line.			*)

PROCEDURE WriteS	(f: tFile; VAR s: tString);

			(* Write string 's' to file 'f'.		*)

PROCEDURE WriteL	(f: tFile; VAR s: tString);

			(* Write string 's' as complete line.		*)

END Strings.
