(*
 *	M T C  -  Modula-2 to C Translator
 *      ----------------------------------
 *
 *	Purpose: base operations for file and argument handling
 *
 *	$Author: grosch $
 *	$Date: 1991/11/21 16:57:59 $
 *	$Revision: 1.2 $
 *
 ***)

DEFINITION MODULE Base;

FROM IO		IMPORT tFile;
FROM Idents	IMPORT tIdent;
FROM Strings	IMPORT tString;

VAR
  SourceFile	: ARRAY [0 .. 127] OF CHAR;	(* source file name	*)
  MtcLibrary	: tString;			(* mtc library		*)

PROCEDURE CheckArguments;
			(* Handling of mtc arguments:			*)
			(*   Checks arguments, sets 'SourceFile' and	*)
			(*   'MtcLib', and defines mtc options and 	*)
			(*   libraries for definition modules.		*)

PROCEDURE OptionIsSet	(Option: CHAR): BOOLEAN;
			(* Returns TRUE if mtc option 'Option' is set.	*)

PROCEDURE CheckDefFile	(    ModuleName	: tIdent;
			 VAR FileName	: ARRAY OF CHAR;
			 VAR Success	: BOOLEAN);
			(* Check if a file named 'ModuleName'.md exists	*)
			(* in the current directory, or in one of the	*)
			(* defined libraries and return the full name	*)
			(* of that file in 'FileName' if such a file	*)
			(* exists.					*)

PROCEDURE OpenHeader	(Ident: tIdent): tFile;
			(* Open a header file named 'Ident'.h for	*)
			(* output.					*)

PROCEDURE OpenProgram	(Ident: tIdent): tFile;
			(* Open a source file named 'Ident'.c for	*)
			(* output.					*)

END Base.
