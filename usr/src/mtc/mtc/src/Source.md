(*
 *	M T C  -  Modula-2 to C Translator
 *      ----------------------------------
 *
 *	Purpose: access to source files
 *
 *	$Author: grosch $
 *	$Date: 1992/01/30 14:49:17 $
 *	$Revision: 1.4 $
 *
 ***)

DEFINITION MODULE Source;

FROM SYSTEM	IMPORT ADDRESS;
FROM System	IMPORT tFile;
FROM Idents	IMPORT tIdent;

PROCEDURE BeginSource (FileName: ARRAY OF CHAR): tFile;

   (*
      BeginSource is called from the scanner to open files.
      If not called input is read form standard input.
   *)

PROCEDURE GetLine (File: tFile; Buffer: ADDRESS; Size: CARDINAL): INTEGER;

   (*
      GetLine is called to fill a buffer starting at address 'Buffer'
      with a block of maximal 'Size' characters. Lines are terminated
      by newline characters (ASCII = 0xa). GetLine returns the number
      of characters transferred. Reasonable block sizes are between 128
      and 2048 or the length of a line. Smaller block sizes -
      especially block size 1 - will drastically slow down the scanner.
   *)

PROCEDURE CloseSource (File: tFile);

   (*
      CloseSource is called from the scanner at end of file respectively
      at end of input. It can be used to close files.
   *)

END Source.
