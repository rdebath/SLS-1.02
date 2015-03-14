(* $Id: IO.md,v 1.5 1992/01/30 13:23:29 grosch rel $ *)

(* $Log: IO.md,v $
 * Revision 1.5  1992/01/30  13:23:29  grosch
 * redesign of interface to operating system
 *
 * Revision 1.4  1991/11/21  14:33:17  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.3  89/08/18  11:11:28  grosch
 * make Write work for Size = 0
 * 
 * Revision 1.2  89/07/14  16:26:27  grosch
 * made WriteN work for numbers with MSBit set
 * 
 * Revision 1.1  89/01/24  19:04:20  grosch
 * added procedure UnRead
 * 
 * Revision 1.0  88/10/04  11:46:57  grosch
 * Initial revision
 * 
 *)

(* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 *)

DEFINITION MODULE IO;				(* buffered IO		*)

FROM SYSTEM	IMPORT ADDRESS;

IMPORT System;

CONST
   StdInput	= System.StdInput;
   StdOutput	= System.StdOutput;
   StdError	= System.StdError;

TYPE
   tFile	= System.tFile;

PROCEDURE ReadOpen	(VAR FileName: ARRAY OF CHAR): tFile;
						(* open  input file	*)
PROCEDURE ReadClose	(f: tFile);		(* close input file	*)
PROCEDURE Read		(f: tFile; Buffer: ADDRESS; Size: CARDINAL): INTEGER;
						(* binary		*)
PROCEDURE ReadC		(f: tFile): CHAR    ;	(* character		*)
PROCEDURE ReadI		(f: tFile): INTEGER ;	(* integer  number	*)
PROCEDURE ReadR		(f: tFile): REAL    ;	(* real     number	*)
PROCEDURE ReadB		(f: tFile): BOOLEAN ;	(* boolean		*)
PROCEDURE ReadN		(f: tFile; Base: INTEGER): INTEGER;
						(* number of base 'Base'*)
PROCEDURE ReadS		(f: tFile; VAR s: ARRAY OF CHAR);
						(* string		*)
PROCEDURE ReadShort	(f: tFile): SHORTINT;	(* shortint number ?	*)
PROCEDURE ReadLong	(f: tFile): LONGINT ;	(* longint  number ?	*)
PROCEDURE ReadCard	(f: tFile): CARDINAL;	(* cardinal number ?	*)
PROCEDURE ReadNl	(f: tFile);		(* new line		*)
PROCEDURE UnRead	(f: tFile);		(* backspace 1 char.	*)

PROCEDURE EndOfLine	(f: tFile): BOOLEAN ;	(* end of line ?	*)
PROCEDURE EndOfFile	(f: tFile): BOOLEAN ;	(* end of file ?	*)


PROCEDURE WriteOpen	(VAR FileName: ARRAY OF CHAR): tFile;
						(* open  output file	*)
PROCEDURE WriteClose	(f: tFile);		(* close output file	*)
PROCEDURE WriteFlush	(f: tFile);		(* flush output buffer	*)
PROCEDURE Write		(f: tFile; Buffer: ADDRESS; Size: INTEGER): INTEGER;
						(* binary		*)
PROCEDURE WriteC	(f: tFile; c: CHAR);	(* character		*)
PROCEDURE WriteI	(f: tFile; n: INTEGER ; FieldWidth: CARDINAL);
						(* integer  number	*)
PROCEDURE WriteR	(f: tFile; n: REAL; Before, After, Exp: CARDINAL);
						(* real     number	*)
PROCEDURE WriteB	(f: tFile; b: BOOLEAN);	(* boolean		*)
PROCEDURE WriteN	(f: tFile; n: LONGCARD; FieldWidth, Base: CARDINAL);
						(* number of base 'Base'*)
PROCEDURE WriteS	(f: tFile; VAR s: ARRAY OF CHAR); 
						(* string		*)
PROCEDURE WriteShort	(f: tFile; n: SHORTINT; FieldWidth: CARDINAL);
						(* shortint number ?	*)
PROCEDURE WriteLong	(f: tFile; n: LONGINT ; FieldWidth: CARDINAL);
						(* longint  number ?	*)
PROCEDURE WriteCard	(f: tFile; n: CARDINAL; FieldWidth: CARDINAL);
						(* cardinal number ?	*)
PROCEDURE WriteNl	(f: tFile);		(* new line		*)


PROCEDURE CloseIO;				(* close all files	*)

END IO.
