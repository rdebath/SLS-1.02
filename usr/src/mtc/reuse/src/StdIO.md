(* $Id: StdIO.md,v 1.3 1991/11/21 14:33:17 grosch rel $ *)

(* $Log: StdIO.md,v $
 * Revision 1.3  1991/11/21  14:33:17  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.2  89/07/14  16:27:20  grosch
 * made WriteN work for numbers with MSBit set
 * 
 * Revision 1.1  89/01/24  19:04:40  grosch
 * added procedure UnRead
 * 
 * Revision 1.0  88/10/04  11:47:15  grosch
 * Initial revision
 * 
 *)

(* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 *)

DEFINITION MODULE StdIO;			(* buffered standard IO	*)

FROM SYSTEM IMPORT ADDRESS;

PROCEDURE ReadClose	;			(* close input file	*)
PROCEDURE Read		(Buffer: ADDRESS; Size: CARDINAL): INTEGER;
						(* binary		*)
PROCEDURE ReadC		(): CHAR    ;		(* character		*)
PROCEDURE ReadI		(): INTEGER ;		(* integer  number	*)
PROCEDURE ReadR		(): REAL    ;		(* real     number	*)
PROCEDURE ReadB		(): BOOLEAN ;		(* boolean		*)
PROCEDURE ReadN		(Base: INTEGER): INTEGER;
						(* number of base 'Base'*)
PROCEDURE ReadS		(VAR s: ARRAY OF CHAR);	(* string		*)
PROCEDURE ReadShort	(): SHORTINT;		(* shortint number ?	*)
PROCEDURE ReadLong	(): LONGINT ;		(* longint  number ?	*)
PROCEDURE ReadCard	(): CARDINAL;		(* cardinal number ?	*)
PROCEDURE ReadNl	;			(* new line		*)
PROCEDURE UnRead	;			(* backspace 1 char.	*)

PROCEDURE EndOfLine	(): BOOLEAN ;		(* end of line ?	*)
PROCEDURE EndOfFile	(): BOOLEAN ;		(* end of file ?	*)


PROCEDURE WriteClose	;			(* close output file	*)
PROCEDURE WriteFlush	;			(* flush output buffer	*)
PROCEDURE Write		(Buffer: ADDRESS; Size: CARDINAL): INTEGER;
						(* binary		*)
PROCEDURE WriteC	(c: CHAR);		(* character		*)
PROCEDURE WriteI	(n: INTEGER ; FieldWidth: CARDINAL);
						(* integer  number	*)
PROCEDURE WriteR	(n: REAL; Before, After, Exp: CARDINAL);
						(* real     number	*)
PROCEDURE WriteB	(b: BOOLEAN);		(* boolean		*)
PROCEDURE WriteN	(n: LONGCARD; FieldWidth, Base: CARDINAL);
						(* number of base 'Base'*)
PROCEDURE WriteS	(s: ARRAY OF CHAR);	(* string		*)
PROCEDURE WriteShort	(n: SHORTINT; FieldWidth: CARDINAL);
						(* shortint number ?	*)
PROCEDURE WriteLong	(n: LONGINT ; FieldWidth: CARDINAL);
						(* longint  number ?	*)
PROCEDURE WriteCard	(n: CARDINAL; FieldWidth: CARDINAL);
						(* cardinal number ?	*)
PROCEDURE WriteNl	;			(* new line		*)


PROCEDURE CloseIO;				(* close all files	*)

END StdIO.
