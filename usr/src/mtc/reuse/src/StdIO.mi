(* $Id: StdIO.mi,v 1.3 1991/11/21 14:33:17 grosch rel $ *)

(* $Log: StdIO.mi,v $
 * Revision 1.3  1991/11/21  14:33:17  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.2  89/07/14  16:27:23  grosch
 * made WriteN work for numbers with MSBit set
 * 
 * Revision 1.1  89/01/24  19:04:43  grosch
 * added procedure UnRead
 * 
 * Revision 1.0  88/10/04  11:47:16  grosch
 * Initial revision
 * 
 *)

(* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 *)

IMPLEMENTATION MODULE StdIO;			(* buffered standard IO	*)

FROM SYSTEM IMPORT ADDRESS;
FROM IO	    IMPORT StdInput, StdOutput;

IMPORT IO;

PROCEDURE ReadClose	;			(* close input file	*)
   BEGIN
      IO.ReadClose (StdInput);
   END ReadClose;

PROCEDURE Read		(Buffer: ADDRESS; Size: CARDINAL): INTEGER;
   BEGIN					(* binary		*)
      RETURN IO.Read (StdInput, Buffer, Size);
   END Read;

PROCEDURE ReadC		(): CHAR    ;		(* character		*)
   BEGIN
      RETURN IO.ReadC (StdInput);
   END ReadC;

PROCEDURE ReadI		(): INTEGER ;		(* integer  number	*)
   BEGIN
      RETURN IO.ReadI (StdInput);
   END ReadI;

PROCEDURE ReadR		(): REAL    ;		(* real     number	*)
   BEGIN
      RETURN IO.ReadR (StdInput);
   END ReadR;

PROCEDURE ReadB		(): BOOLEAN ;		(* boolean		*)
   BEGIN
      RETURN IO.ReadB (StdInput);
   END ReadB;

PROCEDURE ReadN		(Base: INTEGER): INTEGER;
   BEGIN					(* number of base 'Base'*)
      RETURN IO.ReadN (StdInput, Base);
   END ReadN;

PROCEDURE ReadS		(VAR s: ARRAY OF CHAR);	(* string		*)
   BEGIN
      IO.ReadS (StdInput, s);
   END ReadS;

PROCEDURE ReadShort	(): SHORTINT;		(* shortint number ?	*)
   BEGIN
      RETURN IO.ReadShort (StdInput);
   END ReadShort;

PROCEDURE ReadLong	(): LONGINT ;		(* longint  number ?	*)
   BEGIN
      RETURN IO.ReadLong (StdInput);
   END ReadLong;

PROCEDURE ReadCard	(): CARDINAL;		(* cardinal number ?	*)
   BEGIN
      RETURN IO.ReadCard (StdInput);
   END ReadCard;

PROCEDURE ReadNl	;			(* new line		*)
   BEGIN
      IO.ReadNl (StdInput);
   END ReadNl;

PROCEDURE UnRead	;			(* backspace 1 char.	*)
   BEGIN
      IO.UnRead (StdInput);
   END UnRead;


PROCEDURE EndOfLine	(): BOOLEAN ;		(* end of line ?	*)
   BEGIN
      RETURN IO.EndOfLine (StdInput);
   END EndOfLine;

PROCEDURE EndOfFile	(): BOOLEAN ;		(* end of file ?	*)
   BEGIN
      RETURN IO.EndOfFile (StdInput);
   END EndOfFile;



PROCEDURE WriteClose	;			(* close output file	*)
   BEGIN
      IO.WriteClose (StdOutput);
   END WriteClose;

PROCEDURE WriteFlush	;			(* flush output buffer	*)
   BEGIN
      IO.WriteFlush (StdOutput);
   END WriteFlush;

PROCEDURE Write		(Buffer: ADDRESS; Size: CARDINAL): INTEGER;
   BEGIN					(* binary		*)
      RETURN IO.Write (StdOutput, Buffer, Size);
   END Write;

PROCEDURE WriteC	(c: CHAR);		(* character		*)
   BEGIN
      IO.WriteC (StdOutput, c);
   END WriteC;

PROCEDURE WriteI	(n: INTEGER ; FieldWidth: CARDINAL);
   BEGIN					(* integer  number	*)
      IO.WriteI (StdOutput, n, FieldWidth);
   END WriteI;

PROCEDURE WriteR	(n: REAL; Before, After, Exp: CARDINAL);
   BEGIN					(* real     number	*)
      IO.WriteR (StdOutput, n, Before, After, Exp);
   END WriteR;

PROCEDURE WriteB	(b: BOOLEAN);		(* boolean		*)
   BEGIN
      IO.WriteB (StdOutput, b);
   END WriteB;

PROCEDURE WriteN	(n: LONGCARD; FieldWidth, Base: CARDINAL);
   BEGIN					(* number of base 'Base'*)
      IO.WriteN (StdOutput, n, FieldWidth, Base);
   END WriteN;

PROCEDURE WriteS	(s: ARRAY OF CHAR);	(* string		*)
   BEGIN
      IO.WriteS (StdOutput, s);
   END WriteS;

PROCEDURE WriteShort	(n: SHORTINT; FieldWidth: CARDINAL);
   BEGIN					(* shortint number ?	*)
      IO.WriteShort (StdOutput, n, FieldWidth);
   END WriteShort;

PROCEDURE WriteLong	(n: LONGINT ; FieldWidth: CARDINAL);
   BEGIN					(* longint  number ?	*)
      IO.WriteLong (StdOutput, n, FieldWidth);
   END WriteLong;

PROCEDURE WriteCard	(n: CARDINAL; FieldWidth: CARDINAL);
   BEGIN					(* cardinal number ?	*)
      IO.WriteCard (StdOutput, n, FieldWidth);
   END WriteCard;

PROCEDURE WriteNl	;			(* new line		*)
   BEGIN
      IO.WriteNl (StdOutput);
   END WriteNl;


PROCEDURE CloseIO;				(* close all files	*)
   BEGIN
      IO.WriteFlush (StdOutput);
   END CloseIO;

END StdIO.
