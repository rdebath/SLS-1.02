(* $Id: IO.mi,v 1.9 1992/01/30 13:23:29 grosch rel $ *)

(* $Log: IO.mi,v $
 * Revision 1.9  1992/01/30  13:23:29  grosch
 * redesign of interface to operating system
 *
 * Revision 1.8  1991/11/21  14:33:17  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.7  91/06/07  12:19:51  grosch
 * decreased bounds of flexible arrays
 * 
 * Revision 1.6  91/06/07  11:37:42  grosch
 * increased bounds of flexible arrays
 * 
 * Revision 1.5  91/01/16  17:11:13  grosch
 * fixed range check problem with BytesRead
 * 
 * Revision 1.4  89/08/18  11:11:48  grosch
 * make Write work for Size = 0
 * 
 * Revision 1.3  89/07/14  16:27:15  grosch
 * made WriteN work for numbers with MSBit set
 * 
 * Revision 1.2  89/01/25  19:37:28  grosch
 * tuning: ReadC inline in Read and ReadS, WriteC inline in Write and WriteS
 * 
 * Revision 1.1  89/01/24  19:04:35  grosch
 * added procedure UnRead
 * 
 * Revision 1.0  88/10/04  11:46:58  grosch
 * Initial revision
 * 
 *)

(* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 *)

IMPLEMENTATION MODULE IO;			(* buffered IO		*)

FROM	SYSTEM	IMPORT ADDRESS	, ADR	;
FROM	General	IMPORT Exp2	, Exp10	;
FROM	Memory	IMPORT Alloc	, Free	;

IMPORT	System;

CONST
   EolCh		= 12C;
   TabCh		= 11C;
   BufferSize		= 1024;
   MaxInt		= 2147483647;	(* 2 ** 31 - 1 *)
   MaxPow10		= 1000000000;
   MaxIntDiv10		= MaxInt DIV 10;

TYPE
   BufferDescriptor	= RECORD
	 Buffer		: POINTER TO ARRAY [0 .. BufferSize] OF CHAR;
	 BufferIndex	: SHORTINT;
	 BytesRead	: SHORTINT;
	 OpenForOutput	: BOOLEAN;
	 EndOfFile	: BOOLEAN;
	 FlushLine	: BOOLEAN;
      END;

   (* INV BufferIndex points before the character to be read next *)

VAR
   BufferPool	: ARRAY tFile OF BufferDescriptor;
   i		: tFile;
   MyCHR	: ARRAY [0 .. 15] OF CHAR;

PROCEDURE FillBuffer	(f: tFile);
   BEGIN
      WITH BufferPool [f] DO
	 IF FlushLine THEN
	    WriteFlush (StdOutput);
	    WriteFlush (StdError );
	 END;
	 BufferIndex := 0;
	 BytesRead := System.Read (f, ADR (Buffer^ [1]), BufferSize);
	 IF BytesRead <= 0 THEN
	    BytesRead := 0;
	    EndOfFile := TRUE;
	 END;
      END;
   END FillBuffer;

PROCEDURE ReadOpen	(VAR FileName: ARRAY OF CHAR): tFile;
   VAR						(* open  input file	*)
      f		: tFile;
   BEGIN
      f := System.OpenInput (FileName);
      WITH BufferPool [f] DO
	 Buffer		:= Alloc (BufferSize + 1);
	 BufferIndex	:= 0;
	 BytesRead	:= 0;
	 OpenForOutput	:= FALSE;
	 EndOfFile	:= FALSE;
      END;
      CheckFlushLine (f);
      RETURN f;
   END ReadOpen;

PROCEDURE ReadClose	(f: tFile);		(* close input file	*)
   BEGIN
      System.Close (f);
      WITH BufferPool [f] DO
	 Free (BufferSize + 1, Buffer);
	 Buffer := NIL;
      END;
   END ReadClose;

PROCEDURE Read		(f: tFile; Buffer: ADDRESS; Size: CARDINAL): INTEGER;
   VAR						(* binary		*)
      BufferPtr : POINTER TO ARRAY [0 .. 100000000] OF CHAR;
      i		: CARDINAL;
   BEGIN
      BufferPtr := Buffer;
      WITH BufferPool [f] DO
	 i := 0;
	 LOOP
	    IF i = Size THEN RETURN i; END;
	    IF BufferIndex = BytesRead THEN	(* ReadC inline		*)
	       FillBuffer (f);
	       IF EndOfFile THEN Buffer^ [1] := 0C; END;
	    END;
	    INC (BufferIndex);
	    BufferPtr^ [i] := Buffer^ [BufferIndex];
	    IF EndOfFile THEN RETURN i; END;
	    INC (i);
	 END;
      END;
   END Read;

PROCEDURE ReadC		(f: tFile): CHAR;	(* character		*)
   BEGIN
      WITH BufferPool [f] DO
	 IF BufferIndex = BytesRead THEN
	    FillBuffer (f);
	    IF EndOfFile THEN Buffer^ [1] := 0C; END;
	 END;
	 INC (BufferIndex);
	 RETURN Buffer^ [BufferIndex];
      END;
   END ReadC;

PROCEDURE ReadI		(f: tFile): INTEGER;	(* integer  number	*)
   VAR
      n		: INTEGER;
      ch	: CHAR;
      negative	: BOOLEAN;
   BEGIN
      REPEAT
         ch := ReadC (f);
      UNTIL (ch # ' ') AND (ch # TabCh) AND (ch # EolCh);
      CASE ch OF
      |  '+' : negative := FALSE; ch := ReadC (f);
      |  '-' : negative := TRUE ; ch := ReadC (f);
      ELSE     negative := FALSE;
      END;
      n := 0;
      WHILE ('0' <= ch) AND (ch <= '9') DO
	 n := 10 * n + INTEGER (ORD (ch) - ORD ('0'));
	 ch := ReadC (f);
      END;
      DEC (BufferPool [f].BufferIndex);
      IF negative
      THEN RETURN - n;
      ELSE RETURN   n;
      END;
   END ReadI;

PROCEDURE ReadR		(f: tFile): REAL;	(* real     number	*)
   VAR
      n			: REAL;
      Mantissa		: LONGCARD;
      Exponent		: INTEGER;
      MantissaNeg	: BOOLEAN;
      ExponentNeg	: BOOLEAN;
      FractionDigits	: CARDINAL;
      TruncatedDigits	: CARDINAL;
      ch		: CHAR;
   BEGIN
      MantissaNeg	:= FALSE;
      Mantissa		:= 0;
      Exponent		:= 0;
      FractionDigits	:= 0;
      TruncatedDigits	:= 0;

      REPEAT					(* skip white space	*)
	 ch := ReadC (f);
      UNTIL (ch # ' ') AND (ch # TabCh) AND (ch # EolCh);

      CASE ch OF				(* handle sign		*)
      | '+' : ch := ReadC (f);
      | '-' : ch := ReadC (f); MantissaNeg := TRUE;
      | 'E' : Mantissa := 1;
      ELSE
      END;

      WHILE ('0' <= ch) AND (ch <= '9') DO	(* integer part		*)
	 IF Mantissa <= MaxIntDiv10 THEN
	    Mantissa := 10 * Mantissa;
	    IF Mantissa <= MaxInt - (ORD (ch) - ORD ('0')) THEN
	       INC (Mantissa, ORD (ch) - ORD ('0'));
	    ELSE
	       INC (TruncatedDigits);
	    END;
	 ELSE
	    INC (TruncatedDigits);
	 END;
	 ch := ReadC (f);
      END;

      IF ch = '.' THEN ch := ReadC (f); END;	(* decimal point	*)

      WHILE ('0' <= ch) AND (ch <= '9') DO	(* fractional part	*)
	 IF Mantissa <= MaxIntDiv10 THEN
	    Mantissa := 10 * Mantissa;
	    IF Mantissa <= MaxInt - (ORD (ch) - ORD ('0')) THEN
	       INC (Mantissa, ORD (ch) - ORD ('0'));
	    ELSE
	       INC (TruncatedDigits);
	    END;
	 ELSE
	    INC (TruncatedDigits);
	 END;
	 INC (FractionDigits);
	 ch := ReadC (f);
      END;

      IF ch = 'E' THEN				(* exponent		*)
	 ch := ReadC (f);

	 CASE ch OF
	 |  '+' : ExponentNeg := FALSE; ch := ReadC (f);
	 |  '-' : ExponentNeg := TRUE ; ch := ReadC (f);
	 ELSE     ExponentNeg := FALSE;
	 END;

	 WHILE ('0' <= ch) AND (ch <= '9') DO
	    Exponent := 10 * Exponent + INTEGER (ORD (ch) - ORD ('0'));
	    ch := ReadC (f);
	 END;

	 IF ExponentNeg THEN
	    Exponent := - Exponent;
	 END;
      END;

      DEC (BufferPool [f].BufferIndex);
      DEC (Exponent, FractionDigits - TruncatedDigits);
      n := FLOAT (Mantissa) * Exp10 (Exponent);
      IF MantissaNeg
      THEN RETURN - n;
      ELSE RETURN   n;
      END;
   END ReadR;

PROCEDURE ReadB		(f: tFile): BOOLEAN;	(* boolean		*)
   BEGIN
      RETURN ReadC (f) = 'T';
   END ReadB;

PROCEDURE ReadN		(f: tFile; Base: INTEGER): INTEGER;
   VAR						(* number of base 'Base'*)
      n		: INTEGER;
      ch	: CHAR;
      digit	: INTEGER;
   BEGIN
      REPEAT
	 ch := ReadC (f);
      UNTIL (ch # ' ') AND (ch # TabCh) AND (ch # EolCh);
      n := 0;
      LOOP
	 IF ('0' <= ch) AND (ch <= '9') THEN
	    digit := ORD (ch) - ORD ('0');
	 ELSIF ('A' <= ch) AND (ch <= 'F') THEN
	    digit := ORD (ch) - ORD ('A') + 10;
	 ELSE
	    digit := 99;
	 END;
	 IF digit >= Base THEN EXIT; END;
	 n := Base * n + digit;
	 ch := ReadC (f);
      END;
      DEC (BufferPool [f].BufferIndex);
      RETURN n;
   END ReadN;

PROCEDURE ReadS		(f: tFile; VAR s: ARRAY OF CHAR);
   VAR i	: CARDINAL;			(* string		*)
   BEGIN
      WITH BufferPool [f] DO
	 FOR i := 0 TO HIGH (s) DO
	    IF BufferIndex = BytesRead THEN	(* ReadC inline		*)
	       FillBuffer (f);
	       IF EndOfFile THEN Buffer^ [1] := 0C; END;
	    END;
	    INC (BufferIndex);
	    s [i] := Buffer^ [BufferIndex];
	 END;
      END;
   END ReadS;

PROCEDURE ReadShort	(f: tFile): SHORTINT;	(* shortint number	*)
   BEGIN
      RETURN ReadI (f);
   END ReadShort;

PROCEDURE ReadLong	(f: tFile): LONGINT;	(* longint  number	*)
   BEGIN
      RETURN ReadI (f);
   END ReadLong;

PROCEDURE ReadCard	(f: tFile): CARDINAL;	(* cardinal number	*)
   BEGIN
      RETURN ReadI (f);
   END ReadCard;

PROCEDURE ReadNl	(f: tFile);		(* new line		*)
   BEGIN
      REPEAT
      UNTIL ReadC (f) = EolCh;
   END ReadNl;

PROCEDURE UnRead	(f: tFile);		(* backspace 1 char.	*)
   BEGIN
      DEC (BufferPool [f].BufferIndex);
   END UnRead;


PROCEDURE EndOfLine	(f: tFile): BOOLEAN;	(* end of line ?	*)
   VAR ch : CHAR;
   BEGIN
      WITH BufferPool [f] DO
	 IF BufferIndex = BytesRead THEN
	    FillBuffer (f);
	    IF EndOfFile THEN Buffer^ [1] := 0C; END;
	 END;
	 RETURN Buffer^ [BufferIndex + 1] = EolCh;
      END;
   END EndOfLine;

PROCEDURE EndOfFile	(f: tFile): BOOLEAN;	(* end of file ?	*)
   VAR ch : CHAR;
   BEGIN
      ch := ReadC (f);
      DEC (BufferPool [f].BufferIndex);
      RETURN BufferPool [f].EndOfFile;
   END EndOfFile;


PROCEDURE CheckFlushLine (f: tFile);
   BEGIN
      BufferPool [f].FlushLine := System.IsCharacterSpecial (f);
   END CheckFlushLine;

PROCEDURE WriteOpen	(VAR FileName: ARRAY OF CHAR): tFile;
   VAR						(* open  output file	*)
      f		: tFile;
   BEGIN
      f := System.OpenOutput (FileName);
      WITH BufferPool [f] DO
	 Buffer		:= Alloc (BufferSize + 1);
	 BufferIndex	:= 0;
	 OpenForOutput	:= TRUE;
      END;
      CheckFlushLine (f);
      RETURN f;
   END WriteOpen;

PROCEDURE WriteClose	(f: tFile);		(* close output file	*)
   BEGIN
      WriteFlush (f);
      System.Close (f);
      WITH BufferPool [f] DO
	 Free (BufferSize + 1, Buffer);
	 Buffer := NIL;
      END;
   END WriteClose;

PROCEDURE WriteFlush	(f: tFile);		(* flush output buffer	*)
   BEGIN
      WITH BufferPool [f] DO
	 BytesRead := System.Write (f, ADR (Buffer^ [1]), BufferIndex);
	 BufferIndex := 0;
      END;
   END WriteFlush;

PROCEDURE Write		(f: tFile; Buffer: ADDRESS; Size: INTEGER): INTEGER;
   VAR						(* binary		*)
      BufferPtr : POINTER TO ARRAY [0 .. 100000000] OF CHAR;
      i		: INTEGER;
   BEGIN
      BufferPtr := Buffer;
      WITH BufferPool [f] DO
	 FOR i := 0 TO Size - 1 DO
	    INC (BufferIndex);			(* WriteC inline	*)
	    Buffer^ [BufferIndex] := BufferPtr^ [i];
	    IF (BufferIndex = BufferSize) THEN WriteFlush (f); END;
	 END;
      END;
      RETURN Size;
   END Write;

PROCEDURE WriteC	(f: tFile; c: CHAR);	(* character		*)
   BEGIN
      WITH BufferPool [f] DO
	 INC (BufferIndex);
	 Buffer^ [BufferIndex] := c;
	 IF (BufferIndex = BufferSize) OR FlushLine AND (c = EolCh) THEN
	    WriteFlush (f);
	 END;
      END;
   END WriteC;

PROCEDURE WriteI	(f: tFile; n: INTEGER ; FieldWidth: CARDINAL);
   VAR						(* integer  number	*)
      i		: INTEGER;
      length	: CARDINAL;
      negative	: CARDINAL;
      digits	: ARRAY [0 .. 10] OF CHAR;
   BEGIN
      IF n < 0 THEN
	 negative := 1;
	 n := - n;
      ELSE
	 negative := 0;
      END;
      length := 0;
      REPEAT
	 INC (length);
	 digits [length] := MyCHR [n MOD 10];
	 n := n DIV 10;
      UNTIL n = 0;
      FOR i := 1 TO INTEGER (FieldWidth - length - negative) DO
	 WriteC (f, ' ');
      END;
      IF negative = 1 THEN WriteC (f, '-'); END;
      FOR i := INTEGER (length) TO 1 BY -1 DO
	 WriteC (f, digits [i]);
      END;
   END WriteI;

PROCEDURE WriteR	(f: tFile; n: REAL; Before, After, Exp: CARDINAL);
   CONST					(* real   number	*)
      StartIndex	= 100;
   VAR
      i			: CARDINAL;
      j			: INTEGER;
      FirstDigit	: CARDINAL;
      IntegerDigits	: CARDINAL;
      TotalDigits	: CARDINAL;
      IsNegative	: CARDINAL;
      Digits		: ARRAY [0 .. 200] OF CARDINAL;
      MaxCard		: REAL;
      MaxCardDiv10	: REAL;
      Mantissa		: LONGCARD;
      Exponent		: INTEGER;
   BEGIN
      MaxCard		:= FLOAT (MaxInt);
      MaxCardDiv10	:= FLOAT (MaxIntDiv10);

      IF n < 0.0 THEN				(* determine sign	*)
	 IsNegative := 1;
	 n := - n;
      ELSE
	 IsNegative := 0;
      END;

      IF n = 0.0 THEN		(* determine mantissa and exponent	*)
	 Mantissa := 0;
	 Exponent := 1;
      ELSE
	 Exponent := 10;			(* normalize mantissa	*)
	 WHILE n > MaxCard DO
	    n := n / 10.0;
	    INC (Exponent);
	 END;
	 WHILE n <= MaxCardDiv10 DO
	    n := n * 10.0;
	    DEC (Exponent);
	 END;
	 Mantissa := TRUNC (n);
	 IF Mantissa < MaxPow10 THEN
	    DEC (Exponent);
	 END;
      END;
      						(* determine size of:	*)
      IF (Exp > 0) OR (Exponent <= 0) THEN	(* integer part		*)
	 IntegerDigits := 1;
      ELSE
	 IntegerDigits := Exponent;
      END;
      IF After = 0 THEN After := 1; END;	(* fractional part	*)
      TotalDigits := IntegerDigits + After;	(* total # of digits	*)

      FirstDigit := StartIndex;			(* convert mantissa	*)
      REPEAT
	 DEC (FirstDigit);
	 Digits [FirstDigit] := Mantissa MOD 10;
	 Mantissa := Mantissa DIV 10;
      UNTIL Mantissa = 0;
      IF Exp = 0 THEN				(* add leading zeroes	*)
	 FOR j := 1 TO 1 - Exponent DO
	    DEC (FirstDigit);
	    Digits [FirstDigit] := 0;
	 END;
      END;
      FOR i := StartIndex TO FirstDigit + TotalDigits DO
	 Digits [i] := 0;			(* add trailing zeroes	*)
      END;

      Digits [FirstDigit - 1] := 0;		(* round mantissa	*)
      IF Digits [FirstDigit + TotalDigits] >= 5 THEN
	 i := FirstDigit + TotalDigits - 1;
	 WHILE Digits [i] = 9 DO		(* carry		*)
	    Digits [i] := 0;
	    DEC (i);
	 END;
	 INC (Digits [i]);
	 IF i = FirstDigit - 1 THEN (* carry at most significant pos.	*)
	    FirstDigit := i;
	    IF Exp > 0 THEN
	       INC (Exponent);
	    ELSIF Exponent > 0 THEN
	       INC (IntegerDigits);
	    END;
	 END;
      END;
						(* print mantissa	*)
      FOR j := 1 TO INTEGER (Before - IsNegative - IntegerDigits) DO
	 WriteC (f, ' ');			(* leading spaces	*)
      END;
      IF IsNegative = 1 THEN WriteC (f, '-'); END;	(* sign		*)
      FOR i :=  1 TO IntegerDigits DO		(* integer part		*)
	 WriteC (f, MyCHR [Digits [FirstDigit]]);
	 INC (FirstDigit);
      END;
      WriteC (f, '.');				(* decimal point	*)
      FOR i :=  1 TO After DO			(* fractional part	*)
	 WriteC (f, MyCHR [Digits [FirstDigit]]);
	 INC (FirstDigit);
      END;

      IF Exp > 0 THEN				(* print exponent	*)
	 DEC (Exponent);
	 WriteC (f, 'E');
	 IF Exponent < 0 THEN
	    WriteC (f, '-');
	    Exponent := - Exponent;
	 ELSE
	    WriteC (f, '+');
	 END;
	 WriteN (f, Exponent, Exp - 1, 10);
      END;
   END WriteR;

PROCEDURE WriteB	(f: tFile; b: BOOLEAN);	(* boolean		*)
   BEGIN
      IF b
      THEN WriteC (f, 'T');
      ELSE WriteC (f, 'F');
      END;
   END WriteB;

PROCEDURE WriteN	(f: tFile; n: LONGCARD; FieldWidth, Base: CARDINAL);
   VAR						(* number of base 'Base'*)
      i		: INTEGER;
      length	: CARDINAL;
      digits	: ARRAY [0 .. 32] OF CHAR;
   BEGIN
      length := 0;
      REPEAT
	 INC (length);
	 digits [length] := MyCHR [n MOD Base];
	 n := n DIV Base;
      UNTIL n = 0;
      FOR i := 1 TO INTEGER (FieldWidth - length) DO
	 WriteC (f, '0');
      END;
      FOR i := INTEGER (length) TO 1 BY -1 DO
	 WriteC (f, digits [i]);
      END;
   END WriteN;

PROCEDURE WriteS	(f: tFile; VAR s: ARRAY OF CHAR); 
   VAR i	: CARDINAL;			(* string		*)
   VAR c	: CHAR;
   BEGIN
      WITH BufferPool [f] DO
	 FOR i := 0 TO HIGH (s) DO
	    c := s [i];
	    IF c = 0C THEN RETURN; END;
	    INC (BufferIndex);			(* WriteC inline	*)
	    Buffer^ [BufferIndex] := c;
	    IF (BufferIndex = BufferSize) OR FlushLine AND (c = EolCh) THEN
	       WriteFlush (f);
	    END;
	 END;
      END;
   END WriteS;

PROCEDURE WriteShort	(f: tFile; n: SHORTINT; FieldWidth: CARDINAL);
   BEGIN					(* shortint number	*)
      WriteI (f, n, FieldWidth);
   END WriteShort;

PROCEDURE WriteLong	(f: tFile; n: LONGINT ; FieldWidth: CARDINAL);
   BEGIN					(* longint  number	*)
      WriteI (f, n, FieldWidth);
   END WriteLong;

PROCEDURE WriteCard	(f: tFile; n: CARDINAL; FieldWidth: CARDINAL);
   VAR						(* cardinal number	*)
      i		: INTEGER;
      length	: CARDINAL;
      digits	: ARRAY [0 .. 10] OF CHAR;
   BEGIN
      length := 0;
      REPEAT
	 INC (length);
	 digits [length] := MyCHR [n MOD 10];
	 n := n DIV 10;
      UNTIL n = 0;
      FOR i := 1 TO INTEGER (FieldWidth - length) DO
	 WriteC (f, ' ');
      END;
      FOR i := INTEGER (length) TO 1 BY -1 DO
	 WriteC (f, digits [i]);
      END;
   END WriteCard;

PROCEDURE WriteNl	(f: tFile);		(* new line		*)
   BEGIN
      WriteC (f, EolCh);
   END WriteNl;


PROCEDURE CloseIO;				(* close all files	*)
   VAR i	: tFile;
   BEGIN
      FOR i := 0 TO System.cMaxFile DO
	 WITH BufferPool [i] DO
	    IF Buffer # NIL THEN
	       IF OpenForOutput THEN
		  WriteClose (i);
	       ELSE
		  ReadClose (i);
	       END;
	    END;
	 END;
      END;
   END CloseIO;

BEGIN
   MyCHR [ 0] := '0';
   MyCHR [ 1] := '1';
   MyCHR [ 2] := '2';
   MyCHR [ 3] := '3';
   MyCHR [ 4] := '4';
   MyCHR [ 5] := '5';
   MyCHR [ 6] := '6';
   MyCHR [ 7] := '7';
   MyCHR [ 8] := '8';
   MyCHR [ 9] := '9';
   MyCHR [10] := 'A';
   MyCHR [11] := 'B';
   MyCHR [12] := 'C';
   MyCHR [13] := 'D';
   MyCHR [14] := 'E';
   MyCHR [15] := 'F';

   FOR i := 0 TO System.cMaxFile DO
      WITH BufferPool [i] DO
	 Buffer		:= NIL;
	 BufferIndex	:= 0;
	 BytesRead	:= 0;
	 OpenForOutput	:= FALSE;
	 EndOfFile	:= FALSE;
	 FlushLine	:= FALSE;
      END;
   END;

   BufferPool [StdInput ].Buffer := Alloc (BufferSize + 1);
   BufferPool [StdOutput].Buffer := Alloc (BufferSize + 1);
   BufferPool [StdError ].Buffer := Alloc (BufferSize + 1);

   BufferPool [StdInput ].OpenForOutput := FALSE;
   BufferPool [StdOutput].OpenForOutput := TRUE;
   BufferPool [StdError ].OpenForOutput := TRUE;

   CheckFlushLine (StdInput );
   CheckFlushLine (StdOutput);
   CheckFlushLine (StdError );
END IO.
