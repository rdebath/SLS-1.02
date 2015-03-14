(* $Id: General.mi,v 1.3 1992/01/30 13:23:29 grosch rel $ *)

(* $Log: General.mi,v $
 * Revision 1.3  1992/01/30  13:23:29  grosch
 * redesign of interface to operating system
 *
 * Revision 1.2  1991/11/21  14:33:17  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.1  90/03/02  17:36:06  grosch
 * automized handling of machine independent alignment
 * 
 * Revision 1.0  88/10/04  11:46:54  grosch
 * Initial revision
 * 
 *)

(* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 *)

(* General Subroutines: minimum, maximum, binary logarithm, and power of 2 *)

IMPLEMENTATION MODULE General;

FROM SYSTEM	IMPORT ADR;
FROM Arguments	IMPORT ArgTable, GetArgs;
FROM System	IMPORT PutArgs;

VAR
   ForAlign	: RECORD char: CHAR; longreal: LONGREAL; END;
   argc		: SHORTCARD;
   argv		: ArgTable;


(* Returns the minimum of 'a' and 'b'.		*)

PROCEDURE Min		(a, b: INTEGER)			: INTEGER;
   BEGIN
      IF a <= b THEN
	 RETURN a;
      ELSE
	 RETURN b;
      END;
   END Min;

(* Returns the maximum of 'a' and 'b'.		*)

PROCEDURE Max		(a, b: INTEGER)			: INTEGER;
   BEGIN
      IF a >= b THEN
	 RETURN a;
      ELSE
	 RETURN b;
      END;
   END Max;

(* Returns the logarithm to the base 2 of 'x'.	*)

PROCEDURE Log2		(x: LONGINT)			: CARDINAL;
   VAR y: CARDINAL;
   BEGIN
      y := 0;
      IF x >= 65536 THEN INC (y, 16); x := x DIV 65536; END;
      IF x >=   256 THEN INC (y,  8); x := x DIV   256; END;
      IF x >=    16 THEN INC (y,  4); x := x DIV    16; END;
      IF x >=     4 THEN INC (y,  2); x := x DIV     4; END;
      IF x >=     2 THEN INC (y,  1); x := x DIV     2; END;
      RETURN y;
   END Log2;

(* Returns the number of the lowest bit set in 'x'.	*)

PROCEDURE AntiLog	(x: LONGINT)			: CARDINAL;
   VAR y: CARDINAL;
   BEGIN
      y := 0;
      IF (x MOD 65536) = 0 THEN INC (y, 16); x := x DIV 65536; END;
      IF (x MOD   256) = 0 THEN INC (y,  8); x := x DIV   256; END;
      IF (x MOD    16) = 0 THEN INC (y,  4); x := x DIV    16; END;
      IF (x MOD     4) = 0 THEN INC (y,  2); x := x DIV     4; END;
      IF (x MOD     2) = 0 THEN INC (y,  1); x := x DIV     2; END;
      RETURN y;
   END AntiLog;

(* Returns 2 to the power of 'x'.		*)

PROCEDURE Exp2		(x: CARDINAL)			: LONGINT;
   VAR y: LONGINT;
   BEGIN
      y := 1;
      IF x >= 16 THEN DEC (x, 16); y := y * 65536; END;
      IF x >=  8 THEN DEC (x,  8); y := y *   256; END;
      IF x >=  4 THEN DEC (x,  4); y := y *    16; END;
      IF x >=  2 THEN DEC (x,  2); y := y *     4; END;
      IF x >=  1 THEN DEC (x,  1); y := y *     2; END;
      RETURN y;
   END Exp2;

(* Returns 10 to the power of 'x'.		*)

PROCEDURE Exp10		(x: INTEGER)			: REAL;
   VAR
      y		: REAL;
      negative	: BOOLEAN;
   BEGIN
      negative := x < 0;
      x := ABS (x);
      y := 1.0;
   (* IF x >= 64 THEN DEC (x, 64); y := y * 1.0E64; END; too big *)
   (* IF x >= 32 THEN DEC (x, 32); y := y * 1.0E32; END; PCS10 compatibility *)
      IF x >= 16 THEN DEC (x, 16); y := y * 1.0E16; END;
      IF x >= 16 THEN DEC (x, 16); y := y * 1.0E16; END;
      IF x >=  8 THEN DEC (x,  8); y := y * 1.0E8 ; END;
      IF x >=  4 THEN DEC (x,  4); y := y * 1.0E4 ; END;
      IF x >=  2 THEN DEC (x,  2); y := y * 1.0E2 ; END;
      IF x >=  1 THEN DEC (x,  1); y := y * 1.0E1 ; END;
      IF negative
      THEN RETURN 1.0 / y;
      ELSE RETURN       y;
      END;
   END Exp10;

BEGIN
   MaxAlign := CARDINAL (ADR (ForAlign.longreal)) - CARDINAL (ADR (ForAlign.char));

   AlignMasks [1] := BITSET (0FFFFFFFFH);
   AlignMasks [2] := BITSET (0FFFFFFFEH);
   AlignMasks [3] := BITSET (0FFFFFFFFH);
   AlignMasks [4] := BITSET (0FFFFFFFCH);
   AlignMasks [5] := BITSET (0FFFFFFFFH);
   AlignMasks [6] := BITSET (0FFFFFFFFH);
   AlignMasks [7] := BITSET (0FFFFFFFFH);
   AlignMasks [8] := BITSET (0FFFFFFF8H);

   GetArgs (argc, argv);
   PutArgs (argc, argv);
END General.
