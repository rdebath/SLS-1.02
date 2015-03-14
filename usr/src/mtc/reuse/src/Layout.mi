(* $Id: Layout.mi,v 1.2 1991/11/21 14:33:17 grosch rel $ *)

(* $Log: Layout.mi,v $
 * Revision 1.2  1991/11/21  14:33:17  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.1  89/01/24  19:04:59  grosch
 * added procedure SkipSpaces
 * 
 * Revision 1.0  88/10/04  11:47:03  grosch
 * Initial revision
 * 
 *)

(* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 *)

IMPLEMENTATION MODULE Layout;

FROM IO	IMPORT tFile, WriteC, WriteS, WriteI, ReadC, UnRead;

PROCEDURE WriteChar (f: tFile; Ch: CHAR);
   BEGIN
      IF ('!' <= Ch) AND (Ch <= '~') THEN
	 WriteC (f, "'");
	 WriteC (f, Ch);
	 WriteC (f, "'");
      ELSIF Ch = 0C THEN
	 WriteS (f, "eps");
      ELSE
	 WriteI (f, ORD (Ch), 2);
	 WriteC (f, 'C');
      END;
   END WriteChar;

PROCEDURE WriteSpace (f: tFile);
   BEGIN
      WriteC (f, ' ');
   END WriteSpace;

PROCEDURE WriteSpaces (f: tFile; Count: INTEGER);
   VAR i	: INTEGER;
   BEGIN
      FOR i := 1 TO Count DO
	 WriteC (f, ' ');
      END;
   END WriteSpaces;

PROCEDURE ReadSpace (f: tFile);
   VAR Ch	: CHAR;
   BEGIN
      Ch := ReadC (f);
   END ReadSpace;

PROCEDURE ReadSpaces (f: tFile; Count: INTEGER);
   VAR i	: INTEGER;
   VAR Ch	: CHAR;
   BEGIN
      FOR i := 1 TO Count DO
	 Ch := ReadC (f);
      END;
   END ReadSpaces;

PROCEDURE SkipSpaces (f: tFile);
   BEGIN
      REPEAT UNTIL ReadC (f) # ' ';
      UnRead (f);
   END SkipSpaces;

END Layout.
