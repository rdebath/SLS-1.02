(* $Id: Sets.mi,v 1.4 1991/11/21 14:33:17 grosch rel $ *)

(* $Log: Sets.mi,v $
 * Revision 1.4  1991/11/21  14:33:17  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.3  90/05/30  17:08:45  grosch
 * bug fixes in Complement and ReadSet
 * 
 * Revision 1.2  89/09/20  11:50:33  grosch
 * turned many FOR into WHILE loops to increase efficiency
 * 
 * Revision 1.1  89/01/09  17:13:35  grosch
 * added functions Size, Minimum, and Maximum
 * 
 * Revision 1.0  88/10/04  11:47:13  grosch
 * Initial revision
 * 
 *)

(* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 *)

IMPLEMENTATION MODULE Sets;

FROM SYSTEM	IMPORT TSIZE;
FROM General	IMPORT Min, Max;
FROM DynArray	IMPORT MakeArray, ReleaseArray;
FROM IO		IMPORT tFile, StdError, ReadI, ReadC, WriteI, WriteC, WriteS, WriteNl;

CONST
   BitsPerBitset	= 32;
   BitsPerByte		= 8;
   BytesPerBitset	= BitsPerBitset DIV BitsPerByte;
   NoCard		= -1;

VAR
   AllBits		: BITSET;

PROCEDURE MakeSet (VAR Set: tSet; MaxSize: CARDINAL);
   VAR ElmtCount : LONGINT;
   BEGIN
      WITH Set DO
	 ElmtCount := (MaxSize + BitsPerBitset - MaxSize MOD BitsPerBitset)
	    DIV BitsPerBitset;
	 MakeArray (BitsetPtr, ElmtCount, BytesPerBitset);
	 MaxElmt := MaxSize;
	 LastBitset := ElmtCount - 1;
	 AssignEmpty (Set);
      END;
   END MakeSet;
      
PROCEDURE ReleaseSet (VAR Set: tSet);
   VAR ElmtCount : LONGINT;
   BEGIN
      ElmtCount := Set.LastBitset + 1;
      ReleaseArray (Set.BitsetPtr, ElmtCount, BytesPerBitset);
   END ReleaseSet;

PROCEDURE Union (VAR Set1: tSet; Set2: tSet);
   VAR i : CARDINAL;
   BEGIN
      WITH Set1 DO
	 i := 0;
	 WHILE i <= LastBitset DO
	    BitsetPtr^[i] := BitsetPtr^[i] + Set2.BitsetPtr^[i];
	    INC (i);
	 END;
	 Card      := NoCard;
	 FirstElmt := Min (FirstElmt, Set2.FirstElmt);
	 LastElmt  := Max (LastElmt, Set2.LastElmt);
      END;
   END Union;

PROCEDURE Difference (VAR Set1: tSet; Set2: tSet);
   VAR i : CARDINAL;
   BEGIN
      WITH Set1 DO
	 i := 0;
	 WHILE i <= LastBitset DO
	    BitsetPtr^[i] := BitsetPtr^[i] - Set2.BitsetPtr^[i];
	    INC (i);
	 END;
	 Card := NoCard;
      END;
   END Difference;

PROCEDURE Intersection (VAR Set1: tSet; Set2: tSet);
   VAR i : CARDINAL;
   BEGIN
      WITH Set1 DO
	 i := 0;
	 WHILE i <= LastBitset DO
	    BitsetPtr^[i] := BitsetPtr^[i] * Set2.BitsetPtr^[i];
	    INC (i);
	 END;
	 Card      := NoCard;
	 FirstElmt := Max (FirstElmt, Set2.FirstElmt);
	 LastElmt  := Min (LastElmt, Set2.LastElmt);
      END;
   END Intersection;

PROCEDURE SymDiff (VAR Set1: tSet; Set2: tSet);
   VAR i : CARDINAL;
   BEGIN
      WITH Set1 DO
	 i := 0;
	 WHILE i <= LastBitset DO
	    BitsetPtr^[i] := BitsetPtr^[i] / Set2.BitsetPtr^[i];
	    INC (i);
	 END;
	 Card      := NoCard;
	 FirstElmt := Min (FirstElmt, Set2.FirstElmt);
	 LastElmt  := Max (LastElmt, Set2.LastElmt);
      END;
   END SymDiff;

PROCEDURE Complement (VAR Set: tSet);
   VAR i : SHORTINT;
       s : BITSET;
   BEGIN
      WITH Set DO
	 i := 0;
	 WHILE i <= SHORTINT (LastBitset) - 1 DO
	    BitsetPtr^[i] := AllBits - BitsetPtr^[i];
	    INC (i);
	 END;
      (* ifdef MOCKA
	 BitsetPtr^[LastBitset] := {0 .. MaxElmt MOD BitsPerBitset} - BitsetPtr^[LastBitset];
	 else *)
	 s := {};
	 FOR i := 0 TO SHORTINT (MaxElmt) MOD BitsPerBitset DO
	    INCL (s, SHORTCARD (i));
	 END;
	 BitsetPtr^[LastBitset] := s - BitsetPtr^[LastBitset];
      (* endif *)
	 IF Card # NoCard THEN
	    Card   := SHORTINT (MaxElmt) + 1 - Card;
	 END;
	 FirstElmt := 0;
	 LastElmt  := MaxElmt;
      END;
   END Complement;

PROCEDURE Include (VAR Set: tSet; Elmt: CARDINAL);
   BEGIN
      WITH Set DO
	 INCL (BitsetPtr^[Elmt DIV BitsPerBitset], Elmt MOD BitsPerBitset);
	 Card      := NoCard;
      (* FirstElmt := Min (FirstElmt, Elmt);
	 LastElmt  := Max (LastElmt, Elmt); *)
	 IF FirstElmt > Elmt THEN FirstElmt := Elmt; END;
	 IF LastElmt  < Elmt THEN LastElmt  := Elmt; END;
      END;
   END Include;

PROCEDURE Exclude (VAR Set: tSet; Elmt: CARDINAL);
   BEGIN
      WITH Set DO
	 EXCL (BitsetPtr^[Elmt DIV BitsPerBitset], Elmt MOD BitsPerBitset);
	 Card      := NoCard;
	 IF (Elmt = FirstElmt) AND (Elmt < MaxElmt) THEN
	    INC (FirstElmt);
	 END;
	 IF (Elmt = LastElmt) AND (Elmt > 0) THEN
	    DEC (LastElmt);
	 END;
      END;
   END Exclude;

PROCEDURE Card (VAR Set: tSet): CARDINAL;
   VAR i : CARDINAL;
   BEGIN
      WITH Set DO
	 IF Card = NoCard THEN
	    Card := 0;
	    FOR i := FirstElmt TO LastElmt DO
	       IF IsElement (i, Set) THEN INC (Card); END;
	    END;
	 END;
	 RETURN Card;
      END;
   END Card;
    
PROCEDURE Size (VAR Set: tSet): CARDINAL;
   BEGIN
      RETURN Set.MaxElmt;
   END Size;

PROCEDURE Minimum (VAR Set: tSet): CARDINAL;
   VAR i : CARDINAL;
   BEGIN
      WITH Set DO
	 i := FirstElmt;
	 WHILE i <= LastElmt DO
	    IF IsElement (i, Set) THEN
	       FirstElmt := i;
	       RETURN i;
	    END;
	    INC (i);
	 END;
	 RETURN 0;
      END;
   END Minimum;

PROCEDURE Maximum (VAR Set: tSet): CARDINAL;
   VAR i : CARDINAL;
   BEGIN
      WITH Set DO
	 i := LastElmt;
	 WHILE i >= FirstElmt DO
	    IF IsElement (i, Set) THEN
	       LastElmt := i;
	       RETURN i;
	    END;
	    DEC (i);
	 END;
	 RETURN 0;
      END;
   END Maximum;

PROCEDURE Select (VAR Set: tSet): CARDINAL;
   BEGIN
      RETURN Minimum (Set);
   END Select;
    
PROCEDURE Extract (VAR Set: tSet): CARDINAL;
   VAR i : CARDINAL;
   BEGIN
      i := Minimum (Set);
      Exclude (Set, i);
      RETURN i;
   END Extract;

PROCEDURE IsSubset (Set1, Set2: tSet): BOOLEAN;
   VAR i : CARDINAL;
   BEGIN
      WITH Set1 DO
	 i := 0;
	 WHILE i <= LastBitset DO
	    IF NOT (BitsetPtr^[i] <= Set2.BitsetPtr^[i]) THEN RETURN FALSE; END;
	    INC (i);
	 END;
	 RETURN TRUE;
      END;
   END IsSubset;

PROCEDURE IsStrictSubset (Set1, Set2: tSet): BOOLEAN;
   BEGIN
      RETURN IsSubset (Set1, Set2) AND IsNotEqual (Set1, Set2);
   END IsStrictSubset;
    
PROCEDURE IsEqual (VAR Set1, Set2: tSet): BOOLEAN;
   VAR i : CARDINAL;
   BEGIN
      WITH Set1 DO
	 i := 0;
	 WHILE i <= LastBitset DO
	    IF BitsetPtr^[i] # Set2.BitsetPtr^[i] THEN RETURN FALSE; END;
	    INC (i);
	 END;
	 RETURN TRUE;
      END;
   END IsEqual;
    
PROCEDURE IsNotEqual (Set1, Set2: tSet): BOOLEAN;
   BEGIN
      RETURN NOT IsEqual (Set1, Set2);
   END IsNotEqual;

PROCEDURE IsElement (Elmt: CARDINAL; VAR Set: tSet): BOOLEAN;
   BEGIN
      RETURN Elmt MOD BitsPerBitset IN Set.BitsetPtr^[Elmt DIV BitsPerBitset];
   END IsElement;

PROCEDURE IsEmpty (Set: tSet): BOOLEAN;
   VAR i : CARDINAL;
   BEGIN
      WITH Set DO
	 IF FirstElmt <= LastElmt THEN
	    i := 0;
	    WHILE i <= LastBitset DO
	       IF BitsetPtr^[i] # {} THEN RETURN FALSE; END;
	       INC (i);
	    END;
	 END;
	 RETURN TRUE;
      END;
   END IsEmpty;
    
PROCEDURE Forall (Set: tSet; Proc: ProcOfCardToBool): BOOLEAN;
   VAR i : CARDINAL;
   BEGIN
      WITH Set DO
	 FOR i := FirstElmt TO LastElmt DO
	    IF IsElement (i, Set) AND NOT Proc (i) THEN RETURN FALSE; END;
	 END;
	 RETURN TRUE;
      END;
   END Forall;
    
PROCEDURE Exists (Set: tSet; Proc: ProcOfCardToBool): BOOLEAN;
   VAR i : CARDINAL;
   BEGIN
      WITH Set DO
	 FOR i := FirstElmt TO LastElmt DO
	    IF IsElement (i, Set) AND Proc (i) THEN RETURN TRUE; END;
	 END;
	 RETURN FALSE;
      END;
   END Exists;
    
PROCEDURE Exists1 (Set: tSet; Proc: ProcOfCardToBool): BOOLEAN;
   VAR i, n : CARDINAL;
   BEGIN
      WITH Set DO
	 n := 0;
	 FOR i := FirstElmt TO LastElmt DO
	    IF IsElement (i, Set) AND Proc (i) THEN INC (n); END;
	 END;
	 RETURN n = 1;
      END;
   END Exists1;

PROCEDURE Assign (VAR Set1: tSet; Set2: tSet);
   VAR i : CARDINAL;
   BEGIN
      WITH Set1 DO
	 i := 0;
	 WHILE i <= LastBitset DO
	    BitsetPtr^[i] := Set2.BitsetPtr^[i];
	    INC (i);
	 END;
	 Card      := Set2.Card;
	 FirstElmt := Set2.FirstElmt;
	 LastElmt  := Set2.LastElmt;
      END;
   END Assign;

PROCEDURE AssignElmt (VAR Set: tSet; Elmt: CARDINAL);
   BEGIN
      WITH Set DO
	 AssignEmpty (Set);
	 Include (Set, Elmt);
	 Card      := 1;
	 FirstElmt := Elmt;
	 LastElmt  := Elmt;
      END;
   END AssignElmt;

PROCEDURE AssignEmpty (VAR Set: tSet);
   VAR i : CARDINAL;
   BEGIN
      WITH Set DO
	 i := 0;
	 WHILE i <= LastBitset DO
	    BitsetPtr^[i] := {};
	    INC (i);
	 END;
	 Card      := 0;
	 FirstElmt := MaxElmt;
	 LastElmt  := 0;
      END;
   END AssignEmpty;

PROCEDURE ForallDo (Set: tSet; Proc: ProcOfCard);
   VAR i : CARDINAL;
   BEGIN
      WITH Set DO
	 FOR i := FirstElmt TO LastElmt DO
	    IF IsElement (i, Set) THEN Proc (i); END;
	 END;
      END;
   END ForallDo;

PROCEDURE ReadSet (f: tFile; VAR Set: tSet);
   VAR card	: CARDINAL;
   BEGIN
      REPEAT UNTIL ReadC (f) = '{';
      AssignEmpty (Set);
      card := 0;
      LOOP
	 IF ReadC (f) = '}' THEN EXIT; END;
	 Include (Set, ReadI (f));
	 INC (card);
      END;
      Set.Card := card;
   END ReadSet;

VAR g	: tFile;

PROCEDURE WriteSet (f: tFile; Set: tSet);
   BEGIN
      WITH Set DO
      (* WriteS (f, "MaxElmt = "	) ; WriteI (f, MaxElmt	 , 0);
	 WriteS (f, " LastBitset = "	) ; WriteI (f, LastBitset, 0);
	 WriteS (f, " Card = "		) ; WriteI (f, Card	 , 0);
	 WriteS (f, " FirstElmt = "	) ; WriteI (f, FirstElmt , 0);
	 WriteS (f, " LastElmt = "	) ; WriteI (f, LastElmt	 , 0);
	 WriteNl (f);
      *)
	 g := f;
	 WriteC (f, '{');
	 ForallDo (Set, WriteElmt);
	 WriteC (f, '}');
      END;
   END WriteSet;

PROCEDURE WriteElmt (Elmt: CARDINAL);
   BEGIN
      WriteC (g, ' ');
      WriteI (g, Elmt, 0);
   END WriteElmt;

BEGIN
   AllBits := { 0 .. BitsPerBitset - 1 };
   IF TSIZE (BITSET) # BytesPerBitset THEN
      WriteS (StdError, "TSIZE (BITSET) = ");
      WriteI (StdError, TSIZE (BITSET), 0);
      WriteNl (StdError);
   END;
END Sets.
