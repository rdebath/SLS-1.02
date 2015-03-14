(* $Id: Idents.mi,v 1.8 1992/06/22 14:23:18 grosch rel $ *)

(* $Log: Idents.mi,v $
 * Revision 1.8  1992/06/22  14:23:18  grosch
 * cosmetic changes
 *
 * Revision 1.7  1992/03/24  13:50:12  grosch
 * decreased array type size from 100000000 to 1000000 because of C compiler restrictions
 *
 * Revision 1.6  1991/11/21  14:33:17  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.5  91/06/07  12:19:54  grosch
 * decreased bounds of flexible arrays
 * 
 * Revision 1.4  91/06/07  11:37:45  grosch
 * increased bounds of flexible arrays
 * 
 * Revision 1.3  89/06/01  18:21:16  grosch
 * added predefined identifier NoIdent
 * 
 * Revision 1.2  89/01/25  12:05:42  grosch
 * added function MaxIdent
 * 
 * Revision 1.1  89/01/21  23:03:34  grosch
 * added file parameter to procedure WriteIdent
 * 
 * Revision 1.0  88/10/04  11:47:01  grosch
 * Initial revision
 * 
 *)

(* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 *)

IMPLEMENTATION MODULE Idents;

FROM SYSTEM	IMPORT TSIZE;
FROM DynArray	IMPORT MakeArray, ExtendArray;
FROM Strings	IMPORT tString, tStringIndex, AssignEmpty;
FROM StringMem	IMPORT tStringRef, PutString, IsEqual;
FROM IO		IMPORT tFile, StdOutput, WriteC, WriteI, WriteNl, WriteS;

IMPORT Strings, StringMem;

CONST
   cNoIdent		= 0;
   InitialTableSize	= 512;
   HashTableSize	= 256;

TYPE
   IdentTableEntry	=
      RECORD 
	 String		: tStringRef;
	 Length		: tStringIndex;
	 Collision	: tIdent;
      END;

   HashIndex		= [0 .. HashTableSize];

VAR
   TablePtr		: POINTER TO ARRAY [0 .. 1000000] OF IdentTableEntry;
   IdentTableSize	: LONGINT;
   IdentCount		: tIdent;

   HashTable		: ARRAY HashIndex OF tIdent; 
   i			: HashIndex;

PROCEDURE MakeIdent (VAR s: tString): tIdent;
   VAR
      HashTableIndex	: HashIndex;
      CurIdent		: tIdent;
      lIdentCount	: LONGINT;
   BEGIN
      WITH s DO					(* hash *)
  	 IF Length = 0 THEN
	    HashTableIndex := 0;
	 ELSE 
	    HashTableIndex := (ORD (Chars [1]) + ORD (Chars [Length]) * 11
				 + Length * 26) MOD HashTableSize;
	 END;
      END;

      CurIdent := HashTable [HashTableIndex];	(* search *)
      LOOP
	 IF CurIdent = cNoIdent THEN EXIT; END;
	 WITH TablePtr^[CurIdent] DO
	    IF (Length = s.Length) AND IsEqual (String, s) THEN
	       RETURN CurIdent;			(* found *)
	    END;  
	    CurIdent := Collision;
	 END;
      END;

      INC (IdentCount);				(* not found: enter *)
      lIdentCount := IdentCount;		(* damned MODULA *)
      IF lIdentCount = IdentTableSize THEN
	 ExtendArray (TablePtr, IdentTableSize, TSIZE (IdentTableEntry));
      END;
      WITH TablePtr^[IdentCount] DO
	 String		:= PutString (s);
	 Length		:= s.Length;
	 Collision	:= HashTable [HashTableIndex];
      END;
      HashTable [HashTableIndex] := IdentCount;
      RETURN IdentCount;
   END MakeIdent;

PROCEDURE GetString (i: tIdent; VAR s: tString);
   BEGIN
      WITH TablePtr^[i] DO 
	 StringMem.GetString (String, s);
      END;
   END GetString;

PROCEDURE GetStringRef (i: tIdent): tStringRef;
   BEGIN
      RETURN TablePtr^[i].String;
   END GetStringRef;

PROCEDURE MaxIdent (): tIdent;
   BEGIN
      RETURN IdentCount;
   END MaxIdent;

PROCEDURE WriteIdent (f: tFile; i: tIdent);
   VAR s	: tString;
   BEGIN
      GetString (i, s);
      Strings.WriteS (f, s);
   END WriteIdent;

PROCEDURE WriteIdents;
   VAR i	: CARDINAL;
   BEGIN
      FOR i := 1 TO IdentCount DO
	 WriteI (StdOutput, i, 5);
	 WriteC (StdOutput, ' ');
	 WriteIdent (StdOutput, i);
	 WriteNl (StdOutput);
      END;
   END WriteIdents;

PROCEDURE WriteHashTable;
   VAR
      CurIdent	: tIdent;
      i		: HashIndex;
      Count	: CARDINAL;
   BEGIN
      FOR i := 0 TO HashTableSize DO
	 WriteI (StdOutput, i, 5);

	 Count := 0;
	 CurIdent := HashTable [i];
	 WHILE CurIdent # cNoIdent DO
	    INC (Count);
	    CurIdent := TablePtr^[CurIdent].Collision;
	 END;
	 WriteI (StdOutput, Count, 5);

	 CurIdent := HashTable [i];
	 WHILE CurIdent # cNoIdent DO
	    WriteC (StdOutput, ' ');
	    WriteIdent (StdOutput, CurIdent);
	    CurIdent := TablePtr^[CurIdent].Collision;
	 END;
	 WriteNl (StdOutput);
      END;

      WriteNl (StdOutput);
      WriteS (StdOutput, "Idents =");
      WriteI (StdOutput, IdentCount, 5);
      WriteNl (StdOutput);
   END WriteHashTable;
    
PROCEDURE InitIdents;
   VAR String	: tString;
   BEGIN
      FOR i := 0 TO HashTableSize DO
	 HashTable [i] := cNoIdent;
      END;
      IdentCount := 0;
      AssignEmpty (String);
      NoIdent := MakeIdent (String);
   END InitIdents;

BEGIN
   IdentTableSize := InitialTableSize;
   MakeArray (TablePtr, IdentTableSize, TSIZE (IdentTableEntry));
   InitIdents;
END Idents.
