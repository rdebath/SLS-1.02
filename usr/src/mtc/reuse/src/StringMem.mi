(* $Id: StringMem.mi,v 1.4 1991/11/21 14:33:17 grosch rel $ *)

(* $Log: StringMem.mi,v $
 * Revision 1.4  1991/11/21  14:33:17  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.3  91/06/07  12:19:59  grosch
 * decreased bounds of flexible arrays
 * 
 * Revision 1.2  91/06/07  11:38:35  grosch
 * increased bounds of flexible arrays
 * removed length restriction from WriteString
 * 
 * Revision 1.1  89/01/21  23:03:01  grosch
 * added file parameter to procedure WriteString
 * 
 * Revision 1.0  88/10/04  11:47:18  grosch
 * Initial revision
 * 
 *)

(* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 *)

IMPLEMENTATION MODULE StringMem;

FROM SYSTEM	IMPORT TSIZE;
FROM DynArray	IMPORT MakeArray, ExtendArray;
FROM Strings	IMPORT tStringIndex, tString;
FROM IO		IMPORT tFile, StdOutput, WriteC, WriteI, WriteNl, WriteS;

CONST InitialMemorySize	= 1024 * 16;

TYPE Memory		= ARRAY [0 .. 100000000] OF CHAR;

VAR
   MemoryPtr		: POINTER TO Memory;
   MemorySize		: LONGINT;
   MemorySpaceLeft	: LONGINT;
   MemoryFreePtr	: LONGINT;

PROCEDURE PutString (VAR s: tString): tStringRef;
   VAR
      NeededSpace	: LONGINT;
      OldMemorySize	: LONGINT;
      StartPtr		: LONGINT;
      i			: tStringIndex;
   BEGIN
      NeededSpace := s.Length + 2;
      WHILE MemorySpaceLeft < NeededSpace DO
	 OldMemorySize := MemorySize;
	 ExtendArray (MemoryPtr, MemorySize, TSIZE(CHAR));
	 INC (MemorySpaceLeft, MemorySize - OldMemorySize);
      END;
      StartPtr := MemoryFreePtr;
      MemoryPtr^[MemoryFreePtr] := CHR (s.Length DIV 256);
      INC (MemoryFreePtr);
      MemoryPtr^[MemoryFreePtr] := CHR (s.Length MOD 256);
      INC (MemoryFreePtr);
      FOR i := 1 TO s.Length DO
	 MemoryPtr^[MemoryFreePtr] := s.Chars [i];
	 INC (MemoryFreePtr);
      END;
      DEC (MemorySpaceLeft, NeededSpace);
      RETURN StartPtr;
   END PutString;

PROCEDURE GetString (r: tStringRef; VAR s: tString);
   VAR i	: tStringIndex;
   BEGIN
      s.Length := Length (r);
      INC (r, 2);
      FOR i := 1 TO s.Length DO
	 s.Chars [i] := MemoryPtr^[r];
	 INC (r);
      END;
   END GetString;

PROCEDURE Length (r: tStringRef): CARDINAL;
   BEGIN
      RETURN ORD (MemoryPtr^[r]) * 256 + ORD (MemoryPtr^[r+1]);
   END Length;

PROCEDURE IsEqual (r: tStringRef; VAR s: tString): BOOLEAN;
   VAR i	: tStringIndex;
   BEGIN
      INC (r, 2);
      FOR i := 1 TO s.Length DO
	 IF MemoryPtr^[r] # s.Chars [i] THEN RETURN FALSE; END;
	 INC (r);
      END;
      RETURN TRUE;
   END IsEqual;

PROCEDURE WriteString (f: tFile; r: tStringRef);
   VAR i	: tStringRef;
   BEGIN
      FOR i := r + 2 TO r + 1 + tStringRef (Length (r)) DO
	 WriteC (f, MemoryPtr^[i]);
      END;
   END WriteString;

PROCEDURE WriteStringMemory;
   VAR
      StringPtr	: LONGINT;
      sLength	: LONGINT;
   BEGIN
      StringPtr := 0;
      WHILE StringPtr < MemoryFreePtr DO
	 WriteI (StdOutput, StringPtr, 5);
	 WriteC (StdOutput, ' ');
	 WriteString (StdOutput, StringPtr);
	 WriteNl (StdOutput);
	 sLength := Length (StringPtr) + 2;	(* damned MODULA *)
	 INC (StringPtr, sLength);
      END;
      WriteNl (StdOutput);
      WriteI (StdOutput, StringPtr, 5);
      WriteS (StdOutput, " Bytes");
      WriteNl (StdOutput);
   END WriteStringMemory;

PROCEDURE InitStringMemory;
   BEGIN
      MemorySpaceLeft	:= MemorySize;
      MemoryFreePtr 	:= 0;
   END InitStringMemory;

BEGIN
   MemorySize		:= InitialMemorySize;
   MakeArray (MemoryPtr, MemorySize, TSIZE (CHAR));
   InitStringMemory;
END StringMem.
