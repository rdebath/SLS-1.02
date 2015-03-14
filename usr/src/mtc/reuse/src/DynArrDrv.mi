(* $Id: DynArrDrv.mi,v 1.1 1991/11/21 14:33:17 grosch rel $ *)

(* $Log: DynArrDrv.mi,v $
 * Revision 1.1  1991/11/21  14:33:17  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.0  88/10/04  11:46:51  grosch
 * Initial revision
 * 
 *)

(* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 *)

MODULE DynArrDrv;

FROM DynArray	IMPORT MakeArray, ExtendArray;
FROM IO		IMPORT StdOutput, WriteC, WriteI, WriteNl, WriteS, WriteLong, CloseIO;
FROM SYSTEM	IMPORT TSIZE;

VAR  i : LONGINT;
     j : CARDINAL;

TYPE t = ARRAY [1 .. 100000] OF LONGINT;
(* necessary to force index arithmetic to be done with long integers *)

VAR  p : POINTER TO t;
     s : LONGINT;

BEGIN
   s := 10;

   MakeArray (p, s, TSIZE (LONGINT));
   FOR i := 1 TO s DO
      p^[i] := i;
   END;

   FOR j := 1 TO 13 DO
      ExtendArray (p, s, TSIZE (LONGINT));

      IF p = NIL THEN
	 WriteS (StdOutput, "Extend Error"); WriteNl (StdOutput);
      END;

      FOR i := s DIV 2 + 1 TO s DO
	 p^[i] := i;
      END;

      FOR i := 1 TO s DO
	 IF p^[i] # i THEN
	    WriteS (StdOutput, "Error j, i, p^[i] =");
	    WriteI (StdOutput, j, 5);
	    WriteLong (StdOutput, i, 5);
	    WriteLong (StdOutput, p^[i], 10);
	    WriteNl (StdOutput);
	 END;
      END;

      WriteS (StdOutput, "j, size = ");
      WriteI (StdOutput, j, 5);
      WriteLong (StdOutput, s, 10);
      WriteS (StdOutput, " ok");
      WriteNl (StdOutput);
   END;
   CloseIO;
END DynArrDrv.
