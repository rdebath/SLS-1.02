(* $Id: MemoryDrv.mi,v 1.2 1991/11/21 14:33:17 grosch rel $ *)

(* $Log: MemoryDrv.mi,v $
 * Revision 1.2  1991/11/21  14:33:17  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.1  90/03/02  17:35:01  grosch
 * improved output behaviour using WriteFlush
 * 
 * Revision 1.0  88/10/04  11:47:12  grosch
 * Initial revision
 * 
 *)

(* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 *)

MODULE MemoryDrv;

FROM SYSTEM	IMPORT ADDRESS, WORD;
FROM Memory	IMPORT Alloc, Free, MemoryUsed;
FROM IO		IMPORT StdOutput, WriteS, WriteI, WriteLong, WriteN, WriteNl, CloseIO;

VAR
   p1, p2, p3, p4	: ADDRESS;
   i			: CARDINAL;
   small, best, notbest, large: LONGINT;

PROCEDURE AllocPrint (n: LONGINT): ADDRESS;
VAR a: ADDRESS;
BEGIN
   a := Alloc (n);
   WriteS (StdOutput, "Alloc:  n = ");
   WriteLong (StdOutput, n, 10);
   WriteS (StdOutput, ", ADR = ");
   WriteN (StdOutput, INTEGER (a), 8, 16);
   WriteNl (StdOutput);
   RETURN a;
END AllocPrint;

BEGIN
   FOR i := 0 TO 62 DO
      p1 := AllocPrint (i);
      p2 := AllocPrint (i);
      Free (i, p1);
      Free (i, p2);
      p3 := AllocPrint (i);
      p4 := AllocPrint (i);

      IF p3 # p2 THEN
	 WriteS (StdOutput, "Alloc/Free small not inverse ");
	 WriteI (StdOutput, i, 10);
	 WriteNl (StdOutput);
      END;

      IF p4 # p1 THEN
	 WriteS (StdOutput, "Alloc/Free small not inverse ");
	 WriteI (StdOutput, i, 10);
	 WriteNl (StdOutput);
      END;
   END;

   small	:= 80;
   best		:= 96;
   notbest	:= 112;
   large	:= 128;

   LOOP
      FOR i := 7 TO 24 (* 32 *) DO
	 WriteS (StdOutput, "        i = ");
	 WriteI (StdOutput, i, 10);
	 WriteNl (StdOutput);

	 p1 := AllocPrint (small);
	 p2 := AllocPrint (best);
	 p3 := AllocPrint (notbest);
	 p4 := AllocPrint (large);

	 IF (p1 = NIL) OR (p2 = NIL) OR (p3 = NIL) OR (p4 = NIL) THEN EXIT; END;

	 Free (large	, p4);
	 Free (notbest	, p3);
	 Free (best	, p2);
	 Free (small	, p1);

	 p1 := AllocPrint (best);
	 IF p1 # p2 THEN
	    WriteS (StdOutput, "Alloc/Free large not inverse ");
	    WriteI (StdOutput, i, 10);
	    WriteNl (StdOutput);
	 END;

	 p1 := AllocPrint (best);
	 IF p1 # p3 THEN
	    WriteS (StdOutput, "Alloc/Free large not inverse ");
	    WriteI (StdOutput, i, 10);
	    WriteNl (StdOutput);
	 END;

	 p1 := AllocPrint (best);
	 IF p1 # p4 THEN
	    WriteS (StdOutput, "Alloc/Free large not inverse ");
	    WriteI (StdOutput, i, 10);
	    WriteNl (StdOutput);
	 END;
	  
	 INC (small, small);
	 INC (best, best);
	 INC (notbest, notbest);
	 INC (large, large);
      END;
   END;
   WriteNl (StdOutput);
   WriteS (StdOutput, "Memory used: ");
   WriteI (StdOutput, MemoryUsed, 10);
   WriteNl (StdOutput);
   CloseIO;
END MemoryDrv.
