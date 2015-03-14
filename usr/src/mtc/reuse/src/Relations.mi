(* $Id: Relations.mi,v 1.4 1991/11/21 14:33:17 grosch rel $ *)

(* $Log: Relations.mi,v $
Revision 1.4  1991/11/21  14:33:17  grosch
new version of RCS on SPARC

Revision 1.3  91/06/07  12:19:57  grosch
decreased bounds of flexible arrays

Revision 1.2  91/06/07  11:37:47  grosch
increased bounds of flexible arrays

Revision 1.1  90/06/11  10:40:59  grosch
added procedure GetCyclics

Revision 1.0  89/11/02  18:25:01  grosch
Initial revision

 *)

(* Ich, Doktor Josef Grosch, Informatiker, 8.1.1988 *)

IMPLEMENTATION MODULE Relations;

FROM SYSTEM	IMPORT TSIZE;
FROM IO		IMPORT tFile, ReadI, ReadC, WriteI, WriteC;
FROM DynArray	IMPORT MakeArray, ReleaseArray;
FROM Sets	IMPORT tSet;

IMPORT Sets;

VAR i, j	: SHORTCARD;

PROCEDURE MakeRelation	(VAR Rel: tRelation; Size1, Size2: INTEGER);
   VAR ElmtCount : LONGINT;
   BEGIN
      Rel.Size1 := Size1;
      Rel.Size2 := Size2;
      ElmtCount := Size1 + 1;
      MakeArray (Rel.ArrayPtr, ElmtCount, TSIZE (tSet));
      FOR i := 0 TO Rel.Size1 DO
	 Sets.MakeSet (Rel.ArrayPtr^[i], Size2);
      END;
   END MakeRelation;

PROCEDURE ReleaseRelation (VAR Rel: tRelation);
   VAR ElmtCount : LONGINT;
   BEGIN
      FOR i := 0 TO Rel.Size1 DO
	 Sets.ReleaseSet (Rel.ArrayPtr^[i]);
      END;
      ElmtCount := Rel.Size1 + 1;
      ReleaseArray (Rel.ArrayPtr, ElmtCount, TSIZE (tSet));
   END ReleaseRelation;

PROCEDURE Include	(VAR Rel: tRelation; e1, e2: INTEGER);
   BEGIN
      Sets.Include (Rel.ArrayPtr^[e1], e2);
   END Include;

PROCEDURE Exclude	(VAR Rel: tRelation; e1, e2: INTEGER);
   BEGIN
      Sets.Exclude (Rel.ArrayPtr^[e1], e2);
   END Exclude;

PROCEDURE IsElement	(e1, e2: INTEGER; Rel: tRelation): BOOLEAN;
   BEGIN
      RETURN Sets.IsElement (e2, Rel.ArrayPtr^[e1]);
   END IsElement;

PROCEDURE IsRelated	(e1, e2: INTEGER; Rel: tRelation): BOOLEAN;
   BEGIN
      RETURN Sets.IsElement (e2, Rel.ArrayPtr^[e1]);
   END IsRelated;

PROCEDURE IsReflexive1	(e1: INTEGER; Rel: tRelation): BOOLEAN;
   BEGIN
      RETURN Sets.IsElement (e1, Rel.ArrayPtr^[e1]);
   END IsReflexive1;

PROCEDURE IsSymmetric1	(e1, e2: INTEGER; Rel: tRelation): BOOLEAN;
   BEGIN
      RETURN NOT Sets.IsElement (e2, Rel.ArrayPtr^[e1]) OR
                 Sets.IsElement (e1, Rel.ArrayPtr^[e2]);
   END IsSymmetric1;

PROCEDURE IsTransitive1	(e1, e2, e3: INTEGER; Rel: tRelation): BOOLEAN;
   BEGIN
      RETURN NOT (Sets.IsElement (e2, Rel.ArrayPtr^[e1]) AND
		  Sets.IsElement (e3, Rel.ArrayPtr^[e2])) OR
		  Sets.IsElement (e3, Rel.ArrayPtr^[e1]);
   END IsTransitive1;

PROCEDURE IsReflexive	(Rel: tRelation): BOOLEAN;
   BEGIN
      FOR i := 0 TO Rel.Size1 DO
	 IF NOT Sets.IsElement (i, Rel.ArrayPtr^[i]) THEN RETURN FALSE; END;
      END;
      RETURN TRUE;
   END IsReflexive;

VAR gRel	: tRelation;

PROCEDURE gSymmetric (e: CARDINAL): BOOLEAN;
   BEGIN
      RETURN Sets.IsElement (i, gRel.ArrayPtr^[e]);
   END gSymmetric;

PROCEDURE IsSymmetric	(Rel: tRelation): BOOLEAN;
   BEGIN
      gRel := Rel;
      FOR i := 0 TO Rel.Size1 DO
	 IF NOT Sets.Forall (Rel.ArrayPtr^[i], gSymmetric) THEN RETURN FALSE; END;
      END;
      RETURN TRUE;
   END IsSymmetric;

PROCEDURE IsTransitive	(Rel: tRelation): BOOLEAN;
   VAR r	: tRelation;
   VAR Result	: BOOLEAN;
   BEGIN
      MakeRelation (r, Rel.Size1, Rel.Size2);
      Assign (r, Rel);
      Closure (r);
      Result := IsEqual (r, Rel);
      ReleaseRelation (r);
      RETURN Result;
   END IsTransitive;

PROCEDURE IsEquivalence	(Rel: tRelation): BOOLEAN;
   BEGIN
      RETURN IsReflexive (Rel) AND IsSymmetric (Rel) AND IsTransitive (Rel);
   END IsEquivalence;

PROCEDURE HasReflexive	(Rel: tRelation): BOOLEAN;
   BEGIN
      FOR i := 0 TO Rel.Size1 DO
         IF Sets.IsElement (i, Rel.ArrayPtr^[i]) THEN RETURN TRUE; END;
      END;
      RETURN FALSE;
   END HasReflexive;

(*
PROCEDURE IsCyclic	(Rel: tRelation): BOOLEAN;
   VAR r	: tRelation;
   VAR Result	: BOOLEAN;
   BEGIN
      MakeRelation (r, Rel.Size1, Rel.Size2);
      Assign (r, Rel);
      Closure (r);
      Result := HasReflexive (r);
      ReleaseRelation (r);
      RETURN Result;
   END IsCyclic;
*)

TYPE PredCount		= ARRAY [0 .. 100000000] OF SHORTCARD;

VAR PredCountPtr	: POINTER TO PredCount;
VAR WithoutPred		: tSet;

PROCEDURE IsCyclic	(Rel: tRelation): BOOLEAN;
   VAR PredCountSize	: LONGINT;
   VAR WithPred		: tSet;
   VAR Result		: BOOLEAN;
   BEGIN				(* cycle test for graphs *)
      PredCountSize	:= Rel.Size1 + 1;
      MakeArray (PredCountPtr, PredCountSize, TSIZE (SHORTCARD));
      Sets.MakeSet (WithoutPred, Rel.Size1);
      Sets.MakeSet (WithPred, Rel.Size1);
      FOR i := 0 TO Rel.Size1 DO PredCountPtr^[i] := 0; END;
      FOR i := 0 TO Rel.Size1 DO
	 Sets.ForallDo (Rel.ArrayPtr^[i], gPredCount);
      END;
      FOR i := 0 TO Rel.Size1 DO
         IF PredCountPtr^[i] = 0 THEN Sets.Include (WithoutPred, i); END;
      END;
      Sets.Complement (WithPred);
      WHILE NOT Sets.IsEmpty (WithoutPred) DO
         i := Sets.Extract (WithoutPred);
         Sets.Exclude (WithPred, i);
	 Sets.ForallDo (Rel.ArrayPtr^[i], gPredCount2);
      END;
      Result := NOT Sets.IsEmpty (WithPred);
      Sets.ReleaseSet (WithoutPred);
      Sets.ReleaseSet (WithPred);
      ReleaseArray (PredCountPtr, PredCountSize, TSIZE (SHORTCARD));
      RETURN Result;
   END IsCyclic;

PROCEDURE gPredCount	(e: CARDINAL);
   BEGIN
      INC (PredCountPtr^[e]);
   END gPredCount;

PROCEDURE gPredCount2	(e: CARDINAL);
   BEGIN
      DEC (PredCountPtr^[e]);
      IF PredCountPtr^[e] = 0 THEN Sets.Include (WithoutPred, e); END;
   END gPredCount2;

PROCEDURE GetCyclics	(Rel: tRelation; VAR Set: tSet);
   VAR r	: tRelation;
   BEGIN
      MakeRelation (r, Rel.Size1, Rel.Size2);
      Assign (r, Rel);
      Closure (r);
      Sets.AssignEmpty (Set);
      FOR i := 0 TO r.Size1 DO
	 IF Sets.IsElement (i, r.ArrayPtr^[i]) THEN	(* IsReflexive *)
	    Sets.Include (Set, i);
	 END;
      END;
      ReleaseRelation (r);
   END GetCyclics;

PROCEDURE AssignEmpty	(VAR Rel: tRelation);
   BEGIN
      FOR i := 0 TO Rel.Size1 DO
	 Sets.AssignEmpty (Rel.ArrayPtr^[i]);
      END;
   END AssignEmpty;

PROCEDURE AssignElmt	(VAR Rel: tRelation; e1, e2: INTEGER);
   BEGIN
      AssignEmpty (Rel);
      Include (Rel, e1, e2);
   END AssignElmt;

PROCEDURE Assign	(VAR Rel1: tRelation; Rel2: tRelation);
   BEGIN
      FOR i := 0 TO Rel1.Size1 DO
	 Sets.Assign (Rel1.ArrayPtr^[i], Rel2.ArrayPtr^[i]);
      END;
   END Assign;

PROCEDURE Closure	(VAR Rel: tRelation);
   VAR aj	: tSet;
   BEGIN				(* Warshall *)
      WITH Rel DO
	 FOR j := 0 TO Size1 DO
	    IF NOT Sets.IsEmpty (ArrayPtr^[j]) THEN
	       aj := ArrayPtr^[j];
	       FOR i := 0 TO Size1 DO
	          IF Sets.IsElement (j, ArrayPtr^[i]) THEN
		     Sets.Union (ArrayPtr^[i], aj);
	          END;
	       END;
	    END;
	 END;
      END;
   END Closure;

PROCEDURE Union		(VAR Rel1: tRelation; Rel2: tRelation);
   BEGIN
      FOR i := 0 TO Rel1.Size1 DO
	 Sets.Union (Rel1.ArrayPtr^[i], Rel2.ArrayPtr^[i]);
      END;
   END Union;

PROCEDURE Difference	(VAR Rel1: tRelation; Rel2: tRelation);
   BEGIN
      FOR i := 0 TO Rel1.Size1 DO
	 Sets.Difference (Rel1.ArrayPtr^[i], Rel2.ArrayPtr^[i]);
      END;
   END Difference;

PROCEDURE Intersection	(VAR Rel1: tRelation; Rel2: tRelation);
   BEGIN
      FOR i := 0 TO Rel1.Size1 DO
	 Sets.Intersection (Rel1.ArrayPtr^[i], Rel2.ArrayPtr^[i]);
      END;
   END Intersection;

PROCEDURE SymDiff	(VAR Rel1: tRelation; Rel2: tRelation);
   BEGIN
      FOR i := 0 TO Rel1.Size1 DO
	 Sets.SymDiff (Rel1.ArrayPtr^[i], Rel2.ArrayPtr^[i]);
      END;
   END SymDiff;

PROCEDURE Complement	(VAR Rel: tRelation);
   BEGIN
      FOR i := 0 TO Rel.Size1 DO
	 Sets.Complement (Rel.ArrayPtr^[i]);
      END;
   END Complement;

PROCEDURE IsSubset	(Rel1, Rel2: tRelation): BOOLEAN;
   BEGIN
      FOR i := 0 TO Rel1.Size1 DO
	 IF NOT Sets.IsSubset (Rel1.ArrayPtr^[i], Rel2.ArrayPtr^[i]) THEN
	    RETURN FALSE;
	 END;
      END;
      RETURN TRUE;
   END IsSubset;

PROCEDURE IsStrictSubset (Rel1, Rel2: tRelation): BOOLEAN;
   BEGIN
      RETURN IsSubset (Rel1, Rel2) AND IsNotEqual (Rel1, Rel2);
   END IsStrictSubset;

PROCEDURE IsEqual	(VAR Rel1, Rel2: tRelation): BOOLEAN;
   BEGIN
      FOR i := 0 TO Rel1.Size1 DO
	 IF NOT Sets.IsEqual (Rel1.ArrayPtr^[i], Rel2.ArrayPtr^[i]) THEN
	    RETURN FALSE;
	 END;
      END;
      RETURN TRUE;
   END IsEqual;

PROCEDURE IsNotEqual	(Rel1, Rel2: tRelation): BOOLEAN;
   BEGIN
      RETURN NOT IsEqual (Rel1, Rel2);
   END IsNotEqual;

PROCEDURE IsEmpty	(Rel: tRelation): BOOLEAN;
   BEGIN
      FOR i := 0 TO Rel.Size1 DO
	 IF NOT Sets.IsEmpty (Rel.ArrayPtr^[i]) THEN RETURN FALSE; END;
      END;
      RETURN TRUE;
   END IsEmpty;

PROCEDURE Card		(VAR Rel: tRelation): INTEGER;
   VAR n	: INTEGER;
   BEGIN
      n := 0;
      FOR i := 0 TO Rel.Size1 DO
	 INC (n, Sets.Card (Rel.ArrayPtr^[i]));
      END;
      RETURN n;
   END Card;

PROCEDURE Select	(VAR Rel: tRelation; VAR e1, e2: INTEGER);
   BEGIN
      FOR i := 0 TO Rel.Size1 DO
	 IF NOT Sets.IsEmpty (Rel.ArrayPtr^[i]) THEN
	    e1 := i;
	    e2 := Sets.Select (Rel.ArrayPtr^[i]);
	    RETURN;
	 END;
      END;
      e1 := 0;
      e2 := 0;
   END Select;

PROCEDURE Extract	(VAR Rel: tRelation; VAR e1, e2: INTEGER);
   BEGIN
      Select (Rel, e1, e2);
      Exclude (Rel, e1, e2);
   END Extract;

VAR gProc2b	: ProcOfIntIntToBool;

PROCEDURE gProc1b (e: CARDINAL): BOOLEAN;
   BEGIN
      RETURN gProc2b (i, e);
   END gProc1b;

PROCEDURE Forall	(Rel: tRelation; Proc: ProcOfIntIntToBool): BOOLEAN;
   BEGIN
      gProc2b := Proc;
      FOR i := 0 TO Rel.Size1 DO
	 IF NOT Sets.Forall (Rel.ArrayPtr^[i], gProc1b) THEN RETURN FALSE; END;
      END;
      RETURN TRUE;
   END Forall;

PROCEDURE Exists	(Rel: tRelation; Proc: ProcOfIntIntToBool): BOOLEAN;
   BEGIN
      gProc2b := Proc;
      FOR i := 0 TO Rel.Size1 DO
	 IF Sets.Exists (Rel.ArrayPtr^[i], gProc1b) THEN RETURN TRUE; END;
      END;
      RETURN FALSE;
   END Exists;

PROCEDURE Exists1	(Rel: tRelation; Proc: ProcOfIntIntToBool): BOOLEAN;
   VAR n	: INTEGER;
   BEGIN
      n := 0;
      gProc2b := Proc;
      FOR i := 0 TO Rel.Size1 DO
	 IF Sets.Exists (Rel.ArrayPtr^[i], gProc1b) THEN INC (n); END;
      END;
      RETURN n = 1;
   END Exists1;

VAR gProc2	: ProcOfIntInt;

PROCEDURE gProc1 (e: CARDINAL);
   BEGIN
      gProc2 (i, e);
   END gProc1;

PROCEDURE ForallDo	(Rel: tRelation; Proc: ProcOfIntInt);
   BEGIN
      gProc2 := Proc;
      FOR i := 0 TO Rel.Size1 DO
	 Sets.ForallDo (Rel.ArrayPtr^[i], gProc1);
      END;
   END ForallDo;

PROCEDURE ReadRelation	(f: tFile; VAR Rel: tRelation);
   VAR ch	: CHAR;
   BEGIN
      REPEAT
      UNTIL ReadC (f) = '{';
      AssignEmpty (Rel);
      LOOP
	 IF ReadC (f) = '}' THEN EXIT; END;
	 i := ReadI (f);
	 Include (Rel, i, ReadI (f));
         ch := ReadC (f);
      END;
   END ReadRelation;

VAR g	: tFile;

PROCEDURE WriteRelation	(f: tFile;     Rel: tRelation);
   BEGIN
      g := f;
      WriteC (f, '{');
      ForallDo (Rel, WritePair);
      WriteC (f, '}');
   END WriteRelation;

PROCEDURE WritePair	(e1, e2: INTEGER);
   BEGIN
      WriteC (g, ' ');
      WriteI (g, e1, 1);
      WriteC (g, ' ');
      WriteI (g, e2, 1);
      WriteC (g, ',');
   END WritePair;

END Relations.
