(* $Id: RelDrv.mi,v 1.3 1991/11/21 14:33:17 grosch rel $ *)

(* $Log: RelDrv.mi,v $
Revision 1.3  1991/11/21  14:33:17  grosch
new version of RCS on SPARC

Revision 1.2  90/06/11  10:40:57  grosch
added procedure GetCyclics

Revision 1.1  90/03/02  17:35:02  grosch
improved output behaviour using WriteFlush

Revision 1.0  89/11/02  18:24:02  grosch
Initial revision

 *)

(* Ich, Doktor Josef Grosch, Informatiker, 8.1.1988 *)

MODULE RelDrv;

FROM IO		IMPORT StdInput, StdOutput, ReadI, ReadC, WriteI, WriteS,
   WriteFlush, WriteB, WriteNl, CloseIO;

FROM Sets	IMPORT tSet, MakeSet, ReleaseSet, WriteSet;

FROM Relations	IMPORT
   tRelation	,
   MakeRelation	,
   ReleaseRelation ,
   Include	,
   Exclude	,
   IsElement	,
   IsRelated	,
   IsReflexive1	,
   IsSymmetric1	,
   IsTransitive1	,
   IsReflexive	,
   IsSymmetric	,
   IsTransitive	,
   IsEquivalence	,
   HasReflexive	,
   IsCyclic	,
   GetCyclics	,
   Closure	,
   AssignEmpty	,
   AssignElmt	,
   Assign	,
   Union		,
   Difference	,
   Intersection	,
   SymDiff	,
   Complement	,
   IsSubset	,
   IsStrictSubset ,
   IsEqual	,
   IsNotEqual	,
   IsEmpty	,
   Card		,
   Select	,
   Extract	,
   Forall	,
   Exists	,
   Exists1	,
   ForallDo	,
   ReadRelation	,
   WriteRelation;

VAR
   r1, r2: tRelation;
   s	 : INTEGER;
   c	 : CHAR;
   s2	 : tSet;

BEGIN
   MakeRelation (r2, 10, 20);
   Include (r2, 0, 9);
   Include (r2, 9, 1);
   Include (r2, 1, 8);
   Include (r2, 8, 0);
   WriteRelation (StdOutput, r2); WriteNl (StdOutput);
   Closure (r2);
   WriteRelation (StdOutput, r2); WriteNl (StdOutput);
   ReleaseRelation (r2);

   MakeRelation (r1, 10, 20);
   Include (r1, 2, 3);
   Include (r1, 3, 4);
   WriteNl (StdOutput);
   WriteS (StdOutput, "enter Size and Relation like below! (Size=0 terminates)");
   WriteNl (StdOutput);
   WriteI (StdOutput, 4, 3); WriteS (StdOutput, " ");
   WriteRelation (StdOutput, r1);
   WriteNl (StdOutput);
   ReleaseRelation (r1);

   LOOP
      WriteNl (StdOutput);
      WriteFlush (StdOutput);
      s := ReadI (StdInput);
      IF s = 0 THEN EXIT; END;
      MakeSet (s2, s);
      MakeRelation (r2, s, s);
      ReadRelation (StdInput, r2);
      WriteRelation (StdOutput, r2); WriteNl (StdOutput);
      WriteS (StdOutput, "Reflexive	= "); WriteB (StdOutput, IsReflexive (r2)); WriteNl (StdOutput);
      WriteS (StdOutput, "Symmetric	= "); WriteB (StdOutput, IsSymmetric (r2)); WriteNl (StdOutput);
      WriteS (StdOutput, "Transitive	= "); WriteB (StdOutput, IsTransitive (r2)); WriteNl (StdOutput);
      WriteS (StdOutput, "Equivalence	= "); WriteB (StdOutput, IsEquivalence (r2)); WriteNl (StdOutput);
      WriteS (StdOutput, "HasReflexive	= "); WriteB (StdOutput, HasReflexive (r2)); WriteNl (StdOutput);
      WriteS (StdOutput, "Cyclic		= "); WriteB (StdOutput, IsCyclic (r2)); WriteNl (StdOutput);
      WriteS (StdOutput, "Card		= "); WriteI (StdOutput, Card (r2), 1); WriteNl (StdOutput);
      GetCyclics (r2, s2);
      WriteS (StdOutput, "Cyclics		= "); WriteSet (StdOutput, s2); WriteNl (StdOutput);
      Closure (r2);
      WriteRelation (StdOutput, r2); WriteNl (StdOutput);
      WriteS (StdOutput, "Reflexive	= "); WriteB (StdOutput, IsReflexive (r2)); WriteNl (StdOutput);
      WriteS (StdOutput, "Symmetric	= "); WriteB (StdOutput, IsSymmetric (r2)); WriteNl (StdOutput);
      WriteS (StdOutput, "Transitive	= "); WriteB (StdOutput, IsTransitive (r2)); WriteNl (StdOutput);
      WriteS (StdOutput, "Equivalence	= "); WriteB (StdOutput, IsEquivalence (r2)); WriteNl (StdOutput);
      WriteS (StdOutput, "HasReflexive	= "); WriteB (StdOutput, HasReflexive (r2)); WriteNl (StdOutput);
      WriteS (StdOutput, "Cyclic		= "); WriteB (StdOutput, IsCyclic (r2)); WriteNl (StdOutput);
      WriteS (StdOutput, "Card		= "); WriteI (StdOutput, Card (r2), 1); WriteNl (StdOutput);
      GetCyclics (r2, s2);
      WriteS (StdOutput, "Cyclics		= "); WriteSet (StdOutput, s2); WriteNl (StdOutput);
      ReleaseRelation (r2);
      ReleaseSet (s2);
   END;

   CloseIO;
END RelDrv.
