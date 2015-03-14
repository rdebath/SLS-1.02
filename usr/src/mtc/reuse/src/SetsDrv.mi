(* $Id: SetsDrv.mi,v 1.5 1992/09/24 13:05:19 grosch rel $ *)

(* $Log: SetsDrv.mi,v $
 * Revision 1.5  1992/09/24  13:05:19  grosch
 * adaption to MS-DOS
 *
 * Revision 1.4  1992/03/24  13:32:56  grosch
 * added test of Intersection
 *
 * Revision 1.3  1991/11/21  14:33:17  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.2  90/05/30  17:10:10  grosch
 * added test of Complement
 * 
 * Revision 1.1  89/01/09  17:13:47  grosch
 * added functions Size, Minimum, and Maximum
 * 
 * Revision 1.0  88/10/04  11:47:15  grosch
 * Initial revision
 * 
 *)

(* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 *)

MODULE SetsDrv;

FROM Sets	IMPORT
   tSet		, MakeSet	, ReleaseSet	, Union		,
   Difference	, Intersection	, SymDiff	, Include	,
   Exclude	, Card		, Select	, Extract	,
   Size		, Minimum	, Maximum	,
   IsSubset	, IsStrictSubset, IsEqual	, IsNotEqual	,
   IsElement	, IsEmpty	, Forall	, Exists	,
   Exists1	, Assign	, AssignElmt	, AssignEmpty	,
   ForallDo	, ReadSet	, WriteSet	, Complement	;

FROM IO		IMPORT
   tFile	, StdOutput	, WriteOpen	, WriteClose	,
   ReadOpen	, ReadClose	, WriteNl	, WriteI	,
   CloseIO	, WriteS	, WriteFlush	, ReadI		,
   StdInput	;

CONST
   max		= 1000;

VAR
   s, t, u	: tSet;
   i		: CARDINAL;
   f		: tFile;

BEGIN
   MakeSet (s, max);
   MakeSet (t, max);
   MakeSet (u, max);

   FOR i := 2 TO max DO
      Include (t, i);
   END;

   AssignEmpty (s);
   AssignElmt (s, 1);
   Assign (u, t);
   Union (s, t);

   AssignEmpty (t);
   FOR i := 0 TO max BY 2 DO
      Include (t, i);
   END;
   Difference (s, t);

   FOR i := 0 TO max BY 3 DO
      Exclude (s, i);
   END;
   FOR i := 0 TO max BY 5 DO
      Exclude (s, i);
   END;
   FOR i := 0 TO max BY 7 DO
      Exclude (s, i);
   END;
   FOR i := 0 TO max BY 11 DO
      Exclude (s, i);
   END;
   FOR i := 0 TO max BY 13 DO
      Exclude (s, i);
   END;
   FOR i := 0 TO max BY 17 DO
      Exclude (s, i);
   END;
   FOR i := 0 TO max BY 19 DO
      Exclude (s, i);
   END;
   FOR i := 0 TO max BY 23 DO
      Exclude (s, i);
   END;
   FOR i := 0 TO max BY 29 DO
      Exclude (s, i);
   END;

   f := WriteOpen ("t");
   WriteSet (f, s);
   WriteNl  (f);
   WriteClose (f);

   f := ReadOpen ("t");
   ReadSet (f, t);
   ReadClose (f);

   WriteSet (StdOutput, t);
   WriteNl  (StdOutput);
   WriteI   (StdOutput, Size (t), 5);
   WriteI   (StdOutput, Card (t), 5);
   WriteI   (StdOutput, Minimum (t), 5);
   WriteI   (StdOutput, Maximum (t), 5);
   WriteNl  (StdOutput);

   AssignEmpty (u);
   FOR i := 7 TO max BY 10 DO
      Include (u, i);
   END;
   WriteSet (StdOutput, u);
   WriteNl  (StdOutput);
   WriteI   (StdOutput, Size (u), 5);
   WriteI   (StdOutput, Card (u), 5);
   WriteI   (StdOutput, Minimum (u), 5);
   WriteI   (StdOutput, Maximum (u), 5);
   WriteNl  (StdOutput);

   Intersection (u, t);
   WriteSet (StdOutput, u);
   WriteNl  (StdOutput);
   WriteI   (StdOutput, Size (u), 5);
   WriteI   (StdOutput, Card (u), 5);
   WriteI   (StdOutput, Minimum (u), 5);
   WriteI   (StdOutput, Maximum (u), 5);
   WriteNl  (StdOutput);

   ReleaseSet (s);
   ReleaseSet (t);
   ReleaseSet (u);

   MakeSet	(s, 10);
   Include	(s, 3);
   Include	(s, 7);
   WriteNl	(StdOutput);
   WriteS 	(StdOutput, "enter Size and Set like below! (Size=0 terminates)");
   WriteNl	(StdOutput);
   WriteS	(StdOutput, "10 ");
   WriteSet	(StdOutput, s);
   WriteNl	(StdOutput);
   ReleaseSet	(s);

   LOOP
      WriteNl	(StdOutput);
      WriteFlush(StdOutput);
      i := ReadI(StdInput);
      IF i = 0 THEN EXIT; END;
      MakeSet	(s, i);
      ReadSet	(StdInput, s);
      WriteSet	(StdOutput, s);
      WriteS	(StdOutput, " Card = "); WriteI (StdOutput, Card (s), 0); WriteNl (StdOutput);
      Complement(s);
      WriteSet	(StdOutput, s);
      WriteS	(StdOutput, " Card = "); WriteI (StdOutput, Card (s), 0); WriteNl (StdOutput);
      ReleaseSet(s);
   END;
   CloseIO;
END SetsDrv.
