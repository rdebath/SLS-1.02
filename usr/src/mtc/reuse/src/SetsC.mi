(* $Id: SetsC.mi,v 1.3 1991/11/21 14:33:17 grosch rel $ *)

(* $Log: SetsC.mi,v $
Revision 1.3  1991/11/21  14:33:17  grosch
new version of RCS on SPARC

Revision 1.2  90/05/31  09:45:30  grosch
turned Name parameter of Check procedures into value parameter for SUN m2c

Revision 1.1  89/02/23  16:02:37  grosch
Initial revision

 *)

(* Ich, Doktor Josef Grosch, Informatiker, Feb. 1989 *)

IMPLEMENTATION MODULE SetsC;

IMPORT Sets;

FROM IO		IMPORT tFile, StdError, WriteI, WriteN, WriteS, WriteNl;

PROCEDURE CheckSetSet (VAR Set1, Set2: tSet; Name: ARRAY OF CHAR);
   BEGIN
      CheckSet (Set1, Name);
      CheckSet (Set2, Name);
      IF Set1.MaxElmt # Set2.MaxElmt THEN
	 WriteS (StdError, "Sets."); WriteS (StdError, Name); 
	 WriteS (StdError, ": incompatible sets"); WriteNl (StdError);
	 PrintSet (Set1);
	 PrintSet (Set2);
      END;
   END CheckSetSet;

PROCEDURE CheckSetElmt (VAR Set: tSet; Elmt: INTEGER; Name: ARRAY OF CHAR);
   BEGIN
      CheckSet (Set, Name);
      IF Elmt < 0 THEN
	 WriteS (StdError, "Sets."); WriteS (StdError, Name); 
	 WriteS (StdError, ": negative element: ");
	 WriteI (StdError, Elmt, 0); WriteNl (StdError);
	 PrintSet (Set);
      END;
      IF Elmt > SHORTINT (Set.MaxElmt) THEN
	 WriteS (StdError, "Sets."); WriteS (StdError, Name); 
	 WriteS (StdError, ": element out of range: ");
	 WriteI (StdError, Elmt, 0); WriteNl (StdError);
	 PrintSet (Set);
      END;
   END CheckSetElmt;

PROCEDURE CheckSet (VAR Set: tSet; Name: ARRAY OF CHAR);
   BEGIN
      IF Set.BitsetPtr = NIL THEN
	 WriteS (StdError, "Sets."); WriteS (StdError, Name); 
	 WriteS (StdError, ": set probably not initialized"); WriteNl (StdError);
	 PrintSet (Set);
      END;
   END CheckSet;

PROCEDURE CheckNotEmpty (VAR Set: tSet; Name: ARRAY OF CHAR);
   BEGIN
      CheckSet (Set, Name);
      IF Sets.IsEmpty (Set) THEN
	 WriteS (StdError, "Sets."); WriteS (StdError, Name); 
	 WriteS (StdError, ": applied to empty set"); WriteNl (StdError);
	 PrintSet (Set);
      END;
   END CheckNotEmpty;

PROCEDURE PrintSet (Set: tSet);
   BEGIN
      WITH Set DO
WriteS (StdError, "BitsetPtr = "); WriteN (StdError, INTEGER (BitsetPtr), 0, 16); WriteNl (StdError);
WriteS (StdError, "MaxElmt   = "); WriteI (StdError, MaxElmt   , 0); WriteNl (StdError);
WriteS (StdError, "LastBitset= "); WriteI (StdError, LastBitset, 0); WriteNl (StdError);
WriteS (StdError, "Card      = "); WriteI (StdError, Card      , 0); WriteNl (StdError);
WriteS (StdError, "FirstElmt = "); WriteI (StdError, FirstElmt , 0); WriteNl (StdError);
WriteS (StdError, "LastElmt  = "); WriteI (StdError, LastElmt  , 0); WriteNl (StdError);
      END;
   END PrintSet;

PROCEDURE MakeSet (VAR Set: tSet; MaxSize: CARDINAL);
   BEGIN
      Sets.MakeSet (Set, MaxSize);
      CheckSet (Set, "MakeSet");
   END MakeSet;
      
PROCEDURE ReleaseSet (VAR Set: tSet);
   BEGIN
      CheckSet (Set, "ReleaseSet");
      Sets.ReleaseSet (Set);
   END ReleaseSet;

PROCEDURE Union (VAR Set1: tSet; Set2: tSet);
   BEGIN
      CheckSetSet (Set1, Set2, "Union");
      Sets.Union (Set1, Set2);
   END Union;

PROCEDURE Difference (VAR Set1: tSet; Set2: tSet);
   BEGIN
      CheckSetSet (Set1, Set2, "Difference");
      Sets.Difference (Set1, Set2);
   END Difference;

PROCEDURE Intersection (VAR Set1: tSet; Set2: tSet);
   BEGIN
      CheckSetSet (Set1, Set2, "Intersection");
      Sets.Intersection (Set1, Set2);
   END Intersection;

PROCEDURE SymDiff (VAR Set1: tSet; Set2: tSet);
   BEGIN
      CheckSetSet (Set1, Set2, "SymDiff");
      Sets.SymDiff (Set1, Set2);
   END SymDiff;

PROCEDURE Complement (VAR Set: tSet);
   BEGIN
      CheckSet (Set, "Complement");
      Sets.Complement (Set);
   END Complement;

PROCEDURE Include (VAR Set: tSet; Elmt: CARDINAL);
   BEGIN
      CheckSetElmt (Set, Elmt, "Include");
      Sets.Include (Set, Elmt);
   END Include;

PROCEDURE Exclude (VAR Set: tSet; Elmt: CARDINAL);
   BEGIN
      CheckSetElmt (Set, Elmt, "Exclude");
      Sets.Exclude (Set, Elmt);
   END Exclude;

PROCEDURE Card (VAR Set: tSet): CARDINAL;
   BEGIN
      CheckSet (Set, "Card");
      RETURN Sets.Card (Set);
   END Card;
    
PROCEDURE Size (VAR Set: tSet): CARDINAL;
   BEGIN
      CheckSet (Set, "Size");
      RETURN Sets.Size (Set);
   END Size;

PROCEDURE Minimum (VAR Set: tSet): CARDINAL;
   BEGIN
      CheckNotEmpty (Set, "Minimum");
      RETURN Sets.Minimum (Set);
   END Minimum;

PROCEDURE Maximum (VAR Set: tSet): CARDINAL;
   BEGIN
      CheckNotEmpty (Set, "Maximum");
      RETURN Sets.Maximum (Set);
   END Maximum;

PROCEDURE Select (VAR Set: tSet): CARDINAL;
   BEGIN
      CheckNotEmpty (Set, "Select");
      RETURN Sets.Select (Set);
   END Select;
    
PROCEDURE Extract (VAR Set: tSet): CARDINAL;
   BEGIN
      CheckNotEmpty (Set, "Extract");
      RETURN Sets.Extract (Set);
   END Extract;

PROCEDURE IsSubset (Set1, Set2: tSet): BOOLEAN;
   BEGIN
      CheckSetSet (Set1, Set2, "IsSubset");
      RETURN Sets.IsSubset (Set1, Set2);
   END IsSubset;

PROCEDURE IsStrictSubset (Set1, Set2: tSet): BOOLEAN;
   BEGIN
      CheckSetSet (Set1, Set2, "IsStrictSubset");
      RETURN Sets.IsStrictSubset (Set1, Set2);
   END IsStrictSubset;
    
PROCEDURE IsEqual (VAR Set1, Set2: tSet): BOOLEAN;
   BEGIN
      CheckSetSet (Set1, Set2, "IsEqual");
      RETURN Sets.IsEqual (Set1, Set2);
   END IsEqual;
    
PROCEDURE IsNotEqual (Set1, Set2: tSet): BOOLEAN;
   BEGIN
      CheckSetSet (Set1, Set2, "IsNotEqual");
      RETURN Sets.IsNotEqual (Set1, Set2);
   END IsNotEqual;

PROCEDURE IsElement (Elmt: CARDINAL; VAR Set: tSet): BOOLEAN;
   BEGIN
      CheckSetElmt (Set, Elmt, "IsElement");
      RETURN Sets.IsElement (Elmt, Set);
   END IsElement;

PROCEDURE IsEmpty (Set: tSet): BOOLEAN;
   BEGIN
      CheckSet (Set, "IsEmpty");
      RETURN Sets.IsEmpty (Set);
   END IsEmpty;
    
PROCEDURE Forall (Set: tSet; Proc: ProcOfCardToBool): BOOLEAN;
   BEGIN
      CheckSet (Set, "Forall");
      RETURN Sets.Forall (Set, Proc);
   END Forall;
    
PROCEDURE Exists (Set: tSet; Proc: ProcOfCardToBool): BOOLEAN;
   BEGIN
      CheckSet (Set, "Exists");
      RETURN Sets.Exists (Set, Proc);
   END Exists;
    
PROCEDURE Exists1 (Set: tSet; Proc: ProcOfCardToBool): BOOLEAN;
   BEGIN
      CheckSet (Set, "Exists1");
      RETURN Sets.Exists1 (Set, Proc);
   END Exists1;

PROCEDURE Assign (VAR Set1: tSet; Set2: tSet);
   BEGIN
      CheckSetSet (Set1, Set2, "Assign");
      Sets.Assign (Set1, Set2);
   END Assign;

PROCEDURE AssignElmt (VAR Set: tSet; Elmt: CARDINAL);
   BEGIN
      CheckSetElmt (Set, Elmt, "AssignElmt");
      Sets.AssignElmt (Set, Elmt);
   END AssignElmt;

PROCEDURE AssignEmpty (VAR Set: tSet);
   BEGIN
      CheckSet (Set, "AssignEmpty");
      Sets.AssignEmpty (Set);
   END AssignEmpty;

PROCEDURE ForallDo (Set: tSet; Proc: ProcOfCard);
   BEGIN
      CheckSet (Set, "ForallDo");
      Sets.ForallDo (Set, Proc);
   END ForallDo;

PROCEDURE ReadSet (f: tFile; VAR Set: tSet);
   BEGIN
      CheckSet (Set, "ReadSet");
      Sets.ReadSet (f, Set);
   END ReadSet;

PROCEDURE WriteSet (f: tFile; Set: tSet);
   BEGIN
      CheckSet (Set, "WriteSet");
      Sets.WriteSet (f, Set);
   END WriteSet;

END SetsC.
