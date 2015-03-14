(* $Id: SetsC.md,v 1.2 1991/11/21 14:33:17 grosch rel $ *)

(* $Log: SetsC.md,v $
Revision 1.2  1991/11/21  14:33:17  grosch
new version of RCS on SPARC

Revision 1.1  89/02/23  16:02:11  grosch
Initial revision

 *)

(* Ich, Doktor Josef Grosch, Informatiker, Feb. 1989 *)

DEFINITION MODULE SetsC;

FROM IO IMPORT tFile;

IMPORT Sets;

TYPE
   ProcOfCard		= Sets.ProcOfCard;
   ProcOfCardToBool	= Sets.ProcOfCardToBool;
   tSet			= Sets.tSet;

PROCEDURE MakeSet	(VAR Set: tSet; MaxSize: CARDINAL);
PROCEDURE ReleaseSet	(VAR Set: tSet);
PROCEDURE Union		(VAR Set1: tSet; Set2: tSet);
PROCEDURE Difference	(VAR Set1: tSet; Set2: tSet);
PROCEDURE Intersection	(VAR Set1: tSet; Set2: tSet);
PROCEDURE SymDiff	(VAR Set1: tSet; Set2: tSet);
PROCEDURE Complement	(VAR Set: tSet);
PROCEDURE Include	(VAR Set: tSet; Elmt: CARDINAL);
PROCEDURE Exclude	(VAR Set: tSet; Elmt: CARDINAL);
PROCEDURE Card		(VAR Set: tSet): CARDINAL;
PROCEDURE Size		(VAR Set: tSet): CARDINAL;
PROCEDURE Minimum	(VAR Set: tSet): CARDINAL;
PROCEDURE Maximum	(VAR Set: tSet): CARDINAL;
PROCEDURE Select	(VAR Set: tSet): CARDINAL;
PROCEDURE Extract	(VAR Set: tSet): CARDINAL;
PROCEDURE IsSubset	(Set1, Set2: tSet): BOOLEAN;
PROCEDURE IsStrictSubset (Set1, Set2: tSet): BOOLEAN;
PROCEDURE IsEqual	(VAR Set1, Set2: tSet): BOOLEAN;
PROCEDURE IsNotEqual	(Set1, Set2: tSet): BOOLEAN;
PROCEDURE IsElement	(Elmt: CARDINAL; VAR Set: tSet): BOOLEAN;
PROCEDURE IsEmpty	(Set: tSet): BOOLEAN;
PROCEDURE Forall	(Set: tSet; Proc: ProcOfCardToBool): BOOLEAN;
PROCEDURE Exists	(Set: tSet; Proc: ProcOfCardToBool): BOOLEAN;
PROCEDURE Exists1	(Set: tSet; Proc: ProcOfCardToBool): BOOLEAN;
PROCEDURE Assign	(VAR Set1: tSet; Set2: tSet);
PROCEDURE AssignElmt	(VAR Set: tSet; Elmt: CARDINAL);
PROCEDURE AssignEmpty	(VAR Set: tSet);
PROCEDURE ForallDo	(Set: tSet; Proc: ProcOfCard);
PROCEDURE ReadSet	(f: tFile; VAR Set: tSet);
PROCEDURE WriteSet	(f: tFile;     Set: tSet);

END SetsC.
