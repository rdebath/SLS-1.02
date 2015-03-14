(* $Id: Sets.md,v 1.4 1991/11/21 14:33:17 grosch rel $ *)

(* $Log: Sets.md,v $
 * Revision 1.4  1991/11/21  14:33:17  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.3  91/09/18  15:09:49  grosch
 * reduced size of set type
 * 
 * Revision 1.2  91/06/07  11:37:49  grosch
 * increased bounds of flexible arrays
 * 
 * Revision 1.1  89/01/09  17:13:03  grosch
 * added functions Size, Minimum, and Maximum
 * 
 * Revision 1.0  88/10/04  11:47:12  grosch
 * Initial revision
 * 
 *)

(* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 *)

DEFINITION MODULE Sets;

FROM IO IMPORT tFile;

TYPE
   ArrayOfBitset	= ARRAY [0 .. 10000000] OF BITSET;
   ProcOfCard		= PROCEDURE (CARDINAL);
   ProcOfCardToBool	= PROCEDURE (CARDINAL): BOOLEAN;

   tSet = RECORD
      BitsetPtr		: POINTER TO ArrayOfBitset;
      MaxElmt		,
      LastBitset	: SHORTCARD;
      Card		: SHORTINT;
      FirstElmt		,
      LastElmt		: SHORTCARD;
   END;

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

END Sets.
