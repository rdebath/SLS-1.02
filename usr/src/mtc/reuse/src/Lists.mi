(* $Id: Lists.mi,v 1.2 1991/11/21 14:33:17 grosch rel $ *)

(* $Log: Lists.mi,v $
 * Revision 1.2  1991/11/21  14:33:17  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.1  88/10/20  17:20:44  grosch
 * fixed bug: initialization in Length added
 * 
 * Revision 1.0  88/10/04  11:47:05  grosch
 * Initial revision
 * 
 *)

(* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 *)

IMPLEMENTATION MODULE Lists;

FROM SYSTEM	IMPORT ADDRESS, TSIZE;
FROM Memory	IMPORT Alloc;
FROM IO		IMPORT tFile;

PROCEDURE MakeList	(VAR List: tList);
   BEGIN
      List.FirstElmt	:= NIL;
      List.LastElmt	:= NIL;
   END MakeList;

PROCEDURE Insert	(VAR List: tList; Elmt: tElmt);
   VAR ActElmt	: tListElmtPtr;
   BEGIN
      ActElmt := Alloc (TSIZE (tListElmt));
      ActElmt^.Succ := NIL;
      ActElmt^.Elmt := Elmt;
      IF List.FirstElmt = NIL THEN
	 List.LastElmt := ActElmt;
      ELSE
	 ActElmt^.Succ := List.FirstElmt;
      END;
      List.FirstElmt := ActElmt;
   END Insert;

PROCEDURE Append	(VAR List: tList; Elmt: tElmt);
   VAR ActElmt	: tListElmtPtr;
   BEGIN
      ActElmt := Alloc (TSIZE (tListElmt));
      ActElmt^.Succ := NIL;
      ActElmt^.Elmt := Elmt;
      IF List.FirstElmt = NIL THEN
         List.FirstElmt	:= ActElmt;
      ELSE
	 List.LastElmt^.Succ := ActElmt;
      END;
      List.LastElmt := ActElmt;
   END Append;

PROCEDURE Head		(    List: tList): tElmt;
   BEGIN
      RETURN List.FirstElmt^.Elmt;
   END Head;

PROCEDURE Tail		(VAR List: tList);
   BEGIN
      List.FirstElmt := List.FirstElmt^.Succ;
   END Tail;

PROCEDURE Last		(    List: tList): tElmt;
   BEGIN
      RETURN List.LastElmt^.Elmt;
   END Last;

PROCEDURE Front		(VAR List: tList);
   BEGIN
   END Front;

PROCEDURE IsEmpty	(    List: tList): BOOLEAN;
   BEGIN
      RETURN List.FirstElmt = NIL;
   END IsEmpty;

PROCEDURE Length	(    List: tList): CARDINAL;
   VAR n	: CARDINAL;
   BEGIN
      n := 0;
      WHILE List.FirstElmt # NIL DO
	 INC (n);
	 List.FirstElmt := List.FirstElmt^.Succ;
      END;
      RETURN n;
   END Length;

PROCEDURE WriteList	(f: tFile; List: tList; Proc: tProcOfFileAddress);
   BEGIN
      WHILE NOT IsEmpty (List) DO
	 Proc (f, Head (List));
	 Tail (List);
      END;
   END WriteList;

END Lists.
