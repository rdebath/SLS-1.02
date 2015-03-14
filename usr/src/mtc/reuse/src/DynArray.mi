(* $Id: DynArray.mi,v 1.6 1992/08/07 14:45:20 grosch rel $ *)

(* $Log: DynArray.mi,v $
 * Revision 1.6  1992/08/07  14:45:20  grosch
 * added error message if out of memory
 *
 * Revision 1.5  1991/11/21  14:33:17  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.4  90/03/02  17:36:10  grosch
 * automized handling of machine independent alignment
 * 
 * Revision 1.3  90/02/28  22:07:00  grosch
 * comment for alignment on SPARC
 * 
 * Revision 1.2  89/12/08  20:12:44  grosch
 * introduced a machine dependent variant for MIPS
 * 
 * Revision 1.1  88/10/18  16:30:08  grosch
 * fixed bug: invariant must hold: ElmtCount * ElmtSize MOD 4 = 0
 * 
 * Revision 1.0  88/10/04  11:46:52  grosch
 * Initial revision
 * 
 *)

(* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 *)

IMPLEMENTATION MODULE DynArray;

FROM SYSTEM	IMPORT ADDRESS, TSIZE, WORD;
FROM General	IMPORT Log2, Exp2, MaxAlign;
FROM Memory	IMPORT Alloc, Free;
FROM IO		IMPORT StdError, WriteS, WriteNl;

(* INVARIANT ElmtCount * AlignedSize (ElmtSize) MOD TSIZE (LONGINT) = 0 *)

PROCEDURE MakeArray    (VAR ArrayPtr	: ADDRESS	;
			VAR ElmtCount	: LONGINT	;
			    ElmtSize	: LONGINT)	;
   BEGIN
      ElmtSize := AlignedSize (ElmtSize);
      CASE ElmtSize MOD 4 OF
      | 0   :
      | 2   : IF ODD (ElmtCount) THEN INC (ElmtCount); END;
      | 1, 3: INC (ElmtCount, TSIZE (LONGINT) - 1 - (ElmtCount - 1) MOD TSIZE (LONGINT));
      END;
      ArrayPtr := Alloc (ElmtCount * ElmtSize);
      IF ArrayPtr = NIL THEN
	 WriteS (StdError, "MakeArray: out of memory"); WriteNl (StdError);
      END;
   END MakeArray;

PROCEDURE ExtendArray  (VAR ArrayPtr	: ADDRESS	;
			VAR ElmtCount	: LONGINT	;
			    ElmtSize	: LONGINT)	;
   VAR
      NewPtr		: ADDRESS;
      Source, Target	: POINTER TO LONGINT;
      i			: LONGINT;
   BEGIN
      ElmtSize := AlignedSize (ElmtSize);
      NewPtr := Alloc (ElmtCount * ElmtSize * 2);
      IF NewPtr = NIL THEN
	 WriteS (StdError, "ExtendArray: out of memory"); WriteNl (StdError);
      ELSE
	 Source := ArrayPtr;
	 Target := NewPtr;
	 FOR i := 1 TO ElmtCount * ElmtSize DIV TSIZE (LONGINT) DO
	    Target ^ := Source ^;
	    Source := ADDRESS (ADDRESS (Source) + TSIZE (LONGINT));
	    Target := ADDRESS (ADDRESS (Target) + TSIZE (LONGINT));
	 END;
	 Free (ElmtCount * ElmtSize, ArrayPtr);
	 INC (ElmtCount, ElmtCount);
      END;
      ArrayPtr := NewPtr;
   END ExtendArray;

PROCEDURE ReleaseArray (VAR ArrayPtr	: ADDRESS	;
			VAR ElmtCount	: LONGINT	;
			    ElmtSize	: LONGINT)	;
   BEGIN
      ElmtSize := AlignedSize (ElmtSize);
      Free (ElmtCount * ElmtSize, ArrayPtr);
   END ReleaseArray;

PROCEDURE AlignedSize  (ElmtSize: LONGINT): LONGINT;
   VAR Align	: LONGINT;
   BEGIN
      IF ElmtSize >= MaxAlign THEN
	 Align := MaxAlign;
      ELSE
	 Align := Exp2 (Log2 (ElmtSize + ElmtSize - 2));
      END;
      RETURN ElmtSize + Align - 1 - (ElmtSize - 1) MOD Align;
   END AlignedSize;

END DynArray.
