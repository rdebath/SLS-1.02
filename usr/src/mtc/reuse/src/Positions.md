(* $Id: Positions.md,v 1.0 1992/08/07 14:41:59 grosch rel $ *)

(* $Log: Positions.md,v $
# Revision 1.0  1992/08/07  14:41:59  grosch
# Initial revision
#
 *)

(* Ich, Doktor Josef Grosch, Informatiker, Juli 1992 *)

DEFINITION MODULE Positions;

FROM IO		IMPORT tFile;

TYPE	  tPosition	= RECORD Line, Column: SHORTCARD; END;

VAR	  NoPosition	: tPosition;
			(* A default position (0, 0).			*)

PROCEDURE Compare	(Position1, Position2: tPosition): INTEGER;
			(* Returns -1 if Position1 < Position2.		*)
			(* Returns  0 if Position1 = Position2.		*)
			(* Returns  1 if Position1 > Position2.		*)

PROCEDURE WritePosition	(File: tFile; Position: tPosition);
			(* The 'Position' is printed on the 'File'.	*)

END Positions.
