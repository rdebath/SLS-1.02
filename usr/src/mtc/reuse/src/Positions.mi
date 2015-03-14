(* $Id: Positions.mi,v 1.1 1992/08/13 13:47:25 grosch rel $ *)

(* $Log: Positions.mi,v $
# Revision 1.1  1992/08/13  13:47:25  grosch
# increase format in WritePosition
#
# Revision 1.0  1992/08/07  14:42:00  grosch
# Initial revision
#
 *)

(* Ich, Doktor Josef Grosch, Informatiker, Juli 1992 *)

IMPLEMENTATION MODULE Positions;

FROM IO		IMPORT tFile, WriteC, WriteI;

PROCEDURE Compare (Position1, Position2: tPosition): INTEGER;
   BEGIN
      WITH Position1 DO
	 IF Line   < Position2.Line   THEN RETURN -1; END;
	 IF Line   > Position2.Line   THEN RETURN  1; END;
	 IF Column < Position2.Column THEN RETURN -1; END;
	 IF Column > Position2.Column THEN RETURN  1; END;
	 RETURN 0;
      END;
   END Compare;

PROCEDURE WritePosition (File: tFile; Position: tPosition);
   BEGIN
      WriteI (File, Position.Line  , 4);
      WriteC (File, ',');
      WriteI (File, Position.Column, 3);
   END WritePosition;

BEGIN
   NoPosition.Line	:= 0;
   NoPosition.Column	:= 0;
END Positions.
