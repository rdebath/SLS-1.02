DEFINITION MODULE Positions;

FROM IO		IMPORT tFile;
FROM Idents	IMPORT tIdent;

TYPE tPosition	= RECORD File: tIdent; Line, Column: SHORTCARD; END;

VAR  NoPosition	: tPosition;

PROCEDURE WritePosition (File: tFile; Position: tPosition);

END Positions.
