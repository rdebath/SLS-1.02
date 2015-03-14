DEFINITION MODULE Parser;

IMPORT Scanner;

TYPE tParsAttribute = Scanner.tScanAttribute;

VAR
  ParsAttribute	: tParsAttribute;
  ParsTabName	: ARRAY [0..128] OF CHAR;

PROCEDURE Parser (): INTEGER;
PROCEDURE CloseParser ();
PROCEDURE xxTokenName (Token: CARDINAL; VAR Name: ARRAY OF CHAR);

END Parser.
