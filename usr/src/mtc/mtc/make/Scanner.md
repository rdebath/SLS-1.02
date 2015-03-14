(* $Id: Scanner.md,v 2.3 1992/08/18 09:05:32 grosch rel $ *)

DEFINITION MODULE Scanner;

IMPORT Strings;

(* line 1 "modula.rex" *)

FROM Idents	IMPORT tIdent;
FROM Positions	IMPORT tPosition;
   
CONST TokIdent		=  1	;

TYPE
  tScanAttribute	= RECORD
			  Position	: tPosition	;
	CASE : SHORTCARD OF
	| TokIdent	: Ident		: tIdent	;
	ELSE ;
	END;
  END;

PROCEDURE ErrorAttribute (Token: CARDINAL; VAR Attribute: tScanAttribute);


CONST EofToken	= 0;

VAR TokenLength	: INTEGER;
VAR Attribute	: tScanAttribute;
VAR ScanTabName	: ARRAY [0 .. 127] OF CHAR;
VAR Exit	: PROC;

PROCEDURE BeginScanner	;
PROCEDURE BeginFile	(FileName: ARRAY OF CHAR);
PROCEDURE GetToken	(): INTEGER;
PROCEDURE GetWord	(VAR Word: Strings.tString);
PROCEDURE GetLower	(VAR Word: Strings.tString);
PROCEDURE GetUpper	(VAR Word: Strings.tString);
PROCEDURE CloseFile	;
PROCEDURE CloseScanner	;

END Scanner.
