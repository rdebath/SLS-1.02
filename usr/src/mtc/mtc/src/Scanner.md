(* $Id: Scanner.md,v 2.3 1992/08/18 09:05:32 grosch rel $ *)

DEFINITION MODULE Scanner;

IMPORT Strings;

(* line 15 "modula.rex" *)

FROM StringMem	IMPORT tStringRef;
FROM Idents	IMPORT tIdent;
FROM Tokens	IMPORT TokIdent, TokDecConst, TokOctalConst, TokHexConst,
		       TokCharConst, TokRealConst, TokStringConst;
FROM Positions	IMPORT tPosition;

TYPE
  tScanAttribute	= RECORD	(* type for token attributes	*)
			  Position	: tPosition	;
	CASE : SHORTCARD OF
	| TokIdent	: Ident		: tIdent	;
	| TokDecConst	,
	  TokOctalConst ,
	  TokHexConst	: IntValue	: CARDINAL	;
	| TokCharConst	: CharValue	: CHAR		;
	| TokRealConst	: RealValue	: tStringRef	;
	| TokStringConst: StringValue	: tStringRef	;
	END;
  END;

PROCEDURE ErrorAttribute (Token: CARDINAL; VAR Attribute: tScanAttribute);
			 (* Returns in parameter 'Attribute' default	*)
			 (* values for the attributes of token 'Token'	*)


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
