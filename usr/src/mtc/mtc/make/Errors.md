DEFINITION MODULE Errors;

FROM SYSTEM	IMPORT ADDRESS;
FROM Positions	IMPORT tPosition;

CONST
   NoText		= 0	;
   SyntaxError		= 1	;	(* error codes		*)
   ExpectedTokens	= 2	;
   RestartPoint		= 3	;
   TokenInserted	= 4	;
   ReadParseTable	= 5	;
   IllegalChar		= 6	;
   UnclosedComment	= 7	;
   UnclosedString	= 8	;

   Fatal		= 1	;	(* error classes	*)
   Restriction		= 2	;
   Error		= 3	;
   Warning		= 4	;
   Repair		= 5	;
   Note			= 6	;
   Information		= 7	;

   Integer		= 1	;	(* info classes		*)
   Short		= 2	;
   Long			= 3	;
   Real			= 4	;
   Boolean		= 5	;
   Character		= 6	;
   String		= 7	;
   Array		= 8	;
   Set			= 9	;

PROCEDURE ErrorMessage	(ErrorCode, ErrorClass: CARDINAL; Position: tPosition);
PROCEDURE ErrorMessageI	(ErrorCode, ErrorClass: CARDINAL; Position: tPosition;
			 InfoClass: CARDINAL; Info: ADDRESS);
PROCEDURE NumberOfErrors(): CARDINAL;

END Errors.
