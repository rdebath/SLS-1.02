(* $Id: Errors.mi,v 1.0 1992/08/07 14:41:59 grosch rel $ *)

(* $Log: Errors.mi,v $
# Revision 1.0  1992/08/07  14:41:59  grosch
# Initial revision
#
 *)

(* Ich, Doktor Josef Grosch, Informatiker, Juli 1992 *)

IMPLEMENTATION MODULE Errors;

FROM SYSTEM	IMPORT ADDRESS, TSIZE, ADR;
FROM Memory	IMPORT Alloc;
FROM IO		IMPORT tFile, StdError, WriteC, WriteNl, WriteS, WriteI,
		       WriteB, WriteR, CloseIO;
FROM Positions	IMPORT tPosition, Compare, WritePosition;
FROM StringMem	IMPORT tStringRef, PutString, GetString;
FROM Strings	IMPORT tString, ArrayToString, StringToArray;
FROM Idents	IMPORT tIdent, WriteIdent, MakeIdent;
FROM Sets	IMPORT tSet, WriteSet, Assign, MakeSet, Size;
FROM Sort	IMPORT Sort;

IMPORT System, Strings;

CONST MaxError	= 100;

TYPE tArray	= ARRAY [0..255] OF CHAR;

TYPE tError	= RECORD
		     		  Position	: tPosition	;
		     		  IsErrorCode	: BOOLEAN	;
		     		  ErrorNumber	: SHORTCARD	;
		     		  ErrorCode	: SHORTCARD	;
		     		  ErrorClass	: SHORTCARD	;
		     CASE	  InfoClass	: SHORTCARD	OF
		     | None	:
		     | Integer	: vInteger	: INTEGER	;
		     | Short	: vShort	: INTEGER	;
		     | Long	: vLong		: INTEGER	;
		     | Real	: vReal		: REAL		;
		     | Boolean	: vBoolean	: BOOLEAN	;
		     | Character: vCharacter	: CHAR		;
		     | String	: vString	: tStringRef	;
		     | Array	: vArray	: tStringRef	;
		     | Set	: vSet		: POINTER TO tSet;
		     | Ident	: vIdent	: tIdent	;
		     END;
		  END;

VAR
   ErrorTable	: ARRAY [0..MaxError] OF tError;
   MessageCount	: INTEGER;
   IsStore	: BOOLEAN;
   HandleMessage: PROCEDURE (BOOLEAN, CARDINAL, CARDINAL, tPosition, CARDINAL, ADDRESS);
   Out		: tFile;

PROCEDURE ErrorMessage	(ErrorCode, ErrorClass: CARDINAL; Position: tPosition);
   BEGIN
      HandleMessage (TRUE, ErrorCode, ErrorClass, Position, None, NIL);
   END ErrorMessage;

PROCEDURE ErrorMessageI	(ErrorCode, ErrorClass: CARDINAL; Position: tPosition;
			 InfoClass: CARDINAL; Info: ADDRESS);
   BEGIN
      HandleMessage (TRUE, ErrorCode, ErrorClass, Position, InfoClass, Info);
   END ErrorMessageI;

PROCEDURE Message  (ErrorText: ARRAY OF CHAR; ErrorClass: CARDINAL; Position: tPosition);
   VAR String	: tString;
   BEGIN
      ArrayToString (ErrorText, String);
      HandleMessage (FALSE, MakeIdent (String), ErrorClass, Position, None, NIL);
   END Message;

PROCEDURE MessageI (ErrorText: ARRAY OF CHAR; ErrorClass: CARDINAL; Position: tPosition;
			 InfoClass: CARDINAL; Info: ADDRESS);
   VAR String	: tString;
   BEGIN
      ArrayToString (ErrorText, String);
      HandleMessage (FALSE, MakeIdent (String), ErrorClass, Position, InfoClass, Info);
   END MessageI;

PROCEDURE WriteHead (Position: tPosition; ErrorClass: CARDINAL);
   BEGIN
      WritePosition (Out, Position);
      WriteS	(Out, ": ");
      CASE ErrorClass OF
      |  Fatal		: WriteS (Out, "Fatal       ");
      |  Restriction	: WriteS (Out, "Restriction ");
      |  Error		: WriteS (Out, "Error       ");
      |  Warning	: WriteS (Out, "Warning     ");
      |  Repair		: WriteS (Out, "Repair      ");
      |  Note		: WriteS (Out, "Note        ");
      |  Information	: WriteS (Out, "Information ");
      ELSE WriteS (Out, "Error class: "); WriteI (Out, ErrorClass, 0);
      END;
   END WriteHead;

PROCEDURE WriteCode (ErrorCode: CARDINAL);
   BEGIN
      CASE ErrorCode OF
      |  NoText		:
      |  SyntaxError	: WriteS (Out, "syntax error"		);
      |  ExpectedTokens	: WriteS (Out, "expected tokens"	);
      |  RestartPoint	: WriteS (Out, "restart point"		);
      |  TokenInserted	: WriteS (Out, "token inserted "	);
      |  WrongParseTable: WriteS (Out, "parse table mismatch"	);
      |  OpenParseTable	: WriteS (Out, "cannot open parse table");
      |  ReadParseTable	: WriteS (Out, "cannot read parse table");
      |  TooManyErrors	: WriteS (Out, "too many errors"	);
      ELSE WriteS (Out, " error code: "); WriteI (Out, ErrorCode, 0);
      END;
   END WriteCode;

PROCEDURE WriteInfo (InfoClass: CARDINAL; Info: ADDRESS);
   VAR
      PtrToInteger	: POINTER TO INTEGER;
      PtrToShort	: POINTER TO SHORTCARD;
      PtrToLong		: POINTER TO LONGINT;
      PtrToReal		: POINTER TO REAL;
      PtrToBoolean	: POINTER TO BOOLEAN;
      PtrToCharacter	: POINTER TO CHAR;
      PtrToString	: POINTER TO tString;
      PtrToArray	: POINTER TO tArray;
      PtrToIdent	: POINTER TO tIdent;
   BEGIN
      IF InfoClass = None THEN RETURN; END;
      WriteS (Out, ": ");
      CASE InfoClass OF
      | Integer	: PtrToInteger	:= Info; WriteI (Out, PtrToInteger^, 0);
      | Short  	: PtrToShort	:= Info; WriteI (Out, PtrToShort^, 0);
      | Long   	: PtrToLong	:= Info; WriteI (Out, PtrToLong^, 0);
      | Real   	: PtrToReal	:= Info; WriteR (Out, PtrToReal^, 1, 10, 1);
      | Boolean	: PtrToBoolean	:= Info; WriteB (Out, PtrToBoolean^);
      | Character:PtrToCharacter:= Info; WriteC (Out, PtrToCharacter^);
      | String	: PtrToString	:= Info; Strings.WriteS (Out, PtrToString^);
      | Array	: PtrToArray	:= Info; WriteS (Out, PtrToArray^);
      | Ident	: PtrToIdent	:= Info; WriteIdent (Out, PtrToIdent^);
      ELSE
      END;
   END WriteInfo;

PROCEDURE WriteMessage	(IsErrorCode: BOOLEAN; ErrorCode, ErrorClass: CARDINAL;
			 Position: tPosition; InfoClass: CARDINAL; Info: ADDRESS);
   BEGIN
      WriteHead (Position, ErrorClass);
      IF IsErrorCode THEN
	 WriteCode (ErrorCode);
      ELSE
	 WriteIdent (Out, ErrorCode);
      END;
      WriteInfo (InfoClass, Info);
      WriteNl (Out);
      IF (ErrorClass = Fatal) AND NOT IsStore THEN Exit; END;
   END WriteMessage;

PROCEDURE WriteMessages	(File: tFile);
   VAR i	: INTEGER;
   VAR Info	: ADDRESS;
   VAR s	: tString;
   BEGIN
      Sort (1, MessageCount, IsLess, Swap);
      Out := File;
      FOR i := 1 TO MessageCount DO
	 WITH ErrorTable [i] DO
	    CASE InfoClass OF
	    | Integer	: Info := ADR (vInteger	);
	    | Short	: Info := ADR (vShort	);
	    | Long	: Info := ADR (vLong	);
	    | Real	: Info := ADR (vReal	);
	    | Boolean	: Info := ADR (vBoolean	);
	    | Character	: Info := ADR (vCharacter);
	    | String	: GetString (vString, s); Info := ADR (s);
	    | Set	: Info :=      vSet	 ;
	    | Ident	: Info := ADR (vIdent	);
	    ELSE
	    END;
	    WriteMessage (IsErrorCode, ErrorCode, ErrorClass, Position, InfoClass, Info);
	 END;
      END;
      Out := StdError;
   END WriteMessages;

PROCEDURE StoreMessage	(pIsErrorCode: BOOLEAN; pErrorCode, pErrorClass: CARDINAL;
			 pPosition: tPosition; pInfoClass: CARDINAL; pInfo: ADDRESS);
   VAR
      PtrToInteger	: POINTER TO INTEGER	;
      PtrToShort	: POINTER TO SHORTCARD	;
      PtrToLong		: POINTER TO LONGINT	;
      PtrToReal		: POINTER TO REAL	;
      PtrToBoolean	: POINTER TO BOOLEAN	;
      PtrToCharacter	: POINTER TO CHAR	;
      PtrToString	: POINTER TO tString	;
      PtrToArray	: POINTER TO tArray	;
      PtrToSet		: POINTER TO tSet	;
      PtrToIdent	: POINTER TO tIdent	;
      s			: tString		;
   BEGIN
      IF MessageCount < MaxError THEN
	 INC (MessageCount);
	 WITH ErrorTable [MessageCount] DO
	    Position	:= pPosition	;
	    IsErrorCode	:= pIsErrorCode	;
	    ErrorNumber	:= MessageCount	;
	    ErrorCode	:= pErrorCode	;
	    ErrorClass	:= pErrorClass	;
	    InfoClass	:= pInfoClass	;
	    CASE InfoClass OF
	    | Integer	: PtrToInteger	:= pInfo; vInteger	:= PtrToInteger	^;
	    | Short	: PtrToShort	:= pInfo; vShort	:= PtrToShort	^;
	    | Long	: PtrToLong	:= pInfo; vLong		:= PtrToLong	^;
	    | Real	: PtrToReal	:= pInfo; vReal		:= PtrToReal	^;
	    | Boolean	: PtrToBoolean	:= pInfo; vBoolean	:= PtrToBoolean	^;
	    | Character	: PtrToCharacter:= pInfo; vCharacter	:= PtrToCharacter^;
	    | String	: PtrToString	:= pInfo; vString	:= PutString (PtrToString^);
	    | Array	: PtrToArray	:= pInfo; ArrayToString (PtrToArray^, s);
			  InfoClass	:= String;vArray	:= PutString (s);
	    | Set	: PtrToSet	:= pInfo; vSet		:= Alloc (TSIZE (tSet));
						  MakeSet (vSet^, Size (PtrToSet^));
						  Assign  (vSet^, PtrToSet^);
	    | Ident	: PtrToIdent	:= pInfo; vIdent	:= PtrToIdent	^;
	    ELSE
	    END;
	 END;
      ELSE
	 WITH ErrorTable [MessageCount] DO
	    IsErrorCode	:= TRUE		;
	    ErrorCode	:= TooManyErrors;
	    ErrorClass	:= Restriction	;
	    InfoClass	:= None		;
	 END;
      END;
      IF pErrorClass = Fatal THEN WriteMessages (StdError); Exit; END;
   END StoreMessage;

PROCEDURE IsLess (i, j: INTEGER): BOOLEAN;
   VAR r: INTEGER;
   BEGIN
      r := Compare (ErrorTable [i].Position, ErrorTable [j].Position);
      IF r = -1 THEN RETURN TRUE ; END;
      IF r = +1 THEN RETURN FALSE; END;
      RETURN ErrorTable [i].ErrorNumber < ErrorTable [j].ErrorNumber;
   END IsLess;

PROCEDURE Swap (i, j: INTEGER);
   VAR t: tError;
   BEGIN
      t := ErrorTable [i]; ErrorTable [i] := ErrorTable [j]; ErrorTable [j] := t;
   END Swap;

PROCEDURE StoreMessages (Store: BOOLEAN);
   BEGIN
      IF Store THEN
	 HandleMessage := StoreMessage;
	 MessageCount  := 0;
      ELSE
	 HandleMessage := WriteMessage;
      END;
      IsStore := Store;
   END StoreMessages;

PROCEDURE yyExit;
   BEGIN
      CloseIO; System.Exit (1);
   END yyExit;

BEGIN
   Exit		:= yyExit;
   IsStore	:= FALSE;
   Out		:= StdError;
   HandleMessage:= WriteMessage;
END Errors.
