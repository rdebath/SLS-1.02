(* $Id: Errors.md,v 1.0 1992/08/07 14:41:58 grosch rel $ *)

(* $Log: Errors.md,v $
# Revision 1.0  1992/08/07  14:41:58  grosch
# Initial revision
#
 *)

(* Ich, Doktor Josef Grosch, Informatiker, Juli 1992 *)

DEFINITION MODULE Errors;

FROM SYSTEM	IMPORT ADDRESS;
FROM Positions	IMPORT tPosition;
FROM IO		IMPORT tFile;

CONST
   NoText		= 0	;
   SyntaxError		= 1	;	(* error codes		*)
   ExpectedTokens	= 2	;
   RestartPoint		= 3	;
   TokenInserted	= 4	;
   WrongParseTable	= 5	;
   OpenParseTable	= 6	;
   ReadParseTable	= 7	;
   TooManyErrors	= 8	;

   Fatal		= 1	;	(* error classes	*)
   Restriction		= 2	;
   Error		= 3	;
   Warning		= 4	;
   Repair		= 5	;
   Note			= 6	;
   Information		= 7	;

   None			= 0	;
   Integer		= 1	;	(* info classes		*)
   Short		= 2	;
   Long			= 3	;
   Real			= 4	;
   Boolean		= 5	;
   Character		= 6	;
   String		= 7	;
   Array		= 8	;
   Set			= 9	;
   Ident		= 10	;

VAR	  Exit		: PROC;
			(* Refers to a procedure that specifies		*)
			(* what to do if 'ErrorClass' = Fatal.		*)
			(* Default: terminate program execution.	*)

PROCEDURE StoreMessages (Store: BOOLEAN);
			(* Messages are stored if 'Store' = TRUE	*)
			(* for printing with the routine 'WriteMessages'*)
			(* otherwise they are printed immediately.	*)
			(* If 'Store'=TRUE the message store is cleared.*)

PROCEDURE ErrorMessage	(ErrorCode, ErrorClass: CARDINAL; Position: tPosition);
			(* Report a message represented by an integer	*)
			(* 'ErrorCode' and classified by 'ErrorClass'.	*)

PROCEDURE ErrorMessageI	(ErrorCode, ErrorClass: CARDINAL; Position: tPosition;
			 InfoClass: CARDINAL; Info: ADDRESS);
			(* Like the previous routine with additional	*)
			(* information of type 'InfoClass' at the	*)
			(* address 'Info'.				*)

PROCEDURE Message  (ErrorText: ARRAY OF CHAR; ErrorClass: CARDINAL; Position: tPosition);
			(* Report a message represented by a string	*)
			(* 'ErrorText' and classified by 'ErrorClass'.	*)

PROCEDURE MessageI (ErrorText: ARRAY OF CHAR; ErrorClass: CARDINAL; Position: tPosition;
			 InfoClass: CARDINAL; Info: ADDRESS);
			(* Like the previous routine with additional	*)
			(* information of type 'InfoClass' at the	*)
			(* address 'Info'.				*)

PROCEDURE WriteMessages	(File: tFile);
			(* The stored messages are sorted by their	*)
			(* source position and printed on 'File'.	*)

END Errors.
