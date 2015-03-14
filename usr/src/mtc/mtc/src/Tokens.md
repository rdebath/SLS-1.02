(*
 *	M T C  -  Modula-2 to C Translator
 *      ----------------------------------
 *
 *	Purpose: defines codes of tokens, prints tokens and sets of tokens
 *
 *	$Author: grosch $
 *	$Date: 1991/11/21 16:57:59 $
 *	$Revision: 1.3 $
 *
 ***)

DEFINITION MODULE Tokens;

(* FROM Parser	IMPORT xxSetType; *)
FROM IO		IMPORT tFile;

CONST
  TokEOF		=  0	;	(* end of file			*)
  TokIdent		=  1	;	(* letter (letter | digit) *	*)
  TokDecConst		=  2	;	(* digit +			*)
  TokOctalConst		=  3	;	(* octalDigit + B		*)
  TokHexConst		=  4	;	(* digit hexDigit * H		*)
  TokCharConst		=  5	;	(* octalDigit + C		*)
  TokRealConst		=  6	;	(* digit + "." digit *		*)
					(*	(E {+\-} ? digit +) ?	*)
  TokStringConst	=  7	;	(* " (character - {"\n}) * "	*)
					(* ' (character - {'\n}) * '	*)
  TokNotEqual		=  8	;	(* '#', '<>'		*)
  TokLParent		=  9	;	(* '('			*)
  TokRParent		= 10	;	(* ')'			*)
  TokTimes		= 11	;	(* '*'			*)
  TokPlus		= 12	;	(* '+'			*)
  TokComma		= 13	;	(* ','			*)
  TokMinus		= 14	;	(* '-'			*)
  TokDot		= 15	;	(* '.'			*)
  TokRange		= 16	;	(* '..'			*)
  TokDivide		= 17	;	(* '/'			*)
  TokColon		= 18	;	(* ':'			*)
  TokAssign		= 19	;	(* ':='			*)
  TokSemiColon		= 20	;	(* ';'			*)
  TokLess		= 21	;	(* '<'			*)
  TokLessEqual		= 22	;	(* '<='			*)
  TokEqual		= 24	;	(* '='			*)
  TokGreater		= 25	;	(* '>'			*)
  TokGreaterEqual	= 26	;	(* '>='			*)
  TokLBracket		= 27	;	(* '['			*)
  TokRBracket		= 28	;	(* ']'			*)
  TokArrow		= 29	;	(* '^'			*)
  TokLBrace		= 30	;	(* '{'			*)
  TokBar		= 31	;	(* '|'			*)
  TokRBrace		= 32	;	(* '}'			*)

  TokAnd		= 34	;	(* AND, '&'		*)
  TokArray		= 35	;	(* ARRAY		*)
  TokBegin		= 36	;	(* BEGIN		*)
  TokBy			= 37	;	(* BY			*)
  TokCase		= 38	;	(* CASE			*)
  TokConst		= 39	;	(* CONST		*)
  TokDefinition		= 40	;	(* DEFINITION		*)
  TokDiv		= 41	;	(* DIV			*)
  TokDo			= 42	;	(* DO			*)
  TokElse		= 43	;	(* ELSE			*)
  TokElsif		= 44	;	(* ELSIF		*)
  TokEnd		= 45	;	(* END			*)
  TokExit		= 46	;	(* EXIT			*)
  TokExport		= 47	;	(* EXPORT		*)
  TokFor		= 48	;	(* FOR			*)
  TokFrom		= 49	;	(* FROM			*)
  TokIf			= 50	;	(* IF			*)
  TokImplementation	= 51	;	(* IMPLEMENTATION	*)
  TokImport		= 52	;	(* IMPORT		*)
  TokIn			= 53	;	(* IN			*)
  TokLoop		= 54	;	(* LOOP			*)
  TokMod		= 55	;	(* MOD			*)
  TokModule		= 56	;	(* MODULE		*)
  TokNot		= 57	;	(* NOT, '~'		*)
  TokOf			= 58	;	(* OF			*)
  TokOr			= 59	;	(* OR			*)
  TokPointer		= 60	;	(* POINTER		*)
  TokProcedure		= 61	;	(* PROCEDURE		*)
  TokQualified		= 62	;	(* QUALIFIED		*)
  TokRecord		= 63	;	(* RECORD		*)
  TokRepeat		= 64	;	(* REPEAT		*)
  TokReturn		= 65	;	(* RETURN		*)
  TokSet		= 66	;	(* SET			*)
  TokThen		= 67	;	(* THEN			*)
  TokTo			= 68	;	(* TO			*)
  TokType		= 69	;	(* TYPE			*)
  TokUntil		= 70	;	(* UNTIL		*)
  TokVar		= 71	;	(* VAR			*)
  TokWhile		= 72	;	(* WHILE		*)
  TokWith		= 73	;	(* WITH			*)
  TokForeign		= 74	;	(* FOREIGN		*)

TYPE
  tToken		= SHORTCARD;
  tTokenSet		= ARRAY [0..2] OF BITSET; (* xxSetType *)

PROCEDURE WriteToken	(f: tFile; Token: tToken);
			(* print the external representation of a token	*)
			(* on file 'f'					*)

PROCEDURE WriteTokenSet	(f: tFile; TokenSet: tTokenSet);
			(* print the external representation of a set	*)
			(* of tokens on file 'f'			*)

END Tokens.
