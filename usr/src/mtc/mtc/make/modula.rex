EXPORT	{
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
}

GLOBAL	{
FROM Strings	IMPORT tString;
FROM Idents	IMPORT NoIdent, MakeIdent;
FROM Errors	IMPORT ErrorMessage, Error, IllegalChar, UnclosedComment,
		       UnclosedString;

VAR NestingLevel	: CARDINAL;

CONST
  TokIntConst		=  2	;
  TokCharConst		=  4	;
  TokRealConst		=  6	;
  TokStringConst	=  7	;
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

PROCEDURE ErrorAttribute (Token: CARDINAL; VAR Attribute: tScanAttribute);
BEGIN
  CASE Token OF
  | TokIdent		: Attribute.Ident	:= NoIdent;
  ELSE ;
  END;
END ErrorAttribute;
}

LOCAL	{ VAR Word: tString; }

BEGIN	{ NestingLevel := 0; }

DEFAULT	{ ErrorMessage (IllegalChar, Error, Attribute.Position); }

EOF	{
  IF yyStartState = Comment THEN
    ErrorMessage (UnclosedComment, Error, Attribute.Position);
  END;
}
   
DEFINE

   digit	= {0-9}		.
   letter	= {a-z A-Z}	.
   CmtCh	= - {*(\t\n}	.
   StrCh1	= - {'\t\n}	.
   StrCh2	= - {"\t\n}	.

START	Comment, Str1, Str2

RULES

#STD, Comment# "(*"	:- {INC (NestingLevel); yyStart (Comment);}
#Comment#  "*)"		:- {DEC (NestingLevel);
			    IF NestingLevel = 0 THEN yyStart (STD); END;}
#Comment#  "(" | "*" | CmtCh + :- {}

#STD# \f | \r		:- {}

#STD# digit +		,
#STD# digit + / ".."	,
#STD# digit + / BY	,
#STD# {0-7} + B		,
#STD# digit {0-9 A-F} * H : {RETURN TokIntConst		;}

#STD# {0-7} + C		: {RETURN TokCharConst		;}

#STD# digit + "." digit * (E {+\-} ? digit +) ? : {RETURN TokRealConst;}

#STD#	'		:  {yyStart (Str1);}
#Str1#	StrCh1 +	:- {}
#Str1#	'		:- {yyStart (STD); RETURN TokStringConst;}

#STD#	\"		:  {yyStart (Str2);}
#Str2#	StrCh2 +	:- {}
#Str2#	\"		:- {yyStart (STD); RETURN TokStringConst;}

#Str1, Str2# \t		:- {yyTab;}
#Str1, Str2# \n		:- {ErrorMessage (UnclosedString, Error, Attribute.Position);
			    yyEol (0); yyStart (STD); RETURN TokStringConst;}

#STD# "#"		: {RETURN TokNotEqual		;}
#STD# "&"		: {RETURN TokAnd		;}
#STD# "("		: {RETURN TokLParent		;}
#STD# ")"		: {RETURN TokRParent		;}
#STD# "*"		: {RETURN TokTimes		;}
#STD# "+"		: {RETURN TokPlus		;}
#STD# ","		: {RETURN TokComma		;}
#STD# "-"		: {RETURN TokMinus		;}
#STD# "."		: {RETURN TokDot		;}
#STD# ".."		: {RETURN TokRange		;}
#STD# "/"		: {RETURN TokDivide		;}
#STD# ":"		: {RETURN TokColon		;}
#STD# ":="		: {RETURN TokAssign		;}
#STD# ";"		: {RETURN TokSemiColon		;}
#STD# "<"		: {RETURN TokLess		;}
#STD# "<="		: {RETURN TokLessEqual		;}
#STD# "<>"		: {RETURN TokNotEqual		;}
#STD# "="		: {RETURN TokEqual		;}
#STD# ">"		: {RETURN TokGreater		;}
#STD# ">="		: {RETURN TokGreaterEqual	;}
#STD# "["		: {RETURN TokLBracket		;}
#STD# "]"		: {RETURN TokRBracket		;}
#STD# "^"		: {RETURN TokArrow		;}
#STD# "{"		: {RETURN TokLBrace		;}
#STD# "|"		: {RETURN TokBar		;}
#STD# "}"		: {RETURN TokRBrace		;}
#STD# "~"		: {RETURN TokNot		;}

#STD# AND		: {RETURN TokAnd		;}
#STD# ARRAY		: {RETURN TokArray		;}
#STD# BEGIN		: {RETURN TokBegin		;}
#STD# BY		: {RETURN TokBy			;}
#STD# CASE		: {RETURN TokCase		;}
#STD# CONST		: {RETURN TokConst		;}
#STD# DEFINITION	: {RETURN TokDefinition		;}
#STD# DIV		: {RETURN TokDiv		;}
#STD# DO		: {RETURN TokDo			;}
#STD# ELSE		: {RETURN TokElse		;}
#STD# ELSIF		: {RETURN TokElsif		;}
#STD# END		: {RETURN TokEnd		;}
#STD# EXIT		: {RETURN TokExit		;}
#STD# EXPORT		: {RETURN TokExport		;}
#STD# FOR		: {RETURN TokFor		;}
#STD# FROM		: {RETURN TokFrom		;}
#STD# IF		: {RETURN TokIf			;}
#STD# IMPLEMENTATION	: {RETURN TokImplementation	;}
#STD# IMPORT		: {RETURN TokImport		;}
#STD# IN		: {RETURN TokIn			;}
#STD# LOOP		: {RETURN TokLoop		;}
#STD# MOD		: {RETURN TokMod		;}
#STD# MODULE		: {RETURN TokModule		;}
#STD# \NOT		: {RETURN TokNot		;}
#STD# OF		: {RETURN TokOf			;}
#STD# OR		: {RETURN TokOr			;}
#STD# POINTER		: {RETURN TokPointer		;}
#STD# PROCEDURE		: {RETURN TokProcedure		;}
#STD# QUALIFIED		: {RETURN TokQualified		;}
#STD# RECORD		: {RETURN TokRecord		;}
#STD# REPEAT		: {RETURN TokRepeat		;}
#STD# RETURN		: {RETURN TokReturn		;}
#STD# SET		: {RETURN TokSet		;}
#STD# THEN		: {RETURN TokThen		;}
#STD# TO		: {RETURN TokTo			;}
#STD# TYPE		: {RETURN TokType		;}
#STD# UNTIL		: {RETURN TokUntil		;}
#STD# VAR		: {RETURN TokVar		;}
#STD# WHILE		: {RETURN TokWhile		;}
#STD# WITH		: {RETURN TokWith		;}
#STD# FOREIGN		: {RETURN TokForeign		;}

#STD# letter (letter | digit) * : {
			   GetWord (Word);
			   Attribute.Ident := MakeIdent (Word);
			   RETURN TokIdent;}
