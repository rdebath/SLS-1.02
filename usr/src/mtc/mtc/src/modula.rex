/*
 *	M T C  -  Modula-2 to C Translator
 *      ----------------------------------
 *
 *	Purpose: Scanner Specification for Modula-2
 *		 generated scanner recognizes tokens and computes their
 *		 attributes
 *
 *	$Author: grosch $
 *	$Date: 1992/09/24 13:20:39 $
 *	$Revision: 1.7 $
 *
 ***/

EXPORT	{
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
}

GLOBAL	{
FROM Strings	IMPORT
  tString	, AssignEmpty	, Concatenate	, Append	,
  SubString	, Length	, StringToNumber, ArrayToString	;

FROM StringMem	IMPORT
  tStringRef	, PutString	;

FROM Idents	IMPORT
  tIdent	, NoIdent	, MakeIdent	;

FROM Errors	IMPORT
  ErrorMessageP	, Error		, Warning	,
  IllegalChar	, UnclosedComment,UnclosedString, Underscores	;

FROM Tokens	IMPORT
  TokIdent	, TokDecConst	, TokOctalConst	, TokHexConst	,
  TokCharConst	, TokRealConst	,
  TokStringConst, TokNotEqual	, TokLParent	, TokRParent	,
  TokTimes	, TokPlus	, TokComma	, TokMinus	,
  TokDot	, TokRange	, TokDivide	, TokColon	,
  TokAssign	, TokSemiColon	, TokLess	, TokLessEqual	,
  TokEqual	, TokGreater	, TokGreaterEqual, TokLBracket	,
  TokRBracket	, TokArrow	, TokLBrace	, TokBar	,
  TokRBrace	, TokAnd	, TokArray	, TokBegin	,
  TokBy		, TokCase	, TokConst	, TokDefinition	,
  TokDiv	, TokDo		, TokElse	, TokElsif	,
  TokEnd	, TokExit	, TokExport	, TokFor	,
  TokFrom	, TokIf		, TokImplementation, TokImport	,
  TokIn		, TokLoop	, TokMod	, TokModule	,
  TokNot	, TokOf		, TokOr		, TokPointer	,
  TokProcedure	, TokQualified	, TokRecord	, TokRepeat	,
  TokReturn	, TokSet	, TokThen	, TokTo		,
  TokType	, TokUntil	, TokVar	, TokWhile	,
  TokWith	, TokForeign	;

VAR
  NestingLevel	: CARDINAL	;
  DefaultString	,			(* empty string		*)
  DefaultReal	: tStringRef	;	(* 1.0			*)
  String	: tString	;
  UnderscoreUsed: BOOLEAN	;

PROCEDURE ErrorAttribute (Token: CARDINAL; VAR Attribute: tScanAttribute);
BEGIN
  CASE Token OF
  | TokIdent		: Attribute.Ident	:= NoIdent;
  | TokDecConst		,
    TokOctalConst	,
    TokHexConst		: Attribute.IntValue	:= 1;
  | TokCharConst	: Attribute.CharValue	:= CHR (0);
  | TokRealConst	: Attribute.RealValue	:= DefaultReal;
  | TokStringConst	: Attribute.StringValue	:= DefaultString;
  ELSE ;
  END;
END ErrorAttribute;
}

LOCAL	{ VAR String, S, Word: tString; }

BEGIN	{
  UnderscoreUsed := FALSE;
  NestingLevel := 0;

  AssignEmpty (String);
  DefaultString := PutString (String);

  ArrayToString ("1.0", String);
  DefaultReal := PutString (String);
}

DEFAULT	{ ErrorMessageP (IllegalChar, Error, Attribute.Position); }

EOF	{
  (* UnderscoreUsed, NestingLevel, and start state have to be reset,	*)
  (* because scanner is called for more than one file !!!		*)
  IF yyStartState = Comment THEN
    ErrorMessageP (UnclosedComment, Error, Attribute.Position);
    NestingLevel := 0;
  END;
  IF yyStartState # STD THEN
    yyStart (STD);
  END;
  IF UnderscoreUsed THEN
    ErrorMessageP (Underscores, Warning, Attribute.Position);
    UnderscoreUsed := FALSE;
  END;
}
   
DEFINE

  digit		= {0-9}		.
  octalDigit	= {0-7}		.
  hexDigit	= {0-9 A-F}	.
  letter	= {a-z A-Z $}	.
  CmtCh		= - {*(\t\n}	.
  StrCh1	= - {'\t\n}	.
  StrCh2	= - {"\t\n}	.

START	Comment, Str1, Str2

RULES

#STD#      "(*"		:- {NestingLevel := 1; yyStart (Comment);}

#Comment#  "(*"		:- {INC (NestingLevel);}
#Comment#  "*)"		:- {DEC (NestingLevel);
			    IF NestingLevel = 0 THEN yyStart (STD); END;}
#Comment#  "(" | "*" | CmtCh + :- {}

#STD# \f | \r		:- {}

#STD# digit +		,
#STD# digit + / ".."	,
#STD# digit + / BY	: {GetWord (Word);
			   Attribute.IntValue	:= StringToNumber (Word, 10);
			   RETURN TokDecConst;}
#STD# octalDigit + B		: {GetWord (Word);
			   SubString (Word, 1, Length (Word) - 1, String);
			   Attribute.IntValue	:= StringToNumber (String, 8);
			   RETURN TokOctalConst;}
#STD# octalDigit + C		: {GetWord (Word);
			   SubString (Word, 1, Length (Word) - 1, String);
			   Attribute.CharValue	:= CHR (StringToNumber (String, 8));
			   RETURN TokCharConst;}
#STD# digit hexDigit * H : {
			   GetWord (Word);
			   SubString (Word, 1, Length (Word) - 1, String);
			   Attribute.IntValue	:= StringToNumber (String, 16);
			   RETURN TokHexConst;}
#STD# digit + "." digit * (E {+\-} ? digit +) ? : {
			   GetWord (Word);
			   Attribute.RealValue	:= PutString (Word);
			   RETURN TokRealConst;}

#STD#	'		:  {AssignEmpty (String); yyStart (Str1);}
#Str1#	StrCh1 +	:- {GetWord (S); Concatenate (String, S);}
#Str1#	'		:- {yyStart (STD);
			    Attribute.StringValue:= PutString (String);
			    RETURN TokStringConst;}

#STD#	\"		:  {AssignEmpty (String); yyStart (Str2);}
#Str2#	StrCh2 +	:- {GetWord (S); Concatenate (String, S);}
#Str2#	\"		:- {yyStart (STD);
			    Attribute.StringValue:= PutString (String);
			    RETURN TokStringConst;}

#Str1, Str2# \t		:- {Append (String, 11C); yyTab;}
#Str1, Str2# \n		:- {yyEol (0); yyStart (STD);
			    ErrorMessageP (UnclosedString, Error, Attribute.Position);
			    Attribute.StringValue:= PutString (String);
			    RETURN TokStringConst;}

#STD# "#"		: {RETURN TokNotEqual;}
#STD# "&"		: {RETURN TokAnd;}
#STD# "("		: {RETURN TokLParent;}
#STD# ")"		: {RETURN TokRParent;}
#STD# "*"		: {RETURN TokTimes;}
#STD# "+"		: {RETURN TokPlus;}
#STD# ","		: {RETURN TokComma;}
#STD# "-"		: {RETURN TokMinus;}
#STD# "."		: {RETURN TokDot;}
#STD# ".."		: {RETURN TokRange;}
#STD# "/"		: {RETURN TokDivide;}
#STD# ":"		: {RETURN TokColon;}
#STD# ":="		: {RETURN TokAssign;}
#STD# ";"		: {RETURN TokSemiColon;}
#STD# "<"		: {RETURN TokLess;}
#STD# "<="		: {RETURN TokLessEqual;}
#STD# "<>"		: {RETURN TokNotEqual;}
#STD# "="		: {RETURN TokEqual;}
#STD# ">"		: {RETURN TokGreater;}
#STD# ">="		: {RETURN TokGreaterEqual;}
#STD# "["		: {RETURN TokLBracket;}
#STD# "]"		: {RETURN TokRBracket;}
#STD# "^"		: {RETURN TokArrow;}
#STD# "{"		: {RETURN TokLBrace;}
#STD# "|"		: {RETURN TokBar;}
#STD# "}"		: {RETURN TokRBrace;}
#STD# "~"		: {RETURN TokNot;}

#STD# AND		: {RETURN TokAnd;}
#STD# ARRAY		: {RETURN TokArray;}
#STD# BEGIN		: {RETURN TokBegin;}
#STD# BY		: {RETURN TokBy;}
#STD# CASE		: {RETURN TokCase;}
#STD# CONST		: {RETURN TokConst;}
#STD# DEFINITION	: {RETURN TokDefinition;}
#STD# DIV		: {RETURN TokDiv;}
#STD# DO		: {RETURN TokDo;}
#STD# ELSE		: {RETURN TokElse;}
#STD# ELSIF		: {RETURN TokElsif;}
#STD# END		: {RETURN TokEnd;}
#STD# EXIT		: {RETURN TokExit;}
#STD# EXPORT		: {RETURN TokExport;}
#STD# FOR		: {RETURN TokFor;}
#STD# FROM		: {RETURN TokFrom;}
#STD# IF		: {RETURN TokIf;}
#STD# IMPLEMENTATION	: {RETURN TokImplementation;}
#STD# IMPORT		: {RETURN TokImport;}
#STD# IN		: {RETURN TokIn;}
#STD# LOOP		: {RETURN TokLoop;}
#STD# MOD		: {RETURN TokMod;}
#STD# MODULE		: {RETURN TokModule;}
#STD# \NOT		: {RETURN TokNot;}
#STD# OF		: {RETURN TokOf;}
#STD# OR		: {RETURN TokOr;}
#STD# POINTER		: {RETURN TokPointer;}
#STD# PROCEDURE		: {RETURN TokProcedure;}
#STD# QUALIFIED		: {RETURN TokQualified;}
#STD# RECORD		: {RETURN TokRecord;}
#STD# REPEAT		: {RETURN TokRepeat;}
#STD# RETURN		: {RETURN TokReturn;}
#STD# SET		: {RETURN TokSet;}
#STD# THEN		: {RETURN TokThen;}
#STD# TO		: {RETURN TokTo;}
#STD# TYPE		: {RETURN TokType;}
#STD# UNTIL		: {RETURN TokUntil;}
#STD# VAR		: {RETURN TokVar;}
#STD# WHILE		: {RETURN TokWhile;}
#STD# WITH		: {RETURN TokWith;}
#STD# FOREIGN		: {RETURN TokForeign;}

#STD# letter (letter | digit) * : {
			   GetWord (Word);
			   Attribute.Ident	:= MakeIdent (Word);
			   RETURN TokIdent;}

#STD# (letter | "_") (letter | "_" | digit) * : {
			   (* MOCKA allows '_' in Modula-2 identifiers !!! *)
			   UnderscoreUsed	:= TRUE;
			   GetWord (Word);
			   Attribute.Ident	:= MakeIdent (Word);
			   RETURN TokIdent;}
