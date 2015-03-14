(*
 *	M T C  -  Modula-2 to C Translator
 *      ----------------------------------
 *
 *	Purpose: evaluation of constant expressions
 *
 *	$Author: grosch $
 *	$Date: 1991/11/21 16:57:59 $
 *	$Revision: 1.2 $
 *
 ***)

DEFINITION MODULE Values;

FROM SYSTEM	IMPORT ADDRESS;
FROM IO		IMPORT tFile;
FROM StringMem	IMPORT tStringRef;
(*FROM Tree	IMPORT tTree;*)
(*FROM Defs	IMPORT tEnv, tType;*)

CONST
   NoValue	=  0;		(* value kind		*)
   Integer	=  1;
   Real		=  2;
   Boolean	=  3;
   Char		=  4;
   StringChar	=  5;		(* string of length 1	*)
   String	=  6;		(* string of length > 1	*)
   Bitset	=  7;
   Enumeration	=  8;
   NilType	=  9;
   StdProc	= 10;
   Type		= 11;

TYPE tValue	= RECORD		(* type to represent values	*)
		   CASE		  Kind		: SHORTINT	OF
		   | Integer	: IntValue	: LONGINT	;
		   | Real	: RealValue	: LONGREAL	;
		   | Boolean	: BoolValue	: BOOLEAN	;
		   | Char	,
		     StringChar	: CharValue	: CHAR		;
		   | String	: StringValue	: tStringRef	;
		   | Bitset	: BitsetValue	: BITSET	;
		   | Enumeration: EnumValue	: ADDRESS	;
		   | StdProc	: StdProcValue	: SHORTCARD	;
		   | Type	: TypeValue	: ADDRESS	;
		   END;
		END;

VAR
   ErrorValue		,	(* predefined values	*)
   NilValue		,
   TrueValue		,
   FalseValue		,
   MinCharVal		,
   MaxCharVal		,
   ZeroValue		,
   MinShortIntVal	,
   MaxShortIntVal	,
   MinLongIntVal	,
   MaxLongIntVal	,
   MaxShortCardVal	,
   MaxLongCardVal	,	(* = ErrorValue (impl. restriction)	*)
   MinRealVal		,
   MaxRealVal		,
   MinLongRealVal	,
   MaxLongRealVal	: tValue;

PROCEDURE CompConst	(Tree(*:tTree*), Env(*:tEnv*): ADDRESS; VAR Value: tValue);
			(* returns in parameter 'Value' the result of	*)
			(* evaluating the expression 'Tree' using	*)
			(* the environment 'Env'			*)

PROCEDURE TypeOfValue	(Value: tValue): ADDRESS; (*:tType *)
			(* returns the type of value 'Value'		*)

PROCEDURE ValueToInt	(Value: tValue): LONGINT;
			(* the value 'Value' is converted to an integer	*)
			(* value					*)

PROCEDURE WriteValue	(f: tFile; Value: tValue);
			(* the value 'Value' is printed on file 'f'	*)

END Values.
