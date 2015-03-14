(*
 *	M T C  -  Modula-2 to C Translator
 *      ----------------------------------
 *
 *	Purpose: administration of data types
 *
 *	$Author: grosch $
 *	$Date: 1991/11/21 16:57:59 $
 *	$Revision: 1.2 $
 *
 ***)

DEFINITION MODULE Types;

FROM Values	IMPORT tValue;
FROM Defs	IMPORT tType, tTypes;

CONST
  (* The MIN and MAX values depend on both the C and	*)
  (* the Modula-2 compiler				*)	
  MinShortInt		= -32768;
  MaxShortInt		=  32767;
  MinLongInt		= -2147483648;
  MaxLongInt		=  2147483647;
  MaxShortCard		=  65535;
  MaxLongCard		=  4294967295;
  MinChar		=  0C;
  MaxChar		=  377C;
  MaxBitset		=  31;

VAR
  MinReal		,
  MaxReal		: REAL;
  MinLongReal		,
  MaxLongReal		: LONGREAL;

CONST
  NoSize		= 0;	(* no type size		*)

  SizeUnsignedChar	= 1;	(* size of C types	*)
  SizeShort		= 2;
  SizeLong		= 4;
  SizeUnsignedShort	= 2;
  SizeUnsignedLong	= 4;
  SizeFloat		= 4;
  SizeDouble		= 8;
  PointerSize		= 4;

PROCEDURE ResultType	(Operator: SHORTCARD; t1, t2: tType): tType;
			(* return the type of the result of applying	*)
			(* operator 'Operator' to  operands of types	*)
			(* 't1' and 't2'				*)

PROCEDURE StdResultType	(t: tType; a: tTypes): tType;
			(* PRECONDITION: Type^.Kind = StdProcType1	*)
			(* return the result type of the call of the	*)
			(* standard procedure with type 't' and the	*)
			(* list of actual parameter types 'a'		*)

PROCEDURE TypeSize	(t: tType): CARDINAL;
			(* return the number of bytes used for an	*)
			(* object of type 't'				*)
			(* compiler restriction: TypeSize returns	*)
			(* NoSize for record and array types, because	*)
			(* the size of these types depends on the	*)
			(* memory mapping of the C compiler		*)

PROCEDURE GetLwb	(t: tType; VAR Lwb: tValue);
			(* returns the lower bound of type 't' in	*)
			(* parameter 'Lwb'				*)

PROCEDURE GetUpb	(t: tType; VAR Upb: tValue);
			(* returns the upper bound of type 't' in	*)
			(* parameter 'Upb'				*)

PROCEDURE Cast		(Operator: SHORTCARD; t1, t2: tType): BOOLEAN;
			(* checks if the translator must emit a type	*)
			(* cast if the operator 'Operator' is applied	*)
			(* to an operand of type 't2' and the result	*)
			(* should be of type 't1'			*)

END Types.
