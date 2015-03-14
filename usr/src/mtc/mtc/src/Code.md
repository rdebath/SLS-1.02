DEFINITION MODULE Code;

IMPORT SYSTEM, IO, Tree;
(* line 17 "" *)

CONST
  cNoOp			=  0;		(* C operators			*)
  cNotEqual		=  1;
  cTimes		=  2;
  cPlus			=  3;
  cMinus		=  4;
  cDivide		=  5;
  cLess			=  6;
  cLessEqual		=  7;
  cEqual		=  8;
  cGreater		=  9;
  cGreaterEqual		= 10;
  cAnd			= 11;
  cIn			= 13;
  cMod			= 14;
  cNot			= 15;
  cOr			= 16;
  cUnion		= 17;
  cDifference		= 18;
  cIntersection		= 19;
  cSymDiff		= 20;
  cIsSubset1		= 21;
  cIsSubset2		= 22;

  cAssign		= 23;
  cPassValue		= 24;
  cPassAddress		= 25;


VAR yyf	: IO.tFile;
VAR Exit	: PROC;

PROCEDURE CodeCompUnits (yyP1: Tree.tTree);

PROCEDURE BeginCode;
PROCEDURE CloseCode;

END Code.
