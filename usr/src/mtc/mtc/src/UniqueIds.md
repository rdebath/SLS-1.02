(*
 *	M T C  -  Modula-2 to C Translator
 *      ----------------------------------
 *
 *	Purpose: administration of Modula-2 identifiers used in the
 *		 generated C program(s) (according to C scope rules)
 *
 *	$Author: grosch $
 *	$Date: 1991/11/21 16:57:59 $
 *	$Revision: 1.3 $
 *
 ***)

DEFINITION MODULE UniqueIds;

FROM Idents	IMPORT tIdent;

TYPE
  tIdents	= (cIdents);	(* dummy type (used to define	*)
				(* attribute dependencies)	*)

  tIdentClass	= (eKeyword, eConst, eType, eVar, eModuleVar, eProc, eField);

(* The following identifiers are treated as keywords, i.e.	*)
(* NameConflict ( Idents, IdentClass, KeywordIdent ) = TRUE :	*)
(*								*)
(* 1. C keywords and preprocessor directives:			*)
(*								*)
(*    auto, break, case, char, continue, default, define, do	*)
(*    double, else, endif, entry, enum, extern, float, for,	*)
(*    goto, if, ifdef, ifndef, include, int, line, long,	*)
(*    register, return, short, sizeof, static, struct, switch,	*)
(*    typedef, undef, union, unsigned, void, while		*)
(*								*)
(* 2. Identifiers which are defined in SYSTEM_.h and which may	*)
(*    collide with Modula-2 identifiers:			*)
(*								*)
(*    TRUE, FALSE, NIL,						*)
(*    SHORTINT, INTEGER, LONGINT, SHORTCARD, CARDINAL, LONGCARD,*)
(*    BOOLEAN, CHAR, REAL, LONGREAL,				*)
(*    BITSET, PROC, BYTE, WORD, ADDRESS,			*)
(*    ABS, ABSSI, ABSLI, ABSSC, ABSLC, ABSR, ABSLR, CAP, CHR,	*)
(*    FLOAT, ORD, TRUNC, VAL, ODD,				*)
(*    INC, INC1, DEC, DEC1, EXCL, INCL, ADR, ADR1,		*)
(*    OPAQUE, STRING,						*)
(*    CaseError, ReturnError,					*)
(*    alloca, malloc, free, memcpy, strncpy, exit,		*)
(*    StackAlloc						*)
(*								*)
(* 3. Compiler generated identifiers which may collide with	*)
(*    Modula-2 identifiers:					*)
(*								*)
(*    A, dummy							*)

PROCEDURE BeginUniqueIds;
			(* Initialize module UniqueIds			*)

PROCEDURE EnterProc	(Idents: tIdents): tIdents;
			(* Enter local declarations of a procedure	*)

PROCEDURE LeaveProc	(Idents: tIdents): tIdents;
			(* Leave local declarations of a procedure	*)

PROCEDURE DeclareIdent	(
			Idents		: tIdents;
			IdentClass	: tIdentClass;
			Ident		: tIdent
			):		  tIdents;
			(* PRECONDITION:				*)
			(* NOT NameConflict (Idents, IdentClass, Ident)	*)
			(* Declare the identifier 'Ident' of class	*)
			(* 'IdentClass' as used in the generated C	*)
			(* program(s)					*)

PROCEDURE NameConflict	(
			Idents		: tIdents;
			IdentClass	: tIdentClass;
			Ident		: tIdent
			):		  BOOLEAN;
			(* Check if a name conflict with an identifier	*)
			(* which has been already declared would result	*)
			(* from using the identifier 'Ident' of class	*)
			(* 'IdentClass' in the generated C program(s)	*)

PROCEDURE CloseUniqueIds;
			(* Finalize module UniqueIds			*)

END UniqueIds.
