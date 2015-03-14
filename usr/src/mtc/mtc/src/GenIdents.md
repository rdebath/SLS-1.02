(*
 *	M T C  -  Modula-2 to C Translator
 *      ----------------------------------
 *
 *	Purpose: administration of compiler generated identifiers
 *
 *	$Author: grosch $
 *	$Date: 1991/11/21 16:57:59 $
 *	$Revision: 1.2 $
 *
 ***)

DEFINITION MODULE GenIdents;

FROM Idents IMPORT tIdent;

PROCEDURE MakeQualified	(Module, Ident: tIdent): tIdent;
			(* returns the identifier Module_Ident.		*)

PROCEDURE RenameField	(Ident: tIdent): tIdent;
			(* returns the identifier C_0_Ident.		*)

PROCEDURE Rename	(Ident: tIdent): tIdent;
			(* returns an identifier C_nnn_Ident, whereby	*)
			(* nnn is an unique number > 0.			*)

PROCEDURE GenLabel	(): tIdent;
			(* returns an identifier EXIT_nnn, whereby nnn	*)
			(* is an unique number > 0.			*)

PROCEDURE GenSelector	(StructOrUnion: CHAR; Nr: CARDINAL): tIdent;
			(* returns the identifier StructOrUnion_Nr.	*)

PROCEDURE GenParam	(): tIdent;
			(* returns an identifier O_nnn, whereby nnn is	*)
			(* an unique number > 0.			*)

PROCEDURE GenWith	(): tIdent;
			(* returns an identifier W_nnn, whereby nnn is	*)
			(* an unique number > 0.			*)

PROCEDURE GenStruct1	(Module: tIdent; Nr: CARDINAL): tIdent;
			(* returns the identifier Module_Nr or		*)
			(* Module__Nr (if the identifier Module		*)
			(* consists of a single character)		*)

PROCEDURE GenStruct2	(): tIdent;
			(* returns an identifier S_nnn, whereby nnn is	*)
			(* an unique number > 0.			*)

PROCEDURE GenGlobalPtr	(Ident: tIdent): tIdent;
			(* returns an identifier G_nnn_Ident, whereby	*)
			(* nnn is an unique number > 0.			*)

PROCEDURE GenLocalPtr	(): tIdent;
			(* returns an identifier L_nnn, whereby nnn is	*)
			(* an unique number > 0.			*)

PROCEDURE GenBound	(): tIdent;
			(* returns an identifier B_nnn, whereby nnn is	*)
			(* an unique number > 0.			*)

PROCEDURE GenReturn	(): tIdent;
			(* returns an identifier R_nnn, whereby nnn is	*)
			(* an unique number > 0.			*)

PROCEDURE GenString	(): tIdent;
			(* returns an identifier X_nnn, whereby nnn is	*)
			(* an unique number > 0.			*)

PROCEDURE GenOpaque	(TypeName: tIdent): tIdent;
			(* PRECONDITION: TypeName = Module_Ident 	*)
			(* returns the identifier TypeName without	*)
			(* leading module name and underscore.		*)
			(* i.e. GenOpaque (Module_Ident) = Ident	*)
			
END GenIdents.
