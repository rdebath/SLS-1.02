(*
 *	M T C  -  Modula-2 to C Translator
 *      ----------------------------------
 *
 *	Purpose: associate identifiers with objects of arbitrary types
 *
 *	$Author: grosch $
 *	$Date: 1991/11/21 16:57:59 $
 *	$Revision: 1.2 $
 *
 ***)

DEFINITION MODULE AssocTab;

FROM SYSTEM	IMPORT ADDRESS;
FROM Idents	IMPORT tIdent;

PROCEDURE BeginAssocTab;
			(* initialize the association table		*)

PROCEDURE PutAssoc	(Ident: tIdent; Object: ADDRESS);
			(* associate object 'Object' with identifier	*)
			(* 'Ident'.					*)

PROCEDURE GetAssoc	(Ident: tIdent; VAR Object: ADDRESS);
			(* return in 'Object' the object currently	*)
			(* associated with identfier 'Ident'		*)

PROCEDURE CloseAssocTab;
			(* finalize the association table		*)

END AssocTab.
