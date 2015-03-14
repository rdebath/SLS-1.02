DEFINITION MODULE Parser;

(* 'modula.ell' line 15 *)

FROM Tree	IMPORT tTree;

CONST
  RelOpr		= 1; 	(* (some) nonterminals of the	*)
  AddOpr		= 2;	(* parsing grammar		*)
  MulOpr		= 3;
  Block			= 4;

TYPE
  tParsAttribute	= RECORD  (* type for nonterminal attributes	*)
				  (* used during tree construction	*)

	(* synthesized and/or inherited attributes of nonterminals	*)
	CASE : SHORTCARD OF		 
	| RelOpr, AddOpr, MulOpr: Operator	: SHORTCARD;
	| Block			: Decls, Stmts	: tTree;
	ELSE			  Tree		: tTree;
	END; (* CASE *)
    END; (* RECORD *)


VAR
  ParsAttribute	: tParsAttribute;
  ParsTabName	: ARRAY [0..128] OF CHAR;

PROCEDURE Parser (): INTEGER;
PROCEDURE CloseParser ();
PROCEDURE xxTokenName (Token: CARDINAL; VAR Name: ARRAY OF CHAR);

END Parser.
