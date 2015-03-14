/* -*- bison -*- */

/***********************************************************************
 *
 * Copyright (C) 1990, 1991 Free Software Foundation, Inc.
 * Written by Steve Byrne.
 *
 * This file is part of GNU Smalltalk.
 *
 * GNU Smalltalk is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 1, or (at your option) any later 
 * version.
 * 
 * GNU Smalltalk is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 * 
 * You should have received a copy of the GNU General Public License along with
 * GNU Smalltalk; see the file COPYING.  If not, write to the Free Software
 * Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  
 *
 ***********************************************************************/

%{
#include "mst.h"
#include "mstsym.h"
#include "msttree.h"
#include "mstdict.h"
#include <stdio.h>
#ifdef HAS_ALLOCA_H
#include <alloca.h>
#endif

#define YYDEBUG 1

extern Boolean		quietExecution;

%}

%pure_parser

%union{
  char		cval;
  double	fval;
  long		ival;
  char		*sval;
  TreeNode	node;
}

/* single definite characters */     
%token BANG COLON UPARROW DOT ASSIGN SHARP SEMICOLON
%token OPEN_PAREN CLOSE_PAREN OPEN_BRACKET CLOSE_BRACKET
%token PRIMITIVE_START INTERNAL_TOKEN

/* larger lexical items */
%token <sval> IDENTIFIER KEYWORD STRING_LITERAL SYMBOL_KEYWORD BINOP
              VERTICAL_BAR 
%token <ival> INTEGER_LITERAL
%token <fval> FLOATING_LITERAL
%token <cval> CHAR_LITERAL

%type <node> method message_pattern variable_name keyword_variable_list
	temporaries variable_names statements non_empty_statements expression
	assigns primary number symbol_constant symbol 
	character_constant string array_constant array
	array_constant_list block opt_block_variables 
	block_variable_list unary_expression binary_expression
	keyword_expression keyword_binary_object_description_list
	cascaded_message_expression semi_message_list
	message_elt simple_expression literal message_expression
	array_constant_elt unary_object_description
	binary_object_description
%type <sval> unary_selector keyword binary_selector
%type <ival> primitive
%%

program:
	class_definition_list
	| internal_marker method	{ compileMethod($2); }
	;

internal_marker:
	INTERNAL_TOKEN			{ clearMethodStartPos(); }

class_definition_list:
	class_definition
	| class_definition_list class_definition
	;

class_definition:
	class_header method_list BANG
	| class_header  BANG
	| non_empty_statements BANG	{ if (!hadError) {
					    executeStatements(nil, $1,
							    quietExecution); 
					  }
					  hadError = false;
					}
	| temporaries non_empty_statements BANG	
					{ if (!hadError) {
					    executeStatements($1, $2,
							    quietExecution); 
                                          }
					  hadError = false;
                                        }
	| error BANG			{ hadError = false; }
	;

class_header:
	BANG class_specification BANG 	{ clearMethodStartPos(); }
	;

class_specification:
	simple_expression	{ executeStatements(nil, 
				    makeStatementList($1, nil), true); }
	;

/*
method_list:
	method 				{ if (!hadError) {
					    compileMethod($1);
					  } else {
					    hadError = false;
					  }
					}
        | method_list method		{ if (!hadError) {
					    compileMethod($2);
					  } else {
					    hadError = false;
					  }
					}
	;
     
method:
	message_pattern statements BANG 
					{ $$ = makeMethod($1, nil, 0, $2); }
	| message_pattern temporaries statements BANG
					{ $$ = makeMethod($1, $2, 0, $3); }
	| message_pattern primitive statements BANG
					{ $$ = makeMethod($1, nil, $2, $3); }
	| message_pattern temporaries primitive statements BANG
		{ $$ = makeMethod($1, $2, $3, $4); }
	;

*/
method_list:
	method BANG 			{ if (!hadError) {
					    compileMethod($1);
					    clearMethodStartPos();
					  } else {
					    hadError = false;
					  }
					}
        | method_list method BANG	{ if (!hadError) {
					    compileMethod($2);
					    clearMethodStartPos();
					  } else {
					    hadError = false;
					  }
					}
	;
     
method:
	message_pattern statements 
					{ $$ = makeMethod($1, nil, 0, $2); }
	| message_pattern temporaries statements
					{ $$ = makeMethod($1, $2, 0, $3); }
	| message_pattern primitive statements 
					{ $$ = makeMethod($1, nil, $2, $3); }
	| message_pattern temporaries primitive statements
		{ $$ = makeMethod($1, $2, $3, $4); }
	;


message_pattern:
	unary_selector			{ $$ = makeUnaryExpr(nil, $1); }
	| binary_selector variable_name	{ $$ = makeBinaryExpr(nil, $1,
						              $2); }
	| keyword_variable_list		{ $$ = makeKeywordExpr(nil, $1); }
	| error				{ errorf("Invalid message pattern");
					  hadError = true;
					  $$ = nil; }
	;

unary_selector:
	IDENTIFIER
	;

binary_selector:
	BINOP			/* I don't like this usage of binop */
	| VERTICAL_BAR
	;

variable_name:
	IDENTIFIER			{ $$ = makeVariable($1); }
	;

keyword_variable_list:
	keyword variable_name		{ $$ = makeKeywordList($1, $2); }
	| keyword_variable_list keyword variable_name
					{ addNode($1, makeKeywordList($2, $3));
					  $$ = $1; }
	;

keyword:
	KEYWORD
	;

primitive:
	PRIMITIVE_START INTEGER_LITERAL BINOP
					{ $$ = $2;
					  if (strcmp($3, ">") != 0) {
					    YYERROR;
					  }
					}

temporaries:
	VERTICAL_BAR VERTICAL_BAR	{ $$ = nil; }
	| VERTICAL_BAR variable_names VERTICAL_BAR
					{ $$ = $2; }
	;

variable_names:
	variable_name			{ $$ = makeVariableList($1); }
	| variable_names variable_name	{ addNode($1, makeVariableList($2));
					  $$ = $1; }
	;

statements:
	/* empty */			{ $$ = nil; }
	| non_empty_statements
	;

non_empty_statements:
	UPARROW expression 	
				{ $$ = makeStatementList(makeReturn($2),
				       			nil); }
	| expression		{ $$ = makeStatementList($1, nil); }
	| expression DOT statements
			 	/* I don't know if I like this production */
				{ $$ = makeStatementList($1, $3); }
	| error DOT statements  { $$ = $3;
				  yyerrok;
				  errorf("Error in expression");
				  hadError = true;
				}
	;
	
expression:
	simple_expression
	| assigns simple_expression	{ $$ = makeAssign($1, $2); }
	;

assigns:
	variable_name ASSIGN		{ $$ = makeVariableList($1); }
	| assigns variable_name ASSIGN
					{ addNode($1, makeVariableList($2));
					  $$ = $1; }
	;

simple_expression:
	primary
	| message_expression
	| cascaded_message_expression
	;

primary:
	variable_name
	| literal
	| block				
	| OPEN_PAREN expression CLOSE_PAREN { $$ = $2; }
	;

literal:
	number
	| symbol_constant
	| character_constant
	| string
	| array_constant
	;

number:
	INTEGER_LITERAL			{ $$ = makeIntConstant($1); }
	| FLOATING_LITERAL		{ $$ = makeFloatConstant($1); }
	;

symbol_constant:
	SHARP symbol			{ $$ = makeSymbolConstant($2); }
	;

symbol:
	IDENTIFIER			{ $$ = internIdent($1); }
	| binary_selector		{ $$ = internBinOP($1); }
	| SYMBOL_KEYWORD		{ $$ = internIdent($1); }
	| KEYWORD			{ $$ = internIdent($1); }
	;


character_constant:
	CHAR_LITERAL			{ $$ = makeCharConstant($1); }
	;

string:
	STRING_LITERAL			{ $$ = makeStringConstant($1); }
	;

array_constant:
	SHARP array			{ $$ = makeArrayConstant($2); }
	;

array:
	OPEN_PAREN CLOSE_PAREN		{ $$ = nil; }
	| OPEN_PAREN array_constant_list CLOSE_PAREN
					{ $$ = $2; }
	;

array_constant_list:
	array_constant_elt		{ $$ = makeArrayElt($1); }
	| array_constant_list array_constant_elt
			      		{ addNode($1, makeArrayElt($2));
					  $$ = $1; }
	;

array_constant_elt:
	number
	| symbol
	| string
	| character_constant
	| array
	;

block:
	OPEN_BRACKET opt_block_variables statements CLOSE_BRACKET
					{ $$ = makeBlock($2, $3); }
	;

opt_block_variables:
	/* empty */			{ $$ = nil; }
	| block_variable_list VERTICAL_BAR
	;

/* syntax for blocks with temporaries is just args and vertical bar (if
 * any followed by a standard temporaries declarations */

block_variable_list:
	COLON variable_name		{ $$ = makeVariableList($2); }
	| block_variable_list COLON variable_name
				    	{ addNode($1, makeVariableList($3));
					  $$ = $1; }
	;

message_expression:
	unary_expression
	| binary_expression
	| keyword_expression
	;

unary_expression:
	unary_object_description unary_selector { $$ = makeUnaryExpr($1, $2); }
	;

unary_object_description:
	primary
	| unary_expression
	;

binary_expression:
	binary_object_description binary_selector unary_object_description
					{ $$ = makeBinaryExpr($1, $2, $3); }
	;

binary_object_description:
	unary_object_description
	| binary_expression
	;

keyword_expression:
	binary_object_description keyword_binary_object_description_list
					{ $$ = makeKeywordExpr($1, $2); }
 	;

keyword_binary_object_description_list:
	keyword binary_object_description
					{ $$ = makeKeywordList($1, $2); }
	| keyword_binary_object_description_list keyword
	  binary_object_description	{ addNode($1, makeKeywordList($2, $3));
					  $$ = $1; }
	;

cascaded_message_expression:
	message_expression semi_message_list
			   		{ $$ = makeCascadedMessage($1, $2); }
	;

semi_message_list:
	SEMICOLON message_elt		{ $$ = makeMessageList($2); }
	| semi_message_list SEMICOLON message_elt
					{ addNode($1, makeMessageList($3));
					  $$ = $1; }
	;

message_elt:
	unary_selector			{ $$ = makeUnaryExpr(nil, $1); }
	| binary_selector unary_object_description
					{ $$ = makeBinaryExpr(nil, $1, $2); }
	| keyword_binary_object_description_list
					{ $$ = makeKeywordExpr(nil, $1); }
	;


%%
/*     
ADDITIONAL C CODE
*/

