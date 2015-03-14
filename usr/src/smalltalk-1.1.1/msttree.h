/***********************************************************************
 *
 *	Semantic Tree information declarations.
 *
 ***********************************************************************/

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


/*
 *    Change Log
 * ============================================================================
 * Author      Date       Change 
 * sbyrne    30 Dec 88	  Created.
 *
 */


#ifndef __MSTTREE__
#define __MSTTREE__

typedef enum {
  methodNodeType,
  unaryExprType,
  binaryExprType,
  keywordExprType,
  variableNodeType,
  keywordListType,
  variableListType,
  statementListType,
  returnExprType,
  assignExprType,
  constExprType,
  symbolNodeType,
  arrayEltListType,
  blockNodeType,
  cascadedMessageNodeType,
  messageListType,
} NodeType;

#undef TreeNode
typedef struct TreeNodeStruct *TreeNode;

typedef struct ListNodeStruct {
  char		*name;
  TreeNode 	value;
  TreeNode 	next;
  TreeNode	*nextAddr;
} ListNode;

typedef struct ExprNodeStruct {
  TreeNode	receiver;
  OOP		selector;
  TreeNode	expression;
} ExprNode;

typedef enum {
  intConst,
  floatConst,
  charConst,
  stringConst,
  symbolConst,
  arrayConst
} ConstType;

typedef struct ConstNodeStruct {
  ConstType	constType;
  union {
    long	iVal;
    double	fVal;
    Byte	cVal;
    char	*sVal;
    OOP		symVal;
    TreeNode	aVal;
  } val;
} ConstNode;

typedef struct MethodNodeStruct {
  TreeNode 	selectorExpr;
  TreeNode 	temporaries;
  int		primitiveIndex;
  TreeNode 	statements;
} MethodNode;


struct TreeNodeStruct {
  NodeType	nodeType;
  union {
    ListNode		nvList;
    ExprNode		nvExpr;
    ConstNode		nvConst;
    MethodNode		nvMethod;
  } 		nodeVal;
};

#define vList		nodeVal.nvList
#define vExpr		nodeVal.nvExpr
#define vConst		nodeVal.nvConst
#define vMethod		nodeVal.nvMethod


extern TreeNode		makeMethod(), makeUnaryExpr(), makeBinaryExpr(),
			makeKeywordExpr(), makeVariable(), makeKeywordList(),
			makeVariableList(), makeStatementList(), makeAssign(),
			makeReturn(), makeIntConstant(), makeFloatConstant(),
			makeCharConstant(),makeSymbolConstant(),
			makeStringConstant(), makeArrayConstant(),
			internIdent(), internBinOP(), internKeywordList(),
			makeArrayElt(), makeBlock(), makeCascadedMessage(),
			makeMessageList();

extern void		addNode(), freeTree(), printTree();

extern Boolean 		hadError;


#endif /* __MSTTREE__ */
