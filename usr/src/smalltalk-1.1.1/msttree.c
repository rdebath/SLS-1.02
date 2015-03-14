/***********************************************************************
 *
 *	Semantic Tree manipulation module.
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


#include "mst.h"
#include "mstsym.h"
#include "msttree.h"
#include <stdio.h>

char	*nilName = "(nil)"; /* how to print nil */

Boolean hadError = false;

static TreeNode makeMethodNode(), makeListNode(), makeExprNode();
static TreeNode makeTreeNode();
static OOP	makeUnarySelector(), makeBinarySelector();
static void	freeNode(), freeMethodNode(), freeExprNode(), freeListNode(),
  		freeConstNode();
static void	printMethodNode(), printExprNode(), printListNode(),
  		printConstNode(), printNodeType(), indent(), printSelector();

static OOP	*binopSymbols[] = { /* indexed by binop */
  &nilSymbol,			/* there is no zeroth entry  */
  &plusSymbol,
  &minusSymbol,
  &timesSymbol,
  &divideSymbol,
  &lessThanSymbol,
  &greaterThanSymbol,
  &equalSymbol,
  &notEqualSymbol,
  &lessEqualSymbol,
  &greaterEqualSymbol,
  &integerDivideSymbol,
  &remainderSymbol,
  &sameObjectSymbol,
  &notSameObjectSymbol,
  &orSymbol,
  &andSymbol
};

/* Used only for printing tree node names when debugging */
static char *nodeTypeNames[] = {
  "methodNodeType", 		/* methodNodeType */
  "unaryExprType", 		/* unaryExprType */
  "binaryExprType", 		/* binaryExprType */
  "keywordExprType", 		/* keywordExprType */
  "variableNodeType", 		/* variableNodeType */
  "keywordListType", 		/* keywordListType */
  "variableListType", 		/* variableListType */
  "statementListType", 		/* statementListType */
  "returnExprType", 		/* returnExprType */
  "assignExprType", 		/* assignExprType */
  "constExprType", 		/* constExprType */
  "symbolNodeType", 		/* symbolNodeType */
  "arrayEltListType", 		/* arrayEltListType */
  "blockNodeType", 		/* blockNodeType */
  "cascadedMessageNodeType", 	/* cascadedMessageNodeType */
  "messageListType" 		/* messageListType */
};



/*
 *	TreeNode makeArrayElt(elt)
 *
 * Description
 *
 *	Create an element of an array constant, which is a list type object.
 *	Return the element with the next field NILed out.
 *
 * Inputs
 *
 *	elt   : TreeNode array element to use
 *
 * Outputs
 *
 *	TreeNode of type arrayEltListType that contains "elt".
 */
TreeNode makeArrayElt(elt)
TreeNode elt;
{
  return (makeListNode(arrayEltListType, nil, elt));
}

  
/*
 *	TreeNode makeMethod(selectorExpr, temporaries, primitiveIndex,
 *			    statements)
 *
 * Description
 *
 *	Create a method node.  The method will be invoked by a selector dervied
 *	from "selectorExpr", it has (possibly nil) "temporaries" variables,
 *	and contains "statements".  If the method has a primitive associated
 *	with it, then "primitiveIndex" is non-zero.
 *
 * Inputs
 *
 *	selectorExpr: 
 *		Expression that's to be the selector for this method.
 *	temporaries: 
 *		Possibly nil list of temporary variable names.
 *	primitiveIndex:
 *		Integer.  If non-zero, this method has associated with it
 *		a primitive with index "primitiveIndex".
 *	statements: 
 *		List of statements that comprise the procedural part of this
 *		method.
 *
 * Outputs
 *
 *	TreeNode of type methodNodeType.
 */
TreeNode makeMethod(selectorExpr, temporaries, primitiveIndex, statements)
TreeNode selectorExpr, temporaries, statements;
int	primitiveIndex;
{
  return (makeMethodNode(methodNodeType, selectorExpr,
			 temporaries, primitiveIndex, statements));
}


/*
 *	TreeNode makeCascadedMessage(messageExpr, cascadedMessages)
 *
 * Description
 *
 *	Creates a node for holding a list of cascaded messages (basically an
 *	Expr node that isn't using its symbol.  "messageExpr" is the expression
 *	invoke first as it computes the receiver.  Then the remaining cascaded
 *	messages are sent to that receiver.
 *
 * Inputs
 *
 *	messageExpr: 
 *		Evaluates to the receiver of the cascaded messages
 *	cascadedMessages: 
 *		List of the cascaded messages to send to the receiver.
 *
 * Outputs
 *
 *	TreeNode of type cascadedMessageTypeNode.
 */
TreeNode makeCascadedMessage(messageExpr, cascadedMessages)
TreeNode messageExpr, cascadedMessages;
{
  return (makeExprNode(cascadedMessageNodeType, messageExpr, nil,
		       cascadedMessages));
}


TreeNode makeUnaryExpr(receiver, unarySelectorExpr)
TreeNode receiver;
char	 *unarySelectorExpr;
{
  OOP		selector;

  selector = makeUnarySelector(unarySelectorExpr);
  return (makeExprNode(unaryExprType, receiver, selector, nil));
}

TreeNode internBinOP(binaryOp)
char	*binaryOp;
{
  return (makeExprNode(symbolNodeType, nil, makeBinarySelector(binaryOp),
		       nil));
}

TreeNode internIdent(ident)
char	*ident;
{
  return (makeExprNode(symbolNodeType, nil, internString(ident), nil));
}

TreeNode makeStatementList(expression, statements)
TreeNode expression, statements;
{
  return (makeExprNode(statementListType, expression, nilOOP, statements));
}

TreeNode makeReturn(expression)
TreeNode expression;
{
  return (makeExprNode(returnExprType, expression, nilOOP, nil));
}

TreeNode makeKeywordExpr(receiver, keywordMessage)
TreeNode receiver, keywordMessage;
{
  return (makeExprNode(keywordExprType, receiver, nilOOP, keywordMessage));
}

TreeNode makeAssign(variables, expression)
TreeNode variables, expression;
{
  return (makeExprNode(assignExprType, variables, nilOOP, expression));
}

TreeNode makeKeywordList(keyword, expression)
char	*keyword;
TreeNode expression;
{
  return (makeListNode(keywordListType, keyword, expression));
}

/*
 *	TreeNode makeVariableList(variable)
 *
 * Description
 *
 *	Given a variable tree node, this routine returns a variable list tree
 *	node with a nil next link.  Actually, we rely on the fact that a
 *	variable is represented as a tree node of type ListNode, so  all we do
 *	is change the node tag to variableListType.
 *
 * Inputs
 *
 *	variable: 
 *		Name of variable that's to be part of the list, TreeNode.
 *
 * Outputs
 *
 *	New TreeNode.
 */
TreeNode makeVariableList(variable)
TreeNode variable;
{
  variable->nodeType = variableListType;
  return (variable);
}


TreeNode makeBinaryExpr(receiver, binaryOp, argument)
TreeNode receiver, argument;
char	*binaryOp;
{
  OOP		selector;

  selector = makeBinarySelector(binaryOp);
  return (makeExprNode(binaryExprType, receiver, selector, argument));
}

TreeNode makeMessageList(messageElt)
TreeNode messageElt;
{
  return (makeListNode(messageListType, nil, messageElt));
}

/*
 *	TreeNode makeBlock(temporaries, statements)
 *
 * Description
 *
 *	Creates a block tree node and returns it.
 *
 * Inputs
 *
 *	temporaries: 
 *		Possibly nil list of temporary variable names to use for this
 *		block 
 *	statements: 
 *		List of statements that are the procedure part of this block.
 *
 * Outputs
 *
 *	New tree node.
 */
TreeNode makeBlock(temporaries, statements)
TreeNode temporaries, statements;
{
  return (makeMethodNode(blockNodeType, nil, temporaries, 0, statements));
}

TreeNode makeVariable(name)
char	*name;
{
  return (makeListNode(variableNodeType, name, nil));
}


TreeNode makeIntConstant(ival)
long	ival;
{
  TreeNode 	result;

  result = makeTreeNode(constExprType);
  result->vConst.constType = intConst;
  result->vConst.val.iVal = ival;

  return (result);
}

TreeNode makeFloatConstant(fval)
double	fval;
{
  TreeNode 	result;

  result = makeTreeNode(constExprType);
  result->vConst.constType = floatConst;
  result->vConst.val.fVal = fval;

  return (result);
}

TreeNode makeCharConstant(cval)
char	cval;
{
  TreeNode 	result;

  result = makeTreeNode(constExprType);
  result->vConst.constType = charConst;
  result->vConst.val.cVal = cval;

  return (result);
}

TreeNode makeStringConstant(sval)
char	*sval;
{
  TreeNode 	result;

  result = makeTreeNode(constExprType);
  result->vConst.constType = stringConst;
  result->vConst.val.sVal = sval;

  return (result);
}

TreeNode makeSymbolConstant(symbolNode)
TreeNode symbolNode;
{
  TreeNode 	result;

  result = makeTreeNode(constExprType);
  result->vConst.constType = symbolConst;
  if (symbolNode) {
    result->vConst.val.symVal = symbolNode->vExpr.selector;
    freeNode(symbolNode);
  } else {
    result->vConst.val.symVal = nilOOP;
  }

  return (result);
}

TreeNode makeArrayConstant(aval)
TreeNode aval;
{
  TreeNode 	result;

  result = makeTreeNode(constExprType);
  result->vConst.constType = arrayConst;
  result->vConst.val.aVal = aval;

  return (result);
}


/*
 *	void addNode(n1, n2)
 *
 * Description
 *
 *	adds node "n2" onto a list of nodes headed by "n1".  "n1" contains
 *	the address of the last "next" field in the chain, so storing "n2" into
 *	there indirectly and then making that "next" field point to "n2"'s
 *	"next" field works properly.
 *
 * Inputs
 *
 *	n1    : head of list of nodes, of type listNode.
 *	n2    : node to be added, of type listNode.
 *
 */
void addNode(n1, n2)
TreeNode n1, n2;
{
  *(n1->vList.nextAddr) = n2;
  n1->vList.nextAddr = n2->vList.nextAddr; /* since they're all created this
					    * way anyway, we might as well
					    * use it to our advantage */
}

void freeTree(tree)
TreeNode tree;
{
  if (tree == nil) {
    return;
  }

  switch (tree->nodeType) {
  case methodNodeType:
  case blockNodeType:
    freeMethodNode(tree);
    break;
    
  case symbolNodeType:
  case unaryExprType:
  case binaryExprType:
  case keywordExprType:
  case cascadedMessageNodeType:
  case statementListType:
  case returnExprType:
  case assignExprType:
    freeExprNode(tree);
    break;
    
  case variableNodeType:
  case keywordListType:
  case variableListType:
  case arrayEltListType:
  case messageListType:
    freeListNode(tree);
    break;

  case constExprType:
    freeConstNode(tree);
    break;

  }
}



/***********************************************************************
 *
 * Internal tree construction routines.
 *
 ***********************************************************************/


static TreeNode makeMethodNode(nodeType, selectorExpr, temporaries,
			       primitiveIndex, statements)
NodeType nodeType;
TreeNode selectorExpr, temporaries, statements;
int	primitiveIndex;
{
  TreeNode	result;

  result = makeTreeNode(nodeType);
  result->vMethod.selectorExpr = selectorExpr;
  result->vMethod.temporaries = temporaries;
  result->vMethod.primitiveIndex = primitiveIndex;
  result->vMethod.statements = statements;
  return (result);
}

static TreeNode makeListNode(nodeType, name, value)
NodeType nodeType;
char	*name;
TreeNode value;
{
  TreeNode result;

  result = makeTreeNode(nodeType);
  result->vList.name = name;
  result->vList.value = value;
  result->vList.next = nil;
  result->vList.nextAddr = &result->vList.next;
  return (result);
}

static TreeNode makeExprNode(nodeType, receiver, selector, expression)
NodeType nodeType;
TreeNode receiver, expression;
OOP	selector;
{
  TreeNode result;

  result = makeTreeNode(nodeType);
  result->vExpr.receiver = receiver;
  result->vExpr.selector = selector;
  result->vExpr.expression = expression;
  return (result);
}

static TreeNode makeTreeNode(nodeType)
NodeType nodeType;
{
  TreeNode result;

  result = (TreeNode)malloc(sizeof(struct TreeNodeStruct));
  result->nodeType = nodeType;
  return (result);
}

/* ### these should probably be moved over into the symbol table module, yes?*/

static OOP makeUnarySelector(name)
char	*name;
{
  return (internString(name));
}

static OOP makeBinarySelector(binaryOp)
char	*binaryOp;
{
  return (internString(binaryOp));
}

/***********************************************************************
 *
 * Internal tree destruction routines.
 *
 ***********************************************************************/

static void freeMethodNode(node)
TreeNode node;
{
  freeTree(node->vMethod.selectorExpr);
  freeTree(node->vMethod.temporaries);
  freeTree(node->vMethod.statements);
  freeNode(node);
}

static void freeExprNode(node)
TreeNode node;
{
  freeTree(node->vExpr.receiver);
  freeTree(node->vExpr.expression);
  freeNode(node);
}

static void freeListNode(node)
TreeNode node;
{
  freeTree(node->vList.value);
  freeTree(node->vList.next);
  if (node->vList.name) {
    free(node->vList.name);
  }

  freeNode(node);
}

static void freeConstNode(node)
TreeNode node;
{
  switch (node->vConst.constType) {
  case intConst:
  case floatConst:
  case charConst:
  case symbolConst:
    /* these have no storage of their own */
    break;

  case stringConst:
    if (node->vConst.val.sVal) {
      free(node->vConst.val.sVal);
    } else {
      errorf("Internal error: badly formed tree for string constant"); 
    }
    break;

  case arrayConst:
    freeTree(node->vConst.val.aVal);
    break;

  default:
    errorf("Internal error: corrupted tree structure"); 
  }

  freeNode(node);
}


static void freeNode(node)
TreeNode node;
{
  free(node);
}


/***********************************************************************
 *
 *	Printing routines.
 *
 ***********************************************************************/


void printTree(node, level)
TreeNode node;
int level;
{
  if (node == nil) {
    indent(level);
    printf("%s\n", nilName);
    return;
  }

  switch (node->nodeType) {
  case methodNodeType:
  case blockNodeType:
    printMethodNode(node, level);
    break;
    
  case symbolNodeType:
  case unaryExprType:
  case binaryExprType:
  case keywordExprType:
  case cascadedMessageNodeType:
  case statementListType:
  case returnExprType:
  case assignExprType:
    printExprNode(node, level);
    break;
    
  case variableNodeType:
  case keywordListType:
  case variableListType:
  case arrayEltListType:
  case messageListType:
    printListNode(node, level);
    break;

  case constExprType:
    printConstNode(node, level);
    break;

  default:
    errorf("Unknown tree note type %d\n", node->nodeType);
  }
}


static void printListNode(node, level)
TreeNode node;
int level;
{
  printNodeType(node, level);
  indent(level+1);
  printf("name: %s\n", node->vList.name ? node->vList.name : nilName);
  indent(level+1);
  printf("value:\n");
  printTree(node->vList.value, level+2);
  indent(level+1);
  printf("next:\n");
  printTree(node->vList.next, level);
}

static void printExprNode(node, level)
TreeNode node;
int level;
{
  printNodeType(node, level);
  indent(level+1);
  printf("selector: ");
  if (!isNil(node->vExpr.selector)) {
    printSelector(node->vExpr.selector);
  } else {
    printf("%s", nilName);
  }
  printf("\n");
    
  indent(level+1);
  printf("receiver:\n");
  printTree(node->vExpr.receiver, level+2);
  /* ??? don't print the expression for unary type things, and don't print
     the receiver for symbol nodes */
  indent(level+1);
  printf("expression:\n");
  printTree(node->vExpr.expression, level+2);
}

static void printMethodNode(node, level)
TreeNode node;
int level;
{
  printNodeType(node, level);
  indent(level+1);
  printf("selectorExpr: ");
  printTree(node->vMethod.selectorExpr, level+2);
  indent(level+1);
  /* ??? don't print the temporaries label if there are no temporaries */
  printf("temporaries:\n");
  printTree(node->vMethod.temporaries, level+2);
  indent(level+1);
  printf("statements:\n");
  printTree(node->vMethod.statements, level+2);
}

static void printConstNode(node, level)
TreeNode node;
int level;
{
  indent(level);
  switch (node->vConst.constType) {
  case intConst:
    printf("int: %ld\n", node->vConst.val.iVal);
    break;

  case floatConst:
    printf("float: %g\n", node->vConst.val.fVal);
    break;

  case charConst:
    printf("char: %c\n", node->vConst.val.cVal);
    break;

  case stringConst:
    printf("string: \"%s\"\n", node->vConst.val.sVal);
    break;

  case symbolConst:
    printf("symbol: ");
    printSymbol(node->vConst.val.symVal);
    printf("\n");
    break;

  case arrayConst:
    printf("array:\n");
    printTree(node->vConst.val.aVal, level+1);
    break;
    
  default:
    errorf("Unknown constant type %d", node->vConst.constType);
  }
}


static void printNodeType(node, level)
TreeNode node;
int level;
{
  indent(level);
  printf("%s\n", nodeTypeNames[ENUM_INT(node->nodeType)]);
}

/*
 *	static void indent(level)
 *
 * Description
 *
 *	Indent the output by level*2 spaces.
 *
 * Inputs
 *
 *	level : Indentation level.  C integer.
 *
 */
static void indent(level)
int level;
{
  for (; level > 0; level--) {
    printf("  ");
  }
}

static void printSelector(selector)
OOP	selector;
{
  printSymbol(selector);
}

