/*
 *      (c) Copyright 1989 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ident	"@(#)list.c	26.7	91/09/14 SMI"

#include <stdio.h>
#include "mem.h"
#include "list.h"

/***************************************************************************
* Local data
***************************************************************************/

#define NUMCELLS 511

typedef struct _cellBlock {
	struct _cellBlock *next;
	int n;
	List cells[NUMCELLS];
	} cellBlock, *pcellBlock;

static pcellBlock cellList = NULL;
static List *freeList = NULL;

/***************************************************************************
* Local functions
***************************************************************************/

/* local function
 *
 * pcellBlock mkCellBlock()
 * returns a new, minimally initialised cell block.  The block is not linked 
 * into the list of cell blocks.  The cells are not initialised.
 */
static pcellBlock
mkCellBlock()
{
	pcellBlock pcb;
	pcb =  MemNew(cellBlock);
	pcb->n = NUMCELLS;
	pcb->next = NULL;
	return pcb;
}


/* local function
 *
 * void initCellBlock()
 * puts all the cells in the new cell block onto the free list
 */
static void
initCellBlock(pcb)
pcellBlock pcb;
{
	int i;

	for (i=0; i<pcb->n; i++)
	{
		pcb->cells[i].next = freeList;
		pcb->cells[i].value = NULL;
		freeList = &(pcb->cells[i]);
	}
}

/* local function
 *
 * List *allocCell()
 * allocates a cell from the free list; if none are available, allocates
 * a new cell block.
 */
static List *
allocCell()
{
	pcellBlock pcb;
	List *cell;

	if (freeList == NULL)
	{
		pcb = mkCellBlock();
		initCellBlock(pcb);
		pcb->next = cellList;
		cellList = pcb;
	}
	cell = freeList;
	freeList = freeList->next;
	cell->next = NULL;
	return cell;
}

/* local function
 *
 * void freeCell()
 * returns a cell from the free list
 */
static void
freeCell(cell)
List *cell;
{
	cell->next = freeList;
	cell->value = NULL;
	freeList = cell;
}


#ifdef DEBUG

/* debugging function listPrint
 *
 * Intended to be called from a debugger.  Prints out the value of each list
 * item in hex.
 */
static void
listPrint(list)
    List *list;
{
    int count;

    for (count=0 ; list != NULL_LIST ; list = list->next, ++count)
	fprintf(stderr, "item %d: value = 0x%x\n", count, list->value);
}

#endif /* DEBUG */

/***************************************************************************
* Global functions
***************************************************************************/

/* global function
 *
 * void ListInit()
 * no parameters.
 *
 * Initialises the List package:  allocates a block of cells.
 */
void 
ListInit()
{
	cellList = mkCellBlock();
	initCellBlock(cellList);
}

/* global function
 *
 * void ListCons(void *val, List *next)
 *
 * Prepends a value to the head of a list.
 */
List *
ListCons(val,next)
void *val;
List *next;
{
	List *cell;

	cell = allocCell();
	cell->value = val;
	cell->next = next;
	return cell;
}

/* global function
 *
 * int ListCount(List *l)
 *
 * Returns the number of items in a list.
 */
int 
ListCount(l)
List *l;
{
	int len = 0;

	while (l != NULL)
	{
		len++;
		l = l->next;
	}
	return len;
}


/* global function 
 * void ListDestroy(List *l)
 *
 * destroys all the cells in a list.  Leaves the values unaffected.
 */
void 
ListDestroy(l)
List *l;
{
	List *last;

	while (l != NULL)
	{
		last = l;
		l = l->next;
		freeCell(last);
	}
}


/* global function 
 * void ListDestroyCell(List **l)
 *
 * deletes one cell from a list; modifies the List* passed by
 * reference to point to the next cell.
 */
void 
ListDestroyCell(l)
List **l;
{
	List *cell;

	if ((cell = *l) != NULL)
	{
		*l = cell->next;
		freeCell(cell);
	}
}

/* global function 
 * void ListApply(List *l,void *(*f)(),void*c)
 *
 * iteratively calls the function f with list values and the
 * closure c, until f returns a non-NULL value 
 */
void *
ListApply(l,f,c)
List *l; 
void *(*f)();
void *c;
{
	List *cell;
	void *res;

	for (cell = l; cell != NULL; cell = cell->next)
	{
		res = (*f)(cell->value,c);
		if (res != NULL)
			return res;
	}
	return NULL;
}

/* global function
 * void * ListEnum(List **l)
 *
 * returns the value of the cell pointed to by l; modifies l to point
 * to the next cell in the list.
 */
void *
ListEnum(l)
List **l;
{
	void *val;

	if (*l != NULL)
	{
		val = (*l)->value;
		*l = (*l)->next;
		return val;
	}
	else
		return NULL;
}
