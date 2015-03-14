/*
  This file is part of the NetFax system.

  (c) Copyright 1989 by David M. Siegel and Sundar Narasimhan.
      All rights reserved.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation.

    This program is distributed in the hope that it will be useful, 
    but WITHOUT ANY WARRANTY; without even the implied warranty of 
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#include <stdio.h>
#include <rpc/rpc.h>

#include "alloc.h"
#include "bool.h"
#include "list.h"

static int
default_cmp(v1, v2)
int v1, v2;
{
    return (v1 - v2);
}

int
list_init(list, cmp_func, free_func)
LIST *list;
int (*cmp_func)();
int (*free_func)();
{
    list->count = 0;
    list->cmp_func = (cmp_func == NULL) ? default_cmp : cmp_func;
    list->free_func = free_func;

    return (0);
}

LIST *
list_make(cmp_func, free_func)
int (*cmp_func)();
int (*free_func)();
{
    LIST *list;

    if ((list = salloc(1, LIST)) == NULL)
      return (NULL);

    if (list_init(list, cmp_func, free_func) < 0) {
	free(list);
	return (NULL);
    }

    return (list);
}

static NODE *
node_make(value)
char *value;
{
    NODE *node;

    if ((node = salloc(1, NODE)) == NULL)
      return (NULL);

    node->value = value;

    return (node);
}

int
list_free(list)
LIST *list;
{
    NODE *n = list->head;
    NODE *hold;

    while (n) {
	if (list->free_func != NULL) 
	  (*list->free_func)(n->value);
	hold = n->next;
	cfree(n);
	n = hold;
    }

    list->head = NULL;
    list->tail = NULL;
    list->count = 0;
}

int
list_length(list)
LIST *list;
{
    return (list->count);
}

NODE *
list_nth_node(list, n)
LIST *list;
int n;
{
    NODE *node;
    int i;

    if (n >= list->count)
      return (NULL);

    for (i = 0, node = list->head; i < n; i++)
      node = node->next;

    return (node);
}

char *
list_nth(list, n)
LIST *list;
int n;
{
    NODE *node;
    int i;

    if (n >= list->count)
      return (NULL);

    for (i = 0, node = list->head; i < n; i++)
      node = node->next;

    return (node->value);
}

NODE *
list_last_node(list)
LIST *list;
{
    if (list->count == 0)
      return (NULL);

    return (list->tail);
}

char *
list_last(list)
LIST *list;
{
    if (list->count == 0)
      return (NULL);

    return (list->tail->value);
}

NODE *
list_first_node(list)
LIST *list;
{
    if (list->count == 0)
      return (NULL);

    return (list->head);
}

char *
list_first(list)
LIST *list;
{
    if (list->count == 0)
      return (NULL);

    return (list->head->value);
}

static LIST *
add_node(list, prev, new)
LIST *list;
NODE *prev;
NODE *new;
{
    if (prev == NULL) {
	new->next = list->head;
	new->previous = NULL;
	if (list->head) {
	    list->head->previous = new;
	    list->head = new;
	} else {
	    list->tail = new;
	    list->head = new;
	}
    } else {
	new->previous = prev;
	new->next = prev->next;
	if (prev->next)
	  prev->next->previous = new;
	else
	  list->tail = new;
	prev->next = new;
    }

    list->count++;

    return (list);
}

LIST *
list_add_node(list, node)
LIST *list;
NODE *node;
{
    return (add_node(list, list->tail, node));
}

LIST *
list_add(list, value)
LIST *list;
char *value;
{
    return (add_node(list, list->tail, node_make(value)));
}

LIST *
list_add_first_node(list, node)
LIST *list;
NODE *node;
{
    return (add_node(list, NULL, node));
}

LIST *
list_add_first(list, value)
LIST *list;
char *value;
{
    return (add_node(list, NULL, node_make(value)));
}

LIST *
list_append(list1, list2)
LIST *list1;
LIST *list2;
{
    NODE *n;

    for (n = list2->head; n != NULL; n = n->next)
      list_add(list1, n->value);

    return (list1);
}

static NODE *
delete_node(list, n, do_free)
LIST *list;
NODE *n;
int do_free;
{
    NODE *next = n->next;
    
    if (n->previous)
      n->previous->next = n->next;
    if (n->next)
      n->next->previous = n->previous;

    if (n == list->head)
      list->head = n->next;
    if (n == list->tail)
      list->tail = n->previous;
    
    list->count--;
    
    if (do_free && (list->free_func != NULL))
      (*list->free_func)(n->value);

    if (do_free)
      cfree(n);
    
    return (next);
}

static LIST *
delete_list_internal(list, value, func, filter_mode, do_free)
LIST *list;
char *value;
int (*func)();
int filter_mode;
int do_free;
{
    NODE *n = list->head;
    int do_delete = FALSE;

    while(n) {
	if (func != NULL)
	  do_delete = ((*func)(n->value, value) == 0);
	else
	  do_delete = ((*list->cmp_func)(n->value, value) == 0);
	 
	if (do_delete) {
	    n = delete_node(list, n, do_free);
	    if (!filter_mode)
	      return (list);
	} else
	  n = n->next;
    }

    return (list);
}

LIST *
list_delete_node(list, node)
LIST *list;
NODE *node;
{
    delete_node(list, node, FALSE);
    
    return (list);
}

LIST *
list_delete(list, value)
LIST *list;
char *value;
{
    return (delete_list_internal(list, value, NULL, FALSE, TRUE));
}

LIST *
list_delete_with_function(list, value, function)
LIST *list;
char *value;
int (*function)();
{
    return (delete_list_internal(list, value, function, FALSE, TRUE));
}

LIST *
list_filter(list, value)
LIST *list;
char *value;
{
    return (delete_list_internal(list, value, NULL, TRUE, TRUE));
}

LIST *
list_filter_with_function(list, value, function)
LIST *list;
char *value;
int (*function)();
{
    return (delete_list_internal(list, value, function, TRUE, TRUE));
}

LIST *
list_delete_first(list)
LIST *list;
{
    NODE *first = list->head;

    if (first)
      delete_node(list, first, TRUE);
    
    return (list);
}

LIST *
list_delete_last(list)
LIST *list;
{
    NODE *last = list->tail;

    if (last)
      delete_node(list, last, TRUE);
    
    return (list);
}

char *
list_car(list)
LIST *list;
{
    return (list_first(list));
}

LIST *
list_cdr(list)
LIST *list;
{
    return (list_delete_first(list));
}

LIST *
list_push(list, value)
LIST *list;
char *value;
{
    return (list_add_first(list, value));
}

LIST *
list_push_new(list, value)
LIST *list;
char *value;
{
    NODE *n;

    for (n = list->head; n != NULL; n = n->next)
      if ((*list->cmp_func)(n->value, value) == 0)
	return (list);
    
    return (list_add_first(list, value));
}

char *
list_pop(list)
LIST *list;
{
    char *value = list_first(list);
    
    list_delete_first(list);
    return (value);
}

char *
list_find(list, value)
LIST *list;
char *value;
{
    NODE *n;

    for (n = list->head; n != NULL; n = n->next)
      if ((*list->cmp_func)(n->value, value) == 0)
	return (n->value);

    return (NULL);
}

LIST *
list_insert(list, value, order)
LIST *list;
char *value;
int order;
{
    NODE *n, *prev_n;
    int new_order;

    for (n = list->head, prev_n = NULL; n != NULL; prev_n = n, n = n->next) {
	new_order = (*list->cmp_func)(value, n->value);
	if (new_order < 0)
	  new_order = -1;
	else if (new_order > 0)
	  new_order = 1;
	else
	  new_order = 0;
	
	if (order != new_order)
	  return (add_node(list, prev_n, node_make(value)));
    }

    return (add_node(list, prev_n, node_make(value)));
}

NODE *
list_next_node(list, node)
LIST *list;
NODE **node;
{
    if (*node == NULL)
      *node = list->head;
    else
      *node = (*node)->next;
    
    return (*node);
}

char *
list_next(list, node)
LIST *list;
NODE **node;
{
    if (*node == NULL)
      *node = list->head;
    else
      *node = (*node)->next;
    
    if (*node == NULL)
      return (NULL);
    else
      return ((*node)->value);
}

char *
list_delete_next(list, node)
LIST *list;
NODE **node;
{
    if (*node == NULL) {
	*node = list->head;
	return ((*node)->value);
    } else
      *node = delete_node(list, *node, TRUE);

    if (*node != NULL)
      return ((*node)->value);
    else
      return (NULL);
}

/*VARARGS*/
int
list_map(list, function, arg1, arg2, arg3, arg4, arg5, arg6, arg7)
LIST *list;
int (*function)();
char *arg1, *arg2, *arg3, *arg4, *arg5, *arg6, *arg7;
{
    NODE *n;
    int rc;

    for (n = list->head; n != NULL; n = n->next)
      /*SUPPRESS 62*/
      if ((rc = (*function)(n->value,arg1,arg2,arg3,arg4,arg5,arg6,arg7)) != 0)
	return (rc);

    return(0);
}

/*VARARGS*/
int
list_map_node(list, function, arg1, arg2, arg3, arg4, arg5, arg6, arg7)
LIST *list;
int (*function)();
char *arg1, *arg2, *arg3, *arg4, *arg5, *arg6, *arg7;
{
    NODE *n;
    int rc;

    for (n = list->head; n != NULL; n = n->next)
      /*SUPPRESS 62*/
      if ((rc = (*function)(n,arg1,arg2,arg3,arg4,arg5,arg6,arg7)) != 0)
	return (rc);

    return(0);
}

static int 
node_describe(node, fp, i) 
NODE *node;
FILE *fp; 
int *i; 
{
    fprintf(fp, "Node %d: value=%d, node=0x%x, previous=0x%x, next=0x%x\n",
	    *i, node->value, node, node->previous, node->next); 
    (*i)++;

    return (0);
}

int
list_describe(list, fp)
LIST *list;
FILE *fp;
{
    int i = 0;

    if (fp == NULL)
      fp = stdout;

    fprintf(fp, "length=%d, head=0x%x, tail=0x%x\n",
	    list->count, list->head, list->tail);
    list_map_node(list, node_describe, fp, &i);

    return (0);
}

bool_t
xdr_list(xdrs, listpp, size, xdrel)
XDR *xdrs;
LIST **listpp;
int size;
xdrproc_t xdrel;
{
    bool_t more_data = TRUE;
    NODE *node = NULL;

    switch (xdrs->x_op) {
      case XDR_ENCODE:
	if (*listpp != NULL) {
	    while (list_next(*listpp, &node) != NULL) {
		if (!xdr_bool(xdrs, &more_data))
		  return (FALSE);
		if (!xdr_reference(xdrs, &node->value, size, xdrel))
		  return (FALSE);
	    }
	}
	more_data = FALSE;
	if (!xdr_bool(xdrs, &more_data))
	  return (FALSE);
	return (TRUE);
	
      case XDR_DECODE:
	if (!xdr_bool(xdrs, &more_data))
	  return (FALSE);
	while (more_data) {
	    char *value;

	    if (*listpp == NULL) {
		if ((*listpp = list_make(NULL, NULL)) == NULL)
		  return (FALSE);
	    }
	    value = NULL;
	    if (!xdr_reference(xdrs, &value, size, xdrel))
	      return (FALSE);
	    list_add(*listpp, value);
	    if (!xdr_bool(xdrs, &more_data))
	      return (FALSE);
	}
	return (TRUE);

      case XDR_FREE:
	if (*listpp != NULL) {
	    char *value;
	    
	    while ((value = (char *)list_next(*listpp, &node)) != NULL) {
		if (!xdr_reference(xdrs, &value, size, xdrel))
		  return (FALSE);
	    }
	}
	if (*listpp)
	  list_free(*listpp);
	return (TRUE);
    }
    return (FALSE);
}
