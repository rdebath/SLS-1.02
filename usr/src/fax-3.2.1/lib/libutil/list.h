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

#ifndef INClisth
#define INClisth 1

#define LIST_ASCENDING	-1
#define LIST_DESCENDING	 1

typedef struct _node {
    struct _node *next;
    struct _node *previous;
    char *value;
} NODE;

typedef struct _list {
    int count;
    int (*cmp_func)();
    int (*free_func)();
    NODE *head;
    NODE *tail;
} LIST;

#define list_node_value(x) ((x == 0) ? 0 : x->value)

/*
  Prototypes:
*/

int list_init(
#ifdef _PROTO
    LIST *list,
    int (*cmp_func)(),
    int (*free_func)()
#endif
);

LIST *list_make(
#ifdef _PROTO
    int (*cmp_func)(),
    int (*free_func)()
#endif
);

int list_free(
#ifdef _PROTO
    LIST *list
#endif
);

int list_length(
#ifdef _PROTO
    LIST *list
#endif
);

NODE *list_nth_node(
#ifdef _PROTO
    LIST *list,
    int n
#endif
);

char *list_nth(
#ifdef _PROTO
    LIST *list,
    int n
#endif
);

NODE *list_last_node(
#ifdef _PROTO
    LIST *list
#endif
);

char *list_last(
#ifdef _PROTO
    LIST *list
#endif
);

NODE *list_first_node(
#ifdef _PROTO
    LIST *list
#endif
);

char *list_first(
#ifdef _PROTO
    LIST *list
#endif
);

LIST *list_add_node(
#ifdef _PROTO
    LIST *list,
    NODE *node
#endif
);

LIST *list_add(
#ifdef _PROTO
    LIST *list,
    char *value
#endif
);

LIST *list_add_first_node(
#ifdef _PROTO
    LIST *list,
    NODE *node
#endif
);

LIST *list_add_first(
#ifdef _PROTO
    LIST *list,
    char *value
#endif
);

LIST *list_append(
#ifdef _PROTO
    LIST *list1,
    LIST *list2
#endif
);

LIST *list_delete_node(
#ifdef _PROTO
    LIST *list,
    NODE *node
#endif
);

LIST *list_delete(
#ifdef _PROTO
    LIST *list,
    char *value
#endif
);

LIST *list_delete_with_function(
#ifdef _PROTO
    LIST *list,
    char *value,
    int (*function)()
#endif
);

LIST *list_filter(
#ifdef _PROTO
    LIST *list,
    char *value
#endif
);

LIST *list_filter_with_function(
#ifdef _PROTO
    LIST *list,
    char *value,
    int (*function)()
#endif
);

LIST *list_delete_first(
#ifdef _PROTO
    LIST *list
#endif
);

LIST *list_delete_last(
#ifdef _PROTO
    LIST *list
#endif
);

char *list_car(
#ifdef _PROTO
    LIST *list
#endif
);

LIST *list_cdr(
#ifdef _PROTO
    LIST *list
#endif
);

LIST *list_push(
#ifdef _PROTO
    LIST *list,
    char *value
#endif
);

LIST *list_push_new(
#ifdef _PROTO
    LIST *list,
    char *value
#endif
);

char *list_pop(
#ifdef _PROTO
    LIST *list
#endif
);

char *list_find(
#ifdef _PROTO
    LIST *list,
    char *value
#endif
);

LIST *list_insert(
#ifdef _PROTO
    LIST *list,
    char *value,
    int order
#endif
);

NODE *list_next_node(
#ifdef _PROTO
    LIST *list,
    NODE **node
#endif
);

char *list_next(
#ifdef _PROTO
    LIST *list,
    NODE **node
#endif
);

char *list_delete_next(
#ifdef _PROTO
    LIST *list,
    NODE **node
#endif
);

/*VARARGS*/
int list_map();

/*VARARGS*/
int list_map_node();

int list_describe(
#ifdef _PROTO
    LIST *list,
    FILE *fp
#endif
);

#ifdef __RPC_HEADER__
bool_t xdr_list(
#ifdef _PROTO
    XDR *xdrs,
    LIST **listpp,
    int size,
    xdrproc_t xdrel
#endif
);
#endif

#endif
