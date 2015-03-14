#ifndef CMDH
#define CMDH

/*	Copyright (C) 1992, 1993 Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this software; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */
/*  t. lord	Wed Oct 14 12:01:37 1992	*/

/*
 * This file explains the generic interface to interactive functions.
 * This covers how C functions are made available to the user, how
 * keymaps are structured.  This also describes the variables that
 * hold the user's interaction state .
 */
#include "line.h"


/*
 * A set of 26 string buffers, named `a' - `z', hold the default
 * values for strings read from the user.  For example, both
 * visit-spreadsheet and save-spreadsheet use buffer `f'.  Whatever
 * file name the user answers to visit-spreadsheet becomes the default
 * for save-spreadsheet.
 */
extern struct line in_line[];
#define IN_LINE(c)  (&in_line[(C) - 'a'])


/*
 * These describe C functions that can be called interactively.
 * FUNC_NAME: The name that `execute-function' uses.
 * FUNC_FUNC: The C function the implements the command.
 * FUNC_ARGS: Similar to an emacs interactive string. (see global_cmd)
 *     	This is the list of argument types.  When an argument type is
 *	written with a ? after it, it means that an in_line name
 *	should  replace the ?.  For example, `fa' means read a file
 *	name using IN_LINE('a').
 *
 *	C  - Confirm; ask `are you SURE?' (may preceed other args).
 *	r? - Call the C function with a range argument. (`fn(struct range *)')
 *	R? - Call with two range arguments.
 *	t? - Prompt the user for an arbitrary string.
 *	f?mode - Get a file name and pass the opened FILE * to the
 *	         function. `mode' is as the last argument to fopen.
 *		 The file is closed when the function returns.
 *	F?mode - like f, but passes a range too. (`fn(struct range *, FILE *)')
 *	S? - passes a string and range (`fn(char *, struct range *)')
 *	ndigit - Pass along the digit converted to an integer.
 * 	Tdigit - Pass digit and the last character typed to invoke the command.
 *      N  - pass along the integer argument passed to global_cmd.
 *
 * FUNC_FLAGS: see #defines below
 */
typedef void (*interactive_function) ();

struct cmd_func
{
  char *func_name;
  char *func_args;
  int func_flags;
  interactive_function func_func;
};

/* for func_flags */
#define NONTOP	0x1
#define TOPLN	0x2
#define ALL	0x3
#define WTH_CHR 0x4
#define BRK	0x8
#define NC	0x10

/*
 * There is a 2d (argv style) array of command functions.  Each 0 dimension
 * slice is called a `vector'.  The first two vectors have a hardwired use.
 * Other vectors can be added by reallocing the_funcs and incrementing
 * num_funcs.
 *
 * When looking for a function of a particular name, the array is
 * searched in vector-major order.
 */
extern int num_funcs;
extern struct cmd_func **the_funcs;
#define OLEO_VECTOR (0)		/* all oleo's builtins except: */
#define EDIT_VECTOR (1)		/* input area editting commands */



/*
 * Keymaps and keybinds are next.
 * Within a keymap, keys are bound to a `struct key'.
 *
 * Normally,  `the_funcs[akey.vector][akey.code]' is the command binding of a
 * key.  However, the akey.vector == -1, then the binding is to another
 * keymap, found by the_maps[akey.code].  The final exception, is that if
 * both vector and code are -1, then the key is unbound.
 */
struct key
{
  short vector;
  short code;
};

struct keymap
{
  struct keymap *map_next;
  struct key keys[256];
};
extern int num_maps;
extern struct keymap **the_maps;
extern char **map_names;

#define UNBOUND		0
#ifndef CTRL
#define CTRL(X) ((X)&037)
#endif
#ifndef META
#define META(X) ((X)|0200)
#endif

/*
 * Keymap names.  They can be used as indices into the_maps and map_names.
 */
#define MAIN_MAP map_id("main")
#define EDIT_MAP map_id("edit")
#define DIGIT_MAP map_id("digit")
#define NAVIGATE_MAP map_id("navigate")
extern int map_id ();


struct macro
{
  struct macro *mac_prev;
  unsigned char *mac_exe;
  int mac_flags;
  CELLREF mac_row, mac_col;
  struct rng mac_rng;
};

extern int n_bound_macros;
extern struct obstack macro_stack;
extern struct rng *bound_macros;
extern int bound_macro_vec;
extern unsigned char *making_macro;
extern unsigned char *making_macro_start;
extern unsigned int making_macro_size;


/*
 * The following variables control oleo's decisions about which iteractive
 * functionsto call.
 */

extern struct macro *rmac;		/* Currently executing macro, if any. */
extern char *macro_func_arg;

extern int cur_chr;		/* Last character typed by the user. */

extern short cur_vector;	/* Currently selected function vector. */
extern struct cmd_func *cur_cmd;/* Currently executing command. */

/*
 * MAP_CHR takes a keymap number (index into the_maps) as argument,
 * and advances the state of the above variables.  If a macro is executing,
 * it will consume characters from the macro.  Otherwise, characters
 * are read from the keyboard.  MAP_CHR does not execute the newly
 * selected command (in fact, if an unbound key is read, cur_cmd will be set
 * to 0).
 */

#ifdef __STDC__
extern void map_char (int);
#else
extern void map_char ();
#endif


/*
 * GLOBAL_CMD executes cur_cmd, providing arguments in the manner
 * described above (see FUNC_ARGS).
 *
 * Return values:
 *	-3	break key!
 *	-2	c is inappropriate
 *	-1	C is OK
 *	0+	New char value is ret-1;
 */
extern unsigned int how_many;	/* The numeric prefix argument. */
#endif
