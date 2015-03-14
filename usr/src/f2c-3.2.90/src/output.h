/* nice_printf -- same arguments as fprintf.

	All output which is to become C code must be directed through this
   function.  For now, no buffering is done.  Later on, every line of
   output will be filtered to accomodate the style definitions (e.g. one
   statement per line, spaces between function names and argument lists,
   etc.)
*/
#include "niceprintf.h"

extern int nice_printf ();


/* Definitions for the opcode table.  The table is indexed by the macros
   which are #defined in   defines.h   */

#define UNARY_OP 01
#define BINARY_OP 02

#define SPECIAL_FMT NULL

#define is_unary_op(x) (opcode_table[x].type == UNARY_OP)
#define is_binary_op(x) (opcode_table[x].type == BINARY_OP)
#define op_precedence(x) (opcode_table[x].prec)
#define op_format(x) (opcode_table[x].format)

/* _assoc_table -- encodes left-associativity and right-associativity
   information; indexed by precedence level.  Only 2, 3, 14 are
   right-associative.  Source:  Kernighan & Ritchie, p. 49 */

extern char _assoc_table[];

#define is_right_assoc(x) (_assoc_table [x])
#define is_left_assoc(x) (! _assoc_table [x])


typedef struct {
    int type;			/* UNARY_OP or BINARY_OP */
    int prec;			/* Precedence level, useful for adjusting
				   number of parens to insert.  Zero is a
				   special level, and 2, 3, 14 are
				   right-associative */
    char */*short_*/format;
/*    char *long_format;
    char *float_format;
    char *double_format;
    char *complex_format;
    char *dcomplex_format;*/
} table_entry;


extern char *fl_fmt_string;	/* Float constant format string */
extern char *db_fmt_string;	/* Double constant format string */
extern char *cm_fmt_string;	/* Complex constant format string */
extern char *dcm_fmt_string;	/* Double Complex constant format string */

extern int indent;		/* Number of spaces to indent; this is a
				   temporary fix */
extern int tab_size;		/* Number of spaces in each tab */
extern int in_string;

extern table_entry opcode_table[];


void output_expr (), output_init (), output_addr (), output_const ();
void output_name (), output_extern (), output_asgoto ();
void output_if (), output_else (), output_elif ();
void output_endif (), output_endelse ();
void output_comp_goto (), output_for ();
void output_end_for (), output_and_free_statement ();
