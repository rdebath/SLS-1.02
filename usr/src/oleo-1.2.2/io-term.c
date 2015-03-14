/*	Copyright (C) 1990, 1992, 1993 Free Software Foundation, Inc.

This file is part of Oleo, the GNU Spreadsheet.

Oleo is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Oleo is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Oleo; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <signal.h>
#include <sys/types.h>
#ifdef I_IOCTL
#include <sys/ioctl.h>
#endif

#include "getopt.h"
#include "funcdef.h"
#define obstack_chunk_alloc ck_malloc
#define obstack_chunk_free free
#include "obstack.h"
#include "sysdef.h"
#include "global.h"
#include "cell.h"
#include "cmd.h"
#include "init.h"
#include "utils.h"
#include "regions.h"
#include "ref.h"
#include "lists.h"
#include "line.h"
#define DEFINE_IO_VARS 1
#include "io-abstract.h"
#include "io-generic.h"
#include "io-utils.h"
#include "io-edit.h"
#include "io-term.h"
#include "io-x11.h"
#include "io-curses.h"
#include "window.h"
#include "sylk.h"
#include "font.h"
#include "print.h"

/* External spreadsheet functions */
#ifdef USE_DLD
/* If we're using dynamic linking, we get the names of the
	   functions to call by prepending the basename of save_name onto
		   _read_file
		   _write_file
		   _set_options
		   _show_options
	   so, if the file is sylk.o , the functions are named
		   sylk_read_file
		   sylk_write_file
		   sylk_set_options
		   sylk_show_options
		   */
char *io_name;
#else
#include "sylk.h"
#include "oleofile.h"
#include "sc.h"
#include "list.h"
#endif

const char oleo_version_string[] = "Oleo version 1.2.2";


#ifdef __STDC__
static void got_sig (int);
/* Routines for manipulating key bindings */
static void do_bind_key (struct keymap *, int, int, int);
static void desc_map (struct keymap *);

/* File I/O functions */
static FILE *open_file (char *, struct line *, char *);
static void close_file (struct line *, FILE *);

/* Routines for putting info into 'struct rng's */
static int get_two_ranges (char *, struct rng *, struct rng *, struct line *);
static FILE *get_range_and_file (char *, struct rng *, struct line *);
static char *get_range_and_font (char *, struct rng *, struct line *);
static int get_a_range (char *, struct rng *, struct line *);
static int get_abs_rng (char **, struct rng *);

/* Routines for converting text<-->cell_format */
static int str_to_fmt (char *);

/* Routines for converting text<-->cell_jst */
static int chr_to_jst (int);

/* Routines for implementing user commands */
void play_cell (void);

/* Backends for other functions */
static int do_set_option (char *);


/* Spreadsheet (global) functions declared in this file */


int main (int, char **);
void map_chr (int);
extern void clear_spreadsheet (void);
char *cell_name (CELLREF, CELLREF);
char *range_name (struct rng *);
char *fmt_to_str (int);
char *jst_to_str (int);
char *col_to_str (CELLREF);
/* The user commands that are defined in this file. . . */
extern void copy_region (void);
extern void copy_values_region (void);
extern void set_format (void);
extern void move_region (struct rng *, struct rng *);
extern void delete_region (void);
extern void insert_row (void);
extern void insert_col (void);
extern void delete_row (void);
extern void delete_col (void);
extern void format_area (struct rng *);
extern void open_window (char *);
extern void close_window (char *);
extern void hsplit_window (void);
extern void vsplit_window (void);
extern void delete_window (void);
extern void delete_other_windows (void);
extern void goto_window (char *);
extern void other_window (void);
extern void print_region (struct rng *, char *);
extern void psprint_region_cmd (FILE *, struct rng *);

static void end_macro (void);
static void do_break_cmd (void);
static void set_default (void);
static void quit_cmd (void);
static void set_usr_fmt (char *);
static void set_var (char *);
static void show_var (char *);
static void show_all_var (void);
static void write_variables (FILE *);
static void read_variables (FILE *);
static void recalc_cmd (void);
static void bind_key_cmd (char *);
static void desc_key_cmd (void);
static void read_cmds_cmd (FILE *);
static void write_keys_cmd (FILE *);
static void write_cmd (FILE *);
static void read_cmd (FILE *);
static void read_merge_cmd (FILE *);
static void kill_all_cmd (void);
static void sort_region_cmd (char *);
static void write_reg_cmd (FILE *, struct rng *);
static void start_macro (void);
void execute_cmd (char *);
static void interact_macro_cmd (void);
static void goto_region (struct rng *);
static void upper_left (void);
static void lower_left (void);
static void upper_right (void);
static void lower_right (void);
extern void exchange_point_and_mark (void);
static void mark_cell_cmd (void);
static void unmark_cmd (void);
static void do_input_cmd (int, int);
static void set_region_formula (char *, struct rng *);
static void set_cell_formula (char *, CELLREF, CELLREF);
static void format_cell_cmd (void);
static void kill_cell_cmd (void);
static void digit_cmd (int);
static void show_options (void);
static void bound_macro (int);
static void make_keymap (char *);
static void set_cell_font_cmd (char *);
static void set_area_font_cmd (char *, struct rng *);
static void scan_cell_cursor (int);
static void shift_cell_cursor (int);
static void scroll_cell_cursor (int);
static void recenter_cur_win (void);
static void repaint (void);
static void mouse_goto_cmd (void);
static void mouse_mark_cmd (void);
static void mouse_mark_and_goto_cmd (void);
static void set_page_size_cmd (char *);
void toggle_load_hooks (void);
void read_file_and_run_hooks (FILE *, int);
#else
static void got_sig ();
/* Routines for manipulating key bindings */
static void do_bind_key ();
static void desc_map ();

/* File I/O functions */
static FILE *open_file ();
static void close_file ();

/* Routines for putting info into 'struct rng's */
static int get_two_ranges ();
static FILE *get_range_and_file ();
static char *get_range_and_font ();
static int get_a_range ();
static int get_abs_rng ();

/* Routines for converting text<-->cell_format */
static int str_to_fmt ();

/* Routines for converting text<-->cell_jst */
static int chr_to_jst ();

/* Routines for implementing user commands */
void play_cell ();

/* Backends for other functions */
static int do_set_option ();


/* Spreadsheet (global) functions declared in this file */


int main ();
extern void clear_spreadsheet ();
char *cell_name ();
char *range_name ();
char *fmt_to_str ();
char *jst_to_str ();
char *col_to_str ();
/* The user commands that are defined in this file. . . */
extern void copy_region ();
extern void copy_values_region ();
extern void set_format ();
extern void move_region ();
extern void delete_region ();
extern void insert_row ();
extern void insert_col ();
extern void delete_row ();
extern void delete_col ();
extern void format_area ();
extern void open_window ();
extern void hsplit_window ();
extern void vsplit_window ();
extern void delete_window ();
extern void delete_other_windows ();
extern void close_window ();
extern void goto_window ();
extern void other_window ();
extern void print_region ();
extern void psprint_region_cmd ();

static void do_debug ();
static void end_macro ();
static void do_break_cmd ();
static void set_default ();
static void quit_cmd ();
static void set_usr_fmt ();
static void set_var ();
static void show_var ();
static void show_all_var ();
static void write_variables ();
static void read_variables ();
static void recalc_cmd ();
static void bind_key_cmd ();
static void desc_key_cmd ();
static void where_is_cmd ();
static void read_cmds_cmd ();
static void write_keys_cmd ();
static void write_cmd ();
static void read_cmd ();
static void read_merge_cmd ();
static void kill_all_cmd ();
static void sort_region_cmd ();
static void write_reg_cmd ();
static void start_macro ();
void execute_cmd ();
static void interact_macro_cmd ();
static void goto_region ();
static void upper_left ();
static void lower_left ();
static void upper_right ();
static void lower_right ();
extern void exchange_point_and_mark ();
static void mark_cell_cmd ();
static void unmark_cmd ();
static void do_input_cmd ();
static void set_region_formula ();
static void set_cell_formula ();
static void format_cell_cmd ();
static void kill_cell_cmd ();
static void digit_cmd ();
static void show_options ();
static void bound_macro ();
static void make_keymap ();
static void set_cell_font_cmd ();
static void set_area_font_cmd ();
static void scan_cell_cursor ();
static void shift_cell_cursor ();
static void scroll_cell_cursor ();
static void recenter_cur_win ();
static void repaint ();
static void mouse_goto_cmd ();
static void mouse_mark_cmd ();
static void mouse_mark_and_goto_cmd ();
static void set_page_size_cmd ();
void toggle_load_hooks ();
void read_file_and_run_hooks ();


#endif


char *ename[] =
{
  "#WHAT?",
  "#ERROR", "#BAD_INPUT", "#NON_NUMBER", "#NON_STRING",
  "#NON_BOOL", "#NON_RANGE", "#OUT_OF_RANGE", "#NO_VALUES",
  "#DIV_BY_ZERO", "#BAD_NAME", "#NOT_AVAIL", "#PARSE_ERROR",
  "#NEED_OPEN", "#NEED_CLOSE", "#NEED_QUOTE", "#UNK_CHAR",
  "#UNK_FUNC",
  0
};

char tname[] = "#TRUE";
char fname[] = "#FALSE";

struct line val_line;
struct line fmt_line;
struct line wid_line;
struct line hgt_line;
struct line font_line;

/* Lines 'a' through 'z' */
struct line in_line[26];

unsigned short default_width = 8;
unsigned short default_height = 1;
unsigned short saved_default_width = 8;
unsigned short saved_default_height = 1;
int default_jst = JST_LFT;
int default_fmt = FMT_GEN;
int default_lock = LCK_UNL;

int term_flag;

extern char print_buf[];

struct keymap **the_maps;
char **map_names;
int num_maps;

struct cmd_func **the_funcs;
int num_funcs;


/* For lines that take text input, the second letter (eg 'ta' or 'fdr')
   specifies which struct line to use to contain the text generated for that
   command.  That way, the command will have the appropriate defaults, etc.
   Currently 'a' through 'n' are used (?).
	a	set options
	b	set/show variables
	c	read/write keymap
	d	read/write file
	e	read-merge	write-region
	f	copy region/copy values region
	g	format-region
	h	move-region
	i	print-region
	j	sort-region
	k	delete-region
	l	execute-command
	m	goto-cell
	n	open-window
	o	close-window
	p	page size
	q	bind-key
	r	set-user-format
	s	create-keymap
	t	font names
	u	point size
	v	formulas
	w	variable files
	x	command name
 */
static struct cmd_func *end_macro_cmd;
static struct cmd_func *digit_0_cmd;
static struct cmd_func *digit_9_cmd;



static struct cmd_func cmd_funcs[] =
{
  {"unbound", 0, ALL, 0},
/* The location of these motion commands in this array is assumed elsewhere in */
/* the code.  Other motion and scrolling commands are also assumed to be in a */
/* particular order. */
  {"up-cell", "n0", ALL, shift_cell_cursor},
  {"down-cell", "n1", ALL, shift_cell_cursor},
  {"right-cell", "n2", ALL, shift_cell_cursor},
  {"left-cell", "n3", ALL, shift_cell_cursor},
  {"upright-cell", "n4", ALL, shift_cell_cursor},
  {"upleft-cell", "n5", ALL, shift_cell_cursor},
  {"downright-cell", "n6", ALL, shift_cell_cursor},
  {"downleft-cell", "n7", ALL, shift_cell_cursor},

  {"scroll-up", "n0", ALL, scroll_cell_cursor},
  {"scroll-down", "n1", ALL, scroll_cell_cursor},
  {"scroll-right", "n2", ALL, scroll_cell_cursor},
  {"scroll-left", "n3", ALL, scroll_cell_cursor},
  {"scroll-upright", "n4", ALL, scroll_cell_cursor},
  {"scroll-upleft", "n5", ALL, scroll_cell_cursor},
  {"scroll-downright", "n6", ALL, scroll_cell_cursor},
  {"scroll-downleft", "n7", ALL, scroll_cell_cursor},

  {"scan-up", "n0", ALL, scan_cell_cursor},
  {"scan-down", "n1", ALL, scan_cell_cursor},
  {"scan-right", "n2", ALL, scan_cell_cursor},
  {"scan-left", "n3", ALL, scan_cell_cursor},

  {"break", 0, ALL | BRK, do_break_cmd},
  {"recenter-window", 0, ALL, recenter_cur_win},
  {"set-option", "ta", ALL, set_options},
  {"set-defaults", "T", ALL, set_default},
  {"quit", "C", ALL, quit_cmd},
  {"redraw-screen", 0, ALL, repaint},
  {"set-user-format", "trT", ALL, set_usr_fmt},

/* If you change the line used by {set,show}-variable, you must change
   {set,show}_var to use the correct line, else bad things will happen */
  {"set-variable", "tb", ALL, set_var},
  {"show-variable", "tb", ALL, show_var},
  {"show-all-variables", 0, ALL, show_all_var},
  {"write-variables", "fww", ALL, write_variables},
  {"read-variables", "fwr", ALL, read_variables},

  {"recalculate", 0, ALL, recalc_cmd},
  {"bind-key", "tqT", ALL, bind_key_cmd},
  {"describe-key", "T", ALL, desc_key_cmd},
/*  {"where-is", "tx", ALL, where_is_cmd}, */
  {"read-commands", "fcr", ALL, read_cmds_cmd},
  {"write-keys", "fcw", ALL, write_keys_cmd},

/* If you change the line used by {read,write}-file, you must change
   main() to use the correct line, else chaos will erupt */
  {"save-spreadsheet", "fdw", ALL, write_cmd},
  {"visit-spreadsheet", "Cfdr", ALL, read_cmd},
  {"merge-spreadsheet", "fer", ALL, read_merge_cmd},
  {"clear-spreadsheet", "C", ALL, kill_all_cmd},

  {"copy-region", "Rf", ALL, copy_region},
  {"copy-values-in-region", "Rf", ALL, copy_values_region},
  {"format-region", "rg", ALL, format_area},
  {"move-region", "Rh", ALL, move_region},
  {"insert-row", 0, ALL, insert_row },
  {"insert-col", 0, ALL, insert_col },
  {"delete-row", 0, ALL, delete_row },
  {"delete-col", 0, ALL, delete_col },
  {"print-region", "Fi", ALL, print_region},
  {"psprint-region", "Fi", ALL, psprint_region_cmd},
  {"set-page-size", "tp", ALL, set_page_size_cmd },
  {"set-default-ps-font", "tt", ALL, set_ps_font_cmd },
#ifdef HAVE_X11_X_H
  {"set-default-font", "tt", ALL, set_x_default_font },
  {"set-default-point-size", "tu", ALL, set_x_default_point_size },
#endif

  {"sort-region", "tj", ALL, sort_region_cmd},
  {"write-region-to-file", "Fe", ALL, write_reg_cmd},
  {"delete-region", "rk", ALL, delete_region},

  {"start-entering-macro", 0, ALL, start_macro},
  {"execute-command", "tl", ALL, execute_cmd},
  {"macro-interactive-here", 0, ALL, interact_macro_cmd},

/* If you change which line goto-cell uses, you *must* modify goto_region() */
  {"goto-cell", "rm", ALL, goto_region},
  {"upper-left", 0, ALL, upper_left},
  {"lower-left", 0, ALL, lower_left},
  {"upper-right", 0, ALL, upper_right},
  {"lower-right", 0, ALL, lower_right},
  {"exchange-point-and-mark", 0, ALL, exchange_point_and_mark },
  {"mark-cell", 0, ALL, mark_cell_cmd},
  {"clear-mark", 0, ALL, unmark_cmd},
  {"edit-cell", "T0", ALL | WTH_CHR, do_input_cmd},
  {"edit-value-cell", "T1", ALL | WTH_CHR, do_input_cmd},
  {"format-cell", "T", ALL, format_cell_cmd},
  {"delete-cell", 0, ALL, kill_cell_cmd},
  {"edit-cell-with-default", "T3", ALL | WTH_CHR, do_input_cmd},
  {"set-region-formula", "Sv", ALL, set_region_formula},

  {"digit-0", "N", ALL | NC, digit_cmd},
  {"digit-1", "N", ALL | NC, digit_cmd},
  {"digit-2", "N", ALL | NC, digit_cmd},
  {"digit-3", "N", ALL | NC, digit_cmd},
  {"digit-4", "N", ALL | NC, digit_cmd},
  {"digit-5", "N", ALL | NC, digit_cmd},
  {"digit-6", "N", ALL | NC, digit_cmd},
  {"digit-7", "N", ALL | NC, digit_cmd},
  {"digit-8", "N", ALL | NC, digit_cmd},
  {"digit-9", "N", ALL | NC, digit_cmd},

  {"enter-text-in-cell", "T2", WTH_CHR | ALL, do_input_cmd},

  {"show-options", 0, ALL, show_options},
  {"play_cell", 0, ALL, play_cell},
  {"open-window", "tn", ALL, open_window},
  {"split-window-horizontally", 0, ALL, hsplit_window},
  {"split-window-vertically", 0, ALL, vsplit_window},
  {"delete-window", 0, ALL, delete_window},
  {"delete-other-windows", 0, ALL, delete_other_windows},
  {"close-window", "to", ALL, close_window},
  {"goto-window", "to", ALL, goto_window},
  {"other-window", 0, ALL, other_window},

  {"stop-entering-macro", 0, ALL, end_macro},

  {"create-keymap", "ts", ALL, make_keymap},

  {"set-cell-font", "tt", ALL, set_cell_font_cmd},
  {"define-font", "tt", ALL, define_font},
  {"set-region-font", "St", ALL, set_area_font_cmd},
  {"toggle-load-hooks", 0, ALL, toggle_load_hooks },

  {	"mouse-goto",		0,	ALL,	mouse_goto_cmd },
  {	"mouse-mark",		0,	ALL,	mouse_mark_cmd },
  {	"mouse-mark-and-goto",	0,	ALL,	mouse_mark_and_goto_cmd },
  {"bound-menu", 0, WTH_CHR | ALL, 0},

  {0, 0, 0, 0}

};



fd_set read_fd_set;
fd_set read_pending_fd_set;
fd_set exception_fd_set;
fd_set exception_pending_fd_set;

/* Block until we get a signal (unless system calls restart), 
 * can do io or, until we timeout (timeout is specified in seconds,
 * 0 means block indefinately). 
 */
#ifdef __STDC__
static void
block_until_excitment (int timeout_seconds)
#else
static void
block_until_excitment (timeout_seconds)
     int timeout_seconds;
#endif
{
  int ret;
  struct timeval timeout;
  struct timeval * time_p = 0;

  if (timeout_seconds)
    {
      timeout.tv_sec = timeout_seconds;
      timeout.tv_usec = 0;
      time_p = &timeout;
    }
  bcopy ((char *)&read_fd_set, (char *)&read_pending_fd_set, sizeof (fd_set));
  bcopy ((char *)&exception_fd_set,
	 (char *)&exception_pending_fd_set, sizeof (fd_set));
  ret = select (FD_SETSIZE,
		&read_pending_fd_set, 0, &exception_pending_fd_set, time_p);
  if (ret < 0)
    {
      FD_ZERO (&read_pending_fd_set);
      FD_ZERO (&exception_pending_fd_set);
    }
}

/* This is the main interact loop.  As quickly as possible
 * it returns a character from the keyboard.  While waiting,
 * it updates cells and the display.  If a macro is being defined,
 * this function save characters in the macro.
 */

int 
real_get_chr ()
{
  int ch;			/* The char that will be returned. */

  /* Characters with the meta bit set are returned as
   * two characters: ESC and a non-meta character.
   * This buffers the non-meta character between calls.
   * The value is 0 if no character is buffered, C+1 if
   * C is buffered.
   */
  static int saved_char;

  /* A buffer of characters read in one burst from the kbd. */
  static char ibuf[256];
  static int i_in;		/* chars buffered */
  static int i_cnt;		/* buffer position */

  deal_alarm ();
  if (saved_char)
    {
      ch = saved_char - 1;
      saved_char = 0;
      goto fini;
    }

  if (i_cnt)
    {
      ch = ibuf[i_cnt++];
      if (i_cnt == i_in)
	i_cnt = i_in = 0;
      goto fini;
    }

  /* This loop until a character can be read. */
  while (!io_input_avail ())
    {
      deal_alarm ();
      io_scan_for_input (0);
      if (!io_input_avail ())
	{
	  ++current_cycle;
	  if (auto_recalc && eval_next_cell ())
	    {
	      if (bkgrnd_recalc)
		while (!io_input_avail () && eval_next_cell ())
		  io_scan_for_input (0);
	      else
		while (eval_next_cell ())
		  ;
	      io_scan_for_input (0);
	      if (!io_input_avail ())
		io_redisp ();
	      io_flush ();
	      io_scan_for_input (0);
	    }
	  else
	    {
	      int timeout = (alarm_active
			     ? (alarm_seconds == 1
				? 1
				: (alarm_seconds / 2))
			     : 0);
			     
	      --current_cycle;
	      io_redisp ();
	      io_flush ();
	      io_scan_for_input (0);
	      if (!io_input_avail ())
		block_until_excitment (timeout);
	    }
	}
    }

  {
    int ret;
    ret = io_read_kbd (ibuf, sizeof (ibuf));
    if (ret == 1)
      {
	ch = ibuf[0];
	goto fini;
      }
    if (ret > 1)
      {
	i_cnt = 1;
	i_in = ret;
	ch = ibuf[0];
	goto fini;
      }
    if (ret == 0 || (errno != EWOULDBLOCK && errno != EINTR))
      return EOF;
  }

fini:

  if (ch & 0x80)
    {
      saved_char = 1 + (ch & 0x7f);
      ch = CTRL ('[');
    }

  if (making_macro)
    {
      /* This is stoopid and should be fixed.
       * Macros (and other cell strings) should be 
       * `struct line' and not c-strings.   -tl
       */
      if (ch == 0)
	*making_macro++ = 0x80 | 'a';
      else if (ch == '{')
	*making_macro++ = 0x80 | 'b';
      else
	*making_macro++ = ch;
      if (making_macro >= making_macro_start + making_macro_size)
	{
	  making_macro_start =
	    ck_realloc (making_macro_start, 5 + making_macro_size * 2);
	  making_macro = making_macro_start + making_macro_size;
	  making_macro_size *= 2;
	}
    }
  return ch;
}


CELLREF setrow, setcol;

CELLREF curow = MIN_ROW, cucol = MIN_COL;

CELLREF mkrow = NON_ROW, mkcol = NON_COL;
int cur_chr;
struct cmd_func *cur_cmd;
short cur_vector;
unsigned int how_many = 1;
char *macro_func_arg = 0;

int n_bound_macros;
struct rng *bound_macros;
int bound_macro_vec;

/* This variable is non-zero if the spreadsheet has been changed in any way */
int modified = 0;

/* User settable options */
int bkgrnd_recalc = 1;
int auto_recalc = 1;
int a0 = 0;

/* This is how frequently the alarm should go off. */
unsigned int alarm_seconds = 1;
unsigned int alarm_active = 0;	/* Whether or not the alarm matters. */

#ifdef __STDC__
void (*read_file) (FILE *, int) = oleo_read_file;
void (*write_file) (FILE *, struct rng *) = oleo_write_file;
int (*set_file_opts) (int, char *) = oleo_set_options;
void (*show_file_opts) () = oleo_show_options;
#else
void (*read_file) () = oleo_read_file;
void (*write_file) () = oleo_write_file;
int (*set_file_opts) () = oleo_set_options;
void (*show_file_opts) () = oleo_show_options;
#endif

const int colmagic[] =
{0, 0, 1, -1, 1, -1, 1, -1, 0};
const int rowmagic[] =
{-1, 1, 0, 0, -1, -1, 1, 1, 0};


struct macro *rmac = 0;
struct obstack macro_stack;
unsigned char *making_macro;
unsigned char *making_macro_start;
unsigned int making_macro_size;



static int run_load_hooks = 1;
static char load_hooks_string[] = "load_hooks";

void
read_file_and_run_hooks (fp, ismerge)
     FILE * fp;
     int ismerge;
{
  (*read_file)(fp, ismerge);
  if (run_load_hooks)
    {
      struct var * v;
      v = find_var (load_hooks_string, sizeof (load_hooks_string) - 1);
      if (v && v->var_flags != VAR_UNDEF)
	execute_cmd (load_hooks_string);
    }
}

void
toggle_load_hooks ()
{
  if (!how_many && run_load_hooks)
    {
      run_load_hooks = 0;
      io_info_msg ("load hooks turned off");
    }
  else
    {
      run_load_hooks = 1;
      io_info_msg ("load hooks turned on");
    }
}



/* Read a character.  If we're in a macro, read from the macro. . . */
int
get_chr ()
{
  int ch;

  if (rmac)
    {
      ch = *(rmac->mac_exe++);
      switch (ch)
	{
	case '{':		/* What else can we do? */
	case '\0':
	  --(rmac->mac_exe);
	  break;

	case (0x80 | 'a'):
	  ch = 0;
	  break;

	case (0x80 | 'b'):
	  ch = '{';
	  break;
	default:
	  break;
	}
    }
  else
    ch = real_get_chr ();
  return ch;
}

void 
open_window (text)
     char *text;
{
  int hv;
  int where;

  while (*text == ' ')
    text++;

  if (*text == 'h' || *text == 'H')
    hv = 0;
  else if (*text == 'v' || *text == 'V')
    hv = 1;
  else
    {
      io_error_msg ("Open 'h'orizontal or 'v'ertical window, not '%s'", text);
      return;
    }
  where = atoi (text + 1);
  while (isspace (*text))
    ++text;
  while (isalnum (*text))
    ++text;
  while (isspace (*text))
    ++text;
  if (*text == '%')
    {
      where *= (hv
		? (cwin->numr + (cwin->lh_wid ? label_rows : 0))
		: (cwin->numc + cwin->lh_wid));
      where /= 100;
    }
  io_win_open (hv, where);
}

void
hsplit_window ()
{
  open_window ("h50%");
}


void
vsplit_window ()
{
  open_window ("v50%");
}


void 
close_window (text)
     char *text;
{
  int num;

  num = atoi (text) - 1;

  if (num < 0 || num >= nwin)
    {
      io_error_msg ("Window %num?", text);
      return;
    }
  if (nwin == 1)
    {
      io_error_msg ("You can't close the last window!");
      return;
    }
  io_win_close (&wins[num]);
}

void 
delete_window ()
{
  io_win_close (cwin);
}

void
delete_other_windows ()
{
  if (nwin > 1)
    {
      CELLREF r = curow;
      CELLREF c = cucol;
      while (nwin > 1)
	io_win_close (cwin);
      io_move_cell_cursor (r, c);
    }
}

void
goto_window (text)
     char *text;
{
  int n;
  n = atoi (text) - 1;
  if (n < 0 || n > nwin)
    {
      io_error_msg ("Window %s doesn't exist.", text);
      return;
    }
  io_set_cwin (&wins[n]);
}

void
exchange_point_and_mark ()
{
  CELLREF r = curow;
  CELLREF c = cucol;
  struct rng rng;
  
  rng.lr = rng.hr = r;
  rng.lc = rng.hc = c;
  goto_region (&rng);
  mkrow = r;
  mkcol = c;
  io_update_status ();
}

#ifdef __STDC__
void 
nicely_goto_window (int n)
#else
void 
nicely_goto_window (n)
     int n;
#endif
{
  if (input_active)
    {
      io_cellize_cursor ();
      window_after_input = n;
      input_active = 0;
      return;
    }
  else
    {
      if (window_after_input == n)
	{
	  io_inputize_cursor ();
	  window_after_input = -1;
	  input_active = 1;
	}
      else
	io_set_cwin (&wins[n]);
    }
}

void 
other_window ()
{
  int n = cwin - wins;
  if (!input_active)
    n = (n + 1) % nwin;
  nicely_goto_window (n);
}

int
set_window_option (set_opt, text)
     int set_opt;
     char *text;
{
  int n;
  int stat;
  static struct opt
    {
      char *text;
      int bits;
    }
  opts[] =
  {
    {
      "reverse", WIN_EDGE_REV
    }
    ,
    {
      "standout", WIN_EDGE_REV
    }
    ,
    {
      "page", WIN_PAG_HZ | WIN_PAG_VT
    }
    ,
    {
      "pageh", WIN_PAG_HZ
    }
    ,
    {
      "pagev", WIN_PAG_VT
    }
    ,
    {
      "lockh", WIN_LCK_HZ
    }
    ,
    {
      "lockv", WIN_LCK_VT
    }
    ,
    {
      "edges", WIN_EDGES
    }
  };
  if ((stat = (!strincmp (text, "status", 6) && isspace (text[6])))
      || (!strincmp (text, "input", 5) && isspace (text[5])))
    {
      int n = set_opt ? atoi (text + 6 + stat) : 0;	/* A little pun. */
      int new_inp = stat ? user_input : n;
      int new_stat = stat ? n : user_status;
      io_set_input_status (new_inp, new_stat, 1);
    }
  else if (!strincmp (text, "link", 4))
    {
      if (set_opt)
	{
	  n = atoi (text + 4) - 1;
	  if (n < 0 || n > nwin)
	    io_error_msg ("Can't '%s': window # out of range", text);
	  else
	    cwin->link = n;
	}
      else
	cwin->link = -1;
    }
  else if (set_opt && !stricmp (text, "unlink"))
    cwin->link = -1;
  else if (set_opt && !strincmp (text, "row ", 4))
    {
      text += 4;
      curow = astol (&text);
    }
  else if (set_opt && !strincmp (text, "col ", 4))
    {
      text += 4;
      cucol = astol (&text);
    }
  else
    {
      for (n = 0; n < sizeof (opts) / sizeof (struct opt); n++)
	if (!stricmp (text, opts[n].text))
	  {
	    if (set_opt)
	      cwin->flags |= opts[n].bits;
	    else
	      cwin->flags &= ~opts[n].bits;
	    break;
	  }

      if (n == sizeof (opts) / sizeof (struct opt))
	  return 0;
    }
  return 1;
}

void
show_window_options ()
{
  int n;

  cwin->curow = curow;
  cwin->cucol = cucol;
  if (user_status)
    io_text_line ("Status line at %d", user_status);
  else
    io_text_line ("Status line disabled.");
  io_text_line ("");
  for (n = 0; n < nwin; n++)
    {
      int flags = wins[n].flags;
      io_text_line ("Window #%d showing %s, with cursor at %s",
		    n + 1,
		    range_name (&wins[n].screen),
		    cell_name (wins[n].curow, wins[n].cucol));
      io_text_line ("   Options:  %sedges (%sreverse)%s%s%s%s",
		    flags & WIN_EDGES ? "" : "no",
		    flags & WIN_EDGE_REV ? "" : "no",
		    flags & WIN_PAG_HZ ? ", pageh" : "",
		    flags & WIN_PAG_VT ? ", pagev" : "",
		    flags & WIN_LCK_HZ ? ", lockh" : "",
		    flags & WIN_LCK_VT ? ", lockv" : "");
      if (wins[n].link != -1)
	io_text_line ("Linked to window %d", wins[n].link + 1);
    }
}

static struct cmd_func *
find_func (table, name)
     struct cmd_func *table;
     char *name;
{
  while (table->func_name)
    if (!strcmp (name, table->func_name))
      return table;
    else
      ++table;
  return 0;
}

void
init_maps ()
{
  num_maps = 0;
  the_maps = 0;
  map_names = 0;

  the_funcs = ck_malloc (sizeof (struct cmd_func *) * 2);
  num_funcs = 2;
  the_funcs[0] = &cmd_funcs[0];
  the_funcs[1] = &edit_funcs[0];

  run_init_cmds ();

  end_macro_cmd = find_func (cmd_funcs, "stop-entering-macro");
  digit_0_cmd = find_func (cmd_funcs, "digit-0");
  digit_9_cmd = find_func (cmd_funcs, "digit-9");
}

static int 
add_usr_cmds (new_cmds)
     struct cmd_func *new_cmds;
{
  num_funcs++;
  the_funcs = ck_realloc (the_funcs, num_funcs * sizeof (struct cmd_func *));
  the_funcs[num_funcs - 1] = new_cmds;
  return num_funcs - 1;
}

#ifdef USE_DLD
static int 
add_usr_maps (new_maps)
     struct keymap **new_maps;
{
  int n;

  for (n = 1; new_maps[n]; n++)
    ;
  the_maps = ck_realloc (the_maps, (n + num_maps) * sizeof (struct keymap *));
  bcopy (new_maps, &the_maps[num_maps], n * sizeof (struct keymap *));
  num_maps += n;
  return num_maps - n;
}

#endif

static int last_map;


static char * disclaimer[] = 
{
  " Copyright (C) 1990, 1991, 1992, 1993 Free Software Foundation,Inc.\n",
  "There is ABSOLUTELY NO WARRANTY for Oleo; see the file COPYING\n",
  "for details.  Oleo is free software and you are welcome to distribute\n",
  "copies of it under certain conditions; see the file COPYING to see the\n",
  "conditions.\n\n",
  0
};

static char short_options[] = "Vqfh";
static struct option long_options[] =
{
  {"version", 0, NULL, 'V'},
  {"quiet", 0, NULL, 'q'},
  {"ignore-init-file", 0, NULL, 'f'},
  {"nw", 0, NULL, 'x'},
  {"help", 0, NULL, 'h'},
  {NULL, 0, NULL, 0}
};

static char * usage[] = 
{
  " [--version] [--quiet] [--ignore-init-file] [--nw] [--help] \n",
  " [-Vqfh] [file]\n",
  0
};

/* Avoid needless messages to stdout. */
int spread_quietly = 0;

/* Avoid using X no matter what else. (-x --no-x) */
int no_x = 0;

static void
show_usage ()
{
  char ** use = usage;
  fprintf (stderr, "Usage: %s ", argv_name);
  while (*use)
    {
      fprintf (stderr, "%s\n", *use);
      ++use;
    }
}

int 
main (argc, argv)
     int argc;
     char **argv;
{
  int c;
  int num;
  int using_x = 0;
  int using_curses = 0;
  int ignore_init_file = 0;
  FILE * init_fp[2];
  int init_fpc = 0;

  argv_name = argv[0];
  __make_backups = 1;

  /* Set up the minimal io handler. */
#if 0
  cmd_graphics ();
#endif

  {
    int opt;
    for (opt = getopt_long (argc, argv, short_options, long_options, (int *)0);
	 opt != EOF;
	 opt = getopt_long (argc, argv, short_options, long_options, (int *)0))
      {
	switch (opt)
	  {
	  case 'V':
	    fprintf  (stdout, "%s\n", oleo_version_string);
	    break;
	  case 'q':
	    spread_quietly = 1;
	    break;
	  case 'f':
	    ignore_init_file = 1;
	    break;
	  case 'x':
	    no_x = 1;
	    break;
	  case 'h':
	    show_usage ();
	    break;
	  }
      }
  }
  init_infinity ();
  init_mem ();
  init_eval ();
  init_refs ();
  init_cells ();
  init_fonts ();
  obstack_init (&macro_stack);
#ifdef USE_DLD
  if (!index (argv_name, '/'))
    {
      char *name;

      name = dld_find_executable (argv_name);
      num = dld_init (name);
      free (name);
    }
  else
    num = dld_init (argv_name);
  if (num)
    io_error_msg ("dld_init() failed: %s", (dld_errno < 0 || dld_errno > dld_nerr) ? "Unknown error" : dld_errlst[dld_errno]);
  dld_search_path = ":/usr/local/lib/oleo:/lib:/usr/lib:/usr/local/lib";
#endif



  /* Find the init files. 
   * This is done even if ignore_init_file is true because
   * it effects whether the disclaimer will be shown.
   */
    {
      char *ptr, *home;
      
      home = getenv ("HOME");
      if (home)
	{
	  ptr = mk_sprintf ("%s/%s", home, RCFILE);
	  init_fp[init_fpc] = fopen (ptr, "r");
	  if (init_fp[init_fpc])
	    ++init_fpc;
	  free (ptr);
	}
      
      init_fp[init_fpc] = fopen (RCFILE, "r");
      if (init_fp[init_fpc])
	++init_fpc;
    }

  if (!init_fpc && !spread_quietly)
    {
      char ** msg;
      fputs (oleo_version_string, stdout);
      for (msg = disclaimer; *msg; ++msg)
	fputs (*msg, stdout);
      fflush (stdout);
    }

  FD_ZERO (&read_fd_set);
  FD_ZERO (&read_pending_fd_set);
  FD_ZERO (&exception_fd_set);
  FD_ZERO (&exception_pending_fd_set);

#ifdef HAVE_X11_X_H
  if (!no_x)
    get_x11_args (&argc, argv);
  if (!no_x && io_x11_display_name)
    {
      x11_graphics ();
      using_x = 1;
    }
  else
#endif
    {
      tty_graphics ();
      using_curses = 1;
      /* Allow the disclaimer to be read. */
      if (!init_fpc && !spread_quietly)
	sleep (5);
    }

  io_open_display ();

  init_maps ();

  if (argc - optind > 1)
    {
      show_usage ();
      exit (1);
    }

  /* Read the init file. */
  {
    int x;
    for (x = 0; x < init_fpc; ++x)
      {
	if (!ignore_init_file)
	  read_cmds_cmd (init_fp[x]);
	fclose (init_fp[x]);
      }
  }

  /* These probably don't all need to be ifdef, but
   * it is harmless.
   */
#ifdef SIGINT
  signal (SIGINT, got_sig);
#endif
#ifdef SIGQUIT
  signal (SIGQUIT, got_sig);
#endif
#ifdef SIGILL
  signal (SIGILL, got_sig);
#endif
#ifdef SIGEMT
  signal (SIGEMT, got_sig);
#endif
#ifdef SIGBUS
  signal (SIGBUS, got_sig);
#endif
#ifdef SIGSEGV
  signal (SIGSEGV, got_sig);
#endif
#ifdef SIGPIPE
  signal (SIGPIPE, got_sig);
#endif

  if (argc - optind == 1)
    {
      FILE * fp;
      set_line (&in_line['d' - 'a'], argv[optind]);
      ++optind;
      if (fp = fopen (argv[1], "r"))
	{
	  read_file_and_run_hooks (fp, 0);
	  fclose (fp);
	}
      else
	io_error_msg ("Can't open %s: %s", argv[1], err_msg ());
    }

  io_recenter_cur_win ();

  for (;;)
    {
      if (how_many != 1)
	{
	  how_many = 1;
	  io_update_status ();
	}
      io_clear_input_before ();
      map_chr (MAIN_MAP);
      if (!rmac)
	io_clear_input_after ();

    swtch:
      if (!cur_cmd)
	{
	  io_error_msg ("Key %s is unbound in map %s",
			char_to_string (cur_chr),
			map_names[last_map]);
	  continue;
	}
      if ((cur_cmd->func_flags & NONTOP) == 0)
	{
	  io_error_msg ("Command '%s' is not appropriate now.", cur_cmd->func_name);
	  continue;
	}
      num = global_cmd (MAIN_MAP);
      if (num == -2)
	io_error_msg ("Command '%s' is *not* appropriate now.", cur_cmd->func_name);
      else if (num >= 0)
	{
	  c = num;
	  goto swtch;
	}
    }
}

static FILE *
open_file (prompt, line, mode)
     char *prompt;
     struct line *line;
     char *mode;
{
  FILE *fp;

  if (io_get_line (prompt, line))
    return 0;
  if ((fp = xopen_with_backup (line->buf, mode)) == 0)
    {
      io_error_msg ("Can't open file '%s':%s", line->buf, err_msg ());
      return 0;
    }
  return fp;
}

static void
close_file (line, fp)
     struct line *line;
     FILE *fp;
{
  int num;

  if (num = xclose (fp))
    io_error_msg ("Can't close '%s': Error code %d: %s", line->buf, num, err_msg ());
}

/* Deal with point-n-shoot */
static int
get_a_range (whatfor, r, line)
     char *whatfor;
     struct rng *r;
     struct line *line;
{
  char *ptr;

  if (mkrow != NON_ROW)
    {
      /* The range has already been selected */
    got_rng:
      set_rng (r, mkrow, mkcol, curow, cucol);
      mkrow = NON_ROW;
      return 0;
    }
  switch (io_get_line (whatfor, line))
    {
    case 0:
      if (mkrow != NON_ROW)
	goto got_rng;
      ptr = line->buf;
      if (get_abs_rng (&ptr, r) || *ptr != '\0')
	{
	  io_error_msg ("Can't parse '%s'", line->buf);
	  return 1;
	}
      return 0;
    case 1:
      if (mkrow != NON_ROW)
	goto got_rng;
      return 1;
    case 2:
      return 1;
#ifdef TEST
    default:
      panic ("io_get_line() what?");
#endif
    }
  return 1;
}

static char *
get_range_and_font (cmd_name, r, line)
     char *cmd_name;
     struct rng *r;
     struct line *line;
{
  char *ptr;
  char *prm;
  int got_rng = 0;
  extern char *strdup ();

  if (mkrow != NON_ROW)
    {
      /* We've got the range, just get the string */
      set_rng (r, mkrow, mkcol, curow, cucol);
      got_rng++;
      mkrow = NON_ROW;
      prm = mk_sprintf ("%s %s to", cmd_name, range_name (r));
    }
  else
    prm = strdup (cmd_name);

  if (io_get_line (prm, line))
    {
      free (prm);
      return 0;
    }
  free (prm);

  if (mkrow != NON_ROW)
    {
      if (got_rng)
	{
	  io_error_msg ("Two ranges?");
	  return 0;
	}
      set_rng (r, mkrow, mkcol, curow, cucol);
      mkrow = NON_ROW;
      got_rng++;
    }
  ptr = line->buf;
  if (!got_rng)
    {
      if (get_abs_rng (&ptr, r))
	{
	  io_error_msg ("Can't parse '%s'", line->buf);
	  return 0;
	}
      if (*ptr == ',')
	ptr++;
    }
  while (*ptr == ' ')
    ptr++;
  if (!*ptr)
    {
      if (io_get_line (cmd_name, line))
	{
	  return 0;
	}
      ptr = line->buf;
    }
  return ptr;
}


static FILE *
get_range_and_file (whatfor, r, line)
     char *whatfor;
     struct rng *r;
     struct line *line;
{
  char *ptr;
  char *prompt;
  int got_rng = 0;
  FILE *ret;
  extern char *strdup ();

  if (mkrow != NON_ROW)
    {
      /* We've got the range, just get the file */
      set_rng (r, mkrow, mkcol, curow, cucol);
      got_rng++;
      mkrow = NON_ROW;
      prompt = mk_sprintf ("%s %s to file", whatfor, range_name (r));
    }
  else
    prompt = strdup (whatfor);

  if (io_get_line (prompt, line))
    {
      free (prompt);
      return 0;
    }
  free (prompt);

  if (mkrow != NON_ROW)
    {
      if (got_rng)
	{
	  io_error_msg ("Two ranges?");
	  return 0;
	}
      set_rng (r, mkrow, mkcol, curow, cucol);
      mkrow = NON_ROW;
      got_rng++;
    }
  ptr = line->buf;
  if (!got_rng)
    {
      if (get_abs_rng (&ptr, r))
	{
	  io_error_msg ("Can't parse '%s'", line->buf);
	  return 0;
	}
      if (*ptr == ',')
	ptr++;
    }
  while (*ptr == ' ')
    ptr++;
  if (!*ptr)
    {
      if (io_get_line ("To file: ", line))
	return 0;
      ptr = line->buf;
    }
  ret = xopen_with_backup (ptr, "w");
  if (!ret)
    io_error_msg ("Can't open file '%s':%s", ptr, err_msg ());
  return ret;
}


static int
get_two_ranges (whatfor, one, two, line)
     char *whatfor;
     struct rng *one;
     struct rng *two;
     struct line *line;
{
  char *ptr;
  char *prompt;
  int num_got = 0;
  int n;

  prompt = 0;
  if (mkrow != NON_ROW)
    {
      /* We've got the first one already */
      set_rng (one, mkrow, mkcol, curow, cucol);
      mkrow = NON_ROW;
      num_got++;
    }

  while (num_got != 2)
    {
      prompt = (num_got == 1) ? mk_sprintf ("%s %s to", whatfor, range_name (one)) : whatfor;
      n = io_get_line (prompt, line);
      if (prompt != whatfor)
	free (prompt);

      if (n > 1 || (n == 1 && mkrow == NON_ROW))
	return 1;

      if (mkrow != NON_ROW)
	{
	  if (line->buf[0])
	    {
	      io_error_msg (num_got ? "Extra characters '%s'" : "'%s' is ambiguous", line->buf);
	      return 1;
	    }
	  set_rng (num_got ? two : one, mkrow, mkcol, curow, cucol);
	  mkrow = NON_ROW;

	  num_got++;
	}
      else
	{
	  ptr = line->buf;
	  if (!num_got)
	    {
	      if (get_abs_rng (&ptr, one))
		{
		  io_error_msg ("Can't parse first range in '%s'", line->buf);
		  return 1;
		}
	      if (*ptr == ',')
		ptr++;
	    }

	  if (get_abs_rng (&ptr, two))
	    {
	      if (num_got)
		{
		  io_error_msg ("Can't find second range in '%s'", line->buf);
		  return 1;
		}
	      num_got++;
	    }
	  else if (!num_got)
	    num_got += 2;
	  else
	    num_got++;
	}
    }
  if (*ptr)
    {
      io_error_msg ("Extra characters in '%s'", line->buf);
      return 1;
    }

  sprint_line (line, "%s %s", range_name (one), range_name (two));
  return 0;
}


#ifdef __STDC__
void 
map_chr (int map)
#else
void 
map_chr (map)
     int map;
#endif
{
  int ch;
  struct keymap *keymap;
  struct key *key;

  keymap = the_maps[map];
  last_map = map;
  for (;;)
    {
      if (rmac)
	{
	  int len;
	  unsigned char *ptr;

	tryagain:
	  deal_alarm ();	/* If an alarm has gone off, deal. */
	  ch = *(rmac->mac_exe++);
	  switch (ch)
	    {
	    case '\0':
	      cur_vector = 0;
	      cur_cmd = end_macro_cmd;
	      cur_chr = 0;
	      return;

	    case 0x80 | 'a':
	      ch = '\0';
	      break;

	    case 0x80 | 'b':
	      ch = '{';
	      break;

	    case '{':
	      for (ptr = rmac->mac_exe;
		   *ptr && *ptr != ' ' && *ptr != '}';
		   ptr++);
	      len = ptr - rmac->mac_exe;
	      for (cur_vector = 0; cur_vector < num_funcs; cur_vector++)
		for (cur_cmd = &the_funcs[cur_vector][0];
		     cur_cmd->func_name;
		     cur_cmd++)
		  if (!strincmp ((char *) (rmac->mac_exe),
				 cur_cmd->func_name, len)
		      && cur_cmd->func_name[len] == '\0')
		    {
		      cur_chr = '\0';
		      goto out;
		    }
	      io_error_msg ("Ignoring unknown function '%.*s' in macro",
			    len, rmac->mac_exe);
	      while (*ptr != '\0' && *ptr != '}')
		ptr++;
	      if (*ptr == '}')
		ptr++;
	      rmac->mac_exe = ptr;
	      goto tryagain;

	    out:
	      if (*ptr == ' ')
		{
		  /* ... add argument support here ... */
		  if (!cur_cmd->func_args)
		    {
		      io_error_msg ("Ignoring extra operand to %s",
				    cur_cmd->func_name);
		      while (*ptr && *ptr != '}')
			ptr++;
		      if (*ptr == '}')
			ptr++;
		    }
		  else if (cur_cmd->func_args[0] == 'n')
		    {
		      how_many = astol ((char **) (&ptr));
		      if (*ptr == '}')
			ptr++;
		    }
		  else
		    {
		      macro_func_arg = (char *) ptr;
		      while (*ptr && *ptr != '}')
			ptr++;
		      if (*ptr == '}')
			*ptr++ = '\0';
		    }
		  rmac->mac_exe = ptr;
		}
	      else
		rmac->mac_exe += len + 1;
	      return;
	    }
	}
      else
	ch = real_get_chr ();

      for (;;)
	{
	  key = &(keymap->keys[ch]);
	  if (key->vector < 0)
	    if (key->code >= 0)
	      {
		keymap = the_maps[key->code];
		last_map = key->code;
		break;
	      }
	    else if (keymap->map_next)
	      keymap = keymap->map_next;
	    else
	      {
		cur_vector = 0;
		cur_cmd = 0;
		cur_chr = ch;
		return;
	      }
	  else
	    {
	      cur_vector = key->vector;
	      cur_cmd = &(the_funcs[key->vector][key->code]);
	      cur_chr = ch;
	      return;
	    }
	}
    }
}

/* Return values:
   -3	break key!
   -2	c is inappropriate
   -1	C is OK
   0+	New char value is ret-1;
   */

int 
global_cmd (magic)
     int magic;
{
  char *ptr;
  struct rng fm, to;
  FILE *fp;
  static struct line confirm;
  struct cmd_func *cmd;

  if (!cur_cmd)
    return -2;
  cmd = cur_cmd;
  if (cmd->func_args)
    {
      ptr = cmd->func_args;

    next_arg:
      switch (*ptr++)
	{
	case 'R':
	  if (get_two_ranges (cmd->func_name, &fm, &to, &in_line[*ptr++ - 'a']))
	    break;
	  (*cmd->func_func) (&fm, &to);
	  break;

	case 'r':
	  if (get_a_range (cmd->func_name, &to, &in_line[*ptr++ - 'a']))
	    break;
	  (*cmd->func_func) (&to);
	  break;

	case 't':
	  if (io_get_line (cmd->func_name, &in_line[*ptr++ - 'a']))
	    break;
	  (*cmd->func_func) (in_line[cmd->func_args[1] - 'a'].buf);
	  break;

	case 'C':
	  if (modified == 0)
	    goto next_arg;
	  set_line (&confirm, "");
	  if (io_get_line ("Are you SURE", &confirm)
	      || (confirm.buf[0] != 'Y' && confirm.buf[0] != 'y'))
	    return -1;
	  goto next_arg;

	case 'f':
	  fp = open_file (cmd->func_name, &in_line[*ptr - 'a'], ptr + 1);
	  if (!fp)
	    break;
	  (*cmd->func_func) (fp);
	  close_file (&in_line[*ptr++ - 'a'], fp);
	  break;

	case 'F':
	  fp = get_range_and_file (cmd->func_name, &to, &in_line[*ptr - 'a']);
	  if (!fp)
	    break;
	  (*cmd->func_func) (fp, &to);
	  close_file (&in_line[*ptr++ - 'a'], fp);
	  break;

	case 'S':
	  {
	    char *cp = get_range_and_font (cmd->func_name, &to, &in_line[*ptr - 'a']);
	    if (!cp)
	      break;
	    (*cmd->func_func) (cp, &to);
	    break;
	  }

	case 'n':
	  if (!*ptr)
	    (*cmd->func_func) ();
	  else if (*ptr >= '0' && *ptr <= '9')
	    (*cmd->func_func) (*ptr - '0');
	  else if (*ptr >= 'a' && *ptr <= 'z')
	    (*cmd->func_func) ((*ptr - 'a') * 26 + ptr[1] - 'a');
	  else
	    (*cmd->func_func) ();
	  break;

	case 'T':
	  if (*ptr >= '0' && *ptr <= '9')
	    (*cmd->func_func) (*ptr - '0', cur_chr);
	  else
	    (*cmd->func_func) ();
	  break;

	case 'N':
	  (*cmd->func_func) (magic);
	  return cur_chr;

	case '\0':
	  (*cmd->func_func) ();
	  break;

#ifdef TEST
	default:
	  panic ("Unknown arg type %s in global_cmd", ptr);
#endif
	}
      if (cmd->func_flags & BRK)
	return -3;
      return -1;
    }
  else if (cmd->func_func)
    {
      if (cmd->func_flags & WTH_CHR)
	(*cmd->func_func) (cur_chr);
      else
	(*cmd->func_func) ();
      if (cmd->func_flags & BRK)
	return -3;
      return -1;
    }
  return -2;
}

static void 
write_keys_cmd (fp)
     FILE *fp;
{
  struct keymap *map;
  int n;
  int key;
  int vec;
  int code;

  for (n = 0; n < num_maps; n++)
    {
      char *def;
      map = the_maps[n];
      def = 0;
      if (map && map->map_next)
	{
	  for (key = 0; key < num_maps; key++)
	    if (the_maps[key] == map->map_next)
	      {
		def = map_names[key];
		break;
	      }
	}
      if (def)
	fprintf (fp, "create-keymap %s %s\n", map_names[n], def);
      else
	fprintf (fp, "create-keymap %s\n", map_names[n]);
    }
  for (n = 0; n < num_maps; n++)
    {
      map = the_maps[n];
      for (key = 0; key < 256; key++)
	{
	  vec = map->keys[key].vector;
	  code = map->keys[key].code;
	  if (vec < 0 && code >= 0)
	    fprintf (fp, "bind-key %s %s %s\n",
		     map_names[n],
		     map_names[code],
		     char_to_string (key));
	  else if (vec >= 0)
	    fprintf (fp, "bind-key %s %s %s\n",
		     map_names[n],
		     the_funcs[vec][code].func_name,
		     char_to_string (key));
	}
    }
}

static void 
clear_keymap (m)
     struct keymap *m;
{
  int n;
  for (n = 0; n < 256; n++)
    {
      m->keys[n].vector = -1;
      m->keys[n].code = -1;
    }
}

int 
map_id (name)
     char *name;
{
  int x;
  for (x = 0; x < num_maps; ++x)
    if (!strcmp (name, map_names[x]))
      return x;
  return -1;
}

static void 
make_keymap (mapname)
     char *mapname;
{
  char *ptr;
  int num;
  struct keymap *linkmap;
  extern char *strdup ();

  for (ptr = mapname; *ptr && !isspace (*ptr); ++ptr);
  linkmap = 0;
  if (*ptr)
    {
      *ptr++ = '\0';
      for (num = 0; num < num_maps; num++)
	{
	  if (!strcmp (map_names[num], ptr))
	    {
	      linkmap = the_maps[num];
	      break;
	    }
	}
    }
  for (num = 0; num < num_maps; num++)
    {
      if (!strcmp (map_names[num], mapname))
	{
	  clear_keymap (the_maps[num]);
	  return;
	}
    }
  the_maps = ck_realloc (the_maps, (num_maps + 1) * sizeof (struct keymap *));
  the_maps[num_maps] = ck_malloc (sizeof (struct keymap));
  the_maps[num_maps]->map_next = linkmap;
  {
    int c;
    struct key k;
    k.code = k.vector = -1;
    for (c = 0; c < 256; ++c)
      the_maps[num_maps]->keys[c] = k;
  }
  map_names = ck_realloc (map_names, (num_maps + 1) * sizeof (char *));
  map_names[num_maps] = strdup (mapname);
  num_maps++;
}

static void 
read_cmds_cmd (fp)
     FILE *fp;
{
  char *ptr;
  int lineno = 0;
  while (fgets (print_buf, 300, fp))
    {
      ptr = (char *)rindex (print_buf, '\n');
      if (ptr)
	*ptr = '\0';
      ++lineno;
      for (ptr = print_buf; isspace (*ptr); ptr++);
      if (!*ptr || (*ptr == '#'))
	continue;
      execute_cmd (ptr);
    }
  if (!feof (fp))
    io_error_msg ("read-cmds: read error near line %d.", lineno);
}

static void
write_cmd (fp)
     FILE *fp;
{
  (*write_file) (fp, 0);
  modified = 0;
}

static void
read_cmd (fp)
     FILE *fp;
{
  read_file_and_run_hooks (fp, 0);
}

static void
read_merge_cmd (fp)
     FILE *fp;
{
  (*read_file) (fp, 1);
}

static void
write_reg_cmd (fp, rng)
     FILE *fp;
     struct rng *rng;
{
  (*write_file) (fp, rng);
}

static void
sort_region_cmd (ptr)
     char *ptr;
{
  struct rng tmp_rng;

  if (get_abs_rng (&ptr, &sort_rng))
    {
      io_error_msg ("Can't find a range to sort in %s", ptr);
      return;
    }

  cur_row = sort_rng.lr;
  cur_col = sort_rng.lc;

  while (*ptr == ' ')
    ptr++;
  if (!*ptr)
    {
      sort_ele.lr = 0;
      sort_ele.lc = 0;
      sort_ele.hr = 0;
      sort_ele.hc = 0;
    }
  else if (!parse_cell_or_range (&ptr, &sort_ele))
    {
      io_error_msg ("Can't parse elements in %s", ptr);
      return;
    }
  else
    {
      sort_ele.lr -= sort_rng.lr;
      sort_ele.lc -= sort_rng.lc;
      sort_ele.hr -= sort_rng.lr;
      sort_ele.hc -= sort_rng.lc;
    }

  sort_keys_num = 0;
  while (*ptr == ' ')
    ptr++;
  for (; *ptr;)
    {
      if (sort_keys_num == sort_keys_alloc)
	{
	  sort_keys_alloc++;
	  if (sort_keys_alloc > 1)
	    sort_keys = ck_realloc (sort_keys, sort_keys_alloc * sizeof (struct cmp));
	  else
	    sort_keys = ck_malloc (sizeof (struct cmp));
	}
      sort_keys[sort_keys_num].mult = 1;
      if (*ptr == '+')
	ptr++;
      else if (*ptr == '-')
	{
	  sort_keys[sort_keys_num].mult = -1;
	  ptr++;
	}
      if (!*ptr)
	{
	  sort_keys[sort_keys_num].row = 0;
	  sort_keys[sort_keys_num].col = 0;
	  sort_keys_num++;
	  break;
	}
      if (!parse_cell_or_range (&ptr, &tmp_rng) || tmp_rng.lr != tmp_rng.hr || tmp_rng.lc != tmp_rng.hc)
	{
	  io_error_msg ("Can't parse key #%d in %s", sort_keys_num + 1, ptr);
	  sort_keys_num = -1;
	  return;
	}
      sort_keys[sort_keys_num].row = tmp_rng.lr - sort_rng.lr;
      sort_keys[sort_keys_num].col = tmp_rng.lc - sort_rng.lc;
      sort_keys_num++;

      while (*ptr == ' ')
	ptr++;
    }
  if (sort_keys_num == 0)
    {
      if (sort_keys_alloc == 0)
	{
	  sort_keys_alloc++;
	  sort_keys = ck_malloc (sizeof (struct cmp));
	}
      sort_keys[0].mult = 1;
      sort_keys[0].row = 0;
      sort_keys[0].col = 0;
      sort_keys_num++;
    }
  sort_region ();
  io_repaint ();
}

static void
set_var (ptr)
     char *ptr;
{
  int num;
  char *ret;

  while (*ptr == ' ')
    ptr++;
  for (num = 0; ptr[num] && ptr[num] != ' '; num++)
    ;
  modified = 1;
  if (!ptr[num])
    ret = new_var_value (ptr, num, (char *) 0);
  else
    ret = new_var_value (ptr, num, &ptr[num + 1]);
  if (ret)
    io_error_msg ("Can't set-variable %s: %s\n", ptr, ret);
}

static void
show_var (ptr)
     char *ptr;
{
  struct var *v;
  int num;

  while (*ptr == ' ')
    ptr++;
  for (num = 0; ptr[num] && ptr[num] != ' '; num++)
    ;

  v = find_var (ptr, num);
  if (!v || v->var_flags == VAR_UNDEF)
    {
      io_error_msg ("There is no '%s'", ptr);
      return;
    }
  if (a0)
    {
      if (v->v_rng.lr != v->v_rng.hr || v->v_rng.lc != v->v_rng.hc)
	/* FOO */ sprintf (print_buf, "%s $%s$%u:$%s$%u", v->var_name, col_to_str (v->v_rng.lc), v->v_rng.lr, col_to_str (v->v_rng.hc), v->v_rng.hr);
      else
	/* FOO */ sprintf (print_buf, "%s $%s$%u", v->var_name, col_to_str (v->v_rng.lc), v->v_rng.lr);
    }
  else
    sprintf (print_buf, "%s %s", v->var_name, range_name (&(v->v_rng)));
  io_info_msg (print_buf);
  set_line (&in_line[1], print_buf);
}

static void
show_a_var (name, v)
     char *name;
     struct var *v;
{
  if (v->var_flags == VAR_UNDEF)
    return;
  if (a0)
    {
      if (v->v_rng.lr != v->v_rng.hr || v->v_rng.lc != v->v_rng.hc)
	/* FOO */ io_text_line ("%-20s  $%s$%u:$%s$%u", v->var_name, col_to_str (v->v_rng.lc), v->v_rng.lr, col_to_str (v->v_rng.hc), v->v_rng.hr);
      else
	/* FOO */ io_text_line ("%-20s  $%s$%u", v->var_name, col_to_str (v->v_rng.lc), v->v_rng.lr);
    }
  else
    io_text_line ("%-20s  %s", v->var_name, range_name (&(v->v_rng)));
}

static void
show_all_var ()
{
  io_text_start ();
  io_text_line ("%-20s  Current Value", "Variable Name");
  for_all_vars (show_a_var);
  io_text_finish ();
}

static FILE * write_variable_fp = 0;

static void
write_a_var (name, v)
     char *name;
     struct var *v;
{
  CELLREF r, c;
  if (v->var_flags == VAR_UNDEF)
    return;
  r = v->v_rng.lr;
  c = v->v_rng.lc;
  if (v->var_flags == VAR_CELL)
    fprintf (write_variable_fp, "%s=%s\n",
	     v->var_name, cell_value_string (r, c));
}

static void
write_variables (fp)
     FILE * fp;
{
  if (write_variable_fp)
    io_error_msg ("Can't re-enter write_variables.");
  else
    {
      write_variable_fp = fp;
      for_all_vars (write_a_var);
      write_variable_fp = 0;
    }
}

static void
read_variables (fp)
     FILE * fp;
{
  char buf[1024];
  int lineno = 0;
  while (fgets (buf, 1024, fp))
    {
      char * ptr;
      for (ptr = buf; *ptr && *ptr != '\n'; ++ptr)
	;
      *ptr = '\0';
      for (ptr = buf; isspace (*ptr); ptr++)
	;
      if (!*ptr || (*ptr == '#'))
	continue;
      {
	char * var_name = ptr;
	int var_name_len;
	char * value_string;
	while (*ptr && *ptr != '=')
	  ++ptr;
	if (!*ptr)
	  {
	    io_error_msg ("read-variables: format error near line %d.", lineno);
	    return;
	  }
	var_name_len = ptr - var_name;
	++ptr;
	value_string = ptr;
	{
	  struct var * var = find_var (var_name, var_name_len);
	  if (var)
	    {
	      switch (var->var_flags)
		{
		case VAR_UNDEF:
		  break;
		case VAR_CELL:
		  set_cell_formula (value_string,
				    var->v_rng.lr, var->v_rng.lc);
		  break;
		case VAR_RANGE:
		  io_error_msg
		    ("read-variables (line %d): ranges not supported.",
		     lineno);
		  break;
		}
	    }
	}
      }
      ++lineno;
    }
  if (!feof (fp))
    {
      io_error_msg ("read-variables: read error near line %d.", lineno);
      return;
    }
}


static void
shift_cell_cursor (dir)
     int dir;
{
  io_shift_cell_cursor (dir);
}


static void
scroll_cell_cursor (dir)
     int dir;
{
  io_scroll_cell_cursor (dir);
}

static void
repaint ()
{
  io_repaint ();
}

static void
recenter_cur_win ()
{
  io_recenter_cur_win ();
}


static void
scan_cell_cursor (magic)
     int magic;
{
  CELLREF pos, pos_end, pos_inc;
  CELL *cp;
  int over, down;


  over = colmagic[magic] * how_many;
  down = rowmagic[magic] * how_many;
  if (over)
    {
      if (over > 0)
	{
	  pos = max_col (curow);
	  pos_end = MIN_COL;
	  pos_inc = -1;
	}
      else
	{
	  pos = MIN_COL;
	  pos_end = max_col (curow);
	  pos_inc = 1;
	}
      for (;;)
	{
	  for (; ((!(cp = find_cell (curow, pos)) || !GET_TYP (cp))); pos += pos_inc)
	    {
	      if (pos == pos_end)
		{
		  how_many = 1;
		  pos = MIN_COL;
		  break;
		}
	    }
	  if (--how_many == 0)
	    break;
	  pos += pos_inc;
	}
      io_move_cell_cursor (curow, pos);
    }
  else
    {
      if (down > 0)
	{
	  pos = max_row (cucol);
	  pos_end = MIN_ROW;
	  pos_inc = -1;
	}
      else
	{
	  pos = MIN_ROW;
	  pos_end = max_row (cucol);
	  pos_inc = 1;
	}
      for (;;)
	{
	  for (; (!(cp = find_cell (pos, cucol)) || !GET_TYP (cp)); pos += pos_inc)
	    {
	      if (pos == pos_end)
		{
		  how_many = 1;
		  pos = MIN_ROW;
		  break;
		}
	    }
	  if (--how_many == 0)
	    break;
	  pos += pos_inc;
	}

      io_move_cell_cursor (pos, cucol);
    }
}


char *
fmt_to_str (fmt)
     int fmt;
{
  char *ptr;
  static char buf[30];
  char nbuf[10];

  if (fmt == FMT_DEF)
    return "default";
  if (fmt == FMT_HID)
    return "hidden";
  if (fmt == FMT_GPH)
    return "graph";
  if ((fmt & PRC_FLT) == PRC_FLT)
    strcpy (nbuf, "float");
  else
    sprintf (nbuf, "%d", (fmt & PRC_FLT));
  switch (fmt | PRC_FLT)
    {
    case FMT_USR:
      ptr = "user-";
      sprintf (nbuf, "%d", (fmt & PRC_FLT) + 1);
      break;
    case FMT_GEN:
      ptr = "general.";
      break;
    case FMT_DOL:
      ptr = "dollar.";
      break;
    case FMT_CMA:
      ptr = "comma.";
      break;
    case FMT_PCT:
      ptr = "percent.";
      break;
    case FMT_FXT:
      if ((fmt & PRC_FLT) == 0)
	return "integer";
      if (fmt == FMT_FXT)
	return "decimal";
      ptr = "fixed.";
      break;
    case FMT_EXP:
      ptr = "exponent.";
      break;
    default:
      io_error_msg ("Unknown format %d (%x)", fmt, fmt);
      ptr = "UNKNOWN";
      break;
    }
  sprintf (buf, "%s%s", ptr, nbuf);
  return buf;
}

struct fmt
{
  int fmt;
  char **strs;
};

static char *def_names[] =
{"default", "def", "D", 0};
static char *hid_names[] =
{"hidden", "hid", "H", 0};
static char *gph_names[] =
{"graph", "gph", "*", 0};
static char *int_names[] =
{"integer", "int", "I", 0};
static char *dec_names[] =
{"decimal", "dec", 0};

static struct fmt simple[] =
{
  {FMT_DEF, def_names},
  {FMT_HID, hid_names},
  {FMT_GPH, gph_names},
  {FMT_FXT - PRC_FLT, int_names},
  {FMT_FXT, dec_names},
  {0, 0}
};

char *gen_names[] =
{"general.", "gen.", "G", 0};
char *dol_names[] =
{"dollar.", "dol.", "$", 0};
char *cma_names[] =
{"comma.", "com.", ",", 0};
char *pct_names[] =
{"percent.", "pct.", "%", 0};
char *fxt_names[] =
{"fixed.", "fxt.", "F", 0};
char *exp_names[] =
{"exponent.", "exp.", "E", 0};

static struct fmt withprec[] =
{
  {FMT_GEN - PRC_FLT, gen_names},
  {FMT_DOL - PRC_FLT, dol_names},
  {FMT_CMA - PRC_FLT, cma_names},
  {FMT_PCT - PRC_FLT, pct_names},
  {FMT_FXT - PRC_FLT, fxt_names},
  {FMT_EXP - PRC_FLT, exp_names},
  {0, 0}
};

static int
str_to_fmt (ptr)
     char *ptr;
{
  struct fmt *f;
  char **strs;
  int n;
  int ret;
  char *p1, *p2;

  for (f = simple; f->strs; f++)
    {
      for (strs = f->strs; *strs; strs++)
	{
	  if (*ptr != **strs)
	    continue;
	  for (p1 = ptr, p2 = *strs; *p1 == *p2 && *p1; p1++, p2++)
	    ;
	  if (*p1 == '\0' && *p2 == '\0')
	    return f->fmt;
	}
    }
  if (!strncmp (ptr, "user-", 5))
    {
      ptr += 5;
      n = astol (&ptr);
      if (*ptr || n < 1 || n > 16)
	return -1;
      return n - 1 - PRC_FLT + FMT_USR;
    }
  for (f = withprec, ret = 0; !ret && f->strs; f++)
    {
      for (strs = f->strs; *strs; strs++)
	{
	  if (*ptr != **strs)
	    continue;
	  for (p1 = ptr, p2 = *strs; *p2 && *p1 == *p2; p1++, p2++)
	    ;
	  if (!*p2)
	    {
	      ret = f->fmt;
	      ptr = p1;
	      break;
	    }
	}
    }

  if (!ret || !*ptr)
    return -1;
  if (!strcmp (ptr, "float") || !strcmp (ptr, "f"))
    {
      n = PRC_FLT;
    }
  else
    {
      n = astol (&ptr);
      if (*ptr || n < 0 || n > 14)
	return -1;
    }
  return ret + n;
}

char *
jst_to_str (jst)
     int jst;
{
  if (jst == JST_DEF)
    return "default";
  if (jst == JST_LFT)
    return "left";
  if (jst == JST_RGT)
    return "right";
  if (jst == JST_CNT)
    return "center";
  return "unknown";
}

static int
chr_to_jst (chr)
     int chr;
{
  if (chr == 'd' || chr == 'D')
    return JST_DEF;
  if (chr == 'l' || chr == 'L')
    return JST_LFT;
  if (chr == 'r' || chr == 'R')
    return JST_RGT;
  if (chr == 'c' || chr == 'C')
    return JST_CNT;
  return -1;
}

/* parse a range, then turn it into an absolute rng */
static int
get_abs_rng (pptr, retp)
     char **pptr;
     struct rng *retp;
{
  unsigned char n;

  while (**pptr == ' ')
    (*pptr)++;
  if (!**pptr)
    return 1;
  cur_row = curow;
  cur_col = cucol;
  n = parse_cell_or_range (pptr, retp);
  if (!n)
    {
      struct var *v;
      char *ptr;

      ptr = *pptr;
      while (ptr[n] && ptr[n] != ' ')
	n++;
      v = find_var (ptr, n);
      if (!v)
	return 1;
      (*pptr) += n;
      *retp = v->v_rng;
    }
  return 0;
}

#ifdef __STDC__
static void 
desc_map (struct keymap *map)
#else
static void 
desc_map (map)
     struct keymap *map;
#endif
{
  int n;

  io_text_start ();
  for (n = 0; n < 256; n++)
    if (map->keys[n].code >= 0)
      io_text_line ("%s: %d %d %s",
		    char_to_string (n),
		    map->keys[n].vector,
		    map->keys[n].code,
		    (map->keys[n].vector < 0
		     ? "vector"
		     : (the_funcs[map->keys[n].vector]
			[map->keys[n].code].func_name)));
  io_text_finish ();
}

static void 
set_usr_fmt (fmtstr)
     char *fmtstr;
{
  int u_num;
  char *ptr;
  static struct line usr_line;
  struct line tmp_line;
  char *tmp_ptr;
  char *data_buf[9];
  int i;
  static char *names[9] =
  {
    "Positive header",
    "Negative header",
    "Positive trailer",
    "Negative trailer",
    "Zero",
    "Comma",
    "Decimal point",
    "Precision",
    "Scale-factor"
  };

  tmp_line.buf = ck_malloc (10);
  tmp_line.alloc = 10;
  tmp_ptr = tmp_line.buf;

  ptr = fmtstr;
  u_num = astol (&ptr);
  if (u_num < 1 || u_num > 16 || *ptr != '\0')
    {
      io_error_msg ("Unknown number %s", fmtstr);
      return;
    }
  --u_num;

  get_usr_stats (u_num, data_buf);
  for (i = 0; i < 9; i++)
    {
      int slen;
      int prevlen;

      set_line (&usr_line, data_buf[i]);
      if (io_get_line (names[i], &usr_line) > 1)
	return;

      slen = strlen (usr_line.buf);
      if (tmp_line.alloc <= (tmp_ptr - tmp_line.buf) + slen)
	{
	  prevlen = tmp_ptr - tmp_line.buf;
	  tmp_line.alloc += slen + 1;
	  tmp_line.buf = ck_realloc (tmp_line.buf, tmp_line.alloc);
	  tmp_ptr = tmp_line.buf + prevlen;
	}
      strcpy (tmp_ptr, usr_line.buf);
      data_buf[i] = tmp_ptr;
      while (*tmp_ptr != '\0')
	tmp_ptr++;
      tmp_ptr++;
    }

  set_usr_stats (u_num, data_buf);
  free (tmp_line.buf);
  io_repaint ();
}


void
read_mp_usr_fmt (ptr)
     char *ptr;
{
  int usr_n = -1;
  int n_chrs = 0;
  char *p;
  char *buf[9];
  int i;

  for (i = 0; i < 9; i++)
    buf[i] = "";
  p = ptr;
  while (*p == ';')
    {
      *p++ = '\0';
      switch (*p++)
	{
	case 'N':
	  usr_n = astol (&p) - 1;
	  break;
	case 'H':
	  switch (*p++)
	    {
	    case 'P':
	      i = 0;
	      break;
	    case 'N':
	      i = 1;
	      break;
	    default:
	      goto badline;
	    }
	  goto count_chars;
	case 'T':
	  switch (*p++)
	    {
	    case 'P':
	      i = 2;
	      break;
	    case 'N':
	      i = 3;
	      break;
	    default:
	      goto badline;
	    }
	  goto count_chars;

	case 'Z':
	  i = 4;
	  goto count_chars;

	case 'C':
	  i = 5;
	  goto count_chars;

	case 'D':
	  i = 6;
	  goto count_chars;

	case 'P':
	  i = 7;
	  goto count_chars;

	case 'S':
	  i = 8;
	  goto count_chars;

	count_chars:
	  buf[i] = p;
	  n_chrs++;
	  while (*p && *p != ';')
	    {
	      p++;
	      n_chrs++;
	    }
	  break;

	default:
	badline:
	  io_error_msg ("Unknown OLEO line %s", ptr);
	  return;
	}
    }
  if (*p || usr_n < 0 || usr_n > 15)
    goto badline;

  set_usr_stats (usr_n, buf);
}

/* Modify this to write out *all* the options */
void
write_mp_options (fp)
     FILE *fp;
{
  fprintf (fp, "O;%sauto;%sbackground;%sa0;ticks %d\n",
	   auto_recalc ? "" : "no",
	   bkgrnd_recalc ? "" : "no",
	   a0 ? "" : "no",
	   alarm_seconds);
}

void 
read_mp_options (str)
     char *str;
{
  char *np;

  while (np = (char *)index (str, ';'))
    {
      *np = '\0';
      do_set_option (str);
      *np++ = ';';
      str = np;
    }
  if (np = (char *)rindex (str, '\n'))
    *np = '\0';
  (void) do_set_option (str);
}

#ifdef __STDC__
void
set_options (char * ptr)
#else
void
set_options (ptr)
     char *ptr;
#endif
{
  if (do_set_option (ptr))
    io_recenter_cur_win ();
}

void
play_cell ()
{
  CELL *cp;
  static char buf[20];
  int ch;

  cp = find_cell (curow, cucol);
  if (cp
      && ((GET_LCK (cp) == LCK_DEF && default_lock == LCK_LCK)
	  || GET_LCK (cp) == LCK_LCK))
    {
      io_error_msg ("Cell %s is locked", cell_name (curow, cucol));
      return;
    }
  io_info_msg ("Now playing %s.", cell_name (curow, cucol));
  while (1)
    {
      ch = real_get_chr ();
      if (ch == EOF || ch == '\027')
	return;
      sprintf (buf, "%d", ch);
      new_value (curow, cucol, buf);
      modified = 1;
    }
}

static void 
show_options ()
{
  int n;
  int fmts;
  char *data_buf[9];

  extern char *strdup ();
  extern void show_window_options ();
  extern int usr_set_fmts ();

  n = auto_recalc;
  io_text_start ();

  io_text_line ("auto-recalculation: %s        Recalculate in background: %s",
		n ? " on" : "off", bkgrnd_recalc ? "on" : "off");
  io_text_line ("make backup files:  %s        Copy files into backups:   %s",
	__make_backups ? " on" : "off", __backup_by_copying ? "on" : "off");

  io_text_line ("Asynchronous updates every %u ???",
		alarm_seconds);

  io_text_line ("Print width:      %5u", print_width);

  io_text_line ("");

  (*show_file_opts) ();

  io_text_line ("");
  show_window_options ();
  io_text_line ("");

  fmts = usr_set_fmts ();
  if (fmts)
    {
      io_text_line ("User-defined formats:");
      io_text_line ("Fmt    +Hdr    -Hdr   +Trlr   -Trlr    Zero   Comma Decimal  Prec         Scale");
      for (n = 0; n < 16; n++)
	{
	  if (fmts & (1 << n))
	    {
	      get_usr_stats (n, data_buf);
	      io_text_line ("%3d %7s %7s %7s %7s %7s %7s %7s %5s %13s",
			    n + 1,
			    data_buf[0],
			    data_buf[1],
			    data_buf[2],
			    data_buf[3],
			    data_buf[4],
			    data_buf[5],
			    data_buf[6],
			    data_buf[7],
			    data_buf[8]);
	    }
	}
    }
  else
    io_text_line ("No user-defined formats have been defined");

  io_text_finish ();
}

static int 
do_set_option (ptr)
     char *ptr;
{
  int set_opt = 1;
  extern int set_window_option ();

  while (*ptr == ' ')
    ptr++;
  if (!strincmp ("no", ptr, 2))
    {
      ptr += 2;
      set_opt = 0;
      while (*ptr == ' ')
	ptr++;
    }
  if (!stricmp ("auto", ptr))
    {
      auto_recalc = set_opt;
      return 0;
    }
  if (!stricmp ("bkgrnd", ptr) || !stricmp ("background", ptr))
    {
      bkgrnd_recalc = set_opt;
      return 0;
    }
  if (!stricmp ("a0", ptr))
    {
      a0 = set_opt;
      io_repaint ();
      return 0;
    }
  if (!stricmp ("backup", ptr))
    {
      __make_backups = set_opt;
      return 0;
    }
  if (!stricmp ("bkup_copy", ptr))
    {
      __backup_by_copying = set_opt;
      return 0;
    }
  if (set_opt && !strincmp ("ticks ", ptr, 6))
    {
      ptr += 6;
      alarm_seconds = astol (&ptr);
      return 0;
    }
  if (set_opt && !strincmp ("print ", ptr, 6))
    {
      ptr += 6;
      print_width = astol (&ptr);
      return 0;
    }
  if (set_opt && !strincmp ("file ", ptr, 5))
    {
#ifdef USE_DLD
      char *tmpstr;
      extern char *strdup ();

      ptr += 5;
      tmpstr = ck_malloc (strlen (ptr) + 20);
      if (io_name)
	{
	  sprintf (tmpstr, "%s.o", ptr);
	  if (dld_unlink_by_file (tmpstr, 0))
	    {
	      io_error_msg ("Couldn't unlink old file format %s: %s", io_name, (dld_errno < 0 || dld_errno > dld_nerr) ? "Unknown error" : dld_errlst[dld_errno]);
	      goto bad_file;
	    }
	  free (io_name);
	}
      if (!stricmp (ptr, "panic"))
	{
	  io_name = 0;
	  read_file = panic_read_file;
	  write_file = panic_write_file;
	  set_file_opts = panic_set_options;
	  show_file_opts = panic_show_options;
	  free (tmpstr);
	  return 0;
	}
      io_name = strdup (ptr);
      sprintf (tmpstr, "%s.o", ptr);
      if (dld_link (tmpstr))
	{
	  io_error_msg ("Couldn't link new file format %s: %s", io_name, (dld_errno < 0 || dld_errno > dld_nerr) ? "Unknown error" : dld_errlst[dld_errno]);
	  goto bad_file;
	}
      if (dld_link ("libc.a"))
	io_error_msg ("Couldn't link libc.a");
      if (dld_link ("libm.a"))
	io_error_msg ("Couldn't link libm.a");

      sprintf (tmpstr, "%s_read_file", ptr);
      read_file = dld_function_executable_p (tmpstr) ? dld_get_func (tmpstr) : 0;
      sprintf (tmpstr, "%s_write_file", ptr);
      write_file = dld_function_executable_p (tmpstr) ? dld_get_func (tmpstr) : 0;

      sprintf (tmpstr, "%s_set_options", ptr);
      set_file_opts = (int (*)()) (dld_function_executable_p (tmpstr) ? dld_get_func (tmpstr) : 0);
      sprintf (tmpstr, "%s_show_options", ptr);
      show_file_opts = dld_function_executable_p (tmpstr) ? dld_get_func (tmpstr) : 0;

      if (!read_file
	  || !write_file
	  || !set_file_opts
	  || !show_file_opts)
	{
	  char **missing;
	  int n;

	  missing = dld_list_undefined_sym ();
	  io_text_start ();
	  io_text_line ("Undefined symbols in file format %s:", ptr);
	  io_text_line ("");
	  for (n = 0; n < dld_undefined_sym_count; n++)
	    io_text_line ("%s", missing[n]);
	  io_text_line ("");
	  io_text_finish ();
	  free (missing);
	  io_error_msg ("File format %s has undefined symbols: not loaded", ptr);
	bad_file:
	  sprintf (tmpstr, "%s.o", io_name);
	  dld_unlink_by_file (io_name, 0);
	  if (io_name)
	    free (io_name);
	  io_name = 0;
	  read_file = panic_read_file;
	  write_file = panic_write_file;
	  set_file_opts = panic_set_options;
	  show_file_opts = panic_show_options;
	}
      free (tmpstr);
#else
      ptr += 5;
      if (!stricmp ("oleo", ptr))
	{
	  read_file = oleo_read_file;
	  write_file = oleo_write_file;
	  set_file_opts = oleo_set_options;
	  show_file_opts = oleo_show_options;
	}
      else if (!stricmp ("sylk", ptr))
	{
	  sylk_a0 = 1;
	  read_file = sylk_read_file;
	  write_file = sylk_write_file;
	  set_file_opts = sylk_set_options;
	  show_file_opts = sylk_show_options;
	}
      else if (!stricmp ("sylk-noa0", ptr))
	{
	  sylk_a0 = 0;
	  read_file = sylk_read_file;
	  write_file = sylk_write_file;
	  set_file_opts = sylk_set_options;
	  show_file_opts = sylk_show_options;
	}
      else if (!stricmp ("sc", ptr))
	{
	  read_file = sc_read_file;
	  write_file = sc_write_file;
	  set_file_opts = sc_set_options;
	  show_file_opts = sc_show_options;
	}
      else if (!stricmp ("panic", ptr))
	{
	  read_file = panic_read_file;
	  write_file = panic_write_file;
	  set_file_opts = panic_set_options;
	  show_file_opts = panic_show_options;
	}
      else if (!stricmp ("list", ptr))
	{
	  read_file = list_read_file;
	  write_file = list_write_file;
	  set_file_opts = list_set_options;
	  show_file_opts = list_show_options;
	  /*if (ptr[4])
	    {
	    ptr+=4;
	    sl_sep=string_to_char(&ptr);
	    } */
	}
      else
	io_error_msg ("Unknown file format %s", ptr);
#endif
      return 0;
    }
#ifdef USE_DLD
  else if (!strincmp (ptr, "load ", 5))
    {
      char *tmpstr;
      struct function *new_funs;
      struct cmd_func *new_cmds;
      struct keymap **new_maps;
      void (*init_cmd) ();

      extern unsigned long dld_get_symbol (char *);
      extern void add_usr_funs (struct function *);

      ptr += 5;
      tmpstr = ck_malloc (strlen (ptr) + 20);
      sprintf (tmpstr, "%s.o", ptr);
      if (dld_link (tmpstr))
	{
	  io_error_msg ("Couldn't link %s: %s", tmpstr, (dld_errno < 0 || dld_errno > dld_nerr) ? "Unknown error" : dld_errlst[dld_errno]);
	  free (tmpstr);
	  return 0;
	}
      if (dld_link ("libc.a"))
	io_error_msg ("Couldn't link libc.a");
      if (dld_link ("libm.a"))
	io_error_msg ("Couldn't link libm.a");

      if (dld_undefined_sym_count)
	{
	  char **missing;
	  int n;

	  missing = dld_list_undefined_sym ();
	  io_text_start ();
	  io_text_line ("Undefined symbols in file format %s:", ptr);
	  io_text_line ("");
	  for (n = 0; n < dld_undefined_sym_count; n++)
	    io_text_line ("%s", missing[n]);
	  io_text_line ("");
	  io_text_finish ();
	  free (missing);
	  io_error_msg ("%d undefined symbols in %s", dld_undefined_sym_count, ptr);
	  dld_unlink_by_file (tmpstr, 0);
	  free (tmpstr);
	  return 0;
	}
      sprintf (tmpstr, "%s_funs", ptr);
      new_funs = (struct function *) dld_get_symbol (tmpstr);
      if (new_funs)
	add_usr_funs (new_funs);
      sprintf (tmpstr, "%s_cmds", ptr);
      new_cmds = (struct cmd_func *) dld_get_symbol (tmpstr);
      if (new_cmds)
	add_usr_cmds (new_cmds);
      sprintf (tmpstr, "%s_maps", ptr);
      new_maps = (struct keymap **) dld_get_symbol (tmpstr);
      if (new_maps)
	add_usr_maps (new_maps);
      if (!new_funs && !new_cmds && !new_maps)
	{
	  io_error_msg ("Couldn't find anything to load in %s", ptr);
	  sprintf (tmpstr, "%s.o", ptr);
	  dld_unlink_by_file (tmpstr, 0);
	}
      sprintf (tmpstr, "%s_init", ptr);
      init_cmd = dld_function_executable_p (tmpstr) ? dld_get_func (tmpstr) : 0;
      if (init_cmd)
	(*init_cmd) ();
      free (tmpstr);
      return 0;
    }
#endif
  if (set_window_option (set_opt, ptr) == 0)
    {
      if ((*set_file_opts) (set_opt, ptr))
	io_error_msg ("Unknown option '%s'", ptr);
      return 0;
    }
  return 1;
}


void 
execute_cmd (str)
     char *str;
{
  CELL *cp;
  struct macro *old;
  struct rng r;
  char *ptr = str;

  while (isspace (*str))
    ++str;
  if (!*str || *str == '#')
    return;
  for (ptr = str; *ptr && *ptr != ' '; ptr++);

  if (*ptr)
    *ptr++ = '\0';
  else
    ptr = 0;

  for (cur_vector = 0; cur_vector < num_funcs; cur_vector++)
    for (cur_cmd = &the_funcs[cur_vector][0]; cur_cmd->func_name; cur_cmd++)
      if (!stricmp (str, cur_cmd->func_name))
	{
	  /* these lines stolen from map_char */
	  if (ptr)
	    {
	      if (!cur_cmd->func_args)
		io_error_msg ("Ignoring extra operand to %s",
			      cur_cmd->func_name);
	      else if (cur_cmd->func_args[0] == 'n')
		how_many = astol ((char **) (&ptr));
	      else
		macro_func_arg = ptr;
	    }
	  cur_chr = '\0';
	  global_cmd (MAIN_MAP);
	  return;
	}

  if (get_abs_rng (&str, &r))
    {
      io_error_msg ("Unknown command %s", str);
      return;
    }
  if (ptr)
    {
      io_error_msg ("Macros can't take arguments");
      return;
    }

  if (!(cp = find_cell (r.lr, r.lc))
      || GET_TYP (cp) != TYP_STR
      || cp->cell_str[0] == '\0')
    return;

  old = rmac;
  rmac = (struct macro *) obstack_alloc (&macro_stack, sizeof (struct macro));
  rmac->mac_prev = old;
  rmac->mac_rng = r;
  rmac->mac_row = r.lr;
  rmac->mac_col = r.lc;
  (void) obstack_grow (&macro_stack, cp->cell_str, 1 + strlen (cp->cell_str));
  rmac->mac_exe = (unsigned char *) obstack_finish (&macro_stack);
  rmac->mac_flags = 0;
}

static void
goto_region (r)
     struct rng *r;
{
  struct rng tmp;

  if (mkrow != NON_ROW)
    {
      CELLREF cx, cy;

      set_rng (&tmp, curow, cucol, mkrow, mkcol);
      set_line (&in_line[12], range_name (&tmp));
      cx = mkrow;
      mkrow = curow;
      cy = mkcol;
      mkcol = cucol;
      (void) io_move_cell_cursor (cx, cy);
    }
  else
    {
      set_line (&in_line[12], cell_name (curow, cucol));
      (void) io_move_cell_cursor (r->lr, r->lc);
    }
  if (r->hr != r->lr || r->hc != r->lc)
    {
      mkrow = r->hr;
      mkcol = r->hc;
      io_update_status ();
    }
  else if (mkrow != NON_ROW)
    {
      mkrow = NON_ROW;
      io_update_status ();
    }
}

static void
upper_left ()
{
  struct rng rng;
  rng.lr = rng.hr = MIN_ROW;
  rng.lc = rng.hc = MIN_COL;
  goto_region (&rng);
}

static void
lower_left ()
{
  struct rng rng;
  rng.lr = rng.hr = highest_row ();
  rng.lc = rng.hc = MIN_COL;
  goto_region (&rng);
}

static void
upper_right ()
{
  struct rng rng;
  rng.lr = rng.hr = MIN_ROW;
  rng.lc = rng.hc = highest_col ();
  goto_region (&rng);
}

static void
lower_right ()
{
  struct rng rng;
  rng.lr = rng.hr = highest_row ();
  rng.lc = rng.hc = highest_col ();
  goto_region (&rng);
}

static void
set_default ()
{
  int fun;
  char *ptr;
  int num;

  sprint_line (&wid_line, "%u", default_width);
  sprint_line (&hgt_line, "%u", default_height);

  set_line (&fmt_line, fmt_to_str (default_fmt));
  io_info_msg ("Alignment %s   Format %s   %slocked  Width %u  Height %u",
	       jst_to_str (default_jst),
	       fmt_line.buf,
	       default_lock == LCK_LCK ? "" : "un",
	       default_width,
	       default_height);

  fun = io_get_chr ("[A]lignment, [F]ormat, [P]rotection [W]idth, or [H]eight  ");
  switch (fun)
    {
    case 'w':
    case 'W':
      if (io_get_line ("set-default-width", &wid_line))
	break;
      ptr = wid_line.buf;
      num = astol (&ptr);
      if (num < 1)
	io_error_msg ("Can't set default width to '%s'", wid_line.buf);
      else
	{
	  default_width = num;
	  io_recenter_all_win ();
	  return;
	}
      break;

    case 'h':
    case 'H':
      if (io_get_line ("set-default height", &hgt_line))
	break;
      ptr = hgt_line.buf;
      num = astol (&ptr);
      if (num < 1)
	io_error_msg ("Can't set default height to '%s'", hgt_line.buf);
      else
	{
	  default_height = num;
	  io_recenter_all_win ();
	  return;
	}
      break;

    case 'p':
    case 'P':
    case 'l':
    case 'L':
      fun = io_get_chr ("[P]rotected, or [U]nprotected");
      if (fun == 'p' || fun == 'P')
	default_lock = LCK_LCK;
      else if (fun == 'u' || fun == 'U')
	default_lock = LCK_UNL;
      else			/* if (main_map[fun]!=BREAK_CMD) */
	io_error_msg ("Unknown char '%s'", char_to_string (fun));
      break;

    case 'f':
    case 'F':
      if (io_get_line ("set-default-format", &fmt_line))
	break;
      num = str_to_fmt (fmt_line.buf);
      if (num == -1 || num == FMT_DEF)
	{
	  io_error_msg ("Unknown format '%s'", fmt_line.buf);
	  break;
	}
      default_fmt = num;
      io_repaint ();
      return;

    case 'a':
    case 'A':
      fun = io_get_chr ("[L]eft, [R]ight, or [C]enter");
      num = chr_to_jst (fun);
      if (num != -1 && num != JST_DEF)
	{
	  default_jst = num;
	  io_repaint ();
	  return;
	}
      else			/* if (main_map[fun]!=BREAK_CMD) */
	io_error_msg ("Unknown Alignment '%s'", char_to_string (fun));
      break;

    default:
      /* if (main_map[fun]!=BREAK_CMD) */
      io_error_msg ("Unknown command '%s'", char_to_string (fun));
      /* else
	 io_error_msg(""); */
      break;
    }
  io_update_status ();
}

void 
format_area (f)
     struct rng *f;
{
  int c;
  int fmt, jst, wid, hgt;
  CELLREF cc;
  char *locked;
  CELL *cp;
  int fun;
  char *ptr;

  cp = find_cell (f->lr, f->lc);
  if (!cp)
    {
      fmt = FMT_DEF;
      jst = JST_DEF;
    }
  else
    {
      fmt = GET_FMT (cp);
      jst = GET_JST (cp);
    }

  wid = get_nodef_width (f->lc);
  if (wid == 0)
    set_line (&wid_line, "def");
  else
    sprint_line (&wid_line, "%d", wid - 1);

  hgt = get_nodef_height (f->lr);
  if (hgt == 0)
    set_line (&hgt_line, "def");
  else
    sprint_line (&hgt_line, "%d", hgt - 1);

  set_line (&fmt_line, fmt_to_str (fmt));
  set_line_to_nice_font_name (&font_line, cp ? cp->cell_font : 0);

  if (!cp || GET_LCK (cp) == LCK_DEF)
    locked = (default_lock == LCK_UNL ? "unlocked(def)" : "locked(def)");
  else if (GET_LCK (cp) == LCK_UNL)
    locked = "unlocked";
  else if (GET_LCK (cp) == LCK_LCK)
    locked = "locked";
  else
    locked = "Huh What?";

  io_info_msg ("Alignment %s   Format %s   Width %s   Height %s  %s",
	       jst_to_str (jst),
	       fmt_line.buf,
	       wid_line.buf,
	       hgt_line.buf,
	       locked);

  fun =
    io_get_chr
    ("[A]lignment, [F]ormat, f[O]nt, [P]rotection, [H]eight or [W]idth  ");
  switch (fun)
    {
    case 'f':
    case 'F':
      if (io_get_line ("set-format", &fmt_line))
	break;
      fmt = str_to_fmt (fmt_line.buf);
      if (fmt != -1)
	format_region (f, fmt, -1);
      else
	io_error_msg ("Unknown format '%s'", fmt_line.buf);
      break;

    case 'o':
    case 'O':
      if (io_get_line ("set-font", &font_line))
	break;
      set_area_font_cmd (font_line.buf, f);
      break;

    case 'a':
    case 'A':
      c = io_get_chr ("Align [L]eft, [R]ight, [C]enter, or [D]efault");
      fun = chr_to_jst (c);
      if (fun != -1)
	format_region (f, -1, fun);
      else			/* if (main_map[c]!=BREAK_CMD) */
	io_error_msg ("Unknown Justify '%s'", char_to_string (c));
      break;

    case 'p':
    case 'P':
    case 'l':
    case 'L':
      c = io_get_chr ("[D]efault, [P]rotect, or [U]nprotect");
      if (c == 'd' || c == 'D')
	lock_region (f, LCK_DEF);
      else if (c == 'p' || c == 'P')
	lock_region (f, LCK_LCK);
      else if (c == 'u' || c == 'U')
	lock_region (f, LCK_UNL);
      else			/* if (main_map[c]!=BREAK_CMD) */
	io_error_msg ("Unknown lock %s", char_to_string (c));
      break;

    case 'w':
    case 'W':
      if (io_get_line ("set-width", &wid_line))
	break;
      ptr = wid_line.buf;
      if (*ptr == 'd' || *ptr == 'D')
	fun = 0;
      else if (isdigit (*ptr))
	fun = astol (&ptr) + 1;
      else
	{
	  io_error_msg ("Unknown width '%s'", wid_line.buf);
	  break;
	}
      for (cc = f->lc;; cc++)
	{
	  set_width (cc, fun);
	  if (cc == f->hc)
	    break;
	}
      io_recenter_all_win ();
      return;
    case 'h':
    case 'H':
      if (io_get_line ("set height", &hgt_line))
	break;
      ptr = hgt_line.buf;
      if (*ptr == 'd' || *ptr == 'D')
	fun = 0;
      else
	{
	  fun = astol (&ptr) + 1;
	  if (*ptr)
	    {
	      io_error_msg ("Unknown height '%s'", hgt_line.buf);
	      break;
	    }
	}
      for (cc = f->lr;; cc++)
	{
	  set_height (cc, fun);
	  if (cc == f->hr)
	    break;
	}
      io_recenter_all_win ();
      return;

    default:
      /* if (main_map[fun]!=BREAK_CMD) */
      io_error_msg ("Unknown command '%s'", char_to_string (fun));
      break;
    }
  io_update_status ();
}

void
insert_row ()
{
  struct rng from;
  struct rng to;
  if ((how_many > (MAX_ROW - curow)) || (how_many < 0))
    {
      io_error_msg ("insert-row: prefix argument out of range.");
      return;
    }
  from.lc = MIN_COL;
  from.hc = MAX_COL;
  from.lr = curow;
  from.hr = MAX_ROW - how_many;
  to.lc = MIN_COL;
  to.hc = MIN_COL;
  to.lr = curow + how_many;
  to.hr = curow + how_many;
  move_region (&from, &to);
}

void
insert_col ()
{
  struct rng from;
  struct rng to;
  if ((how_many > (MAX_COL - cucol)) || (how_many < 0))
    {
      io_error_msg ("insert-col: prefix argument out of range.");
      return;
    }
  from.lr = MIN_ROW;
  from.hr = MAX_ROW;
  from.lc = cucol;
  from.hc = MAX_COL - how_many;
  to.lr = MIN_ROW;
  to.hr = MIN_ROW;
  to.lc = cucol + how_many;
  to.hc = cucol + how_many;
  move_region (&from, &to);
}

void
delete_row ()
{
  struct rng from;
  struct rng to;
  if ((how_many < 0) || (how_many > (MAX_ROW - curow + 1)))
    {
      io_error_msg ("delete-row: prefix argument out of range.");
      return;
    }
  from.lc = MIN_COL;
  from.hc = MAX_COL;
  from.lr = curow + how_many;
  from.hr = MAX_ROW;
  to.lc = MIN_COL;
  to.hc = MIN_COL;
  to.lr = curow;
  to.hr = curow;
  move_region (&from, &to);
}

void
delete_col ()
{
  struct rng from;
  struct rng to;
  if ((how_many < 0) || (how_many > (MAX_COL - cucol + 1)))
    {
      io_error_msg ("delete-col: prefix argument out of range.");
      return;
    }
  from.lr = MIN_ROW;
  from.hr = MAX_ROW;
  from.lc = cucol + how_many;
  from.hc = MAX_COL;
  to.lr = MIN_ROW;
  to.hr = MIN_ROW;
  to.lc = cucol;
  to.hc = cucol;
  move_region (&from, &to);
}


#ifdef __STDC__
static RETSIGTYPE
got_sig (int sig)
#else
static RETSIGTYPE
got_sig (sig)
     int sig;
#endif
{
}

#if 0
static void
got_sigint (ign)
     int ign;
{
  int ch;

  ch = io_get_chr ("Panic save and exit?");
  if (ch == 'y' || ch == 'Y')
    got_sig ();
}
#endif

static void
start_macro ()
{
  if (making_macro)
    {
      io_error_msg ("Can't define two macros at once");
      return;
    }
  making_macro_size = 20;
  making_macro = making_macro_start = ck_malloc (5 + making_macro_size);
  /* *making_macro++='"'; */
}

static void
end_macro ()
{
  union vals z;
  struct rng to;
  CELL *cp;
  struct macro *old;
  static struct line macro_line;

  if (!rmac && !making_macro)
    {
      io_error_msg ("Not executing or defining a macro!");
      return;
    }
  if (rmac)
    {
      if (rmac->mac_row == rmac->mac_rng.hr && rmac->mac_col == rmac->mac_rng.hc)
	{
	  old = rmac->mac_prev;
	  (void) obstack_free (&macro_stack, rmac);
	  rmac = old;
	  goto deal_making;
	}

      if (rmac->mac_row == rmac->mac_rng.hr)
	{
	  rmac->mac_row = rmac->mac_rng.lr;
	  rmac->mac_col++;
	}
      else
	rmac->mac_row++;
      if (!(cp = find_cell (rmac->mac_row, rmac->mac_col)) || GET_TYP (cp) != TYP_STR || cp->cell_str[0] == '\0')
	{
	  old = rmac->mac_prev;
	  (void) obstack_free (&macro_stack, rmac);
	  rmac = old;
	  goto deal_making;
	}
      (void) obstack_grow (&macro_stack, cp->cell_str, 1 + strlen (cp->cell_str));
      rmac->mac_exe = (unsigned char *) obstack_finish (&macro_stack);
      rmac->mac_flags = 0;
    }
deal_making:
  if (!making_macro)
    return;


  making_macro[0] = '\0';
  making_macro = 0;
  if (get_a_range ("Put macro where", &to, &macro_line))
    io_error_msg ("Forgetting new macro");
  else
    {
      z.c_s = (char *) making_macro_start;
      set_new_value (to.lr, to.lc, TYP_STR, &z);
    }
  free (making_macro_start);
}

static void
quit_cmd ()
{
  io_close_display ();
#ifdef FASYNC
  fcntl (0, F_SETFL, term_flag);
#endif
#ifdef FIOSSAIOSTAT
  if ((term_flag & 1) == 0)
    {
      int i = 0;

      ioctl (0, FIOSSAIOSTAT, &i);
    }
  if ((term_flag & 2) == 0)
    {
      int i = 0;

      ioctl (0, FIOSNBIO, &i);
    }
#endif
  exit (0);
}

static void
bound_macro (num)
     int num;
{
  struct macro *old;
  CELL *cp;
  /* CELLREF rr,cc; */

  cp = find_cell (bound_macros[num].lr, bound_macros[num].lc);
  if (!cp || GET_TYP (cp) != TYP_STR || cp->cell_str[0] == '\0')
    return;
  old = rmac;
  rmac = (struct macro *) obstack_alloc (&macro_stack, sizeof (struct macro));
  rmac->mac_prev = old;
  rmac->mac_rng = bound_macros[num];
  rmac->mac_row = bound_macros[num].lr;
  rmac->mac_col = bound_macros[num].lc;
  (void) obstack_grow (&macro_stack, cp->cell_str, 1 + strlen (cp->cell_str));
  rmac->mac_exe = (unsigned char *) obstack_finish (&macro_stack);
  rmac->mac_flags = 0;
}

static void
mark_cell_cmd ()
{
  mkrow = curow;
  mkcol = cucol;
  io_update_status ();
}

static void
unmark_cmd ()
{
  mkrow = NON_ROW;
  mkcol = NON_COL;
  io_update_status ();
}

static void
recalc_cmd ()
{
  current_cycle++;
  while (eval_next_cell ())
    ;
}

static void
desc_key_cmd ()
{
  io_error_msg ("Key:  ");
  map_chr (MAIN_MAP);
  if (!cur_cmd)
    io_info_msg ("%s is unbound", char_to_string (cur_chr));
  else
    io_info_msg ("%s --> %s", char_to_string (cur_chr), cur_cmd->func_name);
}

static void 
bind_key_cmd (text)
     char *text;
{
  int map;
  int c;
  short vec;
  short code;
  char *ptr;
  char *cmdstr;
  struct rng rng;
  struct cmd_func *tmpfunc;
  static struct line tmpline;

  for (ptr = text; *ptr && !isspace (*ptr); ptr++);
  if (!*ptr)
    {
      if (io_get_line ("Command", &tmpline))
	return;
      ptr = tmpline.buf;
    }
  else
    *ptr++ = '\0';
  while (*ptr == ' ')
    ptr++;
  cmdstr = ptr;
  while (*ptr && *ptr != ' ')
    ptr++;
  if (!*ptr)
    c = io_get_chr ("Key:  ");
  else
    {
      *ptr++ = '\0';
      c = string_to_char (&ptr);
      if (c < 0)
	{
	  io_error_msg ("Illegal character '%s'", ptr);
	  goto fail;
	}
    }

  for (map = 0; map < num_maps; map++)
    if (!strcmp (text, map_names[map]))
      break;
  if (map == num_maps)
    {
      io_error_msg ("Can't find keymap '%s'", text);
      goto fail;
    }
  for (vec = 0; vec < num_funcs; vec++)
    {
      for (code = 0; the_funcs[vec][code].func_name; code++)
	if (!stricmp (cmdstr, the_funcs[vec][code].func_name))
	  goto fini;
    }
  for (code = 0; code < num_maps; code++)
    {
      if (!strcmp (cmdstr, map_names[code]))
	{
	  vec = -1;
	  goto fini;
	}
    }
  if (get_abs_rng (&cmdstr, &rng))
    {
      io_error_msg ("Unknown command '%s'", cmdstr);
      goto fail;
    }

  vec = bound_macro_vec;
  for (code = 0; code < n_bound_macros; code++)
    {
      if (!bcmp (&bound_macros[code], &rng, sizeof (struct rng)))
	  goto fini;
    }

  ptr = range_name (&rng);
  if (!bound_macro_vec)
    {
      bound_macros = ck_malloc (sizeof (struct rng));
      n_bound_macros = 1;
      tmpfunc = ck_malloc (sizeof (struct cmd_func));
      bound_macro_vec = add_usr_cmds (tmpfunc);
    }
  else
    {
      n_bound_macros++;
      bound_macros = ck_realloc (bound_macros, n_bound_macros * sizeof (struct rng));
      the_funcs[bound_macro_vec] = ck_realloc (the_funcs[bound_macro_vec], n_bound_macros * sizeof (struct cmd_func));
      tmpfunc = &the_funcs[bound_macro_vec][n_bound_macros - 1];
    }
  bound_macros[n_bound_macros - 1] = rng;
  tmpfunc->func_args = ck_malloc (strlen (ptr) + 5);
  tmpfunc->func_args[0] = 'n';
  tmpfunc->func_args[1] = 'a' + (n_bound_macros - 1) / 26;
  tmpfunc->func_args[2] = 'a' + (n_bound_macros - 1) % 26;
  tmpfunc->func_args[3] = '\0';
  tmpfunc->func_name = tmpfunc->func_args + 4;
  strcpy (tmpfunc->func_name, ptr);
  tmpfunc->func_flags = ALL;
  tmpfunc->func_func = bound_macro;
fini:
  do_bind_key (the_maps[map], c, vec, code);
fail:
  sprint_line (&in_line[16], "%s %s %s", text, cmdstr, char_to_string (c));
}

static void 
do_bind_key (m, key, vector, code)
     struct keymap *m;
     int key;
     int vector;
     int code;
{
  m->keys[key].vector = (short)vector;
  m->keys[key].code = (short)code;
}

static void 
interact_macro_cmd ()
{
  io_error_msg ("Command not implemented");
  return;
}

static void 
kill_cell_cmd ()
{
  CELL *cp;

  cp = find_cell (curow, cucol);
  if (!cp)
    return;
  if ((GET_LCK (cp) == LCK_DEF && default_lock == LCK_LCK) || GET_LCK (cp) == LCK_LCK)
    {
      io_error_msg ("Cell %s is locked", cell_name (curow, cucol));
      return;
    }
  new_value (curow, cucol, "");
  cp->cell_flags = 0;
  cp->cell_font = 0;
  modified = 1;
}

static void
format_cell_cmd ()
{
  struct rng to;

  to.lr = to.hr = curow;
  to.lc = to.hc = cucol;
  format_area (&to);
}

static void
kill_all_cmd ()
{
  clear_spreadsheet ();
  io_repaint ();
}

static void
do_input_cmd (n, c)
     int n;
     int c;
{
  CELL *cp;
  char *fail;

  switch (n)
    {
      /* Edit cell */
    case 0:
      if (cp = find_cell (curow, cucol))
	{
	  set_line (&val_line, decomp (curow, cucol, cp));
	  decomp_free ();
	}
      else
	set_line (&val_line, "");
      break;

      /* Edit Cell Value */
    case 1:
      set_line (&val_line, cell_value_string (curow, cucol));
      break;

    case 2:			/* Set-cell */
      set_line (&val_line, "x");
      val_line.buf[0] = c;
      break;

      /* New Def Cell */
    case 3:
      break;
#ifdef TEST
    default:
      panic ("Unknown value in do_input_cell (%d)", n);
#endif
    }
  cp = find_cell (curow, cucol);
  if (((!cp || GET_LCK (cp) == LCK_DEF) && default_lock == LCK_LCK) || (cp && GET_LCK (cp) == LCK_LCK))
    {
      io_error_msg ("Cell %s is locked", cell_name (curow, cucol));
      return;
    }
  setrow = curow;
  setcol = cucol;
  if (io_get_line ("set-cell", &val_line) > 1)
    return;
  fail = new_value (setrow, setcol, val_line.buf);
  if (fail)
    io_error_msg (fail);
  else
    modified = 1;
}

static void
set_region_formula (str, rng)
     char * str;
     struct rng * rng;
{
  CELLREF row, col;

  for (row = rng->lr; row <= rng->hr; ++row)
    for (col = rng->lc; col <= rng->hc; ++col)
      {
	char * error = new_value (row, col, str);
	if (error)
	  {
	    io_error_msg (error);
	    return;
	  }
	else
	  modified = 1;
      }
}

#if __STDC__
static void
set_cell_formula (char * str, CELLREF row, CELLREF col)
#else
static void
set_cell_formula (str, row, col)
     char * str;
     CELLREF row, col;
#endif
{
  char * error = new_value (row, col, str);
  if (error)
    {
      io_error_msg (error);
      return;
    }
  else
    modified = 1;
}



static void
do_break_cmd ()
{
  if (mkrow != NON_ROW)
    {
      mkrow = NON_ROW;
      how_many = 1;
      io_update_status ();
    }
}

static void 
digit_cmd (magic)
     int magic;
{
  struct keymap *map;

  map = the_maps[DIGIT_MAP];
  map->map_next = the_maps[magic];
  how_many = 0;
  do
    {
      how_many = how_many * 10 + cur_cmd - digit_0_cmd;
      io_update_status ();
      map_chr (DIGIT_MAP);
    }
  while (cur_cmd >= digit_0_cmd && cur_cmd <= digit_9_cmd);
}




static void 
set_area_font_cmd (name, rng)
     char *name;
     struct rng *rng;
{
  struct font_memo *f = intern_font (name);
  set_area_font (rng, f);
}


static void 
set_cell_font_cmd (name)
     char *name;
{
  struct rng to;

  to.lr = to.hr = curow;
  to.lc = to.hc = cucol;
  set_area_font_cmd (name, &to);
}

struct page_size 
{
    char *name;
    float wid;
    float hgt;
};

static struct page_size size_table[] =
{
  { "letter",       612,  792     }, /* (8.5 x 11  in.)   */
  { "tabloid",      792,  1224    }, /* (11 x 17  in.)    */
  { "ledger",       1224, 792     }, /* (17 x 11  in.)    */
  { "legal",        612,  1008    }, /* (8.5 x 14  in.)   */
  { "statement",    396,  612     }, /* (5.5 x 8.5 in.)   */
  { "executive",    540,  720     }, /* (7.5 x 10  in.)   */
  { "a3",           842,  1190    },
  { "a4",           595,  842     },
  { "latex-a4",     523,  770     }, /* A4 - 1in margins all round */
  { "a5",           420,  595     },
  { "b4",           729,  1032    },
  { "b5",           516,  729     },      
  { "folio",        612,  936     }, /* (8.5 x 13  in.)   */
  { "quarto",       610,  780     }
};

#ifdef __STDC__
static struct page_size *
find_size( char * size, int len )
#else
static struct page_size *
find_size( size, len )
     char *size;
     int len;
#endif
{
  int i;
  struct page_size *p = size_table;
  
  for (i = 0;
       i < sizeof(size_table)/sizeof(struct page_size);
       i++, p++)
    if (strincmp (size, p->name, len) == 0 )
      return p;
  return 0;
}

static float default_pswid = 8.5 * 72.;
static float default_pshgt = 11. * 72.;

#ifdef __STDC__
static void
set_page_size_cmd (char * whole_str)
#else
static void
set_page_size_cmd (whole_str)
     char * whole_str;
#endif
{
  char * str = whole_str;
  float neww;
  float newh;
  while (*str && isspace(*str))
    ++str;
  if (!isdigit (*str) && *str != '.')
    {
      char * end = str;
      struct page_size * ps;
      while (*end && !isspace(*end))
	++end;
      ps = find_size (str, end - str);
      if (ps)
	{
	  default_pswid = ps->wid;
	  default_pshgt = ps->hgt;
	  return;
	}
      io_error_msg
	("Bad page size (should look like `8.5 x 11' or `21.6 x 28c'): %s.",
	 whole_str);
      return;
    }
  neww = atof (str);
  while (*str && isdigit(*str))
    ++str;
  if (*str == '.')
    {
      ++str;
      while (isdigit (*str))
	++str;
    }
  while (*str && isspace(*str))
    ++str;
  if (*str == 'x')
    {
      ++str;
      while (*str && isspace(*str))
	++str;
    }
  if (!isdigit (*str) && *str != '.')
    {
      io_error_msg
	("Bad page size (should look like `8.5 x 11' or `21.6 x 28c'): %s.",
	 whole_str);
      return;
    }
  newh = atof (str);
  while (*str && isdigit(*str))
    ++str;
  if (*str == '.')
    {
      ++str;
      while (*str && isdigit (*str))
	++str;
    }
  while (*str && isspace(*str))
    ++str;
  if (*str == 'c')
    {
      neww *= .3937;
      newh *= .3937;
    }
  if (*str != 'p')
    {
      default_pswid = neww * 72;
      default_pshgt = newh * 72;
    }
}

void 
psprint_region_cmd (fp, rng)
     FILE *fp;
     struct rng *rng;
{
  psprint_region (fp, rng, default_pswid, default_pshgt, 0);
}

/* This is a bit kludgey. Input line editting has its own event loop (grr!),
 * and all of its state is private.  These mouse commands can't entirely
 * handle it when the target is in the input line.  In that case, they
 * save the decoded mouse event where io_get_line can pick it up:
 */
struct mouse_event last_mouse_event;

static void
do_mouse_goto ()
{
  if (last_mouse_event.location >= 0 && last_mouse_event.downp)
    {
      nicely_goto_window (last_mouse_event.location);
      io_move_cell_cursor (last_mouse_event.r, last_mouse_event.c);
    }
}

static void
do_mouse_mark ()
{
  if (last_mouse_event.location >= 0 && last_mouse_event.downp)
    {
      mkrow = last_mouse_event.r;
      mkcol = last_mouse_event.c;
    }
}


static void
do_mouse_mark_and_goto ()
{
  if (last_mouse_event.location >= 0 && last_mouse_event.downp)
    {
      mkrow = curow;
      mkcol = cucol;
      nicely_goto_window (last_mouse_event.location);
      io_move_cell_cursor (last_mouse_event.r, last_mouse_event.c);
    }
}

static void
do_mouse_cmd (fn)
     void (*fn) ();
{
  int seq = real_get_chr ();
  dequeue_mouse_event (&last_mouse_event, seq);
  fn ();
}

static void
mouse_mark_cmd ()
{
  do_mouse_cmd (do_mouse_mark);
}


int was_mouse_goto = 0;
static void
mouse_goto_cmd ()
{
  do_mouse_cmd (do_mouse_goto);
  was_mouse_goto = 1;
}

static void
mouse_mark_and_goto_cmd ()
{
  do_mouse_cmd (do_mouse_mark_and_goto);
}
