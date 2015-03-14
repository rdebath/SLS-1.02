/* readline.h -- changed by Bruno Haible, 16 March 1993 */

/* Readline.h -- the names of functions callable from within readline. */

#if !defined (_READLINE_H_)
#define _READLINE_H_

#ifndef RL
/* For prototypes:  extern int foo RL((int x, int y)); */
#ifdef __STDC__
#define RL(args) args
#else
#define RL(args) ()
#endif
#endif

#include "keymaps.h"

#if !defined (__FUNCTION_DEF)
typedef int Function ();
#define __FUNCTION_DEF
#endif /* __FUNCTION_DEF */

/* Maintaining the state of undo.  We remember individual deletes and inserts
   on a chain of things to do. */

/* The actions that undo knows how to undo.  Notice that UNDO_DELETE means
   to insert some text, and UNDO_INSERT means to delete some text.   I.e.,
   the code tells undo what to undo, not how to undo it. */
enum undo_code { UNDO_DELETE, UNDO_INSERT, UNDO_BEGIN, UNDO_END };

/* What an element of THE_UNDO_LIST looks like. */
typedef struct undo_list {
  struct undo_list *next;
  int start, end;		/* Where the change took place. */
  char *text;			/* The text to insert, if undoing a delete. */
  enum undo_code what;		/* Delete, Insert, Begin, End. */
} UNDO_LIST;

/* The current undo list for RL_LINE_BUFFER. */
extern UNDO_LIST *rl_undo_list;

#include "funmap.h"

/* The functions for manipulating the text of the line within readline.
Most of these functions are bound to keys by default. */
extern void rl_digit_argument RL((int ignore, int key));
extern void rl_universal_argument RL((void));
extern int ding RL((void));
extern void rl_abort RL((void));
extern void rl_forward RL((int count));
extern void rl_backward RL((int count));
extern void rl_beg_of_line RL((void));
extern void rl_end_of_line RL((void));
extern void rl_forward_word RL((int count));
extern void rl_backward_word RL((int count));
extern void rl_clear_screen RL((void));
extern void rl_insert RL((int count, int c));
extern void rl_quoted_insert RL((int count));
extern void rl_tab_insert RL((int count));
extern void rl_newline RL((int count, int key));
extern void rl_do_lowercase_version RL((int ignore1, int ignore2));
extern void rl_rubout RL((int count));
extern void rl_delete RL((int count, int invoking_key));
extern void rl_unix_word_rubout RL((void));
extern void rl_unix_line_discard RL((void));
extern void rl_upcase_word RL((int count));
extern void rl_downcase_word RL((int count));
extern void rl_capitalize_word RL((int count));
extern void rl_transpose_words RL((int count));
extern void rl_transpose_chars RL((int count));
extern void rl_restart_output RL((int count, int key));
extern void rl_complete RL((int ignore, int invoking_key));
extern void rl_possible_completions RL((void));
extern void rl_revert_line RL((void));
extern void rl_undo_command RL((int count));
extern void rl_beginning_of_history RL((void));
extern void rl_end_of_history RL((void));
extern void rl_get_next_history RL((int count));
extern void rl_get_previous_history RL((int count));
extern void rl_reverse_search_history RL((int sign, int key));
extern void rl_forward_search_history RL((int sign, int key));
extern void rl_kill_word RL((int count));
extern void rl_backward_kill_word RL((int count));
extern void rl_kill_line RL((int direction));
extern void rl_backward_kill_line RL((int direction));
extern void rl_yank RL((void));
extern void rl_yank_pop RL((void));
extern void rl_yank_nth_arg RL((int count, int ignore));
extern void rl_re_read_init_file RL((int count, int ignore));
extern int rl_dump_functions RL((int count));

/* These are *both* defined even when VI_MODE is not. */
extern void rl_vi_editing_mode RL((void));
extern void rl_emacs_editing_mode RL((void));

#if defined (VI_MODE)
/* Things for vi mode. See vi_mode.c .*/
extern void rl_vi_yank_arg RL((int count));
extern void rl_vi_fetch_history RL((int count, int c));
extern void rl_vi_search_again RL((int ignore, int key));
extern void rl_vi_search RL((int count, int key));
extern void rl_vi_dosearch RL((char* string, int dir));
extern void rl_vi_complete RL((int ignore, int key));
extern void rl_vi_prev_word RL((int count, int key));
extern void rl_vi_next_word RL((int count, int key));
extern void rl_vi_end_word RL((int count, int key));
extern void rl_vi_fWord RL((int count));
extern void rl_vi_bWord RL((int count));
extern void rl_vi_eWord RL((int count));
extern void rl_vi_fword RL((int count));
extern void rl_vi_bword RL((int count));
extern void rl_vi_eword RL((int count));
extern int rl_vi_insert_beg RL((void));
extern int rl_vi_append_mode RL((void));
extern int rl_vi_append_eol RL((void));
extern void rl_vi_eof_maybe RL((int count, int c));
extern void rl_vi_insertion_mode RL((void));
extern void rl_vi_movement_mode RL((void));
extern void rl_vi_arg_digit RL((int count, int c));
extern void rl_vi_change_case RL((int count, int ignore));
extern void rl_vi_put RL((int count, int key));
extern void rl_vi_check RL((void));
extern void rl_vi_column RL((int count));
extern void rl_vi_delete_to RL((int count, int key));
extern void rl_vi_change_to RL((int count, int key));
extern void rl_vi_yank_to RL((int count, int key));
extern void rl_vi_delete RL((int count));
extern void rl_vi_comment RL((void));
extern void rl_vi_first_print RL((void));
extern void rl_vi_char_search RL((int count, int key));
extern void rl_vi_match RL((void));
extern int rl_vi_bracktype RL((int c));
extern void rl_vi_change_char RL((int count, int key));
extern void rl_vi_subst RL((int count, int key));
extern void rl_vi_overstrike RL((int count, int key));
extern void rl_vi_overstrike_delete RL((int count));
extern void rl_vi_replace RL((int count, int key));
extern int rl_vi_possible_completions RL((void));
#endif /* VI_MODE */

/* Keyboard macro commands. */
extern void rl_start_kbd_macro RL((int ignore1, int ignore2));
extern void rl_end_kbd_macro RL((int count, int ignore));
extern void rl_call_last_kbd_macro RL((int count, int ignore));

/* Other functions. */
extern char* readline RL((char* prompt));
extern void rl_stuff_char RL((int key));
extern int rl_read_key RL((void));
extern void rl_dispatch RL((int key, Keymap map));
extern void rl_digit_loop RL((void));
extern void rl_redisplay RL((void));
extern void rl_message RL((char* string, int arg1, int arg2));
extern void rl_clear_message RL((void));
extern void rl_prep_terminal RL((void));
extern void rl_deprep_terminal RL((void));
extern int numeric RL((int c));
extern void rl_extend_line_buffer RL((int len));
extern void rl_insert_text RL((char* string));
extern void rl_refresh_line RL((void));
extern void rl_arrow_keys RL((int count, int c));
extern void rl_complete_internal RL((int what_to_do));
extern char* username_completion_function RL((char* text, int state));
extern int rl_do_undo RL((void));
extern void rl_begin_undo_group RL((void));
extern void rl_end_undo_group RL((void));
extern void rl_modifying RL((int start, int end));
extern void maybe_unsave_line RL((void));
extern void maybe_save_line RL((void));
extern void rl_search_history RL((int direction, int invoking_key));
extern void rl_execute_next RL((int c));
extern void rl_kill_text RL((int from, int to));
extern unsigned char ** completion_matches RL((char* text, char* (*entry_function)()));
extern char* filename_completion_function RL((char* text, int state));
extern void rl_add_defun RL((char* name, Function* function, int key));
extern int rl_bind_key RL((int key, Function* function));
extern int rl_bind_key_in_map RL((int key, Function* function, Keymap map));
extern int rl_unbind_key RL((int key));
extern int rl_unbind_key_in_map RL((int key, Keymap map));
extern void rl_generic_bind RL((int type, char* keyseq, void* data, Keymap map));
extern int rl_getc RL((FILE* stream));

/* **************************************************************** */
/*								    */
/*			Well Published Variables		    */
/*								    */
/* **************************************************************** */

/* Always true. */
extern int rl_present_p;

/* The name of the calling program.  You should initialize this to
   whatever was in argv[0].  It is used when parsing conditionals. */
extern char *rl_readline_name;

/* The line buffer that is in use. */
extern unsigned char *rl_line_buffer;

/* The location of point, and end. */
extern int rl_point, rl_end;

/* The name of the terminal to use. */
extern char *rl_terminal_name;

/* The input and output streams. */
extern FILE *rl_instream, *rl_outstream;

/* The basic list of characters that signal a break between words for the
   completer routine.  The initial contents of this variable is what
   breaks words in the shell, i.e. "n\"\\'`@$>". */
extern char *rl_basic_word_break_characters;

/* The list of characters that signal a break between words for
   rl_complete_internal.  The default list is the contents of
   rl_basic_word_break_characters.  */
extern char *rl_completer_word_break_characters;

/* Basic list of quote characters */
extern char *rl_basic_quote_characters;

/* List of characters which are used to quote a substring of the command
   line, upon which completion is to be performed for the entire substring.
   Within quoted substrings, rl_completer_word_break_characters are treated
   as normal characters, unless they also appear in this list. */
extern char *rl_completer_quote_characters;

/* List of characters that are word break characters, but should be left
   in TEXT when it is passed to the completion function.  The shell uses
   this to help determine what kind of completing to do. */
extern char *rl_special_prefixes;

/* Pointer to the generator function for completion_matches ().
   NULL means to use filename_entry_function (), the default filename
   completer. */
extern Function *rl_completion_entry_function;

/* If rl_ignore_some_completions_function is non-NULL it is the address
   of a function to call after all of the possible matches have been
   generated, but before the actual completion is done to the input line.
   The function is called with one argument; a NULL terminated array
   of (char *).  If your function removes any of the elements, they
   must be free()'ed. */
extern Function *rl_ignore_some_completions_function;

/* Pointer to alternative function to create matches.
   Function is called with TEXT, START, and END.
   START and END are indices in RL_LINE_BUFFER saying what the boundaries
   of TEXT are.
   If this function exists and returns NULL then call the value of
   rl_completion_entry_function to try to match, otherwise use the
   array of strings returned. */
extern Function *rl_attempted_completion_function;

/* If non-zero, then this is the address of a function to call just
   before readline_internal () prints the first prompt. */
extern Function *rl_startup_hook;

/* If non-zero, indicates that the caller of readline() has already
   output the prompt. */
extern int rl_already_prompted;

/* If non-zero, then this is the address of a function to call when
   completing on a directory name.  The function is called with
   the address of a string (the current directory name) as an arg. */
extern Function *rl_symbolic_link_hook;

/* If non-zero then this is the address of a function you want called
   while Readline is waiting for character input.     */
extern Function *rl_event_hook;

/* Non-zero means that modified history lines are preceded
   with an asterisk. */
extern int rl_show_star;


/* **************************************************************** */
/*                                                                  */
/*             Tilde Variables That Can be Externally Set           */
/*                                                                  */
/* **************************************************************** */

/* If non-null, this contains the address of a function to call if the
   standard meaning for expanding a tilde fails.  The function is called
   with the text (sans tilde, as in "foo"), and returns a malloc()'ed string
   which is the expansion, or a NULL pointer if there is no expansion. */
extern Function *tilde_expansion_failure_hook;

/* When non-null, this is a NULL terminated array of strings which
   are duplicates for a tilde prefix.  Bash uses this to expand
   `=~' and `:~'. */
extern char **tilde_additional_prefixes;

/* When non-null, this is a NULL terminated array of strings which match
   the end of a username, instead of just "/".  Bash sets this to
   `/' and `:'. */
extern char **tilde_additional_suffixes;

/* **************************************************************** */
/*								    */
/*			Well Published Functions		    */
/*								    */
/* **************************************************************** */

/* Read a line of input.  Prompt with PROMPT.  A NULL PROMPT means none. */
extern char *readline RL((char* prompt));

/* Put the terminal in CBREAK mode so that we can detect key presses. */
extern void rl_prep_terminal RL((void));

/* Restore the terminal to its original state. */
extern void rl_deprep_terminal RL((void));

/* Return an array of strings which are the result of repeatadly calling
   FUNC with TEXT. */
extern unsigned char **completion_matches RL((char* text, char* (*entry_function)()));

/* rl_add_defun (char *name, Function *function, int key)
   Add NAME to the list of named functions.  Make FUNCTION
   be the function that gets called.
   If KEY is not -1, then bind it. */
extern void rl_add_defun RL((char* name, Function* function, int key));

#endif /* _READLINE_H_ */
