/* funmap.c -- changed by Bruno Haible, 16 March 1993 */

/* funmap.c -- attach names to functions. */

/* Copyright (C) 1988, 1989, 1991 Free Software Foundation, Inc.

   This file is part of GNU Readline, a library for reading lines
   of text with interactive input and history editing.

   Readline is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   Readline is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* #define STATIC_MALLOC */
#if !defined (STATIC_MALLOC)
extern char *xmalloc (), *xrealloc ();
#else
static char *xmalloc (), *xrealloc ();
#endif /* STATIC_MALLOC */

#include "sysdep.h"
#include <stdio.h>
#include "readline.h"

FUNMAP **funmap = (FUNMAP **)NULL;
static int funmap_size = 0;
static int funmap_entry = 0;

/* After initializing the function map, this is the index of the first
   program specific function. */
int funmap_program_specific_entry_start;

static FUNMAP default_funmap[] = {

  { "abort", (Function *) rl_abort },
  { "accept-line", (Function *) rl_newline },
  { "arrow-key-prefix", (Function *) rl_arrow_keys },
  { "backward-char", (Function *) rl_backward },
  { "backward-delete-char", (Function *) rl_rubout },
  { "backward-kill-line", (Function *) rl_backward_kill_line },
  { "backward-kill-word", (Function *) rl_backward_kill_word },
  { "backward-word", (Function *) rl_backward_word },
  { "beginning-of-history", (Function *) rl_beginning_of_history },
  { "beginning-of-line", (Function *) rl_beg_of_line },
  { "call-last-kbd-macro", (Function *) rl_call_last_kbd_macro },
  { "capitalize-word", (Function *) rl_capitalize_word },
  { "clear-screen", (Function *) rl_clear_screen },
  { "complete", (Function *) rl_complete },
  { "delete-char", (Function *) rl_delete },
  { "digit-argument", (Function *) rl_digit_argument },
  { "do-lowercase-version", (Function *) rl_do_lowercase_version },
  { "downcase-word", (Function *) rl_downcase_word },
  { "dump-functions", (Function *) rl_dump_functions },
  { "end-kbd-macro", (Function *) rl_end_kbd_macro },
  { "end-of-history", (Function *) rl_end_of_history },
  { "end-of-line", (Function *) rl_end_of_line },
  { "forward-char", (Function *) rl_forward },
  { "forward-search-history", (Function *) rl_forward_search_history },
  { "forward-word", (Function *) rl_forward_word },
  { "kill-line", (Function *) rl_kill_line },
  { "kill-word", (Function *) rl_kill_word },
  { "next-history", (Function *) rl_get_next_history },
  { "possible-completions", (Function *) rl_possible_completions },
  { "previous-history", (Function *) rl_get_previous_history },
  { "quoted-insert", (Function *) rl_quoted_insert },
  { "re-read-init-file", (Function *) rl_re_read_init_file },
  { "redraw-current-line", (Function *) rl_refresh_line},
  { "reverse-search-history", (Function *) rl_reverse_search_history },
  { "revert-line", (Function *) rl_revert_line },
  { "self-insert", (Function *) rl_insert },
  { "start-kbd-macro", (Function *) rl_start_kbd_macro },
  { "tab-insert", (Function *) rl_tab_insert },
  { "transpose-chars", (Function *) rl_transpose_chars },
  { "transpose-words", (Function *) rl_transpose_words },
  { "undo", (Function *) rl_undo_command },
  { "universal-argument", (Function *) rl_universal_argument },
  { "unix-line-discard", (Function *) rl_unix_line_discard },
  { "unix-word-rubout", (Function *) rl_unix_word_rubout },
  { "upcase-word", (Function *) rl_upcase_word },
  { "yank", (Function *) rl_yank },
  { "yank-nth-arg", (Function *) rl_yank_nth_arg },
  { "yank-pop", (Function *) rl_yank_pop },

#if defined (VI_MODE)

  { "vi-append-eol", (Function *) rl_vi_append_eol },
  { "vi-append-mode", (Function *) rl_vi_append_mode },
  { "vi-arg-digit", (Function *) rl_vi_arg_digit },
  { "vi-bWord", (Function *) rl_vi_bWord },
  { "vi-bracktype", (Function *) rl_vi_bracktype },
  { "vi-bword", (Function *) rl_vi_bword },
  { "vi-change-case", (Function *) rl_vi_change_case },
  { "vi-change-char", (Function *) rl_vi_change_char },
  { "vi-change-to", (Function *) rl_vi_change_to },
  { "vi-char-search", (Function *) rl_vi_char_search },
  { "vi-column", (Function *) rl_vi_column },
  { "vi-comment", (Function *) rl_vi_comment },
  { "vi-complete", (Function *) rl_vi_complete },
  { "vi-delete", (Function *) rl_vi_delete },
  { "vi-delete-to", (Function *) rl_vi_delete_to },
  { "vi-dosearch", (Function *) rl_vi_dosearch },
  { "vi-eWord", (Function *) rl_vi_eWord },
  { "vi-editing-mode", (Function *) rl_vi_editing_mode },
  { "vi-end-word", (Function *) rl_vi_end_word },
  { "vi-eof-maybe", (Function *) rl_vi_eof_maybe },
  { "vi-eword", (Function *) rl_vi_eword },
  { "vi-fWord", (Function *) rl_vi_fWord },
  { "vi-first-print", (Function *) rl_vi_first_print },
  { "vi-fword", (Function *) rl_vi_fword },
  { "vi-insert-beg", (Function *) rl_vi_insert_beg },
  { "vi-insertion-mode", (Function *) rl_vi_insertion_mode },
  { "vi-match", (Function *) rl_vi_match },
  { "vi-movement-mode", (Function *) rl_vi_movement_mode },
  { "vi-next-word", (Function *) rl_vi_next_word },
  { "vi-overstrike", (Function *) rl_vi_overstrike },
  { "vi-overstrike-delete", (Function *) rl_vi_overstrike_delete },
  { "vi-prev-word", (Function *) rl_vi_prev_word },
  { "vi-put", (Function *) rl_vi_put },
  { "vi-replace", (Function *) rl_vi_replace },
  { "vi-search", (Function *) rl_vi_search },
  { "vi-search-again", (Function *) rl_vi_search_again },
  { "vi-subst", (Function *) rl_vi_subst },
  { "vi-yank-arg", (Function *) rl_vi_yank_arg },
  { "vi-yank-to", (Function *) rl_vi_yank_to },

#endif /* VI_MODE */

 {(char *)NULL, (Function *)NULL }
};

void
rl_add_funmap_entry (name, function)
     char *name;
     Function *function;
{
  if (funmap_entry + 2 >= funmap_size)
    if (!funmap)
      funmap = (FUNMAP **)xmalloc ((funmap_size = 80) * sizeof (FUNMAP *));
    else
      funmap =
	(FUNMAP **)xrealloc (funmap, (funmap_size += 80) * sizeof (FUNMAP *));
  
  funmap[funmap_entry] = (FUNMAP *)xmalloc (sizeof (FUNMAP));
  funmap[funmap_entry]->name = name;
  funmap[funmap_entry]->function = function;

  funmap[++funmap_entry] = (FUNMAP *)NULL;
}

static int funmap_initialized = 0;

/* Make the funmap contain all of the default entries. */
void
rl_initialize_funmap ()
{
  register int i;

  if (funmap_initialized)
    return;

  for (i = 0; default_funmap[i].name; i++)
    rl_add_funmap_entry (default_funmap[i].name, default_funmap[i].function);

  funmap_initialized = 1;
  funmap_program_specific_entry_start = i;
}

/* Stupid comparison routine for qsort () ing strings. */
static int
qsort_string_compare (s1, s2)
     register char **s1, **s2;
{
  return (strcmp (*s1, *s2));
}

/* Produce a NULL terminated array of known function names.  The array
   is sorted.  The array itself is allocated, but not the strings inside.
   You should free () the array when you done, but not the pointrs. */
char **
rl_funmap_names ()
{
  char **result = (char **)NULL;
  int result_size, result_index;

  result_size = result_index = 0;

  /* Make sure that the function map has been initialized. */
  rl_initialize_funmap ();

  for (result_index = 0; funmap[result_index]; result_index++)
    {
      if (result_index + 2 > result_size)
	{
	  if (!result)
	    result = (char **)xmalloc ((result_size = 20) * sizeof (char *));
	  else
	    result = (char **)
	      xrealloc (result, (result_size += 20) * sizeof (char *));
	}

      result[result_index] = funmap[result_index]->name;
      result[result_index + 1] = (char *)NULL;
    }

  qsort (result, result_index, sizeof (char *), qsort_string_compare);
  return (result);
}

/* Things that mean `Control'. */
char *possible_control_prefixes[] = {
  "Control-", "C-", "CTRL-", (char *)NULL
};

char *possible_meta_prefixes[] = {
  "Meta", "M-", (char *)NULL
};

#if defined (STATIC_MALLOC)

/* **************************************************************** */
/*								    */
/*			xmalloc and xrealloc ()		     	    */
/*								    */
/* **************************************************************** */

static void memory_error_and_abort RL((void));

static char *
xmalloc (bytes)
     int bytes;
{
  char *temp = (char *)malloc (bytes);

  if (!temp)
    memory_error_and_abort ();
  return (temp);
}

static char *
xrealloc (pointer, bytes)
     char *pointer;
     int bytes;
{
  char *temp;

  if (!pointer)
    temp = (char *)malloc (bytes);
  else
    temp = (char *)realloc (pointer, bytes);

  if (!temp)
    memory_error_and_abort ();
  return (temp);
}

static void
memory_error_and_abort ()
{
  fprintf (stderr, "history: Out of virtual memory!\n");
  abort ();
}
#endif /* STATIC_MALLOC */
