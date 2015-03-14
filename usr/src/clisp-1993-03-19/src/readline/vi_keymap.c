/* vi_keymap.c -- changed by Bruno Haible, 16 March 1993 */

/* vi_keymap.c -- the keymap for vi_mode in readline (). */

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

extern KEYMAP_ENTRY_ARRAY vi_escape_keymap;

/* The keymap arrays for handling vi mode. */
KEYMAP_ENTRY_ARRAY vi_movement_keymap = {

  /* The regular control keys come first. */
  { ISFUNC, (Function *)0x0 },				/* Control-@ */
  { ISFUNC, (Function *)0x0 },				/* Control-a */
  { ISFUNC, (Function *)0x0 },				/* Control-b */
  { ISFUNC, (Function *)0x0 },				/* Control-c */
  { ISFUNC, (Function *) rl_vi_eof_maybe },		/* Control-d */
  { ISFUNC, (Function *) rl_emacs_editing_mode },	/* Control-e */
  { ISFUNC, (Function *)0x0 },				/* Control-f */
  { ISFUNC, (Function *) rl_abort },			/* Control-g */
  { ISFUNC, (Function *) rl_rubout },			/* Control-h */
  { ISFUNC, (Function *)0x0 },				/* Control-i */
  { ISFUNC, (Function *) rl_newline },			/* Control-j */
  { ISFUNC, (Function *) rl_kill_line },		/* Control-k */
  { ISFUNC, (Function *) rl_clear_screen },		/* Control-l */
  { ISFUNC, (Function *) rl_newline },			/* Control-m */
  { ISFUNC, (Function *) rl_get_next_history },		/* Control-n */
  { ISFUNC, (Function *)0x0 },				/* Control-o */
  { ISFUNC, (Function *) rl_get_previous_history },	/* Control-p */
  { ISFUNC, (Function *) rl_quoted_insert },		/* Control-q */
  { ISFUNC, (Function *) rl_reverse_search_history },	/* Control-r */
  { ISFUNC, (Function *) rl_forward_search_history },	/* Control-s */
  { ISFUNC, (Function *) rl_transpose_chars },		/* Control-t */
  { ISFUNC, (Function *) rl_unix_line_discard },	/* Control-u */
  { ISFUNC, (Function *) rl_quoted_insert },		/* Control-v */
  { ISFUNC, (Function *) rl_unix_word_rubout },		/* Control-w */
  { ISFUNC, (Function *)0x0 },				/* Control-x */
  { ISFUNC, (Function *) rl_yank },			/* Control-y */
  { ISFUNC, (Function *)0x0 },				/* Control-z */

  { ISKMAP, (Function *)vi_escape_keymap },		/* Control-[ */
  { ISFUNC, (Function *)0x0 },				/* Control-\ */
  { ISFUNC, (Function *)0x0 },				/* Control-] */
  { ISFUNC, (Function *)0x0 },				/* Control-^ */
  { ISFUNC, (Function *) rl_undo_command },		/* Control-_ */

  /* The start of printing characters. */
  { ISFUNC, (Function *) rl_forward },			/* SPACE */
  { ISFUNC, (Function *)0x0 },				/* ! */
  { ISFUNC, (Function *)0x0 },				/* " */
  { ISFUNC, (Function *) rl_vi_comment },		/* # */
  { ISFUNC, (Function *) rl_end_of_line },		/* $ */
  { ISFUNC, (Function *) rl_vi_match },			/* % */
  { ISFUNC, (Function *)0x0 },				/* & */
  { ISFUNC, (Function *)0x0 },				/* ' */
  { ISFUNC, (Function *)0x0 },				/* ( */
  { ISFUNC, (Function *)0x0 },				/* ) */
  { ISFUNC, (Function *) rl_vi_complete },		/* * */
  { ISFUNC, (Function *) rl_get_next_history},		/* + */
  { ISFUNC, (Function *) rl_vi_char_search },		/* , */
  { ISFUNC, (Function *) rl_get_previous_history },	/* - */
  { ISFUNC, (Function *)0x0 },				/* . */
  { ISFUNC, (Function *) rl_vi_search },		/* / */

  /* Regular digits. */
  { ISFUNC, (Function *) rl_vi_arg_digit },		/* 0 */
  { ISFUNC, (Function *) rl_vi_arg_digit },		/* 1 */
  { ISFUNC, (Function *) rl_vi_arg_digit },		/* 2 */
  { ISFUNC, (Function *) rl_vi_arg_digit },		/* 3 */
  { ISFUNC, (Function *) rl_vi_arg_digit },		/* 4 */
  { ISFUNC, (Function *) rl_vi_arg_digit },		/* 5 */
  { ISFUNC, (Function *) rl_vi_arg_digit },		/* 6 */
  { ISFUNC, (Function *) rl_vi_arg_digit },		/* 7 */
  { ISFUNC, (Function *) rl_vi_arg_digit },		/* 8 */
  { ISFUNC, (Function *) rl_vi_arg_digit },		/* 9 */

  /* A little more punctuation. */
  { ISFUNC, (Function *)0x0 },				/* : */
  { ISFUNC, (Function *) rl_vi_char_search },		/* ; */
  { ISFUNC, (Function *)0x0 },				/* < */
  { ISFUNC, (Function *) rl_vi_complete },		/* = */
  { ISFUNC, (Function *)0x0 },				/* > */
  { ISFUNC, (Function *) rl_vi_search },		/* ? */
  { ISFUNC, (Function *)0x0 },				/* @ */

  /* Uppercase alphabet. */
  { ISFUNC, (Function *) rl_vi_append_eol },		/* A */
  { ISFUNC, (Function *) rl_vi_prev_word},		/* B */
  { ISFUNC, (Function *) rl_vi_change_to },		/* C */
  { ISFUNC, (Function *) rl_vi_delete_to },		/* D */
  { ISFUNC, (Function *) rl_vi_end_word },		/* E */
  { ISFUNC, (Function *) rl_vi_char_search },		/* F */
  { ISFUNC, (Function *) rl_vi_fetch_history },		/* G */
  { ISFUNC, (Function *)0x0 },				/* H */
  { ISFUNC, (Function *) rl_vi_insert_beg },		/* I */
  { ISFUNC, (Function *)0x0 },				/* J */
  { ISFUNC, (Function *)0x0 },				/* K */
  { ISFUNC, (Function *)0x0 },				/* L */
  { ISFUNC, (Function *)0x0 },				/* M */
  { ISFUNC, (Function *) rl_vi_search_again },		/* N */
  { ISFUNC, (Function *)0x0 },				/* O */
  { ISFUNC, (Function *) rl_vi_put },			/* P */
  { ISFUNC, (Function *)0x0 },				/* Q */
  { ISFUNC, (Function *) rl_vi_replace },		/* R */
  { ISFUNC, (Function *) rl_vi_subst },			/* S */
  { ISFUNC, (Function *) rl_vi_char_search },		/* T */
  { ISFUNC, (Function *) rl_revert_line },		/* U */
  { ISFUNC, (Function *)0x0 },				/* V */
  { ISFUNC, (Function *) rl_vi_next_word },		/* W */
  { ISFUNC, (Function *) rl_rubout },			/* X */
  { ISFUNC, (Function *) rl_vi_yank_to },		/* Y */
  { ISFUNC, (Function *)0x0 },				/* Z */

  /* Some more punctuation. */
  { ISFUNC, (Function *)0x0 },			/* [ */
  { ISFUNC, (Function *) rl_vi_complete },	/* \ */
  { ISFUNC, (Function *)0x0 },			/* ] */
  { ISFUNC, (Function *) rl_vi_first_print },	/* ^ */
  { ISFUNC, (Function *) rl_vi_yank_arg },	/* _ */
  { ISFUNC, (Function *)0x0 },			/* ` */

  /* Lowercase alphabet. */
  { ISFUNC, (Function *) rl_vi_append_mode },		/* a */
  { ISFUNC, (Function *) rl_vi_prev_word },		/* b */
  { ISFUNC, (Function *) rl_vi_change_to },		/* c */
  { ISFUNC, (Function *) rl_vi_delete_to },		/* d */
  { ISFUNC, (Function *) rl_vi_end_word },		/* e */
  { ISFUNC, (Function *) rl_vi_char_search },		/* f */
  { ISFUNC, (Function *)0x0 },				/* g */
  { ISFUNC, (Function *) rl_backward },			/* h */
  { ISFUNC, (Function *) rl_vi_insertion_mode },	/* i */
  { ISFUNC, (Function *) rl_get_next_history },		/* j */
  { ISFUNC, (Function *) rl_get_previous_history },	/* k */
  { ISFUNC, (Function *) rl_forward },			/* l */
  { ISFUNC, (Function *)0x0 },				/* m */
  { ISFUNC, (Function *) rl_vi_search_again },		/* n */
  { ISFUNC, (Function *)0x0 },				/* o */
  { ISFUNC, (Function *) rl_vi_put },			/* p */
  { ISFUNC, (Function *)0x0 },				/* q */
  { ISFUNC, (Function *) rl_vi_change_char },		/* r */
  { ISFUNC, (Function *) rl_vi_subst },			/* s */
  { ISFUNC, (Function *) rl_vi_char_search },		/* t */
  { ISFUNC, (Function *) rl_undo_command },		/* u */
  { ISFUNC, (Function *)0x0 },				/* v */
  { ISFUNC, (Function *) rl_vi_next_word },		/* w */
  { ISFUNC, (Function *) rl_vi_delete },		/* x */
  { ISFUNC, (Function *) rl_vi_yank_to },		/* y */
  { ISFUNC, (Function *)0x0 },				/* z */

  /* Final punctuation. */
  { ISFUNC, (Function *)0x0 },			/* { */
  { ISFUNC, (Function *) rl_vi_column },	/* | */
  { ISFUNC, (Function *)0x0 },			/* } */
  { ISFUNC, (Function *) rl_vi_change_case },	/* ~ */
  { ISFUNC, (Function *) rl_backward },		/* RUBOUT */

#if NUMCHARS==256
  /* Latin-1 or DOS characters */
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
#endif

};


KEYMAP_ENTRY_ARRAY vi_insertion_keymap = {

  /* The regular control keys come first. */
  { ISFUNC, (Function *)0x0 },			/* Control-@ */
  { ISFUNC, (Function *) rl_insert },		/* Control-a */
  { ISFUNC, (Function *) rl_insert },		/* Control-b */
  { ISFUNC, (Function *) rl_insert },		/* Control-c */
  { ISFUNC, (Function *) rl_vi_eof_maybe },	/* Control-d */
  { ISFUNC, (Function *) rl_insert },		/* Control-e */
  { ISFUNC, (Function *) rl_insert },		/* Control-f */
  { ISFUNC, (Function *) rl_insert },		/* Control-g */
  { ISFUNC, (Function *) rl_rubout },		/* Control-h */
  { ISFUNC, (Function *) rl_complete },		/* Control-i */
  { ISFUNC, (Function *) rl_newline },		/* Control-j */
  { ISFUNC, (Function *) rl_insert },		/* Control-k */
  { ISFUNC, (Function *) rl_insert },		/* Control-l */
  { ISFUNC, (Function *) rl_newline },		/* Control-m */
  { ISFUNC, (Function *) rl_insert },		/* Control-n */
  { ISFUNC, (Function *) rl_insert },		/* Control-o */
  { ISFUNC, (Function *) rl_insert },		/* Control-p */
  { ISFUNC, (Function *) rl_insert },		/* Control-q */
  { ISFUNC, (Function *) rl_reverse_search_history }, /* Control-r */
  { ISFUNC, (Function *) rl_forward_search_history }, /* Control-s */
  { ISFUNC, (Function *) rl_transpose_chars },	/* Control-t */
  { ISFUNC, (Function *) rl_unix_line_discard },/* Control-u */
  { ISFUNC, (Function *) rl_quoted_insert },	/* Control-v */
  { ISFUNC, (Function *) rl_unix_word_rubout },	/* Control-w */
  { ISFUNC, (Function *) rl_insert },		/* Control-x */
  { ISFUNC, (Function *) rl_yank },		/* Control-y */
  { ISFUNC, (Function *) rl_insert },		/* Control-z */

  { ISFUNC, (Function *) rl_vi_movement_mode },	/* Control-[ */
  { ISFUNC, (Function *) rl_insert },		/* Control-\ */
  { ISFUNC, (Function *) rl_insert },		/* Control-] */
  { ISFUNC, (Function *) rl_insert },		/* Control-^ */
  { ISFUNC, (Function *) rl_undo_command },	/* Control-_ */

  /* The start of printing characters. */
  { ISFUNC, (Function *) rl_insert },		/* SPACE */
  { ISFUNC, (Function *) rl_insert },		/* ! */
  { ISFUNC, (Function *) rl_insert },		/* " */
  { ISFUNC, (Function *) rl_insert },		/* # */
  { ISFUNC, (Function *) rl_insert },		/* $ */
  { ISFUNC, (Function *) rl_insert },		/* % */
  { ISFUNC, (Function *) rl_insert },		/* & */
  { ISFUNC, (Function *) rl_insert },		/* ' */
  { ISFUNC, (Function *) rl_insert },		/* ( */
  { ISFUNC, (Function *) rl_insert },		/* ) */
  { ISFUNC, (Function *) rl_insert },		/* * */
  { ISFUNC, (Function *) rl_insert },		/* + */
  { ISFUNC, (Function *) rl_insert },		/* , */
  { ISFUNC, (Function *) rl_insert },		/* - */
  { ISFUNC, (Function *) rl_insert },		/* . */
  { ISFUNC, (Function *) rl_insert },		/* / */

  /* Regular digits. */
  { ISFUNC, (Function *) rl_insert },		/* 0 */
  { ISFUNC, (Function *) rl_insert },		/* 1 */
  { ISFUNC, (Function *) rl_insert },		/* 2 */
  { ISFUNC, (Function *) rl_insert },		/* 3 */
  { ISFUNC, (Function *) rl_insert },		/* 4 */
  { ISFUNC, (Function *) rl_insert },		/* 5 */
  { ISFUNC, (Function *) rl_insert },		/* 6 */
  { ISFUNC, (Function *) rl_insert },		/* 7 */
  { ISFUNC, (Function *) rl_insert },		/* 8 */
  { ISFUNC, (Function *) rl_insert },		/* 9 */

  /* A little more punctuation. */
  { ISFUNC, (Function *) rl_insert },		/* : */
  { ISFUNC, (Function *) rl_insert },		/* ; */
  { ISFUNC, (Function *) rl_insert },		/* < */
  { ISFUNC, (Function *) rl_insert },		/* = */
  { ISFUNC, (Function *) rl_insert },		/* > */
  { ISFUNC, (Function *) rl_insert },		/* ? */
  { ISFUNC, (Function *) rl_insert },		/* @ */

  /* Uppercase alphabet. */
  { ISFUNC, (Function *) rl_insert },		/* A */
  { ISFUNC, (Function *) rl_insert },		/* B */
  { ISFUNC, (Function *) rl_insert },		/* C */
  { ISFUNC, (Function *) rl_insert },		/* D */
  { ISFUNC, (Function *) rl_insert },		/* E */
  { ISFUNC, (Function *) rl_insert },		/* F */
  { ISFUNC, (Function *) rl_insert },		/* G */
  { ISFUNC, (Function *) rl_insert },		/* H */
  { ISFUNC, (Function *) rl_insert },		/* I */
  { ISFUNC, (Function *) rl_insert },		/* J */
  { ISFUNC, (Function *) rl_insert },		/* K */
  { ISFUNC, (Function *) rl_insert },		/* L */
  { ISFUNC, (Function *) rl_insert },		/* M */
  { ISFUNC, (Function *) rl_insert },		/* N */
  { ISFUNC, (Function *) rl_insert },		/* O */
  { ISFUNC, (Function *) rl_insert },		/* P */
  { ISFUNC, (Function *) rl_insert },		/* Q */
  { ISFUNC, (Function *) rl_insert },		/* R */
  { ISFUNC, (Function *) rl_insert },		/* S */
  { ISFUNC, (Function *) rl_insert },		/* T */
  { ISFUNC, (Function *) rl_insert },		/* U */
  { ISFUNC, (Function *) rl_insert },		/* V */
  { ISFUNC, (Function *) rl_insert },		/* W */
  { ISFUNC, (Function *) rl_insert },		/* X */
  { ISFUNC, (Function *) rl_insert },		/* Y */
  { ISFUNC, (Function *) rl_insert },		/* Z */

  /* Some more punctuation. */
  { ISFUNC, (Function *) rl_insert },		/* [ */
  { ISFUNC, (Function *) rl_insert },		/* \ */
  { ISFUNC, (Function *) rl_insert },		/* ] */
  { ISFUNC, (Function *) rl_insert },		/* ^ */
  { ISFUNC, (Function *) rl_insert },		/* _ */
  { ISFUNC, (Function *) rl_insert },		/* ` */

  /* Lowercase alphabet. */
  { ISFUNC, (Function *) rl_insert },		/* a */
  { ISFUNC, (Function *) rl_insert },		/* b */
  { ISFUNC, (Function *) rl_insert },		/* c */
  { ISFUNC, (Function *) rl_insert },		/* d */
  { ISFUNC, (Function *) rl_insert },		/* e */
  { ISFUNC, (Function *) rl_insert },		/* f */
  { ISFUNC, (Function *) rl_insert },		/* g */
  { ISFUNC, (Function *) rl_insert },		/* h */
  { ISFUNC, (Function *) rl_insert },		/* i */
  { ISFUNC, (Function *) rl_insert },		/* j */
  { ISFUNC, (Function *) rl_insert },		/* k */
  { ISFUNC, (Function *) rl_insert },		/* l */
  { ISFUNC, (Function *) rl_insert },		/* m */
  { ISFUNC, (Function *) rl_insert },		/* n */
  { ISFUNC, (Function *) rl_insert },		/* o */
  { ISFUNC, (Function *) rl_insert },		/* p */
  { ISFUNC, (Function *) rl_insert },		/* q */
  { ISFUNC, (Function *) rl_insert },		/* r */
  { ISFUNC, (Function *) rl_insert },		/* s */
  { ISFUNC, (Function *) rl_insert },		/* t */
  { ISFUNC, (Function *) rl_insert },		/* u */
  { ISFUNC, (Function *) rl_insert },		/* v */
  { ISFUNC, (Function *) rl_insert },		/* w */
  { ISFUNC, (Function *) rl_insert },		/* x */
  { ISFUNC, (Function *) rl_insert },		/* y */
  { ISFUNC, (Function *) rl_insert },		/* z */

  /* Final punctuation. */
  { ISFUNC, (Function *) rl_insert },		/* { */
  { ISFUNC, (Function *) rl_insert },		/* | */
  { ISFUNC, (Function *) rl_insert },		/* } */
  { ISFUNC, (Function *) rl_insert },		/* ~ */
  { ISFUNC, (Function *) rl_rubout },		/* RUBOUT */

#ifdef ISOLATIN
  /* Latin-1 characters */
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *) rl_insert },	/* nobreakspace */
  { ISFUNC, (Function *) rl_insert },	/* inv. exclamation */
  { ISFUNC, (Function *) rl_insert },	/* U.S. cent */
  { ISFUNC, (Function *) rl_insert },	/* British pound */
  { ISFUNC, (Function *) rl_insert },	/* general currency */
  { ISFUNC, (Function *) rl_insert },	/* Japanese yen */
  { ISFUNC, (Function *) rl_insert },	/* broken bar */
  { ISFUNC, (Function *) rl_insert },	/* section */
  { ISFUNC, (Function *) rl_insert },	/* umlaut accent */
  { ISFUNC, (Function *) rl_insert },	/* copyright */
  { ISFUNC, (Function *) rl_insert },	/* femin. ordinal */
  { ISFUNC, (Function *) rl_insert },	/* open guillemets */
  { ISFUNC, (Function *) rl_insert },	/* not sign */
  { ISFUNC, (Function *) rl_insert },	/* hyphen */
  { ISFUNC, (Function *) rl_insert },	/* registered trade */
  { ISFUNC, (Function *) rl_insert },	/* macron */
  { ISFUNC, (Function *) rl_insert },	/* degree */
  { ISFUNC, (Function *) rl_insert },	/* plus/minus */
  { ISFUNC, (Function *) rl_insert },	/* power 2 */
  { ISFUNC, (Function *) rl_insert },	/* power 3 */
  { ISFUNC, (Function *) rl_insert },	/* accent acute */
  { ISFUNC, (Function *) rl_insert },	/* Greek mu */
  { ISFUNC, (Function *) rl_insert },	/* paragraph */
  { ISFUNC, (Function *) rl_insert },	/* middle dot */
  { ISFUNC, (Function *) rl_insert },	/* cedilla */
  { ISFUNC, (Function *) rl_insert },	/* power 1 */
  { ISFUNC, (Function *) rl_insert },	/* masc. ordinal */
  { ISFUNC, (Function *) rl_insert },	/* close guillemets */
  { ISFUNC, (Function *) rl_insert },	/* one fourth */
  { ISFUNC, (Function *) rl_insert },	/* one half */
  { ISFUNC, (Function *) rl_insert },	/* three fourth */
  { ISFUNC, (Function *) rl_insert },	/* inv. question */
  { ISFUNC, (Function *) rl_insert },	/* A accent grave */
  { ISFUNC, (Function *) rl_insert },	/* A accent acute */
  { ISFUNC, (Function *) rl_insert },	/* A circumflex */
  { ISFUNC, (Function *) rl_insert },	/* A tilde */
  { ISFUNC, (Function *) rl_insert },	/* A umlaut */
  { ISFUNC, (Function *) rl_insert },	/* A degree */
  { ISFUNC, (Function *) rl_insert },	/* AE ligature */
  { ISFUNC, (Function *) rl_insert },	/* C cedilla */
  { ISFUNC, (Function *) rl_insert },	/* E accent grave */
  { ISFUNC, (Function *) rl_insert },	/* E accent acute */
  { ISFUNC, (Function *) rl_insert },	/* E circumflex */
  { ISFUNC, (Function *) rl_insert },	/* E umlaut */
  { ISFUNC, (Function *) rl_insert },	/* I accent grave */
  { ISFUNC, (Function *) rl_insert },	/* I accent acute */
  { ISFUNC, (Function *) rl_insert },	/* I circumflex */
  { ISFUNC, (Function *) rl_insert },	/* I umlaut */
  { ISFUNC, (Function *) rl_insert },	/* D stroke */
  { ISFUNC, (Function *) rl_insert },	/* N tilde */
  { ISFUNC, (Function *) rl_insert },	/* O accent grave */
  { ISFUNC, (Function *) rl_insert },	/* O accent acute */
  { ISFUNC, (Function *) rl_insert },	/* O circumflex */
  { ISFUNC, (Function *) rl_insert },	/* O tilde */
  { ISFUNC, (Function *) rl_insert },	/* O umlaut */
  { ISFUNC, (Function *) rl_insert },	/* multiply */
  { ISFUNC, (Function *) rl_insert },	/* O crossbar */
  { ISFUNC, (Function *) rl_insert },	/* U accent grave */
  { ISFUNC, (Function *) rl_insert },	/* U accent acute */
  { ISFUNC, (Function *) rl_insert },	/* U circumflex */
  { ISFUNC, (Function *) rl_insert },	/* U umlaut */
  { ISFUNC, (Function *) rl_insert },	/* Y accent acute */
  { ISFUNC, (Function *) rl_insert },	/* Thorn */
  { ISFUNC, (Function *) rl_insert },	/* sharp s */
  { ISFUNC, (Function *) rl_insert },	/* a accent grave */
  { ISFUNC, (Function *) rl_insert },	/* a accent acute */
  { ISFUNC, (Function *) rl_insert },	/* a circumflex */
  { ISFUNC, (Function *) rl_insert },	/* a tilde */
  { ISFUNC, (Function *) rl_insert },	/* a umlaut */
  { ISFUNC, (Function *) rl_insert },	/* a degree */
  { ISFUNC, (Function *) rl_insert },	/* ae ligature */
  { ISFUNC, (Function *) rl_insert },	/* c cedilla */
  { ISFUNC, (Function *) rl_insert },	/* e accent grave */
  { ISFUNC, (Function *) rl_insert },	/* e accent acute */
  { ISFUNC, (Function *) rl_insert },	/* e circumflex */
  { ISFUNC, (Function *) rl_insert },	/* e umlaut */
  { ISFUNC, (Function *) rl_insert },	/* i accent grave */
  { ISFUNC, (Function *) rl_insert },	/* i accent acute */
  { ISFUNC, (Function *) rl_insert },	/* i circumflex */
  { ISFUNC, (Function *) rl_insert },	/* i umlaut */
  { ISFUNC, (Function *) rl_insert },	/* d stroke */
  { ISFUNC, (Function *) rl_insert },	/* n tilde */
  { ISFUNC, (Function *) rl_insert },	/* o accent grave */
  { ISFUNC, (Function *) rl_insert },	/* o accent acute */
  { ISFUNC, (Function *) rl_insert },	/* o circumflex */
  { ISFUNC, (Function *) rl_insert },	/* o tilde */
  { ISFUNC, (Function *) rl_insert },	/* o umlaut */
  { ISFUNC, (Function *) rl_insert },	/* divide */
  { ISFUNC, (Function *) rl_insert },	/* o crossbar */
  { ISFUNC, (Function *) rl_insert },	/* u accent grave */
  { ISFUNC, (Function *) rl_insert },	/* u accent acute */
  { ISFUNC, (Function *) rl_insert },	/* u circumflex */
  { ISFUNC, (Function *) rl_insert },	/* u umlaut */
  { ISFUNC, (Function *) rl_insert },	/* y accent acute */
  { ISFUNC, (Function *) rl_insert },	/* thorn */
  { ISFUNC, (Function *) rl_insert },	/* y umlaut */
#endif

#ifdef DOSCHARS
  /* DOS characters */
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
  { ISFUNC, (Function *) rl_insert },
#endif

};

KEYMAP_ENTRY_ARRAY vi_escape_keymap = {

  /* The regular control keys come first. */
  { ISFUNC, (Function *)0x0 },			/* Control-@ */
  { ISFUNC, (Function *)0x0 },			/* Control-a */
  { ISFUNC, (Function *)0x0 },			/* Control-b */
  { ISFUNC, (Function *)0x0 },			/* Control-c */
  { ISFUNC, (Function *)0x0 },			/* Control-d */
  { ISFUNC, (Function *)0x0 },			/* Control-e */
  { ISFUNC, (Function *)0x0 },			/* Control-f */
  { ISFUNC, (Function *)0x0 },			/* Control-g */
  { ISFUNC, (Function *)0x0 },			/* Control-h */
  { ISFUNC, (Function *) rl_tab_insert},	/* Control-i */
  { ISFUNC, (Function *) rl_emacs_editing_mode},/* Control-j */
  { ISFUNC, (Function *) rl_kill_line },	/* Control-k */
  { ISFUNC, (Function *)0x0 },			/* Control-l */
  { ISFUNC, (Function *) rl_emacs_editing_mode},/* Control-m */
  { ISFUNC, (Function *)0x0 },			/* Control-n */
  { ISFUNC, (Function *)0x0 },			/* Control-o */
  { ISFUNC, (Function *)0x0 },			/* Control-p */
  { ISFUNC, (Function *)0x0 },			/* Control-q */
  { ISFUNC, (Function *)0x0 },			/* Control-r */
  { ISFUNC, (Function *)0x0 },			/* Control-s */
  { ISFUNC, (Function *)0x0 },			/* Control-t */
  { ISFUNC, (Function *)0x0 },			/* Control-u */
  { ISFUNC, (Function *)0x0 },			/* Control-v */
  { ISFUNC, (Function *)0x0 },			/* Control-w */
  { ISFUNC, (Function *)0x0 },			/* Control-x */
  { ISFUNC, (Function *)0x0 },			/* Control-y */
  { ISFUNC, (Function *)0x0 },			/* Control-z */

  { ISFUNC, (Function *) rl_vi_movement_mode },	/* Control-[ */
  { ISFUNC, (Function *)0x0 },			/* Control-\ */
  { ISFUNC, (Function *)0x0 },			/* Control-] */
  { ISFUNC, (Function *)0x0 },			/* Control-^ */
  { ISFUNC, (Function *) rl_undo_command },	/* Control-_ */

  /* The start of printing characters. */
  { ISFUNC, (Function *)0x0 },		/* SPACE */
  { ISFUNC, (Function *)0x0 },		/* ! */
  { ISFUNC, (Function *)0x0 },		/* " */
  { ISFUNC, (Function *)0x0 },		/* # */
  { ISFUNC, (Function *)0x0 },		/* $ */
  { ISFUNC, (Function *)0x0 },		/* % */
  { ISFUNC, (Function *)0x0 },		/* & */
  { ISFUNC, (Function *)0x0 },		/* ' */
  { ISFUNC, (Function *)0x0 },		/* ( */
  { ISFUNC, (Function *)0x0 },		/* ) */
  { ISFUNC, (Function *)0x0 },		/* * */
  { ISFUNC, (Function *)0x0 },		/* + */
  { ISFUNC, (Function *)0x0 },		/* , */
  { ISFUNC, (Function *)0x0 },		/* - */
  { ISFUNC, (Function *)0x0 },		/* . */
  { ISFUNC, (Function *)0x0 },		/* / */

  /* Regular digits. */
  { ISFUNC, (Function *) rl_vi_arg_digit },	/* 0 */
  { ISFUNC, (Function *) rl_vi_arg_digit },	/* 1 */
  { ISFUNC, (Function *) rl_vi_arg_digit },	/* 2 */
  { ISFUNC, (Function *) rl_vi_arg_digit },	/* 3 */
  { ISFUNC, (Function *) rl_vi_arg_digit },	/* 4 */
  { ISFUNC, (Function *) rl_vi_arg_digit },	/* 5 */
  { ISFUNC, (Function *) rl_vi_arg_digit },	/* 6 */
  { ISFUNC, (Function *) rl_vi_arg_digit },	/* 7 */
  { ISFUNC, (Function *) rl_vi_arg_digit },	/* 8 */
  { ISFUNC, (Function *) rl_vi_arg_digit },	/* 9 */

  /* A little more punctuation. */
  { ISFUNC, (Function *)0x0 },		/* : */
  { ISFUNC, (Function *)0x0 },		/* ; */
  { ISFUNC, (Function *)0x0 },		/* < */
  { ISFUNC, (Function *)0x0 },		/* = */
  { ISFUNC, (Function *)0x0 },		/* > */
  { ISFUNC, (Function *)0x0 },		/* ? */
  { ISFUNC, (Function *)0x0 },		/* @ */

  /* Uppercase alphabet. */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* A */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* B */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* C */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* D */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* E */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* F */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* G */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* H */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* I */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* J */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* K */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* L */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* M */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* N */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* O */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* P */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* Q */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* R */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* S */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* T */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* U */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* V */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* W */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* X */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* Y */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* Z */

  /* Some more punctuation. */
  { ISFUNC, (Function *)0x0 },		/* [ */
  { ISFUNC, (Function *)0x0 },		/* \ */
  { ISFUNC, (Function *)0x0 },		/* ] */
  { ISFUNC, (Function *)0x0 },		/* ^ */
  { ISFUNC, (Function *)0x0 },		/* _ */
  { ISFUNC, (Function *)0x0 },		/* ` */

  /* Lowercase alphabet. */
  { ISFUNC, (Function *)0x0 },		/* a */
  { ISFUNC, (Function *)0x0 },		/* b */
  { ISFUNC, (Function *)0x0 },		/* c */
  { ISFUNC, (Function *)0x0 },		/* d */
  { ISFUNC, (Function *)0x0 },		/* e */
  { ISFUNC, (Function *)0x0 },		/* f */
  { ISFUNC, (Function *)0x0 },		/* g */
  { ISFUNC, (Function *)0x0 },		/* h */
  { ISFUNC, (Function *)0x0 },		/* i */
  { ISFUNC, (Function *)0x0 },		/* j */
  { ISFUNC, (Function *)0x0 },		/* k */
  { ISFUNC, (Function *)0x0 },		/* l */
  { ISFUNC, (Function *)0x0 },		/* m */
  { ISFUNC, (Function *)0x0 },		/* n */
  { ISFUNC, (Function *)0x0 },		/* o */
  { ISFUNC, (Function *)0x0 },		/* p */
  { ISFUNC, (Function *)0x0 },		/* q */
  { ISFUNC, (Function *)0x0 },		/* r */
  { ISFUNC, (Function *)0x0 },		/* s */
  { ISFUNC, (Function *)0x0 },		/* t */
  { ISFUNC, (Function *)0x0 },		/* u */
  { ISFUNC, (Function *)0x0 },		/* v */
  { ISFUNC, (Function *)0x0 },		/* w */
  { ISFUNC, (Function *)0x0 },		/* x */
  { ISFUNC, (Function *)0x0 },		/* y */
  { ISFUNC, (Function *)0x0 },		/* z */

  /* Final punctuation. */
  { ISFUNC, (Function *)0x0 },			  /* { */
  { ISFUNC, (Function *)0x0 },			  /* | */
  { ISFUNC, (Function *)0x0 },			  /* } */
  { ISFUNC, (Function *)0x0 },			  /* ~ */
  { ISFUNC, (Function *) rl_backward_kill_word }, /* RUBOUT */

#if NUMCHARS==256
  /* Latin-1 or DOS characters */
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
#endif

};
