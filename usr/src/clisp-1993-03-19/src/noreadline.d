# Ein Ersatz für die GNU readline()-Library.
# Bruno Haible 18.3.1993

#include "lispbibl.c"

#define CLISP # Hinweis an chardefs.h; TAB, ESC, RUBOUT definieren wir selbst
#include <stdio.h> # readline.h benutzt den Typ FILE
#include "readline.h"
#undef CLISP

global int rl_present_p = 0; # readline()-Library nicht vorhanden

global char* rl_readline_name;
global Function* rl_attempted_completion_function;
global Function* rl_completion_entry_function;

global char* rl_basic_word_break_characters;
global char* rl_basic_quote_characters;
global char* rl_completer_quote_characters;

global unsigned char * rl_line_buffer;
global int rl_already_prompted;

global char* readline(prompt)
  var reg1 char* prompt;
  { return NULL; }

global void rl_deprep_terminal()
  { ; }

global char* filename_completion_function(text,state)
  var reg1 char* text;
  var reg1 int state;
  { return NULL; }

global void add_history(line)
  var reg1 char* line;
  { ; }

