/* exp_main.h - defn's for main and its subroutines */

extern FILE *exp_cmdfile;
extern int exp_cmdlinecmds;
extern int exp_interactive;
extern int exp_is_debugging;

void exp_init();
void exp_parse_argv();
int  exp_interpreter();
void exp_interpret_cmdfile();
void exp_interpret_rcfiles();

char *exp_cook();
void (*exp_app_exit)();		/* app-specific exit handler */
void exp_exit();

