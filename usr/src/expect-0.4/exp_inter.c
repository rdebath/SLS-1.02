/* interact (using select) - give user keyboard control

Written by: Don Libes, NIST, 2/6/90

Design and implementation of this program was paid for by U.S. tax
dollars.  Therefore it is public domain.  However, the author and NIST
would appreciate credit if this program or parts of it are used.

$Revision: 1.2 $
$Date: 1992/02/21 18:47:50 $

*/

#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include "exp_tty.h"
#include <ctype.h>
#include "tcl.h"
#include "tclInt.h"		/* for tclRegexpError */
#include "string.h"
#include "exp_rename.h"
#include "exp_global.h"
#include "exp_command.h"
#include "exp_log.h"

#include "regexp.h"
#include "exp_regexp.h"

#define new(x)	(x *)malloc(sizeof(x))

struct action {
	char *statement;
	int fast;		/* if true, skip tty mode changing, spawn_id */
				/* check, etc */
	struct action *next;	/* chain only for later for freeing */
};

struct keymap {
	char *keys;	/* original pattern provided by user */
	regexp *re;
	int case_sensitive;
	int echo;	/* if keystrokes should be echoed */
	int writethru;	/* if keystrokes should go through to process */
	struct action action;
	struct keymap *next;
};

struct output {
	int spawn_id;
	struct f *f;
	struct action *action_eof;
	struct output *next;
};

struct input {
	int spawn_id;
	struct f *f;
	struct output *output;
	struct action *action_eof;
	struct action *action_timeout;
	struct keymap *keymap;
	int timeout_nominal;		/* timeout nominal */
	int timeout_remaining;		/* timeout remaining */
	struct input *next;
};

static void free_input();
static void free_keymap();
static void free_output();
static void free_action();
static struct action *new_action();
static struct output *new_output();
static struct input *new_input();

/* special pattern that signifies the expect interpreter itself */
#define INTERPRETER_ACTION	"interpreter"

/* NB: FOLLOWING DESCRIPTION HAS NOT BEEN REVISED SINCE ADDITION OF REGEXP */
/* This function accepts user keystrokes and returns one of the above values */
/* describing whether the keystrokes match a key sequence, and could or */
/* can't if more characters arrive */
/* The function assigns a matching keymap if there is a match or can-match */

/* The basic idea of how this works is it does a smart sequential search. */
/* It is optimized (if you can call it that) towards a small number of */
/* key mappings, but still works well for large maps, since no function */
/* calls are made, and we stop as soon as there is a single-char mismatch, */
/* and go on to the next one.  A hash table or compiled DFA probably would */
/* not buy very much here for most maps. */

static
int
in_keymap(string,stringlen,keymap,km_match,match_length,skip)
char *string;
int stringlen;
struct keymap *keymap;		/* linked list of keymaps */
struct keymap **km_match;	/* keymap that matches or can match */
int *match_length;		/* # of chars that matched */
int *skip;			/* # of chars to skip */
{
	struct keymap *km;
	char *ks;		/* string from a keymap */
	char *start_search;	/* where in the string to start searching */
	char *string_end;

	/* assert (*km == 0) */

	/* a shortcut that should help master output which typically */
	/* is lengthy and has no key maps.  Otherwise it would mindlessly */
	/* iterate on each character anyway. */
	if (!keymap) {
		*skip = stringlen;
		return(EXP_CANTMATCH);
	}

	string_end = string + stringlen;

	/* Mark beginning of line for ^ . */
	regbol = string;

    for (start_search = string;*start_search;start_search++) {
	if (*km_match) break; /* if we've already found a CAN_MATCH */
			/* don't bother starting search from positions */
			/* further along the string */

	for (km=keymap;km;km=km->next) {
	    char *s;	/* current character being examined */

	    if (!km->re) {
		/* fixed string */
		for (s = start_search,ks = km->keys ;;s++,ks++) {
			/* if we hit the end of this map, must've matched! */
			if (*ks == 0) {
				*skip = start_search-string;
				*match_length = s-start_search;
				*km_match = km;
				return(EXP_MATCH);
			}

			/* if we ran out of user-supplied characters, and */
			/* still haven't matched, it might match if the user */
			/* supplies more characters next time */
			if (s == string_end) {
				/* skip to next key entry, but remember */
				/* possibility that this entry might match */
				if (!*km_match) *km_match = km;
				break;
			}
			/* skip to next key entry, if characters don't match */
			if ((*s & 0x7f) != *ks) break;
		}
	    } else {
		/* regexp */
		int r;	/* regtry status */
		regexp *prog = km->re;

		/* if anchored, but we're not at beginning, skip pattern */
		if (prog->reganch) {
			if (string != start_search) continue;
		}

		/* known starting char - quick test 'fore lotta work */
		if (prog->regstart) {
			if ((*start_search & 0x7f) != prog->regstart) break;
		}
		r = regtry(prog,start_search,match_length);
		if (r == EXP_MATCH) {
			*km_match = km;
			*skip = start_search-string;
			return(EXP_MATCH);
		}
		if (r == EXP_CANMATCH) {
			if (!*km_match) *km_match = km;
		}
	    }
	}
    }

	if (*km_match) {
		/* report a can-match */
		*skip = (start_search-string)-1;
		*match_length = stringlen - *skip;
		return(EXP_CANMATCH);
	}

	*skip = start_search-string;
	return(EXP_CANTMATCH);
}
			
#define finish(x)	{ status = x; goto done; }

/*ARGSUSED*/
int
cmdInteract(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	/*declarations*/
	int input_count;	/* count of struct input descriptors */
	extern int fd_max;
	struct input **fd_to_input;	/* map from fd's to "struct input"s */
	int *fd_list;
	struct keymap *km;	/* ptr for above while parsing */
	extern int exp_dev_tty;
	int master = EXP_SPAWN_ID_BAD;

	int all_fast = FALSE;	/* by default, turn off -f */
	int next_fast = FALSE;	/* if we've seen a single -f */
	int next_re = FALSE;	/* if we've seen a single -re */
	int next_writethru = FALSE;/*if macros should also go to proc output */
	int next_echo = FALSE;	/* if macros should be echoed */
/*	int next_case_sensitive = TRUE;*/
	char **oldargv = 0;	/* save original argv here if we split it */
	int status = TCL_OK;	/* final return value */
	int i;			/* trusty temp */

	int timeout_simple = TRUE;	/* if no or global timeout */

	int tty_changed = FALSE;/* true if we had to change tty modes for */
				/* interact to work (i.e., to raw, noecho) */
	int was_raw;
	int was_echo;
	exp_tty tty_old;

	int replace_user_by_process = EXP_SPAWN_ID_BAD; /* for -u flag */

	struct input *input_user;
	struct input *input_default;
#define input_base input_user
	struct input *inp;	/* overused ptr to struct input */
	struct output *outp;	/* overused ptr to struct output */

	int dash_input_count = 0; /* # of "-input"s seen */
	int dash_output_count = 0; /* # of "-output"s seen */
	int arbitrary_timeout;
	int default_timeout;
	struct action action_timeout;	/* common to all */
	struct action action_eof;	/* common to all */
	struct action **action_eof_ptr;	/* allow -input/ouput to */
		/* leave their eof-action assignable by a later */
		/* -eof */
	struct action *action_base = 0;

	struct keymap **end_km;

	int key;


	if (argc == 2 && strchr(argv[1],'\n')) {
		return(exp_eval_with_one_arg(clientData,interp,argc,argv));
	}

	argv++;
	argc--;

	default_timeout = EXP_TIME_INFINITY;
	arbitrary_timeout = EXP_TIME_INFINITY;	/* if user specifies */
		/* a bunch of timeouts with EXP_TIME_INFINITY, this will be */
		/* left around for us to find. */

	if (!(input_user = new_input(interp))) return(TCL_ERROR);
	input_user->spawn_id = 0;	/* stdin by default */
	input_user->output = 0;
	input_user->action_eof = &action_eof;
	input_user->timeout_nominal = EXP_TIME_INFINITY;
	input_user->action_timeout = 0;
	input_user->keymap = 0;
	end_km = &input_user->keymap;
	inp = input_user;

	if (!(input_default = new_input(interp))) return(TCL_ERROR);
	input_default->spawn_id = EXP_SPAWN_ID_BAD;	/* fix later */
	input_default->output = 0;
	input_default->action_eof = &action_eof;
	input_default->timeout_nominal = EXP_TIME_INFINITY;
	input_default->action_timeout = 0;
	input_default->keymap = 0;
	input_default->next = 0;		/* no one else */
	input_user->next = input_default;

	input_count = 2;

	action_eof.statement = "return";
	action_eof.fast = TRUE;
	action_timeout.fast = TRUE;

	for (;argc>0;argc--,argv++) {
		if (streq(*argv,"-input")) {
			dash_input_count++;
			if (dash_input_count == 2) {
				inp = input_default;
			} else if (dash_input_count > 2) {
				struct input *previous_input = inp;
				if (!(inp = new_input(interp)))
					return(TCL_ERROR);
				previous_input->next = inp;
				input_count++;
			}
			inp->output = 0;
			inp->action_eof = &action_eof;
			action_eof_ptr = &inp->action_eof;
			inp->timeout_nominal = default_timeout;
			inp->action_timeout = &action_timeout;
			inp->keymap = 0;
			end_km = &inp->keymap;
			inp->next = 0;
			argc--;argv++;
			if (argc < 1) {
				exp_error(interp,"-input needs argument");
				return(TCL_ERROR);
			}
			inp->spawn_id = atoi(*argv);
		} else if (streq(*argv,"-output")) {
			struct output *tmp;

			dash_output_count++;

			/* imply a "-input" */
			if (dash_input_count == 0) dash_input_count = 1;

			if (!(outp = new_output(interp))) return(TCL_ERROR);

			/* link new output in front of others */
			tmp = inp->output;
			inp->output = outp;
			outp->next = tmp;

			argc--;argv++;
			if (argc < 1) {
				exp_error(interp,"-output needs argument");
				return(TCL_ERROR);
			}
			/* accept Tcl file handles, too */
			/* the following expression looks stupid but we */
			/* really have to verify that at least four chars */
			/* exist before we bop on over to the 5th one so */
			/* we might as well verify them while we're at it */
			if ((argv[0][0] == 'f') && (argv[0][1] == 'i')
			    && (argv[0][2] == 'l') && (argv[0][3] == 'e')) {
				outp->spawn_id = atoi(*argv + 4);
			} else 	outp->spawn_id = atoi(*argv);
			outp->action_eof = &action_eof;
			action_eof_ptr = &outp->action_eof;
		} else if (streq(*argv,"-u")) {	/* treat process as user */
			argc--;argv++;
			if (argc < 1) {
				exp_error(interp,"-u needs argument");
				return(TCL_ERROR);
			}
			replace_user_by_process = atoi(*argv);

			/* imply a "-input" */
			if (dash_input_count == 0) dash_input_count = 1;

		} else if (streq(*argv,"-o")) {	/* apply following patterns */
						/* to opposite side of */
						/* interaction */
			end_km = &input_default->keymap;

			/* imply two "-input" */
			if (dash_input_count < 2) dash_input_count = 2;

		} else if (streq(*argv,"-i")) { /* substitute master */
			argc--;argv++;
			master = atoi(*argv);
			/* will be used later on */

			/* imply two "-input" */
			if (dash_input_count < 2) dash_input_count = 2;
/*		} else if (streq(*argv,"-nocase")) {*/
/*			next_case_sensitive = FALSE;*/
		} else if (streq(*argv,"-re")) {
			if (argc < 1) {
				exp_error(interp,"-re needs pattern");
				return(TCL_ERROR);
			}
			next_re = TRUE;
		} else if (streq(*argv,"-echo")) {
			next_echo = TRUE;
		} else if (streq(*argv,"-flush")) {
			next_writethru = TRUE;
		} else if (streq(*argv,"-f")) {
			if (argc < 1) {
				exp_error(interp,"-f needs pattern");
				return(TCL_ERROR);
			}
			next_fast = TRUE;
		} else if (streq(*argv,"-F")) {
			all_fast = TRUE;
		} else if (streq(*argv,"-eof")) {
			struct action *action;

			argc--;argv++;

			/* if -eof comes before "-input", then applies */
			/* to all descriptors, else just the current one */
			if (dash_input_count > 0 || dash_output_count > 0) {
				*action_eof_ptr = action =
					new_action(action_base);
				if (!action) {
					exp_error(interp,"malloc failed: -action");
					return(TCL_ERROR);
				}
			} else {
				action = &action_eof;
			}
			action->statement = *argv;
			argc--;argv++;
			if (all_fast || next_fast) {
				action->fast = TRUE;
				next_fast = FALSE;
			}
		} else if (streq(*argv,"-timeout")) {
			int t;
			struct action *action;

			argc--;argv++;
			if (argc < 1) {
				exp_error(interp,"-timeout needs time");
				return(TCL_ERROR);
			}

			t = atoi(*argv);
			argc--;argv++;
			if (t != -1)
				arbitrary_timeout = t;
			/* we need an arbitrary timeout to start */
			/* search for lowest one later */


			/* if -timeout comes before "-input", then applies */
			/* to all descriptors, else just the current one */
			if (dash_input_count > 0) {
				timeout_simple = FALSE;
				action = inp->action_timeout;
				inp->timeout_nominal = t;
			} else {
				action = &action_timeout;
				default_timeout = t;
			}
			action->statement = *argv;
			argc--;argv++;
			action->fast = (all_fast || next_fast);
			next_fast = FALSE;
		} else {
			km = new(struct keymap);
			if (!km) {
				exp_error(interp,"malloc failed (struct keymap)");
				return(TCL_ERROR);
			}

			/* so that we can match in order user specified */
			/* link to end of keymap list */
			*end_km = km;
			km->next = 0;
			end_km = &km->next;

			km->echo = next_echo;
			km->writethru = next_writethru;
			km->action.fast = (all_fast || next_fast);
/*			km->case_sensitive = next_case_sensitive;*/

			next_echo = next_writethru = next_fast = FALSE;
/*			next_case_sensitive = TRUE;*/

			km->keys = *argv;

			if (next_re) {
				if (0 == (km->re = regcomp(*argv))) {
					exp_error(interp,"bad regular expression: %s",
						tclRegexpError);
					return(TCL_ERROR);
				}
			} else {
				km->re = 0;
				/* should really compare to f->umsize, but */
				/* it's hard to get at this point */
				if (BUFSIZ < strlen(km->keys)) {
					exp_error(interp,"key sequence \"%s\"> match_max (%s bytes)",km->keys,BUFSIZ);
					return(TCL_ERROR);
				}
			}
			argc--;argv++;

			km->action.statement = *argv;
			debuglog("defining key %s, action %s\r\n",
			 km->keys,
			 km->action.statement?(dprintify(km->action.statement))
				   :INTERPRETER_ACTION);

			/* imply a "-input" */
			if (dash_input_count == 0) dash_input_count = 1;
		}
	}

	/* if the user has not supplied either "-output" for the */
	/* default two "-input"s, fix them up here */

	if (!input_user->output) {
		struct output *o = new_output(interp);

		if (!o) return(TCL_ERROR);
		if (master == EXP_SPAWN_ID_BAD) {
			if (0 == update_master(&master)) return(TCL_ERROR);
		}
	 	o->spawn_id = master;
		o->next = 0;	/* no one else */
		o->action_eof = &action_eof;
		input_user->output = o;
	}

	if (!input_default->output) {
		struct output *o = new_output(interp);

		if (!o) return(TCL_ERROR);
	 	o->spawn_id = 1;	/* stdout by default */
		o->next = 0;	/* no one else */
		o->action_eof = &action_eof;
		input_default->output = o;
	}

	/* if user has given "-u" flag, substitute process for user */
	/* in first two -inputs */
	if (replace_user_by_process != EXP_SPAWN_ID_BAD) {
		input_user->spawn_id = 		  replace_user_by_process;
		input_default->output->spawn_id = replace_user_by_process;
	}

	if (input_default->spawn_id == EXP_SPAWN_ID_BAD) {
		if (master == EXP_SPAWN_ID_BAD) {
			if (0 == update_master(&master)) return(TCL_ERROR);
		}
		input_default->spawn_id = master;
	}

	if (input_user->spawn_id == input_default->spawn_id) {
		exp_error(interp,"cannot interact with self - set spawn_id to a spawned process");
		return(TCL_ERROR);
	}


/* if we are running this using /dev/tty */
#define realtty(x)	((x == 1) || (x == exp_dev_tty && exp_dev_tty != -1))
#define REALTTY ((input_user->spawn_id == 0) || \
		 (input_user->spawn_id == exp_dev_tty && exp_dev_tty != -1))

	if (REALTTY) {
		tty_changed = tty_raw_noecho(interp,&tty_old,&was_raw,&was_echo);
	}

	/* build args for ready() */
	fd_list = (int *)malloc(input_count * sizeof(int));

	fd_to_input = (struct input **)malloc((fd_max+1) * sizeof(struct input *));
	/* at this point, we can now "goto done"; doing so earlier will */
	/* screw up memory allocator */

	for (inp = input_base,i=0;inp;inp=inp->next,i++) {
		/* validate all the input descriptors */
		if ((!(inp->f = fd_to_f(interp,inp->spawn_id,"interact")))
		    || (TCL_ERROR == f_adjust(interp,inp->f)))
			finish(TCL_ERROR);

		/* build map to translate from spawn_id to struct input */
		fd_to_input[inp->spawn_id] = inp;

		/* build input to ready() */
		fd_list[i] = inp->spawn_id;

		/* start timers */
		inp->timeout_remaining = inp->timeout_nominal;
	}

	key = expect_key++;

#ifndef SIMPLE_EVENT

	/* loop waiting (in event handler) for input */
	for (;;) {
		int te;	/* result of Tcl_Eval */
		struct f *u;
		int rc;	/* return code from ready.  This is further */
			/* refined by matcher. */
		int cc;	/* chars count from read() */
		int m;	/* master */
		struct action *action = 0;
		time_t previous_time;
		time_t current_time;
		int match_length, skip;
		int change;	/* if action requires cooked mode */
		int attempt_match = TRUE;
		struct input *soonest_input;
		int print;		/* # of chars to print */
		int oldprinted;		/* old version of u->printed */

		int timeout;	/* current as opposed to default_timeout */

		/* calculate how long to wait */
		/* by finding shortest remaining timeout */
		if (timeout_simple) {
			timeout = default_timeout;
		} else {
			timeout = arbitrary_timeout;

			for (inp=input_base;inp;inp=inp->next) {
				if ((inp->timeout_remaining != EXP_TIME_INFINITY) &&
				    (inp->timeout_remaining < timeout))
					soonest_input = inp;
					timeout = inp->timeout_remaining;
			}

			time(&previous_time);
			/* timestamp here rather than simply saving old */
			/* current time (after ready()) to account for */
			/* possibility of slow actions */

			/* timeout can actually be EXP_TIME_INFINITY here if user */
			/* explicitly supplied it in a few cases (or */
			/* the count-down code is broken) */
		}

		rc = exp_get_next_event(interp,fd_list,input_count,&m,timeout,key);
		if (rc == EXP_TCLERROR)
			return(EXP_TCLERROR);

		if (rc == EXP_TIMEOUT) {
			if (timeout_simple) {
				action = &action_timeout;
				goto got_action;
			} else {
				action = soonest_input->action_timeout;
			}
		}
		if (!timeout_simple) {
			int time_diff;

			time(&current_time);
			time_diff = current_time - previous_time;

			/* update all timers */
			for (inp=input_base;inp;inp=inp->next) {
				if (inp->timeout_remaining != EXP_TIME_INFINITY) {
					inp->timeout_remaining -= time_diff;
					if (inp->timeout_remaining < 0)
						inp->timeout_remaining = 0;
				}
			}
		}

		/* at this point, we have some kind of event which can be */
		/* immediately processed - i.e. something that doesn't block */

		/* figure out who we are */
		inp = fd_to_input[m];
		u = inp->f;

		/* reset timer */
		inp->timeout_remaining = inp->timeout_nominal;

		switch (rc) {
		case EXP_DATA_NEW:
			cc = read(m,	u->buffer + u->size,
					u->msize - u->size);
			if (cc > 0) {
				fs[m].key = key;
				u->size += cc;
				u->buffer[u->size] = '\0';

				/* avoid another function call if possible */
				if (debugfile || is_debugging) {
					debuglog("spawn id %d sent {%s}\r\n",m,
						printify(u->buffer + u->size - cc));
				}
				break;
			}
			/*FALLTHRU*/
#ifdef HPUX
		case EXP_EOF:
#endif
			action = inp->action_eof;
			attempt_match = FALSE;
			skip = u->size;
			rc = EXP_EOF;
			debuglog("interact: received eof from spawn_id %d\r\n",m);
			fd_close(interp,m);
			break;
		case EXP_DATA_OLD:
			cc = 0;
			break;
		case EXP_TIMEOUT:
			action = inp->action_timeout;
			attempt_match = FALSE;
			skip = u->size;
			break;
		}

		km = 0;

		if (attempt_match) {
			rc = in_keymap(u->buffer,u->size,inp->keymap,
				&km,&match_length,&skip);
		} else {
			attempt_match = TRUE;
		}

		/* put regexp result in variables */
		if (km && km->re) {
#define INTER_OUT "interact_out"
#define out(i,val)  debuglog("expect: set %s(%s) {%s}\r\n",INTER_OUT,i, \
						dprintify(val)); \
		    if (!Tcl_SetVar2(interp,INTER_OUT,i,val,0)) \
				{status = TCL_ERROR; goto done;}

			char name[20], value[20];
			regexp *re = km->re;
			char match_char;/* place to hold char temporarily */
					/* uprooted by a NULL */

			for (i=0;i<NSUBEXP;i++) {
				int offset;

				if (re->startp[i] == 0) break;

				/* start index */
				sprintf(name,"%d,start",i);
				offset = re->startp[i]-u->buffer;
				sprintf(value,"%d",offset);
				out(name,value);

				/* end index */
				sprintf(name,"%d,end",i);
				sprintf(value,"%d",re->endp[i]-u->buffer-1);
				out(name,value);

				/* string itself */
				sprintf(name,"%d,string",i);
				/* temporarily null-terminate in */
				/* middle */
				match_char = *re->endp[i];
				*re->endp[i] = 0;
				out(name,re->startp[i]);
				*re->endp[i] = match_char;
			}
		}

		/* dispose of chars that should be skipped */
		
		/* skip is chars not involved in match */
		/* print is with chars involved in match */

		if (km && km->writethru) {
			print = skip + match_length;
		} else print = skip;

		/* figure out if we should echo any chars */
		if (km && km->echo) {
			int seen;	/* either printed or echoed */

			/* echo to stdout rather than stdin */
			if (m == 0) m = 1;

			/* write is unlikely to fail, since we just read */
			/* from same descriptor */
			seen = u->printed + u->echoed;
			if (skip >= seen) {
				write(m,u->buffer+skip,match_length);
			} else if ((match_length + skip - seen) > 0) {
				write(m,u->buffer+seen,match_length+skip-seen);
			}
			u->echoed = match_length + skip - u->printed;
		}

		oldprinted = u->printed;

		/* If expect has left characters in buffer, it has */
		/* already echoed them to the screen, thus we must */
		/* prevent them being rewritten.  Unfortunately this */
		/* gives the possibility of matching chars that have */
		/* already been output, but we do so since the user */
		/* could have avoided it by flushing the output */
		/* buffers directly. */
		if (print > u->printed) {	/* usual case */
			int wc;	/* return code from write() */
			for (outp = inp->output;outp;outp=outp->next) {

				/* send to logfile if open */
				/* and user is seeing it */
				if (logfile && realtty(outp->spawn_id)) {
					fwrite(u->buffer+u->printed,1,
					       print - u->printed,logfile);
				}

				/* send to each output descriptor */
				wc = write(outp->spawn_id,u->buffer+u->printed,
					print - u->printed);
				if (wc <= 0) {
					debuglog("interact: write on spawn id %d failed",wc);
					action = outp->action_eof;
					change = !(action && action->fast);

					if (change && tty_changed)
						tty_set(interp,&tty_old,was_raw,was_echo);
					if (action->statement) {
						te = Tcl_Eval(interp,action->statement,0,(char **)0);
					} else {
						nflog("\r\n",1);
						te = exp_interpreter(interp);
					}

					if (change && REALTTY) tty_changed =
					   tty_raw_noecho(interp,&tty_old,&was_raw,&was_echo);
					switch (te) {
					case TCL_BREAK:
					case TCL_CONTINUE:
						finish(te);
					case TCL_RETURN_TCL:
						finish(TCL_RETURN);
					case TCL_RETURN:
						finish(TCL_OK);
					case TCL_OK:
						/* god knows what the user might */
						/* have done to us in the way of */
						/* closed fds, so .... */
						action = 0;	/* reset action */
						continue;
					default:
						finish(TCL_ERROR);
					}
				}
			}
			u->printed = print;
		}

		/* u->printed is now accurate with respect to the buffer */
		/* However, we're about to shift the old data out of the */
		/* buffer.  Thus, u->size, printed, and echoed must be */
		/* updated */

		/* first update size based on skip information */
		/* then set skip to the total amount skipped */

		if (rc == EXP_MATCH) {
			action = &km->action;

			skip += match_length;
			u->size -= skip;

			if (u->size)
				memcpy(u->buffer, u->buffer + skip, u->size);
				exp_lowmemcpy(u->lower,u->buffer+ skip, u->size);
		} else {
			if (skip) {
				u->size -= skip;
				memcpy(u->buffer, u->buffer + skip, u->size);
				exp_lowmemcpy(u->lower,u->buffer+ skip, u->size);
			}
		}

		/* as long as buffer is still around, null terminate it */
		if (rc != EXP_EOF) {
			u->buffer[u->size] = '\0';
			u->lower [u->size] = '\0';
		}
		/* now update printed based on total amount skipped */

		u->printed -= skip;
		/* if more skipped than printed (i.e., keymap encountered) */
		/* for printed positive */
		if (u->printed < 0) u->printed = 0;

		/* if we are in the middle of a match, force the next event */
		/* to wait for more data to arrive */
		u->force_read = (rc == EXP_CANMATCH);

		/* finally reset echoed if necessary */
		if (rc != EXP_CANMATCH) {
			if (skip >= oldprinted + u->echoed) u->echoed = 0;
		}

		if (action) {
got_action:
			change = !(action && action->fast);

			if (change && tty_changed)
				tty_set(interp,&tty_old,was_raw,was_echo);
			if (action->statement) {
				te = Tcl_Eval(interp,action->statement,0,(char **)0);
			} else {
				nflog("\r\n",1);
				te = exp_interpreter(interp);
			}
			if (change && REALTTY) tty_changed =
			   tty_raw_noecho(interp,&tty_old,&was_raw,&was_echo);
			switch (te) {
			case TCL_BREAK:
			case TCL_CONTINUE:
				finish(te);
			case TCL_RETURN_TCL:
				finish(TCL_RETURN);
			case TCL_RETURN:
				finish(TCL_OK);
			case TCL_OK:
				/* god knows what the user might */
				/* have done to us in the way of */
				/* closed fds, so .... */
				action = 0;	/* reset action */
				continue;
			default:
				finish(TCL_ERROR);
			}
		}
	}

#else /* SIMPLE_EVENT */
	exp_error(interp,"interact not yet supported on systems that lack poll/select");
	status = TCL_ERROR;
#endif /* SIMPLE_EVENT */

 done:
	if (tty_changed) tty_set(interp,&tty_old,was_raw,was_echo);
	if (oldargv) free((char *)argv);
	free((char *)fd_list);
	free((char *)fd_to_input);
	free_input(input_base);
	free_action(action_base);

	return(status);
}

static void
free_keymap(km)
struct keymap *km;
{
	if (km == 0) return;
	free_keymap(km->next);

	free((char *)km);
}

static void
free_action(a)
struct action *a;
{
	struct action *next;

	while (a) {
		next = a->next;
		free((char *)a);
		a = next;
	}
}

static void
free_input(i)
struct input *i;
{
	if (i == 0) return;
	free_input(i->next);

	free_output(i->output);
	free_keymap(i->keymap);
	free((char *)i);
}

static struct action *
new_action(base)
struct action *base;
{
	struct action *o = new(struct action);
	if (!o) return 0;

	/* stick new action into beginning of list of all actions */
	o->next = base;
	base = o;

	return o;
}

static struct output *
new_output(interp)
Tcl_Interp *interp;
{
	struct output *o = new(struct output);

	if (!o) exp_error(interp,"malloc failed: -output");
	return o;
}

static struct input *
new_input(interp)
Tcl_Interp *interp;
{
	struct input *i = new(struct input);
	if (!i) exp_error(interp,"malloc failed: -input");
	return i;
}

static void
free_output(o)
struct output *o;
{
	if (o == 0) return;
	free_output(o->next);

	free((char *)o);
}
