#ifndef lint
static char sccsid[] = "@(#)textedit.c 15.50 90/05/22";
#endif

/*
 * Copyright (c) 1986, 1987, 1988 by Sun Microsystems, Inc.
 */

/*
 * This textedit is the envelope that binds various text display and
 *   editing facilities together.
 */

#include <sys/param.h> /* MAXPATHLEN (include types.h if removed) */
#include <sys/dir.h>   /* MAXNAMLEN */
#include <sys/stat.h>

#include <fcntl.h>
#include <stdio.h>
#include <strings.h>
#include <X11/X.h>
#include <xview/defaults.h>
#include <xview/font.h>
#include <xview/notice.h>
#include <xview/frame.h>
#include <xview/xview.h>
#include <xview/scrollbar.h>
#include <xview/text.h>
#include <xview/panel.h>
#include <xview/selection.h>
#include <xview/sel_svc.h>
#include <xview/sel_attrs.h>

#ifdef ecd.help
#include <suntool/help.h>
#endif

/*
 * The presence of this line caused textedit not to be built
 * The function is already defined in the XView libraries
long	textsw_store_file();
*/
char	*getwd(), *sprintf();
void	frame_cmdline_help();
static int		off();
static Notify_value	mysigproc();
/*
extern Notify_value	textedit_event_proc();
*/

static Frame		base_frame;
static char		current_filename[MAXNAMLEN];
static char		current_directory[MAXPATHLEN];
static int		handling_signal;
static Textsw		textsw;
static char		*cmd_name;
static int		user_label;
static int		caps_lock_on;
static int		edited;
static int      	read_only;
static char		cmdline_args[1024];


int		PANEL_ITEM_DATA;

static short edit_ic_image[256]={
#include "textedit.icon"
};

static short edit_mask_image[256]={
#include "textedit.mask.icon"
};

/*
 * The textedit command line options.
 */
static char	*option_names[] = {
			"auto_indent", "Ei",
			"okay_to_overwrite", "Eo",
			"lower_context", "EL",
			"margin", "Em",
			"multi_click_space", "ES",
			"multi_click_timeout", "ET",
			"number_of_lines", "En",
			"read_only", "Er",
			"scratch_window", "Es",
			"tab_width", "Et",
			"history_limit", "Eu",
			"upper_context", "EU",
			"checkpoint", "Ec",
			"num_cols", "EC",
#ifdef DEBUG
			"malloc_debug_level", "Ed",
#endif
			0  /* Terminator! */  };

#define OPTION_AUTO_INDENT		(1<<0)
#define OPTION_ALWAYS_OVERWRITE		(1<<1)
#define OPTION_LOWER_CONTEXT		(1<<2)
#define OPTION_MARGIN			(1<<3)
#define OPTION_MULTI_CLICK_SPACE	(1<<4)
#define OPTION_MULTI_CLICK_TIMEOUT	(1<<5)
#define OPTION_NUMBER_OF_LINES		(1<<6)
#define OPTION_READ_ONLY		(1<<7)
#define OPTION_SCRATCH_WINDOW		(1<<8)
#define OPTION_TAB_WIDTH		(1<<9)
#define OPTION_UNDO_HISTORY		(1<<10)
#define OPTION_UPPER_CONTEXT		(1<<11)
#define OPTION_CHECKPOINT_FREQUENCY	(1<<12)
#define OPTION_NUM_COLS	                (1<<13)

#ifdef DEBUG
#define OPTION_MALLOC_DEBUG_LEVEL	(1<<14)
#define OPTION_EDIT_LOG_WRAPS_AT	(1<<15)
#endif

#ifdef TEXTEDIT_HELP_STRING
static char	*help_msg()
/*
 * This routine is required because the help_msg is too big for the
 *   compiler to accept as a single string token.
 */
{
	extern char	*calloc();
	char		*result = calloc(1, 5000);
	
	(void)sprintf(result, "\n\t\tHelp for %s\n%s\n%s\n%s\n",
		xv_get(base_frame, FRAME_LABEL),
"Mouse buttons: left is point, middle is adjust, right is menu.\n\
	Multi-click is only implemented for point.\n\
	Adjust-at-end-char is char-adjust.\n\
	CONTROL-select is pending-delete select, e.g. CONTROL-GET is MOVE.\n\
Function keys:",
"	L1 STOP				L2 is AGAIN\n\
	L3 (unused)			L4 is UNDO\n\
	L5 TOP				L6 is PUT\n\
	L7 OPEN				L8 is GET\n\
	L9 (or ^F) is FIND		L10 (or ^D) is DELETE\n\
	^P is an accelerator for the 'Put, then Get' menu item.\n\
	GET with no secondary selection (or ^G) is GET from SHELF.\n\
SHIFTs:",
"	SHIFT-back_char/word/line is forward_char/word/line.\n\
	SHIFT-TOP is BOTTOM; SHIFT-OPEN is CLOSE.\n\
Menu item notes: Save leaves old file as <file>%\n\
	Store writes to selected name, and then edits it.\n\
Startup: \"Text Editor <options> <file>\" is supported.\n\
Options are [where (E<letter>) indicates a short alternative]:\n\
	okay_to_overwrite (Eo) [on]\n\
	auto_indent (Ei) [off]			read_only (Er) [on]\n\
	lower_context (EL) <int>		scratch_window (Es) <int>\n\
	margin (Em) <int>			tab_width (Et) <int>\n\
	multi_click_space (ES) <int>		history_limit (Eu) <int>\n\
	multi_click_timeout (ET) <int>		upper_context (EU) <int>\n\
	number_of_lines (En) <int>\n\
Use defaultsedit to permanently set all of the above (and additional)\n\
	options for all text subwindows.\n\
Saving your edits in case of disaster:\n\
	If the  Text Editor runs wild, send it a SIGHUP to force a Store."
	);
	return(result);
}
#endif

/*
 * Return pointer to longest suffix not beginning with '/'
 */
static char *
base_name(full_name)
	char *full_name;
{
	extern char	*rindex();
	register char	*temp;

	if ((temp = rindex(full_name, '/')) == NULL)
	    return(full_name);
	else
	    return(temp+1);
}


Notify_value
textedit_load_event_proc(window, event, arg, type)

Xv_opaque               window;
Event                   *event;
Notify_arg              arg;
Notify_event_type       type;

{
	char	document_name[4096];
	int	alert_result;
	char	*s_p;

        if (event_action(event) == ACTION_DRAG_LOAD)
        {
               	if (xv_decode_drop(event, document_name, 4096) == -1)
                       	return(NOTIFY_DONE);
	
		/* display an alert here, asking if the user wants 
	   	   to have any edits discarded.  If they don't, 
	   	   then abort the load. */

		if (edited)
			alert_result = (int) notice_prompt(base_frame, &event,
				NOTICE_MESSAGE_STRINGS,
				"Your file has been edited.",
				"Do you wish to discard these edits?",
				0,
				NOTICE_BUTTON_NO, "Cancel",
				NOTICE_BUTTON_YES, "Confirm",
				0);

		if (!edited || (alert_result == NOTICE_YES))
		{

			/* clip out any files after the first one.  We 
			   don't take multiple file names in textedit. */

			s_p = document_name;
			while (*s_p && (*s_p != '\t'))
				s_p++;
			*s_p = NULL;
	
                	xv_set(textsw, 
				TEXTSW_FILE, document_name, 
				TEXTSW_FIRST,   0,
                		TEXTSW_INSERTION_POINT, 0,
				0);
	
			xv_set(base_frame, FRAME_CLOSED, FALSE, 0);
		}
                return(NOTIFY_DONE);
        }

        return notify_next_event_func(window, event, arg, type);
}
	
static
set_name_frame(textsw_local, attributes)
	Textsw		 textsw_local;
	Attr_avlist	 attributes;
{
	char		 frame_label[50+MAXNAMLEN+MAXPATHLEN];
	Icon		 edit_icon;
	char		 icon_text[sizeof(frame_label)];
	char		 *ptr;
	int		 len, pass_on = 0, repaint = 0;
	int		 was_read_only = read_only;
	Attr_avlist	 attrs;
	char		*attr_string;

	if (handling_signal)
	    return;
	icon_text[0] = '\0';
	ptr = icon_text;
	for (attrs = attributes; *attrs; attrs = attr_next(attrs)) {
	    repaint++;	/* Assume this attribute needs a repaint. */
	    switch ((Textsw_action)(*attrs)) {
	      case TEXTSW_ACTION_CAPS_LOCK:
		caps_lock_on = (int)attrs[1];
		ATTR_CONSUME(*attrs);
		break;
	      case TEXTSW_ACTION_CHANGED_DIRECTORY:
		attr_string = (char *)attrs[1];
		switch (attr_string[0]) {
		  case '/':
		    (void)strcpy(current_directory, attrs[1]);
		    break;
		  case '.':
		    if (attr_string[1] != '\0')
			(void) getwd(current_directory);
		    break;
		  case '\0':
		    break;
		  default:
		    (void)strcat(current_directory, "/");
		    (void)strcat(current_directory, attrs[1]);
		    break;
		}
		ATTR_CONSUME(*attrs);
		break;
	      case TEXTSW_ACTION_USING_MEMORY:
		(void)strcpy(current_filename, "(NONE)");
		(void)strcpy(icon_text, "NO FILE");
		edited = read_only = 0;
		ATTR_CONSUME(*attrs);
		break;
	      case TEXTSW_ACTION_LOADED_FILE:
		(void)strcpy(current_filename, attrs[1]);
		edited = read_only = 0;
		goto Update_icon_text;
	      case TEXTSW_ACTION_EDITED_FILE:
		edited = 1;
		*ptr++ = '>';
Update_icon_text:
		len = (strlen(attrs[1]) > sizeof(icon_text) - 2) ?
		    sizeof(icon_text) - 2 : strlen(attrs[1]);
		    /* need 1 char for edit/not, 1 for null */
		(void)strncpy(ptr, attrs[1], len); ptr[len] = '\0';
		(void)strcpy(ptr, base_name(ptr));	/* strip path */
		ATTR_CONSUME(*attrs);
		break;
	      default:
		pass_on = 1;
		repaint--;	/* Above assumption was wrong. */
		break;
	    }
	}
	if (pass_on)
	    (void)textsw_default_notify(textsw_local, attributes);
	if (repaint) {
	    (void)sprintf(frame_label, "%s Text Editor - %s%s, dir; %s",
		    (caps_lock_on) ? "[CAPS] " : "",
		    current_filename,
		    (was_read_only) ? " (read only)"
			: (edited) ? " (edited)" : "",
		    current_directory);
	    (void)xv_set(base_frame, FRAME_LABEL, frame_label, 0);

	    if (icon_text[0] != '\0') {
		struct rect	text_rect, *icon_rect;
		struct pixfont	*font;
		
		edit_icon = xv_get(base_frame, FRAME_ICON);
		icon_rect = (Rect *) (icon_get(edit_icon, ICON_IMAGE_RECT));
		font = (struct pixfont *) (icon_get(edit_icon, ICON_FONT));
		ptr = (user_label) ?
			(char *) xv_get(edit_icon, XV_LABEL) : icon_text;
		
		/* adjust icon text top/height to match font height */
		text_rect.r_height = xv_get(font, FONT_DEFAULT_CHAR_HEIGHT);
		text_rect.r_top =
		    icon_rect->r_height - (text_rect.r_height + 2);

		/* center the icon text */
		text_rect.r_width = strlen(ptr)*(xv_get(font, FONT_DEFAULT_CHAR_WIDTH));
		if (text_rect.r_width > icon_rect->r_width)
		    text_rect.r_width = icon_rect->r_width;
		text_rect.r_left = (icon_rect->r_width-text_rect.r_width)/2;

		(void)xv_set(edit_icon,
		    XV_LABEL,		ptr,
		    ICON_LABEL_RECT,	&text_rect,
		    0);
		/* xv_set actually makes a copy of all the icon fields */
		(void)xv_set(base_frame, FRAME_ICON, edit_icon, 0);
	    }
	}
}

static void
button_notify_proc(item, event)
	Panel_item item;
	Event *event;
{
	Textsw 	textsw = (Textsw)xv_get(item, XV_KEY_DATA, PANEL_ITEM_DATA);
	Menu	menu = (Menu)xv_get(item, PANEL_ITEM_MENU);
        Panel   p_menu = (Panel)xv_get(menu, MENU_PIN_WINDOW);
        Menu_item menu_item;
        Menu pullr_menu;
        int num_items, i;
	
	xv_set(menu, XV_KEY_DATA, TEXTSW_MENU_DATA_KEY, textsw, 0);
        if (p_menu) {
          num_items = (int)xv_get(menu, MENU_NITEMS);
          for (i = 1; i<= num_items; i++) {
              menu_item = (Menu_item)xv_get(menu, MENU_NTH_ITEM, i);
              if (menu_item) {
                  pullr_menu = (Menu)xv_get(menu_item, MENU_PULLRIGHT);
                  if (pullr_menu) {
                     xv_set(pullr_menu, XV_KEY_DATA, TEXTSW_MENU_DATA_KEY, textsw, 0); 
                  }
              }
          }
        }
}

static
textsw_split_proc(origview, newview, pos)

Xv_Window	origview;
Xv_Window	newview;
int		pos;

{
	notify_interpose_event_func(newview, textedit_load_event_proc, NOTIFY_SAFE);
}



static void
my_frame_help(name)
	char	*name;
{
	frame_cmdline_help(name);
#ifdef TEXTEDIT_HELP_STRING
	(void)fprintf(stderr,
		"\nFor further information, use the switch -text_help.\n");
#endif
}

#if	(defined(STANDALONE) || defined(DEBUG) || defined(GPROF))
main(argc, argv)
	int	  argc;
	char	**argv;
{
#ifdef	GPROF
	if (argc > 1 && strcmp(argv[argc-1], "-gprof") == 0) {
	    moncontrol(1);
	    /* Pull the -gprof out of argc/v */
	    argc--;
	    argv[argc] = (char *)0;
	} else {
	    moncontrol(0);
	}
#endif	GPROF
	textedit_main(argc, argv);
}
#endif	(defined(STANDALONE) || defined(DEBUG) || defined(GPROF))

textedit_main(argc, argv)
	int	  argc;
	char	**argv;
{
#define	GET_INT_ATTR_VAL(var)						\
	if (argc > 0) {var = (caddr_t) atoi(argv[1]); argc--, argv++;}
	extern struct pixfont	 *pw_pfsysopen();

	Icon			  edit_icon;
	Textsw_status		  status;
	int			  checkpoint = 0;
	int			  margin;
	int			  textswwidth;
	int			  number_of_lines = 0;
	int			  optioncount = 
				   sizeof(option_names)/sizeof(option_names[0]);
	struct stat           	  stb;
	caddr_t			  textsw_attrs[ATTR_STANDARD_SIZE];
	int			  attrc = 0;
	char			 *file_to_edit = NULL;
	Panel			  panel;
	Panel_item		  file_panel_item, edit_panel_item, display_panel_item, find_panel_item;
	Menu			  file_panel_menu, edit_panel_menu, display_panel_menu, find_panel_menu;
		
#ifdef DEBUG
	caddr_t			  edit_log_wraps_at = (caddr_t)TEXTSW_INFINITY;
#endif
	
	Menu_item		  menu_item;
        int                       num_cols = 0;
        int                       user_set_size = FALSE;
	char			  **argscanner = argv;
	Server_image		  icon_image;
	Server_image		  mask_image;


#define STDERR  2

	/*
	 * Init data
	 * Implicitely zeroed:	caps_lock_on, handling_signal,
	 *			read_only, edited
	 */

	while (*argscanner)
        {
                if (!strcmp(*argscanner, "-Ws") || 
		    !strcmp(*argscanner, "-Ww") || 
		    !strcmp(*argscanner, "-Wh"))
                {
                        user_set_size = TRUE;
			break;
                }
                argscanner++;
        }


	xv_init(XV_INIT_ARGC_PTR_ARGV,	&argc, argv, 0);

     	icon_image = xv_create(0, SERVER_IMAGE,
                SERVER_IMAGE_BITS, edit_ic_image,
                SERVER_IMAGE_DEPTH, 1,
                XV_WIDTH, 64,
                XV_HEIGHT, 64,
                0);

     	mask_image = xv_create(0, SERVER_IMAGE,
                SERVER_IMAGE_BITS, edit_mask_image,
                SERVER_IMAGE_DEPTH, 1,
                XV_WIDTH, 64,
                XV_HEIGHT, 64,
                0);

        edit_icon= xv_create(0, ICON,
                        ICON_IMAGE, icon_image,
			ICON_MASK_IMAGE, mask_image,
			ICON_TRANSPARENT, TRUE,
                        0);

	xv_set(edit_icon,
		WIN_CONSUME_EVENTS,
		LOC_WINENTER,
		LOC_WINEXIT,
		LOC_DRAG,
		0,
		0);

	cmd_name = *argv;		/* Must be BEFORE calls on die() */
	current_filename[0] = '\0';
	(void) getwd(current_directory);
	    /* Error message is placed into current_directory by getwd */
	checkpoint =
	    defaults_get_integer_check("text.checkpointFrequency",
                          "Text.CheckpointFrequency", 0, 0, (int)TEXTSW_INFINITY);
	base_frame = xv_create((Xv_window)NULL, FRAME,
	    FRAME_NO_CONFIRM,		TRUE,
	    FRAME_ICON,			edit_icon,
	    FRAME_LABEL,		"Text Editor",
	    FRAME_SHOW_LABEL,		TRUE,
	    FRAME_CMDLINE_HELP_PROC,	my_frame_help,
#ifdef ecd.help
	    HELP_DATA,			"sunview:textedit",
#endif
	    0);

	/*
	 * Set icon's font to system font [if user hasn't set icon font],
	 * but AFTER xv_create has a chance to change it from the
	 * built-in font.
	 * If the user supplies a label, use it and don't override
	 * with our's later.
	 * Note that we get the icon from the Frame in case user
	 * over-rides via argc, argv!
	 */
	edit_icon = xv_get(base_frame, FRAME_ICON);
	user_label = (int)xv_get(edit_icon, XV_LABEL);
	if (!icon_get(edit_icon, ICON_FONT)) {
	    (void)icon_set(edit_icon, ICON_FONT, pw_pfsysopen(), 0);
	    if (!icon_get(edit_icon, ICON_FONT))
		die("Cannot get default font.\n", (char *)NULL, (char *)NULL);
	    (void)xv_set(base_frame, FRAME_ICON, edit_icon, 0);
	}

	/*
	 * Pick up command line arguments to modify textsw behavior.
	 * Notes: FRAME_ARGC_PTR_ARGV above has stripped window flags.
	 *        case OPTION_MARGIN is used to compute WIN_WIDTH.
	 */
#ifndef lint
	margin = (int)textsw_get_from_defaults(TEXTSW_LEFT_MARGIN);
#endif
	argc--; argv++;				/* Skip the cmd name */
	while ((argc--) && (attrc < ATTR_STANDARD_SIZE)) {

	    strcat(cmdline_args, argv[0]);
	    strcat(cmdline_args, " ");

	    if (argv[0][0] == '-') {
		extern int	match_in_table();
		int		option =
				match_in_table(&(argv[0][1]), option_names);
		if (option < 0 || option >= optioncount) {
		    die(argv[0], " is not a valid option.\n", (char *)NULL);
		}
		switch (1<<(option/2)) {
		    case OPTION_NUMBER_OF_LINES:
			if (argc > 0) {
			    number_of_lines = atoi(argv[1]);
	    		    strcat(cmdline_args, argv[1]);
	    		    strcat(cmdline_args, " ");
			    argc--, argv++;
			}
			break;
		    case OPTION_READ_ONLY:
		 	read_only = 1;
		    	if ((argc > 0) && (argv[1][0] != '-')) {
	    		    strcat(cmdline_args, argv[1]);
	    		    strcat(cmdline_args, " ");
			    argc--, argv++;
			    read_only = !off(argv[0]);
                            file_to_edit = argv[0];
			}
			break;
			
		    case OPTION_AUTO_INDENT:
			textsw_attrs[attrc++] = (caddr_t) TEXTSW_AUTO_INDENT;
			textsw_attrs[attrc] = (caddr_t) 1;
			if ((argc > 0) && (argv[1][0] != '-')) {
	    		    strcat(cmdline_args, argv[1]);
	    		    strcat(cmdline_args, " ");
			    argc--, argv++;
			    textsw_attrs[attrc] = (caddr_t) !off(argv[0]);
			}
			attrc++;
			break;
		    case OPTION_ALWAYS_OVERWRITE:
		 	textsw_attrs[attrc++] =
		 		(caddr_t) TEXTSW_CONFIRM_OVERWRITE;
		 	textsw_attrs[attrc] = (caddr_t) 0;
		    	if ((argc > 0) && (argv[1][0] != '-')) {
	    		    strcat(cmdline_args, argv[1]);
	    		    strcat(cmdline_args, " ");
			    argc--, argv++;
			    textsw_attrs[attrc] = (caddr_t) off(argv[0]);
			}
			attrc++;
			break;
#ifdef DEBUG
		    case OPTION_EDIT_LOG_WRAPS_AT:
			GET_INT_ATTR_VAL(edit_log_wraps_at)
			break;
#endif
		    case OPTION_LOWER_CONTEXT:
			textsw_attrs[attrc++] = (caddr_t) TEXTSW_LOWER_CONTEXT;
			GET_INT_ATTR_VAL(textsw_attrs[attrc++])
			break;
#ifdef DEBUG
		    case OPTION_MALLOC_DEBUG_LEVEL:
			textsw_attrs[attrc++] =
				(caddr_t) TEXTSW_MALLOC_DEBUG_LEVEL;
			GET_INT_ATTR_VAL(textsw_attrs[attrc++])
			break;
#endif
		    case OPTION_MARGIN:
			textsw_attrs[attrc++] =
				(caddr_t) TEXTSW_LEFT_MARGIN;
			margin = atoi(argv[1]);
			GET_INT_ATTR_VAL(textsw_attrs[attrc++])
			break;
		    case OPTION_MULTI_CLICK_SPACE:
			textsw_attrs[attrc++] =
				(caddr_t) TEXTSW_MULTI_CLICK_SPACE;
			GET_INT_ATTR_VAL(textsw_attrs[attrc++])
			break;
		    case OPTION_MULTI_CLICK_TIMEOUT:
			textsw_attrs[attrc++] =
				(caddr_t) TEXTSW_MULTI_CLICK_TIMEOUT;
			GET_INT_ATTR_VAL(textsw_attrs[attrc++])
			break;
		    case OPTION_TAB_WIDTH:
			textsw_attrs[attrc++] = (caddr_t) TEXTSW_TAB_WIDTH;
			GET_INT_ATTR_VAL(textsw_attrs[attrc++])
			break;
		    case OPTION_UNDO_HISTORY:
			textsw_attrs[attrc++] = (caddr_t) TEXTSW_HISTORY_LIMIT;
			GET_INT_ATTR_VAL(textsw_attrs[attrc++])
			break;
		    case OPTION_UPPER_CONTEXT:
			textsw_attrs[attrc++] = (caddr_t) TEXTSW_UPPER_CONTEXT;
			GET_INT_ATTR_VAL(textsw_attrs[attrc++])
			break;
		    case OPTION_CHECKPOINT_FREQUENCY:
			if (argc > 0) {
	    		    strcat(cmdline_args, argv[1]);
	    		    strcat(cmdline_args, " ");
			    checkpoint = atoi(argv[1]);
			    argc--, argv++;
			}
			break;
		    case OPTION_NUM_COLS:
			if (argc > 0) {
			    num_cols = atoi(argv[1]);
	    		    strcat(cmdline_args, argv[1]);
	    		    strcat(cmdline_args, " ");
			    argc--, argv++;
			}
			break;
		    default:
			die("Unrecognized command line option.", (char *)NULL, (char *)NULL);
			break;
		}
	    } else if (file_to_edit == NULL) {
		file_to_edit = argv[0];
	    } else {
		die("Too many files specified.", (char *)NULL, (char *)NULL);
	    }

	    argv++;
	}
	textsw_attrs[attrc] = 0;	/* A-V list terminator */

	xv_set(base_frame,
		WIN_CMD_LINE, cmdline_args,
		0);
		
	read_only = (read_only & (file_to_edit != NULL));
#ifdef DEBUG
	if (edit_log_wraps_at != (caddr_t)TEXTSW_INFINITY) {
	    textsw_attrs[attrc++] = (caddr_t) TEXTSW_WRAPAROUND_SIZE;
	    textsw_attrs[attrc++] = edit_log_wraps_at;
	    textsw_attrs[attrc] = 0;
	}
#endif
	if ((file_to_edit != NULL) && (stat(file_to_edit, &stb) < 0)) 
	{
		char 	buf[256];
		int	fd;
		int	alert_result;
		Event	event;

		sprintf(buf, "The file '%s' does not exist", file_to_edit);
		alert_result = (int) notice_prompt(base_frame, &event,
			NOTICE_MESSAGE_STRINGS,
			buf,
			"Please confirm creation of new",
			"file for textedit.",
			0,
			NOTICE_BUTTON_NO, "Cancel",
			NOTICE_BUTTON_YES, "Confirm",
			0);

		if (alert_result == NOTICE_YES)
			if ((fd = creat(file_to_edit, 0777)) == -1)
				die("Could not create new file.", (char *)NULL, (char *)NULL);
			else
				close(fd);
		else
			exit(0);


	}
	else if ((file_to_edit != NULL) && (stat(file_to_edit, &stb) == 0)) 
	{
		char 	buf[256];
		int	fd;
		int	alert_result;
		Event	event;

		/* see if it was a regular file */

		if (!(stb.st_mode & S_IFREG))
		{
	
			sprintf(buf, "The file '%s' is not a regular file", file_to_edit);
			alert_result = (int) notice_prompt(base_frame, &event,
				NOTICE_MESSAGE_STRINGS,
				buf,
				"Please confirm creation of new",
				"file for textedit.",
				0,
				NOTICE_BUTTON_YES, "Continue",
				NOTICE_BUTTON_NO, "Cancel",
				0);
	
			if (alert_result == NOTICE_YES)
				file_to_edit = NULL;
			else
				die("no file to edit", NULL, NULL);
		}
	}

	/*
	 * Create subwindows
	 */
	
	panel = xv_create(base_frame, PANEL,
		PANEL_LAYOUT,	PANEL_HORIZONTAL,
		XV_HELP_DATA,			"textedit:Panel",
		0);
		
	
	textsw = (Textsw)xv_create(base_frame, TEXTSW,
		ATTR_LIST,			textsw_attrs,
		WIN_IS_CLIENT_PANE,
		TEXTSW_STATUS,			&status,
		TEXTSW_READ_ONLY,		read_only,
		TEXTSW_FILE,			file_to_edit,
		TEXTSW_NOTIFY_PROC,		set_name_frame,
		TEXTSW_CHECKPOINT_FREQUENCY,	checkpoint,
		XV_HELP_DATA,			"textedit:Textsw",
		OPENWIN_SPLIT,
			OPENWIN_SPLIT_INIT_PROC, textsw_split_proc,
		NULL,
	    	0);

	if (!user_set_size)
	{
		xv_set(textsw,
                	WIN_ROWS,     (number_of_lines) ? number_of_lines : 45,
                	WIN_COLUMNS,  (num_cols) ? num_cols : 80,
	    		0);
	}

	/* force the panel to be some reasonable size, so that the 
	   buttons lay out reasonably. */

	xv_set(panel, WIN_WIDTH, 800, 0);

	/* Panel item and fitting height here is for performance reason */
	/* This way the textsw will not require to readjust the rect */
	file_panel_menu = (Menu)xv_get(textsw, TEXTSW_SUBMENU_FILE);

	file_panel_item = xv_create(panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,		"File",
		PANEL_ITEM_MENU, 		file_panel_menu, 
		XV_HELP_DATA,			"textedit:File",
		PANEL_NOTIFY_PROC,		button_notify_proc,
		0);
		
	textswwidth = (int)xv_get(textsw, WIN_WIDTH);

	(void)window_fit_height(panel);

	xv_set(textsw, WIN_BELOW, panel, 0);

	switch (status) {
	  case TEXTSW_STATUS_CANNOT_OPEN_INPUT:
	    die("Cannot open file '", file_to_edit, "', exiting!\n");
	  case TEXTSW_STATUS_OKAY:
	    if (textsw)
		break;
	    /* else fall through */
	  default:
	    die("Cannot create textsw, exiting!\n", (char *)NULL, (char *)NULL);
	}
	/*
	 * Setup signal handlers.
	 */
	(void)notify_set_signal_func(base_frame, mysigproc, SIGINT,  NOTIFY_ASYNC);
	(void)notify_set_signal_func(base_frame, mysigproc, SIGXCPU, NOTIFY_ASYNC);
	(void)notify_set_signal_func(base_frame, mysigproc, SIGBUS,  NOTIFY_ASYNC);
	(void)notify_set_signal_func(base_frame, mysigproc, SIGHUP,  NOTIFY_ASYNC);
	(void)notify_set_signal_func(base_frame, mysigproc, SIGILL,  NOTIFY_ASYNC);
	(void)notify_set_signal_func(base_frame, mysigproc, SIGSEGV, NOTIFY_ASYNC);
	(void)notify_set_signal_func(base_frame, mysigproc, SIGFPE,  NOTIFY_ASYNC);
	/*
	 * Install us in tree of windows
	 */
	 
	PANEL_ITEM_DATA = xv_unique_key();

	display_panel_menu = (Menu)xv_get(textsw, TEXTSW_SUBMENU_VIEW);
	edit_panel_menu = (Menu)xv_get(textsw, TEXTSW_SUBMENU_EDIT);
	find_panel_menu = (Menu)xv_get(textsw, TEXTSW_SUBMENU_FIND);
	 
	 /* This set has to be done before the other panel items are created */
	xv_set(file_panel_item,
		XV_KEY_DATA, 			PANEL_ITEM_DATA, textsw,
		0);
		
	display_panel_item = xv_create(panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,		"View",
		PANEL_NOTIFY_PROC,		button_notify_proc,
		PANEL_ITEM_MENU, 		display_panel_menu,
		XV_HELP_DATA,			"textedit:View",
		XV_KEY_DATA, 			PANEL_ITEM_DATA, textsw,
		0);

	edit_panel_item = xv_create(panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,		"Edit",
		PANEL_NOTIFY_PROC,		button_notify_proc,
		PANEL_ITEM_MENU, 		edit_panel_menu, 
		XV_HELP_DATA,			"textedit:Edit",
		XV_KEY_DATA, 			PANEL_ITEM_DATA, textsw,
		0);
		
	find_panel_item = xv_create(panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,		"Find",
		PANEL_NOTIFY_PROC,		button_notify_proc,
		PANEL_ITEM_MENU, 		find_panel_menu, 
		XV_HELP_DATA,			"textedit:Find",
		XV_KEY_DATA, 			PANEL_ITEM_DATA, textsw,
		0);	
	


	if (!user_set_size)
	{
		xv_set(panel, WIN_WIDTH, textswwidth, 0);
        	(void)window_fit(base_frame);
	}
	else
	{
		/* restore the panel width before coming up */
		xv_set(panel, WIN_WIDTH, WIN_EXTEND_TO_EDGE, 0);
	}

	/* set up to catch load events */

	notify_interpose_event_func(textsw_first(textsw), textedit_load_event_proc, NOTIFY_SAFE);
	notify_interpose_event_func(edit_icon, textedit_load_event_proc, NOTIFY_SAFE);

	if (!user_label && file_to_edit)
	{
		xv_set(edit_icon, XV_LABEL, file_to_edit, 0);
		xv_set(base_frame, FRAME_ICON, edit_icon, 0);
	}

	xv_main_loop(base_frame);
	exit(0);
}

/*
 *	SIGNAL handlers
 */

/* ARGSUSED */
static Notify_value
mysigproc(me, sig, when)
	Notify_client		 me;
	int			 sig;
	Notify_signal_mode	 when;
{
	char			 name_to_use[MAXNAMLEN];
	int			 pid = getpid();
	int			 was_SIGILL = (sig == SIGILL);
	struct sigvec vec;

	if (handling_signal == 2)
	    _exit(3);
	if (handling_signal++ == 1) {
	    (void)fprintf(stderr, "Signal catcher called recursively: ");
	    goto Die;
	}
	if (sig == SIGINT) {
	    if (xv_get((Xv_window)(textsw), TEXTSW_MODIFIED)) {
		(void)xv_destroy(base_frame);	/* It will be vetoed */
		handling_signal = 0;
	    } else {
		/* Skip more user confirmation - just die (but cleanly)! */
		(void) notify_post_destroy(base_frame, DESTROY_PROCESS_DEATH,
					   NOTIFY_IMMEDIATE);
		(void) notify_stop();
	    }
	    return(NOTIFY_DONE);
	}
	(void)sprintf(name_to_use, "textedit.%d", pid);
	(void)fprintf(stderr, "attempting Store to %s ... ", name_to_use);
	(void)fflush(stderr);
	if (textsw_store_file(textsw, name_to_use, 0, 0) == 0)
	    goto Done;
	(void)sprintf(name_to_use, "/usr/tmp/textedit.%d", pid);
	(void)fprintf(stderr, "failed!\nAttempting Store to %s ... ", name_to_use);
	(void)fflush(stderr);
	if (textsw_store_file(textsw, name_to_use, 0, 0) == 0)
	    goto Done;
	(void)sprintf(name_to_use, "/tmp/textedit.%d", pid);
	(void)fprintf(stderr, "failed!\nAttempting Store to %s ... ", name_to_use);
	(void)fflush(stderr);
	if (textsw_store_file(textsw, name_to_use, 0, 0) == 0)
	    goto Done;
	(void)fprintf(stderr, "failed!\nSorry, cannot save your edits: ");
	    goto Die;
Done:
	(void)fprintf(stderr, "finished; ");
Die:
	(void)fprintf(stderr, "aborting for post-mortem ...\n");
	(void)fflush(stderr);
	(void)sigsetmask(0);		/* Make sure signals get through */
	if (was_SIGILL) {
#ifndef lint
	    char	dummy, *bad_ptr = 0;
	    /* (void)signal(SIGSEGV, SIG_DFL);	/* Make sure 0 deref dumps. */
	    vec.sv_handler = SIG_DFL;
	    vec.sv_mask = vec.sv_onstack = 0;
	    sigvec(SIGSEGV, &vec, 0);

	    dummy = *bad_ptr;
#endif
	} else {
	    /* (void)signal(SIGILL, SIG_DFL);	/* Make sure abort() dumps. */
            vec.sv_handler = SIG_DFL; 
	    vec.sv_mask = vec.sv_onstack = 0; 
            sigvec(SIGILL, &vec, 0);

	    abort();
	}
	return(NOTIFY_DONE);
}


/*
 * Misc. utilities
 */
static
die(msg1, msg2, msg3)
	char	*msg1, *msg2, *msg3;
{
	char	*dummy = "";
	(void)fprintf(stderr, "%s: %s%s%s\n", cmd_name, msg1,
			(msg2?msg2:dummy), (msg3?msg3:dummy));
	exit(4);
}

static int
off(str)
	char	*str;
{
    return ((strcmp(str, "off") == 0) ||
	    (strcmp(str, "Off") == 0) ||
	    (strcmp(str, "OFF") == 0)
	   );
}

