#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)server.c 20.128 91/10/04";
#endif
#endif

/*
 *	(c) Copyright 1989 Sun Microsystems, Inc. Sun design patents 
 *	pending in the U.S. and foreign countries. See LEGAL NOTICE 
 *	file for terms of the license.
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/param.h>
#include <dirent.h>
#include <stdio.h>
#include <string.h>
#include <xview/win_input.h>
#include <xview/win_struct.h>
#include <xview_private/ntfy.h>
#include <xview_private/ndet.h>
#include <xview/notify.h>
#include <xview/win_notify.h>
#include <xview/defaults.h>
#include <X11/Xlib.h>
#include <xview_private/i18n_impl.h>
#include <xview_private/portable.h>
#include <xview_private/svr_atom.h>
#include <xview_private/svr_impl.h>
#include <xview_private/svr_kmdata.h>
#include <xview_private/draw_impl.h>
#include <xview_private/xv_color.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/Xresource.h>

#define	LIB_LOCALE	"/lib/locale/"

#ifdef _XV_DEBUG
Xv_private_data int server_gather_stats;
#endif

static void		 load_kbd_cmds();
static void     	 server_init_atoms();
static void		 destroy_atoms();
static Notify_value 	 scheduler();
static unsigned int 	 string_to_modmask();
static Server_atom_type  save_atom();

Xv_private Notify_value  xv_input_pending();
Xv_private void 	 xv_do_enqueued_input();
Xv_private void		 xv_merge_cmdline();
#ifdef OS_HAS_LOCALE
static void		 server_setlocale(),
			 server_setlocale_defaults(),
			 server_null_locale();
#endif OS_HAS_LOCALE

Xv_private int 	    	 xv_has_been_initialized();
Pkg_private void 	 server_refresh_modifiers();

extern char	    	*setlocale();
char		    	*strdup();
XrmDatabase 	    	 XrmGetFileDatabase();
static Notify_func 	 default_scheduler;
extern XrmDatabase  	 defaults_rdb;
extern char	    	 *xv_app_name;
extern char    		*getenv();
Xv_private_data char 	*shell_prompt;

/* global default server parameters */
Xv_private_data Xv_Server xv_default_server;
Xv_private_data Xv_Screen xv_default_screen;
Xv_private_data Display *xv_default_display;

/* Global data */
Xv_private_data Defaults_pairs xv_kbd_cmds_value_pairs[4] = {
    "SunView1", KBD_CMDS_SUNVIEW1,
    "Basic", KBD_CMDS_BASIC,
    "Full", KBD_CMDS_FULL,
    NULL, KBD_CMDS_SUNVIEW1	/* default */
};


static void
load_kbd_cmds(server, kb_table)
    Server_info    *server;
    Key_binding	   *kb_table;
{
    int             i;
    int             j;
    KeySym          keysym;
    char           *keysym_string;
    char           *mapping[MAX_NBR_MAPPINGS];
                        /* A key mapping for one keyboard command.
                         * Format: KeysymName[+Modifer...]
                         */
    char           *modifier;
    int             offset;
    char           *value;
    char	   buffer[100];

    /* Load keyboard commands into keymaps */
    for (i = 0; kb_table[i].action; i++) {
	value = defaults_get_string(kb_table[i].name, kb_table[i].name,
				    kb_table[i].value);
	strcpy(buffer, value);
	value = buffer;
	mapping[0] = strtok(value, ",");
	for (j = 1; j < MAX_NBR_MAPPINGS; j++) {
	    mapping[j] = strtok(NULL, ",");
	    if (mapping[j] == NULL)
		break;
	}

	for (j = 0; j < MAX_NBR_MAPPINGS && mapping[j]; j++) {
	    offset = 0;
	    keysym_string = strtok(mapping[j], "+");
	    if (!keysym_string)
		continue;  /* Error in resource value: ignore */
	    keysym = XStringToKeysym(keysym_string);
	    if (keysym == 0)
		continue;  /* Error in resource value: ignore */
	    do {
		modifier = strtok(NULL, "+");
		if (modifier) {
		    if (!strcmp(modifier, "Ctrl"))
			offset += 0x100;
		    else if (!strcmp(modifier, "Meta"))
			offset += 0x200;
		    else if (!strcmp(modifier, "Alt"))
			offset += 0x400;
		    else if (!strcmp(modifier, "Shift"))
			offset += 0x800;
		}
	    } while (modifier);
	    if ((keysym & KEYBOARD_KYSM_MASK) == KEYBOARD_KYSM) {
		server->sem_map[(keysym & 0xFF) + offset] =
		    kb_table[i].action & 0xFF;
		if (offset == 0) {
		    if (kb_table[i].action == ACTION_CUT)
			server->cut_keysym = keysym;
		    if (kb_table[i].action == ACTION_PASTE)
			server->paste_keysym = keysym;
		}
	    } else
		server->ascii_map[(keysym & 0xFF) + offset] =
		    kb_table[i].action & 0xFF;
	}  /* for each mapping */
    }  /* for each key table entry */
}


static void
server_build_keymap_table(server)
    Server_info    *server;
{
    Kbd_cmds_value kbd_cmds_value;
    extern unsigned int win_keymap[];

    server->xv_map = win_keymap;
    server->sem_map = (unsigned char *)xv_calloc(0x1600, sizeof(unsigned char));
    server->ascii_map = (unsigned char *)xv_calloc(0x800,sizeof(unsigned char));

    /* Load requested keyboard commands into keymaps */
    load_kbd_cmds(server, sunview1_kbd_cmds);
    kbd_cmds_value = defaults_get_enum("openWindows.keyboardCommands",
	"OpenWindows.KeyboardCommands", xv_kbd_cmds_value_pairs);
    if (kbd_cmds_value >= KBD_CMDS_BASIC)
	load_kbd_cmds(server, basic_kbd_cmds);
    if (kbd_cmds_value == KBD_CMDS_FULL)
	load_kbd_cmds(server, full_kbd_cmds);
}


static int
svr_parse_display(display_name)
char *display_name;
{
 
	 /*
	 * The following code stolen form XConnectDisplay to parse the
	 * string and return the default screen number or 0.
	 */
	 char displaybuf[256];       /* Display string buffer */
	 register char *display_ptr; /* Display string buffer pointer */
	 register char *numbuf_ptr;  /* Server number buffer pointer */
	 char *screen_ptr;       /* Pointer for locating screen num */
	 char numberbuf[16];
	 char *dot_ptr = NULL;       /* Pointer to . before screen num */
	 /*
	 * Find the ':' seperator and extract the hostname and the
	 * display number.
	 * NOTE - if DECnet is to be used, the display name is formatted
	 * as "host::number"
	 */
	 (void) strncpy(displaybuf, display_name, sizeof(displaybuf));
	 if ((display_ptr = XV_INDEX(displaybuf,':')) == NULL) return (-1);
	 *(display_ptr++) = '\0';
 
	 /* displaybuf now contains only a null-terminated host name, and
	 * display_ptr points to the display number.
	 * If the display number is missing there is an error. */
		  
	if (*display_ptr == '\0') return(-1);

	/*
	* Build a string of the form <display-number>.<screen-number> in
	* numberbuf, using ".0" as the default.
	*/
	screen_ptr = display_ptr;       /* points to #.#.propname */
	numbuf_ptr = numberbuf;         /* beginning of buffer */
	while (*screen_ptr != '\0') {
		if (*screen_ptr == '.') {       /* either screen or prop */
			if (dot_ptr) {          /* then found prop_name */
				screen_ptr++;
				break;
			}
			dot_ptr = numbuf_ptr;       /* found screen_num */
			*(screen_ptr++) = '\0';
			*(numbuf_ptr++) = '.';
		} else {
			*(numbuf_ptr++) = *(screen_ptr++);
		}
	}
	
	/*
	 * If the spec doesn't include a screen number, add ".0" (or "0" if
         * only "." is present.)
	 */
	if (dot_ptr == NULL) {          /* no screen num or prop */
		dot_ptr = numbuf_ptr;
		*(numbuf_ptr++) = '.';
		*(numbuf_ptr++) = '0';
	} else {
		if (*(numbuf_ptr - 1) == '.')
			*(numbuf_ptr++) = '0';
	}
	*numbuf_ptr = '\0';

	/*
	* Return the screen number
	*/
	return(atoi(dot_ptr + 1));
}

/*ARGSUSED*/
Pkg_private int
server_init(parent, server_public, avlist)
    Xv_opaque       parent;
    Xv_Screen       server_public;
    Attr_avlist     avlist;
{
    register Server_info	*server = (Server_info *) NULL;
    Xv_server_struct 		*server_object;
    Attr_avlist    		 attrs;
    char           		*home,	        /* pathname to home directory */
    				*server_name = NULL,
    				 filename[MAXPATHLEN],
				*xv_message_dir;
    unsigned char		 pmap[256];      /* pointer mapping list */
    int        			 default_screen_num;
    Server_atom_list		*atom_list;
    XrmDatabase			 new_db;
    int				first_server = FALSE;
    extern int			 _xv_use_locale;

    for (attrs = avlist; *attrs; attrs = attr_next(attrs)) {
	switch (attrs[0]) {
	  case XV_NAME:
	    server_name = (char *) attrs[1];
	    *attrs = ATTR_NOP(*attrs);
	    break;
	  default:
	    break;
	}
    }
    if (!server_name)
    	server_name = (char *) defaults_get_string("server.name",
					       	   "Server.Name",
					           getenv("DISPLAY"));

    /* Allocate private data and set up forward/backward links. */
    server = (Server_info *) xv_alloc(Server_info);
    server->public_self = server_public;
    server_object = (Xv_server_struct *) server_public;
    server_object->private_data = (Xv_opaque) server;

    server->display_name = xv_strsave( server_name ? server_name : ":0" );

    if (!(server->xdisplay = (Display *)server_init_x(server->display_name))) {
	goto Error_Return;
    }
    
    if (notify_set_input_func((Notify_client)server->xdisplay, xv_input_pending,
			      XConnectionNumber(server->xdisplay))
	== NOTIFY_FUNC_NULL) {
	notify_perror("server_init");
	goto Error_Return;
    }

    /* Screen creation requires default server to be set. */
    if (!xv_default_server) {
	xv_default_server = server_public;
	xv_default_display = (Display *)server->xdisplay;
	first_server = TRUE;
    }
    /*
     * Now that a server connection has been established, initialize the
     * defaults database. Note - This assumes that server_init will be called
     * only once per X11 server.
     */
    defaults_init_db();		/* init Resource Manager */
    /*
     *  BUG ALERT(isa) - Sundae buyback
     *  The following code replaces:
     *        defaults_load_db(filename);
     *        defaults_load_db((char *) NULL);
     *
     *  This will set defaults_rdb to always be the merge of
     *  .Xdefaults and the latest server xv_creat'd
     *
     *  Create a database from .Xdefaults and from the server property
     *  and stash its XID in the server. This used to be done in the
     *  Xv_database object, which has been removed pending design review
     */

    /* See if defaults have been loaded on server */
    if (((Display *)server->xdisplay)->xdefaults) {
	server->db = XrmGetStringDatabase(
				((Display *)server->xdisplay)->xdefaults);
    } else {
	/* Get the resources from the users .Xdefaults file */
        if (home = getenv("HOME"))
	    (void) strcpy(filename, home);
        else
	    filename[0] = NULL;
        (void) strcat(filename, "/.Xdefaults");
        server->db = XrmGetFileDatabase(filename);
    }

    /*
     * Check if this is the first server being created, and if the merged
     * db already exists.
     * If yes, then merge the newly created server->db into the existing db,
     * and make server->db point to that
     */
    if (first_server && defaults_rdb)  {
	XrmMergeDatabases(server->db, &defaults_rdb);
	server->db = defaults_rdb;
    }

    /*
     * Merge cmdline options into database
     * Note:
     * For the first server object created, this is actually
     * done twice. Once in xv_parse_cmdline() (in xv_init) and
     * once here.
     *
     * xv_merge_cmdline() has to be called in xv_parse_cmdline()
     * because it is a public function, and whoever calls it
     * expects the cmdline options to be merged into the
     * database.
     *
     * xv_merge_cmdline() has to be called here to make sure cmdline
     * options are merged into the server resource database.
     * (we cannot depend on xv_parse_cmdline() being called before
     * every server creation)
     */
    xv_merge_cmdline(&server->db);

    /*
     * Point defaults_rdb to db of most current server created
     */
    defaults_rdb = server->db;

    /*
     *  Retrieve the locale announcers from the environment
     */

    server->basiclocale = NULL;
    server->displaylang = NULL;
    server->inputlang = NULL;
    server->numeric = NULL;
    server->timeformat = NULL;
    server->localedir = NULL;

#ifdef OS_HAS_LOCALE
    if (_xv_use_locale) {
	int	lc_ctype_read = FALSE;

	server_setlocale(server);

	if (getenv("LC_CTYPE"))  {
	    lc_ctype_read = TRUE;
	}

	/*
	 *  Now that we have the locale announcers from the environment
	 *  override w/ the locale information from the defaults 
	 */

	server_setlocale_defaults(server, &lc_ctype_read);
	
	/*
	 *  Override locale from env and Xrm w/ function parameters
	 */

	for (attrs = avlist; *attrs; attrs = attr_next(attrs)) {
	switch (attrs[0]) {
	      case XV_LC_BASIC_LOCALE:
		if (attrs[1]) {
		    if (server->basiclocale) {
			free(server->basiclocale);
		    }
	            lc_ctype_read = FALSE;
		    server->basiclocale = strdup((char *)attrs[1]);
		}
		break;

	      case XV_LC_DISPLAY_LANG:
		if (attrs[1]) {
		    if (server->displaylang) {
			free(server->displaylang);
		    }
		    server->displaylang = strdup((char *)attrs[1]);
		}
		break;

	      case XV_LC_INPUT_LANG:
		if (attrs[1]) {
		    if (server->inputlang) {
			free(server->inputlang);
		    }
		    server->inputlang = strdup((char *)attrs[1]);
		}
		break;

	      case XV_LC_NUMERIC:
		if (attrs[1]) {
		    if (server->numeric) {
			free(server->numeric);
		    }
		    server->numeric = strdup((char *)attrs[1]);
		}
		break;

	      case XV_LC_TIME_FORMAT:
		if (attrs[1]) {
		    if (server->timeformat) {
			free(server->timeformat);
		    }
		    server->timeformat = strdup((char *)attrs[1]);
		}
		break;

	      case XV_LOCALE_DIR:
		if (attrs[1]) {
		    if (server->localedir) {
			free(server->localedir);
		    }
		    server->localedir = strdup((char *)attrs[1]);
		}
		break;

	      default:
		break;
	    }
	}

	/*
	 * For any NULL locales, fill with default locale ("C")
	 */
        server_null_locale(server);


        if (!lc_ctype_read && (setlocale(LC_ALL, server->basiclocale) == NULL)) {
	        xv_error(NULL,
		     ERROR_STRING, 
		     XV_MSG("Error when setting locale category LC_ALL"),
		     ERROR_PKG, SERVER,
		     0);
                free(server->basiclocale);
                server->basiclocale = strdup(setlocale(LC_ALL, (char *)0));
        }
        if (setlocale(LC_CTYPE, server->basiclocale) == NULL) {
	        xv_error(NULL,
		     ERROR_STRING, 
		     XV_MSG("Error when setting locale category LC_CTYPE"),
		     ERROR_PKG, SERVER,
		     0);
                free(server->basiclocale);   
                server->basiclocale = strdup(setlocale(LC_CTYPE, (char *)0));
        }
        if (setlocale(LC_MESSAGES, server->displaylang) == NULL) {
	        xv_error(NULL,
		     ERROR_STRING, 
		     XV_MSG("Error when setting locale category LC_MESSAGES"),
		     ERROR_PKG, SERVER,
		     0);
                free(server->displaylang);   
                server->displaylang = strdup(setlocale(LC_MESSAGES, (char *)0));
        }
        if (setlocale(LC_NUMERIC, server->numeric) == NULL) {
	        xv_error(NULL,
		     ERROR_STRING, 
		     XV_MSG("Error when setting locale category LC_NUMERIC"),
		     ERROR_PKG, SERVER,
		     0);
                free(server->numeric); 
                server->numeric = strdup(setlocale(LC_NUMERIC, (char *)0));
        }
        if (setlocale(LC_TIME, server->timeformat) == NULL) {
	        xv_error(NULL,
		     ERROR_STRING, 
		     XV_MSG("Error when setting locale category LC_TIME"),
		     ERROR_PKG, SERVER,
		     0);
                free(server->timeformat);
                server->timeformat = strdup(setlocale(LC_TIME, (char *)0));
        }
        if (server->inputlang == NULL)
            server->inputlang = strdup(server->basiclocale);
 
    /*
     * Now that we know the locale, get the local specific resource files,
     * merge with server->db
     */
	
	
	if (home = getenv("OPENWINHOME")) {
	    xv_message_dir =
		xv_malloc(strlen(home)+strlen(LIB_LOCALE)+1);
	    strcpy(xv_message_dir, home);
	    strcat(xv_message_dir, LIB_LOCALE);
	    bindtextdomain(xv_domain, xv_message_dir);
	    if (!server->localedir) {
		server->localedir = xv_message_dir;
	    } else {
		free(xv_message_dir);
	    }
	}

	if (server->localedir && xv_app_name) {
	    char		pathname[MAXPATHLEN];
	    DIR			*dirp;

	    bindtextdomain("", server->localedir);
	    strcpy(pathname, server->localedir);
	    strcat(pathname, "/");
	    strcat(pathname, server->basiclocale);
	    strcat(pathname, "/app-defaults/");
	    strcat(pathname, xv_app_name);
	    strcpy(filename, pathname);
	    strcat(filename, ".db");

	    if (new_db = XrmGetFileDatabase(filename)) {
		XrmMergeDatabases(server->db, &new_db);
		server->db = new_db;
		defaults_rdb = server->db;
	    }

	    /*
	     * If the directory XV_LOCALE_DIR/<basic locale>/app-defaults/xv_app_name
	     * exists, read all the files in it with ".db" suffix
	     */
	    if (dirp = opendir(pathname))  {
	        struct dirent	*dp;

		/*
		 * Read all files in directory
		 */
                for (dp = readdir(dirp); dp != NULL; dp = readdir(dirp))  {
		    struct stat		statbuf;
		    char		*dot;

		    if (!dp->d_name)  {
			continue;
		    }

		    /*
		     * Ignore ".", ".."
		     */
                    if ( (dp->d_name[0] == '.') && 
			    ( ((dp->d_name[1] == '.') && (dp->d_name[2] == '\0')) || 
				(dp->d_name[1] == '\0') ) )  {
                        continue;
		    }

		    /*
		     * Read in only files that have .db suffix
		     */
		    dot = XV_RINDEX(dp->d_name, '.');
		    if (!dot || strcmp(dot, ".db"))  {
			continue;
		    }

		    /*
		     * construct filename
		     */
		    filename[0] = '\0';
	            sprintf(filename, "%s/%s", pathname, dp->d_name);

		    if (!stat(filename, &statbuf))  {
			/*
			 * Read only if ordinary file
			 */
			if ((statbuf.st_mode & S_IFMT) == S_IFREG)  {
	                    if (new_db = XrmGetFileDatabase(filename)) {
		                XrmMergeDatabases(server->db, &new_db);
		                server->db = new_db;
		                defaults_rdb = server->db;
	                    }
			}
		    }
		}

                closedir (dirp);
	    }
	}

    } else {	/* if (_xv_use_locale) */
	server->basiclocale = "C";
	server->displaylang = "C";
	server->inputlang = "C";
	server->numeric = "C";
	server->timeformat = "C";
    }
#endif OS_HAS_LOCALE
    /*
     * End of Sundae buyback code replacement for 
     *    defaults_load_db(filename);
     *    defaults_load_db((char *) NULL);
     */

#ifdef OW_I18N
    (void) sprintf(filename, "%s/%s/%s/xview/defaults",
		   getenv("OPENWINHOME"),
		   LIB_LOCALE,
		   server->basiclocale);
    if (new_db = XrmGetFileDatabase(filename)) {
	/*
	 * Presense order of this new_db is lowest!
	 */
	XrmMergeDatabases(server->db, &new_db);
	defaults_rdb = server->db = new_db;
    }
#endif /* OW_I18N */
    /* Used by atom mgr */
    server->atom_mgr[ATOM] = (XID) XAllocID((Display *)server->xdisplay);
    server->atom_mgr[NAME] = (XID) XAllocID((Display *)server->xdisplay);
    server->atom_mgr[TYPE] = (XID) XAllocID((Display *)server->xdisplay);
    server->atom_mgr[DATA] = (XID) XAllocID((Display *)server->xdisplay);

    /* Key for XV_KEY_DATA.  Used in local dnd ops. */
    server->dnd_ack_key = xv_unique_key();

    /* Key for XV_KEY_DATA.  Used for storing the atom list struct. */
    server->atom_list_head_key = xv_unique_key();
    server->atom_list_tail_key = xv_unique_key();
    server->atom_list_number = 0;

    /* We allocate the first block of atoms, others may be allocated as
     * the need arises.
     */
    atom_list = xv_alloc(Server_atom_list);

    XV_SL_INIT(atom_list);

    /* Store away the block of atom storage.  This will be used by the
     * atom manager.
     */
    xv_set(SERVER_PUBLIC(server), XV_KEY_DATA, server->atom_list_head_key,
                                               atom_list, 0);
    xv_set(SERVER_PUBLIC(server), XV_KEY_DATA, server->atom_list_tail_key,
                                               atom_list, 0);

    server_init_atoms(server_public);

    server->idproclist = NULL;
    server->xidlist    = NULL;

    default_screen_num = svr_parse_display(server->display_name);

    server->screens[default_screen_num] = xv_create(server_public, SCREEN,
		    SCREEN_NUMBER, default_screen_num,
		    0);

    if (!server->screens[default_screen_num]) {
	    goto Error_Return;
    }

    /* Create keycode maps */
    (void) server_build_keymap_table(server);

    if (xv_default_server != server_public) {
	(void) XV_SL_ADD_AFTER(SERVER_PRIVATE(xv_default_server),
			SERVER_PRIVATE(xv_default_server), server);
    } else {
	XV_SL_INIT(server);
	xv_default_screen = server->screens[default_screen_num];
	(void) xv_set_scheduler();
    }

    selection_init_agent(server_public, server->screens[default_screen_num]);
    server_refresh_modifiers(SERVER_PUBLIC(server));

    server->chording_timeout =
	defaults_get_integer("OpenWindows.MouseChordTimeout",
			     "OpenWindows.MouseChordTimeout", 100);
    server->chord_menu = defaults_get_boolean("OpenWindows.MouseChordMenu",
					      "OpenWindows.MouseChordMenu",
					      FALSE);


    /* Be prepared to handle a mouse with only one or two physical buttons */
    server->nbuttons = XGetPointerMapping(server->xdisplay, pmap, 256);
    if (server->nbuttons < 3) 
	server->but_two_mod =
	      string_to_modmask(defaults_get_string("mouse.modifier.button2",
						    "Mouse.Modifier.Button2",
						    "Shift"));
    
    if (server->nbuttons < 2) 
	server->but_three_mod =
	      string_to_modmask(defaults_get_string("mouse.modifier.button3",
						    "Mouse.Modifier.Button3",
						    "Ctrl"));
    server->composestatus = (XComposeStatus *)xv_alloc(XComposeStatus);
    server->composestatus->compose_ptr = (char *)NULL;
    server->composestatus->chars_matched = 0;

#ifdef OW_I18N
    if (strcmp(server->inputlang, "C") != 0) {
        /* first release only one input method supported, hence this
           check; when we support multiple locale, each with an input
           method, we should save the LC_CTYPE locale, set it what is
           specified by inputlang, then re-set the saved LC_CTYPE locale */
        if (strcmp(server->inputlang,setlocale(LC_CTYPE,NULL))==0)
            (XIM) server->xim =
                (XIM) XOpenIM(server->xdisplay, server->inputlang);
        else
            fprintf(stderr, "inputlang is different from LC_CTYPE!\n");
    }
#endif OW_I18N

    return XV_OK;

Error_Return:
    if (server) {
	if (xv_default_server == server_public) {
	    xv_default_server = (Xv_Server) NULL;
	}
	free((char *) server);
    }
    return XV_ERROR;
}


#ifdef OS_HAS_LOCALE
static void
server_setlocale (server)
    Server_info  *server;
{
    char	*lc_result;

    /*
     *  Set any locale categories that exist in the 
     *  environment in a POSIX compliant fashion.
     */
    (void) setlocale(LC_ALL, "");

    /*
     *  Copy any existing locale announcers into the server struct
     */
    if (lc_result = setlocale(LC_CTYPE, "")) {
	if (server->basiclocale) {
		free(server->basiclocale);
	}
	server->basiclocale = strdup(lc_result);
    }

    if (lc_result = setlocale(LC_MESSAGES, "")) {
	if (server->displaylang) {
		free(server->displaylang);
	}
	server->displaylang = strdup(lc_result);
    }

#ifdef notdef
    server->displaylang = strdup("C");
#endif

    if (lc_result = setlocale(LC_NUMERIC, "")) {
	if (server->numeric) {
		free(server->numeric);
	}
	server->numeric = strdup(lc_result);
    }
    if (lc_result = setlocale(LC_TIME, "")) {
	if (server->timeformat) {
		free(server->timeformat);
	}
	server->timeformat = strdup(lc_result);
    }
}

static void
server_setlocale_defaults (server, lc_ctype_read)
    Server_info  *server;
    int		 *lc_ctype_read;
{
    char	*strtype;
    XrmValue	xrm_result;

    xrm_result.size = 0;
    xrm_result.addr = NULL;

    if (XrmGetResource(server->db, "basicLocale", "basicLocale",
       &strtype, &xrm_result)) {
	if (server->basiclocale) {
		free(server->basiclocale);
	}
	*lc_ctype_read = FALSE;
	server->basiclocale = strdup((char *) xrm_result.addr);
    }

    xrm_result.size = 0;
    xrm_result.addr = NULL;
    if (XrmGetResource(server->db, "displayLang", "displayLang",
       &strtype, &xrm_result)) {
	if (server->displaylang) {
		free(server->displaylang);
	}
	server->displaylang = strdup((char *) xrm_result.addr);
    }

    xrm_result.size = 0;
    xrm_result.addr = NULL;
    if (XrmGetResource(server->db, "inputLang", "inputLang",
       &strtype, &xrm_result)) {
	if (server->inputlang) {
		free(server->inputlang);
	}
	server->inputlang = strdup((char *) xrm_result.addr);
    }
    
    xrm_result.size = 0;
    xrm_result.addr = NULL;
    if (XrmGetResource(server->db, "numeric", "numeric",
       &strtype, &xrm_result)) {
	if (server->numeric) {
		free(server->numeric);
	}
	server->numeric = strdup((char *) xrm_result.addr);
    }

    xrm_result.size = 0;
    xrm_result.addr = NULL;
    if (XrmGetResource(server->db, "timeFormat", "timeFormat",
       &strtype, &xrm_result)) {
	if (server->timeformat) {
		free(server->timeformat);
	}
	server->timeformat = strdup((char *) xrm_result.addr);
    }
}

/*
 * Checks for null locale, and assigns default ("C")
 */
static void
server_null_locale(server)
    Server_info  *server;
{
    if (!server->basiclocale)  {
        server->basiclocale = strdup("C");
    }

    if (!server->displaylang)  {
        server->displaylang = strdup("C");
    }

    if (!server->numeric)  {
        server->numeric = strdup("C");
    }

    if (!server->timeformat)  {
        server->timeformat = strdup("C");
    }
}
#endif OS_HAS_LOCALE


Pkg_private int
server_destroy(server_public, status)
    Xv_Server       server_public;
    Destroy_status  status;
{
    /*
     * The Notifier knows about both screen and server objects.  When the
     * entire process is dying, the Notifier calls the destroy routines for
     * the objects in an arbitrary order.  We attempt to change the ordering
     * so that the screen(s) are destroyed before the server(s), so that the
     * screen(s) can always assume that the server(s) are valid. In addition,
     * destruction of a server causes destruction of every object attached to
     * that server.  [BUG ALERT!  Not currently implemented.]
     */
    Server_info    *server = SERVER_PRIVATE(server_public);
    int             i;

    /* Give screens a chance to clean up. */
    for (i = 0; i < MAX_SCREENS; i++)
	if (server->screens[i])
	    if (notify_post_destroy(server->screens[i], status,
		NOTIFY_IMMEDIATE) == XV_ERROR)
		return XV_ERROR;

    switch (status) {
      case DESTROY_PROCESS_DEATH:
	return XV_OK;

      case DESTROY_CLEANUP: {
	XCloseDisplay(server->xdisplay);
	/* Remove the client from the notifier. */
	(void) notify_remove((Notify_client)server->xdisplay);
	server->xdisplay = 0;
	if (xv_default_server == server_public) {
	    /* Remove our scheduler else will deref null server */
	    (void) notify_set_scheduler_func(default_scheduler);
	    xv_default_server = (Xv_Server) NULL;
	    xv_default_display = (Display *) NULL;
	    xv_default_screen = (Xv_Screen) NULL;
	}

        destroy_atoms(server);
	xv_free(XV_SL_REMOVE(SERVER_PRIVATE(server_public), server));
	xv_free(server->display_name);
	xv_free(server->composestatus);

	/*
	 * Free locale strings
	 */
	if (server->basiclocale)  {
	    xv_free(server->basiclocale);
	}
	if (server->displaylang)  {
	    xv_free(server->displaylang);
	}
	if (server->inputlang)  {
	    xv_free(server->inputlang);
	}
	if (server->numeric)  {
	    xv_free(server->numeric);
	}
	if (server->timeformat)  {
	    xv_free(server->timeformat);
	}
	if (server->localedir)  {
	    xv_free(server->localedir);
	}
#ifdef OW_I18N
        XCloseIM(server->xim);
        server->xim = NULL;
#endif OW_I18N

	break;
      }
      default:
	break;
    }

    return XV_OK;
}

static void
destroy_atoms(server)
    Server_info         *server;
{
    Server_atom_list    *head, *node;
    unsigned int         number_of_blocks;
    unsigned int         i;
 
    head = (Server_atom_list *)xv_get(SERVER_PUBLIC(server), XV_KEY_DATA,
                                                    server->atom_list_head_key);    node = head;
            
    number_of_blocks = (server->atom_list_number -1)/SERVER_LIST_SIZE;
 
                /* Each atom that is stored by the atom manager has a
                 * string associated with it and two X contexts.  If
                 * the server is being destroyed, we free the strings
                 * and contexts associated to atoms stored on the
                 * server object.
                 */
    for (i = 0; i <= number_of_blocks; i++) {
        unsigned int count,
                     j;
 
        if (i != number_of_blocks)
            count = SERVER_LIST_SIZE;
        else
            count = (server->atom_list_number -1)%SERVER_LIST_SIZE;
 
        for (j = 0; j < count; j++) {
            char        *atomName;
            XrmQuark     quark;
 
            XFindContext(server->xdisplay, server->atom_mgr[NAME],
                        (XContext)node->list[j], &atomName);
            quark = XrmStringToQuark(atomName);
 
            XDeleteContext(server->xdisplay, server->atom_mgr[ATOM],
                          (XContext)quark);
            XDeleteContext(server->xdisplay, server->atom_mgr[NAME],
                          (XContext)node->list[j]);
            xv_free(atomName);
        }
    }
                        /* Free up the atom manager stuff */
    head = (Server_atom_list *)xv_get(SERVER_PUBLIC(server), XV_KEY_DATA,
                                                    server->atom_list_head_key); 

    while(node = (Server_atom_list *) (XV_SL_SAFE_NEXT(head)))
        xv_free(XV_SL_REMOVE_AFTER(head, head));
    xv_free(head);
}

/*
 * invoke the default scheduler, then flush all servers.
 */
static          Notify_value
scheduler(n, clients)
    int             n;
    Notify_client   clients[];
{
    Notify_value    status = (default_scheduler) (n, clients);
    Server_info    *server;

    /* If xv_default_server is NULL we return because, scheduler()
     * dereferences it.  The problem is that default_scheduler will
     * process the xv_destroy(server) (nulling xv_default_server). 
     * The second problem here is that scheduler assumes that
     * there will always be an xv_default_server.  This is not true.  In
     * a multi server env, the xv_default_server could be destroyed but
     * other server will continue to be around to process events.
     */
    if (!xv_default_server)
	return status;

    /*
     * WARNING: we only want to process events from servers when the notifier
     * is ready to run, not whenever the notifier gets called (e.g. as a
     * result of xv_destroy() calling notify_post_destroy()). The notifier is
     * ready to run either after xv_main_loop() calls notify_start(), or
     * after the client calls notify_do_dispatch() or notify_dispatch().
     */
    if ((status == NOTIFY_DONE) &&
	(ndet_flags & (NDET_STARTED | NDET_DISPATCH)))
        XV_SL_TYPED_FOR_ALL(SERVER_PRIVATE(xv_default_server), server,
			    Server_info *) {
	    if (XPending((Display *)server->xdisplay))
	        status = xv_input_pending(server->xdisplay,
			                  XConnectionNumber(server->xdisplay));
	    XFlush(server->xdisplay);
        }

    return status;
}

static int
xv_set_scheduler()
{
    /*
     * register a scheduler and an input handler with the notifier for this
     * process.
     */
    default_scheduler = notify_set_scheduler_func(scheduler);
    if (default_scheduler == NOTIFY_FUNC_NULL) {
	notify_perror("xv_set_scheduler");
	return XV_ERROR;
    }
    return XV_OK;
}

static void
server_init_atoms(server_public)
    Xv_Server       server_public;
{
    Server_info    *server = SERVER_PRIVATE(server_public);
    Atom            atom;

    /*
     * do not create the SERVER_JOURNAL_ATOM atom if it does not already
     * exists
     */
    atom = XInternAtom(server->xdisplay, "JOURNAL_SYNC", TRUE);
    if (atom == BadValue || atom == BadAlloc) {
	xv_error(NULL,
		 ERROR_SEVERITY, ERROR_NON_RECOVERABLE, 
		 ERROR_STRING, 
		 XV_MSG("Can't create SERVER_JOURNAL_ATOM atom"),
		 ERROR_PKG, SERVER,
		 0);
    }
    if (atom == None) {		/* not in journalling mode */
	server->journalling = FALSE;
    } else {			/* in journalling mode */
	int             status, actual_format;
	unsigned long   nitems, bytes;
	Atom            actual_type;
	unsigned char  *data;	/* default prompt */
	char           *shell_ptr;
	shell_prompt = (char *) xv_calloc(40, sizeof(char));

	/* check to see if this property hangs of the root window */

	status = XGetWindowProperty(server->xdisplay,
			    DefaultRootWindow(server->xdisplay),
				atom, 0, 2, False, XA_INTEGER, &actual_type,
				    &actual_format, &nitems, &bytes, &data);

	if (status != Success || actual_type == None) {
	    server->journalling = FALSE;
	    XFree((char *)data);
	} else {
	    server->journalling = TRUE;
	    if ((shell_ptr = getenv("PROMPT")) == NULL) {
		shell_prompt[0] = '%';
	    } else {
		(void) strcpy(shell_prompt, shell_ptr);
	    }
	    (void) xv_set(server_public, SERVER_JOURNAL_SYNC_ATOM, atom, NULL);
	}
    }
}

Xv_private      Server_atom_type
server_get_atom_type(server_public, atom)
    Xv_Server       server_public;
    Atom            atom;
{
    Server_atom_type    type;
    Server_info        *server = SERVER_PRIVATE(server_public);

    if (XFindContext(server->xdisplay, server->atom_mgr[TYPE], 
		     (XContext) atom, (caddr_t *)&type) != XCNOENT)
	return ((Server_atom_type) type);
    else {
	char *atomName;

	if ((int) atom <= XA_LAST_PREDEFINED)      /* Cache predefined atoms */
		return (save_atom(server, atom, SERVER_WM_UNKNOWN_TYPE));

	atomName = (char *)xv_get(server_public, SERVER_ATOM_NAME, atom);

	if (!strcmp(atomName, "_OL_WIN_ATTR"))
		type = save_atom(server, atom, SERVER_WM_WIN_ATTR_TYPE);
	else if (!strcmp(atomName, "_OL_DECOR_ADD"))
		type = save_atom(server, atom, SERVER_WM_ADD_DECOR_TYPE);
	else if (!strcmp(atomName, "_OL_DECOR_DEL"))
		type = save_atom(server, atom, SERVER_WM_DELETE_DECOR_TYPE);
	else if (!strcmp(atomName, "_OL_DECOR_CLOSE"))
		type = save_atom(server, atom, SERVER_WM_DECOR_CLOSE_TYPE);
	else if (!strcmp(atomName, "_OL_DECOR_FOOTER"))
		type = save_atom(server, atom, SERVER_WM_DECOR_FOOTER_TYPE);
	else if (!strcmp(atomName, "_OL_DECOR_RESIZE"))
		type = save_atom(server, atom, SERVER_WM_DECOR_RESIZE_TYPE);
	else if (!strcmp(atomName, "_OL_DECOR_HEADER"))
		type = save_atom(server, atom, SERVER_WM_DECOR_HEADER_TYPE);
	else if (!strcmp(atomName, "_OL_DECOR_OK"))
		type = save_atom(server, atom, SERVER_WM_DECOR_OK_TYPE);
	else if (!strcmp(atomName, "_OL_DECOR_PIN"))
		type = save_atom(server, atom, SERVER_WM_DECOR_PIN_TYPE);
	else if (!strcmp(atomName, "_OL_SCALE_SMALL"))
		type = save_atom(server, atom, SERVER_WM_SCALE_SMALL_TYPE);
	else if (!strcmp(atomName, "_OL_SCALE_MEDIUM"))
		type = save_atom(server, atom, SERVER_WM_SCALE_MEDIUM_TYPE);
	else if (!strcmp(atomName, "_OL_SCALE_LARGE"))
		type = save_atom(server, atom, SERVER_WM_SCALE_LARGE_TYPE);
	else if (!strcmp(atomName, "_OL_SCALE_XLARGE"))
		type = save_atom(server, atom, SERVER_WM_SCALE_XLARGE_TYPE);
	else if (!strcmp(atomName, "_OL_PIN_STATE"))
		type = save_atom(server, atom, SERVER_WM_PIN_STATE_TYPE);
	else if (!strcmp(atomName, "_OL_WIN_BUSY"))
		type = save_atom(server, atom, SERVER_WM_WIN_BUSY_TYPE);
	else if (!strcmp(atomName, "_OL_WINMSG_STATE"))
		type = save_atom(server, atom, SERVER_WM_WINMSG_STATE_TYPE);
	else if (!strcmp(atomName, "_OL_WINMSG_ERROR"))
		type = save_atom(server, atom, SERVER_WM_WINMSG_ERROR_TYPE);
	else if (!strcmp(atomName, "_OL_WT_BASE"))
		type = save_atom(server, atom, SERVER_WM_WT_BASE_TYPE);
	else if (!strcmp(atomName, "_OL_WT_CMD"))
		type = save_atom(server, atom, SERVER_WM_WT_CMD_TYPE);
	else if (!strcmp(atomName, "_OL_WT_PROP"))
		type = save_atom(server, atom, SERVER_WM_WT_PROP_TYPE);
	else if (!strcmp(atomName, "_OL_WT_HELP"))
		type = save_atom(server, atom, SERVER_WM_WT_HELP_TYPE);
	else if (!strcmp(atomName, "_OL_WT_NOTICE"))
		type = save_atom(server, atom, SERVER_WM_WT_NOTICE_TYPE);
	else if (!strcmp(atomName, "_OL_WT_OTHER"))
		type = save_atom(server, atom, SERVER_WM_WT_OTHER_TYPE);
	else if (!strcmp(atomName, "_OL_MENU_FULL"))
		type = save_atom(server, atom, SERVER_WM_MENU_FULL_TYPE);
	else if (!strcmp(atomName, "_OL_MENU_LIMITED"))
		type = save_atom(server, atom, SERVER_WM_MENU_LIMITED_TYPE);
	else if (!strcmp(atomName, "_OL_NONE"))
		type = save_atom(server, atom, SERVER_WM_NONE_TYPE);
	else if (!strcmp(atomName, "_OL_PIN_IN"))
		type = save_atom(server, atom, SERVER_WM_PIN_IN_TYPE);
	else if (!strcmp(atomName, "_OL_PIN_OUT"))
		type = save_atom(server, atom, SERVER_WM_PIN_OUT_TYPE);
	else if (!strcmp(atomName, "WM_TAKE_FOCUS"))
		type = save_atom(server, atom, SERVER_WM_TAKE_FOCUS_TYPE);
	else if (!strcmp(atomName, "XV_DO_DRAG_MOVE"))
		type = save_atom(server, atom, SERVER_DO_DRAG_MOVE_TYPE);
	else if (!strcmp(atomName, "XV_DO_DRAG_COPY"))
		type = save_atom(server, atom, SERVER_DO_DRAG_COPY_TYPE);
	else if (!strcmp(atomName, "XV_DO_DRAG_LOAD"))
		type = save_atom(server, atom, SERVER_DO_DRAG_LOAD_TYPE);
	else if (!strcmp(atomName, "_OL_WIN_DISMISS"))
		type = save_atom(server, atom, SERVER_WM_DISMISS_TYPE);
	else if (!strcmp(atomName, "WM_SAVE_YOURSELF"))
		type = save_atom(server, atom, SERVER_WM_SAVE_YOURSELF_TYPE);
	else if (!strcmp(atomName, "WM_PROTOCOLS"))
		type = save_atom(server, atom, SERVER_WM_PROTOCOLS_TYPE);
	else if (!strcmp(atomName, "WM_DELETE_WINDOW"))
		type = save_atom(server, atom, SERVER_WM_DELETE_WINDOW_TYPE);
	else if (!strcmp(atomName, "WM_COMMAND"))
		type = save_atom(server, atom, SERVER_WM_COMMAND_TYPE);
	else if (!strcmp(atomName, "WM_CHANGE_STATE"))
		type = save_atom(server, atom, SERVER_WM_CHANGE_STATE_TYPE);
	else if (!strcmp(atomName, "_OL_DFLT_BIN"))
		type = save_atom(server, atom, SERVER_WM_DEFAULT_BUTTON_TYPE);
	else if (!strcmp(atomName, "_SUN_DRAGDROP_INTEREST"))
		type = save_atom(server, atom,SERVER_WM_DRAGDROP_INTEREST_TYPE);
	else if (!strcmp(atomName, "_SUN_DRAGDROP_PREVIEW"))
		type = save_atom(server, atom, SERVER_WM_DRAGDROP_PREVIEW_TYPE);
	else if (!strcmp(atomName, "_SUN_DRAGDROP_TRIGGER"))
		type = save_atom(server, atom, SERVER_WM_DRAGDROP_TRIGGER_TYPE);
	else if (!strcmp(atomName, "_SUN_DRAGDROP_ACK"))
		type = save_atom(server, atom, SERVER_WM_DRAGDROP_ACK_TYPE);
	else if (!strcmp(atomName, "_SUN_DRAGDROP_DONE"))
		type = save_atom(server, atom, SERVER_WM_DRAGDROP_DONE_TYPE);
#ifdef OW_I18N
        else if (!strcmp(atomName, "COMPOUND_TEXT"))
                type = save_atom(server, atom, SERVER_COMPOUND_TEXT_TYPE);
#endif OW_I18N
	else 
		type = save_atom(server, atom, SERVER_WM_UNKNOWN_TYPE);

	return ((Server_atom_type) type);
    }
}

static Server_atom_type
save_atom(server, atom, type)
Server_info	*server;
Atom		 atom;
Server_atom_type type;
{
	(void) XSaveContext(server->xdisplay, server->atom_mgr[TYPE],
		     (XContext) atom, (caddr_t) type);
	return (type); 
}

/*
 * BUG:  use_default_mapping should be set by comparing the default keycode
 * to keysym table.
 */
Xv_private void
server_journal_sync_event(server_public, type)
    Xv_Server       server_public;
    int             type;

{
    Server_info    *server = SERVER_PRIVATE(server_public);
    Atom            sync_atom = (Atom) xv_get(server_public, 
				     (Attr_attribute)SERVER_JOURNAL_SYNC_ATOM);
    XEvent          send_event;
    XClientMessageEvent *cme = (XClientMessageEvent *) & send_event;
    unsigned int    mask;
    Display        *dpy = (Display *) server->xdisplay;
    /*
     * Xv_Drawable_info       *info;
     */

    cme->type = ClientMessage;
    cme->serial = ClientMessage;/* should prob leave this alone */
    cme->send_event = 1;
    cme->display = dpy;
    /* get the xid of the root window -- not 100% correct */
    /*
     * DRAWABLE_INFO(xv_get(xv_get(server_public,SERVER_NTH_SCREEN,0),XV_ROOT),
     * info); cme->window = xv_xid(info); */
    cme->window = DefaultRootWindow((Display *) server->xdisplay),
    cme->message_type = sync_atom;
    cme->format = 32;
    cme->data.l[0] = type;
    mask = 0;
    XSync(dpy, 0);		/* make sure journal process has been
				 * scheduled and is waiting for the sync
				 * event */
    (void) XSendEvent(dpy, cme->window, 0, mask, (XEvent *)cme);
    XSync(dpy, 0);		/* make sure journal event has occurred */
}

xv_string_to_rgb(buffer, red, green, blue)
    char            *buffer;
    unsigned char   *red;
    unsigned char   *green;
    unsigned char   *blue;

{
        int     hex_buffer;
        unsigned char   *conv_ptr;
        (void) sscanf(buffer, "#%6x", &hex_buffer);

        conv_ptr = (unsigned char *) &hex_buffer;
        *red = conv_ptr[1];
        *green = conv_ptr[2];
        *blue = conv_ptr[3];
}

static unsigned int
string_to_modmask(str)
char *str;
{
	if (strcmp(str, "Shift") == 0) 
		return ShiftMask;
	else if (strcmp(str, "Ctrl") == 0) 
		return ControlMask;
	else if (strcmp(str, "Meta") == 0) 
		return Mod1Mask;
	else  { /* Punt for now, just return Mod1Mask */
		/* What really needs to be done here is look up the 
		   modifier mapping from the server and add the new modifier
		   keys we are now interested in.   			     */
		xv_error(NULL,
			 ERROR_STRING,
		  XV_MSG("Only support Shift, Ctrl and Meta as mouse button modifiers"),
			 ERROR_PKG, SERVER,
			 0);
		return(Mod1Mask);
	}
}
