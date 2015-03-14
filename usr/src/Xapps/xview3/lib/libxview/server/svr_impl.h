/*	@(#)svr_impl.h 20.51 91/09/14 SMI	*/

/*	
 *	(c) Copyright 1989 Sun Microsystems, Inc. Sun design patents 
 *	pending in the U.S. and foreign countries. See LEGAL NOTICE 
 *	file for terms of the license. 
 */

#ifndef _server_impl_h_already_included
#define _server_impl_h_already_included

#include <sys/types.h>
#include <xview_private/xv_list.h>
#include <xview/server.h>
#include <xview/screen.h>
#include <xview/sel_svc.h>
#include <xview/sel_pkg.h>
#include <xview_private/seln_impl.h>

#include <X11/Xresource.h>

#ifdef OW_I18N
#include <xview/xv_i18n.h> 
#include <xview_private/svr_atom.h>
#endif OW_I18N

/* maximum # of screens per server (arbitrary) */
#define	MAX_SCREENS		10
#define BITS_PER_BYTE		8

	/* For atom mgr */
#define ATOM			0
#define NAME			1
#define TYPE			2
#define DATA			3
#define SERVER_LIST_SIZE	25


typedef struct server_proc_list {
	Xv_sl_link	next;
	Xv_opaque	id;  	/* unique id, typically xview handle */
	void	       (*extXeventProc)();  /* application callback proc */   
	void	       (*pvtXeventProc)();  /* xview private callback proc */
} Server_proc_list;

typedef struct server_mask_list {
	Xv_sl_link	next;
	Xv_opaque	id; 	/* unique id, typically xview handle */
	Xv_opaque	extmask;   /* mask of X events req. by app */
	Xv_opaque	pvtmask;   /* mask of X events req. by xview pkgs*/
	Server_proc_list *proc;
} Server_mask_list;

typedef struct server_xid_list {
	Xv_sl_link	next;
	Xv_opaque	xid;    /* XID of the window */
	Server_mask_list *masklist;
} Server_xid_list;

typedef struct {
    Xv_sl_link		 next;
    Xv_Server		 public_self;	 /* Back pointer */
    Xv_Screen		 screens[MAX_SCREENS];
    Display		*xdisplay;
    unsigned int	*xv_map;
    unsigned char	*sem_map;
    unsigned char	*ascii_map;
    KeySym		 cut_keysym;
    KeySym		 paste_keysym;
    Xv_opaque		 xtime;           /* Last time stamp */
    int			 seln_function_pending;
    unsigned		 journalling;
    Atom		 journalling_atom;
    short		 in_fullscreen;
    Xv_opaque		 top_level_win;
    XID			 atom_mgr[4];
    short		 nbuttons;       /* Number of physical mouse buttons */
    unsigned int	 but_two_mod;    /* But 1 + this modifier == but 2 */
    unsigned int	 but_three_mod;  /* But 1 + this modifier == but 3 */
					 /* Above only valid if nbuttons < 3 */
    void	       (*extensionProc)(); 
    char		*display_name;
    int			 alt_modmask;    /* Represents the modifier slot the
					  * ALT key is in, between
					  * Mod1MapIndex -> Mod5MapIndex.
					  */
    int			 meta_modmask;   /* Represents the modifier slot the
					  * META key is in, between
					  * Mod1MapIndex -> Mod5MapIndex.
					  */
    int			 num_lock_modmask;/* Represents the modifier slot the
					   * Num Lock key is in, between
					   * Mod1MapIndex -> Mod5MapIndex.
					   */
    int			 sel_modmask;     /* Represents the modifier slot the
					   * selection function keys are in,
					   * between
					   * Mod1MapIndex -> Mod5MapIndex.
					   */
    int                  chording_timeout;
    unsigned int         chord_menu;
    unsigned int	 dnd_ack_key;     /* For Dnd acks under local drops */
    unsigned int	 atom_list_head_key;/* For the list of allocated atoms*/
    unsigned int	 atom_list_tail_key;/* For the list of allocated atoms*/
    unsigned int         atom_list_number;/* The size of the atom list */
    XrmDatabase		 db;
    char		*basiclocale;
    char                *displaylang;
    char                *inputlang;
    char                *numeric;
    char                *timeformat;
    char		*localedir;
    unsigned long	 focus_timestamp;/* storing the FocusIn/Out timestamps
                                          * recieved during WM_TAKE_FOCUS/
					  * ButtonPress used in soft function
					  * keys
					  */
    XComposeStatus	*composestatus;

#ifdef OW_I18N
    XIM                 xim;
#endif OW_I18N
    Server_proc_list	*idproclist;
    Server_xid_list	*xidlist;
} Server_info;

typedef struct {
    Xv_sl_link		next;
    Atom		list[SERVER_LIST_SIZE];
} Server_atom_list;

#define	SERVER_PRIVATE(server)	XV_PRIVATE(Server_info, Xv_server_struct, server)
#define	SERVER_PUBLIC(server)	XV_PUBLIC(server)

#define         SELN_FN_NUM     3

Pkg_private Xv_opaque	server_init_x();

/* server_get.c */
Pkg_private int		server_init();
Pkg_private int		server_destroy();

/* server_get.c */
Pkg_private Xv_opaque	server_get_attr();

/* server_set.c */
Pkg_private Xv_opaque	server_set_avlist();
Pkg_private Server_xid_list *server_xidnode_from_xid ();
Pkg_private Server_mask_list *server_masknode_from_xidid ();
Pkg_private Server_proc_list *server_procnode_from_id ();

#endif
