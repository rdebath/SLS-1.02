#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)om_set.c 20.75 91/09/14";
#endif
#endif

/*
 *	(c) Copyright 1989 Sun Microsystems, Inc. Sun design patents 
 *	pending in the U.S. and foreign countries. See LEGAL NOTICE 
 *	file for terms of the license.
 */

/* --------------------------------------------------------------------- */
#include <sys/types.h>
#include <xview_private/om_impl.h>
#include <xview_private/draw_impl.h>
#include <xview/font.h>
#include <xview/frame.h>
#include <xview/notify.h>
#include <xview/panel.h>

/* -------------------------------------------------------------------- */

/*
 * Public
 */
/* None */

/*
 * XView Private
 */
Xv_private void screen_set_cached_window_busy();

/*
 * Package private
 */
Pkg_private Xv_opaque menu_sets();
Pkg_private Xv_opaque menu_item_sets();
Pkg_private void menu_create_pin_panel_items();
Pkg_private void menu_destroys();
Pkg_private void menu_item_destroys();
Pkg_private void menu_set_pin_window();
Pkg_private Notify_value menu_pin_window_event_proc();
Pkg_private void menu_return_no_value();

/*
 * Private
 */
static int      extend_item_list();
static void     insert_item();
static int      lookup();
static void	menu_add_pin();
static void 	menu_create_title();
static void     remove_item();
static void     replace_item();
static void	destroy_panel_item_handles();

/*
 * Private defs
 */
/* None */

/* -------------------------------------------------------------------- */


Pkg_private     Xv_opaque
menu_sets(menu_public, attrs)
    Menu            menu_public;
    register Attr_attribute *attrs;
{
    register Xv_menu_info *m = MENU_PRIVATE(menu_public);
    int		    bad_attr;
    Xv_menu_info   *parent_menu;
    Xv_menu_item_info *mi;
    int		    repin = FALSE;	/* need to recreate pin window */

    for (; *attrs; attrs = attr_next(attrs)) {
	bad_attr = FALSE;
	switch (attrs[0]) {

	  case MENU_ACTION_IMAGE:
	  case MENU_ACTION_ITEM:
	  case MENU_GEN_PULLRIGHT_IMAGE:
	  case MENU_GEN_PULLRIGHT_ITEM:
	  case MENU_GEN_PROC_IMAGE:
	  case MENU_GEN_PROC_ITEM:
	  case MENU_IMAGE_ITEM:
	  case MENU_PULLRIGHT_IMAGE:
	  case MENU_PULLRIGHT_ITEM:
	  case MENU_STRING_ITEM:
#ifdef OW_I18N
	  case MENU_ACTION_ITEM_WCS:
	  case MENU_GEN_PULLRIGHT_ITEM_WCS:
	  case MENU_GEN_PROC_ITEM_WCS:
	  case MENU_PULLRIGHT_ITEM_WCS:
	  case MENU_STRING_ITEM_WCS:
#endif OW_I18N
	    if (m->nitems < m->max_nitems || extend_item_list(m)) {
		m->item_list[m->nitems++] = MENU_ITEM_PRIVATE(
						   xv_create(NULL, MENUITEM,
			    MENU_RELEASE, attrs[0], attrs[1], attrs[2], 0));
	    }
	    break;

	  case MENU_APPEND_ITEM:
	    if (m->nitems < m->max_nitems || extend_item_list(m))
		m->item_list[m->nitems++] = MENU_ITEM_PRIVATE(attrs[1]);
	    repin = TRUE;
	    break;

	  case MENU_BUSY_PROC:
	    m->busy_proc = (void (*) ()) attrs[1];
	    break;

	  case MENU_CLIENT_DATA:
	    m->client_data = (Xv_opaque) attrs[1];
	    break;

	  case MENU_COLOR:
	    if (m->color_index != (int) attrs[1]) {
		m->color_index = (int) attrs[1];
	    }
	    break;

	  case MENU_COL_MAJOR:
	    m->column_major = (int) attrs[1];
	    break;

	  case MENU_DEFAULT:
	    if ((int) attrs[1] > 0)
		m->default_position = (int) attrs[1];
	    else
		m->default_position = 1;
	    break;

	  case MENU_DEFAULT_ITEM:
	    m->default_position = lookup(m->item_list, m->nitems,
					 MENU_ITEM_PRIVATE(attrs[1]));
	    if (m->default_position <= 0)
		m->default_position = 1;
	    break;

	  case MENU_DONE_PROC:
	    m->done_proc = (void (*) ()) attrs[1];
	    break;

	  case XV_FONT:
	    *attrs = ATTR_NOP(*attrs);
            if (m->default_image.font) {
                xv_set(m->default_image.font, XV_DECREMENT_REF_COUNT, 0);
            }
            m->default_image.font = attrs[1];
            if (m->default_image.font) {
                xv_set(m->default_image.font, XV_INCREMENT_REF_COUNT, 0);
            }
            m->default_image.width = m->default_image.height = 0;
	    break;

#ifdef  OW_I18N
          case MENU_GEN_PIN_WINDOW:{
            int    len;
            m->pin_parent_frame = (Xv_opaque) attrs[1];
            if( m->pin_window_header ) {
                free( m->pin_window_header );
                m->pin_window_header = NULL;
            }
            len = strlen( (char *)attrs[2] );
            m->pin_window_header = (wchar_t *)mbstowcsdup((char *) attrs[2]);
            if( m->pin_window_header_mbs ) {
                free( m->pin_window_header_mbs );
                m->pin_window_header_mbs = NULL;
            }
            if (!m->pin)
                menu_add_pin(m);
            break;
          }
          case MENU_GEN_PIN_WINDOW_WCS:{
            int    len;
            m->pin_parent_frame = (Xv_opaque) attrs[1];
            if( m->pin_window_header ) {
                free( m->pin_window_header );
                m->pin_window_header = NULL;
            }
            len = wslen( (wchar_t *)attrs[2] );
            m->pin_window_header = (wchar_t *)xv_alloc_n(wchar_t, len +1 );
            (void)wscpy( m->pin_window_header, (wchar_t *) attrs[2]);
            if( m->pin_window_header_mbs ) {
                free( m->pin_window_header_mbs );
                m->pin_window_header_mbs = NULL;
            }
            if (!m->pin)
                menu_add_pin(m);
            break;
          }
#else
	  case MENU_GEN_PIN_WINDOW:
	    m->pin_parent_frame = (Xv_opaque) attrs[1];
	    m->pin_window_header = (char *) attrs[2];
	    if (!m->pin)
		menu_add_pin(m);
	    break;
#endif OW_I18N

	  case MENU_GEN_PROC:
	    m->gen_proc = (Menu(*) ()) attrs[1];
	    break;

	  case MENU_IMAGES:
	    {
		char          **a = (char **) &attrs[1];
		while (*a) {
		    if (m->nitems < m->max_nitems || extend_item_list(m)) {
			m->item_list[m->nitems] = MENU_ITEM_PRIVATE(
						   xv_create(NULL, MENUITEM,
							     MENU_RELEASE,
						      MENU_IMAGE_ITEM, *a++,
							 m->nitems + 1, 0));
		    }
		    m->nitems++;
		}
	    }
	    repin = TRUE;
	    break;

	  case MENU_INSERT:
	    if (++m->nitems < m->max_nitems || extend_item_list(m)) {
		insert_item(m, (int) attrs[1], MENU_ITEM_PRIVATE(attrs[2]));
	    }
	    repin = TRUE;
	    break;

	  case MENU_INSERT_ITEM:
	    if (++m->nitems < m->max_nitems || extend_item_list(m)) {
		insert_item(m,
			    lookup(m->item_list, m->nitems,
				   MENU_ITEM_PRIVATE(attrs[1])),
			    MENU_ITEM_PRIVATE(attrs[2]));
	    }
	    repin = TRUE;
	    break;

	  case MENU_ITEM:
	    if (m->nitems < m->max_nitems || extend_item_list(m)) {
		m->item_list[m->nitems] = MENU_ITEM_PRIVATE(xv_create_avlist(
						NULL, MENUITEM, &attrs[1]));
	    }
	    (void) xv_set(MENU_ITEM_PUBLIC(m->item_list[m->nitems++]), MENU_RELEASE, 0);
	    repin = TRUE;
	    break;

	  case MENU_NCOLS:
	    m->ncols = imax(0, (int) attrs[1]);
	    m->ncols_fixed = m->ncols > 0;
	    break;

	  case MENU_NROWS:
	    m->nrows = imax(0, (int) attrs[1]);
	    m->nrows_fixed = m->nrows > 0;
	    break;

	  case MENU_NOTIFY_PROC:
	    m->notify_proc = (Xv_opaque(*) ()) attrs[1];
	    if (!m->notify_proc)
		m->notify_proc = MENU_DEFAULT_NOTIFY_PROC;
	    break;

	  case MENU_NOTIFY_STATUS:
	    /* Set the notify status on the top level menu */
	    parent_menu = m;
	    while (parent_menu && (mi = parent_menu->parent))
		parent_menu = mi->parent;	/* get current enclosing menu */
	    parent_menu->notify_status = (int) attrs[1];
	    break;

	  case MENU_PIN:
	    if (attrs[1]) {
		if (!m->pin)
		    menu_add_pin(m);
	    } else if (m->pin) {
		m->pin = FALSE;
		if (m->item_list[0]->image.string ||
		    m->item_list[0]->image.svr_im) {
		    /* Force recomputation of item size in compute_item_size */
		    m->item_list[0]->image.width = 0;
		} else {
		    /* No title string or image: Remove pin (title) menu item */
		    remove_item(m, 1);
		}
	    }
	    break;

	  case MENU_PIN_PROC:
	    m->pin_proc = (void (*) ()) attrs[1];
	    break;

	  case MENU_PIN_WINDOW:
	    menu_set_pin_window(m, attrs[1]);
	    break;

	  case MENU_REMOVE:
	    destroy_panel_item_handles(m);
	    remove_item(m, (int) attrs[1]);
	    if (m->pin_window) {
	        menu_create_pin_panel_items(xv_get(m->pin_window, FRAME_CMD_PANEL), m);
	        window_fit(m->pin_window);
	    }
	    break;

	  case MENU_REMOVE_ITEM:
	    destroy_panel_item_handles(m);
	    remove_item(m,
			lookup(m->item_list, m->nitems,
			       MENU_ITEM_PRIVATE(attrs[1])));
	    if (m->pin_window) {
		menu_create_pin_panel_items(xv_get(m->pin_window, FRAME_CMD_PANEL), m);
		window_fit(m->pin_window);
	    }
	    break;

	  case MENU_REPLACE:
	    destroy_panel_item_handles(m);
	    replace_item(m->item_list, m->nitems, (int) attrs[1],
			 MENU_ITEM_PRIVATE(attrs[2]));
	    if (m->pin_window) {
		menu_create_pin_panel_items(xv_get(m->pin_window, FRAME_CMD_PANEL), m);
		window_fit(m->pin_window);
	    }
	    break;

	  case MENU_REPLACE_ITEM:
	    destroy_panel_item_handles(m);
	    replace_item(m->item_list, m->nitems,
			 lookup(m->item_list, m->nitems,
				MENU_ITEM_PRIVATE(attrs[1])),
			 MENU_ITEM_PRIVATE(attrs[2]));
	    if (m->pin_window) {
		menu_create_pin_panel_items(xv_get(m->pin_window, FRAME_CMD_PANEL), m);
		window_fit(m->pin_window);
	    }
	    break;

#ifdef  OW_I18N
          case MENU_STRINGS_WCS:
          case MENU_STRINGS:
            {
                char          **a = (char **) &attrs[1];
                while (*a) {
                    if (m->nitems < m->max_nitems || extend_item_list(m)) {
                        m->item_list[m->nitems] = MENU_ITEM_PRIVATE(
                                                   xv_create(NULL, MENUITEM,
                                                             MENU_RELEASE,
                                        (attrs[0] == MENU_STRINGS) ?
                                                           MENU_STRING_ITEM :
                                                           MENU_STRING_ITEM_WCS,                                                   *a++, m->nitems + 1, 0));
                    }
                    m->nitems++;
                }
            }    
            repin = TRUE;
            break;
#else
	  case MENU_STRINGS:
	    {
		char          **a = (char **) &attrs[1];
		while (*a) {
		    if (m->nitems < m->max_nitems || extend_item_list(m)) {
			m->item_list[m->nitems] = MENU_ITEM_PRIVATE(
						   xv_create(NULL, MENUITEM,
							     MENU_RELEASE,
							   MENU_STRING_ITEM,
						   *a++, m->nitems + 1, 0));
		    }
		    m->nitems++;
		}
	    }
	    repin = TRUE;
	    break;
#endif OW_I18N

#ifdef  OW_I18N
          case MENU_TITLE_ITEM:
          case MENU_TITLE_ITEM_WCS:
            if (!m->item_list[0] || !m->item_list[0]->title)
                menu_create_title(m,
                    MENU_TITLE_ITEM == (Menu_attribute) attrs[0] ?
                        MENU_STRING :
                        MENU_TITLE_ITEM_WCS == (Menu_attribute) attrs[0] ?
                            MENU_STRING_WCS : MENU_IMAGE,
                    attrs[1]);
            else
                xv_set(MENU_ITEM_PUBLIC(m->item_list[0]),
                    MENU_TITLE_ITEM == (Menu_attribute) attrs[0] ?
                        MENU_STRING :
                        MENU_TITLE_ITEM_WCS == (Menu_attribute) attrs[0] ?
                            MENU_STRING_WCS : MENU_IMAGE,
                       attrs[1], 0);
            break;
#else
	  case MENU_TITLE_ITEM:
	    if (!m->item_list[0] || !m->item_list[0]->title)
		menu_create_title(m,
		    MENU_TITLE_ITEM == (Menu_attribute) attrs[0] ?
		        MENU_STRING : MENU_IMAGE,
		    attrs[1]);
	    else
		xv_set(MENU_ITEM_PUBLIC(m->item_list[0]),
		       MENU_TITLE_ITEM == (Menu_attribute) attrs[0] ?
			   MENU_STRING : MENU_IMAGE,
		       attrs[1], 0);
	    break;
#endif OW_I18N

	  case MENU_VALID_RESULT:
	    m->valid_result = (int) attrs[1];
	    break;

	  case MENU_LINE_AFTER_ITEM:
	    switch ((int) attrs[1]) {
	      case MENU_HORIZONTAL_LINE:
		m->h_line = 1;
		break;
	      case MENU_VERTICAL_LINE:
		m->v_line = 1;
		break;
	      default:{
		    char            dummy[128];

		    (void) sprintf(dummy,
			   XV_MSG("Invalid argument for attribute MENU_LINE_AFTER_ITEM: %d"),
				   (int) attrs[1]);
		    xv_error(NULL,
			     ERROR_STRING, dummy,
			     ERROR_PKG, MENU,
			     0);
		}
	    }
	    break;

	  case MENU_CLASS:
	    xv_error(NULL,
		 ERROR_STRING, 
		    XV_MSG("MENU_CLASS attribute is get-only"),
		 ERROR_PKG, MENU,
		 0);
	    break;
	
	  default:
	    bad_attr = TRUE;
	    break;

	}
	if (!bad_attr)
	    ATTR_CONSUME(attrs[0]);
    }

    if (repin && m->pin_window) {
	destroy_panel_item_handles(m);
	menu_create_pin_panel_items(xv_get(m->pin_window, FRAME_CMD_PANEL), m);
	window_fit(m->pin_window);
    }
    return (Xv_opaque) XV_OK;
}


Pkg_private     Xv_opaque
menu_item_sets(menu_item_public, attrs)
    Menu_item       menu_item_public;
    register Attr_attribute *attrs;
{
    register Xv_menu_item_info *mi = MENU_ITEM_PRIVATE(menu_item_public);
    int		    bad_attr;

    for (; *attrs; attrs = attr_next(attrs)) {
	bad_attr = FALSE;
	switch (attrs[0]) {

	  case MENU_ACTION:	/* == MENU_ACTION_PROC == MENU_NOTIFY_PROC */
	    mi->notify_proc = (Xv_opaque(*) ()) attrs[1];
	    if (mi->parent && mi->parent->pin_window && mi->panel_item_handle) {
		xv_set(mi->panel_item_handle,
		       XV_KEY_DATA, MENU_NOTIFY_PROC, mi->notify_proc,
		       0);
	    }
	    break;

#ifdef  OW_I18N
          case MENU_ACTION_ITEM:{
            int   len;
            /*
            if (mi->image.free_string && mi->image.string)
            */
            if (mi->image.string ) {
                free(mi->image.string);
                mi->image.string = NULL;
            }
            if( mi->image.string_mbs ) {
                free(mi->image.string_mbs);
                mi->image.string_mbs = NULL;
            }
            len =  strlen( (char *)attrs[1] );
            mi->image.string = (wchar_t *)mbstowcsdup( (char *) attrs[1]);
            mi->image.width = mi->image.height = 0;
            mi->notify_proc = (Xv_opaque(*) ()) attrs[2];
            if (mi->parent && mi->parent->pin_window && mi->panel_item_handle) {
                xv_set(mi->panel_item_handle,
                       PANEL_LABEL_STRING_WCS, mi->image.string,
                       XV_KEY_DATA, MENU_NOTIFY_PROC, mi->notify_proc,
                       0);
            }
            break;
          }
          case MENU_ACTION_ITEM_WCS:{
            int   len;
            /*
            if (mi->image.free_string && mi->image.string)
            */
            if ( mi->image.string) {
                free(mi->image.string);
                mi->image.string = NULL;
            }
            if( mi->image.string_mbs ) {
                free( mi->image.string_mbs );
                mi->image.string_mbs = NULL;
            }
            len = wslen( (wchar_t *)attrs[1] ) ;
            mi->image.string = (wchar_t *)xv_alloc_n(wchar_t, len +1);
            (void)wscpy(mi->image.string , (wchar_t *) attrs[1]);
            mi->image.width = mi->image.height = 0;
            mi->notify_proc = (Xv_opaque(*) ()) attrs[2];
            if (mi->parent && mi->parent->pin_window && mi->panel_item_handle) {
                xv_set(mi->panel_item_handle,
                       PANEL_LABEL_STRING_WCS, mi->image.string,
                       XV_KEY_DATA, MENU_NOTIFY_PROC, mi->notify_proc,
                       0);
            }
            break;
          }
#else
	  case MENU_ACTION_ITEM:
            if (mi->image.free_string && mi->image.string)
                free(mi->image.string);
            mi->image.string = (char *) attrs[1];
            mi->image.width = mi->image.height = 0;
	    mi->notify_proc = (Xv_opaque(*) ()) attrs[2];
	    if (mi->parent && mi->parent->pin_window && mi->panel_item_handle) {
		xv_set(mi->panel_item_handle,
		       PANEL_LABEL_STRING, mi->image.string,
		       XV_KEY_DATA, MENU_NOTIFY_PROC, mi->notify_proc,
		       0);
	    }
	    break;
#endif OW_I18N

#ifdef  OW_I18N
          case MENU_STRING_ITEM:{
            int   len;
            /*
            if (mi->image.free_string && mi->image.string)
            */
            if (mi->image.string ) {
                free(mi->image.string);
                mi->image.string = NULL;
            }
            if( mi->image.string_mbs ) {
                free(mi->image.string_mbs);
                mi->image.string_mbs = NULL;
            }
            len =  strlen( (char *)attrs[1] );
            mi->image.string = (wchar_t *)mbstowcsdup( (char *) attrs[1]);
            mi->image.width = mi->image.height = 0;
            mi->value = (Xv_opaque) attrs[2];
            if (mi->parent && mi->parent->pin_window && mi->panel_item_handle) {
                xv_set(mi->panel_item_handle,
                       PANEL_LABEL_STRING_WCS, mi->image.string,
                       0);
            }
            break;
          }
          case MENU_STRING_ITEM_WCS:{
            int   len;
            /*
            if (mi->image.free_string && mi->image.string)
            */
            if ( mi->image.string) {
                free(mi->image.string);
                mi->image.string = NULL;
            }
            if( mi->image.string_mbs ) {
                free( mi->image.string_mbs );
                mi->image.string_mbs = NULL;
            }
            len = wslen( (wchar_t *)attrs[1] ) ;
            mi->image.string = (wchar_t *)xv_alloc_n(wchar_t, len +1);
            (void)wscpy(mi->image.string , (wchar_t *) attrs[1]);
            mi->image.width = mi->image.height = 0;
            mi->value = (Xv_opaque) attrs[2];
            if (mi->parent && mi->parent->pin_window && mi->panel_item_handle) {
                xv_set(mi->panel_item_handle,
                       PANEL_LABEL_STRING_WCS, mi->image.string,
                       0);
            }
            break;
          }
#else
	  case MENU_STRING_ITEM:
            if (mi->image.free_string && mi->image.string)
                free(mi->image.string);
            mi->image.string = (char *) attrs[1];
            mi->image.width = mi->image.height = 0;
	    mi->value = (Xv_opaque) attrs[2];
	    if (mi->parent && mi->parent->pin_window && mi->panel_item_handle) {
		xv_set(mi->panel_item_handle,
		       PANEL_LABEL_STRING, mi->image.string,
		       0);
	    }
	    break;
#endif OW_I18N

	  case MENU_ACTION_IMAGE:
	    if (mi->image.free_svr_im && mi->image.svr_im)
		xv_destroy(mi->image.svr_im);
            mi->image.svr_im = (Server_image) attrs[1];
            mi->image.width = mi->image.height = 0;
	    mi->notify_proc = (Xv_opaque(*) ()) attrs[2];
	    if (mi->parent && mi->parent->pin_window && mi->panel_item_handle) {
		xv_set(mi->panel_item_handle,
		       PANEL_LABEL_IMAGE, mi->image.svr_im,
		       XV_KEY_DATA, MENU_NOTIFY_PROC, mi->notify_proc,
		       0);
	    }
	    break;

	  case MENU_IMAGE_ITEM:
	    if (mi->image.free_svr_im && mi->image.svr_im)
                xv_destroy(mi->image.svr_im);
            mi->image.svr_im = (Server_image) attrs[1];
            mi->image.width = mi->image.height = 0;
	    mi->value = (Xv_opaque) attrs[2];
	    if (mi->parent && mi->parent->pin_window && mi->panel_item_handle) {
		xv_set(mi->panel_item_handle,
		       PANEL_LABEL_IMAGE, mi->image.svr_im,
		       0);
	    }
	    break;

	  case MENU_CLIENT_DATA:
	    mi->client_data = (Xv_opaque) attrs[1];
	    break;

	  case MENU_COLOR:
	    mi->color_index = (int) attrs[1];
	    break;

	  case MENU_FEEDBACK:
	    mi->no_feedback = !(int) attrs[1];
	    break;

	  case XV_FONT:
	    *attrs = ATTR_NOP(*attrs);
            if (mi->image.font) {
                (void) xv_set(mi->image.font, XV_DECREMENT_REF_COUNT, 0);
            }
            mi->image.font = attrs[1];
            if (mi->image.font) {
                (void) xv_set(mi->image.font, XV_INCREMENT_REF_COUNT, 0);
            }
            mi->image.width = mi->image.height = 0;
	    break;

	  case MENU_GEN_PROC:
	    mi->gen_proc = (Menu_item(*) ()) attrs[1];
	    break;

	  case MENU_GEN_PROC_IMAGE:
	    if (mi->image.free_svr_im && mi->image.svr_im)
                xv_destroy(mi->image.svr_im);
            mi->image.svr_im = (Server_image) attrs[1];
            mi->image.width = mi->image.height = 0;
	    mi->gen_proc = (Menu_item(*) ()) attrs[2];
	    if (mi->parent && mi->parent->pin_window && mi->panel_item_handle) {
		xv_set(mi->panel_item_handle,
		       PANEL_LABEL_IMAGE, mi->image.svr_im,
		       0);
	    }
	    break;

#ifdef  OW_I18N
          case MENU_GEN_PROC_ITEM:{
            int   len;
            /*
            if (mi->image.free_string && mi->image.string)
            */
            if (mi->image.string ) {
                free(mi->image.string);
                mi->image.string = NULL;
            }
            if( mi->image.string_mbs ) {
                free(mi->image.string_mbs);
                mi->image.string_mbs = NULL;
            }
            len =  strlen( (char *)attrs[1] );
            mi->image.string = (wchar_t *)mbstowcsdup( (char *) attrs[1]);
            mi->image.width = mi->image.height = 0;
            mi->gen_proc = (Menu_item(*) ()) attrs[2];
            if (mi->parent && mi->parent->pin_window && mi->panel_item_handle) {
                xv_set(mi->panel_item_handle,
                       PANEL_LABEL_STRING_WCS, mi->image.string,
                       0);
            }
            break;
          }
          case MENU_GEN_PROC_ITEM_WCS:{
            int   len;
            /*
            if (mi->image.free_string && mi->image.string)
            */
            if ( mi->image.string) {
                free(mi->image.string);
                mi->image.string = NULL;
            }
            if( mi->image.string_mbs ) {
                free( mi->image.string_mbs );
                mi->image.string_mbs = NULL;
            }
            len = wslen( (wchar_t *)attrs[1] ) ;
            mi->image.string = (wchar_t *)xv_alloc_n(wchar_t, len +1 );
            (void)wscpy(mi->image.string , (wchar_t *) attrs[1]);
            mi->image.width = mi->image.height = 0;
            mi->gen_proc = (Menu_item(*) ()) attrs[2];
            if (mi->parent && mi->parent->pin_window && mi->panel_item_handle) {
                xv_set(mi->panel_item_handle,
                       PANEL_LABEL_STRING_WCS, mi->image.string,
                       0);
            }
            break;
          }
#else
	  case MENU_GEN_PROC_ITEM:
	    if (mi->image.free_string && mi->image.string)
                free(mi->image.string);
            mi->image.string = (char *) attrs[1];
            mi->image.width = mi->image.height = 0;
	    mi->gen_proc = (Menu_item(*) ()) attrs[2];
	    if (mi->parent && mi->parent->pin_window && mi->panel_item_handle) {
		xv_set(mi->panel_item_handle,
		       PANEL_LABEL_STRING, mi->image.string,
		       0);
	    }
	    break;
#endif OW_I18N

	  case MENU_GEN_PULLRIGHT:
	    mi->gen_pullright = (Menu(*) ()) attrs[1];
	    mi->pullright = mi->gen_pullright != NULL;
	    mi->value = 0; /* Pullright Generate procedure not called yet */
	    break;

	  case MENU_GEN_PULLRIGHT_IMAGE:
	    if (mi->image.free_svr_im && mi->image.svr_im)
                xv_destroy(mi->image.svr_im);
            mi->image.svr_im = (Server_image) attrs[1];
            mi->image.width = mi->image.height = 0;
	    mi->gen_pullright = (Menu(*) ()) attrs[2];
	    mi->pullright = mi->gen_pullright != NULL;
	    mi->value = 0;
	    if (mi->parent && mi->parent->pin_window && mi->panel_item_handle) {
		xv_set(mi->panel_item_handle,
		       PANEL_LABEL_IMAGE, mi->image.svr_im,
		       0);
	    }
	    break;

#ifdef  OW_I18N
          case MENU_GEN_PULLRIGHT_ITEM:{
            int   len;
            /*
            if (mi->image.free_string && mi->image.string)
            */
            if (mi->image.string ) {
                free(mi->image.string);
                mi->image.string = NULL;
            }
            if( mi->image.string_mbs ) {
                free(mi->image.string_mbs);
                mi->image.string_mbs = NULL;
            }
            len =  strlen( (char *)attrs[1] );
            mi->image.string = (wchar_t *)mbstowcsdup( (char *) attrs[1]);
            mi->image.width = mi->image.height = 0;
            mi->gen_pullright = (Menu(*) ()) attrs[2];
            mi->pullright = mi->gen_pullright != NULL;
            mi->value = 0;
            if (mi->parent && mi->parent->pin_window && mi->panel_item_handle) {
                xv_set(mi->panel_item_handle,
                       PANEL_LABEL_STRING_WCS, mi->image.string,
                       0);
            }
            break;
          }
          case MENU_GEN_PULLRIGHT_ITEM_WCS:{
            int   len;
            /*
            if (mi->image.free_string && mi->image.string)
            */
            if ( mi->image.string) {
                free(mi->image.string);
                mi->image.string = NULL;
            }
            if( mi->image.string_mbs ) {
                free( mi->image.string_mbs );
                mi->image.string_mbs = NULL;
            }
            len = wslen( (wchar_t *)attrs[1] ) ;
            mi->image.string = (wchar_t *)xv_alloc_n(wchar_t, len +1 );
            (void)wscpy(mi->image.string , (wchar_t *) attrs[1]);
            mi->image.width = mi->image.height = 0;
            mi->gen_pullright = (Menu(*) ()) attrs[2];
            mi->pullright = mi->gen_pullright != NULL;
            mi->value = 0;
            if (mi->parent && mi->parent->pin_window && mi->panel_item_handle) {
                xv_set(mi->panel_item_handle,
                       PANEL_LABEL_STRING_WCS, mi->image.string,
                       0);
            }
            break;
          }
#else
	  case MENU_GEN_PULLRIGHT_ITEM:
	    if (mi->image.free_string && mi->image.string)
                free(mi->image.string);
            mi->image.string = (char *) attrs[1];
            mi->image.width = mi->image.height = 0;
	    mi->gen_pullright = (Menu(*) ()) attrs[2];
	    mi->pullright = mi->gen_pullright != NULL;
	    mi->value = 0;
	    if (mi->parent && mi->parent->pin_window && mi->panel_item_handle) {
		xv_set(mi->panel_item_handle,
		       PANEL_LABEL_STRING, mi->image.string,
		       0);
	    }
	    break;
#endif OW_I18N

	  case MENU_IMAGE:
	    if (mi->image.free_svr_im && mi->image.svr_im)
                xv_destroy(mi->image.svr_im);
            mi->image.svr_im = (Server_image) attrs[1];
            mi->image.width = mi->image.height = 0;
	    if (mi->parent && mi->parent->pin_window && mi->panel_item_handle) {
		xv_set(mi->panel_item_handle,
		       PANEL_LABEL_IMAGE, mi->image.svr_im,
		       0);
	    }
	    break;

	  case MENU_INACTIVE:
	    if (((int) attrs[1] ? TRUE : FALSE) != mi->inactive) {
		mi->inactive = (int) attrs[1];
		if (mi->parent && mi->parent->pin_window &&
		    mi->panel_item_handle) {
		    xv_set(mi->panel_item_handle,
			   PANEL_INACTIVE, mi->inactive,
			   0);
		}
	    }
	    break;

	  case MENU_INVERT:
	    mi->image.invert = (int) attrs[1];
	    break;

	  case MENU_PULLRIGHT:
	    mi->value = (Xv_opaque) attrs[1];
	    if (mi->value)
		MENU_PRIVATE(mi->value)->parent = mi;
	    mi->pullright = mi->value != NULL;
	    if (mi->parent && mi->parent->pin_window && mi->panel_item_handle) {
		xv_set(mi->panel_item_handle,
		       PANEL_ITEM_MENU, mi->value,
		       0);
	    }
	    break;

	  case MENU_PULLRIGHT_IMAGE:
	    if (mi->image.free_svr_im && mi->image.svr_im)
                xv_destroy(mi->image.svr_im);
            mi->image.svr_im = (Server_image) attrs[1];
            mi->image.width = mi->image.height = 0;
	    mi->value = (Xv_opaque) attrs[2];
	    mi->pullright = mi->value != NULL;
	    if (mi->parent && mi->parent->pin_window && mi->panel_item_handle) {
		xv_set(mi->panel_item_handle,
		       PANEL_LABEL_IMAGE, mi->image.svr_im,
		       PANEL_ITEM_MENU, mi->value,
		       0);
	    }
	    break;

#ifdef  OW_I18N
          case MENU_PULLRIGHT_ITEM:{
            int   len;
            /*
            if (mi->image.free_string && mi->image.string)
            */
            if (mi->image.string ) {
                free(mi->image.string);
                mi->image.string = NULL;
            }
            if( mi->image.string_mbs ) {
                free(mi->image.string_mbs);
                mi->image.string_mbs = NULL;
            }
            len =  strlen( (char *)attrs[1] );
            mi->image.string = (wchar_t *)mbstowcsdup( (char *) attrs[1]);
            mi->image.width = mi->image.height = 0;
            mi->value = (Xv_opaque) attrs[2];
            mi->pullright = mi->value != NULL;
            if (mi->parent && mi->parent->pin_window && mi->panel_item_handle) {
                xv_set(mi->panel_item_handle,
                       PANEL_LABEL_STRING_WCS, mi->image.string,
                       PANEL_ITEM_MENU, mi->value,
                       0);
            }
            break;
          }
          case MENU_PULLRIGHT_ITEM_WCS:{
            int   len;
            /*
            if (mi->image.free_string && mi->image.string)
            */
            if (mi->image.string ) {
                free(mi->image.string);
                mi->image.string = NULL;
            }
            if( mi->image.string_mbs ) {
                free(mi->image.string_mbs);
                mi->image.string_mbs = NULL;
            }
            len = wslen( (wchar_t *)attrs[1] ) ;
            mi->image.string = (wchar_t *)xv_alloc_n(wchar_t, len +1 );
            (void)wscpy(mi->image.string , (wchar_t *) attrs[1]);
            mi->image.width = mi->image.height = 0;
            mi->value = (Xv_opaque) attrs[2];
            mi->pullright = mi->value != NULL;
            if (mi->parent && mi->parent->pin_window && mi->panel_item_handle) {
                xv_set(mi->panel_item_handle,
                       PANEL_LABEL_STRING_WCS, mi->image.string,
                       PANEL_ITEM_MENU, mi->value,
                       0);
            }
            break;
          }
#else
	  case MENU_PULLRIGHT_ITEM:
	    if (mi->image.free_string && mi->image.string)
                free(mi->image.string);
            mi->image.string = (char *) attrs[1];
            mi->image.width = mi->image.height = 0;
	    mi->value = (Xv_opaque) attrs[2];
	    mi->pullright = mi->value != NULL;
	    if (mi->parent && mi->parent->pin_window && mi->panel_item_handle) {
		xv_set(mi->panel_item_handle,
		       PANEL_LABEL_STRING, mi->image.string,
		       PANEL_ITEM_MENU, mi->value,
		       0);
	    }
	    break;
#endif OW_I18N

	  case MENU_RELEASE:
	    mi->free_item = TRUE;
	    break;

	  case MENU_RELEASE_IMAGE:
	    mi->image.free_string = TRUE;
	    mi->image.free_svr_im = TRUE;
	    break;

	  case MENU_SELECTED:
	    mi->selected = (int) attrs[1];
	    break;

#ifdef  OW_I18N
          case MENU_STRING:{
            int   len;
            /*
            if (mi->image.free_string && mi->image.string)
            */
            if (mi->image.string ) {
                free(mi->image.string);
                mi->image.string = NULL;
            }
            if( mi->image.string_mbs ) {
                free(mi->image.string_mbs);
                mi->image.string_mbs = NULL;
            }
            len =  strlen( (char *)attrs[1] );
            mi->image.string = (wchar_t *)mbstowcsdup( (char *) attrs[1]);
            mi->image.width = mi->image.height = 0;
            if (mi->parent && mi->parent->pin_window && mi->panel_item_handle) {
                xv_set(mi->panel_item_handle,
                       PANEL_LABEL_STRING_WCS, mi->image.string,
                       0);
            }
            break;
          }
          case MENU_STRING_WCS:{
            int   len;
            /*
            if (mi->image.free_string && mi->image.string)
            */
            if (mi->image.string ) {
                free(mi->image.string);
                mi->image.string = NULL;
            }
            if( mi->image.string_mbs ) {
                free(mi->image.string_mbs);
                mi->image.string_mbs = NULL;
            }
            len = wslen( (wchar_t *)attrs[1] ) ;
            mi->image.string = (wchar_t *)xv_alloc_n(wchar_t, len +1);
            (void)wscpy(mi->image.string , (wchar_t *) attrs[1]);
            mi->image.width = mi->image.height = 0;
            if (mi->parent && mi->parent->pin_window && mi->panel_item_handle) {
                xv_set(mi->panel_item_handle,
                       PANEL_LABEL_STRING_WCS, mi->image.string,
                       0);
            }
            break;
          }
#else
	  case MENU_STRING:
	    if (mi->image.free_string && mi->image.string)
                free(mi->image.string);
            mi->image.string = (char *) attrs[1];
            mi->image.width = mi->image.height = 0;
	    if (mi->parent && mi->parent->pin_window && mi->panel_item_handle) {
		xv_set(mi->panel_item_handle,
		       PANEL_LABEL_STRING, mi->image.string,
		       0);
	    }
	    break;
#endif OW_I18N

	  case MENU_TITLE:
	    mi->title = TRUE;
	    mi->image.title = TRUE;
	    break;

	  case MENU_VALUE:
	    mi->value = (Xv_opaque) attrs[1];
	    mi->pullright = FALSE;
	    if (mi->parent && mi->parent->pin_window && mi->panel_item_handle) {
		xv_set(mi->panel_item_handle,
		       PANEL_ITEM_MENU, NULL,
		       0);
	    }
	    break;

	  case MENU_LINE_AFTER_ITEM:
	    switch ((int) attrs[1]) {
	      case MENU_HORIZONTAL_LINE:
		mi->h_line = 1;
		break;
	      case MENU_VERTICAL_LINE:
		mi->v_line = 1;
		break;
	      default:{
		    char            dummy[128];

		    (void) sprintf(dummy,
			   XV_MSG("Invalid argument for attribute MENU_LINE_AFTER_ITEM: %d"),
				   (int) attrs[1]);
		    xv_error(NULL,
			     ERROR_STRING, dummy,
			     ERROR_PKG, MENU,
			     0);
		}
	    }
	    break;

	  default:
	    bad_attr = TRUE;
	    break;

	}
	if (!bad_attr)
	    ATTR_CONSUME(attrs[0]);
    }

    return (Xv_opaque) XV_OK;
}


Pkg_private void
menu_destroys(m, destroy_proc)
    register Xv_menu_info *m;
    void            (*destroy_proc) ();
{
    register Xv_menu_item_info *mi;
    Xv_Drawable_info *info;

    if (!m || m->type != (int) MENU_MENU)
	return;
    if (m->item_list) {
	for (; m->nitems-- > 0;) {
	    mi = m->item_list[m->nitems];
	    if (mi->pullright && !mi->gen_pullright && mi->value) {
#ifdef NOTIFIER_WARNING_FIXED
		xv_destroy(mi->value);
#else
		/*** BUG ALERT!  The following line will leak memory
		 * because the Generic object's data structures are
		 * not freed.  However, there's a possible conflict
		 * with the submenu already being destroyed by the
		 * application.  Implementing the xv_destroy alternative
		 * on the previous line causes 
		 *     XView warning: Notifier error: Unknown client
		 * to be printed out when mailtool is quit.
		 */
		menu_destroys((Xv_menu_info *) mi->value, destroy_proc);
#endif
	    }
	    xv_destroy(MENU_ITEM_PUBLIC(mi));
	}
	free(m->item_list);
    }
    if (m->window) {
	DRAWABLE_INFO_MACRO(m->window, info);
	screen_set_cached_window_busy(xv_screen(info),
				      m->window, FALSE);
    }
    if (m->shadow_window) {
	DRAWABLE_INFO_MACRO(m->shadow_window, info);
	screen_set_cached_window_busy(xv_screen(info),
				      m->shadow_window, FALSE);
    }
    if (destroy_proc)
	destroy_proc(m, MENU_MENU);
    free(m);
}


Pkg_private void
menu_item_destroys(mi, destroy_proc)
    register Xv_menu_item_info *mi;
    void            (*destroy_proc) ();
{
    if (!mi || !mi->free_item)
	return;
    if (mi->image.free_image) {
	if (mi->image.free_string && mi->image.string)
	    free(mi->image.string);
	if (mi->image.free_svr_im && mi->image.svr_im)
	    xv_destroy(mi->image.svr_im);
    }
    if (destroy_proc)
	destroy_proc(MENU_ITEM_PUBLIC(mi), MENU_ITEM);
    free((char *) mi);
}


static void
menu_add_pin(m)
    Xv_menu_info *m;
{
    m->pin = TRUE;

    /* Add pushpin-out image to menu title */
    if (!m->item_list[0] || !m->item_list[0]->title)
	menu_create_title(m, 0, (Xv_opaque) 0);

    /* Force recomputation of item size in compute_item_size */
    m->item_list[0]->image.width = 0;
}


Pkg_private void
menu_set_pin_window(m, pin_window)
    Xv_menu_info   *m;
    Xv_opaque	    pin_window;
{
    m->pin_window = pin_window;
    if (m->pin_window) {
	xv_set(m->pin_window, XV_KEY_DATA, MENU_MENU, m, 0);
	notify_interpose_event_func(m->pin_window,
	    menu_pin_window_event_proc, NOTIFY_SAFE);
    }
}


static int
extend_item_list(m)
    register Xv_menu_info *m;
{
    m->max_nitems = m->max_nitems + MENU_FILLER;
    m->item_list = (Xv_menu_item_info **) xv_realloc(
						  (char *) m->item_list,
		       (u_int) (m->max_nitems * sizeof(Xv_menu_item_info)));
    if (!m->item_list) {
	xv_error((Xv_opaque)m,
		 ERROR_LAYER, ERROR_SYSTEM,
		 ERROR_STRING,
		     XV_MSG("menu_set: Malloc failed to allocate an item list"),
		 ERROR_PKG, MENU,
		 0);
	m->max_nitems = m->max_nitems - MENU_FILLER;
	return FALSE;
    }
    return TRUE;
}


static void
remove_item(m, pos)
    Xv_menu_info *m;
    int pos;	/* position: 1= first item */
{
    register Xv_menu_item_info **il = m->item_list;
    register int    i;

    if (pos < 1 || pos > m->nitems)
	return;	/* invalid position */
    if (pos == 1 && il[0]->title && m->pin)
	m->pin = FALSE;
    for (i = pos; i < m->nitems; i++)
	il[i - 1] = il[i];
    --m->nitems;
    if (!m->ncols_fixed)
	m->ncols = 0;
    if (!m->nrows_fixed)
	m->nrows = 0;
    return;
}


static void
replace_item(il, len, pos, mi)
    Xv_menu_item_info *il[];	/* item list ptr */
    int len;	/* nbr of menu items */
    int pos;	/* position: 1= first item */
    Xv_menu_item_info *mi;
{
    if (pos < 1 || pos > len)
	return; /* invalid position */
    il[pos - 1] = mi;
    return;
}


static void
insert_item(m, pos, mi)
    Xv_menu_info *m;
    int pos;	/* position: 1= first item */
    Xv_menu_item_info *mi;
{
    register Xv_menu_item_info **il = m->item_list;
    register int    i;

    if (pos < 0 || pos >= m->nitems) {
	--m->nitems;
	return; /* invalid position */
    }
    for (i = m->nitems - 1; i > pos; --i)
	il[i] = il[i - 1];
    il[i] = mi;
    if (!m->ncols_fixed)
	m->ncols = 0;
    if (!m->nrows_fixed)
	m->nrows = 0;
    return;
}


static int
lookup(il, len, mi)
    register Xv_menu_item_info *il[];
    Xv_menu_item_info *mi;
{
    int             i;

    for (i = 0; i < len; i++)
	if (il[i] == mi)
	    return i + 1;
    return -1;
}


static void
menu_create_title(m, type, arg1)
    register Xv_menu_info *m;
    int             type;	/* MENU_STRING, MENU_IMAGE or 0 (= no title) */
    Xv_opaque       arg1;	/* the string or pixrect */
{
    register int    i;
    Menu_item       menu_item;

    if (m->nitems < m->max_nitems || extend_item_list(m)) {
	m->nitems++;
	for (i = m->nitems - 1; i > 0; i--)
	    m->item_list[i] = m->item_list[i - 1];
	menu_item = xv_create(NULL, MENUITEM,
			      MENU_FEEDBACK, FALSE,
			      MENU_RELEASE,
			      MENU_TITLE,
			      MENU_NOTIFY_PROC, menu_return_no_value,
			      0);
	m->item_list[0] = MENU_ITEM_PRIVATE(menu_item);
	if (type)
	    xv_set(menu_item,
		   type, arg1,
		   MENU_LINE_AFTER_ITEM, MENU_HORIZONTAL_LINE,
		   0);
	if (m->default_position == 1)
	    m->default_position++;
    }
}

static void
destroy_panel_item_handles(m)
    register Xv_menu_info *m;
{
    int	panel_item_destroyed = FALSE;	/* for Choice and Toggle menus */
    int	i;

    for (i=0; i < m->nitems; i++) {
        if (m->item_list[i]->panel_item_handle) {
	    if (m->class == MENU_COMMAND) {
		xv_set(m->item_list[i]->panel_item_handle,
		       PANEL_ITEM_MENU, NULL,
		       0);
		xv_destroy(m->item_list[i]->panel_item_handle);
	    } else if (!panel_item_destroyed) {
		xv_destroy(m->item_list[i]->panel_item_handle);
		panel_item_destroyed = TRUE;
	    }
	    m->item_list[i]->panel_item_handle = NULL;
	    }
    }
}
