#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)p_callback.c 50.24 91/09/14";
#endif
#endif

/*
 *	(c) Copyright 1989 Sun Microsystems, Inc. Sun design patents 
 *	pending in the U.S. and foreign countries. See LEGAL NOTICE 
 *	file for terms of the license.
 */

#include <xview_private/panel_impl.h>

#define	ITERM_BUFSIZE	1024

/* get the private handle, allocate buffer for intermediate text and
 * attach it to the panel.
 */

void
panel_text_start (ic, client_data, callback_data)
    XIC		ic;
    XPointer	client_data;
    XPointer	callback_data;
{
    Panel	panel_public;
    Panel_info	*panel;

    /* Get the panel handle from xim */
    panel_public = (Panel)client_data;
    panel = PANEL_PRIVATE(panel_public);

    /* Ignore all request if there's nothing with keyboard
     * focus, the if the item with the focus is 
     * not panel text item
     */

    if (!panel->kbd_focus_item)
	    return;

    if (panel->kbd_focus_item->item_type != PANEL_TEXT_ITEM)
	    return;

    /*  Save the handle of the item with focus
     *  for implicit commit purpose
     */

    panel->pre_edit_item = panel->kbd_focus_item;

    /* store the current_caret_offset */
    ml_panel_saved_caret(panel->pre_edit_item);

}

		

void
panel_text_draw(ic, client_data, callback_data)
    XIC			  ic;
    XPointer		  client_data;
    XPointer		  callback_data;

{
    Panel		  panel_public;
    Panel_info 		  *panel;
    XIMPreeditDrawCallbackStruct  *pre_edit_changes;
    
    /* Get the panel handle from xim */
    panel_public = (Panel)client_data;
    panel = PANEL_PRIVATE(panel_public);

    
    /* Ignore all request if there's nothing with
     * keyboard focus, and if the item with the focus is
     * not a panel text item
     */

    if (!panel->kbd_focus_item)
	    return;

    if (panel->kbd_focus_item->item_type != PANEL_TEXT_ITEM)
	    return;

    /* Get the pre_edit text from xim */
    pre_edit_changes = (XIMPreeditDrawCallbackStruct *)callback_data;

    /*  panel_text_start may not have been called, therefore
     *  panel->pre_edit_item may not have an value.  So
     *  it should be the item under focus if it's null
     */
    if (!panel->pre_edit_item) panel->pre_edit_item = panel->kbd_focus_item;
    pre_edit_display(panel->pre_edit_item, pre_edit_changes);


}

void
panel_text_done(ic, client_data, callback_data)
    XIC		ic;
    XPointer	client_data;
    XPointer	callback_data;

{
    Panel	panel_public;
    Panel_info	*panel;
    
    /* Get the panel handle from xim */
    panel_public = (Panel)client_data;
    panel = PANEL_PRIVATE(panel_public);

    /* Ignore all request if there's no item with keyboard
     * focus
     */

    if (!panel->kbd_focus_item)
	    return;

    /* Ignore all request if this is not a panel
     * text item.
     */

    if (panel->kbd_focus_item->item_type != PANEL_TEXT_ITEM)
	    return; 

    /* saved the caret offset, 
     * and zero out the pre_edit item handle 
     */
    ml_panel_saved_caret(panel->pre_edit_item);
    panel->pre_edit_item = 0;
    panel->pre_edit->text->string.wide_char[0] = 0;
}

void
pre_edit_display(ip, pre_edit_changes)

    Item_info			 *ip;
    XIMPreeditDrawCallbackStruct *pre_edit_changes;
{
    int			  length;

    /*  IMLogic gives me only partial update information
     *  for pre_edit text, therefore I have to piece
     *  together the full pre_edit text by
     *  calling the function 'cache_text_state'
     *  HAVE TO OPTIMIZE LATER:  panel should not
     *  use cache_text_state, should do my own
     *  partial update to speed things up.
     */

    cache_text_state(pre_edit_changes, ip->panel->pre_edit);
    length = ip->panel->pre_edit->text->length;
    ip->panel->pre_edit->text->string.wide_char[length] = 0;

    if (ip->ignore_im != TRUE) ml_panel_display_interm(ip);
}

/*
void
panel_text_caret(ic, direction, udata)
    XIC			ic;
    IMTextDirection	direction;
    caddr_t		udata;
{
	interprete direction and calculate row and col;
	set cursor position
}
	
*/
