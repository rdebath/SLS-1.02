#ifndef lint
static char sccsid[] = "@(#)props.c 1.64 91/09/14";
#endif


#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "props.h"

#include <xview/text.h>
#include <xview/font.h>
#include <xview/notice.h>
#include <xview/font.h>
#include <xview/cms.h>

#include <locale.h>
#include <sys/param.h> 
#ifdef OW_I18N
#include <widec.h>
#include <mltext/XFontSet.h>
#endif /* OW_I18N */

#define MONO_PROPS LOCALIZE("Icons"), LOCALIZE("Menus"), LOCALIZE("Miscellaneous"), LOCALIZE("Mouse Settings"), LOCALIZE("Localization"), NULL
#define COLOR_PROPS LOCALIZE("Color"), MONO_PROPS


Frame       frame;
Panel       flavor_panel;
Panel_item  flavor_choice;
Panel       panel_group[TOTAL_PANELS + 1];
Panel       current_panel;
Panel_item  apply_button[TOTAL_PANELS + 1];
Panel_item  reset_button[TOTAL_PANELS + 1];
int         showing_factory;
static char saved_defaults[20];
static char user_defaults[256];
int         color = FALSE;
Display    *dsp;

Description *
allocate_desc_struct(name, class, type, default_value, misc, item, msg_item)
    char       *name;
    char       *class;
    Deftype     type;
    caddr_t     default_value;
    caddr_t     misc;
    Panel_item  item;
    Panel_item  msg_item;
{
    Description *new_struct;

    new_struct = (Description *) malloc(sizeof(Description));

    if (new_struct) {
	new_struct->name = (char *) strdup(name);
	new_struct->class = (char *) strdup(class);
	new_struct->type = type;
	if ((type == D_number) || (type == D_boolean))
	    new_struct->default_value = default_value;
	else
	    new_struct->default_value = (char *) strdup(default_value);
	if ((type == D_string) && misc)
	    new_struct->misc = (caddr_t) strdup(misc);
	else
	    new_struct->misc = misc;
	new_struct->change_mark = FALSE;
	new_struct->change_mark_item = msg_item;
	new_struct->panel_item = item;
    }
    return (new_struct);
}

Panel_setting
add_change_bar(panel_item, event)
    Panel_item  panel_item;
    Event      *event;
{
    Description *id;

    id = (Description *) xv_get(panel_item, PANEL_CLIENT_DATA);

    if (!id->change_mark) {
	xv_set(id->change_mark_item,
	       PANEL_LABEL_STRING, LOCALIZE("|"),
	       NULL);

	id->change_mark = TRUE;
    }
    if ((Panel_item_type) xv_get(panel_item, PANEL_ITEM_CLASS) ==
	    PANEL_TEXT_ITEM)
	return (panel_text_notify(panel_item, event));
    return PANEL_NONE;		/* return value is ignored */
}

static void
destroy_change_bars()
{
    int         i;
    Panel_item  local_item;
    Description *id;

    for (i = color; i < TOTAL_PANELS + color; i++) {
	PANEL_EACH_ITEM(panel_group[i], local_item) {
	    id = (Description *) xv_get(local_item,
					PANEL_CLIENT_DATA);

#ifdef OW_I18N
	    if (!id &&
		    ((Panel_item_type) xv_get(local_item, PANEL_ITEM_CLASS) ==
		     PANEL_MESSAGE_ITEM) && (wscmp((wchar_t *)xv_get(local_item, PANEL_LABEL_STRING_WCS), mbstowcsdup(LOCALIZE("|")))) == 0)
#else
            if (!id && ((Panel_item_type) xv_get(local_item, PANEL_ITEM_CLASS) == 
			PANEL_MESSAGE_ITEM) && (strcmp((char *)xv_get(local_item, PANEL_LABEL_STRING), "|") == 0))
#endif

		/* Remove change bar from message item */
		xv_set(local_item, PANEL_LABEL_STRING, LOCALIZE(" "), NULL);
	    else if (id && ((int) id->type < D_nop))
		id->change_mark = FALSE;
	} PANEL_END_EACH
    }
}


static void
apply_props_proc(panel_item, event)
    Panel_item  panel_item;
    Event      *event;
{
    int         i;
    Panel_item  local_item;
    Description *id;
    int         result;
    Event       ie;

    if (panel_item) {
	result = notice_prompt(frame, &ie,
			       NOTICE_MESSAGE_STRINGS,
			       LOCALIZE(" Applying your changes will modify\n your ~/.Xdefaults file.  All comments\n in your file will be lost.  Do you\n want to do this?"),
			       NULL,
			       NOTICE_BUTTON_YES, LOCALIZE("Yes"),
			       NOTICE_BUTTON_NO, LOCALIZE("No"),
			       NULL);

	if (result == NOTICE_NO) {
	    xv_set(panel_item, PANEL_NOTIFY_STATUS, XV_ERROR, NULL);
	    return;
	}
    }
    if (color)
	apply_colors();
    for (i = color; i < TOTAL_PANELS + color; i++) {
	PANEL_EACH_ITEM(panel_group[i], local_item) {
	    if (id = (Description *) xv_get(local_item, PANEL_CLIENT_DATA)) {
		switch (id->type) {
		case D_number:
		    if ((Panel_item_type) xv_get(id->panel_item,
					PANEL_ITEM_CLASS) == PANEL_TEXT_ITEM)
			defaults_set_integer(id->name,
					     atoi(xv_get(id->panel_item,
							 PANEL_VALUE)));
		    else
			defaults_set_integer(id->name,
					xv_get(id->panel_item, PANEL_VALUE));
		    break;
		case D_string:
		    if ((Panel_item_type) xv_get(id->panel_item,
					PANEL_ITEM_CLASS) == PANEL_TEXT_ITEM)
			defaults_set_string(id->name,
					xv_get(id->panel_item, PANEL_VALUE));
		    else {
			int         index;
			int         i;
			char       *options_copy;
			char       *scanner;

			index = (int) xv_get(id->panel_item,
					     PANEL_VALUE);
			options_copy = (char *) strdup(id->misc);
			scanner = (char *) strtok(options_copy, ":");
			for (i = 0; i < index; i++)
			    scanner = strtok(NULL, ":");
			defaults_set_string(id->name, scanner);
			free(options_copy);
		    }
		    break;
		case D_boolean:
		    defaults_set_boolean(id->name,
					 xv_get(id->panel_item,
						PANEL_VALUE));
		    break;
		}
	    }
	} PANEL_END_EACH;
    }
    if (panel_item) {
	unlink(user_defaults);
	defaults_store_db(user_defaults);
	destroy_change_bars();
	xv_set(panel_item, PANEL_NOTIFY_STATUS, XV_ERROR, NULL);
    }
}

static void
set_props_values(panel_item, event)
    Panel_item  panel_item;
    Event      *event;
{
    int         i,
                tmp;
    Panel_item  local_item;
    Description *id;
    char        number_string[20];

    for (i = color; i < TOTAL_PANELS + color; i++) {
	PANEL_EACH_ITEM(panel_group[i], local_item) {

	    if (!(id = (Description *) xv_get(local_item, PANEL_CLIENT_DATA)))
		continue;

	    switch (id->type) {
	    case D_number:
		tmp = defaults_get_integer(id->name, id->class,
					   id->default_value);
		if ((Panel_item_type) xv_get(id->panel_item, PANEL_ITEM_CLASS)
			== PANEL_TEXT_ITEM) {
		    sprintf(number_string, "%d", tmp);
		    xv_set(id->panel_item, PANEL_VALUE, number_string, NULL);
		} else
		    xv_set(id->panel_item, PANEL_VALUE, tmp, NULL);
		break;
	    case D_string:
		if ((Panel_item_type) xv_get(id->panel_item, PANEL_ITEM_CLASS)
			== PANEL_TEXT_ITEM) {
		    xv_set(id->panel_item, PANEL_VALUE,
			   defaults_get_string(id->name, id->class,
					       id->default_value),
			   NULL);
		} else {
		    char       *value;
		    int         i = 0;
		    char       *options_copy;
		    char       *scanner;

		    value = defaults_get_string(id->name, id->class,
						id->default_value);
		    options_copy = (char *) strdup(id->misc);
		    scanner = strtok(options_copy, ":");
		    while (scanner != NULL && strcmp(scanner, value)) {
			i++;
			scanner = strtok(NULL, ":");
		    }
		    if (!scanner)
			i = 0;

		    free(options_copy);

		    xv_set(id->panel_item, PANEL_VALUE, i, NULL);
		}
		break;
	    case D_boolean:
		xv_set(id->panel_item, PANEL_VALUE,
		       defaults_get_boolean(id->name, id->class,
					    id->default_value),
		       NULL);
		break;
	    default:
		break;
	    }
	} PANEL_END_EACH;
    }
}


static void
reset_props_proc(panel_item, event)
    Panel_item  panel_item;
    Event      *event;
{
	
    extern Panel_item l10n_bs;

    defaults_init_db();
    defaults_load_db(NULL);
    defaults_load_db(user_defaults);

    /* 
     * Localization values for Specific Settings based on
     * Basic Setting.  reset_localization() added since 
     * set_props_values() does not understand this dependency.
     */
    if (!xv_get(l10n_bs, PANEL_INACTIVE))
    	reset_localization();

    set_props_values(panel_item, event);

    /* Remove all of the change bars that were inserted */
    destroy_change_bars();

    if (color)
	reset_colors();

    xv_set(panel_item, PANEL_NOTIFY_STATUS, XV_ERROR, NULL);
}


create_icon_panel()
{
    Panel_item  choice_item;
    Panel_item  msg_item;

    xv_set(panel_group[ICON_PANEL + color],
	   XV_HELP_DATA, "props:IconPanelInfo",
	   NULL);

    msg_item = xv_create(panel_group[ICON_PANEL + color],
			 PANEL_MESSAGE,
			 PANEL_LABEL_STRING, LOCALIZE(" "),
			 NULL);
    choice_item =
	xv_create(panel_group[ICON_PANEL + color], PANEL_CHOICE,
		  PANEL_LABEL_STRING, LOCALIZE("Location:"),
		  PANEL_CHOICE_STRINGS, LOCALIZE("Top"), LOCALIZE("Bottom"), 
			LOCALIZE("Left"), LOCALIZE("Right"), NULL,
		  PANEL_NOTIFY_PROC, add_change_bar,
		  XV_HELP_DATA, "props:IconLocationInfo",
		  NULL);

    xv_set(choice_item,
	   PANEL_CLIENT_DATA,
	   allocate_desc_struct("OpenWindows.IconLocation", 
				"OpenWindows.IconLocation", D_string, 
				"bottom", "top:bottom:left:right",
				choice_item, msg_item),
	   NULL);

    window_fit_width(panel_group[ICON_PANEL + color]);
}


create_menu_panel()
{
    Panel_item  dr_item, mp_item;
    Panel_item  msg_item;
    int		i;

    xv_set(panel_group[MENU_PANEL + color],
	   XV_HELP_DATA, "props:MenuPanelInfo",
	   NULL);

    msg_item = xv_create(panel_group[MENU_PANEL + color],
			 PANEL_MESSAGE,
			 PANEL_LABEL_STRING, LOCALIZE(" "),
			 NULL);
    dr_item = xv_create(panel_group[MENU_PANEL + color], PANEL_TEXT,
		     PANEL_LABEL_STRING, LOCALIZE("Drag-Right distance (pixels):"),
		     PANEL_VALUE_DISPLAY_LENGTH, 2,
		     PANEL_NOTIFY_PROC, add_change_bar,
		     PANEL_NOTIFY_LEVEL, PANEL_ALL,
		     XV_HELP_DATA, "props:MenuDragRightInfo",
		     NULL);
    xv_set(dr_item,
	   PANEL_CLIENT_DATA,
	   allocate_desc_struct("OpenWindows.DragRightDistance", 
				"OpenWindows.DragRightDistance", D_number,
				(caddr_t) 100, (caddr_t) 0, dr_item, msg_item),
	   NULL);

    /*
     * The PANEL_VALUE of "Selects Default" is 0 == FALSE. The PANEL_VALUE of
     * "Displays Menu" is 1 == TRUE. So, the name of the boolean resource is
     * "SelectDisplaysMenu".
     */
    msg_item = xv_create(panel_group[MENU_PANEL + color],
			 PANEL_MESSAGE,
			 PANEL_LABEL_STRING, LOCALIZE(" "),
			 PANEL_NEXT_ROW, -1,
			 NULL);
    mp_item = xv_create(panel_group[MENU_PANEL + color], PANEL_CHOICE,
		     PANEL_LABEL_STRING, LOCALIZE("SELECT Mouse Press:"),
		     PANEL_CHOICE_STRINGS,
		     LOCALIZE("Selects Default"), LOCALIZE("Displays Menu"), 0,
		     PANEL_NOTIFY_PROC, add_change_bar,
		     XV_HELP_DATA, "props:SelectMousePressInfo",
		     NULL);
    xv_set(mp_item,
	   PANEL_CLIENT_DATA,
	   allocate_desc_struct("OpenWindows.SelectDisplaysMenu",
				"OpenWindows.SelectDisplaysMenu", D_boolean,
				(caddr_t) FALSE, (caddr_t) 0, mp_item, msg_item),
	   NULL);

    { int a,b;

      a = xv_get(dr_item, PANEL_LABEL_WIDTH);
      b = xv_get(mp_item, PANEL_LABEL_WIDTH);

      i = MAX_VALUE(a,b) + xv_get(msg_item, PANEL_LABEL_WIDTH) + DEFAULT_X_GAP;
    }
    xv_set(dr_item, PANEL_VALUE_X, i, NULL);
    xv_set(mp_item, PANEL_VALUE_X, i, NULL);

    window_fit_width(panel_group[MENU_PANEL + color]);
}


create_misc_panel()
{
    Panel       misc_panel = panel_group[MISC_PANEL + color];
    Panel_item  msg_item;
    Panel_item	bp_item, scl_item, ia_item, sbp_item;
    int		i;


    xv_set(misc_panel,
	   XV_HELP_DATA, "props:MiscPanelInfo",
	   NULL);

    msg_item = xv_create(misc_panel, PANEL_MESSAGE,
			 PANEL_LABEL_STRING, LOCALIZE(" "),
			 NULL);
    bp_item = xv_create(misc_panel, PANEL_CHOICE,
			 PANEL_LABEL_STRING, LOCALIZE("Beep:"),
			 PANEL_CHOICE_STRINGS,
			 LOCALIZE("Always"),
			 LOCALIZE("Notices Only"),
			 LOCALIZE("Never"),
			 0,
			 PANEL_NOTIFY_PROC, add_change_bar,
			 XV_HELP_DATA, "props:Beep",
			 NULL);

    xv_set(bp_item,
	   PANEL_CLIENT_DATA,
	   allocate_desc_struct("OpenWindows.Beep",
				"OpenWindows.Beep", D_string, "always",
				"always:notices:never", bp_item, msg_item),
	   NULL);

#ifdef RESCALE_ENTRY
    msg_item = xv_create(misc_panel, PANEL_MESSAGE,
			 PANEL_LABEL_STRING, LOCALIZE(" "),
			 PANEL_NEXT_ROW, -1,
			 NULL);
    scl_item = xv_create(misc_panel, PANEL_CHOICE,
			 PANEL_LABEL_STRING, LOCALIZE("Scale at Startup:"),
			 PANEL_CHOICE_STRINGS,
			 LOCALIZE("Small"),
			 LOCALIZE("Medium"),
			 LOCALIZE("Large"),
			 LOCALIZE("Extra Large"),
			 0,
			 PANEL_NOTIFY_PROC, add_change_bar,
			 XV_HELP_DATA, "props:InitialScaleInfo",
			 NULL);

    xv_set(scl_item,
	   PANEL_CLIENT_DATA,
	   allocate_desc_struct("Window.Scale",
				"Window.Scale", D_string, "medium",
				"small:medium:large:extra_large", scl_item,
				msg_item),
	   NULL);
#endif				/* RESCALE_ENTRY */

    msg_item = xv_create(misc_panel, PANEL_MESSAGE,
			 PANEL_LABEL_STRING, LOCALIZE(" "),
			 PANEL_NEXT_ROW, -1,
			 NULL);
    ia_item = xv_create(misc_panel, PANEL_CHOICE,
			 PANEL_LABEL_STRING, LOCALIZE("Set Input Area:"),
			 PANEL_CHOICE_STRINGS,
			 LOCALIZE("Click SELECT"),
			 LOCALIZE("Move Pointer"),
			 0,
			 PANEL_NOTIFY_PROC, add_change_bar,
			 XV_HELP_DATA, "props:InputFocusInfo",
			 NULL);

    xv_set(ia_item,
	   PANEL_CLIENT_DATA,
	   allocate_desc_struct("OpenWindows.SetInput",
				"OpenWindows.SetInput", D_string, "select",
				"select:followmouse", ia_item, msg_item),
	   NULL);

    msg_item = xv_create(misc_panel, PANEL_MESSAGE,
			 PANEL_LABEL_STRING, LOCALIZE(" "),
			 PANEL_NEXT_ROW, -1,
			 NULL);
    sbp_item = xv_create(misc_panel, PANEL_CHOICE,
			 PANEL_LABEL_STRING, LOCALIZE("Scrollbar Placement:"),
			 PANEL_CHOICE_STRINGS,
			 LOCALIZE("Left"),
			 LOCALIZE("Right"),
			 0,
			 PANEL_NOTIFY_PROC, add_change_bar,
			 XV_HELP_DATA, "props:ScrollbarPlacementInfo",
			 NULL);

    xv_set(sbp_item,
	   PANEL_CLIENT_DATA,
    	   allocate_desc_struct("OpenWindows.ScrollbarPlacement",
			"OpenWindows.ScrollbarPlacement", D_string, "right",
			"left:right", sbp_item, msg_item),
	   NULL);

    /*
     * tmp_item = xv_create(misc_panel, PANEL_MESSAGE, PANEL_LABEL_STRING,
     * "SELECT Always Brings", PANEL_LABEL_BOLD, TRUE, NULL); xv_set(tmp_item,
     * XV_Y, xv_row(misc_panel, 3), XV_X, 225 - (int) xv_get(tmp_item,
     * XV_WIDTH, NULL), NULL);
     * 
     * tmp_item = xv_create(misc_panel, PANEL_CHECK_BOX, PANEL_LABEL_STRING,
     * "Window Forward:", PANEL_NOTIFY_PROC, add_change_bar, PANEL_VALUE_X,
     * 225, XV_Y, xv_row(misc_panel, 4), XV_HELP_DATA, "props:InputFocusInfo",
     * NULL);
     * 
     * xv_set(tmp_item, PANEL_CLIENT_DATA,
     * allocate_desc_struct("OpenWindows.AutoRaise", D_boolean, (caddr_t)
     * FALSE, (caddr_t) 0, tmp_item), NULL);
     */

    { int a,b,c,d;

      a = xv_get(bp_item,  PANEL_LABEL_WIDTH);
#ifdef RESCALE_ENTRY
      b = xv_get(scl_item, PANEL_LABEL_WIDTH);
#endif
      c = xv_get(ia_item,  PANEL_LABEL_WIDTH);
      d = xv_get(sbp_item, PANEL_LABEL_WIDTH);

#ifdef RESCALE_ENTRY
      i = MAX_VALUE(a,MAX_VALUE(b,MAX_VALUE(c,d))) + xv_get(msg_item, PANEL_LABEL_WIDTH) + DEFAULT_X_GAP;
#else
      i = MAX_VALUE(a,MAX_VALUE(c,d)) + xv_get(msg_item, PANEL_LABEL_WIDTH) + DEFAULT_X_GAP;
#endif
    }
    xv_set(bp_item,  PANEL_VALUE_X, i, NULL);
#ifdef RESCALE_ENTRY
    xv_set(scl_item, PANEL_VALUE_X, i, NULL);
#endif
    xv_set(ia_item,  PANEL_VALUE_X, i, NULL);
    xv_set(sbp_item, PANEL_VALUE_X, i, NULL);

    window_fit_width(panel_group[MISC_PANEL + color]);
}

create_mouse_set_panel()
{
    Panel_item  msg_item;
    Panel       mouse_set_panel;
    Panel_item  sbj_item, ppj_item, mct_item;
    int		i;

    mouse_set_panel = panel_group[MOUSE_SET_PANEL + color];
    xv_set(mouse_set_panel,
	   XV_HELP_DATA, "props:MouseSetPanelInfo",
	   NULL);

    msg_item = xv_create(mouse_set_panel, PANEL_MESSAGE,
			 PANEL_LABEL_STRING, LOCALIZE(" "),
			 NULL);
    sbj_item = xv_create(mouse_set_panel, PANEL_CHECK_BOX,
			 PANEL_LABEL_STRING, LOCALIZE("Scrollbar Pointer Jumping:"),
			 PANEL_NOTIFY_PROC, add_change_bar,
		       XV_HELP_DATA, "props:ScrollbarPointerJumpInfo", NULL);

    xv_set(sbj_item,
	   PANEL_CLIENT_DATA,
	   allocate_desc_struct("Scrollbar.JumpCursor", 
				"Scrollbar.JumpCursor", D_boolean,
				(caddr_t) TRUE, (caddr_t) 0, sbj_item,
				msg_item),
	   NULL);

    msg_item = xv_create(mouse_set_panel, PANEL_MESSAGE,
			 PANEL_LABEL_STRING, LOCALIZE(" "),
			 PANEL_NEXT_ROW, -1,
			 NULL);
    ppj_item = xv_create(mouse_set_panel, PANEL_CHECK_BOX,
			 PANEL_LABEL_STRING, LOCALIZE("Pop-up Pointer Jumping:"),
			 PANEL_NOTIFY_PROC, add_change_bar,
			 XV_HELP_DATA, "props:PopupPointerJumpInfo",
			 NULL);

    xv_set(ppj_item,
	   PANEL_CLIENT_DATA,
	   allocate_desc_struct("OpenWindows.PopupJumpCursor",
				"OpenWindows.PopupJumpCursor", D_boolean,
				(caddr_t) TRUE, (caddr_t) 0, ppj_item,
				msg_item),
	   NULL);

    msg_item = xv_create(mouse_set_panel, PANEL_MESSAGE,
			 PANEL_LABEL_STRING, LOCALIZE(" "),
			 PANEL_NEXT_ROW, -1,
			 NULL);
    mct_item = xv_create(mouse_set_panel, PANEL_SLIDER,
			 PANEL_LABEL_STRING, LOCALIZE("Multi-click Timeout (sec/10) :"),
			 PANEL_MIN_VALUE, 2,
			 PANEL_MAX_VALUE, 10,
			 PANEL_SHOW_RANGE, TRUE,
			 PANEL_NOTIFY_PROC, add_change_bar,
			 XV_HELP_DATA, "props:MultiClickInfo",
			 NULL);

    xv_set(mct_item,
	   PANEL_CLIENT_DATA,
	   allocate_desc_struct("OpenWindows.MultiClickTimeout",
				"OpenWindows.MultiClickTimeout", D_number,
				(caddr_t) 4, (caddr_t) 0, mct_item,
				msg_item),
	   NULL);

    { int a,b,c,d;

      a = xv_get(sbj_item, PANEL_LABEL_WIDTH);
      b = xv_get(ppj_item, PANEL_LABEL_WIDTH);
      c = xv_get(mct_item, PANEL_LABEL_WIDTH);

      i = MAX_VALUE(a,MAX_VALUE(b,c)) + xv_get(msg_item, PANEL_LABEL_WIDTH) + DEFAULT_X_GAP;
    }
    xv_set(sbj_item,  PANEL_VALUE_X, i, NULL);
    xv_set(ppj_item,  PANEL_VALUE_X, i, NULL);
    xv_set(mct_item,  PANEL_VALUE_X, i, NULL);

    window_fit_width(mouse_set_panel);
}

add_buttons()
{
    Font_string_dims apply_size;
    Font        font;
    int         i;
    Font_string_dims reset_size;
    int         width;

#ifdef OW_I18N
    XFontSet	font_set;
    wchar_t	*ws;
    Display	*dpy;
    XRectangle     overall_ink_extents, overall_logical_extents;

    font = xv_get(flavor_panel, WIN_FONT);
    dpy = (Display *)xv_get(flavor_panel, XV_DISPLAY);
    font_set= (XFontSet)xv_get(font, FONT_SET_ID);
    ws = (wchar_t *)mbstowcsdup(LOCALIZE("Apply"));
    XwcTextExtents(font_set, ws, wslen(ws),
                &overall_ink_extents, &overall_logical_extents);
    apply_size.width = overall_logical_extents.width;
    ws = (wchar_t *)mbstowcsdup(LOCALIZE("Reset"));
    XwcTextExtents(font_set, ws, wslen(ws),
                &overall_ink_extents, &overall_logical_extents);
    reset_size.width = overall_logical_extents.width;

#else

    font = xv_get(flavor_panel, WIN_FONT);
    xv_get(font, FONT_STRING_DIMS, LOCALIZE("Apply"), &apply_size);
    xv_get(font, FONT_STRING_DIMS, LOCALIZE("Reset"), &reset_size);

#endif

    for (i = 0; i < TOTAL_PANELS + color; i++) {
	width = (int) xv_get(panel_group[i], XV_WIDTH);
	xv_create(panel_group[i], PANEL_BUTTON,
		  PANEL_LABEL_STRING, LOCALIZE("Apply"),
		  PANEL_NOTIFY_PROC, apply_props_proc,
		  PANEL_NEXT_ROW, -1,
		  XV_X, (width / 4) - (apply_size.width / 2),
		  XV_HELP_DATA, "props:ApplyInfo",
		  NULL);

	xv_create(panel_group[i], PANEL_BUTTON,
		  PANEL_LABEL_STRING, LOCALIZE("Reset"),
		  PANEL_NOTIFY_PROC, reset_props_proc,
		  XV_X, (3 * width) / 4 - (reset_size.width / 2), 
		  XV_HELP_DATA, "props:ResetInfo",
		  NULL);
	window_fit(panel_group[i]);
    }
}

create_panels()
{
    int         i;

    for (i = 0; i < TOTAL_PANELS + color; i++)
	panel_group[i] = xv_create(frame, PANEL,
				   WIN_BORDER, TRUE,
				   WIN_BELOW, flavor_panel,
				   XV_X, 0,
#ifdef OW_I18N
				   WIN_USE_IM, FALSE,
#endif
				   XV_SHOW, FALSE,
				   NULL);

    if (color)
	create_color_panel();

    create_icon_panel();
    create_menu_panel();
    create_misc_panel();
    create_mouse_set_panel();
    create_localization_panel();

    xv_set(panel_group[0], XV_SHOW, TRUE, NULL);
}

/* ARGSUSED */
factory_choice(panel_item, event)
    Panel_item  panel_item;
    Event      *event;
{
    int         factory = !(int) xv_get(panel_item, PANEL_VALUE);

    if (factory == showing_factory)
	return;

    if (factory) {
	if (!saved_defaults[0]) {
	    strcpy(saved_defaults, "/tmp/.XtempXXXXXX");
	    mktemp(saved_defaults);
	} else
	    unlink(saved_defaults);

	apply_props_proc(NULL, NULL);

	defaults_store_db(saved_defaults);

	defaults_init_db();
	defaults_load_db(NULL);
	showing_factory = TRUE;
    } else {
	defaults_load_db(saved_defaults);
	showing_factory = FALSE;
    }
    set_props_values(NULL, NULL);
}


/* ARGSUSED */
show_props_category(panel_item, which_panel, event)
    Panel_item  panel_item;
    int         which_panel;
    Event      *event;
{
    int         height;
    int         width;

    if (panel_group[which_panel] == current_panel)
	return;

    /* Show requested panel */
    xv_set(current_panel, XV_SHOW, FALSE, NULL);
    current_panel = panel_group[which_panel];
#ifndef XV_SHOW_FIXED
    height = (int) xv_get(current_panel, XV_HEIGHT);
    width = (int) xv_get(current_panel, XV_WIDTH);
#endif
    xv_set(current_panel, XV_SHOW, TRUE, NULL);
#ifndef XV_SHOW_FIXED
    xv_set(current_panel,
	   XV_HEIGHT, height,
	   XV_WIDTH, width,
	   NULL);
#endif

    /* Resize frame around new panel */
    width = (int) xv_get(current_panel, XV_WIDTH) + 2 * BORDER_WIDTH;
    xv_set(flavor_panel,
	   XV_WIDTH, width,
	   NULL);
    height = (int) xv_get(flavor_panel, XV_HEIGHT) +
	xv_get(current_panel, XV_HEIGHT) + 2 * BORDER_WIDTH;
    xv_set(frame,
	   XV_HEIGHT, height,
	   XV_WIDTH, width,
	   NULL);
}

Notify_value
frame_unmap_proc(frame, event, arg, type)
    Frame       frame;
    Event      *event;
    Notify_arg  arg;
    Notify_event_type type;
{
    if (event_action(event) == ACTION_CLOSE) {
	exit(0);
    }
    return (notify_next_event_func(frame, event, arg, type));
}

main(argc, argv)
    int         argc;
    char       *argv[];
{
    int		visual_class;
    int		depth;
    char	*openwin_dir;
    char	localepath[MAXPATHLEN];
    Cms		cms;

    xv_init(XV_USE_LOCALE, TRUE,
	    XV_INIT_ARGC_PTR_ARGV, &argc, argv, 
	    NULL);

    if ((openwin_dir = (char *)getenv("OPENWINHOME")) == NULL)
    {
	fprintf(stderr, LOCALIZE("%s: $OPENWINHOME has not been set\n"), argv[0]);
    }
    else
    {
        strcpy(localepath, openwin_dir);
        strcat(localepath, "/lib/locale/");
        bindtextdomain("props", localepath);
    }

    frame = xv_create(NULL, FRAME_CMD,
		      XV_VISUAL_CLASS, PseudoColor,
		      XV_DEPTH, 8,
		      WIN_INHERIT_COLORS, TRUE,
		      XV_X, 0,
		      XV_Y, 0,
		      XV_LABEL, LOCALIZE("Workspace Properties"),
		      FRAME_NO_CONFIRM, TRUE,
		      FRAME_CMD_PUSHPIN_IN, TRUE,
#ifdef OW_I18N
		      WIN_USE_IM, FALSE,
#endif
		      NULL);

    dsp = (Display *) xv_get(frame, XV_DISPLAY);
    visual_class = (int)xv_get(frame, XV_VISUAL_CLASS);
    depth = (int)xv_get(frame, XV_DEPTH);

    /* Note: We should probably handle a 4-bit color visual with a
     *       smaller color palette, but we don't have the time to
     *       do this for V3.  Therefore we are treating a 4 bit visual
     *	     like monochrome, and thus shut off the color palette.
     */
    color = ((depth >= 8) && (visual_class % 2));

    sprintf(user_defaults, "%s/.Xdefaults", getenv("HOME"));

    if ( color ) {
	cms = create_palette();
	xv_set(frame, WIN_CMS, cms, NULL);
    }

    /*
     * Might not get the colors set up right, so don't use FRAME_CMD_PANEL.
     * We must create a new one.
     */
    xv_set((Panel)xv_get(frame, FRAME_CMD_PANEL) , XV_SHOW, FALSE, 0);

    flavor_panel = xv_create(frame, PANEL, 
			     XV_X, 0, 
			     XV_Y, 0, 
#ifdef OW_I18N
			     WIN_USE_IM, FALSE,
#endif 
			     NULL);
    
    notify_interpose_event_func(frame, frame_unmap_proc, NOTIFY_SAFE);

    flavor_choice = xv_create(flavor_panel, PANEL_CHOICE,
			      PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
			      PANEL_LAYOUT, PANEL_HORIZONTAL,
			      PANEL_CHOICE_NROWS, 1,
			      PANEL_LABEL_STRING, LOCALIZE("Category:"),
			      PANEL_NOTIFY_PROC, show_props_category,
			      XV_HELP_DATA, "props:CategoryInfo",
			      NULL);

    if (color)
	xv_set(flavor_choice, PANEL_CHOICE_STRINGS, COLOR_PROPS, NULL);
    else
	xv_set(flavor_choice, PANEL_CHOICE_STRINGS, MONO_PROPS, NULL);

    window_fit_height(flavor_panel);

    create_panels();

    current_panel = panel_group[0];

    set_props_values(NULL, NULL);

    add_buttons();

    xv_set(flavor_panel,
	   XV_WIDTH, xv_get(current_panel, XV_WIDTH) + 2 * BORDER_WIDTH,
	   NULL);

    window_fit(frame);

    xv_set(frame, XV_SHOW, TRUE, NULL);

    xv_main_loop(frame);

    unlink(saved_defaults);
}
