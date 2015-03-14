#ifndef lint
static char *sccsid = "@(#)l10n_props.c 1.6 91/09/14";
#endif

/*
 * l10n_props.c - Localization property sheet for the OpenWindows Workspace 
 *  		  Properties program.
 */

#include <locale.h>
#ifdef OW_I18N
#include <widec.h>
#endif
#include <string.h>
#include <stdio.h>
#include <sys/param.h>
#include <sys/types.h>
#include "props.h"	
#include "l10n_props.h"

#ifdef SYSV
#define bcopy(a,b,c) memmove(b,a,c)
#endif

#ifdef OW_I18N

#ifdef notdef
static wchar_t	*l10n_time_convert();
#endif

/*
 * Wide char constant table (this trick necessary until we can get the
 * ANSI/C compiler.
 */
static wchar_t	_wcs_basic_setting[] =
	{'b', 'a', 's', 'i', 'c', '_', 's', 'e', 't', 't', 'i', 'n', 'g', 0};
static wchar_t	_wcs_input_language[] =
	{'i', 'n', 'p', 'u', 't', '_', 'l', 'a', 'n', 'g', 'u', 'a', 'g', 'e',
	0};
static wchar_t	_wcs_display_language[] =
	{'d', 'i', 's', 'p', 'l', 'a', 'y', '_', 'l', 'a', 'n', 'g', 'u', 'a',
	'g', 'e', 0};
static wchar_t	_wcs_time_format[] =
	{'t', 'i', 'm', 'e', '_', 'f', 'o', 'r', 'm', 'a', 't', 0};
static wchar_t	_wcs_numeric_format[] =
	{'n', 'u', 'm', 'e', 'r', 'i', 'c', '_', 'f', 'o', 'r', 'm', 'a', 't',
	0};

#define	CLASS_NAME_LEN	20
l10n_config_list_t	l10n_config_basic_setting[] = {
#define	BASIC_SETTING		0
{_wcs_basic_setting,	"basicLocale",	D_string, NULL},

{0},
};

l10n_config_list_t	l10n_config_initial_specific_setting[] = {
#define	INPUT_LANGUAGE		0
{_wcs_input_language,	"inputLang", 	D_string, NULL},

#define	DISPLAY_LANGUAGE	1
{_wcs_display_language, "displayLang",	D_string, NULL},

#define	TIME_FORMAT		2
{_wcs_time_format,	"timeFormat",	D_string, NULL},

#define	NUMERIC_FORMAT		3
{_wcs_numeric_format,	"numeric",	D_string, NULL},

#define	SS_CATEGORY		4
{0}
};
#else

#define	CLASS_NAME_LEN	20
l10n_config_list_t	l10n_config_basic_setting[] = {
#define	BASIC_SETTING		0
{"basic_setting",	"basicLocale",	D_string, NULL},

{0},
};

l10n_config_list_t	l10n_config_initial_specific_setting[] = {
#define	INPUT_LANGUAGE		0
{"input_language",	"inputLang", 	D_string, NULL},

#define	DISPLAY_LANGUAGE	1
{"display_language", "displayLang",	D_string, NULL},

#define	TIME_FORMAT		2
{"time_format",	"timeFormat",	D_string, NULL},


#define	NUMERIC_FORMAT		3
{"numeric_format",	"numeric",	D_string, NULL},

#define	SS_CATEGORY		4
{0}
};
#endif OW_I18N

l10n_config_sss_t		*l10n_config_sss = NULL;
static l10n_config_sss_t	*l10n_config_sss_cur = NULL;

l10n_config_sss_t		*sss, *sss_past;
static char			lc_messages[LOCALE_NAME_LEN];


static l10n_config_list_item_t	*l10n_config_slot2ptr();
static int			l10n_ss_setup();
static				l10n_set_choice();

Panel_item	l10n_bs, l10n_ss, l10n_ss_display_language,
		l10n_ss_input_language, l10n_ss_time_format,
		l10n_ss_numeric_format, l10n_ss_il_supplement;


static l10n_config_list_item_t *
l10n_config_slot2ptr(list, slotno)
	l10n_config_list_t	*list;
	register int		slotno;
{
	register l10n_config_list_item_t	*item;


	for (item = list->items; item != NULL; item = item->next)
		if (slotno-- <= 0)
			break;
	if (item == NULL)
		printf("internal error slotno#%d is not there\n", slotno);
	return item;
}

static int
l10n_ss_setup(locale)
	char	*locale;
{

	for (sss = l10n_config_sss; sss != NULL;
	     sss_past = sss, sss = sss->next)
		if (strcmp(sss->locale, locale) == 0)
			break;
	if (sss != NULL)
		goto display;
	sss = (l10n_config_sss_t *) calloc(1, sizeof(l10n_config_sss_t));
	if (l10n_config_sss == NULL)
		l10n_config_sss = sss;
	else
		sss_past->next = sss;
	sss->locale = strdup(locale);
	sss->list = (l10n_config_list_t *)
			malloc(sizeof(l10n_config_initial_specific_setting));
	bcopy(l10n_config_initial_specific_setting, sss->list,
			sizeof(l10n_config_initial_specific_setting));

	/* 
	 * Retrieve Specific Settings from configuration files.
	 */
	strcpy(lc_messages, setlocale(LC_MESSAGES, NULL));
	if (l10n_config_read(lc_messages, locale, sss->list) == -1) {
		fprintf(stderr, (char *)LOCALIZE("Invalid Specific Settings for Category: Localization.\n"));
		return (-1);
	}

display:

	l10n_config_sss_cur = sss;
	return(1);
}

static
l10n_set_choice(pi, list, xrdb_default)
	Panel_item		pi;
	l10n_config_list_t	*list;
	int			xrdb_default;
{
	register l10n_config_list_item_t	*item;
	register int		count;
	char			*texts[MAX_CHOICES];
	int			i;


	for (count = 0, item = list->items; item != NULL; item = item->next)
	{
		if (count >= MAX_CHOICES)
		{
			fprintf(stderr, (char *)LOCALIZE("Maximum # of choices in item is [%d] in Category: Localization\n"), MAX_CHOICES);
			break;
		}

#ifdef OW_I18N
		/* Convert mbs to wchar */
		if (item->mbs_label == NULL)
		{
			item->mbs_label = malloc(wslen(item->label)
						* sizeof(wchar_t) + 1);
			wstostr(item->mbs_label, item->label);
		}
#else
		if (item->mbs_label == NULL)
		{
			item->mbs_label = malloc(strlen(item->label)
						* sizeof(char) + 1);
			strcpy(item->mbs_label, item->label);
		}

#endif 
	
		texts[count] = item->mbs_label;
		count++;
	}

	xv_set(pi, PANEL_CHOICE_STRINGS, LOCALIZE("Choice"), 0, 0);
	for (i=0; i< count; i++)
		xv_set(pi, PANEL_CHOICE_STRING, i, (char *)texts[i], NULL);
	
	if (xrdb_default)
		xv_set(pi,
	       		PANEL_VALUE,		list->current_value,
	       		PANEL_DEFAULT_VALUE,	list->default_value,
	       		NULL);
	else
		xv_set(pi,
	       		PANEL_VALUE,		list->default_value,
	       		PANEL_DEFAULT_VALUE,	list->default_value,
	       		NULL);
		
}

reset_localization()
{

	l10n_config_list_item_t	*l_item = NULL;
	Panel_item		msg_item = 0;
    	Description 		*id;

	if (!(id = (Description *)xv_get(l10n_bs, PANEL_CLIENT_DATA))) {
		printf("Bad client data for Basic Setting\n");
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

        l_item = l10n_config_slot2ptr(
                        &l10n_config_basic_setting[BASIC_SETTING], 
			xv_get(l10n_bs, PANEL_VALUE));
        if (l_item != NULL)
		l10n_ss_setup(l_item->value);

	/*
	 *Reset Specific Settings based on Basic Setting 
	 */

	l10n_set_choice(l10n_ss_input_language,
			&sss->list[INPUT_LANGUAGE], TRUE);
	l10n_set_client_data(l10n_ss_input_language,
			 &sss->list[INPUT_LANGUAGE], msg_item,
			(Description *)xv_get(l10n_ss_input_language,
			 PANEL_CLIENT_DATA));

	l10n_set_choice(l10n_ss_display_language,
			&sss->list[DISPLAY_LANGUAGE], TRUE);
	l10n_set_client_data(l10n_ss_display_language,
			&sss->list[DISPLAY_LANGUAGE], msg_item,
			(Description *)xv_get(l10n_ss_display_language,
			PANEL_CLIENT_DATA));

	l10n_set_choice(l10n_ss_time_format,
			&sss->list[TIME_FORMAT], TRUE);
	l10n_set_client_data(l10n_ss_time_format,
			&sss->list[TIME_FORMAT], msg_item,
			(Description *)xv_get(l10n_ss_time_format,
			PANEL_CLIENT_DATA));

	l10n_set_choice(l10n_ss_numeric_format,
			&sss->list[NUMERIC_FORMAT], TRUE);
	l10n_set_client_data(l10n_ss_numeric_format,
			&sss->list[NUMERIC_FORMAT], msg_item,
			(Description *)xv_get(l10n_ss_numeric_format,
			PANEL_CLIENT_DATA));

}

/*
 * Notify callback function for `l10n_bs'.
 */
void
l10n_bs_notify(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{	
	l10n_config_list_item_t	*l_item;
	Panel_item		msg_item = 0;

	l10n_config_basic_setting[BASIC_SETTING].current_value = value;
        l_item = l10n_config_slot2ptr(
                        &l10n_config_basic_setting[BASIC_SETTING], value);
        l10n_ss_setup(l_item->value);

	/*
	 *Reset Specific Settings based on Basic Setting 
	 */

	l10n_set_choice(l10n_ss_input_language,
			&sss->list[INPUT_LANGUAGE], FALSE);
	l10n_set_client_data(l10n_ss_input_language,
			 &sss->list[INPUT_LANGUAGE], msg_item,
			(Description *)xv_get(l10n_ss_input_language,
			 PANEL_CLIENT_DATA));

	l10n_set_choice(l10n_ss_display_language,
			&sss->list[DISPLAY_LANGUAGE], FALSE);
	l10n_set_client_data(l10n_ss_display_language,
			&sss->list[DISPLAY_LANGUAGE], msg_item,
			(Description *)xv_get(l10n_ss_display_language,
			PANEL_CLIENT_DATA));

	l10n_set_choice(l10n_ss_time_format,
			&sss->list[TIME_FORMAT], FALSE);
	l10n_set_client_data(l10n_ss_time_format,
			&sss->list[TIME_FORMAT], msg_item,
			(Description *)xv_get(l10n_ss_time_format,
			PANEL_CLIENT_DATA));

	l10n_set_choice(l10n_ss_numeric_format,
			&sss->list[NUMERIC_FORMAT], FALSE);
	l10n_set_client_data(l10n_ss_numeric_format,
			&sss->list[NUMERIC_FORMAT], msg_item,
			(Description *)xv_get(l10n_ss_numeric_format,
			PANEL_CLIENT_DATA));

	add_change_bar(item, event);

}


/*
 * Notify callback function for `l10n_ss_il_supplement'.
 */
void
l10n_ss_il_supplement_notify(item, event)
   	Panel_item	item;
   	Event		*event;
{
	

}


l10n_set_client_data(p_item, l10n_list, msg_item, client_data)
   	Panel_item		p_item;
   	l10n_config_list_t	*l10n_list;
   	Panel_item		msg_item;
   	Description		*client_data;
{

	register l10n_config_list_item_t	*item;
	register int	slotno;

	char	client_choices[256]; /* LOCALE_NAME_LEN*MAX_CHOICES + MAX_CHOICES */
	char	choice[LOCALE_NAME_LEN];  	     /* 20  in l1on_props.h */
	char	resource_name[DEFAULTS_MAX_VALUE_SIZE]; /* 128 in xview/defaults.h */


	for (slotno = 0, item = l10n_list->items;  item != NULL;
		     slotno++, item = item->next)
	{
		if (slotno) {
			sprintf(choice, ":%s", item->value);
			strcat(client_choices, choice);
		} else
			sprintf(client_choices, "%s", item->value);

		
	}

	sprintf(resource_name, "*%s", l10n_list->class);

	if (client_data == NULL) {
		xv_set(p_item, PANEL_CLIENT_DATA,
		allocate_desc_struct(resource_name,
			l10n_list->class,
			l10n_list->class_type,
			l10n_list->items->value,
			client_choices, p_item, msg_item),
			NULL);
	} else {
		client_data->misc = strdup(client_choices);
	}



}

void
create_localization_panel()
{

	Panel 		l10n_area;
	Panel_item	msg_item[5];
	char		locale[LOCALE_NAME_LEN];
	int		i;

	register l10n_config_list_t	*bs;
	l10n_config_list_item_t         *l_item = NULL;


	l10n_area = panel_group[LOCALIZATION_PANEL + color];

        xv_set(l10n_area,
	   XV_HELP_DATA, "props:LocalizationPanelInfo",
	   NULL);


	/* 
	 * Create Basic Setting 
	 */

	msg_item[0] = xv_create(l10n_area, 
		PANEL_MESSAGE, 
		PANEL_LABEL_STRING, LOCALIZE(" "),
		NULL);

        l10n_bs = xv_create(l10n_area, PANEL_CHOICE, 
		PANEL_LABEL_STRING, LOCALIZE("Basic Setting:"),
		PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_NROWS, 1,
		PANEL_NOTIFY_PROC, l10n_bs_notify,
		PANEL_CHOICE_STRING, 0, LOCALIZE("Choice"),
		XV_HELP_DATA,	"props:BasicSetting",
		NULL);

	bs = l10n_config_basic_setting;
	strcpy(locale, setlocale(LC_MESSAGES, NULL));

	/* 
	 * Retrieve basic setting from l10n configuration file.
	 */
	if (l10n_config_read(locale, "basic_setting", bs) == -1) {

		fprintf(stderr, (char *)LOCALIZE("Invalid Basic Setting for Category: Localization.\n"));
		xv_set(l10n_bs, PANEL_INACTIVE, TRUE, 0);

	} else {
		l10n_set_choice(l10n_bs, &bs[BASIC_SETTING], TRUE);
		l10n_set_client_data(l10n_bs, bs, msg_item[0], NULL);
		l_item = l10n_config_slot2ptr(&l10n_config_basic_setting[BASIC_SETTING], 
			(int)xv_get(l10n_bs, PANEL_VALUE));

	}


	/* 
	 * Create Specific Settings 
	 */

	/* 
	 * Create Specific Setting item 
	 */
	l10n_ss = xv_create(l10n_area, PANEL_MESSAGE,
		PANEL_LABEL_STRING, LOCALIZE("Specific Setting:"),
		PANEL_LABEL_BOLD, TRUE,
		PANEL_NEXT_ROW, -1,
		XV_HELP_DATA,	"props:SpecificSetting",
		NULL);

    	{ int a,b;

          a = xv_get(l10n_bs, PANEL_LABEL_WIDTH);
          b = xv_get(l10n_ss, PANEL_LABEL_WIDTH);

          i = MAX(a,b) + xv_get(msg_item[0], PANEL_LABEL_WIDTH) + DEFAULT_X_GAP;
        }
        xv_set(l10n_bs, PANEL_VALUE_X, i, NULL);
        xv_set(l10n_ss, PANEL_VALUE_X, i, NULL);

	/* 
	 * Create display language item 
	 */
	msg_item[1] = xv_create(l10n_area, 
		PANEL_MESSAGE, 
		PANEL_LABEL_STRING, LOCALIZE(" "),
		PANEL_NEXT_ROW, -1,
		NULL);

	l10n_ss_display_language = xv_create(l10n_area, PANEL_CHOICE, 
		PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LABEL_STRING, LOCALIZE("Display Language:"),
		PANEL_NOTIFY_PROC, add_change_bar,
		PANEL_CHOICE_STRING, 0, LOCALIZE("Choice"),
		XV_HELP_DATA, "props:DisplayLanguage",
		NULL);

	/* 
	 * Create input language item 
	 */
	msg_item[2] = xv_create(l10n_area, 
		PANEL_MESSAGE, 
		PANEL_LABEL_STRING, LOCALIZE(" "),
		PANEL_NEXT_ROW, -1,
		NULL);

	l10n_ss_input_language = xv_create(l10n_area, PANEL_CHOICE, 
	        PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
		PANEL_LABEL_STRING, LOCALIZE("Input Language:"),
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_NROWS, 1,
		PANEL_NOTIFY_PROC, add_change_bar,
		PANEL_CHOICE_STRING, 0, LOCALIZE("Choice"),
		XV_HELP_DATA, "props:InputLanguage",
		NULL);

	/* 
	 * Create supplementary item 
	 */
	l10n_ss_il_supplement = xv_create(l10n_area, PANEL_BUTTON,
		XV_X, 340,
		PANEL_LABEL_STRING, LOCALIZE("Supplementary..."),
		/*PANEL_NOTIFY_PROC, l10n_ss_il_supplement_notify,*/
		XV_HELP_DATA, "props:SupplementarySetting",
		NULL);

	 /* "Supplement" is not supported yet.  */
        xv_set(l10n_ss_il_supplement,
                PANEL_INACTIVE,          TRUE,
                NULL);

	/* 
	 * Create time format item 
	 */
	msg_item[3] = xv_create(l10n_area, 
		PANEL_MESSAGE, 
		PANEL_LABEL_STRING, LOCALIZE(" "),
		PANEL_NEXT_ROW, -1,
		NULL);

	l10n_ss_time_format = xv_create(l10n_area, PANEL_CHOICE, 
		PANEL_LABEL_STRING, LOCALIZE("Time Format:"),
		PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_NROWS, 1,
		PANEL_NOTIFY_PROC, add_change_bar,
		PANEL_CHOICE_STRING, 0, LOCALIZE("Choice"),
		XV_HELP_DATA, "props:TimeFormat",
		NULL);

	/* 
	 * Create numeric format item 
	 */
	msg_item[4] = xv_create(l10n_area, 
		PANEL_MESSAGE, 
		PANEL_LABEL_STRING, LOCALIZE(" "),
		PANEL_NEXT_ROW, -1,
		NULL);

	l10n_ss_numeric_format = xv_create(l10n_area, PANEL_CHOICE, 
		PANEL_LABEL_STRING, LOCALIZE("Numeric Format:"),
		PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_NROWS, 1,
		PANEL_NOTIFY_PROC, add_change_bar,
		PANEL_CHOICE_STRING, 0, LOCALIZE("Choice"),
		XV_HELP_DATA, "props:NumericFormat",
		NULL);


	/* 
	 * Retrieve Specific Settings from configuration file.
	 */

	if (l_item == NULL || l10n_ss_setup(l_item->value) == -1)
	{

		fprintf(stderr, (char *)LOCALIZE("Invalid Specific Setting for Category: Localization.\n"));
		xv_set(l10n_ss, PANEL_INACTIVE, TRUE, 0);
		xv_set(l10n_ss_display_language, PANEL_INACTIVE, TRUE, 0);
		xv_set(l10n_ss_input_language, PANEL_INACTIVE, TRUE, 0);
		xv_set(l10n_ss_time_format, PANEL_INACTIVE, TRUE, 0);
		xv_set(l10n_ss_numeric_format, PANEL_INACTIVE, TRUE, 0);

	} else {

		l10n_set_choice(l10n_ss_display_language,
			&sss->list[DISPLAY_LANGUAGE], TRUE);
		l10n_set_client_data(l10n_ss_display_language,
			&sss->list[DISPLAY_LANGUAGE], msg_item[1], NULL);

		l10n_set_choice(l10n_ss_input_language,
			&sss->list[INPUT_LANGUAGE], TRUE);
		l10n_set_client_data(l10n_ss_input_language,
			 &sss->list[INPUT_LANGUAGE], msg_item[2], NULL);

		l10n_set_choice(l10n_ss_time_format,
			&sss->list[TIME_FORMAT], TRUE);
		l10n_set_client_data(l10n_ss_time_format,
			 &sss->list[TIME_FORMAT], msg_item[3], NULL);

		l10n_set_choice(l10n_ss_numeric_format,
			&sss->list[NUMERIC_FORMAT], TRUE);
		l10n_set_client_data(l10n_ss_numeric_format,
			 &sss->list[NUMERIC_FORMAT], msg_item[4], NULL);

	}

    	{ int a,b,c,d;

          a = xv_get(l10n_ss_display_language, PANEL_LABEL_WIDTH);
          b = xv_get(l10n_ss_input_language,   PANEL_LABEL_WIDTH);
          c = xv_get(l10n_ss_time_format,      PANEL_LABEL_WIDTH);
          d = xv_get(l10n_ss_numeric_format,   PANEL_LABEL_WIDTH);

          i = MAX(a,b) + xv_get(msg_item[0], PANEL_LABEL_WIDTH) + (3 * DEFAULT_X_GAP);
        }
        xv_set(l10n_ss_display_language, PANEL_VALUE_X, i, NULL);
        xv_set(l10n_ss_input_language,   PANEL_VALUE_X, i, NULL);
        xv_set(l10n_ss_time_format,      PANEL_VALUE_X, i, NULL);
        xv_set(l10n_ss_numeric_format,   PANEL_VALUE_X, i, NULL);
	xv_set(l10n_ss_il_supplement,	 XV_X, i+175, NULL);

    	window_fit_width(l10n_area);
}
