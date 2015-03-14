#ifndef lint
static char *sccsid = "@(#)l10n_read.c 1.4 91/09/14";
#endif

/*
 * l10n_read.c - Reader for l10n configuration file(s)
 */

#include	<sys/types.h>
#include	<sys/param.h>
#include	<locale.h>
#ifdef OW_I18N
#include	<widec.h>
#endif 
#include	<stdio.h>
#include	<string.h>
#include	"props.h"
#include	"l10n_props.h"


#define	MAX_LINE_LENGTH		256

#define	NAME_SEPARATOR		'='
#define	FIELD_SEPARATOR		'|'
#define	ITEM_SEPARATOR		';'


extern char	*malloc();
extern char	*calloc();


/*
 * FIX_ME! This routine must be sophisticated enough to allow more
 * flexible syntax in file.  Such as allow the white space between
 * each token.
 */
int
l10n_config_read(locale, file_name, a_list)
	char			*locale;
	char			*file_name;
	l10n_config_list_t	*a_list;
{
#ifdef OW_I18N
	register wchar_t	*p1, *p2;
#else
	register char		*p1, *p2;
#endif

	register l10n_config_list_t	*list;
	register l10n_config_list_item_t *item;
	l10n_config_list_item_t	*item_new;

	register FILE		*config_file;
	int			i;
#ifdef OW_I18N
	wchar_t			*dft;
#else
	char			*dft;
#endif
	char			*mbs;
	int			lineno;
	int			slotno;
	char			fullpath[MAXPATHLEN];
#ifdef OW_I18N
	wchar_t			line[MAX_LINE_LENGTH+1];
#else
	char			line[MAX_LINE_LENGTH+1];
#endif
	int			rcode = -1;


	/*  
	 * Find path for localization configuration files under
	 * $OPENWINHOME/share/locale/<locale>/props
	 */
	sprintf(fullpath, "%s/share/locale/%s/props/%s",
		getenv("OPENWINHOME"), locale, file_name);

	if ((config_file = fopen(fullpath, "r")) == NULL)
	{
		perror(fullpath);
		goto ret;
	}

	/* 
	 * Parse configuration file. 
	 */
#ifdef OW_I18N
	for (lineno = 1; fgetws(line, MAX_LINE_LENGTH, config_file) != NULL; lineno++)
#else
	for (lineno = 1; fgets(line, MAX_LINE_LENGTH, config_file) != NULL; lineno++)
#endif
	{
		p1 = line;
		if (*p1 == '#' || *p1 == '\n')
			continue;

		/*
		 * Pickup the category name.
		 */
#ifdef OW_I18N
		if ((p2 = wschr(p1, NAME_SEPARATOR)) == NULL)
#else
		if ((p2 = strchr(p1, NAME_SEPARATOR)) == NULL)
#endif
		{
			fprintf(stderr, (char *)LOCALIZE("Bad format in %s (line#%d): missing name separator '%c')\n"),
					fullpath, lineno, NAME_SEPARATOR);
			goto ret;
		}

		/*
		 * Looking for the category name in list...
		 */
		*p2 = 0;	/* Overwrite '=' with string terminator */
		for (list = a_list; list->name != NULL; list++)
#ifdef OW_I18N
			if (wscmp(list->name, p1) == 0)
#else
			if (strcmp(list->name, p1) == 0)
#endif 
				break;
		if (list->name == NULL)
		{
#ifdef OW_I18N
			fprintf(stderr, (char *)LOCALIZE("Bad format in %s (line#%d): Unknown category name [%ws]\n"),
					fullpath, lineno, p1);
#else
			fprintf(stderr, (char *)LOCALIZE("Bad format in %s (line#%d): Unknown category name [%s]\n"),
					fullpath, lineno, p1);
#endif
			goto ret;
		}
		p1 = ++p2;

		/*
		 * Picking up the default value.
		 */
#ifdef OW_I18N
		if ((p2 = wschr(p1, ITEM_SEPARATOR)) == NULL)
#else
		if ((p2 = strchr(p1, ITEM_SEPARATOR)) == NULL)
#endif
		{
			fprintf(stderr, (char *)LOCALIZE("Bad format in %s (line#%d): no default value\n"),
					fullpath, lineno);
			goto ret;
		}
		*p2++ = 0;
		dft = p1;	/* Keep it for now */
		p1 = p2;


		/*
		 * Picking up the items.
		 */
		item = NULL;
		for (slotno = 0; *p1 != '\n' && *p1 != 0; slotno++)
		{
			/*
			 * Allocate new space.
			 */
			item_new = (l10n_config_list_item_t *)
				calloc(1, sizeof (l10n_config_list_item_t));
			if (item == NULL)
				list->items = item_new;
			else
				item->next = item_new;
			item = item_new;

			/*
			 * Picking up the "value".
			 */
#ifdef OW_I18N
			if ((p2 = wschr(p1, FIELD_SEPARATOR)) == NULL)
#else
			if ((p2 = strchr(p1, FIELD_SEPARATOR)) == NULL)
#endif
			{
				fprintf(stderr, (char *)LOCALIZE("Bad format in %s (line#%d): missing field separator '%c'\n"),
					fullpath, lineno, FIELD_SEPARATOR);
				goto ret;
			}
			*p2++ = 0;
#ifdef OW_I18N
			item->value = malloc(wslen(p1) * sizeof(wchar_t) + 1);
			wstostr(item->value, p1);
#else
			item->value = malloc(strlen(p1) * sizeof(char) + 1);
			strcpy(item->value, p1);
#endif
#ifdef OW_I18N
			if (dft != NULL && wscmp(p1, dft) == 0)
#else
			if (dft != NULL && strcmp(p1, dft) == 0)
#endif
			{
				list->default_value = slotno;
				dft = NULL;
			}
			p1 = p2;


			/*
			 * Picking up the label.
			 */
#ifdef OW_I18N
			if ((p2 = wschr(p1, ITEM_SEPARATOR)) != NULL
			 || (p2 = wschr(p1, '\n')) != NULL)
#else
			if ((p2 = strchr(p1, ITEM_SEPARATOR)) != NULL
			 || (p2 = strchr(p1, '\n')) != NULL)
#endif
			{
				if (*p2 != ITEM_SEPARATOR)
					slotno = -1;
				*p2++ = 0;
			}
#ifdef OW_I18N
			if (list->convert_label != NULL)
				item->label = (*(list->convert_label))
							(item->value, p1);
			else
				item->label = wsdup(p1);
#else
			item->label = strdup(p1);

#endif
			if (slotno < 0)
				break;
			p1 = p2;
		}
		if (dft != NULL)
		{
#ifdef OW_I18N
			fprintf(stderr, LOCALIZE("Invalid default value in %s (line#%d): [%ws]\n"),
				fullpath, lineno, dft);
#else
			fprintf(stderr, (char *)LOCALIZE("Invalid default value in %s (line#%d): [%s]\n"),
				fullpath, lineno, dft);

#endif
			goto ret;
		}
		if (defaults_exists(list->class, list->class) == TRUE)
		{
			mbs = defaults_get_string(list->class, list->class, "");
			for (i = 0, item = list->items;
			     item != NULL;
			     i++, item = item->next)
				if (strcmp(item->value, mbs) == 0)
				{
					list->current_value = i;
					break;
				}
			if (item == NULL)
			{
#ifdef notdef
				/*
				 * This could happen all the time, if
				 * you switch the "Basic Setting" from
				 * English to Japanese.  So, I'm
				 * decided do it silently.
				 */
				fprintf(stderr, LOCALIZE("Bad configuration: %s should not be %s for basic setting %s\n"),
						list->class, mbs, file_name);
#endif
				list->current_value = list->default_value;
			}
		}
		else
			list->current_value = list->default_value;
		list->initial_value = list->current_value;
	}
	rcode = 0;

ret:
	(void) fclose(config_file);

	return rcode;
}
