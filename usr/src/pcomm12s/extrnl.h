/*
 * The external file transfer program database.  The list is limited to
 * 3 uploads and 3 downloads because the xfer_menu() routine uses single
 * character input (and these selections become 7, 8, and 9).
 */

struct EXTRNL {
	char	*name[2][3];		/* program name (for display only) */
	char	*command[2][3];		/* the command line */
	char	prompt[2][3];		/* need to prompt for names? */

	int	up_entries;		/* number of up entries in the file */
	int	dn_entries;		/* number of down entries in the file */

	char	*e_path;		/* path to the pcomm.extrnl file */
};

#ifndef MAIN
extern struct EXTRNL *extrnl;
#endif /* MAIN */
