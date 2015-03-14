/* usershell.c - emulate BSD getusershell() - rick sladkey */

#include <stdio.h>

static FILE *shell_fp = NULL;

char *getusershell()
{
	static char buf[256];

	if (!shell_fp && (shell_fp = fopen("/etc/shells", "r")) == NULL)
		return NULL;
	if (!fgets(buf, 256, shell_fp))
		return NULL;
	buf[strlen(buf) - 1] = '\0'; /* chop */
	return buf;
}

void endusershell()
{
	if (shell_fp) {
		fclose(shell_fp);
		shell_fp = NULL;
	}
}

