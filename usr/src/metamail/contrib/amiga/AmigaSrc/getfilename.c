/* Ask the user to name a file in a given format.
 * The getfilename program will ask the user for the name of a
 * file in the specified format, and will copy that file to the
 * file-name given as the second argument.
 *
 * Usage:
 *  getfilename format_name temp_file_name
 */

#include <stdio.h>
#include <string.h>

main(argc, argv)
int argc;
char **argv;
{
    char lineBuf[200];
    char command[250];
    int i;

    if (argc != 3) {
        fprintf(stderr, "Usage: %s format_name temp_file_name\n", argv[0]);
        exit(10);
    }

    printf("Enter the name of a file in '%s' format: ", argv[1]);
    fgets(lineBuf, sizeof(lineBuf), stdin);
    i = strlen(lineBuf);
    if (i > 0 && lineBuf[i - 1] == '\n') {
        lineBuf[i - 1] = 0;
        i--;
    }
    if (i == 0 || access(lineBuf, 4)) {
        fprintf(stderr, "%s: Unable to access file '%s'\n", argv[0], lineBuf);
        exit(10);
    }

    sprintf(command, "Copy %s %s", lineBuf, argv[2]);
    exit(system(command));
}
