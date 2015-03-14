/* Create an audio file in ulaw format from a 8SVX IFF file.
 * Ask the user to name an 8SVX IFF. We then call programs that do the actual
 * conversion. The ulaw audio file will be given the name provided as the first
 * (and only) argument.
 *
 * Usage:
 *  svxToUlaw temp_file_name
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

    if (argc != 2) {
        fprintf(stderr, "Usage: %s temp_file_name\n", argv[0]);
        exit(10);
    }

    printf("Enter the name of an audio file in 8SVX IFF format: ");
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

    sprintf(command, "as2ss %s %s", lineBuf, argv[1]);
    exit(system(command));
}
