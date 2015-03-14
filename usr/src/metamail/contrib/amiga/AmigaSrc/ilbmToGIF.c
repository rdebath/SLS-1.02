/* Create a GIF image from an IFF ILBM file.
 * Ask the user to name an ILBM. We then call programs that do the actual
 * conversion. The GIF image will be given the name provided as the first
 * (and only) argument.
 *
 * Usage:
 *  ilbmToGIF temp_file_name
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

    printf("Enter the name of an ILBM IFF file: ");
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

/*    sprintf(command, "ilbmtoppm %s | ppmtogif >%s", lineBuf, argv[1]); */
    sprintf(command, "fbcat -G <%s >%s", lineBuf, argv[1]);
    exit(system(command));
}
