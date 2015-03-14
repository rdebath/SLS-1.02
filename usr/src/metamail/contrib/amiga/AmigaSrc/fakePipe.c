/* Some programs do not take piped input, they require a file as an argument.
 * This command is a simpleminded workaround for this.
 * It pipes standard input to a temporary file, then invokes a command on the
 * file. The temporary file is then removed.
 *
 * Usage:
 *  fakePipe command [options_to_command]
 */

#include <stdio.h>
#include <string.h>

main(argc, argv)
int argc;
char **argv;
{
    char command[300], pipeName[15], deleteCommand[30];
    char buf[BUFSIZ];
    int i;
    int bytesRead;
    FILE *pipeFile;

    if (argc < 2) {
        fprintf(stderr, "Usage: %s command [options_to_command]\n", argv[0]);
        exit(1);
    }

    strcpy(pipeName, "T:fpXXXXXX");
    mktemp(pipeName);
    if ((pipeFile = fopen(pipeName, "w")) == NULL) {
        fprintf(stderr, "%s: Unable to open %s for writing\n", argv[0],
                pipeName);
        exit(1);
    }

    while ((bytesRead = fread(buf, 1, BUFSIZ, stdin)) > 0) {
        fwrite(buf, 1, bytesRead, pipeFile);
    }
    fclose(pipeFile);

    sprintf(command, "%s %s", argv[1], pipeName);

    for (i = 2; i < argc; i++) {
        strcat(command, " ");
        strcat(command, argv[i]);
    }

    system(command);
    sprintf(deleteCommand, "Delete %s", pipeName);
    system(deleteCommand);
    exit(0);
}
