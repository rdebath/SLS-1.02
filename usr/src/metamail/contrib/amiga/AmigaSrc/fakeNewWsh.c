/* The WShell can be invoked with a command as an argument, thus:
    newwsh CMD ls -l
   newcli and newshell require a commandfile, thus:
    newcli FROM a_file
   This program accepts a command as arguments to the program. It squirrels
   the commands away in a file and invokes newcli on this file.

   The metamail program has a call to newwsh hardwired in. You can get metamail
   to use fakeNewWsh instead by setting the environment variable TERMINAL_CMD
    setenv TERMINAL_CMD "fakeNewWsh CMD "
   (the trailing space is important).

   Usage:
    fakeNewWsh [options_to_newcli] CMD command_to_newcli
 */

#include <stdio.h>
#include <string.h>

main(argc, argv)
int argc;
char **argv;
{
    char command[300], fromName[15], deleteCommand[30];
    int i;
    FILE *fromFile;

    strcpy(command, "newcli ");
    for (i = 1; i < argc; i++) {
        if (stricmp(argv[i], "CMD") == 0) {
            break;
        }
        strcat(command, argv[i]);
        strcat(command, " ");
    }
    if (i < argc) {
        i++;
        strcpy(fromName, "T:mmXXXXXX");
        mktemp(fromName);
        strcat(command, "FROM ");
        strcat(command, fromName);
        if ((fromFile = fopen(fromName, "w")) == NULL) {
            fprintf(stderr, "%s: Unable to open %s for writing\n", argv[0],
                    fromName);
            exit(1);
        }
        for ( ; i < argc; i++) {
            fprintf(fromFile, "%s ", argv[i]);
        }
        fprintf(fromFile, "\n");
        fclose(fromFile);
    }

    system(command);
    sprintf(deleteCommand, "Delete %s", fromName);
    system(deleteCommand);
    exit(0);
}
