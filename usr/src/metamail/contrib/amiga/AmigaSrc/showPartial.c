/* The MIME standard has a message type called "message/partial". This is
   used to send a large message split up in several smaller messages. On
   the receiving end we are now faced with the task of reassembling the
   original large message.

   We do this by collecting the partial messages in the directory
        METAMAIL_P_DIR/UserName
   where METAMAIL_P_DIR is an environment variable, and "UserName" is retrieved
   from the UUCP Config.

   Usage:
    showPartial file id part_num [total_num]
    total_num is required for the last partial message.
 */

#include <stdio.h>
#include <string.h>
#include <sys/stat.h>

#define Prototype   extern

#include <getfiles.h>
#include <time.h>
#include <lib_protos.h>

#define DEFAULT_P_DIR   "T:message-parts"
#define DEFAULT_TOTAL   25

void createDirIfNotExists(char *dirName);
void safeFileName(const unsigned char *unsafeName, unsigned char *safeName);

char *myName;

main(argc, argv)
int argc;
char **argv;
{
    char partialDir[200], buf[200];
    unsigned char safeId[100];
    char command[100];
    char *cp;
    int i;
    struct stat stat_buf;
    unsigned char *fileName, *id;
    int partNum, totalNum;
    int numFound;
    int limit;
    FILE *countFile;

    myName = argv[0];

    if (argc < 4 && argc > 5) {
        fprintf(stderr, "Usage: %s file id part_num [total_num]\n", myName);
        exit(10);
    }
    fileName = argv[1];
    id = argv[2];
    safeFileName(id, safeId);
    partNum = atoi(argv[3]);
    totalNum = -1;
    if (argc == 5 && argv[4][0] != 0) {
        totalNum = atoi(argv[4]);
    }

    strcpy(partialDir, DEFAULT_P_DIR);
    if ((cp = getenv("METAMAIL_P_DIR")) != NULL) {
        strcpy(partialDir, cp);
    }
    createDirIfNotExists(partialDir);

    if ((cp = FindConfig("UserName")) == NULL) {
        fprintf(stderr, "%s: Unable to find UserName in UUCP configuration\n",
                myName);
        exit(10);
    }
    i = strlen(partialDir);
    if (i > 0 && partialDir[i - 1] != ':') {
        strcat(partialDir, "/");
    }
    strcat(partialDir, cp);
    createDirIfNotExists(partialDir);

    strcat(partialDir, "/");
    strcat(partialDir, safeId);
    createDirIfNotExists(partialDir);

    sprintf(command, "copy %s %s/%d", fileName, partialDir, partNum);
    if (system(command) != 0) {
        fprintf(stderr, "%s: %s failed\n", myName, command);
        exit(10);
    }

    sprintf(buf, "%s/CT", partialDir);
    if (totalNum == -1) {
        if ((countFile = fopen(buf, "r")) != NULL) {
            fscanf(countFile, "%d", &totalNum);
            fclose(countFile);
        }
    } else {
        if ((countFile = fopen(buf, "w")) != NULL) {
            fprintf(countFile, "%d\n", totalNum);
            fclose(countFile);
        }
    }

    /* Check if we have received all parts. */
    if (chdir(partialDir) != 0) {
        fprintf(stderr, "%s: Unable to chdir to %s\n", myName, partialDir);
        exit(10);
    }

    limit = DEFAULT_TOTAL;
    if (totalNum != -1) {
        limit = totalNum;
    }
    numFound = 0;
    for (i = 1; i <= limit; i++) {
        sprintf(buf, "%d", i);
        if (stat(buf, &stat_buf) == 0) {
            numFound++;
        }
    }

    if (totalNum != -1) {
        if (numFound == totalNum) {
            /* We will now concatenate the parts into the complete message.
             * We do not remove the parts until we have generated the complete
             * message. This uses up more space, but it should simplify things
             * for the user in case of a crash, a full file system or whatever.
             */
            unlink("FULL");
            for (i = 1; i <= totalNum; i++) {
                sprintf(command, "Type >>FULL %d", i);
                if(system(command) != 0) {
                    fprintf(stderr, "%s: %s failed in directory %s\n", myName,
                            command, partialDir);
                    exit(10);
                }
            }
            for (i = 1; i <= totalNum; i++) {
                sprintf(buf, "%d", i);
                unlink(buf);
            }
            printf("All parts of this %d-part message have now been read.\n",
                   totalNum);
            system("metamail <* -d FULL");
            printf("WARNING:  To save space, the full file is now being deleted.\n");
            printf("You will have to read all %d parts again to see the full message again.\n",
                   totalNum);
            unlink("FULL");
            unlink("CT");
            chdir("/");
            unlink(safeId);
        } else {
            printf("So far you have only read %d of the %d parts of this message.\n",
                   numFound, totalNum);
            printf("When you have read them all, then you will see the message in full.\n");
        }
    } else {
        printf("So far you have only read %d of the several parts of this message.\n",
               numFound);
        printf("When you have read them all, then you will see the message in full.\n");
    }
    exit(0);
}

void
createDirIfNotExists(char *dirName)
{
    struct stat stat_buf;

    if (stat(dirName, &stat_buf) != 0) {
        if (mkdir(dirName) != 0) {
            fprintf(stderr, "%s: Unable to create directory %s.\n", myName,
                    dirName);
            exit(10);
        }
    } else {
        if (!(stat_buf.st_mode & S_IFDIR)) {
            fprintf(stderr, "%s: %s is not a directory.\n", myName,
                    dirName);
            exit(10);
        }
    }
}

/*
 * Change all characters that might have special meaning to the shell
 * or to the filesystem.
 */
void
safeFileName(const unsigned char *unsafeName, unsigned char *safeName)
{
    static char dangerousChar[] = ":!$&*()|'\";/<>\\";

    while (*unsafeName) {
        if (strchr(dangerousChar, *unsafeName) != NULL) {
            *safeName++ = *unsafeName++ + 128;
        } else {
            *safeName++ = *unsafeName++;
        }
    }
    *safeName = 0;
}

