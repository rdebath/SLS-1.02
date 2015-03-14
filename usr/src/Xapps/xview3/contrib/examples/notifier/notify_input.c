/*
 * notify_input.c -- use notify_set_input_func to monitor the state of
 * a file.  The notifier is running and checking the file descriptors
 * of the opened files associated with the command line args.  The
 * routine installed by notify_set_input_func() is called whenever
 * there is data to be read.  When there is no more data to be read
 * for that file, the input function is unregistered.  When all files
 * have been read, notify_start() returns and the program exits.
 */
#include <stdio.h>
#include <sys/ioctl.h>
#include <xview/notify.h>

main(argc, argv)
char *argv[];
{
    Notify_value   read_it();
    Notify_client  client = (Notify_client)10101; /* arbitrary */
    FILE           *fp;

    while (*++argv)
        if (!(fp = fopen(*argv, "r")))
            perror(*argv);
        else {
            (void) notify_set_input_func(client, read_it, fileno(fp));
            client++; /* next client is new/unique */
        }

    /* loops continuously */
    notify_start();
}

/*
 * read_it() is called whenever there is input to be read.  Actually,
 * it's called continuously, so check to see if there is input to be
 * read first.
 */
Notify_value
read_it(client, fd)
Notify_client   client;
int fd;
{
    char buf[BUFSIZ];
    int bytes, i;

    if (ioctl(fd, FIONREAD, &bytes) == -1 || bytes == 0)
        (void) notify_set_input_func(client, NOTIFY_FUNC_NULL, fd);
    else
        do
            if ((i = read(fd, buf, sizeof buf)) > 0)
                (void) write(1, buf, i);
        while (i > 0 && (bytes -= i) > 0);
    return NOTIFY_DONE;
}
