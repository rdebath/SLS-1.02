/*
 * notify_pipe.c -- fork and set up a pipe to read the IO from the
 * forked process.  The program to run is specified on the command
 * line.  The functions notify_set_input_func() and
 * notify_set_output_func() are used to install functions which read
 * and write to the process' stdin and stdout.
 * The program does not use any xview code -- just the notifier.
 */
#include <stdio.h>
#include <errno.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/resource.h>
#include <sys/ioctl.h>
#include <xview/notify.h>

Notify_client client1 = (Notify_client)10;
Notify_client client2 = (Notify_client)11;

int pipe_io[2][2]; /* see diagram */
/*
 *                 [0]                           [1]
 *    child reads:  |========= pipe_io[0] ========| <- parent writes
 *   pipe_io[0][0]                                     pipe_io[0][1]
 *
 *    parent reads: |========= pipe_io[1] ========| <- child writes
 *   pipe_io[1][0]                                     pipe_io[1][1]
 *
 * The parent process reads the output of the child process by reading
 * pipe_io[1][0] because the child is writing to pipe_io[1][1].
 * The child process gets its input from pipe_io[0][0] because the
 * parent writes to pipe_io[0][1].  Thus, one process is reading from
 * one end of the pipe while the other is writing at the other end.
 */
main(argc, argv)
char *argv[];
{
    Notify_value        read_it(), write_it(), sigchldcatcher();
    int                 i, pid;
    FILE                *fp;

    if (!*++argv)
        puts("specify a program [w/args]"), exit(1);

    pipe(pipe_io[0]); /* set up input pipe */
    pipe(pipe_io[1]); /* set up output pipe */
    switch (pid = fork()) {
        case -1:
            close(pipe_io[0][0]);
            close(pipe_io[0][1]);
            close(pipe_io[1][0]);
            close(pipe_io[1][1]);
            perror("fork failed");
            exit(1);
        case  0: /* child */
            /* redirect child's stdin (0), stdout (1) and stderr(2) */
            dup2(pipe_io[0][0], 0);
            dup2(pipe_io[1][1], 1);
            dup2(pipe_io[1][1], 2);
            for (i = getdtablesize(); i > 2; i--)
                (void) close(i);
            for (i = 0; i < NSIG; i++)
                (void) signal(i, SIG_DFL);
            execvp(*argv, argv);
            if (errno == ENOENT)
                printf("%s: command not found.\n", *argv);
            else
                perror(*argv);
            perror("execvp");
            _exit(-1);
        default: /* parent */
            close(pipe_io[0][0]); /* close unused portions of pipes */
            close(pipe_io[1][1]);
        }

    /* when the process outputs data, read it */
    notify_set_input_func(client1, read_it, pipe_io[1][0]);
    notify_set_wait3_func(client1, sigchldcatcher, pid);

    /* wait for user input -- then write data to pipe */
    notify_set_input_func(client2, write_it, 0);
    notify_set_wait3_func(client2, sigchldcatcher, pid);

    notify_start();
}

/*
 * callback routine for when there is data on the parent's stdin to
 * read.  Read it and then write the data to the child process via
 * the pipe.
 */
Notify_value
write_it(client, fd)
Notify_client   client;
int fd;
{
    char buf[BUFSIZ];
    int bytes, i;

    /* only write to pipe (child's stdin) if user typed anything */
    if (ioctl(fd, FIONREAD, &bytes) == -1 || bytes == 0) {
        notify_set_input_func(client, NOTIFY_FUNC_NULL, pipe_io[0][1]);
        close(pipe_io[0][1]);
    } else
        while (bytes > 0) {
            if ((i = read(fd, buf, sizeof buf)) > 0) {
                printf("[Sending %d bytes to pipe (fd=%d)]\n",
                    i, pipe_io[0][1]);
                write(pipe_io[0][1], buf, i);
            } else if (i == -1)
                break;
            bytes -= i;
        }
    return NOTIFY_DONE;
}

/*
 * callback routine for when there is data on the child's stdout to
 * read.  Read, then write the data to stdout (owned by the parent).
 */
Notify_value
read_it(client, fd)
Notify_client   client;
register int fd;
{
    char buf[BUFSIZ];
    int bytes, i;

    if (ioctl(fd, FIONREAD, &bytes) == 0)
        while (bytes > 0) {
            if ((i = read(fd, buf, sizeof buf)) > 0) {
                printf("[Reading %d bytes from pipe (fd=%d)]\n",
                    i, fd);
                (void) write(1, buf, i);
                bytes -= i;
            }
        }
    return NOTIFY_DONE;
}

/*
 * handle the death of the child.  If the process dies, the child
 * dies and generates a SIGCHLD signal.  Capture it and disable the
 * functions that talk to the pipes.
 */
Notify_value
sigchldcatcher(client, pid, status, rusage)
Notify_client client; /* the client noted in main() */
int pid; /* the pid that died */
union wait *status; /* the status of the process (unused here) */
struct rusage *rusage; /* resources used by this process (unused) */
{
    if (WIFEXITED(*status)) {
        printf("Process termined with status %d\n", status->w_retcode);
        /* unregister input func with appropriate file descriptor */
        notify_set_input_func(client, NOTIFY_FUNC_NULL,
            (client == client1)? pipe_io[1][0] : 0);
        return NOTIFY_DONE;
    }
    puts("SIGCHLD not handled");
    return NOTIFY_IGNORED;
}
