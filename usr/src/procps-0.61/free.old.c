/* Linus B. Torvalds */
#include <unistd.h>
#include <stdio.h>
#include <ctype.h>
#include <fcntl.h>

unsigned char buf[1024];
unsigned long divisor = 1024;

volatile void usage(void)
{
        fprintf(stderr,"usage: free [-k] [-b]\n");
        exit(1);
}

void prnum(unsigned long num, char * p)
{
        do {
                p--;
                *p = '0'+(num % 10);
                num /= 10;
        } while (num > 0);
}

void convert(unsigned char * buf, int size)
{
        int i = 0;
        unsigned int num;

        if (divisor == 1)
                return;
        while (i < size) {
                if (!isdigit(buf[i])) {
                        i++;
                        continue;
                }
                num = buf[i]-'0';
                buf[i] = ' ';
                while (++i < size && isdigit(buf[i])) {
                        num *=10;
                        num += buf[i]-'0';
                        buf[i] = ' ';
                }
                num /= 1024;
                prnum(num,buf+i);
        }
}

int main(int argc, char ** argv)
{
        int fd, i;

        while (argc > 1) {
                argv++;
                argc--;
                if (!strcmp("-k",argv[0]))
                        divisor = 1024;
                else if (!strcmp("-b",argv[0]))
                        divisor = 1;
                else
                        usage();
        }
        fd = open("/proc/meminfo",O_RDONLY);
        if (fd < 0) {
                perror("/proc/meminfo");
                exit(i);
        }
        i = read(fd,buf,sizeof(buf)-1);
        if (i < 0) {
                perror("/proc/meminfo");
                exit(i);
        }
        close(fd);
        convert(buf,i);
        write(1,buf,i);
        return 0;
}


