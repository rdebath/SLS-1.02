#include <sys/types.h>
#include <unistd.h>
#include <limits.h>
#include <signal.h>

void sig_alarm() {
	signal(SIGALRM, &sig_alarm);
	alarm(30);
	sync();
}

int main(void) {
	int i;

	signal(SIGTERM, SIG_IGN);
	signal(SIGINT, SIG_IGN);
	for (i=0 ; i < OPEN_MAX ; i++)
		close(i);
	chdir("/");
	sig_alarm();
	while (1) {
		pause();
	}
}
