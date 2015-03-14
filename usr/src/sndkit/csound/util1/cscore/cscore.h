# include <stdio.h>                     /*             cscore.h  */

# define PMAX 50

struct event {
	char op;
	char tnum;      /*  total sizeof these first 3  */
	short pcnt;     /*   should equal sizeof(float) */
	float p[PMAX+1];
};

struct evlist {
	int nslots;             /*   sizeof this = sizeof(*)    */
	struct event *e[1];
};

struct event buf;
struct event *createv(), *defev(), *getev(), *copyev();
struct evlist *lcreat(), *lappev(), *lget(), *lcopy(), *lcopyev();
struct evlist *lxins(), *lxtimev(), *lsepf(), *lcat();
