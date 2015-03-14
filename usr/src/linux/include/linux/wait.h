#ifndef _LINUX_WAIT_H
#define _LINUX_WAIT_H

#define WNOHANG		1
#define WUNTRACED	2

struct wait_queue {
	struct task_struct * task;
	struct wait_queue * next;
};

typedef struct select_table_struct {
	int nr;
	struct select_table_entry {
		struct wait_queue wait;
		struct wait_queue ** wait_address;
	} * entry;
} select_table;

#define __MAX_SELECT_TABLE_ENTRIES (4096 / sizeof (struct select_table_entry))

#endif
