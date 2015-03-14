/* funmap.h -- Manipulation of readline funmaps. */
/* Bruno Haible 16.3.1993 */

#ifndef _FUNMAP_H_
#define _FUNMAP_H_

#ifndef __FUNCTION_DEF
typedef int Function ();
#define __FUNCTION_DEF
#endif

/* The data structure for mapping textual names to code addresses. */
typedef struct {
  char *name;
  Function *function;
} FUNMAP;

extern FUNMAP** funmap;

extern void rl_add_funmap_entry RL((char* name, Function* function));
extern void rl_initialize_funmap RL((void));
extern char** rl_funmap_names RL((void));

#endif /* _FUNMAP_H_ */
