/* some things shouldn't be macros, get out your barf bag */

#define malloc library_malloc
#define realloc library_realloc
#define calloc library_calloc
#define NO_FIX_MALLOC
#include_next <stdlib.h>
#undef malloc
#undef realloc
#undef calloc
#undef atoi
#undef atol

