#include <ansidecl.h>
#include <stdlib.h>

#undef	abs
#undef	labs

#include <gnu-stabs.h>

function_alias(abs, labs, int, (i),
		DEFUN(abs, (i), int i))
