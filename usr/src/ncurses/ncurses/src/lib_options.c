
/* This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for   *
*  details. If they are missing then this copy is in violation of    *
*  the copyright conditions.                                        */

/*
**	lib_options.c
**
**	The routines to handle option setting.
**
*/

#include <malloc.h>
#include "terminfo.h"
#include "curses.h"
#include "curses.priv.h"

int idlok(WINDOW *win, int flag)
{
#ifdef TRACE
	if (_tracing)
	    _tracef("idlok(%x,%d) called", win, flag);
#endif

    	if ((insert_line  &&  delete_line)
#ifdef UNIMPLEMENTED
	     ||  (change_scroll_region)
#endif
	   )
	    curscr->_idlok = flag;
}


int clearok(WINDOW *win, int flag)
{
#ifdef TRACE
	if (_tracing)
	    _tracef("clearok(%x,%d) called", win, flag);
#endif

    	if (win == curscr)
	    newscr->_clear = flag;
	else
	    win->_clear = flag;
}


int leaveok(WINDOW *win, int flag)
{
#ifdef TRACE
	if (_tracing)
	    _tracef("leaveok(%x,%d) called", win, flag);
#endif

    	win->_leave = flag;
}


int scrollok(WINDOW *win, int flag)
{
#ifdef TRACE
	if (_tracing)
	    _tracef("scrollok(%x,%d) called", win, flag);
#endif

    	win->_scroll = flag;
}


int nodelay(WINDOW *win, int flag)
{
#ifdef TRACE
	if (_tracing)
	    _tracef("nodelay(%x,%d) called", win, flag);
#endif

    	if (flag == TRUE)
		win->_delay = 0;
	else win->_delay = -1;
	return OK;
}

int notimeout(WINDOW *win, bool f)
{
#ifdef TRACE
	if (_tracing)
		_tracef("notimout(%x,%d) called", win, f);
#endif

	win->_notimeout = f;
	return OK;
}

int wtimeout(WINDOW *win, int delay)
{
#ifdef TRACE
	if (_tracing)
		_tracef("wtimeout(%x,%d) called", win, delay);
#endif

	win->_delay = delay;
	return OK;
}

static void init_keytry();
static void add_to_try(char *, short);

int keypad(WINDOW *win, int flag)
{
#ifdef TRACE
	if (_tracing)
	    _tracef("keypad(%x,%d) called", win, flag);
#endif

   	win->_use_keypad = flag;

	if (flag  &&  keypad_xmit)
	    tputs(keypad_xmit, 1, outc);
	else if (! flag  &&  keypad_local)
	    tputs(keypad_local, 1, outc);
	    
    if (SP->_keytry == UNINITIALISED)
	    init_keytry();
}



int meta(WINDOW *win, int flag)
{
#ifdef TRACE
	if (_tracing)
	    _tracef("meta(%x,%d) called", win, flag);
#endif

	win->_use_meta = flag;

	if (flag  &&  meta_on)
	    tputs(meta_on, 1, outc);
	else if (! flag  &&  meta_off)
	    tputs(meta_off, 1, outc);
}

/*
**      init_keytry()
**
**      Construct the try for the current terminal's keypad keys.
**
*/


static struct  try *newtry;

static void init_keytry()
{
        newtry = NULL;
	
#include "keys.tries"

	SP->_keytry = newtry;
}


void add_to_try(char *str, short code)
{
        static bool     out_of_memory = FALSE;
        struct try      *ptr, *savedptr;

	if (! str  ||  out_of_memory)
	    return;
	
	if (newtry != NULL)    
	{
	    ptr = newtry;
	    
       	    for (;;)
	    {
	        while (ptr->ch != *str  &&  ptr->sibling != NULL)
	            ptr = ptr->sibling;
	    
	        if (ptr->ch == *str)
		{
		    if (*(++str))
		    {
	                if (ptr->child != NULL)
		            ptr = ptr->child;
                        else
		            break;
		    }
		    else
		    {
		        ptr->value = code;
			return;
		    }
		}
		else
	        {
		    if ((ptr->sibling = (struct try *) malloc(sizeof *ptr)) == NULL)
		    {
		        out_of_memory = TRUE;
			return;
		    }
		    
		    savedptr = ptr = ptr->sibling;
		    ptr->child = ptr->sibling = NULL;
		    ptr->ch = *str++;
		    ptr->value = (short) NULL;
		    
                    break;
	        }
	    } /* end for (;;) */  
	}
	else    /* newtry == NULL :: First sequence to be added */
	{
	    savedptr = ptr = newtry = (struct try *) malloc(sizeof *ptr);
	    
	    if (ptr == NULL)
	    {
	        out_of_memory = TRUE;
		return;
	    }
	    
	    ptr->child = ptr->sibling = NULL;
	    ptr->ch = *(str++);
	    ptr->value = (short) NULL;
	}
	
	    /* at this point, we are adding to the try.  ptr->child == NULL */
	    
	while (*str)
	{
	    ptr->child = (struct try *) malloc(sizeof *ptr);
	    
	    ptr = ptr->child;
	    
	    if (ptr == NULL)
	    {
	        out_of_memory = TRUE;
		
		ptr = savedptr;
		while (ptr != NULL) 
		{
		    savedptr = ptr->child;
		    free(ptr);
		    ptr = savedptr;
		}
		
		return;
	    }
	    
	    ptr->child = ptr->sibling = NULL;
	    ptr->ch = *(str++);
	    ptr->value = (short) NULL;
	}
	
	ptr->value = code;
	return;
}
