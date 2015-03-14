/* history.h -- changed by Bruno Haible, 16 March 1993 */

/* History.h -- the names of functions that you can call in history. */

#ifndef _HISTORY_H_
#define _HISTORY_H_

#ifndef RL
/* For prototypes:  extern int foo RL((int x, int y)); */
#ifdef __STDC__
#define RL(args) args
#else
#define RL(args) ()
#endif
#endif

/* The structure used to store a history entry. */
typedef struct _hist_entry {
  char *line;
  void *data;
} HIST_ENTRY;

/* For convenience only.  You set this when interpreting history commands.
   It is the logical offset of the first history element. */
extern int history_base;

/* Begin a session in which the history functions might be used.  This
   just initializes the interactive variables. */
extern void using_history RL((void));

/* Place STRING at the end of the history list.
   The associated data field (if any) is set to NULL. */
extern void add_history RL((char* string));

/* Returns the number which says what history element we are now
   looking at.  */
extern int where_history RL((void));
  
/* Set the position in the history list to POS. */
extern int history_set_pos RL((int pos));

/* Search for STRING in the history list, starting at POS, an
   absolute index into the list.  DIR, if negative, says to search
   backwards from POS, else forwards.
   Returns the absolute index of the history element where STRING
   was found, or -1 otherwise. */
extern int history_search_pos RL((char* string, int dir, int pos));

/* A reasonably useless function, only here for completeness.  WHICH
   is the magic number that tells us which element to delete.  The
   elements are numbered from 0. */
extern HIST_ENTRY *remove_history RL((int which));

/* Stifle the history list, remembering only MAX number of entries. */
extern void stifle_history RL((int max));

/* Stop stifling the history.  This returns the previous amount the
   history was stifled by.  The value is positive if the history was
   stifled, negative if it wasn't. */
extern int unstifle_history RL((void));

/* Add the contents of FILENAME to the history list, a line at a time.
   If FILENAME is NULL, then read from ~/.history.  Returns 0 if
   successful, or errno if not. */
extern int read_history RL((char* filename));

/* Read a range of lines from FILENAME, adding them to the history list.
   Start reading at the FROM'th line and end at the TO'th.  If FROM
   is zero, start at the beginning.  If TO is less than FROM, read
   until the end of the file.  If FILENAME is NULL, then read from
   ~/.history.  Returns 0 if successful, or errno if not. */
extern int read_history_range RL((char* filename, int from, int to));

/* Append the current history to FILENAME.  If FILENAME is NULL,
   then append the history list to ~/.history.  Values returned
   are as in read_history ().  */
extern int write_history RL((char* filename));

/* Append NELEMENT entries to FILENAME.  The entries appended are from
   the end of the list minus NELEMENTs up to the end of the list. */
extern int append_history RL((int nelements, char* filename));

/* Make the history entry at WHICH have LINE and DATA.  This returns
   the old entry so you can dispose of the data.  In the case of an
   invalid WHICH, a NULL pointer is returned. */
extern HIST_ENTRY *replace_history_entry RL((int which, char* line, void* data));

/* Return the history entry at the current position, as determined by
   history_offset.  If there is no entry there, return a NULL pointer. */
extern HIST_ENTRY *current_history RL((void));

/* Back up history_offset to the previous history entry, and return
   a pointer to that entry.  If there is no previous entry, return
   a NULL pointer. */
extern HIST_ENTRY *previous_history RL((void));

/* Move history_offset forward to the next item in the input_history,
   and return the a pointer to that entry.  If there is no next entry,
   return a NULL pointer. */
extern HIST_ENTRY *next_history RL((void));

/* Return a NULL terminated array of HIST_ENTRY which is the current input
   history.  Element 0 of this list is the beginning of time.  If there
   is no history, return NULL. */
extern HIST_ENTRY **history_list RL((void));

/* Search the history for STRING, starting at history_offset.
   If DIRECTION < 0, then the search is through previous entries,
   else through subsequent.  If the string is found, then
   current_history () is the history entry, and the value of this function
   is the offset in the line of that history entry that the string was
   found in.  Otherwise, nothing is changed, and a -1 is returned. */
extern int history_search RL((char* string, int direction));

/* Expand the string STRING, placing the result into OUTPUT, a pointer
   to a string.  Returns:

   0) If no expansions took place (or, if the only change in
      the text was the de-slashifying of the history expansion
      character)
   1) If expansions did take place
  -1) If there was an error in expansion.

  If an error ocurred in expansion, then OUTPUT contains a descriptive
  error message. */
extern int history_expand RL((char* string, char** output));

/* Extract a string segment consisting of the FIRST through LAST
   arguments present in STRING.  Arguments are broken up as in
   the shell. */
extern char *history_arg_extract RL((int first, int last, char* string));

/* Return the number of bytes that the primary history entries are using.
   This just adds up the lengths of the_history->lines. */
extern int history_total_bytes RL((void));

extern int history_search_prefix RL((char* string, int direction));

#endif /* _HISTORY_H_ */
