


ZMORE(1)                 USER COMMANDS                   ZMORE(1)



NAME
     zmore - file perusal filter for crt  viewing  of  compressed
     text

SYNOPSIS
     zmore [ name ...  ]

DESCRIPTION
     _Z_m_o_r_e is a filter which  allows  examination  of  compressed
     text  files one screenful at a time on a soft-copy terminal.
     It normally pauses after each screenful,  printing  --More--
     at  the bottom of the screen.  If the user then types a car-
     riage return, one more line is displayed.  If the user  hits
     a space, another screenful is displayed.  Other possibilites
     are enumerated later.

     _Z_m_o_r_e looks in the file /_e_t_c/_t_e_r_m_c_a_p to  determine  terminal
     characteristics,  and  to determine the default window size.
     On a terminal capable of displaying 24  lines,  the  default
     window size is 22 lines.

     Other sequences which may be typed when  _z_m_o_r_e  pauses,  and
     their  effects,  are  as  follows  (_i is an optional integer
     argument, defaulting to 1) :

     _i<space>
          display _i more lines, (or another screenful if no argu-
          ment is given)

     ^D   display 11 more lines (a ``scroll'').  If _i  is  given,
          then the scroll size is set to _i.

     d    same as ^D (control-D)

     _iz   same as typing a  space  except  that  _i,  if  present,
          becomes the new window size.  Note that the window size
          reverts back to the default at the end of  the  current
          file.

     _is   skip _i lines and print a screenful of lines

     _if   skip _i screenfuls and print a screenful of lines

     q or Q
          quit reading the current file; go on to  the  next  (if
          any)

     e    When the prompt --More--(Next file: _f_i_l_e)  is  printed,
          this command causes zmore to exit.

     =    Display the current line number.




Sun Release 4.1           Last change:                          1






ZMORE(1)                 USER COMMANDS                   ZMORE(1)



     _i/expr
          search for the _i-th occurrence of the  regular  expres-
          sion  _e_x_p_r.  If the pattern is not found, _z_m_o_r_e goes on
          to the next file (if any).  Otherwise, a  screenful  is
          displayed,  starting  two  lines before the place where
          the expression was found.  The user's  erase  and  kill
          characters  may be used to edit the regular expression.
          Erasing back past the first column cancels  the  search
          command.

     _in   search for the _i-th  occurrence  of  the  last  regular
          expression entered.

     !command
          invoke a shell with _c_o_m_m_a_n_d. The character `!' in "com-
          mand" are replaced with the the previous shell command.
          The sequence "\!" is replaced by "!".

     :q or :Q
          quit reading the current file; go on to  the  next  (if
          any) (same as q or Q).

     .    (dot) repeat the previous command.

     The commands take effect immediately, i.e., it is not neces-
     sary  to  type  a  carriage return.  Up to the time when the
     command character itself is given, the user may hit the line
     kill  character  to  cancel  the  numerical  argument  being
     formed.  In addition, the user may hit the  erase  character
     to redisplay the --More-- message.

     At any time when output is being sent to the  terminal,  the
     user  can hit the quit key (normally control-\).  _Z_m_o_r_e will
     stop sending output, and will  display  the  usual  --More--
     prompt.   The  user may then enter one of the above commands
     in the normal manner.  Unfortunately, some  output  is  lost
     when this is done, due to the fact that any characters wait-
     ing in the terminal's output queue are flushed when the quit
     signal occurs.

     The terminal is set to _n_o_e_c_h_o mode by this program  so  that
     the  output  can be continuous.  What you type will thus not
     show on your terminal, except for the / and !  commands.

     If the standard output is not a teletype,  then  _z_m_o_r_e  acts
     just  like _z_c_a_t, except that a header is printed before each
     file.

FILES
     /etc/termcap        Terminal data base





Sun Release 4.1           Last change:                          2






ZMORE(1)                 USER COMMANDS                   ZMORE(1)



SEE ALSO
     more(1), zcat(1), compress(1), uncompress(1)





















































Sun Release 4.1           Last change:                          3



