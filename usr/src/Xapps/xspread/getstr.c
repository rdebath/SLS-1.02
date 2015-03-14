
/*
 * Copyright (C) 1992  Board of Regents of the University of Wisconsin
 * on behalf of the Department of Electrical Engineering and Computer
 * Science, University of Wisconsin-Milwaukee, Milwaukee, WI 53201.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * The programs in this directory were developed by software engineering 
 * teams as part of the course "Introduction to Software Engineering" 
 * under the supervision of Professor G. Davida.
 * This is a modification of a program written or modified by
 * others.  The original copyrights, as per GNU General Public License,
 * may still be applicable.  The UWM copyright is applicable only
 * the those parts generated at UWM.
 *
 * Please send all changes, enhancements, and other comments about this
 * software to
 *     		soft-eng@cs.uwm.edu
 *
 * No Warranty, expressed or implied, comes with this software.
 * This software is intended to be used by not-for-profit
 * organizations. Selling this software for profit without
 * the permission of the Board of Regents of the University
 * of Wisconsin is strictly forbidden. 
 *
 * Contact:	soft-eng@cs.uwm.edu
 *			or
 *		
 *		Software Engineering Coordinator
 *		Computer Science
 *    		Department of EECS
 *		University of Wisconsin - Milwaukee
 *		Milwaukee, WI  53201
 *		414-229-4677
 *
 *		HISTORY,CLAIMS and CONTRIBUTIONS
 */


#define ctl(x) (x&037)
char *get_str(s, max_str_len)
  char *s;               /* prompt and returned string */
  int max_str_len;
{
  static char buf[1024]; /* hold the characters as they are typed */
  int count=0;           /* how many characters have been entered */
  int maxcount;          /* the max number of chars to be entered */
  int done=0;            /* true when input is finished */
  int slen;              /* length of prompt string */
  XEvent event;          /* input event structure */
  char keystr[3];        /* ASCII version of keypress */
  KeySym key;            /* keysym for keypress */

  clearlines(0,0);
  slen = strlen(s);
  max_str_len--;      /* decrease this to save room for null byte */
  maxcount = maintextcols - slen;
  maxcount = ((maxcount < max_str_len) ? maxcount : max_str_len);
  buf[0]='_'; /* the "cursor" */
  if (slen)
    XDrawImageString(dpy,mainwin,maingc,
                     textcol(0), textrow(0),
                     s, slen);
  XDrawImageString(dpy,mainwin,maingc,
                   textcol(slen), textrow(0),
                   "_", 1 );
  while (!done){
    XNextEvent(dpy,&event);
    switch(event.type){
      case Expose:
        update();
        if (slen)
          XDrawImageString(dpy,mainwin,maingc,
                           textcol(0), textrow(0),
                           s, slen);
        if (count)
          XDrawImageString(dpy,mainwin,maingc,
                           textcol(slen), textrow(0),
                           buf, count);
        XDrawImageString(dpy,mainwin,maingc,
                         textcol(slen+count), textrow(0),
                         "_", 1 );
        break;
      case MappingNotify:
        XRefreshKeyboardMapping(&event);
        break;
      case ConfigureNotify:
        sc_handleresize(&event);
        maxcount = maintextcols - slen;
        maxcount = ((maxcount < max_str_len) ? maxcount : max_str_len);
        break;
      case KeyPress:
        if (XLookupString(&event, keystr, 3, key, 0)){
          switch( keystr[0]){
            case 10: /* linefeed */
            case 13: /* carriage return */
              done =1;
              break;
            case ctl('h'):  /* backspace */
            case ctl('?'):  /* delete */
              if (count){
                buf[--count]='_';
                XDrawImageString(dpy,mainwin,maingc,
                                 textcol(slen+count+1),textrow(0),
                                 "_ ", 2);
              } else {
                fprintf(stderr,"\007"); /* bell */
              }
              break;
            default:
              if ((keystr[0]>=32) && (keystr[0]<127)){
                if (count<maxcount){
                  buf[count++]=keystr[0];
                  buf[count]='_';
                  XDrawImageString(dpy,mainwin,maingc,
                                   textcol(slen+count),textrow(0),
                                   buf+count-1, 2);
                } else
                    fprintf(stderr,"\007");
              } else
                  fprintf(stderr,"\007");
              break;
          } /* switch keystr[0] */
        } /* if XLookupString */
      } /* switch event.type */
    } /* while !done */
    buf[count]=0;
    strcpy(s,buf);
    clearlines(0,0);
    show_top_line();
    return s;
}

