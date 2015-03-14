/* Variable length strings
   Copyright (C) 1992 Joseph H. Allen

This file is part of JOE (Joe's Own Editor)

JOE is free software; you can redistribute it and/or modify it under the 
terms of the GNU General Public License as published by the Free Software 
Foundation; either version 1, or (at your option) any later version.  

JOE is distributed in the hope that it will be useful, but WITHOUT ANY 
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more 
details.  

You should have received a copy of the GNU General Public License along with 
JOE; see the file COPYING.  If not, write to the Free Software Foundation, 
675 Mass Ave, Cambridge, MA 02139, USA.  */ 

#ifndef _Ivs
#define _Ivs

#include "config.h"

/* Functions and global variable you have to define.  Replace these with
 * macros or defines here if they are not to be actual functions 
 */

#define sELEMENT(a) char a
#define sCAST char
/* sELEMENT(sdup()); */
#define sdup(a) (a)
/* sELEMENT(sdel()); */
#define sdel(a) 0
/* int scmp(); */
#define scmp(a,b) ((a)>(b)?1:((a)==(b)?0:-1))

int sicmp();

/* extern sELEMENT(sblank); */
#define sblank ' '
/* extern sELEMENT(sterm); */
#define sterm '\0'

/************************/
/* Creation/Destruction */
/************************/

/* sELEMENT(*vsmk(int len));
 * Create a variable length array.  Space for 'len' elements is preallocated.
 */
sELEMENT(*vsmk());

/* void vsrm(sELEMENT(*vary));
 * Free an array and everything which is in it.  Does nothing if 'vary' is
 * 0.
 */
void vsrm();

/********************/
/* Space management */
/********************/

/* int sSIZ(sELEMENT(*vary));
 * int sSiz(sELEMENT(*vary));
 * Access size part of array.  This int indicates the number of elements which
 * can fit in the array before realloc needs to be called.  It does not include
 * the extra space needed for the terminator and the header.
 *
 * sSIZ returns 0 if you pass it 0.  sSiz does not do this checking,
 * but can be used as an lvalue.
 */
#define sSIZ(a) ((a)?*((int *)(a)-2):0)
#define sSiz(a) (*((int *)(a)-2))

/* int sLEN(sELEMENT(*vary));
 * int sLen(sELEMENT(*vary));
 * Access length part of array.  This int indicates the number of elements
 * currently in the array (not including the terminator).  This should be
 * used primarily for reading the size of the array.  It can be used for
 * setting the size of the array, but it must be used with care since it
 * does not eliminate elements (if the size decreases) or make sure there's
 * enough room (if the size increases).  See vensure and vtrunc.
 *
 * sLEN return a length of zero if 'vary' is 0.
 * sLen doesn't do this checking, but can be used as an lvalue
 */
#define sLEN(a) ((a)?*((int *)(a)-1):0)
#define sLen(a) (*((int *)(a)-1))

/* int slen(sELEMENT(*ary));
 * Compute length of char or variable length array by searching for termination
 * element.  Returns 0 if 'vary' is 0.
 */
int slen();

/* sELEMENT(*vsensure(sELEMENT(*vary),int len));
 * Make sure there's enough space in the array for 'len' elements.  Whenever
 * vsensure reallocs the array, it allocates 25% more than the necessary
 * minimum space in anticipation of future expansion.  If 'vary' is 0,
 * it creates a new array.
 */
sELEMENT(*vsensure());

/* sELEMENT(*vszap(sELEMENT(*vary),int pos,int n));
 * Destroy n elements from an array beginning at pos.  Is ok if pos/n go
 * past end of array.  This does not change the sLEN() value of the array.
 * This does nothing and returns 0 if 'vary' is 0.  Note that this
 * function does not actually write to the array.  This does not stop if
 * a sterm is encountered.
 */
sELEMENT(*vszap());

/* sELEMENT(*vstrunc(sELEMENT(*vary),int len));
 * Truncate array to indicated size.  This zaps or expands with blank elements
 * and sets the LEN() of the array.  A new array is created if 'vary' is 0.
 */
sELEMENT(*vstrunc());

/************************************/
/* Function which write to an array */
/************************************/

/* sELEMENT(*vsfill(sELEMENT(*vary),int pos,sELEMENT(el),int len));
 * Set 'len' element of 'vary' beginning at 'pos' to duplications of 'el'.
 * Ok, if pos/len are past end of array.  If 'vary' is 0, a new array is
 * created.
 *
 * This does not zap previous values.  If you need that to happen, call
 * vszap first.  It does move the terminator around properly though.
 */
sELEMENT(*vsfill());

/* sELEMENT(*vsncpy(sELEMENT(*vary),int pos,sELEMENT(*array),int len));
 * Copy 'len' elements from 'array' onto 'vary' beginning at position 'pos'.
 * 'array' can be a normal char array since the length is passed seperately.  The
 * elements are copied, not duplicated.  A new array is created if 'vary' is
 * 0.  This does not zap previous elements.
 */
sELEMENT(*vsncpy());

/* sELEMENT(*vsndup(sELEMENT(*vary),int pos,sELEMENT(*array),int len));
 * Duplicate 'len' elements from 'array' onto 'vary' beginning at position
 * 'pos'.  'array' can be a char array since its length is passed seperately.  A
 * new array is created if 'vary' is 0.
 */
sELEMENT(*vsndup());

/* sELEMENT(*vsfield(sELEMENT(*vary),int pos,int len));
 * Make sure a field exists at 'pos' of length 'len'.  If there isn't one,
 * 'vary' is extended with blank elements.  This does not eliminate elements
 * which already exist in the field.  Use vszap for that.
 */
sELEMENT(*vsfield());

/* sELEMENT(*vsdup(sELEMENT(*vary)));
 * Duplicate array.  This is just a functionalized version of:
 *
 *   vsndup(NULL,0,vary,sLEN(vary));
 *
 * but since you need to be able to refer to this particular function by
 * address often it's given here.
 *
 * (actually, there's bazillions of these simple combinations of the above
 * functions and the macros of the next section.  You'll probably want to make
 * functionalized instances of the ones you use most often - especially since
 * the macros aren't safe).
 */ 
sELEMENT(*vsdup());

/* sELEMENT(*vsset(sELEMENT(*vary),int pos,sELEMENT(element)));
 * Set an element in an array.  Any value of 'pos' is valid.  A new array
 * is created if 'vary' is 0.  The previous contents of the position is
 * deleted.    This does not duplicate 'element'.  If you need 'element'
 * duplicated, call: vsset(vary,pos,sdup(element));
 */
sELEMENT(*_vsset());

#define vsset(v,p,el)  \
 (!(v) || (p)>sLen(v) || (p)>=sSiz(v) ?  \
  _vsset((v),(p),(el)) \
 : \
  ((p)==sLen(v) ? \
   ((v)[(p)+1]=0, sLen(v)=(p)+1, (v)[p]=(el), (v)) \
  : \
   ((v)[p]=(el), (v)) \
  ) \
 )   

/* sELEMENT(*vsadd(sELEMENT(*vary),sELEMENT(element)));
 * Concatenate a single element to the end of 'vary'.  A new array is created
 * if 'vary' is 0.  This does not duplicate element: call
 * vsadd(vary,sdup(element));  If you need it duplicated.
 */
#define vsadd(v,el) \
 (!(v) || sLen(v)==sSiz(v) ? \
  _vsset((v),sLEN(v),(el)) \
 : \
  ((v)[sLen(v)+1]=0, (v)[sLen(v)]=(el), sLen(v)=sLen(v)+1, (v)) \
 )

/**************************************/
/* Functions which read from an array */
/**************************************/

/* These macros are used to generate the address/size pairs which get
 * passed to the functions of the previous section.
 */

/* { sELEMENT(*),int } sv(sELEMENT(*array));
 * Return array,size pair.  Uses sLEN to get size.
 */
#define sv(a) (a),sLEN(a)

/* { sELEMENT(*),int } sz(sELEMENT(*array));
 * Return array,size pair.  Uses slen to get size.
 */
#define sz(a) (a),slen(a)

/* { sELEMENT(*),int } sc(sELEMENT(*array));
 * Return array,size pair.  Uses 'sizeof' to get size.
 */
#define sc(a) (a),(sizeof(a)/sizeof(sCAST)-1)

/* { sELEMENT(*),int } srest(sELEMENT(*vary),int pos);
 * Return array,size pair of rest of array beginning at pos.  If
 * pos is past end of array, gives size of 0.
 */
#define srest(a,p) ((a)+(p)),(((p)>sLEN(a))?0:sLen(a)-(p))

/* { sELEMENT(*),int } spart(sELEMENT(*vary),int pos,int len);
 * Return array,size pair of 'len' elements of array beginning with pos.  If
 * pos is past end of array, gives size of 0.  If pos+len is past end of array,
 * returns number of elements to end of array.
 */
#define spart(a,p,l) \
 ((a)+(p)),((p)>=sLEN(a)?0:((p)+(l)>sLen(a)?sLen(a)-(p):(l)))

/* sELEMENT(vsget(sELEMENT(*vary),int pos));
 * Get an element from an array.  Any value of pos is valid; if it's past the
 * end of the array or if 'vary' is 0, the terminator is returned.  This
 * does not make a duplicate of the returned element.  If you want that, pass
 * the return value of this to sdup.
 */
#define vsget(a,p) ((p)>=sLEN(a)?sterm:(a)[p])

/**********************/
/* Insertion/Deletion */
/**********************/

/* sELEMENT(*vsins(sELEMENT(*vary),int pos,int n));
 * Insert n empty slots into the array.  If 'pos' >= the length of the array,
 * the array is simply extended.  The new slots are not set to anything.
 * This does not set the elements in the created hole to any particular
 * value: use vsfill if you need that to occur.
 */
sELEMENT(*vsins());

/* sELEMENT(*vsdel(sELEMENT(*vary),int pos,int n));
 * Delete n slots from the array.  This does not zap the elements first; call
 * vszap first if you need this to happen.
 */
sELEMENT(*vsdel());

/*************************/
/* Searching and Sorting */
/*************************/

/* sELEMENT(*vssort(sELEMENT(*ary),int len))
 * Sort the elements of an array (char or variable length) using qsort().
 */
sELEMENT(*vssort());

/* int vsbsearch(sELEMENT(*ary),int len,sELEMENT(element));
 * Do a binary search on a sorted variable length or char array.  Returns position
 * of matching element or the position where the element should be if it was
 * not found.  (You should test with scmp to find out which).
 *
 * Hmm... this should really indicate whether or not the element was found.
 */
int vsbsearch();

/* int vsfirst(sELEMENT(*ary),int len,sELEMENT(element));
 * Find offset to first matching element in 'vary' or return ~0 if not found.
 */
int vsfirst();

/* int vslast(sELEMENT(*ary),int len,sELEMENT(element));
 * Find offset to last matching element in 'vary' or return ~0 if none found.
 */
int vslast();

/* int vss(sELEMENT(*a),int alen,sELEMENT(*b),int blen);
 * Do a substring search on 'a'.  Return offset from 'a' to first matching
 * occurance of 'b' in 'a' or return ~0 if none found.
 */
int vss();

/* int vscmpn(sELEMENT(*a),int alen,sELEMENT(*b),int blen);
 *
 * Compare two arrays using scmp.  If 'a' > 'b', return 1.  If 'a' == 'b',
 * return 0.  If 'a' < 'b', return -1.  Longer strings are > shorter ones if
 * their beginning match.
 */
int vscmpn();

/* int vscmp(sELEMENT(*a),sELEMENT(*b));
 *
 * Functionalized version of: vscmpn(sv(a),sv(b));
 */
int vscmp();

/* int vsicmpn(sELEMENT(*a),int alen,sELEMENT(*b),int blen);
 *
 * Compare two arrays using sicmp.  If 'a' > 'b', return 1.  If 'a' == 'b',
 * return 0.  If 'a' < 'b', return -1.  Longer strings are > shorter ones if
 * their beginning match.
 *
 * This is same as vscmpn except that it is case insensitive.
 */
int vsicmpn();

/* int vsicmp(sELEMENT(*a),sELEMENT(*b));
 *
 * Functionalized version of: vsicmpn(sv(a),sv(b));
 */
int vsicmp();

/* int vsscan(sELEMENT(*a),int alen,sELEMENT(*b),int blen);
 * Find offset of first matching element in 'a' which matches any
 * of the elements passed in 'b'.  Array 'b' must be sorted.
 *
 * Hmm... this really needs to return what the found element is.
 */
int vsscan();

/* int vsspan(sELEMENT(*a),int alen,sELEMENT(*b),int blen);
 * Find offset of first matching element in 'a' which does not match any
 * of the elements passed in 'b'.  Array 'b' must be sorted.
 */
int vsspan();

/***************/
/* Other stuff */
/***************/

/* char *vsread(char *d,int p,int (*getC)(void *ptr),void *ptr);
 * Replace 'd' with next line read from read-character function 'getC'.  If 
 * 'd' is 0, a new string is allocated.  If there is no more input, the string
 * is freed and 0 is returned.  The \n is deleted from the entered line.
 *
 * 'ptr' is passed as the first arg to 'getC'.  'getC' should return -1 if
 * there is no more input.
 */
char *vsread();

/* char *vwords(char *s,char **a,int len,char t);
 *
 * Generate a 't'-seperated word list from the words in the zero-terminated
 * array of zero-terminated strings 'a'.  For example a simple 'echo.c':
 *
 * main(argc,argv)
 * char *argv[];
 * {
 * printf("%s\n",vwords(NULL,argv,argc,' ')):
 * }
 *
 */
char *vswords();

/* char *vsfmt(char *s,char *fmt,...);
 *
 * (Yeah, yeah.. I really need to make this printf compatible, I know.)
 *
 * Printf (almost) to a variable length string.  If 's' is zero, a string is
 * created.  All chars from 'fmt' are copied to string except for these '%'
 * sequences:
 *
 *    % [' '|'+'|'-'] Base _ FieldWidth . Precision [l] {d|D|u|U|c|s}
 *    %% generates %
 *
 *    '+' means leading + needed for zero and positive numbers
 *    ' ' means leading space needed for zero and positive numbers
 *    '-' means left justified within field instead of right justified
 *    FieldWidth is minimum field width
 *
 *    s     Means insert next zero-terminated string from argument list
 *          Precision means maximum string size
 *
 *    c     Means insert next character from argument list.  The character is
 *          normally passed as an int.  If 'l' is given, the character is
 *          passed as a long.
 *
 *    d signed integer, use lower case letters for digits above 9
 *    D signed integer, use upper case letters for digits above 9
 *    u unsigned integer, use lower case letters for digits above 9
 *    U unsigned integer, use upper case letters for digits above 9
 *          If 'l' is give, a long or unsigned long is requested instead.
 *          Precision is minimum number of digits to generate in number.
 *          Default base is decimal.
 */
char *vsfmt();


#endif
