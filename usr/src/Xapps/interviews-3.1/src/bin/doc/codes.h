/*
 * Copyright (c) 1991 Stanford University
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Stanford not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  Stanford makes no representations about
 * the suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * STANFORD DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.
 * IN NO EVENT SHALL STANFORD BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION
 * WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#ifndef codes_h
#define codes_h

/*
 * special TeX-like formatting 'characters'
 * (control characters with high bit set)
 */

const int hfil = 0200;
const int vfil = 0201;
const int hrule = 0202;
const int smallskip = 0203;
const int medskip = 0204;
const int bigskip = 0205;
const int newpage = 0206;
const int pagebreak = 0207;
const int parbreak = 0210;
const int linebreak = 0211;
const int nobreakspace = 0212;
const int wordspace = 0213;
const int sentencespace = 0214;
const int quadspace = 0215;
const int discretionaryhyphen = 0216;
const int visiblehyphen = 0217;
const int anchor = 0220;
const int bigvspace = 0221;
const int medvspace = 0222;
const int smallvspace = 0223;
const int thinspace = 0224;
const int negthinspace = 0225;
const int hfill = 0226;
const int hleader = 0227;

/*
 * Adobe encodings of some TeX characters
 */

const int section = 0247;
const int endash = 0261;
const int dag = 0262;
const int ddag = 0263;
const int cdot = 0264;
const int paragraph = 0266;
const int bullet = 0267;
const int emdash = 0320;

#endif
