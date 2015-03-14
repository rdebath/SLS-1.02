/* xkeycaps, Copyright (c) 1991, 1992 Jamie Zawinski <jwz@lucid.com>
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation.  No representations are made about the suitability of this
 * software for any purpose.  It is provided "as is" without express or 
 * implied warranty.
 */

#include <X11/keysym.h>

#include "kbd-sun-type2.h"
#include "kbd-sun-type3.h"
#include "kbd-sun-type4.h"
#include "kbd-sun-type5.h"
#include "kbd-sun-type5pc.h"
#include "kbd-sun-type4ow.h"
#include "kbd-sun-type5ow.h"
#include "kbd-sun-type5pcow.h"
#include "kbd-ncd-n97.h"
#include "kbd-ncd-n101.h"
#include "kbd-ncd-n102.h"
#include "kbd-ncd-n102sf.h"
#include "kbd-ncd-n102n.h"
#include "kbd-ncd-n102fr.h"
#include "kbd-ncd-n108.h"
#include "kbd-ncd-vt220.h"
#include "kbd-dec-lk201.h"
#include "kbd-dec-lk401.h"
#include "kbd-dec-lk421.h"
#include "kbd-ibm-rs6k.h"
#include "kbd-sco-110.h"
#include "kbd-hp-hil.h"
#include "kbd-hp-700rx.h"
#include "kbd-hp-pc.h"
#include "kbd-atari-tt.h"
#include "kbd-sony-nws.h"
#include "kbd-dell.h"
#include "kbd-sgi-iris.h"
#include "kbd-labtam.h"
#include "kbd-tek-101.h"
#include "kbd-tek-101-4.h"
#include "kbd-tek-vt200.h"
#include "kbd-explorer.h"

struct keyboard *all_kbds [] = {
  &Sun_type2,
  &Sun_type3,
  &Sun_type4,
  &Sun_type5,
  &Sun_type5pc,
  &Sun_type4ow,
  &Sun_type5ow,
  &Sun_type5pcow,
  &NCD_N97,
  &NCD_N101,
  &NCD_N102,
  &NCD_N102sf,
  &NCD_N102n,
  &NCD_N102fr,
  &NCD_N108,
  &NCD_VT220,
  &DEC_LK201,
  &DEC_LK401,
  &DEC_LK421,
  &IBM_rs6k,
  &SCO_ODT_110,
  &HP_HIL,
  &HP_700RX,
  &HP_PC,
  &Atari_TT,
  &NWS_1250,
  &DELL_PC,
  &SGI_iris,
  &H101RXd,
  &TEK_101,
  &TEK_101_4,
  &TEK_vt200,
  &Explorer,
  0
};


/* When we have to guess, and haven't a clue.
   The Imakefile can define this to be 0, to make there be no default.
 */
#ifndef DEFAULT_KBD_NAME
# define DEFAULT_KBD_NAME "Sun4"
#endif
