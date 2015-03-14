/* xkeycaps, Copyright (c) 1991, 1992 Jamie Zawinski <jwz@lucid.com>
 *
 * This file (along with the file kbd-sun-type5.h) describes the 
 * Sun type 5 PC keyboard.  This file is for the OpenWindows X 
 * server's conception of this keyboard.
 */

#define KBD_SUN_PC
#define KBD_OPENWINDOWS
#define Sun_type5_row0 Sun_type5pcow_row0
#define Sun_type5_row1 Sun_type5pcow_row1
#define Sun_type5_row2 Sun_type5pcow_row2
#define Sun_type5_row3 Sun_type5pcow_row3
#define Sun_type5_row4 Sun_type5pcow_row4
#define Sun_type5_row5 Sun_type5pcow_row5
#define Sun_type5_rows Sun_type5pcow_rows
#define Sun_type5 Sun_type5pcow

#include "kbd-sun-type5.h"

#undef Sun_type5_row0
#undef Sun_type5_row1
#undef Sun_type5_row2
#undef Sun_type5_row3
#undef Sun_type5_row4
#undef Sun_type5_row5
#undef Sun_type5_rows
#undef Sun_type5
#undef KBD_SUN_PC
#undef KBD_OPENWINDOWS
