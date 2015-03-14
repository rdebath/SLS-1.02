/*
 * Copyright 1989 by Georgia Tech Research Corporation, Atlanta, GA.
 * Copyright 1988, 1989 by Robert Viduya.
 * Copyright 1990 Jeff Sparkes.
 *
 *                         All Rights Reserved
 */

/*
 *	3270.h
 *
 *		Header file for 3270tool.  Contains a number of constants
 *		and macros that shouldn't be changed unless you know what
 *		you're doing.
 */

/* 3270 commands */
#define CMD_EAU		0x0F	/* erase all unprotected */
#define CMD_EW		0x05	/* erase/write */
#define CMD_EWA		0x0D	/* erase/write alternate */
#define CMD_RB		0x02	/* read buffer */
#define CMD_RM		0x06	/* read modified */
#define CMD_W		0x01	/* write */
#define CMD_NOP		0x03	/* no-op */

/* 3270 orders */
#define ORDER_SF	0x1D	/* start field */
#define ORDER_SFE	0x29	/* start field extended */
#define ORDER_SBA	0x11	/* set buffer address */
#define ORDER_SA	0x28	/* set attribute */
#define ORDER_MF	0x2C	/* modify field */
#define ORDER_IC	0x13	/* insert cursor */
#define ORDER_PT	0x05	/* program tab */
#define ORDER_RA	0x3C	/* repeat to address */
#define ORDER_EUA	0x12	/* erase unprotected to address */
#define ORDER_GE	0x08	/* graphic escape */
#define ORDER_YALE	0x2B	/* Yale sub command */

#define CHAR_WIDTH	(ibmfontinfo->max_bounds.rbearing + \
			 ibmfontinfo->min_bounds.lbearing)
#define CHAR_HEIGHT	(ibmfontinfo->max_bounds.ascent + \
			 ibmfontinfo->max_bounds.descent)
#define CHAR_BASE	(ibmfontinfo->max_bounds.descent)
#define X_TO_COL(x_pos)	((x_pos) / char_width)
#define Y_TO_ROW(y_pos)	((y_pos) / char_height)
#define COL_TO_X(col)	((col) * char_width)
#define ROW_TO_Y(row)	(((row) * char_height) + char_height)
#define ROW_TO_YC(row)	((row) * char_height - char_base)

#define BA_TO_ROW(ba)		((ba) / COLS)
#define BA_TO_COL(ba)		((ba) % COLS)
#define ROWCOL_TO_BA(r,c)	(((r) * COLS) + c)
#define INC_BA(ba)			\
    {					\
	if (++ba >= (COLS * ROWS))	\
	    ba = 0;			\
    }

#define DEC_BA(ba)			\
    {					\
	if (--ba < 0)			\
	    ba = (COLS * ROWS) - 1;	\
    }

/* field attribute definitions
 * 	The font used (3270.font) in ibmfont is based on the 3270 character
 *	generator font found on page 12-2 in the IBM 3270 Information Display
 *	System Character Set Reference.  Characters 0xC0 through 0xCF and
 *	0xE0 through 0xEF (inclusive) are purposely left blank and are used
 *	to represent field attributes as follows:
 *
 *		11x0xxxx
 *		  | ||||
 *		  | ||++--- 00 normal intensity/non-selectable
 *		  | ||      01 normal intensity/selectable
 *		  | ||      10 high intensity/selectable
 *		  | ||	    11 zero intensity/non-selectable
 *		  | |+----- unprotected(0)/protected(1)
 *		  | +------ alphanumeric(0)/numeric(1)
 *		  +-------- unmodified(0)/modified(1)
 */
#define FA_BASE			0xC0
#define FA_MASK			0xD0
#define FA_MODIFY		0x20
#define FA_NUMERIC		0x08
#define FA_PROTECT		0x04
#define FA_INTENSITY		0x03

#define FA_INT_NORM_NSEL	0x00
#define FA_INT_NORM_SEL		0x01
#define FA_INT_HIGH_SEL		0x02
#define FA_INT_ZERO_NSEL	0x03

#define IS_FA(c)		(((c) & FA_MASK) == FA_BASE)

#define FA_IS_MODIFIED(c)	((c) & FA_MODIFY)
#define FA_IS_NUMERIC(c)	((c) & FA_NUMERIC)
#define FA_IS_PROTECTED(c)	((c) & FA_PROTECT)

#define FA_IS_ZERO(c)					\
	(((c) & FA_INTENSITY) == FA_INT_ZERO_NSEL)
#define FA_IS_HIGH(c)					\
	(((c) & FA_INTENSITY) == FA_INT_HIGH_SEL)
#define FA_IS_NORMAL(c)					\
    (							\
	((c) & FA_INTENSITY) == FA_INT_NORM_NSEL	\
	||						\
	((c) & FA_INTENSITY) == FA_INT_NORM_SEL		\
    )
#define FA_IS_SELECTABLE(c)				\
    (							\
	((c) & FA_INTENSITY) == FA_INT_NORM_SEL		\
	||						\
	((c) & FA_INTENSITY) == FA_INT_HIGH_SEL		\
    )

/* WCC definitions */
#define WCC_START_PRINTER(c)	((c) & 0x08)
#define WCC_SOUND_ALARM(c)	((c) & 0x04)
#define WCC_KEYBOARD_RESTORE(c)	((c) & 0x02)
#define WCC_RESET_MDT(c)	((c) & 0x01)

/* AIDs */
#define AID_NO		0x60	/* no AID generated */
#define AID_ENTER	0x7D
#define AID_PF1		0xF1
#define AID_PF2		0xF2
#define AID_PF3		0xF3
#define AID_PF4		0xF4
#define AID_PF5		0xF5
#define AID_PF6		0xF6
#define AID_PF7		0xF7
#define AID_PF8		0xF8
#define AID_PF9		0xF9
#define AID_PF10	0x7A
#define AID_PF11	0x7B
#define AID_PF12	0x7C
#define AID_PF13	0xC1
#define AID_PF14	0xC2
#define AID_PF15	0xC3
#define AID_PF16	0xC4
#define AID_PF17	0xC5
#define AID_PF18	0xC6
#define AID_PF19	0xC7
#define AID_PF20	0xC8
#define AID_PF21	0xC9
#define AID_PF22	0x4A
#define AID_PF23	0x4B
#define AID_PF24	0x4C
#define AID_OICR	0xE6
#define AID_MSR_MHS	0xE7
#define AID_SELECT	0x7E
#define AID_PA1		0x6C
#define AID_PA2		0x6E
#define AID_PA3		0x6B
#define AID_CLEAR	0x6D
#define AID_SYSREQ	0xF0
