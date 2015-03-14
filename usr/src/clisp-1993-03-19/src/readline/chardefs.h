/* chardefs.h -- changed by Bruno Haible, 16 March 1993 */

/* chardefs.h -- Character definitions for readline. */
#ifndef _CHARDEFS_H_
#define _CHARDEFS_H_

#ifndef savestring
#define savestring(x) (char *)strcpy (xmalloc (1 + strlen (x)), (x))
#endif

#ifndef whitespace
#define whitespace(c) (((c) == ' ') || ((c) == '\t'))
#endif

#ifdef CTRL
#undef CTRL
#endif

/* Define ISOLATIN if we are supporting all ISO Latin-1 characters. */
#if defined(linux)
#  define ISOLATIN
#endif

/* Define DOSCHARS if we are supporting all the MSDOS character set. */
#if defined(__MSDOS__) || defined(__EMX__)
#  define DOSCHARS
#endif

/* Number of characters in character set. */
#if defined(ISOLATIN) || defined(DOSCHARS)
#  define NUMCHARS 256
#else
#  define NUMCHARS 128
#endif

/* Some character stuff. */
#define control_character_threshold 0x020   /* smaller than this is control */
#define meta_character_threshold (NUMCHARS-1) /* larger than this is Meta. */
#define control_character_bit 0x40	    /* 0x000000, must be off. */
#define meta_character_bit NUMCHARS	    /* x0000000, must be on. */

#define CTRL(c) ((c) & (~control_character_bit))
#define META(c) ((c) | meta_character_bit)

#define UNMETA(c) ((c) & (~meta_character_bit))
#define UNCTRL(c) to_upper(((c)|control_character_bit))

#ifdef ISOLATIN
#define lowercase_p(c) \
  (((c) >= 'a' && (c) <= 'z') || ((c) >= 223 && (c) <= 255 && (c) != 247))
#define uppercase_p(c) \
  (((c) >= 'A' && (c) <= 'Z') || ((c) >= 192 && (c) <= 222 && (c) != 215))
#define to_upper(c) \
  ((c) >= 'a' && (c) <= 'z' ? (c) - 32 : \
   (c) >= 224 && (c) <= 225 && (c) != 247 ? (c) - 32 : \
   (c))
#define to_lower(c) \
  ((c) >= 'A' && (c) <= 'Z' ? (c) + 32 : \
   (c) >= 192 && (c) <= 222 && (c) != 215 ? (c) + 32 : \
   (c))
#else
#ifdef DOSCHARS
#define lowercase_p(c) \
  (((c) >= 'a' && (c) <= 'z')    \
   || ((c) >= 129 && (c) <= 141) \
   || ((c) == 145)               \
   || ((c) >= 147 && (c) <= 152) \
   || ((c) >= 160 && (c) <= 164))
#define uppercase_p(c) \
  (((c) >= 'A' && (c) <= 'Z')    \
   || ((c) == 128)               \
   || ((c) >= 142 && (c) <= 144) \
   || ((c) == 146)               \
   || ((c) >= 153 && (c) <= 154) \
   || ((c) == 165))
#define to_upper(c) \
  ((c) >= 'a' && (c) <= 'z' ? (c) - 32 : \
   (c) == 0x87 ? 0x80 : \
   (c) == 0x81 ? 0x9A : \
   (c) == 0x82 ? 0x90 : \
   (c) == 0x84 ? 0x8E : \
   (c) == 0x86 ? 0x8F : \
   (c) == 0x91 ? 0x92 : \
   (c) == 0x94 ? 0x99 : \
   (c) == 0xA4 ? 0xA5 : \
   (c))
#define to_lower(c) \
  ((c) >= 'A' && (c) <= 'Z' ? (c) + 32 : \
   (c) == 0x80 ? 0x87 : \
   (c) == 0x9A ? 0x81 : \
   (c) == 0x90 ? 0x82 : \
   (c) == 0x8E ? 0x84 : \
   (c) == 0x8F ? 0x86 : \
   (c) == 0x92 ? 0x91 : \
   (c) == 0x99 ? 0x94 : \
   (c) == 0xA5 ? 0xA4 : \
   (c))
#else
#define lowercase_p(c) (((c) >= 'a' && (c) <= 'z'))
#define uppercase_p(c) (((c) >= 'A' && (c) <= 'Z'))
#define to_upper(c) ((c) >= 'a' && (c) <= 'z' ? (c) - 32 : (c))
#define to_lower(c) ((c) >= 'A' && (c) <= 'Z' ? (c) + 32 : (c))
#endif
#endif

#define pure_alphabetic(c) (lowercase_p(c) || uppercase_p(c))
#define isletter(c) pure_alphabetic(c)

#ifndef to_upper
#define to_upper(c) (lowercase_p(c) ? ((c) - 32) : (c))
#define to_lower(c) (uppercase_p(c) ? ((c) + 32) : (c))
#endif

#define CTRL_P(c) ((c) < control_character_threshold)
#define META_P(c) ((c) > meta_character_threshold)

#ifndef CLISP /* avoid trouble when CLISP includes this file */
#define NEWLINE '\n'
#define RETURN CTRL('M')
#define RUBOUT 0x07f
#define TAB '\t'
#define ABORT_CHAR CTRL('G')
#define PAGE CTRL('L')
#define SPACE 0x020
#define ESC CTRL('[')
#endif

#endif  /* _CHARDEFS_H_ */
