/* Definitions for Intel 386 running Linux
 * Copyright (C) 1992 Free Software Foundation, Inc.
 *
 * Written by H.J. Lu (hlu@eecs.wsu.edu)
 *
 * Linux is a POSIX.1 compatible UNIX clone for i386, which uses GNU
 * stuffs as the native stuffs.
 */

#if 0	/* The FSF has fixed the known bugs. But ....... */

/* Linux has a hacked gas 1.38.1, which can handle repz, repnz
 * and fildll.
 */

#define GOOD_GAS

#endif

/* This is tested by i386gas.h.  */
#define YES_UNDERSCORES

#include "i386gstabs.h"

/* Specify predefined symbols in preprocessor.  */

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dunix -Di386 -Dlinux"

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef WCHAR_TYPE
#define WCHAR_TYPE "long int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE BITS_PER_WORD

#undef HAVE_ATEXIT
#define HAVE_ATEXIT

/* Linux uses ctype from glibc.a. I am not sure how complete it is.
 * For now, we play safe. It may change later.
 */
#if 0
#undef MULTIBYTE_CHARS
#define MULTIBYTE_CHARS	1
#endif

#undef LIB_SPEC
#define LIB_SPEC "%{g*:-lg} %{!g*:%{!p:%{!pg:-lc}}%{p:-lgmon -lc_p}%{pg:-lgmon -lc_p}}"


#undef STARTFILE_SPEC
#undef GPLUSPLUS_INCLUDE_DIR

#ifdef CROSS_COMPILE

/*
 * For cross-compile, we just need to search `$(tooldir)/lib'
 */

#define STARTFILE_SPEC  \
  "%{g*:crt0.o%s -static} %{!g*:%{pg:gcrt0.o%s -static} %{!pg:%{p:gcrt0.o%s -static} %{!p:crt0.o%s %{!static:%{nojump:-nojump}} %{static:-static}}}} -L"TOOLDIR"/lib"

/*
 *The cross-compile uses this.
 */
#define GPLUSPLUS_INCLUDE_DIR TOOLDIR"/g++-include"

#else

#define STARTFILE_SPEC  \
  "%{g*:crt0.o%s -static} %{!g*:%{pg:gcrt0.o%s -static} %{!pg:%{p:gcrt0.o%s -static} %{!p:crt0.o%s %{!static:%{nojump:-nojump}} %{static:-static}}}}"

/*
 *The native Linux system uses this.
 */
#define GPLUSPLUS_INCLUDE_DIR "/usr/g++-include"
#endif

/* We need this to get symbols to be indirected properly. */
#undef ENCODE_SECTION_INFO
#define ENCODE_SECTION_INFO(DECL) \
do									\
  {									\
    if (flag_pic)							\
      {									\
	rtx rtl = (TREE_CODE_CLASS (TREE_CODE (DECL)) != 'd'		\
		   ? TREE_CST_RTL (DECL) : DECL_RTL (DECL));		\
	SYMBOL_REF_FLAG (XEXP (rtl, 0))					\
	  = (TREE_CODE_CLASS (TREE_CODE (DECL)) != 'd'			\
	     || ! TREE_PUBLIC (DECL));					\
      }									\
    else  if (TREE_PUBLIC (DECL))					\
	{ register rtx temp = gen_reg_rtx (Pmode);			\
	 LINUX_FIX_GLOBALNAME(XSTR((DECL_RTL (DECL)),0)); 		\
	 emit_move_insn (temp, gen_rtx (MEM, Pmode, (DECL_RTL (DECL))));  \
	   (DECL_RTL (DECL)) = temp; 					\
	 goto WIN;							\
	}								\
  }									\
while (0)

#define LINUX_FIX_GLOBALNAME(X) \
    {								\
        char * newname;          				\
	newname = (char*) malloc(strlen(X)+7);			\
	sprintf(newname,"__GOT_%s",X);				\
	printf("Using %s\n",newname);				\
	(X) = newname;						\
      };

#if 0
        rtx new;  char newname[256];          				\
	rtx rtl = (TREE_CODE_CLASS (TREE_CODE (DECL)) != 'd'		\
		   ? DECL_RTL (DECL) : DECL_RTL (DECL));		\
	    rtl = gen_rtx (MEM, Pmode, rtl);				\
	 /* emit_move_insn (rtl, new); */      				\
	    sprintf(newname,"__GOT_%s",DECL_NAME(DECL));		\
	      printf("Using %s\n",newname);				\
	  DECL_ASSEMBLER_NAME(DECL) = get_identifier(newname); 



#define LINUX_SYMBOLIC_CONST(X)	\
((GET_CODE (X) == SYMBOL_REF && !SYMBOL_REF_FLAG(X))   			\
 || GET_CODE (X) == LABEL_REF						\
 || (GET_CODE (X) == CONST && symbolic_reference_mentioned_p (X)))

/* Zero if this contains a (CONST (PLUS (SYMBOL_REF) (...))) and the
   symbol in the SYMBOL_REF is an external symbol.  */

#define LINUX_CONSTANT_P(X) \
 (! (GET_CODE ((X)) == CONST					\
     && GET_CODE (XEXP ((X), 0)) == PLUS			\
     && GET_CODE (XEXP (XEXP ((X), 0), 0)) == SYMBOL_REF	\
     && SYMBOL_REF_FLAG (XEXP (XEXP ((X), 0), 0))))

#define LINUX_CONSTANT_ADDRESS_P(X)   				\
  (GET_CODE (X) == LABEL_REF 						\
   || (GET_CODE (X) == SYMBOL_REF && !SYMBOL_REF_FLAG (X))		\
   || (GET_CODE (X) == CONST && LINUX_CONSTANT_P(X))		\
   || GET_CODE (X) == CONST_INT)


#undef GO_IF_LEGITIMATE_ADDRESS
#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)	\
{									\
  if (LINUX_CONSTANT_ADDRESS_P (X)					\
      && (! flag_pic || LEGITIMATE_PIC_OPERAND_P (X)))			\
    goto ADDR;								\
  GO_IF_INDEXING (X, ADDR);						\
  if (GET_CODE (X) == PLUS && LINUX_CONSTANT_ADDRESS_P (XEXP (X, 1))) \
    {									\
      rtx x0 = XEXP (X, 0);						\
      if (! flag_pic || ! LINUX_SYMBOLIC_CONST (XEXP (X, 1)))	\
	{ GO_IF_INDEXING (x0, ADDR); }					\
      else if (x0 == pic_offset_table_rtx)				\
	goto ADDR;							\
      else if (GET_CODE (x0) == PLUS)					\
	{								\
	  if (XEXP (x0, 0) == pic_offset_table_rtx)			\
	    { GO_IF_INDEXABLE_BASE (XEXP (x0, 1), ADDR); }		\
	  if (XEXP (x0, 1) == pic_offset_table_rtx)			\
	    { GO_IF_INDEXABLE_BASE (XEXP (x0, 0), ADDR); }		\
	}								\
    }									\
}


#undef LEGITIMIZE_ADDRESS
#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN)   \
{ extern rtx legitimize_pic_address ();					\
  int ch = (X) != (OLDX);						\
	  debug_rtx(X); \
  if (flag_pic && SYMBOLIC_CONST (X))					\
    {									\
      (X) = legitimize_pic_address (X, 0);				\
      if (memory_address_p (MODE, X))					\
	goto WIN;							\
    }									\
      if(GET_CODE (X) == SYMBOL_REF && SYMBOL_REF_FLAG(X))		\
	{ register rtx temp = gen_reg_rtx (Pmode);			\
	 SYMBOL_REF_FLAG(X) = 0;					\
	 LINUX_FIX_GLOBALNAME(XSTR((X),0)); 			     	\
	 emit_move_insn (temp, gen_rtx (MEM, Pmode, (X)));      	\
	   (X) = temp;							\
	 goto WIN;							\
	}								\
      if((GET_CODE ((X)) == CONST					\
     && GET_CODE (XEXP ((X), 0)) == PLUS				\
     && GET_CODE (XEXP (XEXP ((X), 0), 0)) == SYMBOL_REF		\
     && SYMBOL_REF_FLAG (XEXP (XEXP ((X), 0), 0))))			\
	{ register rtx temp = gen_reg_rtx (Pmode);			\
	 SYMBOL_REF_FLAG(XEXP (XEXP ((X), 0), 0)) = 0;			\
	   LINUX_FIX_GLOBALNAME(XSTR(XEXP (XEXP ((X), 0), 0),0));      	\
	 emit_move_insn (temp, gen_rtx (MEM, Pmode, (XEXP (XEXP ((X), 0), 0))));      	\
	   (XEXP (XEXP ((X), 0), 0)) = temp;				\
	   temp = gen_reg_rtx (Pmode);					\
	  emit_move_insn (temp, (XEXP ((X), 0)));			\
         (X) = temp;					\
	 GO_IF_LEGITIMATE_ADDRESS (MODE, X, WIN); 			\
	}								\
  if (GET_CODE (X) == PLUS)						\
    { if (GET_CODE (XEXP (X, 0)) == MULT)				\
	ch = 1, XEXP (X, 0) = force_operand (XEXP (X, 0), 0);		\
      if (GET_CODE (XEXP (X, 1)) == MULT)				\
	ch = 1, XEXP (X, 1) = force_operand (XEXP (X, 1), 0);		\
      if (ch && GET_CODE (XEXP (X, 1)) == REG				\
	  && GET_CODE (XEXP (X, 0)) == REG)				\
	goto WIN;							\
      if (flag_pic && SYMBOLIC_CONST (XEXP (X, 1)))			\
        ch = 1, (X) = legitimize_pic_address (X, 0);			\
      if(GET_CODE (XEXP (X, 0)) == SYMBOL_REF && SYMBOL_REF_FLAG(XEXP (X, 0)))		\
	{ register rtx temp = gen_reg_rtx (Pmode);			\
	 LINUX_FIX_GLOBALNAME(XSTR(XEXP ((X), 0),0));		      	\
	 SYMBOL_REF_FLAG(XEXP (X, 0)) = 0;	       			\
	 emit_move_insn (temp, gen_rtx (MEM, Pmode, (XEXP (X, 0))));    \
	   ((XEXP (X, 0))) = temp;     					\
	 printf("Using %s\n",GET_RTX_NAME(X));				\
	 goto WIN;							\
	}								\
      if (ch) { GO_IF_LEGITIMATE_ADDRESS (MODE, X, WIN); }		\
      if (GET_CODE (XEXP (X, 0)) == REG)                                \
	{ register rtx temp = gen_reg_rtx (Pmode);			\
	  register rtx val = force_operand (XEXP (X, 1), temp);		\
	  if (val != temp) emit_move_insn (temp, val);			\
	  XEXP (X, 1) = temp;						\
	  goto WIN; }							\
      else if (GET_CODE (XEXP (X, 1)) == REG)				\
	{ register rtx temp = gen_reg_rtx (Pmode);			\
	  register rtx val = force_operand (XEXP (X, 0), temp);		\
	  if (val != temp) emit_move_insn (temp, val);			\
	  XEXP (X, 0) = temp;						\
	  goto WIN; }}}

#endif
