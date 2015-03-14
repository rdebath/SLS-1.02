/* Print mips instructions for GDB, the GNU debugger, or for objdump.
   Copyright 1989, 1991, 1992 Free Software Foundation, Inc.
   Contributed by Nobuyuki Hikichi(hikichi@sra.co.jp).

This file is part of GDB.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <ansidecl.h>
#include "sysdep.h"
#include <stdio.h>
#include "bfd.h"
#include "opcode/mips.h"

/* FIXME: we need direct access to the swapping functions.  */
#include "libbfd.h"

/* We use bfd_vma in a couple of places where gdb expects CORE_ADDR.  */
#ifdef HOST_64_BIT
 #error FIXME: bfd_vma will not match gdb expectations
#endif

/* This file is used both by gdb and by objdump.  A program which
   wants to use this code must provide an external function
   print_address.  */
extern int print_address PARAMS ((bfd_vma, FILE *));

/* Mips instructions are never longer than this many bytes.  */
#define MAXLEN 4

/* Number of elements in the opcode table.  */
#define NOPCODES (sizeof mips_opcodes / sizeof mips_opcodes[0])

/* FIXME: This should be shared with gdb somehow.  */
#define REGISTER_NAMES 	\
    {	"zero",	"at",	"v0",	"v1",	"a0",	"a1",	"a2",	"a3", \
	"t0",	"t1",	"t2",	"t3",	"t4",	"t5",	"t6",	"t7", \
	"s0",	"s1",	"s2",	"s3",	"s4",	"s5",	"s6",	"s7", \
	"t8",	"t9",	"k0",	"k1",	"gp",	"sp",	"s8",	"ra", \
	"sr",	"lo",	"hi",	"bad",	"cause","pc",    \
	"f0",   "f1",   "f2",   "f3",   "f4",   "f5",   "f6",   "f7", \
	"f8",   "f9",   "f10",  "f11",  "f12",  "f13",  "f14",  "f15", \
	"f16",  "f17",  "f18",  "f19",  "f20",  "f21",  "f22",  "f23",\
	"f24",  "f25",  "f26",  "f27",  "f28",  "f29",  "f30",  "f31",\
	"fsr",  "fir",  "fp",   "inx",  "rand", "tlblo","ctxt", "tlbhi",\
	"epc",  "prid"\
    }

static CONST char * CONST reg_names[] = REGISTER_NAMES;

/* subroutine */
static void
print_insn_arg (d, l, stream, pc)
     char *d;
     register unsigned long int l;
     FILE *stream;
     bfd_vma pc;
{
  switch (*d)
    {
    case ',':
    case '(':
    case ')':
      fputc (*d, stream);
      break;

    case 's':
      fprintf (stream, "$%s", reg_names[(l >> OP_SH_RS) & OP_MASK_RS]);
      break;

    case 't':
      fprintf (stream, "$%s", reg_names[(l >> OP_SH_RT) & OP_MASK_RT]);
      break;

    case 'i':
      fprintf (stream, "%d", (l >> OP_SH_IMMEDIATE) & OP_MASK_IMMEDIATE);
      break;

    case 'j': /* same as i, but sign-extended */
      fprintf (stream, "%d", (l >> OP_SH_DELTA) & OP_MASK_DELTA);
      break;

    case 'a':
      print_address (((pc & 0xF0000000)
		      | (((l >> OP_SH_TARGET) & OP_MASK_TARGET) << 2)),
		     stream);
      break;

    case 'b':
      print_address ((((l >> OP_SH_DELTA) & OP_MASK_DELTA) << 2) + pc + 4,
		     stream);
      break;

    case 'd':
      fprintf (stream, "$%s", reg_names[(l >> OP_SH_RD) & OP_MASK_RD]);
      break;

    case 'h':
      fprintf (stream, "0x%x", (l >> OP_SH_SHAMT) & OP_MASK_SHAMT);
      break;

    case 'B':
      fprintf (stream, "0x%x", (l >> OP_SH_CODE) & OP_MASK_CODE);
      break;

    case 'S':
      fprintf (stream, "$f%d", (l >> OP_SH_FS) & OP_MASK_FS);
      break;

    case 'T':
      fprintf (stream, "$f%d", (l >> OP_SH_FT) & OP_MASK_FT);
      break;

    case 'D':
      fprintf (stream, "$f%d", (l >> OP_SH_FD) & OP_MASK_FD);
      break;

    default:
      fprintf (stream, "# internal error, undefined modifier(%c)", *d);
      break;
    }
}

/* Print the mips instruction at address MEMADDR in debugged memory,
   on STREAM.  Returns length of the instruction, in bytes, which is
   always 4.  BIGENDIAN must be 1 if this is big-endian code, 0 if
   this is little-endian code.  */

int
print_insn_mips (memaddr, buffer, stream, bigendian)
     bfd_vma memaddr;
     bfd_byte *buffer;
     FILE *stream;
     int bigendian;
{
  register int i;
  register char *d;
  unsigned long int l;

  /* FIXME: can't we export these functions from bfd?  */
  if (bigendian)
    l = _do_getb32 (buffer);
  else
    l = _do_getl32 (buffer);

  for (i = 0; i < NOPCODES; i++)
    {
      register unsigned int opcode = mips_opcodes[i].opcode;
      register unsigned int match = mips_opcodes[i].match;
      if ((l & match) == opcode)
	break;
    }

  /* Handle undefined instructions.  */
  if (i == NOPCODES)
    {
      fprintf (stream, "0x%x",l);
      return 4;
    }

  fprintf (stream, "%s", mips_opcodes[i].name);

  if (!(d = mips_opcodes[i].args))
    return 4;

  fputc (' ', stream);

  while (*d)
    print_insn_arg (d++, l, stream, memaddr);

  return 4;
}
