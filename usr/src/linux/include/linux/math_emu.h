#ifndef _LINUX_MATH_EMU_H
#define _LINUX_MATH_EMU_H

struct fpu_reg {
	char sign;
	char tag;
	long exp;
	unsigned sigl;
	unsigned sigh;
};

struct info {
	long ___orig_eip;
	long ___ret_from_system_call;
	long ___ebx;
	long ___ecx;
	long ___edx;
	long ___esi;
	long ___edi;
	long ___ebp;
	long ___eax;
	long ___ds;
	long ___es;
	long ___fs;
	long ___gs;
	long ___orig_eax;
	long ___eip;
	long ___cs;
	long ___eflags;
	long ___esp;
	long ___ss;
};

#if 0
#define EAX (info->___eax)
#define EBX (info->___ebx)
#define ECX (info->___ecx)
#define EDX (info->___edx)
#define ESI (info->___esi)
#define EDI (info->___edi)
#define EBP (info->___ebp)
#define ESP (info->___esp)
#define EIP (info->___eip)
#define ORIG_EIP (info->___orig_eip)
#define EFLAGS (info->___eflags)
#define DS (*(unsigned short *) &(info->___ds))
#define ES (*(unsigned short *) &(info->___es))
#define FS (*(unsigned short *) &(info->___fs))
#define CS (*(unsigned short *) &(info->___cs))
#define SS (*(unsigned short *) &(info->___ss))
#endif

void __math_abort(struct info *, unsigned int);

#define math_abort(x,y) \
(((volatile void (*)(struct info *,unsigned int)) __math_abort)((x),(y)))

#endif
