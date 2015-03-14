/* $Header: /home/x_cvs/mit/server/ddx/x386/vga256/vga/vgaAsm.h,v 1.4 1992/08/29 11:14:24 dawes Exp $ */

/* Definitions for VGA bank assembler routines */

#ifdef __386BSD__
#define VGABASE $0xFF000000
#else
#define VGABASE $0xF0000000
#endif

#if !defined(SYSV) && !defined(SVR4)
#define vgaSetReadWrite _vgaSetReadWrite
#define vgaReadWriteNext _vgaReadWriteNext
#define vgaReadWritePrev _vgaReadWritePrev
#define vgaSetRead _vgaSetRead
#define vgaReadNext _vgaReadNext
#define vgaReadPrev _vgaReadPrev
#define vgaSetWrite _vgaSetWrite
#define vgaWriteNext _vgaWriteNext
#define vgaWritePrev _vgaWritePrev
#define vgaSaveBank _vgaSaveBank
#define vgaRestoreBank _vgaRestoreBank
#define vgaPushRead _vgaPushRead
#define vgaPopRead _vgaPopRead
#define vgaSegmentShift _vgaSegmentShift
#define vgaSegmentMask _vgaSegmentMask
#define vgaSegmentSize _vgaSegmentSize
#define vgaSetReadFunc _vgaSetReadFunc
#define vgaSetWriteFunc _vgaSetWriteFunc
#define vgaSetReadWriteFunc _vgaSetReadWriteFunc
#define vgaReadBottom _vgaReadBottom
#define vgaReadTop _vgaReadTop
#define vgaWriteBottom _vgaWriteBottom
#define vgaWriteTop _vgaWriteTop
#endif
