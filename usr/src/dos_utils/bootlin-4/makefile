AS	= masm 
ASFLAGS	= /Ml /z /t 
LD	= tlink 
LDFLAGS	= /x/c/t 
 
all:	bootlin.com 
 
bootlin.com:	bootlin.obj 
	$(LD) $(LDFLAGS) bootlin.obj, bootlin.com,, 
 
bootlin.obj:	bootlin.asm 
	$(AS) $(ASFLAGS) bootlin.asm; 
