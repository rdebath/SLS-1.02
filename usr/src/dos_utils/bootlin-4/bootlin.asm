 
		NAME	BootLinux 
 
MainSeg		SEGMENT PUBLIC 'TEXT' 
		ASSUME cs:MainSeg, ds:MainSeg, es:MainSeg 
 
		ORG 0100h		; it is going to be a COM file 
 
FileNameSize	EQU 80 
StackSize	EQU 2048 
PlusSize	EQU FileNameSize+StackSize 
PageLength	EQU 4000h		; MUST be a power of 2 
KernelStart	EQU 1000h 
KernelEnd	EQU 9000h 
BootlinMove	EQU 9800h 
 
JumpStart	PROC FAR 
		jmp near ptr start 
JumpStart	ENDP 
 
		ASSUME cs:MainSeg, ds:NOTHING, es:NOTHING 
NewInt13	PROC FAR 
oldint		dd 0 
pageToMove	dw 0 
pageToClean	dw 0 
 
newint:		cmp ax,1500h 
		je nobypass 
		jmp dword ptr cs:oldint 
 
nobypass:	push bp 
		mov bp,sp 
		push word ptr [bp+6] 
		call dword ptr cs:oldint 
		pop bp 
		cli 
		pushf 
		push ds 
		push es 
		push si 
		push di 
		push ax 
		push bx 
		push cx 
		push dx 
 
		mov si,cs:pageToMove 
		mov di,KernelStart 
		jmp short endmove 
 
domove:		call near ptr MovePage 
endmove:	cmp si,KernelEnd 
		jne domove 
 
		mov di,cs:pageToClean 
		jmp short endclean 
 
doclean:	call near ptr CleanPage 
		mov di,dx 
endclean:	cmp di,KernelEnd 
		jne doclean 
 
		pop dx 
		pop cx 
		pop bx 
		pop ax 
		pop di 
		pop si 
		pop es 
		pop ds 
		popf 
		ret 2 
NewInt13	ENDP 
 
		ASSUME cs:MainSeg, ds:NOTHING, es:NOTHING 
CleanPage	PROC NEAR		; di = segment of page to fill 
		push es 
		push ax 
		push cx 
		mov es,di 
		xor ax,ax 
		mov di,ax 
		mov cx,(PageLength SHR 1) 
		cld 
		rep stosw 
		mov di,es 
		lea dx,[di][PageLength SHR 4] 
		pop cx 
		pop ax 
		pop es 
		ret 
CleanPage	ENDP			; returns di unchanged, dx on next page 
 
		ASSUME cs:MainSeg, ds:NOTHING, es:NOTHING 
MovePage	PROC NEAR		; moves page at si:0 to page at di:0 
		push ds 
		push es 
		push cx 
		mov ds,si 
		mov es,di 
		xor si,si 
		xor di,di 
		mov cx,(PageLength SHR 1) 
		cld 
		rep movsw 
		mov si,ds 
		mov di,es 
		add si,(PageLength SHR 4) 
		add di,(PageLength SHR 4) 
		pop cx 
		pop es 
		pop ds 
		ret 
MovePage	ENDP			; returns di and si on next pages 
 
		ASSUME cs:MainSeg, ds:MainSeg, es:MainSeg 
TheStart	PROC FAR 
start: 
		mov ah,9 
		lea dx,Copyright 
		int 21h 
 
		lea bx,TheEnd 
		add bx,PlusSize		; compute start of the new stack 
 
		cli 
		push cs 
		pop ss 
		mov sp,bx 
		sti			; sets the new stack 
 
		mov cl,4 
		dec bx 
		shr bx,cl 
		inc bx 
		mov ax,ss 
		add ax,bx		; compute the segment after the stack 
 
		cmp ax,KernelStart	; and check it doesn't overlap 
		jge moreComplicated 
		 
		call near ptr CleanMemory 
		call near ptr GetFileName	; get filename from command line 
		mov ax,KernelEnd 
		call near ptr ReadLinux		; loads kernel in memory 
		jmp short RunLinux 
 
moreComplicated: 
		add ax,(PageLength SHR 4) 
		mov bx,ax 
		and ax,((PageLength-1) SHR 4) 
		xor bx,ax 
		sub bx,KernelStart 
		mov ax,KernelEnd 
		sub ax,bx 
		mov pageToMove,ax 
		push ax 
		mov ax,3513h 
		int 21h 
		mov word ptr [oldint],bx 
		mov word ptr [oldint+2],es 
		call near ptr GetFileName	; get filename from command line 
		pop ax 
		call near ptr ReadLinux 
		mov pageToClean,ax 
		mov si,cs 
		mov di,BootlinMove 
		push ds 
		push di 
		call near ptr MovePage 
		pop ds 
		lea dx,newint 
		mov ax,2513h 
		int 21h 
		pop ds 
 
RunLinux: 
		mov ax,9000h		; and now, serious things begin ! 
		mov ds,ax 
		mov bx,200h-4 
		mov ax,word ptr [bx]	; fetch root device 
 
		cmp ax,0		; check for non NULL device number 
		jne DoIt 
 
		push bx			; the ROOT is not defined 
		push ds			; so let's ask it ! 
		push cs 
		pop ds 
 
		mov ah,9 
		lea dx,RootMsg 
		int 21h 
 
TheKey:		mov ah,0 
		int 16h 
		sub al,'1' 
		cmp al,'4'-'1' 
		jle DevNum		; jump if '1'<=key<='4' ; al=0..3 
		sub al,'A'-'1' 
		cmp al,'a'-'A'		; capitalization 
		jl DevAlp 
		sub al,'a'-'A' 
DevAlp:		cmp al,'H'-'A' 
		jg TheKey		; pass if 'A'<=CAP(key)<='H' ; al=0..7 
		add al,'5'-'1'		; translate al to 4..11 
DevNum:		add al,al 
		mov ah,0 
		lea bx,DeviceTable 
		add bx,ax 
		mov ax,word ptr [bx]	; now we have the ROOT 
 
		pop ds		; so we save it (for posterity) 
		pop bx		; in the bootsector 
		mov word ptr [bx],ax 
 
DoIt:		cli 
		mov bx,9000h 
		mov ds,bx 
		mov es,bx 
		mov ss,bx 
		mov sp,4000h		; the setup stack ; no space for 
		sti			; disk param table, because no need 
 
		mov bx,9020h	 	; here I made a mistake last time 
		push bx 
		xor bx,bx 
		push bx 
		ret 
TheStart	ENDP 
 
		ASSUME cs:MainSeg, ds:MainSeg, es:MainSeg 
ReadLinux	PROC NEAR 
		push ax 
TryAgain: 
		mov ax,03d00h		; tries to open the specified file 
		lea dx,filename		; in READ-ONLY mode 
		int 21h 
		jnc OpenIsOk 
 
		lea dx,BadFileMsg	; fail: print error msg and 
		mov ah,9 
		int 21h 
		call near ptr AskFileName	; asks for another file name 
		jmp short TryAgain	; and try again until it succeeds 
 
OpenIsOk:	mov word ptr filehandle,ax	; save file handle 
 
		mov ah,9 
		lea dx,BootMsg 
		int 21h			; bootsect load message 
 
		mov di,9000h		; target:= 9000:0 
		mov cx,200h		; size:= 1 sector = 512 bytes 
		call near ptr sectread	; reads boot sector 
		jne Image		; fail => bad length 
 
		call near ptr bootcheck	; checks for 0AA55h at the end 
		jne BootError 
 
		mov ah,9 
		lea dx,SetupMsg 
		int 21h			; setup load message 
 
		mov di,9020h		; target:= 9000:200 
		mov cx,800h		; size:= 4 sectors = 2K 
		call near ptr sectread	; reads setup 
		jne Image		; fail => bad length 
 
		mov ah,9 
		lea dx,KernelMsg 
		int 21h			; kernel load message 
 
		pop di			; start kernel loading at ????:0 
		mov cx,PageLength	; read it by pages 
		mov si,KernelStart	; virtual loading address 
		mov dx,di		; saves start page for future compare 
 
kernrd:		push dx 
		push si 
		cmp di,KernelEnd 
		jne readpage 
		mov di,si 
readpage:	call near ptr CleanPage 
		push dx 
		call near ptr sectread	; reads the page 
 
		pushf			; save useful registers 
		push cx 
		mov ah,2 
		mov dl,'.' 
		int 21h			; print one dot 
		pop cx 
		popf			; and restore 
 
		pop di 
		pop si 
		pop dx 
 
		jnz close		; if the whole page has been read, 
					; continue, else close file 
		add si,(PageLength SHR 4) 
		cmp si,dx 
		je NotEnoughMemory 
		jmp short kernrd 
 
prnt:		push cs 
		pop ds 
		mov ah,9 
		int 21h 
bcl:		jmp short bcl 
 
BootError: 
		lea dx,CheckMsg 
		jmp short prnt 
 
NotEnoughMemory: 
		lea dx,MemoryMsg 
		jmp short prnt 
 
Image:		lea dx,ImageMsg 
		jmp short prnt 
 
Error:		lea dx,ErrorMsg 
		jmp short prnt 
 
close:		push si 
		mov ah,3eh 
		mov bx,word ptr filehandle 
		int 21h			; otherwise, close the file 
 
		mov ah,9 
		lea dx,DoneMsg 
		int 21h			; done message 
 
		pop ax 
		add ax,(PageLength SHR 4) 
		ret 
ReadLinux	ENDP 
 
		ASSUME ds:NOTHING 
sectread	PROC NEAR		; tries to read cx bytes at di:0 
		push ds			; returns ZF set if all bytes read 
		push bx 
		push cx 
		mov ds,di 
		mov bx,cs:word ptr filehandle 
		xor dx,dx 
		mov ah,03fh 
		int 21h 
		mov di,ds 
		pop cx 
		pop bx 
		pop ds 
		jc dumbError 
		cmp ax,cx 
		ret 
dumbError:	jmp Error 
sectread	ENDP 
 
CleanMemory	PROC NEAR 
		push dx 
		push di 
		mov di,KernelStart 
goonclean:	call near ptr CleanPage 
		mov di,dx 
		cmp di,KernelEnd 
		jne goonclean 
		pop di 
		pop dx 
CleanMemory	ENDP 
 
		ASSUME ds:NOTHING 
bootcheck	PROC NEAR 
		push ds 
		push bx 
		mov ax,9000h 
		mov ds,ax 
		mov bx,01FEh 
		mov ax,[bx] 
		pop bx 
		pop ds 
		cmp ax,0AA55h 
		ret 
bootcheck	ENDP 
 
		ASSUME ds:MainSeg 
AskFileName	PROC NEAR 
		lea bx,filename 
		push bx 
		xor cx,cx 
checkEnd:	cmp byte ptr [bx],0 
		je proceed 
		cmp cx,FileNameSize-1 
		je proceed 
		inc cx 
		inc bx 
		jmp short checkEnd 
proceed:	mov byte ptr [bx],'$' 
		pop dx 
		mov ah,9 
int21call:	push bx 
		push cx 
		int 21h 
		pop cx 
		pop bx 
input:		call near ptr Inkey 
		cmp ax,127 
		jge input 
		cmp ax,8 
		je backspace 
		cmp ax,13 
		je return 
		cmp ax,31 
		jle input 
		cmp cx,FileNameSize-1 
		jge beep 
		mov byte ptr [bx],al 
		inc bx 
		inc cx 
		mov dl,al 
prchar:		mov ah,2 
		jmp short int21call 
beep:		mov dl,7 
		jmp short prchar 
backspace:	jcxz beep 
		dec bx 
		dec cx 
		push bx 
		push cx 
		mov ah,2 
		mov dl,8 
		int 21h 
		mov dl,' ' 
		int 21h 
		pop cx 
		pop bx 
		mov dl,8 
		jmp short prchar 
return:		mov byte ptr [bx],0 
		mov ah,9 
		lea dx,SomeCrLfs 
		int 21h 
		ret 
AskFileName	ENDP 
 
Inkey		PROC NEAR 
		push bx 
		push cx 
		mov ah,7 
		int 21h 
		mov ah,0 
		cmp al,0 
		jne endkey 
		mov ah,0bh 
		int 21h 
		mov ah,0 
		cmp al,0 
		je endkey 
		mov ah,7 
		int 21h 
		mov ah,1 
endkey:		pop cx 
		pop bx 
		ret 
Inkey		ENDP 
		 
GetFileName	PROC NEAR		; copy first argument to filename 
		mov ax,cs 
		mov ds,ax 
		mov es,ax 
		mov si,0080h		; start of command line parameters 
		lea di,filename 
		mov ch,0 
		mov cl,byte ptr [si] 
		inc si 
		cld 
		jcxz stoploop 
startloop:	lodsb 
		cmp al,' ' 
		je endloop 
		stosb 
endloop:	loop startloop 
stoploop:	mov al,0		; make it in ASCIIZ format 
		stosb 
		ret 
GetFileName	ENDP 
 
 
Copyright	db 13,25 dup (10) 
		db "BOOTLINUX v1.3 (C) 1992 F.COUTANT" 
SomeCrLfs	db 13,10,10,"$" 
BadFileMsg	db "Could not open Image file. Please enter new file name" 
		db 13,10,"or press Ctrl-Alt-Del to reboot:",13,10,"$" 
MemoryMsg	db "BootLinux is loaded too high in memory",13,10,"$" 
BootMsg		db "BootSector: ... $" 
CheckMsg	db "not a boot-disk image !$" 
SetupMsg	db "loaded",13,10,"LINUX Setup: ... $" 
KernelMsg	db "loaded",13,10,"LINUX Kernel: $" 
ImageMsg	db "bad image length$" 
ErrorMsg	db " error during read !$" 
DoneMsg		db " done",13,10,10,"$" 
RootMsg		db "ROOT device is not defined. Please choose it:",13,10,10 
		db "	[1] at0 	[3] at1",13,10 
		db "	[2] ps0 	[4] ps1",13,10,10 
		db "	[A] hda1	[E] hdb1",13,10 
		db "	[B] hda2	[F] hdb2",13,10 
		db "	[C] hda3	[G] hdb3",13,10 
		db "	[D] hda4	[H] hdb4",13,10,10,"$" 
 
DeviceTable	dw 0208h, 021Ch, 0209h, 021Dh 
		dw 0301h, 0302h, 0303h, 0304h 
		dw 0341h, 0342h, 0343h, 0344h 
 
filehandle	dw 0 
filename: 
 
TheEnd		LABEL BYTE 
 
MainSeg		ENDS 
 
 
		END MainSeg:JumpStart 
 
