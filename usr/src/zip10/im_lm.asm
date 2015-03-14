;
; Copyright (C) 1990,1991 Mark Adler, Richard B. Wales, and Jean-loup Gailly.
; Permission is granted to any individual or institution to use, copy, or
; redistribute this software so long as all of the original files are included
; unmodified, that it is not sold for profit, and that this copyright notice
; is retained.
;

; im_lm.asm by Jean-loup Gailly.

;im_lm.asm, optimized version of longest_match() and lm_process() in im_lmat.c
; Must be assembled with masm -ml. To be used only with C small model or
; compact model. This file is only optional. If you don't have masm, use the
; C version (add -DNO_ASM to CFLAGS in makefile.dos and remove im_lm.obj
; from OBJI).
;
; Turbo C 2.0 does not support static allocation of far data in
; the small model. So TC 2.0 users must assemble this with:
;   tasm -ml -DDYN_ALLOC im_lm;
; OS/2 users must assemble this with -DOS2 -ml. To simplify the code,
; the option -DDYN_ALLOC is not supported for OS/2.
        name    im_lm

ifndef DYN_ALLOC
        extrn   _prev         : word
        extrn   _next         : word
        prev    equ  _prev
        next    equ  _next
load_es_prev macro
        mov     cx,seg _prev
        mov     es,cx
endm
load_ds_next macro
        mov     si,seg _next
        mov     ds,si
endm
endif

_BSS    segment  word public 'BSS'
        extrn   _window       : byte
        extrn   _match_length : word
        extrn   _start_length : word
        extrn   _strstart     : word
        extrn   _strsize      : word
        extrn   _ins_h        : word
        extrn   _h_shift      : byte
        extrn   _checkpoint   : word
        extrn   _bufsize      : word
        extrn   _max_chain_length : word
        extrn   _min_match_length : word
ifdef DYN_ALLOC
        extrn   _prev         : word
        extrn   _next         : word
        prev    equ 0         ; offset normalized to zero (except for OS/2)
        next    equ 0         ; offset normalized to zero (except for OS/2)
load_es_prev macro
        mov     es,_prev[2]   ; warning: the offset should be added under OS/2
endm
load_ds_next macro
        mov     ds,_next[2]   ; warning: the offset should be added under OS/2
endm
endif
        cur_best dw 1 dup(?)  ; best match within longest_match
_BSS    ends

DGROUP  group _BSS

_TEXT   segment word public 'CODE'
        assume  cs: _TEXT, ds: DGROUP, ss: DGROUP

        extrn   _write_match : near

        BSZ          equ 4096           ; keep consistent with tailor.h
        WSIZE        equ 8192           ; keep consistent with im_lmat.c
        NIL          equ (WSIZE+BSZ)
        MAX_DIST     equ NIL
        MAX_LENGTH   equ 320            ; keep consistent with im_lmat.c
        HASH_MASK    equ 16383          ; (1<<14)-1

; Process a block of characters already inserted in the window
; IN assertion: count > 0
        public _lm_process
_lm_process  proc near
        push    bp
        mov     bp,sp
        push    di
        push    si

        count   equ word ptr [bp+4]
;       delete_point equ ax
;       ins_h        equ bx
;       h_shift      equ cl
;       count        equ dx
;       cur_match    equ si
;       strstart     equ di
;       min_match_length equ bp

        mov     dx,count
        mov     bx,_ins_h
        mov     di,_strstart
        lea     ax,[di+MAX_LENGTH-1]
        sub     ax,_bufsize
        jae     del_ok
        add     ax,MAX_DIST
del_ok: shl     ax,1                    ;delete_point as word index
        load_es_prev
        mov     cl,_h_shift
        load_ds_next
        assume  ds: nothing
        jmp     short insert

start_overflow:
        sub     di,di                   ;strstart = 0
        sub     ss:_checkpoint,MAX_DIST
        jmp     short check_count
del_overflow:
        mov     ax,-2                   ;delete_point = 0
main_loop:
        add     ax,2                    ;delete_point++ (word index)
        cmp     ax,2*MAX_DIST
        je      del_overflow
        xchg    ax,si                   ;si=2*delete_point
        mov     bp,next[si]
        shl     bp,1
        mov     es:prev[bp],NIL
        xchg    ax,si                   ;ax=2*delete_point

        inc     di                      ;strstart++
        cmp     di,MAX_DIST
        je      start_overflow
check_count:
        dec     dx                      ;count--
        jz      end_proc_ok
insert:
        shl     bx,cl                   ;ins_h <<= h_shift
        mov     bp,ss:_min_match_length
        xor     bl,_window[di+bp-1]     ;ins_h ^= window[s+min_match_length-1]
        and     bx,HASH_MASK
        add     bx,MAX_DIST+1
        shl     bx,1                    ;word index
        mov     si,es:prev[bx]          ;cur_match = match_head[ins_h]
        mov     es:prev[bx],di          ;prev[ins_h+MAX_DIST+1] = strstart
        shr     bx,1                    ;bx = ins_h + MAX_DIST + 1
        shl     di,1                    ;di = strstart as word index
        mov     next[di],bx
        sub     bx,MAX_DIST+1           ;bx = ins_h
        mov     es:prev[di],si          ;prev[s] = cur_match
        shr     di,1                    ;di = strstart
        shl     si,1
        mov     next[si],di             ;next[cur_match] = strstart
        cmp     di,ss:_checkpoint
        jne     main_loop

        push    ax
        push    bx
        push    dx
        mov     ax,ss
        mov     ds,ax
        assume  ds: DGROUP
        mov     _match_length,0
        mov     _strstart,di
        shr     si,1                    ;si = cur_match
        cmp     si,NIL                  ;cur_match == NIL ?
        je      call_write
        call    _longest_match          ;returns best_match in si
call_write:
        push    _match_length
        push    si                      ;best_match or NIL
        call    _write_match
        add     sp,4
        pop     dx
        pop     bx
        or      al,al
        jnz     failure
        pop     ax
        load_es_prev
        mov     cl,_h_shift
        load_ds_next
        assume  ds: nothing
        jmp     main_loop
end_proc_ok:
        mov     ax,ss
        mov     ds,ax
        assume  ds: DGROUP
        sub     ax,ax
        mov     _strstart,di
end_proc:
        mov     _ins_h,bx
        pop     si
        pop     di
        pop     bp
        ret
failure:
        add     sp,2                    ;skip pushed ax
        jmp     short end_proc

_lm_process  endp

; Find the longest match starting at the given string. Return its position
; and set its length in match_length. Matches shorter or equal to
; start_length are discarded, in which case match_length is unchanged
; and the result position is NIL.
; IN assertions: cur_match is the head of the hash chain for the current
;    string (strstart) and is not NIL, start_length >= 1.
;    registers: es points to the base of the prev array (preserved)
;               si = cur_match and return value
;               di = strstart

; IPos longest_match(cur_match)

        public _longest_match
_longest_match  proc near

        push    bp
        push    di

;       match        equ si
;       scan         equ di
;       chain_count  equ bp
;       ma_length    equ bx

        lea     di,_window[di+2]        ; di = window + strstart + 2
        mov     cur_best,NIL
        mov     bp,_max_chain_length    ; chain_count = max_chain_length
        mov     bx,_start_length        ; ma_length = start_length
        mov     ax,[bx+di-3]            ; ax = scan[ma_length-1..ma_length]
        mov     cx,[di-2]               ; cx = scan[0..1]
        jmp     short do_scan

        even                            ; align destination of branch
long_loop:
; at this point, di == scan+2, si = cur_match
        mov     ax,[bx+di-3]            ; ax = scan[ma_length-1..ma_length]
        mov     cx,[di-2]               ; cx = scan[0..1]
short_loop:
        dec     bp                      ; --chain_count
        jz      the_end
; at this point, di == scan+2, si = cur_match,
; ax = scan[ma_length-1..ma_length] and cx = scan[0..1]
        shl     si,1                    ; cur_match as word index
        mov     si,es:prev[si]          ; cur_match = prev[cur_match]
        cmp     si,NIL
        je      the_end
do_scan:
        cmp     ax,word ptr _window[bx+si-1] ; check match at ma_length-1
        jne     short_loop
        cmp     cx,word ptr _window[si]      ; check min_match_length match
        jne     short_loop
        mov     dx,si                   ; dx = cur_match
        lea     si,_window[si+2]        ; si = match
        mov     ax,di                   ; ax = scan+2
        mov     cx,ds
        mov     es,cx
        mov     cx,(MAX_LENGTH-2)/2     ; scan for at most MAX_LENGTH bytes
        repe    cmpsw                   ; loop until mismatch
        load_es_prev                    ; reset es to address the prev array
        mov     cl,[di-2]               ; mismatch on first or second byte?
        sub     cl,[si-2]               ; dl = 0 if first bytes equal
        xchg    ax,di                   ; di = scan+2, ax = end of scan
        sub     ax,di                   ; ax = len
        sub     cl,1                    ; set carry if cl == 0 (can't use DEC)
        adc     ax,0                    ; ax = carry ? len+1 : len
        mov     si,dx                   ; si = cur_match
        cmp     ax,bx                   ; len > ma_length ?
        jle     long_loop
        mov     cur_best,si             ; cur_best = cur_match
        mov     bx,ax                   ; bx = ma_length = len
        cmp     ax,_strsize             ; len >= strsize ?
        jle     long_loop
the_end:
        cmp     bx,_start_length        ; ma_length > start_length ?
        jle     return
        mov     _match_length,bx        ; match_length = ma_length
return:
        mov     si,cur_best             ; result = cur_best
        pop     di
        pop     bp
        ret

_longest_match  endp

_TEXT   ends
end
