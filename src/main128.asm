

	incdir data_bin/

	include parameters.asm
	
	output Scrll8way128.rom

	defpage	0,0x4000, 0x2000		; page 0 contains main code + far call routines
	defpage 1,0x6000, 0x2000		; static code 
	defpage	2,0x8000, 0x2000		; static code
	
	defpage	3,0xA000, 0x2000		; 
	defpage	4,0xA000, 0x2000		; 
	defpage	5,0xA000, 0x2000		; sprites
	
	defpage	6,0xA000, 0x2000		; meta map
	defpage	7,0xA000, 0x2000		; metavec 
	
	defpage	 8,0xA000, 0x2000		; patterns_base
	defpage	 9,0xA000, 0x2000		; patterns_base
	defpage	10,0xA000, 0x2000		; patterns_base
	defpage	11,0xA000, 0x2000		; patterns_base
	defpage	12,0xA000, 0x2000		; colors_base
	defpage	13,0xA000, 0x2000		; colors_base
	defpage	14,0xA000, 0x2000		; colors_base
	defpage	15,0xA000, 0x2000		; colors_base

	page 6	
metamap:
	incbin metamap.bin

	page 7
metavec:
	incbin metavec.bin

	page 8
patterns_base:	
patterns11_13_15_17:
	incbin testpat.bin,00*2*1024,4*2*1024	
	page 9
patterns31_33_35_37:
	incbin testpat.bin,04*2*1024,8*1024	
	page 10
patterns51_53_55_57:
	incbin testpat.bin,08*2*1024,8*1024	
	page 11
patterns71_73_75_77:
	incbin testpat.bin,12*2*1024,8*1024	
	
	page 12
colors_base:
colors11_13_15_17:
	incbin testcol.bin,00*2*1024,4*2*1024	
	page 13
colors31_33_35_37:
	incbin testcol.bin,04*2*1024,8*1024	
	page 14
colors51_53_55_57:
	incbin testcol.bin,08*2*1024,8*1024	
	page 15
colors71_73_75_77:
	incbin testcol.bin,12*2*1024,8*1024	




	macro setVdp register,value       ; macro definition
	di
	ld	a,value
	out	(0x99),a
	ld	a,register
	or	0x80
	out	(0x99),a
	ei
	endmacro
  
  	macro setvdpwvram value
  	di			
	if (value & 0xFF)
		ld	a,value & 0xFF
	else
		xor	a
	endif
	out (0x99),a
	ld	a,0x40 + (value/256)
	out (0x99),a
	ei
	endmacro
; -----------------------------
; smooth scroller demo
; Trilobyte 2014
; ------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   Mapper Konami 5 (mapper +  scc)
;
; Bank 1: 4000h - 5FFFh
; Bank 2: 6000h - 7FFFh
; Bank 3: 8000h - 9FFFh
; Bank 4: A000h - BFFFh

; Bank 1: 5000h - 57FFh (5000h used)
; Bank 2: 7000h - 77FFh (7000h used)
; Bank 3: 9000h - 97FFh (9000h used)
; Bank 4: B000h - B7FFh (B000h used)


; *** CONSTANTS ***
_bank1			equ	0x5000
_bank2			equ	0x7000
_bank3			equ	0x9000
_bank4			equ	0xB000

_PCT:			equ	0x0000
_PGT:			equ	0x2000


; -----------------------------
; parameters
; 
;	LvlWidth:	equ	373
;	nphase:		equ	4 
;	xstep:		equ	2 
;

;		
maxspeed:   	equ 8*16
maxspeedstep:	equ	1
;
;minspeed:	equ	1		i.e. 	1/16

; ------------------------------

; *** RAM ***

	map 0xc000

buffer:			#8*1024


slotvar:		#1
slotram:       	#1

msxtype			#1
palette			#1

vsf:			#1
cnt:			#1
muteflag:		#1

SLOT            #1
PAGE1RAM        #1
RAMSLOT         #1

SCC				#1
SUB             #1

xmap			#2		; FP 8.8
ymap			#2		; FP 8.8

dxmap           #1		; FP 4.4
dymap           #1		; FP 4.4

phase			#1


	endmap


	
	
; *** tile set data in rom ***


	page 5
test_spt:
	incbin spt.bin

;	place 32 sprites
test_sat:
counter:=0
	repeat	8
	repeat	4
	db	@@# * 24
	db	48+@# * 32+@@#*8
	db	6*4+(counter and %00001100)
	db	15
counter:=counter+4
	endrepeat
	endrepeat
; ------------
; megarom header

	page 0
	code page 0
	
	org	04000h
	db	041h,042h
	dw	initmain
	dz 	'TRI004'
	ds	5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; checkkbd: ckeck keyboard line
; in:  E = row
; out: L = bit pattern (1 = not pressed)
;
; row/bit|    7     6     5     4     3     2     1     0
; -------+--------------------------------------------------
;    0   |    7     6     5     4     3     2     1     0
;    1   |    ;     ]     [     \     =     -     9     8
;    2   |    B     A    ???    /     .     ,     '     `
;    3   |    J     I     H     G     F     E     D     C
;    4   |    R     Q     P     O     N     M     L     K
;    5   |    Z     Y     X     W     V     U     T     S
;    6   |   F3    F2    F1   CODE   CAP  GRAPH CTRL  SHIFT
;    7   |   RET   SEL   BS   STOP   TAB   ESC   F5    F4
;    8   |  RIGHT DOWN   UP   LEFT   DEL   INS  HOME  SPACE
;    9   |    4     3     2     1     0     /     +     *
;   10   |    .     ,     -     9     8     7     6     5
;   11   |                           NO          YES
;
;

i8255portb  equ 0a9h        ; keyboard column input
i8255portc  equ 0aah        ; leds, motor, cassette, kbd line

checkkbd:
    in  a,(i8255portc)
    and 011110000B          ; upper 4 bits contain info to preserve
    or  e
    out (i8255portc),a
    in  a,(i8255portb)
    ld  l,a
    ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a = value
; e = register
_setvdpreg:
	di
	out (0x99),a
	ld	a,e
	or	0x80
	out (0x99),a
	ei
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; vdp access

_vdpReg equ 0xF3DF
LINL40	equ 0xF3AE

; e = screen mode
_scr2:
	ld	a,(_vdpReg+1)
	or 2
	ld	(_vdpReg+1),a
	
	ld	a,2
	call 0x005f
	
	ret
	
; set sprites

set_sprites:
	halt
	setvdpwvram 0x3800
	ld	hl,test_spt
	ld	a,:test_spt
	call	write_256
	halt
	setvdpwvram 0x1B00
	ld	hl,test_sat
	ld	a,:test_sat
	ld	(_bank4),a	
	ld	bc,0x8098
	otir
	ret

write_256:
	ld	(_bank4),a
	ld	bc,0x0098
	repeat 8
	otir
	endrepeat
	ret

cls:
	setvdpwvram 0x1800
	ld	b,0
	ld	a,255
1:	out	(0x98),a
	dec	b
	jr	nz,1b
	
	setvdpwvram 0x2000
	ld	b,0
	xor	a
[8]	call	1f
	setvdpwvram 0x0000
	ld	b,0
	xor	a
[8]	call	1f
	ret

1:	out	(0x98),a
	dec	b
	jr	nz,1b
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;	load tile sets
setvramp1:
	call getbank	;->a 
	ld (_bank4),a
	call getaddr	;->hl
	call getsize	;->a
	ld	de,0x0800
	call write_2k
	
	call getaddr	;->hl
	call getsize	;->a
	ld	de,0x1000
	call write_2k

	call getbank	;->a 
	add	a,4			; colorbank = tilebank+4
	ld (_bank4),a
	call getaddr	;->hl
	call getsize	;->a
	ld	de,0x2000
	call write_2k
	ret
	
setvramp0:
	call getbank	;->a 
	ld (_bank4),a
	call getaddr	;->hl
	call getsize	;->a
	ld	de,0x2800
	call write_2k
	
	call getaddr	;->hl
	call getsize	;->a
	ld	de,0x3000
	call write_2k

	call getbank	;->a 
	add	a,4			; colorbank = tilebank+4
	ld (_bank4),a
	call getaddr	;->hl
	call getsize	;->a
	ld	de,0x0000
	call write_2k
	ret
	
getbank:
	ld	de,(phase)
	ld	hl,tilebank
	ld	d,0
	add	hl,de
	ld	a,(hl)
	ret
getaddr:
	ld	de,(phase)
	ld	hl,tileaddress
	ld	d,0
	add	hl,de
	ld	h,(hl)
	ld	l,0
	ret
getsize:
	push	hl
	ld	de,(phase)
	ld	hl,tilesize
	ld	d,0
	add	hl,de
	ld	a,(hl)
	pop	hl
	ret

	
	;	patternsxy
tilebank:
	db	:patterns11_13_15_17,:patterns11_13_15_17,:patterns11_13_15_17,:patterns11_13_15_17
	db	:patterns31_33_35_37,:patterns31_33_35_37,:patterns31_33_35_37,:patterns31_33_35_37
	db	:patterns51_53_55_57,:patterns51_53_55_57,:patterns51_53_55_57,:patterns51_53_55_57
	db	:patterns71_73_75_77,:patterns71_73_75_77,:patterns71_73_75_77,:patterns71_73_75_77
tilesize:	
	db	TileSize11,TileSize13,TileSize15,TileSize17
	db	TileSize31,TileSize33,TileSize35,TileSize37
	db	TileSize51,TileSize53,TileSize55,TileSize57
	db	TileSize71,TileSize73,TileSize75,TileSize77
tileaddress:	
	db	(patterns_base+0*2*1024)/256,(patterns_base+1*2*1024)/256,(patterns_base+2*2*1024)/256,(patterns_base+3*2*1024)/256
	db	(patterns_base+0*2*1024)/256,(patterns_base+1*2*1024)/256,(patterns_base+2*2*1024)/256,(patterns_base+3*2*1024)/256
	db	(patterns_base+0*2*1024)/256,(patterns_base+1*2*1024)/256,(patterns_base+2*2*1024)/256,(patterns_base+3*2*1024)/256
	db	(patterns_base+0*2*1024)/256,(patterns_base+1*2*1024)/256,(patterns_base+2*2*1024)/256,(patterns_base+3*2*1024)/256
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; write 2K while ints are active from hl to de
; in: de vram address
;     hl ram address
;	   a counter of 8 bytes chunks	
write_2k:
	push hl
	ex	de,hl
	set	6,h
	ld	c,0x99
	ld	de,8
	exx
	pop	hl		; ram source in HL'
	ld	c,0x98	; data port in c'
2:	di
	exx
	out (c),l
	out (c),h	;c = 0x99, HL with write setup bit set
	add hl,de	;de = 8
	exx
	ld b,8
1:	outi		;c' = 0x98
	jp nz,1b
	ei
	dec a
	jp nz,2b
	ret
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	in xmap,ymap
;
_plot_pnt:
	ld	a,:metavec
	ld	(_bank4),a

	ld	a,(phase)
	add	a,metavec/256
	ld	(phase),a
	
	ld	hl,buffer	; meta map in ram
	
	
	ld	a,(ymap+1)	; ymap 8.8	
	and	a
	jr	z,1f
	ld	bc,LvlWidth
2:	add	hl,bc
	dec a
	jr	nz,2b
1:
	ld	a,(xmap+1)	; xmap 8.8	
	ld	e,a
	ld	d,0
	add	hl,de
	ex	de,hl
	
	setvdpwvram 0x1900
	ld	bc,0x1098
1:	push	bc
	repeat 32
	ld	a,(de)
	ld	l,a
	ld	a,(phase)
	ld	h,a
	outi         ; Send data pointed by HL to VDP port (reg.C preloaded 
	inc	de
	endrepeat
	
	ld	hl,LvlWidth-32
	add	hl,de
	ex	de,hl
	
	pop	bc
	dec b
	jp nz,1b

	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;          defb 0x02 ; Reg# 0 000000[M3][EV]
;          defb 0x62 ; Reg# 1 [4/16k][BLANK][IE][M1][M2]0[SIZE][MAG]
;          defb 0x06 ; Reg# 2 0000[NAME TABLE BASE ADDRESS]          = 1800h

;          defb 0x9F ; Reg# 3 [COLOR BASE ADDRESS]                   = 2000h ; hybrid mode for colors
;          defb 0xFF ; Reg# 3 [COLOR BASE ADDRESS]                   = 2000h ; regular mode for colors	

;          defb 0x1F ; Reg# 3 [COLOR BASE ADDRESS]                   = 0000h ; hybrid mode for colors
;          defb 0x7F ; Reg# 3 [COLOR BASE ADDRESS]                   = 0000h ; regular mode for colors	
	  
;          defb 0x00 ; Reg# 4 00000[PATTERN GENERATOR BASE ADDRESS]  = 0000h ; hybrid mode for patterns
;          defb 0x03 ; Reg# 4 00000[PATTERN GENERATOR BASE ADDRESS]  = 0000h ; regular mode for patterns

;          defb 0x04 ; Reg# 4 00000[PATTERN GENERATOR BASE ADDRESS]  = 2000h ; hybrid mode for patterns
;          defb 0x07 ; Reg# 4 00000[PATTERN GENERATOR BASE ADDRESS]  = 2000h ; regular mode for patterns
          
;          defb 0x36 ; Reg# 5 0[SPRITE ATTRIBUTE TABLE BASE ADDRESS] = 1b00h
;          defb 0x07 ; Reg# 6 00000[SPRITE PTRN GNRTR BASE ADDRESS]  = 3800h
;          defb 0x01 ; Reg# 7 [TEXT COLOR 4bts][BACKDROP COLOR 4bts]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
disp_page1:			; page 1 active
	setVdp 3,0x9F	; 	colours at 0x2000	(hybrid)
	setVdp 4,0x03	;	patterns at 0x0000	(regular: used 0x0800 0x1000)
	ret

disp_page0:			; page 0 active
	setVdp 3,0x1F	; 	colours at 0x0000	(hybrid)
	setVdp 4,0x07	;	patterns at 0x2000	(regular: used 0x2800 0x3000)
	ret	


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; set pages and subslot
;
rominit:
	call    0x138
	rrca
	rrca
	and     0x03
	ld      c,a
	ld      b,0
	ld      hl,0xfcc1
	add     hl,bc
	or      (hl)
	ld      b,a
	inc     hl
	inc     hl
	inc     hl
	inc     hl
	ld      a,(hl)
	and     0x0c
	or      b
    
	ld      h,0x80
	call    0x24  
	
	; now we have:
	; page 0 	- bios
	; page 1,2	- megarom mapper
	; page 3	- RAM
	
	; init megarom mapper
	xor	a
	ld	(_bank1),a
	inc	a
	ld	(_bank2),a
	inc	a
	ld	(_bank3),a
	inc	a
	ld	(_bank4),a

	ret
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ROM initialisation

initmain:

	ei
	halt
	di
	call 	rominit
	
	; now the first 32KB of the megarom are active

	;clear 8K RAM
	ld	bc,1024*8-1
	ld	hl,0xc000
	ld	de,0xc001

	ld	(hl),0
	ldir	

	; screen 2
	call _scr2
	
	ld	a,0
	ld	e,7
	call	_setvdpreg
	
	halt
	call	cls
	call	set_sprites

	; move meta map in ram
	ld	a,:metamap
	ld	(_bank4),a
	ld	bc,LvlWidth*LvlHeigh
	ld	hl,metamap
	ld	de,buffer
	ldir	
	
	ld	hl,0
	ld	(xmap),hl
	ld	hl,0
	ld	(ymap),hl
	xor a
	ld	(dxmap),a
	ld	(dymap),a


mainloop:	
	
	halt 
	call 	disp_page0
	call	_plot_pnt

	call	sub_main
	call	setvramp1

	halt 
	call 	disp_page1
	call	_plot_pnt

	call	sub_main
	call	setvramp0
	
	jp mainloop
	
dxdycontrol:	
    ld  a,l
    and 0x80    ; right
    jr  nz,1f
    ld  a,(dxmap)
    cp  maxspeed        ; MAX SPEED
    jr  z,1f
    add a,maxspeedstep
    ld  (dxmap),a
1:
    ld  a,l
    and 0x10    ; left
    jr  nz,1f
    ld  a,(dxmap)
    cp  -maxspeed       ; MAX SPEED
    jr  z,1f
    add a,-maxspeedstep
    ld  (dxmap),a
1:
    ; y position
    ld  a,l
    and 0x20    ; up
    jr  nz,1f
    ld  a,(dymap)
    cp  -maxspeed        ; MAX SPEED
    jr  z,1f
    add a,-maxspeedstep
    ld  (dymap),a
1:
    ld  a,l
    and 0x40    ; down
    jr  nz,1f
    ld  a,(dymap)
    cp  maxspeed       ; MAX SPEED
    jr  z,1f
    add a,maxspeedstep
    ld  (dymap),a
1:
	ret
stopxrigth:	
stopxleft:	
	xor	a
	ld  (dxmap),a
	ld	hl,0
	ret	
stopydown:
stopytop:	
	xor	a
	ld  (dymap),a
	ld	hl,0
	ret	
sub_main:
    ; x speed control
    ld  e,8
    call    checkkbd
	call	dxdycontrol

    ld  de,(xmap)		; FP 4.4
    ld  a,(dxmap)
    ld  l,a
    add a,a
    sbc a,a
    ld  h,a
[4]	add	hl,hl	
	and a
    adc hl,de
	call	m,stopxleft
	ld	de,(LvlWidth-32)*256-64
	and a	
	sbc	hl,de
	call	p,stopxrigth
	add	hl,de
    ld  (xmap),hl

    ld  de,(ymap)		; FP 4.4
    ld  a,(dymap)
    ld  l,a
    add a,a
    sbc a,a
    ld  h,a
[4]	add	hl,hl
	and	a
    adc hl,de
	call	m,stopytop
	ld	de,(LvlHeigh-16)*256-64
	and a	
	sbc	hl,de
	call	p,stopydown
	add	hl,de
    ld  (ymap),hl


	ld	a,(xmap)	; xmap 8.8	
[2]	rlca
	and		%00000011
	ld	b,a
	ld	a,(ymap)	; ymap 8.8	
[4]	rlca
	and		%00001100
	or	b
	ld	(phase),a
	ret
	



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	convert a VRAM position pointed by HL 
;	in a VRAM address with offset in DE
;	sets the VDP for write
;	in:
;		HL -> VRAM Position
;		DE = VRAM Offset
;	out:
;		HL++

setvramaddr:
	ld	a,(hl)
	push hl
	ld	l,a
	ld	h,0
	add	hl,hl
	add	hl,hl
	add	hl,hl
	add	hl,de
	ld	a,l
	di
	out	(0x99),a
	ld	a,h
	or	0x40
	out	(0x99),a
	ei
	pop	hl
	inc	hl
	ret
	

		
	