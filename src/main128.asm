

	incdir data_bin/

	include parameters.asm

	output Scrll8way128.rom

	defpage	0,0x4000, 0x2000		; page 0 contains main code + far call routines
	defpage 1,0x6000, 0x2000		; static code
	defpage	2,0x8000, 0x2000		; static code - ISR code
	
	defpage	3,0xA000, 0x2000		; 
	defpage	4,0xA000, 0x2000		; common patterns and colors
	
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



; *** tile set data in rom ***
	page 3
tileset:
	incbin tileset.bin
	
	; place sprites in the score bar
scorebar_sat:
	repeat  4
	db  5*8
	db  @# * 16
	db  32*4
	db  0
	endrepeat
	db  5*8+14
	db  4*16
	db  32*4
	db  0
	db	0xd0
	
	page 4
common_pattern:
	incbin testpat.bin,0,CommonTiles*8
common_color:
	incbin testcol.bin,0,CommonTiles*8

	page 5
test_spt:
	incbin spt.bin
;	place 32 sprites
test_sat:
counter:=0
	repeat	8
	repeat	4
	db	64 + @@# * 16
	db	48 +  @# * 32 + @@#*8
	db	6*4+(counter and %00001100)
	db	15
counter:=counter+4
	endrepeat
	endrepeat

	page 6
metamap:
	incbin metamap.bin

	page 7
metavec:
	incbin metavec.bin

	page 8
patterns_base:
patterns11_13_15_17:
	incbin testpat.bin,00*2*1024+CommonTiles*8,4*2*1024-CommonTiles*8
	page 9
patterns31_33_35_37:
	incbin testpat.bin,04*2*1024+CommonTiles*8,8*1024-CommonTiles*8
	page 10
patterns51_53_55_57:
	incbin testpat.bin,08*2*1024+CommonTiles*8,8*1024-CommonTiles*8
	page 11
patterns71_73_75_77:
	incbin testpat.bin,12*2*1024+CommonTiles*8,8*1024-CommonTiles*8

	page 12
colors_base:
colors11_13_15_17:
	incbin testcol.bin,00*2*1024+CommonTiles*8,4*2*1024-CommonTiles*8
	page 13
colors31_33_35_37:
	incbin testcol.bin,04*2*1024+CommonTiles*8,8*1024-CommonTiles*8
	page 14
colors51_53_55_57:
	incbin testcol.bin,08*2*1024+CommonTiles*8,8*1024-CommonTiles*8
	page 15
colors71_73_75_77:
	incbin testcol.bin,12*2*1024+CommonTiles*8,8*1024-CommonTiles*8




    macro _setVdp register,value       ; without DI/EI
    ld  a,value
    out (0x99),a
    ld  a,register + 0x80
    out (0x99),a
    endmacro

    macro setVdp register,value       ; macro definition
    di
    _setVdp register,value
    ei
    endmacro

    macro _setvdpwvram value
    if (value & 0xFF)
        ld  a,value & 0xFF
    else
        xor a
    endif
    out (0x99),a
    ld  a,0x40 + (value/256)
    out (0x99),a
    endmacro

    macro setvdpwvram value
    di
    _setvdpwvram value
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

; -----------------------------
; parameters
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
slotram:		#1

xmap:			#2		; FP 8.8
ymap:			#2		; FP 8.8
dxmap:			#1		; FP 4.4
dymap:			#1		; FP 4.4

phase:			#1
vpage:			#1

	endmap

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Interrupt service routine
;

; remove any processing from the ISR

	page 2
myisr:
 	pop	af		; 	remove return address
	in  a,(0x99)	; 	s0 reset
 	pop    ix
	pop    iy
	pop    af
	pop    bc
	pop    de
	pop    hl
	ex     af,af'
	exx
	pop    af
	pop    bc
	pop    de
	pop    hl
	ei
	ret
	
;	push   hl
;	push   de
;	push   bc
;	push   af
;	exx
;	ex     af,af'
;	push   hl
;	push   de
;	push   bc
;	push   af
;	push   iy
;	push   ix

;	disabled code for future developments

;	pop		af		; 	remove return address

;	_setVdp 0,0x00  ; 	screen 1

;	_setVdp 4,0x03  ; 	PGT at 1800h (used from 0x1C00 to 0x1FFF, only 128 characters)
;	_setVdp 3,0x6F  ; 	PCT at 1BC0h (used from 0x1BD0 to 0x1BDF, only 16 bytes)

;	_setVdp 5,0x37  ;   SAT at 1B80
;	_setVdp 6,0x03  ;   SPT at 1800   (used from 0x1C00 to 0x1FFF  only 32 sprites 16x16)

;	in  a,(0x99)	; 	s0 reset

	; _setVdp 7,8  	; 	
	; call    _plot_pnt
	; _setVdp 7,0  	; 	

;1:  in  a,(0x99)	; 	wait raster line
;	and %01011111
;	cp  %01000100	; 	plane 4 =0x44
;	jp  nz,1b

;	_setVdp 0,0x02  ;	screen 2

;	ld  a,(vpage)	
;	and a
;	jp  z,page0
;page1:				; 	page 1 active
;	_setVdp 3,0x9F	; 	colours at 0x2000	(hybrid)
;	_setVdp 4,0x03	;	patterns at 0x0000	(regular: used 0x0800 0x1000)
;	jp  1f
;page0:				; 	page 0 active
;	_setVdp 3,0x1F	; 	colours at 0x0000	(hybrid)
;	_setVdp 4,0x07	;	patterns at 0x2000	(regular: used 0x2800 0x3000)
;1:
;	_setVdp 5,0x36  ;   SAT at 0x1b00
;	_setVdp 6,0x07  ;   SPT at 0x3800   (64 sprites 16x16)

;	pop    ix
;	pop    iy
;	pop    af
;	pop    bc
;	pop    de
;	pop    hl
;	ex     af,af'
;	exx
;	pop    af
;	pop    bc
;	pop    de
;	pop    hl
;	ei
;	ret


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
	ld	a,8
1:	outi
	jp nz,1b
	dec	a
	jp nz,1b
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	Initialise the tiles common to all banks
;	Common tiles are stored only once in a separate bank
;
initcommontiles:
	ld	a,:common_pattern
	ld (_bank4),a
	ld	hl,common_pattern
	ld	a,CommonTiles
	ld	de,0x0800 + 4000h
	call write_2k

	ld	hl,common_pattern
	ld	a,CommonTiles
	ld	de,0x1000 + 4000h
	call write_2k

	ld	hl,common_pattern
	ld	a,CommonTiles
	ld	de,0x2800 + 4000h
	call write_2k

	ld	hl,common_pattern
	ld	a,CommonTiles
	ld	de,0x3000 + 4000h
	call write_2k

	ld	a,:common_color
	ld (_bank4),a
	ld	hl,common_color
	ld	a,CommonTiles
	ld	de,0x2000 + 4000h
	call write_2k
	ld	hl,common_color
	ld	a,CommonTiles
	ld	de,0x0000 + 4000h
	call write_2k
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	load tile sets: only differences are loaded

setvramp:
	ld	a,(vpage)
	and	a
	jp	z,setvramp1	;	PGT at	0x0800/0x1000,	PCT at 0x0800
	jp	setvramp0	;	PGT at	0x2800/0x3000,	PCT at 0x0000

setvramp1:			;	PGT at	0x0800/0x1000,	PGT at 0x2000
	call getbank	;->a
	ld (_bank4),a
	call getsize	;->a
	call getaddr	;->hl
	ld	de,0x0800 + 8*CommonTiles + 4000h
	call write_2k

	call getsize	;->a
	call getaddr	;->hl
	ld	de,0x1000 + 8*CommonTiles + 4000h
	call write_2k

	call getbank	;->a
	add	a,4			; colorbank = tilebank+4
	ld (_bank4),a
	call getsize	;->a
	call getaddr	;->hl
	ld	de,0x2000 + 8*CommonTiles + 4000h
	call write_2k
	ret

setvramp0:			;	PGT at	0x2800/0x3000,	PGT at 0x0000
	call getbank	;->a
	ld (_bank4),a
	call getsize	;->a
	call getaddr	;->hl
	ld	de,0x2800 + 8*CommonTiles + 4000h
	call write_2k

	call getsize	;->a
	call getaddr	;->hl
	ld	de,0x3000 + 8*CommonTiles + 4000h
	call write_2k

	call getbank	;->a
	add	a,4			; colorbank = tilebank+4
	ld (_bank4),a
	call getsize	;->a
	call getaddr	;->hl
	ld	de,0x0000 + 8*CommonTiles + 4000h
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
	ld	de,(phase)
	ld	hl,tilesize
	ld	d,0
	add	hl,de
	ld	a,(hl)
	ret


	;	patternsxy
tilebank:
	db	:patterns11_13_15_17,:patterns11_13_15_17,:patterns11_13_15_17,:patterns11_13_15_17
	db	:patterns31_33_35_37,:patterns31_33_35_37,:patterns31_33_35_37,:patterns31_33_35_37
	db	:patterns51_53_55_57,:patterns51_53_55_57,:patterns51_53_55_57,:patterns51_53_55_57
	db	:patterns71_73_75_77,:patterns71_73_75_77,:patterns71_73_75_77,:patterns71_73_75_77
tilesize:
	db	TileSize11-CommonTiles,TileSize13-CommonTiles,TileSize15-CommonTiles,TileSize17-CommonTiles
	db	TileSize31-CommonTiles,TileSize33-CommonTiles,TileSize35-CommonTiles,TileSize37-CommonTiles
	db	TileSize51-CommonTiles,TileSize53-CommonTiles,TileSize55-CommonTiles,TileSize57-CommonTiles
	db	TileSize71-CommonTiles,TileSize73-CommonTiles,TileSize75-CommonTiles,TileSize77-CommonTiles
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
	di
	ld	c,0x99
	out (c),e
	out (c),d	;c = 0x99, HL with write setup bit set

	ld	e,a
	ld	d,0
	ex	de,hl
[3]	add hl,hl
	ex	de,hl
	
	dec	c
	inc	d
	ld	b,e
2:	outi
	jp nz,2b
	dec	d
	jp nz,2b	
	ei
	ret
	
; write_2k:
	; push hl
	; ex	de,hl
	; set	6,h
	; ld	c,0x99
	; ld	de,8
	; exx
	; pop	hl		; ram source in HL'
	; ld	c,0x98	; data port in c'
; 2:	di
	; exx
	; out (c),l
	; out (c),h	;c = 0x99, HL with write setup bit set
	; add hl,de	;de = 8
	; exx
	; ld b,8
; 1:	outi		;c' = 0x98
	; jp nz,1b
	; ei
	; dec a
	; jp nz,2b
	; ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	in xmap,ymap,phase
;
plot_pnt:
	ld	a,:metavec
	ld	(_bank4),a

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
	ex	de,hl		; DE = buffer + round(ymap)*LvlWidth+round(xmap)

	setvdpwvram 0x1900
	
	ld  c,0x98
	ld  ixh,16

	ld	a,(phase)
	add	a,metavec/256
	
1:	
	ld	h,a
	ex	af',af
	
	ld  a,e
	add a,32
	jp  nc,.fast_loop

	
	repeat 32
	ld	a,(de)
	ld	l,a
	outi         	; Send data pointed by HL to VDP port (reg.C preloaded)
	inc	de
	endrepeat
	
	ex	af',af
	
	ld	hl,LvlWidth-32
	add	hl,de
	ex	de,hl

	dec ixh
	jp nz,1b

	ret

.fast_loop

	repeat 32
	ld	a,(de)
	ld	l,a
	outi         	; Send data pointed by HL to VDP port (reg.C preloaded)
	inc	e			; save 32 INC DE 
	endrepeat
	
	ex	af',af
	
	ld	hl,LvlWidth-32
	add	hl,de
	ex	de,hl

	dec ixh
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
	_setVdp 3,0x9F	; 	colours at 0x2000	(hybrid)
	_setVdp 4,0x03	;	patterns at 0x0000	(regular: used 0x0800 0x1000)
	ret

disp_page0:			; page 0 active
	_setVdp 3,0x1F	; 	colours at 0x0000	(hybrid)
	_setVdp 4,0x07	;	patterns at 0x2000	(regular: used 0x2800 0x3000)
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; set pages and subslot
;


ENASLT:         equ     024h
RSLREG:         equ     0138h
EXPTBL:         equ     0FCC1h  ; Bios Slot / Expansion Slot

; -----------------------
; SEARCH_SLOTSET
; Posiciona en pagina 2
; Nuestro ROM.
; -----------------------

search_slotset:
	di
	call    search_slot
	call	ENASLT
	
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


; -----------------------
; SEARCH_SLOT
; Busca slot de nuestro rom
; -----------------------

search_slot:
	call    RSLREG
	rrca
	rrca
	and     3
	ld      c,a
	ld      b,0
	ld      hl,EXPTBL
	add     hl,bc
	ld      a,(hl)
	and     080h
	or      c
	ld      c,a
	inc     hl
	inc     hl
	inc     hl
	inc     hl
	ld      a,(hl)
	and     0Ch
	or      c;
	ld      h,080h
	ld      (slotvar),a
	ret

; ------------------------------
; SETROMPAGE0
; Posiciona nuestro cartucho en
; Pagina 0
; -----------------------------

setrompage0:
	ld      a,(slotvar)
	jr      setslotpage0

; ------------------------------
; RECBIOS
; Posiciona la bios ROM
; -------------------------------
recbios:
	ld      a,(EXPTBL)

; ---------------------------
; SETSLOTPAGE0
; Posiciona el slot pasado
; en pagina 0 del Z80
; A: Formato FxxxSSPP
; ----------------------------

setslotpage0:
	di
	ld      b,a                 ; B = Slot param in FxxxSSPP format

	in      a,(0A8h)
	and     0xFC
	ld      d,a                 ; D = Primary slot value

	ld      a,b

	and     3
	or      d
	ld      d,a                 ; D = Final Value for primary slot

	; Check if expanded
	ld      a,b
	bit     7,a
	jr      z,1f    ; Not Expanded

	and     3
	rrca
	rrca
	and     0xC0
	ld      c,a
	ld      a,d
	and     0x3F
	or      c
	ld      c,a                 ; Primary slot value with main slot in page 3

	ld      a,b
	and     0x0C
	rrca
	rrca
	and     3
	ld      b,a                 ; B = Expanded slot in page 3
	ld      a,c
	out     (0A8h),a            ; Slot : Main Slot, xx, xx, Main slot
	ld      a,(0FFFFh)
	cpl
	and     0xFC
	or      b
	ld      (0FFFFh),a          ; Expanded slot selected

1:
	ld      a,d             ; A = Final value
	out     (0A8h),a		; Slot Final. Ram, rom c, rom c, Main
	ret
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   load tile sets and sprites
vram_init:
	setvdpwvram 0x1B80              ; sprites in the scorebar
	ld  hl,scorebar_sat
	ld	a,:scorebar_sat
	call    write_256

	setvdpwvram 0x1C00
	ld  hl,tileset					; dummy tile set from basic rom
	ld	a,:tileset
	call    write_256

	setvdpwvram 0x1800				;   dummy PNT
	ld	b,0
	ld	a,4
1:	out (0x98),a
	inc	b
	jr	nz,1b

	setvdpwvram 0x1BD0
	ld	b,16				; 	dummy colors
	ld	a,11h
1:	out (0x98),a
	dec	b
	jr	nz,1b
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ROM initialisation

initmain:
	
	call _scr2				; screen 2
	
	call 	search_slotset	; now the first 32KB of the megarom are active
			
	;call	setrompage0		; 48K of rom are active - bios is excluded
	
	_setVdp 7,0
	ei
	call	set_sprites
	call 	vram_init		; set data in the top 3d of the screen
	
	call	initcommontiles		;	load common tiles
	
	; move meta map in ram
	ld	a,:metamap
	ld	(_bank4),a
	ld	bc,LvlWidth*LvlHeigh
	ld	hl,metamap
	ld	de,buffer
	ldir

	ld	hl,myisr
	ld	(0xFD9A+1),hl
	ld	a,0xC3
	ld	(0xFD9A),a

	ld	hl,0
	ld	(xmap),hl
	ld	(ymap),hl
	xor a
	ld	(dxmap),a
	ld	(dymap),a
	ld	(phase),a
	inc	a
	ld	(vpage),a
	

mainloop:
	
	halt
	; setVdp 7,8
	call	show_activepage
	call    plot_pnt		;	use phase to plot the correct PNT
	; setVdp 7,0
	
	call	sub_main		; 	compute new "phase"

	; setVdp 7,10
	call	setvramp		;	use phase to select the tileset loaded to page 1/0
	; setVdp 7,0
		
	ld	a,(vpage)
	xor 1				; 	swap page
	ld	(vpage),a			
		
	jp 	mainloop
	
	
show_activepage:	
	ld  a,(vpage)	
	and a
	jp  z,.page0
.page1:				; 	page 1 active
	di
	_setVdp 3,0x9F	; 	PCT at 0x2000	(hybrid)
	_setVdp 4,0x03	;	PGT at 0x0000	(regular: used 0x0800 0x1000)
	ei
	ret
.page0:				; 	page 0 active
	di
	_setVdp 3,0x1F	; 	PCT at 0x0000	(hybrid)
	_setVdp 4,0x07	;	PGT at 0x2000	(regular: used 0x2800 0x3000)
	ei
	ret
	
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

stopx:
	xor	a
	ld  (dxmap),a
	ld	hl,0
	ret
stopy:
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
	call	m,stopx
	ld	de,(LvlWidth-32)*256-64
	and a
	sbc	hl,de
	call	p,stopx
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
	call	m,stopy
	ld	de,(LvlHeigh-16)*256-64
	and a
	sbc	hl,de
	call	p,stopy
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




