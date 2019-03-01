; -----------------------------
; 8 directions smooth scroller engine
; ------------------------------

; ------------
; macros
;
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


	output Scrll8way.rom

	incdir data_bin/
	incdir data_miz/
	incdir src/

	defpage 0,0x0000,0x4000
	defpage 1,0x4000,0x8000

	page 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Interrupt service routine
;
	code @ 0x0038

	push   hl
	push   de
	push   bc
	push   af
	exx
	ex     af,af'
	push   hl
	push   de
	push   bc
	push   af
	push   iy
	push   ix

	_setVdp 0,0x00  ; 	screen 1

	_setVdp 4,0x03  ; 	PGT at 1800h (used from 0x1C00 to 0x1FFF, only 128 characters)
	_setVdp 3,0x6F  ; 	PCT at 1BC0h (used from 0x1BD0 to 0x1BDF, only 16 bytes)

	_setVdp 5,0x37  ;   SAT at 1B80
	_setVdp 6,0x03  ;   SPT at 1800   (used from 0x1C00 to 0x1FFF  only 32 sprites 16x16)

	in  a,(0x99)	; s0 reset

;	call _sat_update
	_setvdpwvram 0x1900
	call    _plot_pnt


1:  in  a,(0x99)	; wait raster line
	and %01011111
	cp  %01000100	; plane 4 =0x44
	jp  nz,1b

	_setVdp 0,0x02  ;	screen 2

	ld  a,(xmap)	; lower bits
	ld  b,a
	ld  a,(ymap)	; lower bits
	xor b
	and 2
	jp  z,page0
page1:
	call    disp_page1
	jp  1f
page0:
	call    disp_page0
1:
	_setVdp 5,0x36  ;   SAT at 0x1b00
	_setVdp 6,0x07  ;   SPT at 0x3800   (64 sprites 16x16)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; -------------------------------------------------------
; MSX-O-Mizer v1.5f datas depacker    *ROM based version*
; Improved from Metalbrain's z80 version.
; -------------------------------------------------------
; source in hl
; dest in de

; 328 bytes which must be aligned on 8 bits boundary
mom_map_bits_rom    =       0xe000
; 26 bytes located in ram
mom_offset_table    =       0xe000 + 328


mom_depack_rom:     push    de
					ld      bc, mom_offset_table
					push    bc
					ld      d,b
					ld		e,c
					ld      bc, 26
					ldir
					push    hl
					pop     af
					pop     hl
					push    af
					ld      iy, mom_map_bits_rom + 0xf0
					ld      b, 52
mom_init_bits_rom:  ld      a, iyl
					and     15
					jr      nz, mom_node_rom
					ld      de, 1
mom_node_rom:       rrd
					ld      (iy), a
					ld      (iy + 36), e
					ld      (iy + 72), d
					inc     iyl
					inc     a
					push    hl
					ld      hl, 0
					scf
mom_set_bit_rom:    adc     hl, hl
					dec     a
					jr      nz, mom_set_bit_rom
					add     hl, de
					ex      de, hl
					pop     hl
					bit     0, b
					jr      z, mom_wait_step_rom
					inc     hl
mom_wait_step_rom:  djnz    mom_init_bits_rom
					pop     hl
					ld      a, (hl)
					inc     hl
					ld      ixh, a
					pop     de
mom_lit_copy_rom:   ldi
mom_main_loop_rom:  call    mom_get_bit_rom
					jr      c, mom_lit_copy_rom
					ld      c, -17
mom_get_index_rom:  call    mom_get_bit_rom
					inc     c
					jr      nc, mom_get_index_rom
					ld      a, c
					ret     z
					push    de
					call    mom_get_pair_rom
					push    bc
					jr      nz, mom_out_range_rom
					ld      de, 0x0220
					dec     c
					jr      z, mom_go_for_it_rom
					ld      de, 0x0410
					dec     c
					jr      z, mom_go_for_it_rom
mom_out_range_rom:  ld      de, 0x0400
mom_go_for_it_rom:  pop     af
					ex      af, af'
					call    mom_get_bits_rom
					add     a, e
					call    mom_get_pair_rom
					pop     de
					push    hl
					ld      h, d
					ld      l, e
					sbc     hl, bc
					ex      af, af'
					push    af
					pop     bc
					ldir
					pop     hl
					jr      mom_main_loop_rom
mom_get_pair_rom:   ld      iyl, a
					ld      d, (iy)
					call    mom_get_bits_rom
					add     (iy + 36)
					ld      c, a
					ld      a, b
					adc     (iy + 72)
					ld      b, a
					ret
mom_get_bits_rom:   ld      bc, 0
mom_getting_bits_rom:
					dec     d
					ld      a, c
					ret     m
					call    mom_get_bit_rom
					rl      c
					rl      b
					jr      mom_getting_bits_rom
mom_get_bit_rom:    ld      a, ixh
					add     a
					jr      nz, mom_byte_done_rom
					ld      a, (hl)
					inc     hl
					rla
mom_byte_done_rom:  ld      ixh, a
					ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PLOT PNT
; input : xmap,ymap
;
_plot_pnt:
	ld  hl,(xmap)   ; x position in pixels
	ld  a,l
	rrca
	and %00000011
	ld	b,a
	ld  hl,(ymap)   ; y position in pixels
	ld  a,l
	and %00000110
	add a,a
	add a,b
	add a,meta_pnt_table/256
	ld  ixl,a		; 			row in meta vec

	; hl = x/8+y/8*64 = x/8 + (y & %1111 1111 1111 1000)*8

	ld  hl,(xmap)   ; x position in pixels
	srl h
	rr  l
	srl h
	rr  l
	srl h
	rr  l
	push hl
	ld  hl,(ymap)   ; y position in pixels
	ld  a,l
	and %11111000
	ld  l,a
	add hl,hl
	add hl,hl
	add hl,hl
	pop de
	add hl,de
	ld  de,level_buffer
	add hl,de

	ex  de,hl

	ld  c,0x98
	ld  ixh,16
1:
	ld  a,ixl
	ld  h,a
	ld  a,e
	add a,32
	jp  nc,.fast_loop

.slow_loop:
	repeat 32
	ld  a,(de)
	ld  l,a
	outi
	inc de
	endrepeat

	ld  hl,LvlWidth-32
	add hl,de
	ex  de,hl

	dec ixh
	jp  nz,1b

	ret

.fast_loop:
	repeat 32
	ld  a,(de)
	ld  l,a
	outi
	inc e
	endrepeat

	ld  hl,LvlWidth-32
	add hl,de
	ex  de,hl

	dec ixh
	jp  nz,1b

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

disp_page1:         ; page 1 active
	_setVdp 3,0x9F  ;   colours at 0x2000   (hybrid)
	_setVdp 4,0x03  ;   patterns at 0x0000  (regular: used 0x0800 0x1000)
	ret

disp_page0:         ; page 0 active
	_setVdp 3,0x1F  ;   colours at 0x0000   (hybrid)
	_setVdp 4,0x07  ;   patterns at 0x2000  (regular: used 0x2800 0x3000)
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   sprite multiplexing
;
_sat_update:
	ld  a,(reverse_sat)
	xor 1
	ld  (reverse_sat),a
	jp  nz,_reverse_sat

_directsat:
	ld  a,(visible_sprts)
	and 0xFC
	ret z
	ld  b,a
	ld  c,0x98
	ld  hl,ram_sat
	_setvdpwvram 0x1b00
1:  outi
	outi
	outi
	outi
	jp  nz,1b
	ld  a,0xD0
	out (0x98),a
	ret

_reverse_sat
	ld  a,(visible_sprts)
	and 0xFC
	ret z
	ld  b,a
	ld  c,0x98
	ld  hl,ram_sat-4+8

	ld  e,b
	ld  d,0
	add hl,de
	ld  de,-8

	_setvdpwvram 0x1b00
1:  add hl,de
	outi
	outi
	outi
	outi
	jp  nz,1b
	ld  a,0xD0
	out (0x98),a
	ret




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; IN HL:DE INPUT
; BC POINTS TO OUTPUT

long2ascii:
					; HL = HIGH WORD
	PUSH    DE
	EXX
	POP     HL      ; HL' = LOW WORD
	EXX

	LD  E,C
	LD  D,B

	LD  BC,-1000000000/0x10000 -1
	EXX
	LD  BC,-1000000000&0xFFFF
	EXX
	CALL    NUM1

	LD  BC,-100000000/0x10000 -1
	EXX
	LD  BC,-100000000&0xFFFF
	EXX
	CALL    NUM1

	LD  BC,-10000000/0x10000 -1
	EXX
	LD  BC,-10000000&0xFFFF
	EXX
	CALL    NUM1

	LD  BC,-1000000/0x10000 -1
	EXX
	LD  BC,-1000000&0xFFFF
	EXX
	CALL    NUM1

	LD  BC,-100000/0x10000 -1
	EXX
	LD  BC,-100000&0xFFFF
	EXX
	CALL    NUM1

	LD  BC,-10000/0x10000 -1
	EXX
	LD  BC,-10000&0xFFFF
	EXX
	CALL    NUM1

	LD  BC,-1000/0x10000 -1
	EXX
	LD  BC,-1000&0xFFFF
	EXX
	CALL    NUM1

	LD  BC,-100/0x10000 -1
	EXX
	LD  BC,-100&0xFFFF
	EXX
	CALL    NUM1

	LD  BC,-10/0x10000 -1
	EXX
	LD  BC,-10&0xFFFF
	EXX
	CALL    NUM1

	LD  BC,-1/0x10000 -1
	EXX
	LD  BC,-1&0xFFFF
	EXX

NUM1:
	LD  A,'0'-1  ; '0' IN THE TILESET

1:
	INC A
	EXX
	add HL,BC       ; low word
	EXX
	ADC HL,BC       ; high word
	jp  C,1b

	EXX
	SBC HL,BC       ; low word
	EXX
	SBC HL,BC       ; high word

	LD  (DE),A
	INC DE
	RET

; ------------------------------------------------
; expand level  in A
;
levelinit:
	ld	hl,level_buffer
	ld	de,level_buffer+1
	xor	a
	ld	(hl),a
	ld	bc,6*1024-1
	ldir

	ld	hl,	meta_pnt
	ld	de,level_buffer
	ld	bc,64*64
	ldir
	ret

; ------------------------------------------------
; rom header

	page 1

	code @ 0x4000

	org 04000h
	db  041h,042h
	dw  initmain
	dz  'TRI004'
	ds  5

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

; -----------------------------
; parameters
;
LvlWidth:   equ 64
LvlHigh:   	equ 64

;   nphase:     equ 4
;   xstep:      equ 2
;


maxspeed:   equ 8
max_enem:   equ 29      ; max number of enemies -> + 3 for ms  = 32 sprites

; ------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; vdp access

_vdpReg equ 0xF3DF
LINL40  equ 0xF3AE

; e = screen mode
_scr:
	ld  a,(_vdpReg+1)
	or 2
	ld  (_vdpReg+1),a

	ld  a,e
	call 0x005f

	ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   load tile sets and sprites
vraminit:


; set sprites

	_setvdpwvram 0x3800
	ld  hl,test_spt
	call    write_256

	_setvdpwvram 0x1B80              ; sprites in the scorebar
	ld  hl,scorebar_sat
	call    write_256

	_setvdpwvram 0x1C00
	ld  hl,scorebar 		;0x1BBF	; dummy tile set from basic rom
	call    write_256


	_setvdpwvram 0x1800
	xor a               ;   clear top panel
	ld	b,a
1:	or	128
	out (0x98),a
	inc a
	inc	b
	jr	nz,1b

	_setvdpwvram 0x1BD0
	ld	b,16
1:	ld	a,b
[2]	inc	a
[4]	add	a,a
	or b
	out (0x98),a
	dec	b
	jr	nz,1b

; set pat

	_setvdpwvram 0x0800
	ld  hl,chr_tileset1
	call    write_256

	_setvdpwvram 0x1000
	ld  hl,chr_tileset1
	call    write_256

	_setvdpwvram 0x2800
	ld  hl,chr_tileset0
	call    write_256

	_setvdpwvram 0x3000
	ld  hl,chr_tileset0
	call    write_256

; set colours

	_setvdpwvram 0x0000
	ld  hl,clr_tileset0
	call    write_256

	_setvdpwvram 0x2000
	ld  hl,clr_tileset1
	call    write_256

	ret

write_256:
	ld  bc,0x0098
	repeat 8
1:  outi
	jp nz,1b
	endrepeat
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

rominit:
search_slotset:
	di
	call    search_slot
	jp      ENASLT

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ROM initialisation

initmain:

	call    rominit     ; now the first 32KB of the rom are active

	ld  e,2
	call _scr			; screen 2

	setVdp 7,0x00

	ld  hl,0
	ld  (xmap),hl
	ld  (ymap),hl
	ld  a,0
	ld  (dxmap),a
	ld  (dymap),a


	call	setrompage0		; 48K of rom are active - bios is excluded

	call    vraminit

	call	levelinit

	ld  hl,test_sat
	ld  de,ram_sat
	ld  bc,128
	ldir

	ld  a,32*4
	ld  (visible_sprts),a

	xor	a
	ld  (reverse_sat),a

	ld  b,32*4
	ld  c,0x98
	ld  hl,ram_sat
	_setvdpwvram 0x1b00
1:  outi
	jp  nz,1b

	ei


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   main loop
;


main_loop:

	; x speed control
	ld  e,8
	call    checkkbd

	ld  a,l
	and 0x80    ; right
	jr  nz,1f
	ld  a,(dxmap)
	cp  maxspeed        ; MAX SPEED
	jr  z,1f
	inc a
	ld  (dxmap),a
1:
	ld  a,l
	and 0x10    ; left
	jr  nz,1f
	ld  a,(dxmap)
	cp  -maxspeed       ; MAX SPEED
	jr  z,1f
	dec a
	ld  (dxmap),a
1:
	; y position
	ld  a,l
	and 0x20    ; up
	jr  nz,1f
	ld  a,(dymap)
	cp  -maxspeed        ; MAX SPEED
	jr  z,1f
	dec a
	ld  (dymap),a
1:
	ld  a,l
	and 0x40    ; down
	jr  nz,1f
	ld  a,(dymap)
	cp  maxspeed       ; MAX SPEED
	jr  z,1f
	inc a
	ld  (dymap),a
1:


	; wait refresh and update map position

	halt
	ld  hl,(xmap)
	ld  a,(dxmap)
	sra a
	sra a
	ld  e,a
	add a,a
	sbc a,a
	ld  d,a
	add hl,de
	ld  (xmap),hl

	ld  hl,(ymap)
	ld  a,(dymap)
	sra a
	sra a
	ld  e,a
	add a,a
	sbc a,a
	ld  d,a
	add hl,de
	ld  (ymap),hl

	; Test map limits

	ld  hl,(xmap)
	ld  de,8*(LvlWidth)-256
	and a
	sbc hl,de
	jp  p,xendmapr   	; stop right

	and a
	adc hl,de
	jp  m,xendmapl		; stop left
testy:
	ld  hl,(ymap)
	ld  de,8*(LvlHigh)-128
	and a
	sbc hl,de
	jp  p,yendmapd   	; stop down

	and a
	adc hl,de
	jp  m,yendmapl		; stop up

	jp  main_loop

xendmapr:
	ld  hl,8*(LvlWidth)-256
	ld  (xmap),hl
	xor a
	ld	(dxmap),a
	jp  testy
xendmapl:
	ld  hl,0
	ld  (xmap),hl
	xor a
	ld	(dxmap),a
	jp  testy
yendmapd:
	ld  hl,8*(LvlHigh)-128
	ld  (ymap),hl
	xor a
	ld	(dymap),a
	jp  main_loop
yendmapl:
	ld  hl,0
	ld  (ymap),hl
	xor a
	ld	(dymap),a
	jp  main_loop



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   all sprites in the score bar


	; place the score bar
scorebar_sat:
	repeat  4
	db  48
	db  @# * 16
	db  32*4
	db  0
	endrepeat

	db  48+14
	db  4*16
	db  32*4
	db  0

	db	0xD0
test_sat
	repeat  8
	repeat  4
	db  64+16*(@@#)
	db  64 + @# * 32
	db  32*4
	db  15
	endrepeat
	endrepeat



; *** PNT data in rom ***

	code
	ALIGN 0x01000

meta_pnt_table:
	incbin metavec48.bin

meta_pnt:
	incbin metamap48.bin


; *** tile set data in rom ***
	code
chr_tileset0:
	incbin "testpat0.bin",7,256*8
chr_tileset1:
	incbin "testpat1.bin",7,256*8

clr_tileset0:
	incbin "testcol0.bin",7,256*8
clr_tileset1:
	incbin "testcol1.bin",7,256*8

	code
test_spt:
	include uridium.asm
scorebar:
	incbin tileset.bin

; *** RAM ***

	map  0C000h

level_buffer:	#6*1024

slotvar         #1

dxmap           #1
xmap            #2
dymap           #1
ymap            #2


visible_sprts   #1
reverse_sat     #1

ram_sat         #128


	endmap
