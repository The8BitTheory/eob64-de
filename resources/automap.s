.include "global.inc"
.include "vdc.inc"

titleD016 = frameBuffer_d800+0
colorCycle = frameBuffer_d800+1
SCROLLY = frameBuffer_d800+2
_d010 = frameBuffer_d800+3
_d015 = frameBuffer_d800+4
renderedRowChars = frameBuffer_bitmap+0 ; 32+8 +8 extra for col overrun on VDC
renderedRowColors = frameBuffer_bitmap+40 ; 32+8 +8 extra for col overrun on VDC
scrollers_base = frameBuffer_bitmap+80

CPTRm = RESULT+0
NPTRm = RESULT+2
EPTRm = RESULT+4
SPTRm = RESULT+6
WPTRm = RESULT+8

CPTRv = RESULT+10
NPTRv = RESULT+12
EPTRv = RESULT+14
SPTRv = RESULT+16
WPTRv = RESULT+18

NTMPv = RESULT+20
ETMPv = RESULT+21
STMPv = RESULT+22
WTMPv = RESULT+23

YREG4 = TMP2

; High-nybble = VDC, Low-nybble = VIC
UNVISITED_COLOR  = $0f
VISITED_COLOR 	 = $f1
WALL_COLOR 		 = $e0
ICON_COLOR 		 = $76
DOOR_COLOR		 = $36
TITLE_COLOR		 = $7
TITLE_BGCOLOR	 = $ec
STAIR_COLOR		 = $92
TELEPORTER_COLOR = $b4
PORTAL_COLOR	 = $c8
HOLE_PLATE_COLOR = $45
HIDDENWALL_COLOR = $e0

.segment "BSS"
MAX_NBR_DECORATIONS = 27
WALL_DEC_TABLE: .res MAX_NBR_DECORATIONS
FLOOR_DEC_TABLE: .res MAX_NBR_DECORATIONS

.segment "START"
textScreen_d018: .res 1
textScreen_d011: .res 1

screenMode:
	.word init
	.word teardown
	.byte 5
	.word $20, mode0
	.word $38, mode1
	.word $61, exitIrq
	.word $a1, exitIrq
	.word $fa, final

.proc final
		ldx #$fc
		lda twomhzMode
		beq :+
			ldx #$fd
		:
		stx $d030
		jmp exitIrq_quick
.endproc

.proc mode0
		jsrf :+
		jmp exitIrq_quick
.pushseg
.segment "AUTOMAP"
:
		lda $d010
		and #%11111011
		ora _d010
		sta $d010
		lda #0
		ldx mouseType
		cpx #MOUSETYPE_NONE
		beq :+
			lda #2
		:
		sta $d015
		lda titleD016
		sta $d016
		lda #TITLE_BGCOLOR
		sta $d021
		lda #(((vic_screen2/$400)&15)<<4) | ((vic_font/$400)&14)
		sta $d018
		lda #$fc
		sta $d030
		lda #$1b
		and screenEnabled
		sta $d011
		lda colorCycle
		and #15
		tax
		lda colors,x
		sta $d027+2
		inx
		stx colorCycle
		rts
colors:	.byte 0,6,2,4,$c,5,3,7,1,$d,$f,$a,$e,8,$b,9
.popseg
.endproc

.proc mode1
		jsrf :+
		jmp exitIrq_quick
.pushseg
.segment "AUTOMAP"
:
		lda #$7b
		and screenEnabled
		sta $d011
		lda #8
		sta $d016
		lda #UNVISITED_COLOR
		sta $d021
		lda textScreen_d018
		sta $d018
		lda $d015
		and #%11111011
		ora _d015
		sta $d015
		lda system
		cmp #SYSTEM_PAL
		beq :+
			cmp #SYSTEM_DREAN
			bne :+
				nop
		:
		nop
		lda textScreen_d011
		and screenEnabled
		sta $d011
		rts
.popseg
.endproc

.proc init
		jsrf :+
		rts
.pushseg
.segment "AUTOMAP"
:
		lda #<$d800
		sta MSRC+0
		lda #>$d800
		sta MSRC+1
		lda #<automap_d800
		sta MDST+0
		lda #>automap_d800
		sta MDST+1
		lda #<1000
		ldx #>1000
		jsr _memcpy_ram

		lda #$00
		sta $d015
		sta $d017
		sta $d01b
		sta $d01d
		lda #(1<<1)
		sta $d01c

		lda #$fc
		sta $d030
		lda #0
		sta $d011
		tax
		:
			sta $d800,x
			sta $d900,x
			sta $da00,x
			sta vic_screen2+$000,x
			sta vic_screen2+$100,x
			sta vic_screen2+$200,x
			inx
		bne :-
		:
			sta $db00,x
			sta vic_screen2+$300,x
			inx
			cpx #<1000
		bne :-

		ldx #39
		:
			lda #' '
			sta vic_screen2,x
			lda #TITLE_COLOR
			sta $d800,x
			dex
		bpl :-

		lda #<(automap_spr/$40)
		sta vic_screen2+$3f8+2

		lda #UNVISITED_COLOR
		sta $d021
		lda #$0
		sta $d022
		lda #VISITED_COLOR
		sta $d023
		lda #$0
		sta $d024

		rts
.popseg
.endproc

.proc teardown
		lda #$38
 		sta $d018
   		rts
.endproc

.segment "AUTOMAP"
.proc restoreD800
		lda #0
		sta $d011
		lda #<automap_d800
		sta MSRC+0
		lda #>automap_d800
		sta MSRC+1
		lda #<$d800
		sta MDST+0
		lda #>$d800
		sta MDST+1
		lda #<1000
		ldx #>1000
		jmp _memcpy_ram
.endproc

.export textScreen_open
.proc textScreen_open
		stx textScreen_d011
		sty textScreen_d018
		lda #0
		sta _d015
		sta _d010
		lda #8
		sta titleD016
		lda #$ff
		sta MOUSEEVENT+MouseEvent::buttons
		lda #$00
		sta KEYEVENT+KeyEvent::keyPressed

		lda #0
		sta timersEnabled

		lda POINTER_ITEM+0
		pha
		lda POINTER_ITEM+1
		pha
		lda #0
		sta POINTER_ITEM+0
		sta POINTER_ITEM+1
		jsrf updatePointerSprite
		pla
		sta POINTER_ITEM+1
		pla
		sta POINTER_ITEM+0

		lda #0
		sta screenEnabled
		clc
		ldx #<screenMode
		ldy #>screenMode
		jsr setScreenMode

		rts
.endproc

.export textScreen_close
.proc textScreen_close
		lda #0
		sta screenEnabled
		jsr restoreD800
		jsrf reinstallFont
		jsrf clearSprites

		clc
		ldx #<screenModeGameNormal
		ldy #>screenModeGameNormal
		jsr setScreenMode

		jsrf render

		jsrf updatePointerSprite
		lda #$ff
		sta screenEnabled

		rts
.endproc

.proc scrollers
.proc up
		lda 1
		pha
		lda #$35
		sta 1
		ldx #39
target:
		:
			.repeat 23,I
				lda vic_screen2+40*(I+2),x
				sta vic_screen2+40*(I+1),x
				lda $d800+40*(I+2),x
				sta $d800+40*(I+1),x
			.endrep
			dex
			bmi :+
fix:	
		jmp :-
:		pla
		sta 1
		rts
.endproc
.proc down
		lda 1
		pha
		lda #$35
		sta 1
		ldx #39
target:
		:
			.repeat 23,I
				lda vic_screen2+40*(23-I),x
				sta vic_screen2+40*(24-I),x
				lda $d800+40*(23-I),x
				sta $d800+40*(24-I),x
			.endrep
			dex
			bmi :+
fix:	
		jmp :-
:		pla
		sta 1
		rts
.endproc
.endproc

hardScrollUp = scrollers::up - scrollers + scrollers_base
hardScrollDown = scrollers::down - scrollers + scrollers_base

defineTextMode vdcTextMode, 32, 33
defineTextModeNTSC vdcTextModeNTSC, 32, 33

; ptr a,x
; size y
.proc setMode
		sta TMP+0
		stx TMP+1
		sty COUNT
		jsr vdc_waitVBL
		ldy #0
		:
			lda (TMP),y
			tax
			iny
			lda (TMP),y
			iny
			WriteVDC
			sm:cpy COUNT
		bne :-
		rts
.endproc

.proc vdc_waitVBL
		:lda VDCADR
		and #$20
		bne :-
		:lda VDCADR
		and #$20
		beq :-
		rts
.endproc

; a,y
.proc setAddress
		ldx #VDC_UAlo
		WriteVDC
		ldx #VDC_UAhi
		tya
		WriteVDC
		rts
.endproc

; a,y src
; pages x
.proc copyToVDC
		sta TMP+0
		sty TMP+1

		lda #VDC_DA
		sta VDCADR

		ldy #0
		pageLoop:
			:
				sm:lda (TMP),y
				:bit VDCADR
				bpl :-
				sta VDCDAT
				iny
			bne :--
			inc TMP+1
			dex
		bne pageLoop
		rts
.endproc

; a,y src
; x = nbrChars
.proc copyFontToVDC
		sta TMP+0
		sty TMP+1

		lda #VDC_DA
		sta VDCADR
		ldy #0
		charLoop:
			; Write 8
			.repeat 8,I
				lda (TMP),y
				:bit VDCADR
				bpl :-
				sta VDCDAT
				iny
			.endrep
			bne :+
				inc TMP+1
			:
			; Skip 8
			.repeat 8,I
				:bit VDCADR
				bpl :-
				lda VDCDAT
			.endrep
			dex
			bne :+
				rts
			:
		jmp charLoop
.endproc

.export automapVDC_init
.proc automapVDC_init
		lda vdcPresent
		bne :+
			rts
		:

		lda system
		cmp #SYSTEM_NTSC
		beq :+
		cmp #SYSTEM_NTSC_OLD
		beq :+
			lda #<vdcTextMode
			ldx #>vdcTextMode
			ldy #.sizeof(vdcTextMode)
			jmp :++
		:
			lda #<vdcTextModeNTSC
			ldx #>vdcTextModeNTSC
			ldy #.sizeof(vdcTextModeNTSC)
		:
		jsr setMode

		; Clear VDC
		lda #VDC_FILL
		ldx #VDC_VSS
		WriteVDC
		lda #0
		ldx #VDC_UAlo
		WriteVDC
		ldx #VDC_UAhi
		WriteVDC
		ldx #VDC_DA
		WriteVDC
		ldy #65
		ldx #VDC_WC
		:
			WriteVDC
			dey
		bpl :-
		rts
.endproc

.export automapVDC_copyFont
.proc automapVDC_copyFont
		lda vdcPresent
		bne :+
			rts
		:

		; Copy font
		lda #<vdcTextMode::font
		ldy #>vdcTextMode::font
		jsr setAddress
		lda #<font
		ldy #>font
		ldx #64
		jsr copyFontToVDC
		
		; Copy alternative font
		memcpy_pureram frameBuffer_bitmap, vic_font, $400
		lda #<vdcTextMode::alternateFont
		ldy #>vdcTextMode::alternateFont
		jsr setAddress
		lda #<frameBuffer_bitmap
		ldy #>frameBuffer_bitmap
		ldx #128
		jmp copyFontToVDC
.endproc

.export automapGo
.proc automapGo
		lda timersEnabled
		pha

		ldx #$5b
		ldy #(((automap_font/$400)&$f) | (((vic_screen2/$400)&$f)<<4))
		jsr textScreen_open

		lda partyPosition+0
		sta TMP
		lda partyPosition+1
		and #$3f
		lsr
		ror TMP
		lsr
		ror TMP
		lsr
		ror TMP
		lsr
		ror TMP
		lsr
		ror TMP
		sec
		lda TMP
		sbc #12
		sta SCROLLY
		jsr scrollBoundsCheck

		memcpy scrollers_base, scrollers, .sizeof(scrollers)
		lda #<(scrollers_base + scrollers::up::target - scrollers)
		sta scrollers_base + scrollers::up::fix+1 - scrollers
		lda #>(scrollers_base + scrollers::up::target - scrollers)
		sta scrollers_base + scrollers::up::fix+2 - scrollers
		lda #<(scrollers_base + scrollers::down::target - scrollers)
		sta scrollers_base + scrollers::down::fix+1 - scrollers
		lda #>(scrollers_base + scrollers::down::target - scrollers)
		sta scrollers_base + scrollers::down::fix+2 - scrollers

		jsr copyFont
		clc
		jsr automapRender

		lda #$ff
		sta screenEnabled

		jsr processKeyEvents

		jsr textScreen_close
		pla
		sta timersEnabled
		rts
.endproc

.proc font
		; Base wall pattern applied over 'wall tiles'
		f0=%01110111
		f1=%10101010
		f2=%11011101
		f3=%10101010
		f4=%01110111
		f5=%10101010
		f6=%11011101
		f7=%10101010

		.byte %00000000|f0
		.byte %00000000|f1
		.byte %00000000|f2
		.byte %00000000|f3
		.byte %00000000|f4
		.byte %00000000|f5
		.byte %00000000|f6
		.byte %00000000|f7

		.byte %11111111|f0
		.byte %00000000|f1
		.byte %00000000|f2
		.byte %00000000|f3
		.byte %00000000|f4
		.byte %00000000|f5
		.byte %00000000|f6
		.byte %00000000|f7

		.byte %00000001|f0
		.byte %00000001|f1
		.byte %00000001|f2
		.byte %00000001|f3
		.byte %00000001|f4
		.byte %00000001|f5
		.byte %00000001|f6
		.byte %00000001|f7

		.byte %11111111|f0
		.byte %00000001|f1
		.byte %00000001|f2
		.byte %00000001|f3
		.byte %00000001|f4
		.byte %00000001|f5
		.byte %00000001|f6
		.byte %00000001|f7

		.byte %00000000|f0
		.byte %00000000|f1
		.byte %00000000|f2
		.byte %00000000|f3
		.byte %00000000|f4
		.byte %00000000|f5
		.byte %11111111|f6
		.byte %11111111|f7

		.byte %11111111|f0
		.byte %00000000|f1
		.byte %00000000|f2
		.byte %00000000|f3
		.byte %00000000|f4
		.byte %00000000|f5
		.byte %11111111|f6
		.byte %11111111|f7

		.byte %00000001|f0
		.byte %00000001|f1
		.byte %00000001|f2
		.byte %00000001|f3
		.byte %00000001|f4
		.byte %00000001|f5
		.byte %11111111|f6
		.byte %11111111|f7

		.byte %11111111|f0
		.byte %00000001|f1
		.byte %00000001|f2
		.byte %00000001|f3
		.byte %00000001|f4
		.byte %00000001|f5
		.byte %11111111|f6
		.byte %11111111|f7

		.byte %11000000|f0
		.byte %11000000|f1
		.byte %11000000|f2
		.byte %11000000|f3
		.byte %11000000|f4
		.byte %11000000|f5
		.byte %11000000|f6
		.byte %11000000|f7

		.byte %11111111|f0
		.byte %11000000|f1
		.byte %11000000|f2
		.byte %11000000|f3
		.byte %11000000|f4
		.byte %11000000|f5
		.byte %11000000|f6
		.byte %11000000|f7

		.byte %11000001|f0
		.byte %11000001|f1
		.byte %11000001|f2
		.byte %11000001|f3
		.byte %11000001|f4
		.byte %11000001|f5
		.byte %11000001|f6
		.byte %11000001|f7

		.byte %11111111|f0
		.byte %11000001|f1
		.byte %11000001|f2
		.byte %11000001|f3
		.byte %11000001|f4
		.byte %11000001|f5
		.byte %11000001|f6
		.byte %11000001|f7

		.byte %11000000|f0
		.byte %11000000|f1
		.byte %11000000|f2
		.byte %11000000|f3
		.byte %11000000|f4
		.byte %11000000|f5
		.byte %11111111|f6
		.byte %11111111|f7

		.byte %11111111|f0
		.byte %11000000|f1
		.byte %11000000|f2
		.byte %11000000|f3
		.byte %11000000|f4
		.byte %11000000|f5
		.byte %11111111|f6
		.byte %11111111|f7

		.byte %11000001|f0
		.byte %11000001|f1
		.byte %11000001|f2
		.byte %11000001|f3
		.byte %11000001|f4
		.byte %11000001|f5
		.byte %11111111|f6
		.byte %11111111|f7

		.byte %11111111|f0
		.byte %11000001|f1
		.byte %11000001|f2
		.byte %11000001|f3
		.byte %11000001|f4
		.byte %11000001|f5
		.byte %11111111|f6
		.byte %11111111|f7

empty:
		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00000000

lever:
button:
		.byte %00011000
		.byte %00011000
		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00000000

		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00000011
		.byte %00000011
		.byte %00000000
		.byte %00000000
		.byte %00000000

		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00011000
		.byte %00011000

		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %11000000
		.byte %11000000
		.byte %00000000
		.byte %00000000
		.byte %00000000

portal:
inscription:
rune:
		.byte %00111100
		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00000000

		.byte %00000000
		.byte %00000000
		.byte %00000001
		.byte %00000001
		.byte %00000001
		.byte %00000001
		.byte %00000000
		.byte %00000000

		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00111100

		.byte %00000000
		.byte %00000000
		.byte %10000000
		.byte %10000000
		.byte %10000000
		.byte %10000000
		.byte %00000000
		.byte %00000000

hole:
		.byte %00000000
		.byte %00000000
		.byte %00111100
		.byte %01100110
		.byte %01100110
		.byte %00111100
		.byte %00000000
		.byte %00000000

pressurePlate:
		.byte %00000000
		.byte %00000000
		.byte %00111100
		.byte %01111110
		.byte %01111110
		.byte %00111100
		.byte %00000000
		.byte %00000000

door:
		.byte %00011000
		.byte %00011000
		.byte %00011000
		.byte %00000000
		.byte %00000000
		.byte %00011000
		.byte %00011000
		.byte %00011000

		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %11100111
		.byte %11100111
		.byte %00000000
		.byte %00000000
		.byte %00000000

down:
		.byte %00000000
		.byte %01111110
		.byte %01111110
		.byte %00000000
		.byte %00111100
		.byte %00000000
		.byte %00011000
		.byte %00000000

up:
		.byte %00000000
		.byte %00011000
		.byte %00000000
		.byte %00111100
		.byte %00000000
		.byte %01111110
		.byte %01111110
		.byte %00000000

alcove:
		.byte %01111110
		.byte %01000010
		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00000000

		.byte %00000000
		.byte %00000011
		.byte %00000001
		.byte %00000001
		.byte %00000001
		.byte %00000001
		.byte %00000011
		.byte %00000000

		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %01000010
		.byte %01111110

		.byte %00000000
		.byte %11000000
		.byte %10000000
		.byte %10000000
		.byte %10000000
		.byte %10000000
		.byte %11000000
		.byte %00000000

socket:
keyhole:
		.byte %00111000
		.byte %00101000
		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00000000

		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00000011
		.byte %00000001
		.byte %00000011
		.byte %00000000
		.byte %00000000

		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00101000
		.byte %00111000

		.byte %00000000
		.byte %00000000
		.byte %11000000
		.byte %10000000
		.byte %11000000
		.byte %00000000
		.byte %00000000
		.byte %00000000

laucher:
		.byte %00011000
		.byte %00111100
		.byte %00100100
		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00000000

		.byte %00000000
		.byte %00000000
		.byte %00000110
		.byte %00000011
		.byte %00000011
		.byte %00000110
		.byte %00000000
		.byte %00000000

		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00100100
		.byte %00111100
		.byte %00011000

		.byte %00000000
		.byte %00000000
		.byte %01100000
		.byte %11000000
		.byte %11000000
		.byte %01100000
		.byte %00000000
		.byte %00000000

pidestal:
		.byte %00000000
		.byte %00000000
		.byte %00100100
		.byte %00011000
		.byte %00011000
		.byte %00100100
		.byte %00000000
		.byte %00000000

teleporter:
		.byte %00000001
		.byte %01000100
		.byte %11100000
		.byte %01001000
		.byte %00011100
		.byte %10001000
		.byte %00000010
		.byte %00100000

hiddenWall:
		.byte %11111111
		.byte %11010101
		.byte %10101011
		.byte %11010101
		.byte %10101011
		.byte %11010101
		.byte %10101011
		.byte %11111111
.endproc

_ALCOVE = (font::alcove - font)/8
_BUTTON = (font::button - font)/8
_INSCRIPTION = (font::inscription - font)/8
_LEVER = (font::lever - font)/8
_PRESSUREPLATE = (font::pressurePlate - font)/8
_SOCKET = (font::socket - font)/8
_RUNE = (font::rune - font)/8
_KEYHOLE = (font::keyhole - font)/8
_HOLE = (font::hole - font)/8
_LAUNCHER = (font::laucher - font)/8
_PIDESTAL = (font::pidestal - font)/8
_HIDDENWALL = (font::hiddenWall - font)/8

.proc setVDCCursorPosition
		clc
		lda partyPosition+0
		adc #<(vdcTextMode::screen+32)
		ldx #VDC_CPlo
		WriteVDC
		lda partyPosition+1
		adc #>(vdcTextMode::screen+32)
		ldx #VDC_CPhi
		WriteVDC
		rts		
.endproc

.proc setSpritePosition
		lda partyPosition+0
		sta TMP
		and #31
		asl
		asl
		asl
		clc
		adc #24+4*8
		sta $d000+2*2
		lda #0
		bcc :+
			lda #(1<<2)
		:
		sta _d010

		lda partyPosition+1
		and #$3f
		lsr
		ror TMP
		lsr
		ror TMP
		lsr
		ror TMP
		lsr
		ror TMP
		lsr
		lda TMP
		ror
		sec
		sbc SCROLLY
		cmp #1
		bmi :+
		cmp #25
		bpl :+
			asl
			asl
			asl
			clc
			adc #50
			sta $d001+2*2
			lda #4
			sta _d015
			rts
		:
		lda #0
		sta _d015
		rts
.endproc

.proc blitVICRow
		jsr setupDestinationPointers
		ldy #0
		:
			lda renderedRowChars,y
			sta (DST),y
			lda renderedRowColors,y
			sta (DST_D800),y
			iny
			cpy #32
		bne :-
		rts
.endproc

.proc blitVDCRow
		lda vdcPresent
		bne :+
			rts
		:

		; a,y = x*32
		lda #0
		sta TMP
		txa
		asl
		rol TMP
		asl
		rol TMP
		asl
		rol TMP
		asl
		rol TMP
		asl
		rol TMP
		ldy TMP
		pha
		clc
		adc #<(vdcTextMode::screen)
		pha
		lda TMP
		adc #>(vdcTextMode::screen)
		tay
		pla
		jsr setAddress

		lda #VDC_DA
		sta VDCADR
		ldy #0
		:
			lda renderedRowChars,y
			and #$3f
			:bit VDCADR
			bpl :-
			sta VDCDAT
			iny
			cpy #32
		bne :--

		clc
		pla
		adc #<(vdcTextMode::attributes)
		pha
		lda TMP
		adc #>(vdcTextMode::attributes)
		tay
		pla
		jsr setAddress

		lda #VDC_DA
		sta VDCADR
		ldy #0
		:
			lda renderedRowColors,y
			lsr
			lsr
			lsr
			lsr
			:bit VDCADR
			bpl :-
			sta VDCDAT
			iny
			cpy #32
		bne :--

		rts

.endproc

.export automapVDCUpdate 
.proc automapVDCUpdate
		lda vdcPresent
		bne :+
			rts
		:

		; Calc update box
		ldx partyDirection
		lda scanWidth,x
		sta WIDTH
		lda scanHeight,x
		sta HEIGHT
		clc
		lda partyPosition+0
		adc startOffsetLo,x
		sta DST+0
		sta ROW
		and #$1f
		sta COL

		; Calculate start row index
		lda partyPosition+1
		adc startOffsetHi,x
		and #>(32*32-1)
		sta DST+1
		asl ROW
		rol
		asl ROW
		rol
		asl ROW
		rol
		sta ROW

		; Setup rendered src pointers
		clc
		lda COL
		adc #<renderedRowChars
		sta SRC+0
		lda #0
		adc #>renderedRowChars
		sta SRC+1
		clc
		lda COL
		adc #<renderedRowColors
		sta SRC2+0
		lda #0
		adc #>renderedRowColors
		sta SRC2+1

		; Render rows
		lda ROW
		rowLoop:
			and #$1f
			pha
			tax
			jsr setupSourcePointers

			; Render columns
			lda WIDTH
			sta COUNT
			ldy COL
			:
				jsr renderVDCChar
				iny
				dec COUNT
			bne :-

			; Blit VDC chars
			clc
			lda DST+0
			adc #<(vdcTextMode::screen+32)
			pha
			lda DST+1
			adc #>(vdcTextMode::screen+32)
			tay
			pla
			jsr setAddress
			lda #VDC_DA
			sta VDCADR
			ldy #0
			:
				lda (SRC),y
				:bit VDCADR
				bpl :-
				sta VDCDAT
				iny
				cpy WIDTH
			bne :--

			; Blit VDC colors
			clc
			lda DST+0
			adc #<(vdcTextMode::attributes+32)
			pha
			lda DST+1
			adc #>(vdcTextMode::attributes+32)
			tay
			pla
			jsr setAddress
			lda #VDC_DA
			sta VDCADR
			ldy #0
			:
				lda (SRC2),y
				:bit VDCADR
				bpl :-
				sta VDCDAT
				iny
				cpy WIDTH
			bne :--

			; Next VDC row
			clc
			lda DST+0
			adc #32
			sta DST+0
			lda DST+1
			adc #0
			and #>(32*32-1)
			sta DST+1

			; Next source row
			pla
			adc #1
			dec HEIGHT
			beq :+
		jmp rowLoop
		:jmp setVDCCursorPosition

		; -North
		; Start:	-3*32-3
		; Width:	7
		; Height:	4
		;
		; -East
		; Start:	-3*32
		; Witdh:	4
		; Height:	7
		;
		; -South
		; Start:	-3
		; Width:	7
		; Height:	4
		;
		; -West
		; Start:	-3*32-3
		; Width:	4
		; Height:	7
startOffsetLo:	.byte <(-3*32-3), <(-3*32), <(-3), <(-3*32-3)
startOffsetHi:	.byte >(-3*32-3), >(-3*32), >(-3), >(-3*32-3)
scanWidth:		.byte 7,4,7,4
scanHeight:		.byte 4,7,4,7
.endproc

; c=1, render VDC-only
.export automapRender 
.proc automapRender
		php
		bcs :+
			jsr renderPlayerSprite
			jsr renderLevelName
			jmp :+++
		:
			lda vdcPresent
			bne :+
				plp
				rts
			:
			jsr renderLevelNameVDC
		:

		; Install dec tables
		ldx levelIndex
		dex
		lda wall_dec_table_lo,x
		sta TMP+0
		lda wall_dec_table_hi,x
		sta TMP+1
		lda floor_dec_table_lo,x
		sta TMP2+0
		lda floor_dec_table_hi,x
		sta TMP2+1
		ldy #(MAX_NBR_DECORATIONS-1)
		:
			lda (TMP),y
			sta WALL_DEC_TABLE,y
			lda (TMP2),y
			sta FLOOR_DEC_TABLE,y
			dey
		bpl :-

		plp
		bcs vdc_only

	; Render all rows
	.proc quick
		lda #0
		sta _d015
		lda vdcPresent
		beq noVDC
		
		ldx #0
		rowLoop:
			txa
			pha

			jsr setupSourcePointers
			jsr renderRow

			pla
			pha
			tax
			inx
			jsr blitVDCRow

			pla
			pha
			sec
			sbc SCROLLY
			tax
			beq :+
			bmi :+
			cpx #25
			bpl :+
				jsr blitVICRow
			:

			pla
			tax

			inx
			cpx #32
		bne rowLoop
		jsr setVDCCursorPosition
		jmp setSpritePosition

noVDC:
		ldx #1
		:
			jsr renderOrClearRow
			jsr blitVICRow
			inx
			cpx #25
		bne :-
		jmp setSpritePosition
	.endproc

	; Render all rows
	.proc vdc_only
		lda #$10
		ldx #VDC_FGBG
		WriteVDC

		ldx #0
		rowLoop:
			txa
			pha

			jsr setupSourcePointers
			jsr renderRow

			pla
			pha
			tax
			inx
			jsr blitVDCRow

			pla
			tax

			inx
			cpx #32
		bne rowLoop

		jmp setVDCCursorPosition
	.endproc

	.proc quickTop
		lda #0
		sta _d015
		ldx #1
		jsr renderOrClearRow
		jsr blitVICRow
		jmp setSpritePosition
	.endproc

	.proc quickBottom
		lda #0
		sta _d015
		ldx #24
		jsr renderOrClearRow
		jsr blitVICRow
		jmp setSpritePosition
	.endproc

	.proc renderOrClearRow
		txa
		pha

		clc
		adc SCROLLY
		tax
		bmi :+
		cpx #32
		bpl :+
			jsr setupSourcePointers
			jsr renderRow
			jmp :++
		:
			jsr clearRow
		:

		pla
		tax
		rts
	.endproc

wall_dec_table_lo:
		.byte <brick1_wall_dec_table
		.byte <brick2_wall_dec_table
		.byte <brick3_wall_dec_table
		.byte <blue_wall_dec_table
		.byte <blue_wall_dec_table
		.byte <blue_wall_dec_table
		.byte <drow_wall_dec_table
		.byte <drow_wall_dec_table
		.byte <drow_wall_dec_table
		.byte <green_wall_dec_table
		.byte <green_wall_dec_table
		.byte <xanatha_wall_dec_table

wall_dec_table_hi:
		.byte >brick1_wall_dec_table
		.byte >brick2_wall_dec_table
		.byte >brick3_wall_dec_table
		.byte >blue_wall_dec_table
		.byte >blue_wall_dec_table
		.byte >blue_wall_dec_table
		.byte >drow_wall_dec_table
		.byte >drow_wall_dec_table
		.byte >drow_wall_dec_table
		.byte >green_wall_dec_table
		.byte >green_wall_dec_table
		.byte >xanatha_wall_dec_table

floor_dec_table_lo:
		.byte <brick1_floor_dec_table
		.byte <brick2_floor_dec_table
		.byte <brick3_floor_dec_table
		.byte <blue_floor_dec_table
		.byte <blue_floor_dec_table
		.byte <blue_floor_dec_table
		.byte <drow_floor_dec_table
		.byte <drow_floor_dec_table
		.byte <drow_floor_dec_table
		.byte <green_floor_dec_table
		.byte <green_floor_dec_table
		.byte <xanatha_floor_dec_table

floor_dec_table_hi:
		.byte >brick1_floor_dec_table
		.byte >brick2_floor_dec_table
		.byte >brick3_floor_dec_table
		.byte >blue_floor_dec_table
		.byte >blue_floor_dec_table
		.byte >blue_floor_dec_table
		.byte >drow_floor_dec_table
		.byte >drow_floor_dec_table
		.byte >drow_floor_dec_table
		.byte >green_floor_dec_table
		.byte >green_floor_dec_table
		.byte >xanatha_floor_dec_table

brick1_wall_dec_table:	.byte 0,0,0,0,_ALCOVE,0,0,0,0,0,0,0,_BUTTON,_INSCRIPTION,_LEVER,_LEVER,0,_BUTTON,_BUTTON,0,0,_RUNE,0
brick1_floor_dec_table:	.byte 0,0,0,_PRESSUREPLATE,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
brick2_wall_dec_table:	.byte 0,0,0,0,0,_ALCOVE,0,0,_BUTTON,_INSCRIPTION,_KEYHOLE,_KEYHOLE,_LEVER,_LEVER,_INSCRIPTION,0,_BUTTON,_BUTTON,0,0,_RUNE,_INSCRIPTION
brick2_floor_dec_table:	.byte 0,0,0,_HOLE,_PRESSUREPLATE,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
brick3_wall_dec_table:	.byte 0,0,0,0,0,_ALCOVE,_SOCKET,_SOCKET,_SOCKET,_SOCKET,_SOCKET,0,0,_KEYHOLE,_BUTTON,_BUTTON,_BUTTON,_BUTTON,_BUTTON,_INSCRIPTION,_KEYHOLE,_LEVER,_LEVER,_INSCRIPTION,0,_BUTTON,_BUTTON
brick3_floor_dec_table:	.byte 0,0,0,_HOLE,_PRESSUREPLATE,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
blue_wall_dec_table:	.byte 0,0,0,0,_LEVER,_LEVER,0,_LEVER,_LEVER,_LAUNCHER,0,0,0,0,0,0,_BUTTON,_INSCRIPTION,_KEYHOLE,_KEYHOLE,_LEVER,_LEVER,0,0,0,_RUNE
blue_floor_dec_table:	.byte 0,_HOLE,_PRESSUREPLATE,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
drow_wall_dec_table:	.byte 0,0,0,_ALCOVE,_BUTTON,_SOCKET,0,0,0,0,0,0,0,_LAUNCHER,_LAUNCHER,_INSCRIPTION,_KEYHOLE,_KEYHOLE,_LEVER,_LEVER,0,_BUTTON,_BUTTON,_BUTTON,_BUTTON
drow_floor_dec_table:	.byte 0,_HOLE,_PRESSUREPLATE,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
green_wall_dec_table:	.byte 0,0,0,_ALCOVE,0,_BUTTON,_BUTTON,_BUTTON,_BUTTON,0,0,0,0,_LAUNCHER,_INSCRIPTION,_KEYHOLE,_LEVER,_LEVER,_BUTTON,_BUTTON,0,0,0,0
green_floor_dec_table:	.byte 0,_HOLE,_PRESSUREPLATE,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
xanatha_wall_dec_table:	.byte 0,0,0,0,0,0,_ALCOVE,_LAUNCHER,0,0,_LEVER,_LEVER,0,0,0,_INSCRIPTION,_KEYHOLE,0,_BUTTON,_BUTTON,0,0
xanatha_floor_dec_table:.byte _PIDESTAL,0,0,_PIDESTAL,0,_PRESSUREPLATE,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

.endproc

.macro DEFKEY key,key2,callback,value
	.byte key, key2, <callback, >callback, value
.endmacro

.proc numberToBCD
		sta ARGS+0
		lda #0
		sta ARGS+1
		sta ARGS+2
		sta ARGS+3
		jsrf htd32

		lda RESULT+8
		asl
		asl
		asl
		asl
		ora RESULT+9
		sta TMP+0
		lda RESULT+7
		sta TMP+1
		rts
.endproc

; Prints the map coord under the mouse
; Returns C=1 if button was clicked, otherwise C=0
.proc printCoordinatesUnderMouse
		lda currentypos
		cmp #8
		bcs :+
			rts
		:

		; Setup text screen 2 pointers and color
		lda #<vic_screen2
		sta TEXT2_SCREEN+0
		lda #>vic_screen2
		sta TEXT2_SCREEN+1
		lda #<$d800
		sta TEXT2_D800+0
		lda #>$d800
		sta TEXT2_D800+1
		lda #0
		sta textCol2
		sta textRow2
		lda #1
		sta textColor2

		; Print X-coord
		lda currentxpos+1
		sta TMP+0
		lda currentxpos+0
		lsr TMP+0
		ror
		lsr TMP+0
		ror
		lsr TMP+0
		ror
		clc
		sbc #3
		bpl :+
			lda #0
		:
		cmp #31
		bcc :+
			lda #31
		:
		pha
		jsr numberToBCD
		ldx #2
		jsrf printNumber

		; Print colon
		lda #':'
		sta CUR_CHAR
		jsrf putChar2

		; Print Y-coors
		sec
		lda currentypos
		lsr
		lsr
		lsr
		clc
		adc SCROLLY
		pha
		jsr numberToBCD
		ldx #2
		jsrf printNumber

		; Check mouse button
		lda MOUSEEVENT+MouseEvent::buttons
		bmi :++
			lda partyDirection
			sta ARGS+2
			lda #0
			sta ARGS+1
			pla ;Y-coord
			asl
			rol ARGS+1
			asl
			rol ARGS+1
			asl
			rol ARGS+1
			asl
			rol ARGS+1
			asl
			rol ARGS+1
			sta ARGS+0
			pla ;X-coord
			clc
			adc ARGS+0
			sta ARGS+0
			bcc :+
				inc ARGS+1
			:
			jsrf gameState_teleportParty
			sec
			rts
		:
		pla
		pla
		clc
		rts
.endproc

.proc processKeyEvents
		lda #$ff
		sta MOUSEEVENT+MouseEvent::buttons
		lda #0
		sta KEYEVENT+KeyEvent::keyPressed
		sta KEYEVENT+KeyEvent::normalChar
		sta KEYEVENT+KeyEvent::specialChar
		sta KEYEVENT+KeyEvent::modifiers

		:
			lda godMode
			beq :+
				jsr printCoordinatesUnderMouse
				bcs escape
			:
			lda KEYEVENT+KeyEvent::keyPressed
		beq :--

		lda KEYEVENT+KeyEvent::modifiers
		and #$80
		beq :+
			jmp escape
		:

		ldx #$00
	nextKey:
		lda keyboardActionList+1,x
		beq :+
			cmp KEYEVENT+KeyEvent::specialChar
			beq match
		:
		lda KEYEVENT+KeyEvent::normalChar
		cmp keyboardActionList+0,x
		bne noMatch
		match:
			lda keyboardActionList+2,x
		sta INDJMP+0
		lda keyboardActionList+3,x
    		sta INDJMP+1
	    	lda keyboardActionList+4,x
		tax
		jmp (INDJMP)
		noMatch:
			inx
	       		inx
	       		inx
			inx
			inx
			lda keyboardActionList+0,x
			ora keyboardActionList+1,x
	  	bne nextKey

	  	jmp processKeyEvents

escape:
		rts

cursor:
		lda KEYEVENT+KeyEvent::modifiers
		and #$50
		beq down
		up:
			dec SCROLLY
			jsr scrollBoundsCheck
			bne nos
			jsr hardScrollDown
			jsr automapRender::quickTop
			jmp nos

		down:
			inc SCROLLY
			jsr scrollBoundsCheck
			bne nos
			jsr hardScrollUp
			jsr automapRender::quickBottom

nos:	jmp processKeyEvents

keyboardActionList:
	   DEFKEY $1f, 0, escape, 0
       DEFKEY $0d, 0, escape, 0
       DEFKEY 0, $80, cursor, 0
       .word 0
.endproc

.proc scrollBoundsCheck
		lda #0
		sta TMP
		lda SCROLLY
		cmp #<-1
		bpl :+
		    lda #<-1
		    inc TMP
		:
		cmp #8
		bmi :+
		    lda #7
		    inc TMP
		:
		sta SCROLLY
		lda TMP
		rts
.endproc

.proc renderPlayerSprite
		lda #0
		ldx #62
		:
			sta automap_spr,x
			dex
		bpl :-
		
		lda partyDirection
		and #3
		tax
		lda _lo,x
		sta TMP+0
		lda _hi,x
		sta TMP+1

		ldx #0
		ldy #0
		:
			lda (TMP),y
			sta automap_spr,x
			inx
			inx
			inx
			iny
			cpy #8
		bne :-
		rts
_lo:	.byte <_00,<_01,<_02,<_03
_hi:	.byte >_00,>_01,>_02,>_03
_00:	.byte %00011000
		.byte %00111100
		.byte %01111110
		.byte %11111111
		.byte %00011000
		.byte %00011000
		.byte %00011000
		.byte %00011000

_01:	.byte %00001000
		.byte %00001100
		.byte %00001110
		.byte %11111111
		.byte %11111111
		.byte %00001110
		.byte %00001100
		.byte %00001000

_02:	.byte %00011000
		.byte %00011000
		.byte %00011000
		.byte %00011000
		.byte %11111111
		.byte %01111110
		.byte %00111100
		.byte %00011000

_03:	.byte %00010000
		.byte %00110000
		.byte %01110000
		.byte %11111111
		.byte %11111111
		.byte %01110000
		.byte %00110000
		.byte %00010000
.endproc

.proc renderLevelName
		ldx levelIndex
		dex
		lda _lo,x
		sta TMP+0
		lda _hi,x
		sta TMP+1
		ldy #0
		lda (TMP),y
		iny
		lsr
		tax
		lda #$8
		bcc :+
			lda #$c
		:
		sta titleD016

		:
			lda (TMP),y
			beq :+
			sta vic_screen2,x
			inx
			iny
		jmp :-
		:
		rts

_lo:	.byte <_01,<_02,<_03,<_04,<_05,<_06,<_07,<_08,<_09,<_10,<_11,<_12
_hi:	.byte >_01,>_02,>_03,>_04,>_05,>_06,>_07,>_08,>_09,>_10,>_11,>_12
_01:	.byte (320-17*8)/8,"OBERE ABWASSERKAN",93,"LE",0
_02:	.byte (320-18*8)/8,"MITTLERE ABWASSERKAN",93,"LE",0
_03:	.byte (320-17*8)/8,"UNTERE ABWASSERKAN",93,"LE",0
_04:	.byte (320-25*8)/8,"OBERE EBENE DER ZWERGENRUINEN",0
_05:	.byte (320-22*8)/8,"ZWERGENRUINEN UND LAGER",0
_06:	.byte (320-29*8)/8,"UNTERE EBENE DER ZWERGENRUINEN",0
_07:	.byte (320-25*8)/8,"OBERE GEBIETE DER DUNKELELFEN",0
_08:	.byte (320-13*8)/8,"GEBIETE DER DUNKELELFEN",0
_09:	.byte (320-25*8)/8,"UNTERE GEBIETE DER DUNKELELFEN",0
_10:	.byte (320-37*8)/8,"NEST DER MANTIS KRIEGER",0
_11:	.byte (320-39*8)/8,"UNTERE AUSL",93,"UFER",0
_12:	.byte (320-24*8)/8,"XANATHARS HEILIGE HALLEN",0
.endproc

.proc renderLevelNameVDC
		ldx levelIndex
		dex
		lda _lo,x
		sta TMP+0
		lda _hi,x
		sta TMP+1

		lda #<vdcTextMode::screen
		ldy #>vdcTextMode::screen
		jsr setAddress
		lda #VDC_DA
		sta VDCADR
		ldy #0
		:
			lda (TMP),y
			:bit VDCADR
			bpl :-
			sta VDCDAT
			iny
			cpy #32
		bne :--

		lda #<vdcTextMode::attributes
		ldy #>vdcTextMode::attributes
		jsr setAddress
		lda #VDC_DA
		sta VDCADR
		lda #$c0|(TITLE_BGCOLOR>>4)
		ldy #31
		:
			:bit VDCADR
			bpl :-
			sta VDCDAT
			dey
		bpl :--

		rts

_lo:	.byte <_01,<_02,<_03,<_04,<_05,<_06,<_07,<_08,<_09,<_10,<_11,<_12
_hi:	.byte >_01,>_02,>_03,>_04,>_05,>_06,>_07,>_08,>_09,>_10,>_11,>_12
_01:	.byte "      OBERE ABWASSERKAN",93,"LE      "
_02:	.byte "     MITTLERE ABWASSERKAN",93,"LE    "
_03:	.byte "     UNTERE ABWASSERKAN",93,"LE      "
_04:	.byte " OBERE EBENE DER ZWERGENRUINEN  "
_05:	.byte "    ZWERGENRUINEN UND LAGER     "
_06:	.byte " UNTERE EBENE DER ZWERGENRUINEN "
_07:	.byte " OBERE GEBIETE DER DUNKELELFEN  "
_08:	.byte "    GEBIETE DER DUNKELELFEN     "
_09:	.byte " UNTERE GEBIETE DER DUNKELELFEN "
_10:	.byte "    NEST DER MANTIS KRIEGER     "
_11:	.byte "        UNTERE AUSL",93,"UFER        "
_12:	.byte "    XANATHARS HEILIGE HALLEN    "
.endproc

.proc clearRow
		ldy #31
		lda #0
		:
			sta renderedRowChars,y
			dey
		bpl :-
		rts
.endproc

.proc renderRow
		ldy #31
		colLoop:
			sty YREG
			ldx mul4,y
			stx YREG4

			; Render background color
			lda (CPTRv),y
			and #$80
			sta TMP
			bmi :+
				jsr renderWalls
				jmp :++
			:
				jsr renderIcon
				stx TMP
			:

			ldy YREG
			sta renderedRowColors,y
			lda TMP
			sta renderedRowChars,y
			dey
		bpl colLoop
		rts

renderVDCChar:
		sty YREG

		ldx mul4,y
		stx YREG4

		; Render background color
		lda (CPTRv),y
		and #$80
		sta TMP
		bmi :+
			jsr renderWalls
			jmp :++
		:
			jsr renderIcon
			stx TMP
		:

		ldy YREG
		lsr
		lsr
		lsr
		lsr
		sta renderedRowColors,y
		lda TMP
		and #$3f
		sta renderedRowChars,y
		rts
		
mul4:	.repeat 32+8,I ;+8 for VDC overrun
			.byte I*4
		.endrep

renderIcon:
		; Handle door icon
		ldy YREG4
		lax (CPTRm),y
		lda wallFlags,x
		and #WALLFLAG_ISDOOR
		beq :+
			ldx #(((font::door - font)/8+1)|$80)
			lda #DOOR_COLOR
			rts
		:
		iny
		lax (CPTRm),y
		lda wallFlags,x
		and #WALLFLAG_ISDOOR
		beq :+
			ldx #(((font::door - font)/8+0)|$80)
			lda #DOOR_COLOR
			rts
		:

		; Handle floor icon
		ldy wallDecoration,x
		bmi :+
			jsr drawFloorIcon
		:

		; Handle wall icons
		ldy YREG4
		lax (NPTRm),y
		lda wallType,x
		cmp #4
		bcs :+
		ldy wallDecoration,x
		bmi :++
		:	ldx #0
			jsr drawWallIcon
		:

		ldy YREG4
		lax (EPTRm),y
		lda wallType,x
		cmp #4
		bcs :+
		ldy wallDecoration,x
		bmi :++
		:	ldx #1
			jsr drawWallIcon
		:

		ldy YREG4
		lax (SPTRm),y
		lda wallType,x
		cmp #4
		bcs :+
		ldy wallDecoration,x
		bmi :++
		:	ldx #2
			jsr drawWallIcon
		:

		ldy YREG4
		lax (WPTRm),y
		lda wallType,x
		cmp #4
		bcs :+
		ldy wallDecoration,x
		bmi :++
		:	ldx #3
			jsr drawWallIcon
		:

		; Handle teleporter
		ldy YREG4
		lda #$34
		cmp (CPTRm),y
		beq :+
		iny
		cmp (CPTRm),y
		beq :+
		iny
		cmp (CPTRm),y
		beq :+
		iny
		cmp (CPTRm),y
		bne :++
		:
			ldx #(((font::teleporter - font)/8)|$80)
			lda #TELEPORTER_COLOR
			rts			
		:

		ldy YREG4
		lda (CPTRm),y
		iny
		ora (CPTRm),y
		iny
		ora (CPTRm),y
		iny
		ora (CPTRm),y
		bne :+
			ldx #(((font::empty - font)/8)|$80)
			lda #VISITED_COLOR
			rts
		:
		ldx #(((font::hiddenWall - font)/8)|$80)
		lda #HIDDENWALL_COLOR
		rts

; y=decoration index
drawFloorIcon:
		lda FLOOR_DEC_TABLE,y
		beq :+++
			ldy #ICON_COLOR
			cmp #<((font::pressurePlate - font)/8)
			beq :+
			cmp #<((font::hole - font)/8)
			bne :++
				:ldy #HOLE_PLATE_COLOR
			:
			ora #$80
			tax
			pla
			pla
			tya
		:rts

; x=direction
; y=decoration index
drawWallIcon:
		cmp #4 ;up
		bne :+
			ldx #(((font::up - font)/8)|$80)
			pla
			pla
			lda #STAIR_COLOR
			rts
		:
		cmp #5 ;down
		bne :+
			ldx #(((font::down - font)/8)|$80)
			pla
			pla
			lda #STAIR_COLOR
			rts
		:
		cmp #6 ;portal
		bne :+
			clc
			txa
			adc #(((font::portal - font)/8)|$80)
			tax
			pla
			pla
			lda #PORTAL_COLOR
			rts
		:		

		lda WALL_DEC_TABLE,y
		beq :+
			clc
			txa
			adc WALL_DEC_TABLE,y
			ora #$80
			tax
			pla
			pla
			lda #ICON_COLOR
		:rts

renderWalls:
		lda (NPTRv),y
		sta NTMPv
		lda (EPTRv),y
		sta ETMPv
		lda (SPTRv),y
		sta STMPv
		lda (WPTRv),y
		sta WTMPv

		ldy YREG4

		lda NTMPv
		bpl :+
			lax (CPTRm),y
			lda wallType,x
			beq :+
				lda wallFlags,x
				and #WALLFLAG_ISDOOR
				bne :+
					lda TMP
					ora #1
					sta TMP
		:

		iny
		lda ETMPv
		bpl :+
			lax (CPTRm),y
			lda wallType,x
			beq :+
				lda wallFlags,x
				and #WALLFLAG_ISDOOR
				bne :+
					lda TMP
					ora #2
					sta TMP
		:

		iny
		lda STMPv
		bpl :+
			lax (CPTRm),y
			lda wallType,x
			beq :+
				lda wallFlags,x
				and #WALLFLAG_ISDOOR
				bne :+
					lda TMP
					ora #4
					sta TMP
		:

		iny
		lda WTMPv
		bpl :+
			lax (CPTRm),y
			lda wallType,x
			beq :+
				lda wallFlags,x
				and #WALLFLAG_ISDOOR
				bne :+
					lda TMP
					ora #8
					sta TMP
		:

		lda #WALL_COLOR
		rts
.endproc
renderVDCChar = renderRow::renderVDCChar

; x=row
.proc setupDestinationPointers
		lda screenRow_lo,x
		sta DST+0
		lda screenRow_hi,x
		sta DST+1
		lda d800Row_lo,x
		sta DST_D800+0
		lda d800Row_hi,x
		sta DST_D800+1
		rts
		
screenRow_lo:
		.repeat 25,I
			.byte <(vic_screen2+I*40+4)
		.endrep

screenRow_hi:
		.repeat 25,I
			.byte >(vic_screen2+I*40+4)
		.endrep

d800Row_lo:
		.repeat 25,I
			.byte <($d800+I*40+4)
		.endrep

d800Row_hi:
		.repeat 25,I
			.byte >($d800+I*40+4)
		.endrep
.endproc

; x=row
.proc setupSourcePointers
		; 5 maze pointers (setup per row rendered to handle proper wrap-around):
		;   CPTRm = maze + partyPosition*4
		;   NPTRm = CPTRm - (32*4) + SOUTH
		;   EPTRm = CPTRm + 4 + WEST
		;   SPTRm = CPTRm + (32*4) + NORTH
		;   WPTRm = CPTRm - 4 + EAST
		;
		; 5 visited pointers (setup per row rendered to handle proper wrap-around):
		;   CPTRv = triggerIndices + partyPosition
		;   NPTRv = CPTRv - 32
		;   EPTRv = CPTRv + 1
		;   SPTRv = CPTRv + 32
		;   WPTRv = CPTRv - 1

		txa
		pha

		clc
		lda mazeRow_lo,x
		sta CPTRm+0
		adc #<(4+WEST)
		sta EPTRm+0
		lda mazeRow_hi,x
		sta CPTRm+1
		adc #>(4+WEST)
		sta EPTRm+1

		clc
		lda visitedRow_lo,x
		sta CPTRv+0
		adc #<1
		sta EPTRv+0
		lda visitedRow_hi,x
		sta CPTRv+1
		adc #>1
		sta EPTRv+1

		sec
		lda mazeRow_lo,x
		sbc #<(4-EAST)
		sta WPTRm+0
		lda mazeRow_hi,x
		sbc #>(4-EAST)
		sta WPTRm+1

		sec
		lda visitedRow_lo,x
		sbc #<1
		sta WPTRv+0
		lda visitedRow_hi,x
		sbc #>1
		sta WPTRv+1

		dex
		txa
		and #31
		tax

		clc
		lda mazeRow_lo,x
		adc #<SOUTH
		sta NPTRm+0
		lda mazeRow_hi,x
		adc #>SOUTH
		sta NPTRm+1

		lda visitedRow_lo,x
		sta NPTRv+0
		lda visitedRow_hi,x
		sta NPTRv+1

		inx
		inx
		txa
		and #31
		tax

		clc
		lda mazeRow_lo,x
		adc #<NORTH
		sta SPTRm+0
		lda mazeRow_hi,x
		adc #>NORTH
		sta SPTRm+1

		lda visitedRow_lo,x
		sta SPTRv+0
		lda visitedRow_hi,x
		sta SPTRv+1

		pla
		tax

		rts	

mazeRow_lo:
		.repeat 32,I
			.byte <(maze+32*4*I)
		.endrep

mazeRow_hi:
		.repeat 32,I
			.byte >(maze+32*4*I)
		.endrep

visitedRow_lo:
		.repeat 32,I
			.byte <(triggerIndices+32*I)
		.endrep

visitedRow_hi:
		.repeat 32,I
			.byte >(triggerIndices+32*I)
		.endrep
.endproc

.proc copyFont
		lda #<(font)
		sta MSRC+0
		lda #>(font)
		sta MSRC+1
		lda #<(automap_font)
		sta MDST+0
		lda #>(automap_font)
		sta MDST+1
		lda #<.sizeof(font)
		ldx #>.sizeof(font)
		jmp _memcpy_ram_underio
.endproc
