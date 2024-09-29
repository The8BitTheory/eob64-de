; 8x8 font copied here in RAM
volatileFont = __MEMCODE_RO_RUN__
.export volatileFont

vic_bitmap_row_lo = __MEMCODE_RO_RUN__+$400
vic_bitmap_row_hi = __MEMCODE_RO_RUN__+$400+25
vic_screen_row_lo = __MEMCODE_RO_RUN__+$400+50
vic_screen_row_hi = __MEMCODE_RO_RUN__+$400+75
_grab = __MEMCODE_RO_RUN__+$400+100

; Assets decrunched to here
assets_base = __MEMCODE_RO_RUN__+$600

; Grabbed graphics backbuffers here
topBackgroundGfx = __LEVELSTATE_RUN__
bg0Gfx = topBackgroundGfx + 10*20*4
bg1Gfx = bg0Gfx + 10*4*4
bg2Gfx = bg1Gfx + 10*4*4
bg3Gfx = bg2Gfx + 10*4*4
bottomBackgroundGfx = bg3Gfx + 10*4*4
txt0Gfx = bottomBackgroundGfx + 10*11*5
txt1Gfx = txt0Gfx + 10*8*1
txt2Gfx = txt1Gfx + 10*8*1
txt3Gfx = txt2Gfx + 10*8*1
playGfx = txt3Gfx + 10*8*1
endGfx = playGfx + 10*11*3
.assert endGfx < $8000, error, "Grabbed graphics flows into cart area"

.include "global.inc"
.include "chargen_assets.bin.inc"

.segment "START"
enableColorCycle: .byte 0
cycleColor: .byte 0

.macro boundCheckMouse x1,y1,x2,y2
	cpx #x1/2
	bcc :+
	cpx #x2/2+1
	bcs :+
	cpy #y2+1
	bcs :+
	cpy #y1
	bcs :++	;c=1 => hit
	:
		clc ;c=0 => miss
	:
.endmacro

.proc screenMode
	.word init
	.word teardown
	.byte 7
	.word $40,resetMultiplexer
	.word $7e,prepareMenuSprites
	.word $82-4+1*21,plex0
	.word $82-4+2*21,plex1
	.word $82-3+3*21,plex2
	.word $82-4+4*21,plex3
	.word $f8,enableSprites 

	; Sprite allocation
	; menu  0,2,3,4,5,7
	; mouse 1
	; box   6

	.proc init
		lda #$3b
		sta $d011
		lda #$18
		sta $d016
		lda $dd00
		and #$fc
		sta $dd00
		lda #$38
		sta $d018
		lda #$0
		sta $d021
		sta $d010
		sta $d017
		sta $d01b
		sta $d01d
		lda #$02
		sta $d01c
		lda #SPRITE_COL0
		sta $d025
		lda #SPRITE_COL2
		sta $d026
		lda #<(vic_sprites/$40)
		sta spriteptrs+6 ;Make sure the box is initially hidden
		rts
	.endproc

	.proc teardown
		lda #0
		sta $d015
		sta $d020
		sta $d021
		sta $d011
		rts
	.endproc

	.proc setColors
		sta $d027+0
		sta $d027+2
		sta $d027+3
		sta $d027+4
		sta $d027+5
		sta $d027+7
		rts
	.endproc

	.proc setXpos
		sta $d000+2*7
		clc
		adc #24
		sta $d000+2*5
		clc
		adc #24
		sta $d000+2*4
		clc
		adc #24
		sta $d000+2*3
		clc
		adc #24
		sta $d000+2*2
		clc
		adc #24
		sta $d000+2*0
		lda $d010
		and #%01000010
		ora #%00000101
		sta $d010
		rts
	.endproc

	.proc setYpos
		sta $d001+2*0
		sta $d001+2*2
		sta $d001+2*3
		sta $d001+2*4
		sta $d001+2*5
		sta $d001+2*7
		rts
	.endproc

	.proc setSpritePointers
		stx spriteptrs+7
		inx
		stx spriteptrs+5
		inx
		stx spriteptrs+4
		inx
		stx spriteptrs+3
		inx
		stx spriteptrs+2
		inx
		stx spriteptrs+0
		rts
	.endproc

	.proc enableSprites
		lda #%11111101
		sta $d015
		jmp exitIrq_quick
	.endproc

	.proc resetMultiplexer
		lda cycleColor
		jsr setColors
		lda #$68+2
		jsr setYpos
		lda #$ae
		jsr setXpos
		ldx #<(vic_sprites/$40 + 6*0)
		jsr setSpritePointers

		lda enableColorCycle
		beq :+
			pause:lda #0
			inc pause+1
			and #3
			bne :+
				sm:ldx #0
				lda cycle,x
				sta cycleColor
				sta $d02d
				inx
				txa
				and #7
				sta sm+1
		:

		jmp exitIrq

cycle:	.byte $0,$b,$c,$f,$1,$f,$c,$b
	.endproc

	.proc prepareMenuSprites
		lda #$82+0*21
		jsr setYpos
		lda #$b2
		jsr setXpos
		lda #1
		jsr setColors
		ldx #<(vic_sprites/$40 + 6*1)
		jsr setSpritePointers
		jmp exitIrq_quick
	.endproc

	.proc plex0
		lda #$82+1*21
		jsr setYpos
		ldx #9
		:
			dex
		bne :-
		ldx #<(vic_sprites/$40 + 6*2)
		jsr setSpritePointers
		jmp exitIrq_quick		
	.endproc

	.proc plex1
		lda #$82+2*21
		jsr setYpos
		ldx #8
		:
			dex
		bne :-
		ldx #<(vic_sprites/$40 + 6*3)
		jsr setSpritePointers
		jmp exitIrq_quick		
	.endproc

	.proc plex2
		lda #$82+3*21
		jsr setYpos
		ldx #8
		:
			dex
		bne :-
		ldx #<(vic_sprites/$40 + 6*4)
		jsr setSpritePointers
		jmp exitIrq_quick		
	.endproc

	.proc plex3
		lda #$82+4*21
		jsr setYpos
		ldx #8
		:
			dex
		bne :-
		ldx #<(vic_sprites/$40 + 6*5)
		jsr setSpritePointers
		jmp exitIrq_quick		
	.endproc
.endproc

.segment "MUSIC"
.proc decrunchMusic
.include "converters/music/eye-chargen.inc"
		lda #<$9000
		sta BB_DST+0
		lda #>$9000
		sta BB_DST+1
		ldy #<music
		ldx #>music
		jsr decrunchTo
		lda system
		cmp #SYSTEM_PAL
		beq :+
			memcpy_pureram $9000, $9000+eye_NTSC_char_generator_BOTH_CHIPS_fe, eye_NTSC_char_generator_BOTH_CHIPS_fe_size
		:
		rts
music:	.incbin "converters/music/eye-chargen.prg.b2",2
.endproc

.segment "CHARGEN"

background:	.incbin "resources/CharacterGeneration.kla.b2",2
assets:		.incbin "chargen_assets.prg.b2",2

.proc clearSpritesAndInstallBox
			jsr clearSprites
			ldx #62
			:
				lda box,x
				sta vic_sprites + (6*6+0)*64,x
				dex
			bpl :-
		
			rts

box:		.byte %11111111,%11111111,%10000000
			.byte %10000000,%00000000,%10000000
			.byte %10000000,%00000000,%10000000
			.byte %10000000,%00000000,%10000000
			.byte %10000000,%00000000,%10000000
			.byte %10000000,%00000000,%10000000
			.byte %10000000,%00000000,%10000000
			.byte %10000000,%00000000,%10000000
			.byte %10000000,%00000000,%10000000
			.byte %10000000,%00000000,%10000000
			.byte %10000000,%00000000,%10000000
			.byte %10000000,%00000000,%10000000
			.byte %10000000,%00000000,%10000000
			.byte %10000000,%00000000,%10000000
			.byte %10000000,%00000000,%10000000
			.byte %10000000,%00000000,%10000000
			.byte %11111111,%11111111,%10000000
			.byte 0,0,0
			.byte 0,0,0
			.byte 0,0,0
			.byte 0,0,0
.endproc

.export clearSprites
.proc clearSprites
		ldy #0
skip:	ldx #0
		tya
		:
			sta vic_sprites+$000,x
			sta vic_sprites+$100,x
			sta vic_sprites+$200,x
			sta vic_sprites+$300,x
			sta vic_sprites+$400,x
			sta vic_sprites+$500,x
			sta vic_sprites+$600,x
			sta vic_sprites+$700,x
			sta vic_sprites+$800,x
			sta vic_sprites+$900,x
			sta vic_sprites+$980,x
			inx
		bne :-
		rts
.endproc
.export clearSpritesWithData=clearSprites::skip

; n   r,y0, y1 (y2, y3)
; 0   0,13..20
; 1   1, 0..7
; 2   1, 8..15
; 3   1,16..20
;     2, 0..2
; 4   2, 3..10
; 5   2,11..18
; 6   2,19..20
;     3, 0..5
; 7   3, 6..13
; 
; tmp = n*8+13
;  r = tmp/21
; y0 = tmp%21
; y1 = min(21,y0+8)
; c0 = y1-y0
; c1 = 8-c0)
; left = (y0+7)-y1
sprite_offset_lo:
	.repeat 14,I
	.scope
		tmp=I*8+13
		y0=tmp .mod 21
		r=tmp/21
		.byte <(vic_sprites+r*6*64+y0*3)
	.endscope
	.endrep

sprite_offset_hi:
	.repeat 14,I
	.scope
		tmp=I*8+13
		y0=tmp .mod 21
		r=tmp/21
		.byte >(vic_sprites+r*6*64+y0*3)
	.endscope
	.endrep

sprite_next_lo:
	.repeat 14,I
	.scope
		tmp=I*8+13
		y0=tmp .mod 21
		r=tmp/21
		.byte <((r+1)*6*64-(r*6*64+y0*3))
	.endscope
	.endrep

sprite_next_hi:
	.repeat 14,I
	.scope
		tmp=I*8+13
		y0=tmp .mod 21
		r=tmp/21
		.byte >((r+1)*6*64-(r*6*64+y0*3))
	.endscope
	.endrep

sprite_c0:
	.repeat 14,I
	.scope
		tmp=I*8+13
		y0=tmp .mod 21
		y1=y0+8
		.if y1 < 21
			.byte 8
		.else
			.byte 21-y0
		.endif
	.endscope
	.endrep

sprite_c1:
	.repeat 14,I
	.scope
		tmp=I*8+13
		y0=tmp .mod 21
		y1=y0+8
		.if y1 < 21
			.byte 0
		.else
			.byte y1-21
		.endif
	.endscope
	.endrep

sprite_col_lo:
	.repeat 18,I
		.byte <((I .mod 3) + (I/3)*64)
	.endrep

sprite_col_hi:
	.repeat 18,I
		.byte >((I .mod 3) + (I/3)*64)
	.endrep

.proc renderChar
		sta TMP2
		stx TMP+1
		sty TMP

		clc
		lda sprite_offset_lo,y
		adc sprite_col_lo,x
		sta DST+0
		lda sprite_offset_hi,y
		adc sprite_col_hi,x
		sta DST+1

		lda #0
		sta TMP2+1
		lda TMP2+0
		asl
		rol TMP2+1
		asl
		rol TMP2+1
		asl
		rol TMP2+1
		adc #<volatileFont
		sta SRC+0
		lda TMP2+1
		adc #>volatileFont
		sta SRC+1

		ldx sprite_c0,y
		.repeat 8,I
			ldy #I
			dex
			bmi :+
			lda (SRC),y
			eor reverse
			ldy #I*3
			sta (DST),y
		.endrep
		:

		tya
		adc SRC+0
		sta SRC+0
		bcc :+
			inc SRC+1
		:

		ldy TMP
		ldx sprite_c1,y
		beq :+
			clc
			lda DST+0
			adc sprite_next_lo,y
			sta DST+0
			lda DST+1
			adc sprite_next_hi,y
			sta DST+1
			.repeat 7,I
				ldy #I
				lda (SRC),y
				eor reverse
				ldy #I*3
				sta (DST),y
				dex
				beq :+
			.endrep
		:

		lda TMP2
		ldx TMP+1
		ldy TMP
		rts
.endproc

.pushseg
.segment "BSS"
	tabstop: .res 1
	clearstop: .res 1
	cursorX: .res 1
	cursorY: .res 1
	reverse: .res 1
.popseg

.proc setCursor
	stx cursorX
	sty cursorY
	rts
.endproc

.proc return
	ldx tabstop
	stx cursorX
	inc cursorY
	rts
.endproc

.proc putChar
	cmp #10
	bne :+
		jmp return
	:
	ldx cursorX
	ldy cursorY
	inc cursorX
	jmp renderChar
.endproc

.proc drawNumber
	sta ARGS+0
	cmp #100
	bcc :+
		lda #'0'
		jsr putChar
		jmp putChar
	:
	jsrf htd

	lda TMP+0
	pha

	; 100s
	lda TMP+1
	and #$0f
	beq :+
		tax
		lda digits,x
		jsr putChar
	:

	; 10s
	pla
	pha
	lsr
	lsr
	lsr
	lsr
	beq :+
		tax
		lda digits,x
		jsr putChar
	:

	; 1s
	pla
	and #$0f
	tax
	lda digits,x
	jsr putChar

	rts
digits:		.byte "0123456789"
.endproc

.proc drawString
	stx SRC2+0
	sty SRC2+1
immediate:
	ldy #0
	:
		sty TMP3
		lda (SRC2),y
		beq :+

		jsr putChar

		ldy TMP3
		iny
	jmp :-
	:
	rts
.endproc

.pushseg
.segment "BSS"
	nbrMenuItems: .res 1
	menuItemToIndex: .res 13
	menuItemMask: .res 2
	menuIndex: .res 1
.popseg

; x,y ptr to menu
.proc drawMenu
	stx SRC3+0
	sty SRC3+1

	; Make sure title always is visible
	sec
	rol menuItemMask+0
	rol menuItemMask+1

	ldx #0
	ldy #0
	jsr setCursor

	ldx #<-1
	stx nbrMenuItems
	stx menuIndex
	ldy #0
	nextItem:
		lda (SRC3),y
		sta SRC2+0
		iny
		lda (SRC3),y
		sta SRC2+1
		iny
		ora SRC2+0
		beq done
		tya
		pha

		lsr menuItemMask+1
		ror menuItemMask+0
		bcc :++
			ldx nbrMenuItems
			bmi :+
				lda menuIndex
				sta menuItemToIndex,x
			:
			inc nbrMenuItems
			jsr drawString::immediate
			jsr return			
		:
		pla
		tay
		inc menuIndex
	jmp nextItem
	done:
	rts
.endproc

; y = item
.proc drawMenuItem
	sta reverse
	txa
	pha
	tya
	pha
	iny
	ldx #0
	jsr setCursor

	clc
	dey
	lda menuItemToIndex,y
	adc #1
	asl
	tay

	lda (SRC3),y
	sta SRC2+0
	iny
	lda (SRC3),y
	sta SRC2+1
	iny
	jsr drawString::immediate

	pla
	tay
	pla
	tax
	rts
.endproc

.proc handleMenu
	buttonGfx = assets_base + chargen_back_bitmap
	buttonSpr = assets_base + chargen_back_sprites

	.pushseg
	.segment "BSS"
		selectedItem: .res 1
	.popseg

	txa
	pha
	tya
	pha
	bitblt buttonGfx,5,3,33,21
	ldx #11*3-1
	:
		lda buttonSpr+0*11*3,x
		sta vic_sprites + (6*5+4)*64+9*3,x
		lda buttonSpr+1*11*3,x
		sta vic_sprites + (6*5+5)*64+9*3,x
		dex
	bpl :-
	pla
	tay
	pla
	tax

	lda #0
	sta selectedItem
	lda #0
	sta reverse

	jsr drawMenu

	lda #$ff
	ldy selectedItem
	jsr drawMenuItem

	loop:
		lda KEYEVENT+KeyEvent::keyPressed
		beq doneKeyboard
			lda #0
			sta KEYEVENT+KeyEvent::keyPressed
			lda KEYEVENT+KeyEvent::modifiers
			cmp #$80 ;run/stop
			beq :+
			lda KEYEVENT+KeyEvent::normalChar
			cmp #$02 ;b
			bne :++
			:
				lda #0
				sta KEYEVENT+KeyEvent::keyPressed
				jsr clearSpritesAndInstallBox
				clc
				rts
			:
			lda KEYEVENT+KeyEvent::specialChar
			bpl notUpDown
				lda KEYEVENT+KeyEvent::modifiers
				and #%01010000
				bne up
				down:
					ldy selectedItem
					iny
					cpy nbrMenuItems
					bcs doneKeyboard
						dey
						lda #0
						jsr drawMenuItem
						iny
						sty selectedItem
						lda #$ff
						jsr drawMenuItem
					jmp doneKeyboard
				up:
					ldy selectedItem
					beq doneKeyboard
						lda #0
						jsr drawMenuItem
						dey
						sty selectedItem
						lda #$ff
						jsr drawMenuItem
					jmp doneKeyboard
			notUpDown:
				cmp #2
				beq done
		doneKeyboard:

		lda MOUSEEVENT+MouseEvent::buttons
		bmi loop

		lda #$ff
		sta MOUSEEVENT+MouseEvent::buttons

		lsr MOUSEEVENT+MouseEvent::xpos+1
		ror MOUSEEVENT+MouseEvent::xpos+0
		ldx MOUSEEVENT+MouseEvent::xpos+0
		ldy MOUSEEVENT+MouseEvent::ypos+0

		boundCheckMouse 264,172, 300,187
		bcc :+
			jsr clearSpritesAndInstallBox
			clc
			rts
		:

		cpx #$4c
		bcc :+
		cpx #$98
		bcs :+
			cpy #$50
			bcc :+
			cpy #$50+14*8
			bcs :+
				tya
				sec
				sbc #$50
				lsr
				lsr
				lsr
				cmp nbrMenuItems
				beq :+
				bcs :+
					cmp selectedItem
					beq done
					pha
					lda #0
					ldy selectedItem
					jsr drawMenuItem
					pla
					tay
					sty selectedItem
					lda #$ff
					jsr drawMenuItem
					jmp loop
		:
	jmp loop

done:
	ldx #2
	ldy selectedItem
	lda menuItemToIndex,y
	pha
	:
		lda #2
		jsr wait
		lda #0
		jsr drawMenuItem
		lda #2
		jsr wait
		lda #$ff
		jsr drawMenuItem
		dex
	bne :-

	jsr clearSpritesAndInstallBox
	pla
	sec
	rts
.endproc

.proc getPartyMember
	lda lo,x
	sta CURRENT_PARTYMEMBER+0
	lda hi,x
	sta CURRENT_PARTYMEMBER+1
	rts
lo: .repeat 4,I
		.byte <(party+I*.sizeof(PartyMember))
	.endrep
hi:	.repeat 4,I
		.byte >(party+I*.sizeof(PartyMember))
	.endrep
.endproc

.proc doRaceMenu
		lda #$ff
		sta menuItemMask+0
		sta menuItemMask+1
		
		ldx #<menu
		ldy #>menu
		jsr handleMenu
		bcc :+
			ldy #PartyMember::race
			sta (CURRENT_PARTYMEMBER),y
		:
		rts

menu:
		.word title
races:
		.word o0,o1,o2,o3,o4,o5,o6,o7,o8,o9,o10,o11
		.word 0

		title:	.byte "W",93,"HLE RASSE:",0
		o0: 	.byte "MENSCH M",93,"NNL.",0
		o1: 	.byte "MENSCH WEIBL.",0
		o2: 	.byte "ELF M",93,"NNL.",0
		o3: 	.byte "ELF WEIBL.",0
		o4: 	.byte "HALB-ELF M",93,"NNL.",0
		o5: 	.byte "HALB-ELF WEIBL.",0
		o6: 	.byte "ZWERG M",93,"NNL.",0
		o7: 	.byte "ZWERG WEIBL.",0
		o8: 	.byte "GNOM M",93,"NNL.",0
		o9: 	.byte "GNOM WEIBL.",0
		o10: 	.byte "HALBLING M",93,"NNL.",0
		o11: 	.byte "HALBLING WEIBL.'",0
.endproc

.proc doClassMenu
		; Apply classMenuMasks based on race w/o sex
		ldy #PartyMember::race
		lda (CURRENT_PARTYMEMBER),y
		and #$fe ; mask away sex
		tay
		lda masks+0,y
		sta menuItemMask+0
		lda masks+1,y
		sta menuItemMask+1

		; Check for evil characters
		ldx #3
		:
			jsr getPartyMember
			ldy #PartyMember::name
			lda (CURRENT_PARTYMEMBER),y
			beq :+
				ldy #PartyMember::alignment
				lda (CURRENT_PARTYMEMBER),y
				cmp #6
				bcc :+
					lda menuItemMask+0
					and #<$fffb
					sta menuItemMask+0
					lda menuItemMask+1
					and #>$fffb
					sta menuItemMask+1
					jmp :++
			:
			dex
		bpl :--
		:
		ldx CURRENT_PARTYMEMBER_INDEX
		jsr getPartyMember

		ldx #<menu
		ldy #>menu
		jsr handleMenu
		bcc :+
			ldy #PartyMember::class
			sta (CURRENT_PARTYMEMBER),y
		:
		rts

masks:
		.word $003F, $07BB, $77FB, $00F1, $08F1, $00B1

menu:
		.word title
classes:
		.word o0,o1,o2,o3,o4,o5,o6,o7,o8,o9,o10,o11,o12,o13,o14
		.word 0

		title:	.byte "W",93,"HLE KLASSE:",0
		o0: 	.byte "K",93,"MPFER",0
		o1: 	.byte "WALDL",93,"UFER",0
		o2: 	.byte "PALADIN",0
		o3: 	.byte "MAGIER",0
		o4: 	.byte "KLERIKER",0
		o5: 	.byte "DIEB",0
		o6: 	.byte "K",93,"MPFER/KLERIKER",0
		o7: 	.byte "K",93,"MPFER/DIEB",0
		o8: 	.byte "K",93,"MPFER/MAGIER",0
		o9: 	.byte "K",93,"MPFR/MAGIER/DIEB",0
		o10: 	.byte "DIEB/MAGIER",0
		o11: 	.byte "KLERIKER/DIEB",0
		o12: 	.byte "K",93,"MPFR/KLRIKR/MAGR",0 ;This is not a spelling error. The sprite layer cant hold more chars.
		o13: 	.byte "WALDL",93,"UFER/KLERIKR",0
		o14: 	.byte "KLERIKER/MAGIER",0
.endproc

.proc doAlignmentMenu
		; Apply alignmentMenuMask based on class
		ldy #PartyMember::class
		lda (CURRENT_PARTYMEMBER),y
		asl
		tay
		lda masks+0,y
		sta menuItemMask+0
		lda masks+1,y
		sta menuItemMask+1

		; Check for paladins
		ldx #3
		:
			jsr getPartyMember
			ldy #PartyMember::name
			lda (CURRENT_PARTYMEMBER),y
			beq :+
				ldy #PartyMember::class
				lda (CURRENT_PARTYMEMBER),y
				cmp #2
				bne :+
					lda menuItemMask+0
					and #<$fe3f
					sta menuItemMask+0
					lda menuItemMask+1
					and #>$fe3f
					sta menuItemMask+1
					jmp :++
			:
			dex
		bpl :--
		:
		ldx CURRENT_PARTYMEMBER_INDEX
		jsr getPartyMember

		ldx #<menu
		ldy #>menu
		jsr handleMenu
		bcc :+
			ldy #PartyMember::alignment
			sta (CURRENT_PARTYMEMBER),y
		:
		rts

masks:
		.word $01FF, $0007, $0001, $01FF, $01FF, $01FE, $01FF, $01FE
		.word $01FF, $01FE, $01FE, $01FE, $01FF, $0007, $01FF

menu:
		.word title
		.word o0,o1,o2,o3,o4,o5,o6,o7,o8
		.word 0

		title:	.byte "GESINNUNG W",93,"HLEN:",0
		o0: 	.byte "RECHTSCH. GUT",0
		o1: 	.byte "NEUTRAL GUT",0
		o2: 	.byte "CHAOTISCH GUT",0
		o3: 	.byte "RECHTSCH. NEUTRAL",0
		o4: 	.byte "ABSOLUT NEUTRAL",0
		o5: 	.byte "CHAOTISCH NEUTRAL",0
		o6: 	.byte "RECHTSCH. B",94,"SE",0
		o7: 	.byte "NEUTRAL B",94,"SE",0
		o8: 	.byte "CHAOTISCH B",94,"SE",0
.endproc

.proc doSelectMember
		.pushseg
		.segment "BSS"
			partyComplete: .res 1
		.popseg

		ldx #0
		stx partyComplete
		stx reverse

		ldy #1
		jsr setCursor
		ldx #<text1
		ldy #>text1
		jsr drawString

		; Check if party is complete
		lda party + .sizeof(PartyMember)*0 + PartyMember::name
		beq :+
		lda party + .sizeof(PartyMember)*1 + PartyMember::name
		beq :+
		lda party + .sizeof(PartyMember)*2 + PartyMember::name
		beq :+
		lda party + .sizeof(PartyMember)*3 + PartyMember::name
		beq :+
			ldx #0
			ldy #7
			jsr setCursor
			ldx #<text2
			ldy #>text2
			jsr drawString
			bitblt (assets_base+chargen_play_bitmap),11,3,3,22
			inc partyComplete
			jmp :++
		:
			bitblt playGfx,11,3,3,22
		:

		lda #<(vic_sprites/$40 + 6*6+0)
		sta spriteptrs+6
		lda $d017
		ora #$40
		sta $d017
		lda $d01d
		ora #$40
		sta $d01d
		lda $d01c
		and #$40^$ff
		sta $d01c
		lda #1
		sta enableColorCycle

		loop:
			ldx CURRENT_PARTYMEMBER_INDEX
			lda box_x,x
			sta $d00c
			lda box_y,x
			sta $d00d

			lda KEYEVENT+KeyEvent::keyPressed
			beq doneKeyboard
				lda #0
				sta KEYEVENT+KeyEvent::keyPressed

				lda partyComplete
				beq :+
					lda KEYEVENT+KeyEvent::normalChar ; 'p' pressed
					cmp #$10
					bne :+
						clc
						rts
				:
				lda KEYEVENT+KeyEvent::specialChar
				cmp #$80
				bne :+
					; Up/Down
					lda CURRENT_PARTYMEMBER_INDEX
					eor #2
					sta CURRENT_PARTYMEMBER_INDEX
					jmp doneKeyboard
				:
				cmp #$04
				bne :+
					; Left/Right
					lda CURRENT_PARTYMEMBER_INDEX
					eor #1
					sta CURRENT_PARTYMEMBER_INDEX
					jmp doneKeyboard
				:
				cmp #$02
				bne :+
					; Return
					lda #0
					sta KEYEVENT+KeyEvent::keyPressed
					jmp done
				:
			doneKeyboard:

			lda MOUSEEVENT+MouseEvent::buttons
			bmi loop
				lda #$ff
				sta MOUSEEVENT+MouseEvent::buttons
				lsr MOUSEEVENT+MouseEvent::xpos+1
				ror MOUSEEVENT+MouseEvent::xpos+0
				ldx MOUSEEVENT+MouseEvent::xpos+0
				ldy MOUSEEVENT+MouseEvent::ypos+0
				lda partyComplete
				beq :+
					boundCheckMouse 26,181,104,194 ; Play pressed
					bcc :+
						clc
						rts
				:

				lsr MOUSEEVENT+MouseEvent::ypos+1
				ror MOUSEEVENT+MouseEvent::ypos+0
				ldx #2
				:
					lsr MOUSEEVENT+MouseEvent::xpos+1
					ror MOUSEEVENT+MouseEvent::xpos+0
					lsr MOUSEEVENT+MouseEvent::ypos+1
					ror MOUSEEVENT+MouseEvent::ypos+0
					dex
				bne :-
				ldx MOUSEEVENT+MouseEvent::xpos+0
				ldy MOUSEEVENT+MouseEvent::ypos+0
				cpy #8
				bcc doneMouse
					cpy #12
					bcs :+
						check0:
						cpx #2
						bcc doneMouse
						cpx #6
						bcs check1
							lda #0
							sta CURRENT_PARTYMEMBER_INDEX
							jmp done
						check1:
						cpx #10
						bcc doneMouse
						cpx #14
						bcs doneMouse
							lda #1
							sta CURRENT_PARTYMEMBER_INDEX
							jmp done
				:
				cpy #16
				bcc doneMouse
					cpy #20
					bcs doneMouse
						check2:
						cpx #2
						bcc doneMouse
						cpx #6
						bcs check3
							lda #2
							sta CURRENT_PARTYMEMBER_INDEX
							jmp done
						check3:
						cpx #10
						bcc doneMouse
						cpx #14
						bcs doneMouse
							lda #3
							sta CURRENT_PARTYMEMBER_INDEX
							jmp done
			doneMouse:
		jmp loop

done:	ldx CURRENT_PARTYMEMBER_INDEX
		lda box_x,x
		sta $d00c
		lda box_y,x
		sta $d00d
		jsr clearSpritesAndInstallBox
		sec
		rts

box_x:	.byte $28,$68,$28,$68
box_y:	.byte $71,$71,$b1,$b1

text1:	.byte "W",125,"hle das Feld",$a
		.byte "des Charakters",$a
		.byte "zum Erstellen oder",$a
		.byte "Anschauen.",0

text2:	.byte "Ihr Team ist",$a
		.byte "komplett. PLAY-",$a
		.byte "Knopf oder 'P'",$a
		.byte "dr",127,"cken, um zu",$a
		.byte "beginnen.",0
.endproc

.proc showCharacterStats
		.pushseg
		.segment "BSS"
			selectedIndex: .res 1
		.popseg

		lda #0
		sta reverse


		ldx #0
		ldy #3
		jsr setCursor

		; Prepare print name
		ldx CURRENT_PARTYMEMBER_INDEX
		lda names_lo,x
		sta SRC2+0
		lda names_hi,x
		sta SRC2+1

		; But first strlen
		ldy #0
		:
			lda (SRC2),y
			beq :+
			iny
		jmp :-
		:

		; To center it
		sty TMP
		sec
		lda #18
		sbc TMP
		lsr
		tax
		ldy #3
		jsr setCursor

		; Print it
		jsr drawString::immediate

		ldy #PartyMember::race
		lda (CURRENT_PARTYMEMBER),y
		tay
		ldx raceStringLeftPad,y
		ldy #5
		jsr setCursor
		ldy #PartyMember::race
		lda (CURRENT_PARTYMEMBER),y
		asl
		tay
		ldx doRaceMenu::races,y
		iny
		lda doRaceMenu::races,y
		tay
		jsr drawString

		ldy #PartyMember::class
		lda (CURRENT_PARTYMEMBER),y
		tay
		ldx classStringLeftPad,y
		ldy #6
		jsr setCursor
		ldy #PartyMember::class
		lda (CURRENT_PARTYMEMBER),y
		asl
		tay
		ldx doClassMenu::classes,y
		iny
		lda doClassMenu::classes,y
		tay
		jsr drawString

		lda #$ff
update: sta selectedIndex
		ldx #0
		stx tabstop
		ldy #7
		jsr setCursor
		lda #8
		sta clearstop

		lda #0
		jsr isSelected
		ldx #<strTxt
		ldy #>strTxt
		jsr drawString
		ldy #PartyMember::strengthCurrent
		lda (CURRENT_PARTYMEMBER),y
		pha
		jsr drawNumber
		pla
		cmp #18
		bcc :+
			ldy #PartyMember::extraStrengthCurrent
			lda (CURRENT_PARTYMEMBER),y
			beq :+
				pha
				lda #'/'
				jsr putChar
				pla
				jsr drawNumber
		:
		lda #0
		sta reverse
		jsr clearToStop

		lda #5
		sta clearstop

		lda #1
		jsr isSelected
		ldx #<intTxt
		ldy #>intTxt
		lda #PartyMember::intelligenceCurrent
		jsr drawStat

		lda #2
		jsr isSelected
		ldx #<wisTxt
		ldy #>wisTxt
		lda #PartyMember::wisdomCurrent
		jsr drawStat

		lda #3
		jsr isSelected
		ldx #<dexTxt
		ldy #>dexTxt
		lda #PartyMember::dexterityCurrent
		jsr drawStat

		lda #4
		jsr isSelected
		ldx #<conTxt
		ldy #>conTxt
		lda #PartyMember::constitutionCurrent
		jsr drawStat

		lda #5
		jsr isSelected
		ldx #<chaTxt
		ldy #>chaTxt
		lda #PartyMember::charismaCurrent
		jsr drawStat

		ldx #9
		stx tabstop
		ldy #7
		jsr setCursor
		lda #17
		sta clearstop

		ldx #<acTxt
		ldy #>acTxt
		lda #PartyMember::ac
		jsr drawStat

		lda #6
		jsr isSelected
		ldx #<hpTxt
		ldy #>hpTxt
		lda #PartyMember::hpCurrent
		jsr drawStat

		ldx #<lvlTxt
		ldy #>lvlTxt
		jsr drawString
		ldy #PartyMember::level
		tya
		pha
		lda (CURRENT_PARTYMEMBER),y
		:clc
		adc #'0'
		jsr putChar
		pla
		tay
		iny
		tya
		pha
		cpy #PartyMember::level+3
		beq :+
		lda (CURRENT_PARTYMEMBER),y
		beq :+
			pha
			lda #'/'
			jsr putChar
			pla
			jmp :-
		:
		pla
		jsr clearToStop

		lda #0
		sta tabstop

		rts

isSelected:
		cmp selectedIndex
		bne :+
			lda #$ff
			sta reverse
		:
		rts

drawStat:
		pha
		jsr drawString
		pla
		tay
		lda (CURRENT_PARTYMEMBER),y
		jsr drawNumber
		lda #0
		sta reverse
		jmp clearToStop

		strTxt:.byte "STR ",0
		intTxt:.byte "INT ",0
		wisTxt:.byte "WIS ",0
		dexTxt:.byte "DEX ",0
		conTxt:.byte "CON ",0
		chaTxt:.byte "CHA ",0
		 acTxt:.byte " AC ",0
		 hpTxt:.byte " HP ",0
		lvlTxt:.byte "LVL ",0

raceStringLeftPad: .byte 4,3,5,4,3,2,4,3,4,3,3,2
classStringLeftPad: .byte 5,6,5,7,6,6,2,2,3,0,4,3,0,2,3
.endproc

.proc clearToStop
		lda clearstop
		cmp cursorX
		bcc :+
			lda #' '
			jsr putChar
			jmp clearToStop
		:
		lda #0
		sta reverse
		jmp return
.endproc

.proc doFaceMenu
		buttonGfx = assets_base + chargen_arrows_bitmap

		;    Male: 00..25 = 26
		;    Both: 26..28 = 3
		; Females: 29..43 = 15
		.pushseg
		.segment "BSS"
			charSex: .res 1
			shp: .res 1
			dir: .res 1
			faces: .res 4
		.popseg

		bitblt buttonGfx,4,4,18,8

		ldx #0
		ldy #PartyMember::race
		lda (CURRENT_PARTYMEMBER),y
		and #1
		sta charSex
		beq :+
			ldx #26
		:
		lda #1
		sta dir
		jsr getNextFreeFaceShape
		jsr drawFaces

loop:
		lda KEYEVENT+KeyEvent::keyPressed
		beq doneKeyboard
			lda #0
			sta KEYEVENT+KeyEvent::keyPressed
			lda KEYEVENT+KeyEvent::specialChar
			cmp #$04 ;Cursor left/right
			bne notLeftRight
				lda KEYEVENT+KeyEvent::modifiers
				and #%01010000 ;Left+Right shift
				bne left
				right:
					jsr nextFace
					jmp doneKeyboard
				left:
					jsr previousFace
					jmp doneKeyboard
			notLeftRight:
				cmp #2 ;Return
				bne doneKeyboard
					ldx #1
					jmp done
		doneKeyboard:

		lda MOUSEEVENT+MouseEvent::buttons
		bmi loop

		lda #$ff
		sta MOUSEEVENT+MouseEvent::buttons
		ldx #3
		:
			lsr MOUSEEVENT+MouseEvent::xpos+1
			ror MOUSEEVENT+MouseEvent::xpos+0
			lsr MOUSEEVENT+MouseEvent::ypos+1
			ror MOUSEEVENT+MouseEvent::ypos+0
			dex
		bne :-

		ldx MOUSEEVENT+MouseEvent::xpos+0
		ldy MOUSEEVENT+MouseEvent::ypos+0
		cpy #8
		bcc loop
		cpy #12
		bcs loop
		cpx #18
		bcc loop
		cpx #38
		bcs loop
		sec
		txa
		sbc #18
		lsr
		lsr
		tax
		dex
		bpl done ;Select face 0..3
		cpy #10
		bcc :+
			jsr nextFace
			jmp loop
		:
		jsr previousFace
		jmp loop

done:	lda faces,x
		ldy #PartyMember::picture
		sta (CURRENT_PARTYMEMBER),y
		jmp restoreTop
		rts

nextFace:
		lda #1
		sta dir
		ldx shp
		inx
		jmp :+

previousFace:
		lda #<-1
		sta dir
		ldx shp
		dex
:
		jsr getNextFreeFaceShape
		jmp drawFaces

drawFaces:
		lda #1
		sta dir
		lda shp
		sta faces+0
		pha
		sta ARGS+0
		lda #0
		sta ARGS+1
		ldx #22
		ldy #8
		jsrf showPortrait
		ldx shp
		inx
		jsr getNextFreeFaceShape
		sta faces+1
		sta ARGS+0
		ldx #26
		ldy #8
		jsrf showPortrait
		ldx shp
		inx
		jsr getNextFreeFaceShape
		sta faces+2
		sta ARGS+0
		ldx #30
		ldy #8
		jsrf showPortrait
		ldx shp
		inx
		jsr getNextFreeFaceShape
		sta faces+3
		sta ARGS+0
		ldx #34
		ldy #8
		jsrf showPortrait
		pla
		sta shp
		rts

getNextFreeFaceShape:
		txa
		bpl :+
			lda #43
			jmp :++
		:
			cmp #44
			bcc :+
				sbc #44
				jmp :-
		:
		sta shp

		.repeat 4,I
			lda party + .sizeof(PartyMember)*I + PartyMember::name
			beq :+
				lda party + .sizeof(PartyMember)*I + PartyMember::picture
				cmp shp
				beq notUsable
			:
		.endrep

		lda charSex
		bne :+
			lda shp
			cmp #29
			bcs notUsable
			jmp :++
		:
			lda shp
			cmp #26
			bcc notUsable
		:
		rts

notUsable:
		clc
		lda shp
		adc dir
		sta shp
		tax
		jmp getNextFreeFaceShape
.endproc

.proc doStatsMenu
		buttonGfx = assets_base + chargen_menu_bitmap
		buttonSpr = assets_base + chargen_menu_sprites

		ldy #PartyMember::picture
		lda (CURRENT_PARTYMEMBER),y
		sta ARGS+0
		lda #0
		sta ARGS+1
		ldx #26
		ldy #8
		jsrf showPortrait
		bitblt buttonGfx,10,5,28,19

		ldx #7*3-1
		:
			lda buttonSpr+0*7*3,x
			sta vic_sprites + (6*4+3)*64+14*3,x
			lda buttonSpr+1*7*3,x
			sta vic_sprites + (6*4+4)*64+14*3,x
			lda buttonSpr+2*7*3,x
			sta vic_sprites + (6*4+5)*64+14*3,x
			dex
		bpl :-
		ldx #20*3-1
		:
			lda buttonSpr+3*7*3+0*20*3,x
			sta vic_sprites + (6*5+3)*64,x
			lda buttonSpr+3*7*3+1*20*3,x
			sta vic_sprites + (6*5+4)*64,x
			lda buttonSpr+3*7*3+2*20*3,x
			sta vic_sprites + (6*5+5)*64,x
			dex
		bpl :-

loop:
		lda KEYEVENT+KeyEvent::keyPressed
		beq doneKeyboard
			lda #0
			sta KEYEVENT+KeyEvent::keyPressed
			lda KEYEVENT+KeyEvent::normalChar
			cmp #$12 ;Reroll
			bne :+
				lda #0
				jmp doneNoClear
			:
			cmp #$0d ;Modify
			bne :+
				lda #1
				jmp done
			:
			cmp #$06 ;Faces
			bne :+
				lda #2
				jmp done
			:
			cmp #$0b ;Keep
			bne :+
				lda #3
				jmp done
			:
		doneKeyboard:

		lda MOUSEEVENT+MouseEvent::buttons
		bmi loop

		lda #$ff
		sta MOUSEEVENT+MouseEvent::buttons

		lsr MOUSEEVENT+MouseEvent::xpos+1
		ror MOUSEEVENT+MouseEvent::xpos+0
		ldx MOUSEEVENT+MouseEvent::xpos+0
		ldy MOUSEEVENT+MouseEvent::ypos+0

		boundCheckMouse 224,156, 260,171 ; Reroll
		bcc :+
			lda #0
			jmp doneNoClear
		:
		boundCheckMouse 264,156, 300,171 ; Modify
		bcc :+
			lda #1
			jmp done
		:
		boundCheckMouse 224,172, 260,187 ; Faces
		bcc :+
			lda #2
			jmp done
		:
		boundCheckMouse 264,172, 300,187 ; Keep
		bcc :+
			lda #3
			jmp done
		:
		jmp loop

done:	pha
		lda #0
		ldx #7*3-1
		:
			sta vic_sprites + (6*4+3)*64+14*3,x
			sta vic_sprites + (6*4+4)*64+14*3,x
			sta vic_sprites + (6*4+5)*64+14*3,x
			dex
		bpl :-
		ldx #20*3-1
		:
			sta vic_sprites + (6*5+3)*64,x
			sta vic_sprites + (6*5+4)*64,x
			sta vic_sprites + (6*5+5)*64,x
			dex
		bpl :-
		jsr restoreBottom
		pla
doneNoClear:
		rts
.endproc

.proc doModifyMenu
		buttonGfx = assets_base + chargen_modify_bitmap
		buttonSpr = assets_base + chargen_modify_sprites

		bitblt buttonGfx,11,3,27,21
		ldx #11*3-1
		:
			lda buttonSpr+0*11*3,x
			sta vic_sprites + (6*5+2)*64+9*3,x
			lda buttonSpr+1*11*3,x
			sta vic_sprites + (6*5+3)*64+9*3,x
			lda buttonSpr+2*11*3,x
			sta vic_sprites + (6*5+4)*64+9*3,x
			lda buttonSpr+3*11*3,x
			sta vic_sprites + (6*5+5)*64+9*3,x
			dex
		bpl :-

		lda #0
		jsr showCharacterStats::update

loop:
		lda KEYEVENT+KeyEvent::keyPressed
		beq doneKeyboard
			lda #0
			sta KEYEVENT+KeyEvent::keyPressed

			lda KEYEVENT+KeyEvent::modifiers
			cmp #$80 ; Run-stop
			beq :+
			lda KEYEVENT+KeyEvent::normalChar
			cmp #$0f ; O
			bne :++
			:
				jmp done
			:

			lda KEYEVENT+KeyEvent::specialChar
			cmp #$80 ; crsr ud
			bne notUpDown
				lda KEYEVENT+KeyEvent::modifiers
				and #%01010000 ;shifts
				bne up
				down:
					ldx showCharacterStats::selectedIndex
					inx
					cpx #7
					bne :+
						ldx #0
					:
					jmp :+
				up:	
					ldx showCharacterStats::selectedIndex
					dex
					bpl :+
						ldx #6
				:
				txa
				jsr showCharacterStats::update
				jmp doneKeyboard
			notUpDown:
			cmp #$04 ; crsr lr
			bne notLeftRight
				lda KEYEVENT+KeyEvent::modifiers
				and #$10 ;right shift
				bne left
				right:
					ldy #1
					lda KEYEVENT+KeyEvent::modifiers
					and #$40 ; left shift
					jmp increase
				left:
					ldy #<-1
					lda KEYEVENT+KeyEvent::modifiers
					and #$40 ; left shift
					jmp decrease
			notLeftRight:
		doneKeyboard:

		lda MOUSEEVENT+MouseEvent::buttons
		bpl :+
			jmp doneMouse
		:
			lsr MOUSEEVENT+MouseEvent::xpos+1
			ror MOUSEEVENT+MouseEvent::xpos+0
			ldx MOUSEEVENT+MouseEvent::xpos+0
			ldy MOUSEEVENT+MouseEvent::ypos+0

			boundCheckMouse 216,172, 236,187
			bcc :++
				; Plus
				ldy #1
				ldx MOUSEEVENT+MouseEvent::buttons
				cpx #1
increase:			beq :+
					ldy #10
				:
				tya
				jsr modifyStat
				jmp ackMouse
			:

			boundCheckMouse 240,172, 260,187
			bcc :++
				; Minus
				ldy #<-1
				ldx MOUSEEVENT+MouseEvent::buttons
				cpx #1
decrease:			beq :+
					ldy #<-10
				:
				tya
				jsr modifyStat
				jmp ackMouse
			:

			boundCheckMouse 264,172, 300,187
			bcc :+
				; OK
				jmp done
			:

			boundCheckMouse 144,128, 222,175
			bcc :+
				; Left stat side
				tya
				sbc #128
				lsr
				lsr
				lsr
				jsr showCharacterStats::update
				jmp ackMouse
			:

			boundCheckMouse 224,136, 302,143
			bcc :+
				; HP
				lda #6
				jsr showCharacterStats::update
			:

			ackMouse:
			lda #$ff
			sta MOUSEEVENT+MouseEvent::buttons
		doneMouse:

		jmp loop

done:	lda #$ff
		sta MOUSEEVENT+MouseEvent::buttons
		jsr showCharacterStats::update

		lda #0
		ldx #11*3-1
		:
			sta vic_sprites + (6*5+2)*64+9*3,x
			sta vic_sprites + (6*5+3)*64+9*3,x
			sta vic_sprites + (6*5+4)*64+9*3,x
			sta vic_sprites + (6*5+5)*64+9*3,x
			dex
		bpl :-
		jmp restoreBottom

; a = amount
modifyStat:
		; Clamp delta to +/- 1 unless stat is strength and at 18
		ldx showCharacterStats::selectedIndex
		cpx #0 ;str
		bne clampDelta
			ldy statOffsets,x
			pha
			lda (CURRENT_PARTYMEMBER),y
			tay
			pla
			cpy #18
			bcc clampDelta
				; Make sure extraStrengthCurrent dont go below 0
				pha
				ldy #PartyMember::extraStrengthCurrent
				clc
				adc (CURRENT_PARTYMEMBER),y
				bpl :+
					lda (CURRENT_PARTYMEMBER),y
					tax
					pla
					cpx #0
					beq clampDelta
					lda #1
					sta (CURRENT_PARTYMEMBER),y
					lda #<-1
					jmp doneClampDelta
				:
				pla
				jmp doneClampDelta
		clampDelta:
			; Clamp delta to +/- 1
			cmp #0
			bpl :+
				lda #<-1
				jmp doneClampDelta
			:
			lda #1
		doneClampDelta:

		; Apply delta
		ldx showCharacterStats::selectedIndex
		ldy statOffsets,x
		clc
		adc (CURRENT_PARTYMEMBER),y
		sta (CURRENT_PARTYMEMBER),y

		; Spill over overflow stength into extraStrength if classModifier == 0
		cpx #0
		bne notStrength
		ldy #PartyMember::class
		pha
		lda (CURRENT_PARTYMEMBER),y
		tay
		pla
		ldx classModifier,y
		bne notStrength
			pha
			ldy #PartyMember::extraStrengthCurrent
			lda (CURRENT_PARTYMEMBER),y
			tax
			pla
			:
				cmp #19
				bcc :+
					sbc #1
					inx
			jmp :-
			:
				cpx #1
				bmi :+
				cmp #18
				bcs :+
					adc #1
					dex
			jmp :-
			:
			ldy #PartyMember::strengthCurrent
			sta (CURRENT_PARTYMEMBER),y
			ldy #PartyMember::extraStrengthCurrent
			txa

			; Clamp extra strength to [0..100]
			bpl :+
				lda #0
			:
			cmp #100+1
			bcc :+
				lda #100
			:
			sta (CURRENT_PARTYMEMBER),y
		notStrength:

		; Bound stats according to min/max
		ldx showCharacterStats::selectedIndex
		jsr boundStat

		; Update AC based on DEX
		jsr calculateAC

		; If CON changed, recalculate HP and re-bound it
		ldx showCharacterStats::selectedIndex
		cpx #4 ;4=CON stat
		bne :+
			jsr calculateMinMaxHP
			ldx #6 ;6=HP stat
			jsr boundStat
		:

		lda showCharacterStats::selectedIndex
		jsr showCharacterStats::update
		rts

boundStat:
		ldy statOffsets,x
		lda (CURRENT_PARTYMEMBER),y
		cmp minStats,x
		bcs :+
			lda minStats,x
		:
		cmp maxStats,x
		bcc :+
			lda maxStats,x
		:
		sta (CURRENT_PARTYMEMBER),y
		rts
.endproc

.proc calculateMinMaxHP
		sec
		jsr getMinMaxHP
		ldx #6
		sta minStats,x

		clc
		jsr getMinMaxHP
		ldx #6
		sta maxStats,x
		rts
.endproc

; A = mode (c=0:max c=1:min)
; Return max/min HP in A
.proc getMinMaxHP
		lda #0
		sta TMP2
		adc #0
		sta ARGS+3

		ldy #PartyMember::class
		lda (CURRENT_PARTYMEMBER),y
		tax
		ldy #PartyMember::constitutionCurrent
		lda (CURRENT_PARTYMEMBER),y
		tay
		jsrf getClassAndConstHitpointsModifier
		stx ARGS+1

		.repeat 3,I
			ldy #PartyMember::class
			lda (CURRENT_PARTYMEMBER),y
			tax
			ldy #I
			jsrf getCharacterClassType
			tya
			bmi :+
				sty ARGS+0
				ldy #PartyMember::level+I
				lda (CURRENT_PARTYMEMBER),y
				sta ARGS+2
				jsr appendModifiedHpLimits
			:
		.endrep

		ldy #PartyMember::class
		lda (CURRENT_PARTYMEMBER),y
		tay
		lda classExperienceDivisor,y
		sta DIVISOR+0
		lda #0
		sta DIVISOR+1
		sta DIVIDEND+1
		lda TMP2+0
		sta DIVIDEND+0
		jsr divide
		lda QUOTIENT
		rts
classExperienceDivisor: .byte 1, 1, 1, 1, 1, 1, 2, 2, 2, 3, 2, 2, 3, 2, 2
.endproc

; Appends result to TMP2
.proc appendModifiedHpLimits
		hpModifier = ARGS+0
		constModifier = ARGS+1
		level = ARGS+2
		mode = ARGS+3
		s = TMP+0
		res = TMP+1

		clc
		lda hpModifier
		adc #6
		tax
		lda hpIncrPerLevel,x
		cmp level
		bcc :+
		beq :+
			lda level
		:
		sta s
		sta res

		lda mode
		bne :++
			ldx hpModifier
			bpl :+
				lda #0
				sta res
				jmp :++
			:
				lda hpIncrPerLevel,x
				sta NUM2
				lda res
				sta NUM1+0
				lda #0
				sta NUM1+1
				jsr multiply
				sta res
		:

		lda level
		cmp s
		beq :+
		bmi :+
			sec
			sbc s
			sta s
			sta NUM2
			clc
			lda hpModifier
			adc #12
			tax
			lda hpIncrPerLevel,x
			sta NUM1+0
			lda #0
			sta NUM1+1
			jsr multiply
			clc
			adc res
			sta res
		:

		lda mode
		beq :+
		lda constModifier
		beq :++
		bmi :++
		:
			lda constModifier
			sta NUM2
			lda level
			sta NUM1+0
			lda #0
			sta NUM1+1
			jsr multiply
			clc
			adc res
			sta res
		:

		clc
		lda res
		adc TMP2
		sta TMP2
		rts
hpIncrPerLevel: .byte 10, 4, 8, 6, 10, 10, 9, 10, 9, 10, 9, 9, 3, 1, 2, 2, 3, 3
.endproc

; modifier return in x for far, in a for local
.export getDexterityModifier
.proc getDexterityModifier
		ldy #PartyMember::dexterityCurrent
		lda (CURRENT_PARTYMEMBER),y
		tax
		lda dexterityArmorClassModifier,x
		tax
		rts
dexterityArmorClassModifier:
		.byte 5, 5, 5, 4, 3, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, <-1, <-2, <-3, <-4, <-4, <-5, <-5, <-5, <-6, <-6
.endproc

.export calculateAC 
.proc calculateAC
		jsr getDexterityModifier
		clc
		adc #10
		ldy #PartyMember::ac
		sta (CURRENT_PARTYMEMBER),y
		rts
.endproc

.proc doDeleteMenu
		buttonGfx = assets_base + chargen_delete_bitmap
		buttonSpr = assets_base + chargen_delete_sprites

		ldy #PartyMember::picture
		lda (CURRENT_PARTYMEMBER),y
		sta ARGS+0
		lda #0
		sta ARGS+1
		ldx #26
		ldy #8
		jsrf showPortrait
		jsr showCharacterStats

		bitblt buttonGfx,10,3,28,21
		ldx #11*3-1
		:
			lda buttonSpr+0*11*3,x
			sta vic_sprites + (6*5+3)*64+9*3,x
			lda buttonSpr+1*11*3,x
			sta vic_sprites + (6*5+4)*64+9*3,x
			lda buttonSpr+2*11*3,x
			sta vic_sprites + (6*5+5)*64+9*3,x
			dex
		bpl :-

loop:
		lda KEYEVENT+KeyEvent::keyPressed
		beq doneKeyboard
			ackKeyboard:
			lda #0
			sta KEYEVENT+KeyEvent::keyPressed
			lda KEYEVENT+KeyEvent::normalChar
			cmp #4 ; D - Delete
			bne :+
				lda #0
				jmp done
			:
			cmp #$f; O - OK
			beq :+
			lda KEYEVENT+KeyEvent::specialChar
			cmp #$2 ;Return
			beq :+
			lda KEYEVENT+KeyEvent::modifiers
			cmp #$80
			bne :++
			:
				lda #1
				jmp done
			:
		doneKeyboard:

		lda MOUSEEVENT+MouseEvent::buttons
		bmi loop

		lda #$ff
		sta MOUSEEVENT+MouseEvent::buttons

		lsr MOUSEEVENT+MouseEvent::xpos+1
		ror MOUSEEVENT+MouseEvent::xpos+0
		ldx MOUSEEVENT+MouseEvent::xpos+0
		ldy MOUSEEVENT+MouseEvent::ypos+0

		boundCheckMouse 224,172, 260,187 ; Delete
		bcc :+
			lda #0
			jmp done
		:
		boundCheckMouse 264,172, 300,187 ; OK
		bcc :+
			lda #1
			jmp done
		:
		jmp loop

done:	pha
		jsr clearSpritesAndInstallBox
		jsr restoreTop
		jsr restoreBottom
		pla
		rts
.endproc

.proc restoreTop
		bitblt topBackgroundGfx, 20,4, 18,8
		rts
.endproc

.proc restoreBottom
		bitblt bottomBackgroundGfx, 11,5, 27,19
		rts
.endproc

.pushseg
.segment "BSS"
	minStats: .res 7
	maxStats: .res 7
.popseg

statOffsets:
		.byte PartyMember::strengthCurrent
		.byte PartyMember::intelligenceCurrent
		.byte PartyMember::wisdomCurrent
		.byte PartyMember::dexterityCurrent
		.byte PartyMember::constitutionCurrent
		.byte PartyMember::charismaCurrent
		.byte PartyMember::hpCurrent
		.byte PartyMember::extraStrengthCurrent

statOffsetsMax:
		.byte PartyMember::strength
		.byte PartyMember::intelligence
		.byte PartyMember::wisdom
		.byte PartyMember::dexterity
		.byte PartyMember::constitution
		.byte PartyMember::charisma
		.byte PartyMember::hp
		.byte PartyMember::extraStrength

.proc generateStats
		.pushseg
		.segment "BSS"
			curStat: .res 1
		.popseg

		; Assign levels
		ldy #PartyMember::class
		lda (CURRENT_PARTYMEMBER),y
		asl
		asl
		tax
		ldy #PartyMember::level
		lda startLevels,x
		sta (CURRENT_PARTYMEMBER),y
		inx
		iny
		lda startLevels,x
		sta (CURRENT_PARTYMEMBER),y
		inx
		iny
		lda startLevels,x
		sta (CURRENT_PARTYMEMBER),y
		inx
		lda startLevels,x
		asl
		asl
		tax

		; Assign experience
		.repeat 4,I
			lda startXP+I,x
			ldy #PartyMember::experience+0+I
			sta (CURRENT_PARTYMEMBER),y
			ldy #PartyMember::experience+4+I
			sta (CURRENT_PARTYMEMBER),y
			ldy #PartyMember::experience+8+I
			sta (CURRENT_PARTYMEMBER),y
		.endrep

		; Calculate min/max stats
		lda #5
		sta TMP ;current stat
		calcMinMaxLoop:
			ldy #PartyMember::class
			lda (CURRENT_PARTYMEMBER),y
			tax
			clc
			lda mul6,x
			adc TMP
			tax
			lda classMinStats,x
			sta TMP+1

			ldy #PartyMember::race
			lda (CURRENT_PARTYMEMBER),y
			lsr
			tax
			clc
			lda mul6,x
			adc TMP
			tax
			lda raceMinStats,x
			cmp TMP+1
			bcs :+
				lda TMP+1
			:

			ldy TMP
			sta minStats,y

			lda raceMaxStats,x
			sta maxStats,y

			dec TMP
		bpl calcMinMaxLoop

		; Roll the stats
		lda #5
		sta curStat
		rollStatsLoop:
			jsr roll4t6keep3
			sta TMP
			ldy #PartyMember::race
			lda (CURRENT_PARTYMEMBER),y
			lsr
			tax
			lda raceModifier,x
			ldx curStat
			clc
			adc TMP
			cmp minStats,x
			bcs :+
				lda minStats,x
			:
			cmp maxStats,x
			bcc :+
				lda maxStats,x
			:
			ldy statOffsets,x
			sta (CURRENT_PARTYMEMBER),y
			iny
			sta (CURRENT_PARTYMEMBER),y

			dec curStat
		bpl rollStatsLoop

		; Roll extra strength if STR=18 and classModifier > 0
		ldx #0
		ldy #PartyMember::class
		lda (CURRENT_PARTYMEMBER),y
		tay
		lda classModifier,y
		bne :+
			ldy #PartyMember::strengthCurrent
			lda (CURRENT_PARTYMEMBER),y
			cmp #18
			bne :+
				dice 1,100,0
				jsrf rollDice
				ldx DICE_RESULT
		:
		ldy #PartyMember::extraStrengthCurrent
		txa
		sta (CURRENT_PARTYMEMBER),y
		iny
		sta (CURRENT_PARTYMEMBER),y

		; AC
		jsr calculateAC

		; HP
		ldy #PartyMember::hpCurrent
		lda #0
		sta (CURRENT_PARTYMEMBER),y
		iny
		sta (CURRENT_PARTYMEMBER),y

		ldx #0
		levelIndexLoop:
			lda shift,x
			sta TMP3
			txa
			pha
				clc
				adc #PartyMember::level
				tay
				lda (CURRENT_PARTYMEMBER),y
				tax
				levelLoop:
					dex
					bmi doneLevel
					txa
					pha
						lda TMP3
						sta ARGS+0
						jsrf generateCharacterHitpointsByLevel

						clc
						ldy #PartyMember::hpCurrent
						lda (CURRENT_PARTYMEMBER),y
						adc TMP2+0
						sta (CURRENT_PARTYMEMBER),y
						iny
						lda (CURRENT_PARTYMEMBER),y
						adc #0
						sta (CURRENT_PARTYMEMBER),y
					pla
					tax
				jmp levelLoop
				doneLevel:
			pla
			tax
			inx
			cpx #3
		bne levelIndexLoop

		jsr calculateMinMaxHP

		rts
shift:	.byte 1,2,4

roll4t6keep3:
		lda #0
		sta TMP2+0
		lda #10
		sta TMP2+1
		ldx #4
		:
			dice 1,6,0
			jsrf rollDice
			clc
			lda TMP2+0
			adc DICE_RESULT
			sta TMP2+0
			lda DICE_RESULT
			cmp TMP2+1
			bcs :+
				sta TMP2+1
			:
			dex
		bne :--
		sec
		lda TMP2+0
		sbc TMP2+1
		rts

startXP:
		.dword 5000/1
		.dword 5000/2
		.dword 5000/3

startLevels:
		; Level for multiclass 1,2 & 3 + experience divider (divider changed to table lookups 0,1 or 2 instead)
		.byte 3, 0, 0, 1-1  ;MULTICLASS_FIGHTER
		.byte 3, 0, 0, 1-1  ;MULTICLASS_RANGER
        .byte 3, 0, 0, 1-1  ;MULTICLASS_PALADIN
        .byte 3, 0, 0, 1-1  ;MULTICLASS_MAGE
        .byte 3, 0, 0, 1-1  ;MULTICLASS_CLERIC
        .byte 4, 0, 0, 1-1  ;MULTICLASS_THIEF
        .byte 2, 2, 0, 2-1  ;MULTICLASS_FIGHTER_CLERIC
        .byte 2, 3, 0, 2-1  ;MULTICLASS_FIGHTER_THIEF
        .byte 2, 2, 0, 2-1  ;MULTICLASS_FIGHTER_MAGE
        .byte 1, 1, 2, 3-1  ;MULTICLASS_FIGHTER_MAGE_THIEF
        .byte 3, 2, 0, 2-1  ;MULTICLASS_THIEF_MAGE
        .byte 2, 3, 0, 2-1  ;MULTICLASS_CLERIC_THIEF
        .byte 1, 2, 1, 3-1  ;MULTICLASS_FIGHTER_CLERIC_MAGE
        .byte 2, 2, 0, 2-1  ;MULTICLASS_RANGER_CLERIC
        .byte 2, 2, 0, 2-1  ;MULTICLASS_CLERIC_MAGE

classMinStats:
 	        ; STR, INT, WIS, DEX, CON, CHA 
		.byte $09, $00, $00, $00, $00, $00  ;MULTICLASS_FIGHTER
		.byte $0D, $00, $0E, $0D, $0E, $00  ;MULTICLASS_RANGER
		.byte $0C, $00, $0D, $00, $09, $11  ;MULTICLASS_PALADIN
		.byte $00, $09, $00, $00, $00, $00  ;MULTICLASS_MAGE
        .byte $00, $00, $09, $00, $00, $00  ;MULTICLASS_CLERIC 
        .byte $00, $00, $00, $09, $00, $00  ;MULTICLASS_THIEF
        .byte $09, $00, $09, $00, $00, $00  ;MULTICLASS_FIGHTER_CLERIC
        .byte $09, $00, $00, $09, $00, $00  ;MULTICLASS_FIGHTER_THIEF
        .byte $09, $09, $00, $00, $00, $00  ;MULTICLASS_FIGHTER_MAGE
        .byte $09, $09, $00, $09, $00, $00  ;MULTICLASS_FIGHTER_MAGE_THIEF
        .byte $00, $09, $00, $09, $00, $00  ;MULTICLASS_THIEF_MAGE
        .byte $00, $00, $09, $09, $00, $00  ;MULTICLASS_CLERIC_THIEF
        .byte $09, $09, $09, $00, $00, $00  ;MULTICLASS_FIGHTER_CLERIC_MAGE
        .byte $0D, $00, $0E, $0D, $0E, $00  ;MULTICLASS_RANGER_CLERIC
        .byte $00, $09, $09, $00, $00, $00  ;MULTICLASS_CLERIC_MAGE

raceModifier:
 	        ; STR, INT, WIS, DEX, CON, CHA 
		.byte $00, $00, $00, $00, $00, $00  ;RACE_HUMAN
		.byte $00, $00, $00, $01, $ff, $00  ;RACE_ELF
		.byte $00, $00, $00, $00, $00, $00  ;RACE_HALFELF
		.byte $00, $00, $00, $00, $01, $ff  ;RACE_DWARF
		.byte $00, $01, $ff, $00, $00, $00  ;RACE_GNOME
		.byte $ff, $00, $00, $01, $00, $00  ;RACE_HALFLING

raceMinStats:
			; STR, INT, WIS, DEX, CON, CHA 
		.byte $03, $03, $03, $03, $03, $03
		.byte $03, $08, $03, $07, $06, $08
		.byte $03, $04, $03, $06, $06, $03
		.byte $08, $03, $03, $03, $0c, $02
        .byte $06, $07, $02, $03, $08, $03
        .byte $07, $06, $03, $08, $0a, $06

raceMaxStats:
			; STR, INT, WIS, DEX, CON, CHA 
  		.byte $12, $12, $12, $12, $12, $12
  		.byte $12, $12, $12, $13, $11, $12
  		.byte $12, $12, $12, $12, $12, $12
  		.byte $12, $12, $12, $11, $13, $10
  		.byte $12, $13, $11, $12, $12, $12
  		.byte $11, $12, $11, $13, $12, $12

mul6:	.repeat 15,I
			.byte I*6
		.endrep
.endproc
classModifier:
		.byte 0, 0, 0, 1, 2, 3, 0, 0, 0, 0, 3, 2, 0, 0, 2

.proc doEnterName
		.pushseg
		.segment "BSS"
			inputCursor: .res 1
			flashCursor: .res 1
		.popseg

		lda #0
		sta inputCursor
		sta flashCursor
		lda #15
		sta clearstop

		ldx #0
		ldy #3
		jsr setCursor
		ldx #<nameStr
		ldy #>nameStr
		jsr drawString

		loop:
			; Redraw name
			ldx #6
			ldy #3
			jsr setCursor

			lda #0
			sta reverse

			ldx CURRENT_PARTYMEMBER_INDEX
			lda names_hi,x
			tay
			lda names_lo,x
			tax
			jsr drawString

			; Clear until end
			jsr clearToStop
			dec cursorY

			; Draw cursor
			lda flashCursor
			eor #1
			sta flashCursor
			beq :++
				lda #$ff
				sta reverse

				clc
				lda inputCursor
				tay
				adc #6
				sta cursorX

				ldy inputCursor
				lda (SRC2),y
				bne :+
					lda #' '
				:
				jsr putChar
			:

			; Poll keyboard
			lda #10
			sta waitCount
			pollKeyboard:
				lda KEYEVENT+KeyEvent::keyPressed
				bne keyPressed
					jmp doneKeyboard
				keyPressed:
					lda #0
					sta KEYEVENT+KeyEvent::keyPressed

					checkCursorKeys:
						lda KEYEVENT+KeyEvent::specialChar
						cmp #$04 ; crsr lr
						bne checkDelete
							lda KEYEVENT+KeyEvent::modifiers
							and #$50 ;left/right shift
							bne left
							right:
								ldy inputCursor
								lda (SRC2),y
								beq :+
									inc inputCursor
								:
								jmp donePoll
							left:
								dec inputCursor
								jmp donePoll

					checkDelete:
						cmp #$01 ; delete
						bne checkEnter
							ldy inputCursor
							beq donePoll
								:
									lda (SRC2),y
									dey
									sta (SRC2),y
									cmp #0
									beq :+
									iny
									iny
								jmp :-
								:
								dec inputCursor
								jmp donePoll

					checkEnter:
						cmp #$02 ; enter
						bne checkNormalKeys
							ldy #0
							lda (SRC2),y
							beq doneKeyboard
								rts

					checkNormalKeys:
						ldx KEYEVENT+KeyEvent::normalChar
						cpx #$40
						bcs doneKeyboard
							lda keyChars,x
							beq doneKeyboard
								cmp #' '
								bne :+
									ldx inputCursor
									bne :+
										jmp doneKeyboard
								:
								pha
								ldy #9
								:
									cpy inputCursor
									beq :+
									dey
									lda (SRC2),y
									iny
									sta (SRC2),y
									dey
								jmp :-
								:
								pla
								ldy inputCursor
								sta (SRC2),y
								inc inputCursor
								jmp donePoll
				doneKeyboard:

				lda waitCount
				beq donePollNoFlash
			jmp pollKeyboard
			donePoll:
				lda #0
				sta flashCursor
			donePollNoFlash:

			; Bound cursor
			lda inputCursor
			bpl :+
				lda #0
			:
			cmp #10
			bcc :+
				lda #9
			:
			sta inputCursor
		jmp loop

keyChars:
		.byte 0
		.byte 'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'
		.byte 0,0,0,$27,0
		.byte ' '
		.byte 0,0,0,0,0,0,0,0,0
		.byte '*','+',',','-','.','/'
		.byte '0','1','2','3','4','5','6','7','8','9'
		.byte ':',';'
		.byte 0
		.byte '='
		.byte 0,0
		.assert *-keyChars = 64, error, "Should be 64 keys!"

nameStr:.byte "Name:",0
.endproc

names_lo:
		.repeat 4,I
			.byte <(party + .sizeof(PartyMember)*I + PartyMember::name)
		.endrep
names_hi:
		.repeat 4,I
			.byte >(party + .sizeof(PartyMember)*I + PartyMember::name)
		.endrep

.proc clearName
		; Clear name
		ldx #11
		ldy #PartyMember::name
		lda #0
		:
			sta (CURRENT_PARTYMEMBER),y
			iny
			dex
		bne :-
		rts
.endproc

.export createDefaultParty
.proc createDefaultParty
		jsrf initGameState

		ldx #3
		nextMember:
			lda members_lo,x
			sta SRC+0
			lda members_hi,x
			sta SRC+1

			stx CURRENT_PARTYMEMBER_INDEX
			jsr getPartyMember

			clc
			lda CURRENT_PARTYMEMBER+0
			adc #<(PartyMember::name)
			sta DST+0
			lda CURRENT_PARTYMEMBER+1
			adc #>(PartyMember::name)
			sta DST+1

			ldy #10
			:
				lda (SRC),y
				sta (DST),y
				dey
			bpl :-

			ldy #11
			lda (SRC),y
			ldy #PartyMember::race
			sta (CURRENT_PARTYMEMBER),y

			ldy #12
			lda (SRC),y
			ldy #PartyMember::class
			sta (CURRENT_PARTYMEMBER),y

			ldy #13
			lda (SRC),y
			ldy #PartyMember::alignment
			sta (CURRENT_PARTYMEMBER),y

			ldy #14
			lda (SRC),y
			ldy #PartyMember::picture
			sta (CURRENT_PARTYMEMBER),y

			txa
			pha
			jsr generateStats
			pla
			tax

			dex
		bpl nextMember
		jmp doFinish

members_lo:
		.byte <_1, <_2, <_3, <_4
members_hi:
		.byte >_1, >_2, >_3, >_4

_1:		.byte "K",125,"mpferA  ",0
		.byte (RACE_HUMAN<<1)|SEX_MALE
		.byte MULTICLASS_FIGHTER
		.byte ALIGNMENT_LAWFUL_GOOD
		.byte 0
_2:		.byte "K",125,"mpferB  ",0
		.byte (RACE_HUMAN<<1)|SEX_MALE
		.byte MULTICLASS_FIGHTER
		.byte ALIGNMENT_LAWFUL_GOOD
		.byte 1
_3:		.byte "Magier    ",0
		.byte (RACE_ELF<<1)|SEX_FEMALE
		.byte MULTICLASS_MAGE
		.byte ALIGNMENT_LAWFUL_GOOD
		.byte 29
_4:		.byte "Kleriker  ",0
		.byte (RACE_HALFELF<<1)|SEX_FEMALE
		.byte MULTICLASS_CLERIC
		.byte ALIGNMENT_LAWFUL_GOOD
		.byte 30
.endproc

.proc doFinish
		; Commission and letter of Marque
		lda #35
		sta party + .sizeof(PartyMember)*0 + PartyMember::inventory + INVENTORY_BACKPACK*2

		ldx #0
		loop:
			stx CURRENT_PARTYMEMBER_INDEX
			jsr getPartyMember

			; Make active
			lda #PARTYMEMBER_STATUS_ACTIVE
			ldy #PartyMember::status
			sta (CURRENT_PARTYMEMBER),y

			; Full food
			lda #100
			ldy #PartyMember::food
			sta (CURRENT_PARTYMEMBER),y

			; Position
			txa
			ldy #PartyMember::position
			sta (CURRENT_PARTYMEMBER),y

			; Some food in the backpack
			lda #10
			ldy #(PartyMember::inventory + (INVENTORY_BACKPACK+1)*2)
			sta (CURRENT_PARTYMEMBER),y

.if 0
			; Default values for testing
			lda #MULTICLASS_MAGE
			ldy #PartyMember::class
			sta (CURRENT_PARTYMEMBER),y
			lda #RACE_HUMAN*2 + SEX_MALE
			ldy #PartyMember::race
			sta (CURRENT_PARTYMEMBER),y
 
			lda #75
			ldy #PartyMember::hpCurrent
			sta (CURRENT_PARTYMEMBER),y
			iny
			lda #0
			sta (CURRENT_PARTYMEMBER),y

			lda #100
			ldy #PartyMember::hp
			sta (CURRENT_PARTYMEMBER),y
			iny
			lda #0
			sta (CURRENT_PARTYMEMBER),y
.endif
			; Create up to 4 default items
			ldy #PartyMember::class
			lda (CURRENT_PARTYMEMBER),y
			asl
			asl
			tax

			lda classDefaultItemsListIndex+0,x
			bmi :+
				jsr createItem
			:
			lda classDefaultItemsListIndex+1,x
			bmi :+
				jsr createItem
			:
			lda classDefaultItemsListIndex+2,x
			bmi :+
				jsr createItem
			:
			lda classDefaultItemsListIndex+3,x
			bmi :+
				jsr createItem
			:

			; Default mage spells
			ldy #PartyMember::class
			lda (CURRENT_PARTYMEMBER),y
			tax
			lda classProfessions,x
			and #IS_MAGE
			beq :+
				ldy #PartyMember::scribedScrolls+0
				lda #<$26c
				sta (CURRENT_PARTYMEMBER),y
				iny
				lda #>$26c
				sta (CURRENT_PARTYMEMBER),y
			:

			; Copy current stats to max stats
			ldx #7
			:
				ldy statOffsets,x
				lda (CURRENT_PARTYMEMBER),y
				ldy statOffsetsMax,x
				sta (CURRENT_PARTYMEMBER),y
				dex
			bpl :-

			ldx CURRENT_PARTYMEMBER_INDEX
			inx
			cpx #4
			beq :+
		jmp loop
		:
		rts

classProfessions: .byte 1, 1, 5, 2, 4, 8, 5, 9, 3,11,10,12, 7, 5, 6 ; See gamestate.s for full explanation

; classDefaultItemsListIndex in A
createItem:
		stx TMP

		asl
		tay
		ldx classDefaultItemsList,y
		iny
		lda classDefaultItemsList,y

		; x=itemIndex, a=slot
		bpl directSlot
			; Find empty hand
			ldy #PartyMember::inventory
			lda (CURRENT_PARTYMEMBER),y
			beq :+
				iny
				iny
			:
			lda (CURRENT_PARTYMEMBER),y
			beq :+
				iny
				iny
			:
			jmp :+
		directSlot:
			; Calculate &slot
			asl
			adc #PartyMember::inventory
			tay
		:

		; y=&slot

		; If a direct slot was selected and is occupied, bail out
		cpy #PartyMember::inventory + INVENTORY_BACKPACK*2
		beq :+
			lda (CURRENT_PARTYMEMBER),y
			bne continue
		:

		; Switch short sword to axe if drawf
		cpx #5
		bne :++
			tya
			pha
			ldy #PartyMember::race
			lda (CURRENT_PARTYMEMBER),y
			lsr
			cmp #RACE_DWARF
			bne :+
				ldx #36
			:
			pla
			tay
		:

		; Find first empty slot if in backback
		cpy #PartyMember::inventory + INVENTORY_BACKPACK*2
		bne :+
			next:lda (CURRENT_PARTYMEMBER),y
			beq :+
				iny
				iny
				jmp next
		:

		; Assign item to slot
		txa
		sta (CURRENT_PARTYMEMBER),y
		iny
		lda #0
		sta (CURRENT_PARTYMEMBER),y

continue:
		ldx TMP
		rts

classDefaultItemsList:
		;   Item, Position (-1 = free hand)
		.byte  1, INVENTORY_BREAST_ARMOUR	;  0:Leather armor
		.byte  2, INVENTORY_BREAST_ARMOUR	;  1:Robe
		.byte 46, <-1						;  2:Mace
		.byte  4, <-1						;  3:Dagger
		.byte  5, <-1						;  4:Short sword
		.byte  6, INVENTORY_BACKPACK		;  5:Lock picks
		.byte  7, <-1						;  6:Spellbook
		.byte  8, <-1						;  7:Cleric Holy symbol
		.byte  9, INVENTORY_BOOTS			;  8:Leather boots
		.byte 10, INVENTORY_BACKPACK		;  9:Iron Rations
		.byte 31, INVENTORY_BACKPACK		; 10:Paladin Holy Symbol

classDefaultItemsListIndex:
		;Item 0   1   2   3 (-1 = no item)
		.byte 4,  8,  0,<-1  ;MULTICLASS_FIGHTER
		.byte 4,  3,  0,<-1  ;MULTICLASS_RANGER
		.byte 4, 10,  0,  8  ;MULTICLASS_PALADIN
		.byte 3,  6,  1,<-1  ;MULTICLASS_MAGE
		.byte 2,  7,  0,<-1  ;MULTICLASS_CLERIC
		.byte 4,  5,  0,<-1  ;MULTICLASS_THIEF
		.byte 4,  7,  0,  8  ;MULTICLASS_FIGHTER_CLERIC
		.byte 4,  5,  0,  8  ;MULTICLASS_FIGHTER_THIEF
		.byte 4,  6,  8,  8  ;MULTICLASS_FIGHTER_MAGE
		.byte 4,  6,  5,  8  ;MULTICLASS_FIGHTER_MAGE_THIEF
		.byte 3,  6,  5,<-1  ;MULTICLASS_THIEF_MAGE
		.byte 2,  7,  5,  0  ;MULTICLASS_CLERIC_THIEF
		.byte 4,  6,  7,  0  ;MULTICLASS_FIGHTER_CLERIC_MAGE
		.byte 4,  3,  7,  0  ;MULTICLASS_RANGER_CLERIC
		.byte 2,  6,  7,  1  ;MULTICLASS_CLERIC_MAGE
.endproc

.export chargen
.proc chargen
		jsrf decrunchMusic

		lda #<assets_base
		sta BB_DST+0
		lda #>assets_base
		sta BB_DST+1
		ldy #<background
		ldx #>background
		jsr decrunchTo
		memcpy vic_bitmap, assets_base, 8000
		memcpy vic_screen, (assets_base+8000), 1000
		memcpy $d800, (assets_base+9000), 1000
		jsrf grabGraphics

		lda #<assets_base
		sta BB_DST+0
		lda #>assets_base
		sta BB_DST+1
		ldy #<assets
		ldx #>assets
		jsr decrunchTo

		jsrf initGameState

		lda #0
		sta tabstop
		sta KEYEVENT+KeyEvent::keyPressed
		sta KEYEVENT+MouseEvent::buttons

		jsr initMusic

		lda #15
		sta musicVolume
		lda #1
		sta musicEnabled
		lda #POINTERENABLED_REAL
		sta pointerEnabled

		jsr clearSpritesAndInstallBox

		clc
		ldx #<screenMode
		ldy #>screenMode
		jsr setScreenMode
		jsrf mouse_vic

	;	jmp playPressed

		lda #30
		sta autoFireLimit

		lda #0
		sta CURRENT_PARTYMEMBER_INDEX
		loop:
			jsr doDrawFaces

			back0:
			jsr doSelectMember
			ldx CURRENT_PARTYMEMBER_INDEX
			jsr getPartyMember
			bcs :+
				jmp playPressed
			:

			; Delete partymember if already existing
			ldy #PartyMember::name
			lda (CURRENT_PARTYMEMBER),y
			beq :+
				jsr doDeleteMenu
				cmp #0 ; Delete
				bne back0
					jsr clearName
				jmp loop
			:

			jsr clearName

			back1:jsr doRaceMenu
			bcs :+
				jsr restoreBottom
				jmp back0
			:

			back2:jsr doClassMenu
			bcc back1 

			jsr doAlignmentMenu
			bcc back2

			jsr restoreBottom
			jsr generateStats
			jsr showCharacterStats
			jsr doFaceMenu

			statsMenu:
			jsr doStatsMenu
			cmp #0 ; Reroll
			bne :+
				jsr generateStats
				jsr showCharacterStats
				jmp statsMenu
			:
			cmp #1 ; Modify
			bne :+
				jsr doModifyMenu
				jmp statsMenu
			:
			cmp #2 ; Faces
			bne :+
				jsr doFaceMenu
				jmp statsMenu
			:

			jsr doEnterName

			jsr restoreTop
			jsr clearSpritesAndInstallBox

			; Next
			ldx CURRENT_PARTYMEMBER_INDEX
			inx
			txa
			and #3
			sta CURRENT_PARTYMEMBER_INDEX
		jmp loop

playPressed:
		jsr doFinish

		clc
		ldx #<screenModeNoop
		ldy #>screenModeNoop
		jsr setScreenMode

		lda #0
		sta autoFireLimit
		sta musicEnabled
		ldx #$1f
		:
			sta $d400,x
			dex
		bpl :-
		rts
.endproc

.pushseg
.segment "CHARGEN2"
; a=rows
; x=xpos
; y=ypos
; SRC=*bitmap
; SRC2=*screen
; SRC3=*d800
; TMP2+0=cols*8
; TMP2+1=cols
.proc _tables
;		vic_bitmap_row_lo:
		.repeat 25,I
			.byte <(vic_bitmap+I*320)
		.endrep
;		vic_bitmap_row_hi:
		.repeat 25,I
			.byte >(vic_bitmap+I*320)
		.endrep
;		vic_screen_row_lo:
		.repeat 25,I
			.byte <(vic_screen+I*40)
		.endrep
;		vic_screen_row_hi:
		.repeat 25,I
			.byte >(vic_screen+I*40)
		.endrep
.endproc

.proc _grabTemplate
		pha
		lda #$35
		sta 1
		; DST2 = vic_screen[y]+x
		; DST3 = d800[y]+x
		stx TMP
		clc
		lda vic_screen_row_lo,y
		adc TMP
		sta DST2+0
		sta DST3+0
		lda vic_screen_row_hi,y
		adc #0
		sta DST2+1
		clc
		adc #>($d800-vic_screen)
		sta DST3+1

		; TMP=x*8
		lda #0
		asl TMP+0
		rol
		asl TMP+0
		rol
		asl TMP+0
		rol
		sta TMP+1

		; DST = vic_bitmap[y]+TMP
		clc
		lda vic_bitmap_row_lo,y
		adc TMP+0
		sta DST+0
		lda vic_bitmap_row_hi,y
		adc TMP+1
		sta DST+1
 
		pla
		tax
		rowLoop:
			ldy #0
			:
				lda (DST),y
				sta (SRC),y
				iny
				cpy TMP2+0
			bne :-
			ldy #0
			:
				lda (DST2),y
				sta (SRC2),y
				lda (DST3),y
				sta (SRC3),y
				iny
				cpy TMP2+1
			bne :-

			clc
			lda SRC+0
			adc TMP2+0
			sta SRC+0
			bcc :+
				inc SRC+1
				clc
			:
			lda SRC2+0
			adc TMP2+1
			sta SRC2+0
			bcc :+
				inc SRC2+1
				clc
			:
			lda SRC3+0
			adc TMP2+1
			sta SRC3+0
			bcc :+
				inc SRC3+1
				clc
			:
			lda DST+0
			adc #<320
			sta DST+0
			lda DST+1
			adc #>320
			sta DST+1
			lda DST2+0
			adc #40
			sta DST2+0
			sta DST3+0
			bcc :+
				inc DST2+1
				inc DST3+1
			:
			dex
		bpl rowLoop
		lda #$37
		sta 1
		rts
.endproc

.proc grabGraphics
		memcpy vic_bitmap_row_lo, _tables, (.sizeof(_tables) + .sizeof(_grabTemplate))

		grab topBackgroundGfx,20,4,18,8
		grab bg0Gfx,4,4,2,8
		grab bg1Gfx,4,4,10,8
		grab bg2Gfx,4,4,2,16
		grab bg3Gfx,4,4,10,16
		grab bottomBackgroundGfx,11,5,27,19
		grab txt0Gfx,8,1,0,13
		grab txt1Gfx,8,1,8,13
		grab txt2Gfx,8,1,0,21
		grab txt3Gfx,8,1,8,21
		grab playGfx,11,3,3,22
		rts
.endproc
.popseg

.proc centerName
		ldy #0
		:
			lda (SRC),y
			beq :+
			iny
		jmp :-
		:
		sec
		lda #8
		sty TMP
		sbc TMP
		bpl :+
			lda #0
		:
		clc
		adc #1
		lsr
		clc
		stx TMP
		adc TMP
		tax
		rts
.endproc

.proc _drawName
		jsrf setupDestination
		inc DST
		lda #3
		sta textColor
		jsrf drawNamePlain
		rts
.endproc

.proc doDrawFaces
		ldx #2
		ldy #8
		lda party + .sizeof(PartyMember)*0 + PartyMember::name
		bne :+
			bitblt2 bg0Gfx,4,4
			jmp :++
		:
			lda party + .sizeof(PartyMember)*0 + PartyMember::picture
			jsr show
		:
		bitblt txt0Gfx,8,1,0,13
		lda #<(party + .sizeof(PartyMember)*0 + PartyMember::name)
		sta SRC+0
		lda #>(party + .sizeof(PartyMember)*0 + PartyMember::name)
		sta SRC+1
		ldx #0
		jsr centerName
		ldy #13
		jsr _drawName

		ldx #10
		ldy #8
		lda party + .sizeof(PartyMember)*1 + PartyMember::name
		bne :+
			bitblt2 bg1Gfx,4,4
			jmp :++
		:
			lda party + .sizeof(PartyMember)*1 + PartyMember::picture
			jsr show
		:
		bitblt txt1Gfx,8,1,8,13
		lda #<(party + .sizeof(PartyMember)*1 + PartyMember::name)
		sta SRC+0
		lda #>(party + .sizeof(PartyMember)*1 + PartyMember::name)
		sta SRC+1
		ldx #8
		jsr centerName
		ldy #13
		jsr _drawName

		ldx #2
		ldy #16
		lda party + .sizeof(PartyMember)*2 + PartyMember::name
		bne :+
			bitblt2 bg2Gfx,4,4
			jmp :++
		:
			lda party + .sizeof(PartyMember)*2 + PartyMember::picture
			jsr show
		:
		bitblt txt2Gfx,8,1,0,21
		lda #<(party + .sizeof(PartyMember)*2 + PartyMember::name)
		sta SRC+0
		lda #>(party + .sizeof(PartyMember)*2 + PartyMember::name)
		sta SRC+1
		ldx #0
		jsr centerName
		ldy #21
		jsr _drawName

		ldx #10
		ldy #16
		lda party + .sizeof(PartyMember)*3 + PartyMember::name
		bne :+
			bitblt2 bg3Gfx,4,4
			jmp :++
		:
			lda party + .sizeof(PartyMember)*3 + PartyMember::picture
			jsr show
		:
		bitblt txt3Gfx,8,1,8,21
		lda #<(party + .sizeof(PartyMember)*3 + PartyMember::name)
		sta SRC+0
		lda #>(party + .sizeof(PartyMember)*3 + PartyMember::name)
		sta SRC+1
		ldx #8
		jsr centerName
		ldy #21
		jsr _drawName
		rts

show:	sta ARGS+0
		lda #0
		sta ARGS+1
		jsrf showPortrait
		rts
.endproc
