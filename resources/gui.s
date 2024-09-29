.include "global.inc"

.segment "BSS"
regionHandler:	.res 2
.export partyMemberStatsChanged
partyMemberStatsChanged:	.res 6
.export shouldUpdateCompass
shouldUpdateCompass:	.res 1
.export partyCompositionChanged
partyCompositionChanged: .res 1
showingStats: .res 1
transparentText: .res 1
SPELLBOOK_LEVEL_INDEX: .res 1

.segment "PORTRAITS"
.export showPortrait
.proc showPortrait
		; ARGS+0=index 0..56
		; ARGS+1. 0=normal 1=unconscious 2=invisible
		; x=char x
		; y=char y
		lda ARGS+0

		; TMP = index*16
		sta TMP+0
		lda #0
		asl TMP+0
		rol
		asl TMP+0
		rol
		asl TMP+0
		rol
		asl TMP+0
		rol
		sta TMP+1

		; SRC2 = portraitsScreen+TMP
		clc
		lda #<portraitsScreen
		adc TMP+0
		sta SRC2+0
		lda #>portraitsScreen
		adc TMP+1
		sta SRC2+1

		; SRC3 = portraitsD800+TMP
		clc
		lda #<portraitsD800
		adc TMP+0
		sta SRC3+0
		lda #>portraitsD800
		adc TMP+1
		sta SRC3+1

		; TMP *=8
		lda TMP+1
		asl TMP+0
		rol
		asl TMP+0
		rol
		asl TMP+0
		rol
		sta TMP+1

		; SRC = portraitsBitmap+TMP
		clc
		lda #<portraitsBitmap
		adc TMP+0
		sta SRC+0
		lda #>portraitsBitmap
		adc TMP+1
		sta SRC+1

		jsrf setupDestination

		; Unconscious mask
		ldx #$ff
		ldy #$00
		lda ARGS+1
		cmp #1
		bne :+
			ldx #%00110011
			ldy #%11111111
		:
		cmp #2
		bne :+
			ldx #0
;			ldy #0
		:
		stx TMP+0
		sty TMP+1

		ldx #3
		rowLoop:
			ldy #31
			:
				lda (SRC),y
				and TMP+0
				sta (DST),y
				lda TMP+0
				eor TMP+1
				sta TMP+0
				dey
			bpl :-
			ldy #3
			:
				lda (SRC2),y
				sta (DST2),y
				lda (SRC3),y
				sta (DST3),y
				dey
			bpl :-

			clc
			lda SRC+0
			adc #32
			sta SRC+0
			bcc :+
				inc SRC+1
				clc
			:
			lda SRC2+0
			adc #4
			sta SRC2+0
			bcc :+
				inc SRC2+1
				clc
			:
			lda SRC3+0
			adc #4
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
		rts

portraitsBitmap:
		.incbin "converters/portraits/portraits-bitmap.bin"

portraitsScreen:
		.incbin "converters/portraits/portraits-screen.bin"

portraitsD800:
		.incbin "converters/portraits/portraits-d800.bin"

.endproc

.segment "GUI3"

; x=xpos
; y=ypos
.export setupDestination
.proc setupDestination
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

		; TMP=vic_bitmap_col[x]
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
		rts


		vic_bitmap_row_lo:
		.repeat 25,I
			.byte <(vic_bitmap+I*320)
		.endrep
		vic_bitmap_row_hi:
		.repeat 25,I
			.byte >(vic_bitmap+I*320)
		.endrep
		vic_screen_row_lo:
		.repeat 25,I
			.byte <(vic_screen+I*40)
		.endrep
		vic_screen_row_hi:
		.repeat 25,I
			.byte >(vic_screen+I*40)
		.endrep
.endproc

.export gui_refreshWholeParty
.proc gui_refreshWholeParty
	lda #1
	ldx #5
	:
		sta partyMemberStatsChanged,x
		dex
	bpl :-
	rts
.endproc

;x=memberIndex
;y=frameIndex
.export magicalGlimmer
.proc magicalGlimmer
	.pushseg
	.segment "BSS"
		backup: .res 4*4*2*6
	.popseg
	frameIndex=TMP
	currentOffset=TMP2
	fadeOffset=TMP2+1

	sty frameIndex

	lda backupScreen_lo,x
	sta DST+0
	lda backupScreen_hi,x
	sta DST+1
	lda backupD800_lo,x
	sta DST2+0
	lda backupD800_hi,x
	sta DST2+1
	lda screen_lo,x
	sta SRC+0
	lda screen_hi,x
	sta SRC+1
	lda d800_lo,x
	sta SRC2+0
	lda d800_hi,x
	sta SRC2+1

	cpy #0
	bne skipBackup
		; Create backup
		ldx #15
		:
			ldy offsets,x
			lda (SRC),y
			ldy backup_offsets,x
			sta (DST),y
			ldy offsets,x
			lda (SRC2),y
			and #$0f
			ldy backup_offsets,x
			sta (DST2),y
			dex
		bpl :-
	skipBackup:

	ldx #15
	stx currentOffset
	:
		lda #15
		sec
		ldx currentOffset
		sbc delays,x
		clc
		adc frameIndex
		sta fadeOffset
		tax

		; screen, hi nybble
		ldy currentOffset
		lda (DST),y
		and #$f0
		clc
		adc delayToFadeIndex,x
		tax
		lda fadeTable,x
		asl
		asl
		asl
		asl
		pha

		; screen, lo-nybble
		lda (DST),y
		and #$0f
		asl
		asl
		asl
		asl
		ldx fadeOffset
		adc delayToFadeIndex,x
		tax
		pla
		ora fadeTable,x

		; Store screen
		ldx currentOffset
		ldy offsets,x
		sta (SRC),y

		; $d800
		ldy currentOffset
		lda (DST2),y
		asl
		asl
		asl
		asl
		ldx fadeOffset
		adc delayToFadeIndex,x
		tax
		lda fadeTable,x

		; Store $d800
		ldx currentOffset
		ldy offsets,x
		sta (SRC2),y

		dec currentOffset
	bpl :-
	ldy frameIndex
	rts

fadeTable:
	.byte $0,$6,$2,$4,$c,$5,$3,$7,$1,$7,$3,$5,$c,$4,$2,$6
	.byte $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
	.byte $2,$4,$c,$c,$5,$3,$7,$7,$1,$7,$7,$3,$5,$c,$c,$4
	.byte $3,$3,$7,$7,$7,$7,$1,$1,$1,$1,$1,$7,$7,$7,$7,$3
	.byte $4,$c,$c,$5,$3,$3,$7,$7,$1,$7,$7,$3,$3,$5,$c,$c
	.byte $5,$5,$3,$3,$7,$7,$7,$1,$1,$1,$7,$7,$7,$3,$3,$5
	.byte $6,$2,$4,$c,$5,$5,$3,$7,$1,$7,$3,$5,$5,$c,$4,$2
	.byte $7,$7,$7,$7,$1,$1,$1,$1,$1,$1,$1,$1,$1,$7,$7,$7
	.byte $8,$e,$e,$a,$f,$f,$d,$d,$1,$d,$d,$f,$f,$a,$e,$e
	.byte $9,$b,$8,$e,$a,$a,$f,$d,$1,$d,$f,$a,$a,$e,$8,$b
	.byte $a,$a,$f,$f,$d,$d,$d,$1,$1,$1,$d,$d,$d,$f,$f,$a
	.byte $b,$8,$e,$e,$a,$f,$d,$d,$1,$d,$d,$f,$a,$e,$e,$8
	.byte $c,$5,$5,$3,$3,$7,$7,$1,$1,$1,$7,$7,$3,$3,$5,$5
	.byte $d,$d,$d,$d,$1,$1,$1,$1,$1,$1,$1,$1,$1,$d,$d,$d
	.byte $e,$a,$a,$f,$f,$d,$d,$1,$1,$1,$d,$d,$f,$f,$a,$a
	.byte $f,$f,$d,$d,$d,$d,$1,$1,$1,$1,$1,$d,$d,$d,$d,$f

delayToFadeIndex:
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

delays:	.byte 13,4,1,2,9,15,6,14,0,3,10,11,12,5,8,7

backupScreen_lo:
	.byte <(backup+32*0+0)
	.byte <(backup+32*1+0)
	.byte <(backup+32*2+0)
	.byte <(backup+32*3+0)
	.byte <(backup+32*4+0)
	.byte <(backup+32*5+0)
	.byte <(backup+32*0+0)

backupScreen_hi:
	.byte >(backup+32*0+0)
	.byte >(backup+32*1+0)
	.byte >(backup+32*2+0)
	.byte >(backup+32*3+0)
	.byte >(backup+32*4+0)
	.byte >(backup+32*5+0)
	.byte >(backup+32*0+0)

backupD800_lo:
	.byte <(backup+32*0+16)
	.byte <(backup+32*1+16)
	.byte <(backup+32*2+16)
	.byte <(backup+32*3+16)
	.byte <(backup+32*4+16)
	.byte <(backup+32*5+16)
	.byte <(backup+32*0+16)

backupD800_hi:
	.byte >(backup+32*0+16)
	.byte >(backup+32*1+16)
	.byte >(backup+32*2+16)
	.byte >(backup+32*3+16)
	.byte >(backup+32*4+16)
	.byte >(backup+32*5+16)
	.byte >(backup+32*0+16)

backup_offsets:
	.byte 0,1,2,3
	.byte 4,5,6,7
	.byte 8,9,10,11
	.byte 12,13,14,15

offsets:.byte 0,1,2,3
	.byte 40,41,42,43
	.byte 80,81,82,83
	.byte 120,121,122,123

screen_lo:
	.byte <(vic_screen+23+2*40)
	.byte <(vic_screen+32+2*40)
	.byte <(vic_screen+23+9*40)
	.byte <(vic_screen+32+9*40)
	.byte <(vic_screen+23+16*40)
	.byte <(vic_screen+32+16*40)
	.byte <(vic_screen+22+0*40)

screen_hi:
	.byte >(vic_screen+23+2*40)
	.byte >(vic_screen+32+2*40)
	.byte >(vic_screen+23+9*40)
	.byte >(vic_screen+32+9*40)
	.byte >(vic_screen+23+16*40)
	.byte >(vic_screen+32+16*40)
	.byte >(vic_screen+22+0*40)

d800_lo:
	.byte <($d800+23+2*40)
	.byte <($d800+32+2*40)
	.byte <($d800+23+9*40)
	.byte <($d800+32+9*40)
	.byte <($d800+23+16*40)
	.byte <($d800+32+16*40)
	.byte <($d800+22+0*40)

d800_hi:
	.byte >($d800+23+2*40)
	.byte >($d800+32+2*40)
	.byte >($d800+23+9*40)
	.byte >($d800+32+9*40)
	.byte >($d800+23+16*40)
	.byte >($d800+32+16*40)
	.byte >($d800+22+0*40)
.endproc

; X = destination column 0..38
; Y = destination row 0..23
.export clearIcon
.proc clearIcon
		jsr setupDestination

		lda #$ff
		ldy #15
		:
			sta (DST),y
			dey
		bpl :-
		clc
		lda DST+0
		adc #<320
		sta DST+0
		lda DST+1
		adc #>320
		sta DST+1
		lda #$ff
		ldy #6
		:
			sta (DST),y
			dey
		bpl :-
		ldy #14
		:
			sta (DST),y
			dey
			cpy #7
		bne :-

		lda #$f
		ldy #0
		sta (DST3),y
		iny
		sta (DST3),y
		ldy #40
		sta (DST3),y
		iny
		sta (DST3),y

		rts
.endproc

; ICONINDEX = icon index 0..88
; X = destination column 0..38
; Y = destination row 0..23
; ARGS+0 = disabled
; c = magical
.export drawIcon
.proc drawIcon
		php

		jsr setupDestination

		; Disabled mask
		ldx #$ff
		ldy #$00
		lda ARGS+0
		beq :+
			ldx #%00110011
			ldy #%11111111
		:
		stx TMP+0
		sty TMP+1

		; Source
		ldx ICONINDEX
		lda icons_bitmap_lo,x
		sta SRC+0
		lda icons_bitmap_hi,x
		sta SRC+1
		lda icons_screen_lo,x
		sta SRC2+0
		lda icons_screen_hi,x
		sta SRC2+1

		ldy #15
		:
			lda (SRC),y
			and TMP+0
			sta (DST),y
			lda TMP+0
			eor TMP+1
			sta TMP+0
			dey
		bpl :-
		clc
		lda DST+0
		adc #<320
		sta DST+0
		lda DST+1
		adc #>320
		sta DST+1
		lda SRC+0
		adc #16
		sta SRC+0
		bcc :+
			inc SRC+1
		:

		lda TMP+0
		eor TMP+1
		sta TMP+0

		ldy #6
		:
			lda (SRC),y
			and TMP+0
			sta (DST),y
			lda TMP+0
			eor TMP+1
			sta TMP+0
			dey
		bpl :-

		lda TMP+0
		eor TMP+1
		sta TMP+0

		ldy #14
		:
			lda (SRC),y
			and TMP+0
			sta (DST),y
			lda TMP+0
			eor TMP+1
			sta TMP+0
			dey
			cpy #7
		bne :-

		ldy #0
		lda (SRC2),y
		sta (DST2),y
		iny
		lda (SRC2),y
		sta (DST2),y
		iny
		lda (SRC2),y
		ldy #40
		sta (DST2),y
		ldy #3
		lda (SRC2),y
		ldy #41
		sta (DST2),y

		lda #$f
		plp
		bcc :+
			lda #$e
		:
		ldy #0
		sta (DST3),y
		iny
		sta (DST3),y
		ldy #40
		sta (DST3),y
		iny
		sta (DST3),y
		rts

		icon_bitmap:
		.repeat 4,J
			.repeat 20,I
				.incbin "resources/icons.kla",2+J*2*320+I*16,16
				.incbin "resources/icons.kla",2+J*2*320+I*16+320,16
			.endrep
		.endrep
		.repeat 9,I
			.incbin "resources/icons.kla",2+4*2*320+I*16,16
			.incbin "resources/icons.kla",2+4*2*320+I*16+320,16
		.endrep
		icons_bitmap_lo:
		.repeat 89*4,I
			.byte <(icon_bitmap+I*32)
		.endrep
		icons_bitmap_hi:
		.repeat 89*4,I
			.byte >(icon_bitmap+I*32)
		.endrep

		icon_screen:	
		.repeat 4,J
			.repeat 20,I
				.incbin "resources/icons.kla",2+8000+J*2*40+I*2,2
				.incbin "resources/icons.kla",2+8000+J*2*40+I*2+40,2
			.endrep
		.endrep
		.repeat 9,I
			.incbin "resources/icons.kla",2+8000+4*2*40+I*2,2
			.incbin "resources/icons.kla",2+8000+4*2*40+I*2+40,2
		.endrep
		icons_screen_lo:
		.repeat 89*4,I
			.byte <(icon_screen+I*4)
		.endrep
		icons_screen_hi:
		.repeat 89*4,I
			.byte >(icon_screen+I*4)
		.endrep
.endproc

.segment "GUI"
characters_gfx:		.incbin "converters/ui/characters-bitmap.bin"
			.incbin "converters/ui/characters-screen.bin"
			.incbin "converters/ui/characters-d800.bin"
character5_gfx:		.incbin "converters/ui/character5-bitmap.bin"
			.incbin "converters/ui/character5-screen.bin"
			.incbin "converters/ui/character5-d800.bin"
character6_gfx:		.incbin "converters/ui/character6-bitmap.bin"
			.incbin "converters/ui/character6-screen.bin"
			.incbin "converters/ui/character6-d800.bin"
inventory_gfx:		.incbin "converters/ui/inventory-bitmap.bin"
			.incbin "converters/ui/inventory-screen.bin"
			.incbin "converters/ui/inventory-d800.bin"

.proc renderBackdrop
		lda partyMembers_lo,x
		sta CURRENT_PARTYMEMBER+0
		lda partyMembers_hi,x
		sta CURRENT_PARTYMEMBER+1
		cpx #4
		bne :+
			ldy #PartyMember::status
			lda (CURRENT_PARTYMEMBER),y
			beq :+
				bitblt character5_gfx, 10,8, 22,14
				rts
		:
		cpx #5
		bne :+
			ldy #PartyMember::status
			lda (CURRENT_PARTYMEMBER),y
			beq :+
				bitblt character6_gfx, 9,8, 31,14
				rts
		:

		rts
.endproc

.segment "GUI2"
.export guiInit
.proc guiInit
		lda #0
		sta transparentText
		lda #<vic_bitmap
		sta BB_DST+0
		lda #>vic_bitmap
		sta BB_DST+1
		ldy #<camp_gfx
		ldx #>camp_gfx
		jsr decrunchTo
		memcpy_pureram camp_spr, vic_bitmap, 3*64

		lda #0
		sta ASK_FOR_SPELL_RECEIVER_GUI
		lda #<-1
		sta SPELLBOOK_MEMBER_INDEX
		sta MEMBER_INVENTORY_OPEN
		sta SWAPPING_MEMBER_INDEX
		lda #1
		sta SPELLBOOK_LEVEL_INDEX
		jsrf renderControls
		jsrf showNormal
		jsrf text_init

		rts

camp_gfx:
.incbin "converters/ui/camp.prg.b2",2
.endproc

.export clearVICSprites
.proc clearVICSprites
		lda #0
		tax
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
			inx
		   	bne :-
		:
		   	sta vic_sprites+$a00,x
		   	inx
	   	bpl :-
		rts
.endproc

.segment "GUI"
.proc renderPortraitOverview
		; Check death
		ldy #PartyMember::hpCurrent
		lda (CURRENT_PARTYMEMBER),y
		cmp #<-10
		bne :+
		iny
		lda (CURRENT_PARTYMEMBER),y
		cmp #>-10
		bne :+
			; Death
			lda #0
			sta ARGS+1
			lda #53
			jmp pictureOverride
		:

		; Check unconsciousness
		ldy #PartyMember::hpCurrent+1
		lda (CURRENT_PARTYMEMBER),y
		bmi :+
		dey
		lda (CURRENT_PARTYMEMBER),y
		beq :+
		lda #0
		jmp :++
		:
			lda #1
		:
		sta ARGS+1

		; Check invisibility
		ldy #PartyMember::activeSpells+0
		lda (CURRENT_PARTYMEMBER),y
		and #SPELL_INVISIBLE
		beq :+
			lda #2
			sta ARGS+1
		:

		ldy #PartyMember::picture
		lda (CURRENT_PARTYMEMBER),y
		pictureOverride:sta ARGS+0
		ldx XPOS
		ldy YPOS
		iny
		jsrf showPortrait

		ldy #PartyMember::damageInfo+0
		lda (CURRENT_PARTYMEMBER),y
		sta TMP2+0
		iny
		lda (CURRENT_PARTYMEMBER),y
		sta TMP2+1
		ora TMP2+0
		bne :+
			rts
		:
		lda TMP2+0
		pha

		ldx XPOS
		ldy YPOS
		iny
		iny
		jsrf setupDestination
		bitblt2 redGfx,4,2
		sec
		lda DST+0
		sbc #<320*2
		sta DST+0
		lda DST+1
		sbc #>320*2
		sta DST+1
		pla
		sta ARGS+0
		jsrf drawActionValue
		rts
redGfx:		.incbin "converters/ui/red-bitmap.bin"
		.incbin "converters/ui/red-screen.bin"
		.incbin "converters/ui/red-d800.bin"
.endproc

; y = handIndex
.proc renderHandOverview
		.pushseg
		.segment "BSS"
			handIndex: .res 1
			hitInfo: .res 2
		.popseg

		sty handIndex

		tya
		asl
		pha
		clc
		adc #PartyMember::inventory
		tay
		lda (CURRENT_PARTYMEMBER),y
		sta CUR_ITEM+0
		iny
		lda (CURRENT_PARTYMEMBER),y
		sta CUR_ITEM+1

		pla
		clc
		adc #PartyMember::hitInfo
		tay
		lda (CURRENT_PARTYMEMBER),y
		sta hitInfo+0
		iny
		lda (CURRENT_PARTYMEMBER),y
		sta hitInfo+1

		clc
		lda YPOS
		ldx handIndex
		adc handy,x
		tay
		lda XPOS
		adc #4
		tax

		lda hitInfo+1
		bpl :+
			lda hitInfo+0
			cmp #<ATTACK_MISS
			beq miss
			cmp #<ATTACK_HACK
			beq hack
			cmp #<ATTACK_CANTREACH
			beq cantReach
			jmp noAmmo
		:
		beq :+
			jmp hit
		:
		lda hitInfo+0
		beq :+
			jmp hit
		:
		jmp normal

miss:		bitblt2 missGfx,4,2
		rts

hack:		bitblt2 hackGfx,4,2
		rts

cantReach:	bitblt2 cantReachGfx,4,2
		rts

noAmmo:		bitblt2 noAmmoGfx,4,2
		rts

hit:		bitblt2 greenGfx,4,2
		sec
		lda DST+0
		sbc #<320*2
		sta DST+0
		lda DST+1
		sbc #>320*2
		sta DST+1
		lda hitInfo+0
		sta ARGS+0
		jsrf drawActionValue
		rts

normal:		bitblt2 defaultGfx,4,2

		; Check disabled
		;if ((member.disabledHands&(1<<handIndex)) != 0)
		ldy #PartyMember::disabledHands
		lda (CURRENT_PARTYMEMBER),y
		ldx handIndex
		and bits,x
		bne :+
		; || (!GameState.mayUseItemInHand(memberIndex, handIndex))
		jsrf gameState_mayUseItemInHand
		cpx #0
		beq :+
		; || (member.hpCurrent<=0)
		ldy #PartyMember::hpCurrent+1
		lda (CURRENT_PARTYMEMBER),y
		bmi :+
		dey
		lda (CURRENT_PARTYMEMBER),y
		beq :+
		; || ((member.status&PartyMember.STATUS_PARALYZED)!=0)
		ldy #PartyMember::status
		lda (CURRENT_PARTYMEMBER),y
		and #PARTYMEMBER_STATUS_PARALYZED
		bne :+
		lda #0
		jmp :++
		:
			lda #1
		:
		sta ARGS+0

		ldx #85
		lda CUR_ITEM+0
		ora CUR_ITEM+1
		beq :+
			ldy #Item::picture
			jsr lax_CUR_ITEM_y
		:
		stx ICONINDEX

		clc
		lda partySpells+0
		and #SPELL_DETECT_MAGIC
		beq :+
			ldy #Item::flags
			jsr lda_CUR_ITEM_y
			and #ITEM_FLAGS_MAGICAL
			beq :+
				sec
		:
		php

		clc
		lda YPOS
		ldx handIndex
		adc handy,x
		tay
		lda XPOS
		adc #5
		tax
		plp
		jsrf drawIcon

		rts
bits:		.byte 1,2
handy:		.byte 1,3

missGfx:	.incbin "converters/ui/miss-bitmap.bin"
		.incbin "converters/ui/miss-screen.bin"
		.incbin "converters/ui/miss-d800.bin"
hackGfx:	.incbin "converters/ui/hack-bitmap.bin"
		.incbin "converters/ui/hack-screen.bin"
		.incbin "converters/ui/hack-d800.bin"
cantReachGfx:	.incbin "converters/ui/cantreach-bitmap.bin"
		.incbin "converters/ui/cantreach-screen.bin"
		.incbin "converters/ui/cantreach-d800.bin"
noAmmoGfx:	.incbin "converters/ui/noammo-bitmap.bin"
		.incbin "converters/ui/noammo-screen.bin"
		.incbin "converters/ui/noammo-d800.bin"
greenGfx:	.incbin "converters/ui/green-bitmap.bin"
		.incbin "converters/ui/green-screen.bin"
		.incbin "converters/ui/green-d800.bin"
defaultGfx:	.incbin "converters/ui/default-bitmap.bin"
		.incbin "converters/ui/default-screen.bin"
		.incbin "converters/ui/default-d800.bin"
.endproc

.segment "GUI2"

.proc drawActionValue
		.pushseg
		.segment "BSS"
			digitsOn: .byte 0
		.popseg

		jsrf htd

		lda #0
		sta digitsOn

		lda TMP+1
		and #$0f
		beq :+
			inc digitsOn
			jsr renderChar
		:
		jsr forwardChar
 		lda TMP+0
 		lsr
		lsr
		lsr
		lsr
		tax
		ora digitsOn
		php
		txa
		plp
		beq :+
			inc digitsOn
			jsr renderChar
		:
		jsr forwardChar
 		lda TMP+0
 		and #$0f
 		jsr renderChar
		rts

renderChar:	asl
		asl
		asl
		clc
		adc #<digitsGfx
		sta SRC+0
		lda #>digitsGfx
		adc #0
		sta SRC+1
		clc
		lda DST+0
		adc #3
		sta DST+0
		ldy #0
		:
			lda (SRC),y
			sta (DST),y
			iny
			cpy #5
		bne :-
		clc
		lda DST+0
		adc #<(320-8)
		sta DST+0
		lda DST+1
		adc #>(320-8)
		sta DST+1

		ldy #5
		:
			lda (SRC),y
			sta (DST),y
			iny
			cpy #8
		bne :-

		sec
		lda DST+0
		sbc #<(320-5)
		sta DST+0
		lda DST+1
		sbc #>(320-5)
		sta DST+1

		rts

forwardChar:	clc
		lda DST+0
		adc #8
		sta DST+0
		lda DST+1
		adc #0
		sta DST+1
		rts

digitsGfx:	.incbin "converters/ui/digits-bitmap.bin"
.endproc

.segment "GUI"
_setupDestination:
		jsrf setupDestination
		rts

.proc renderPartyMemberOverview
		stx XPOS
		sty YPOS
		tax
		sta CURRENT_PARTYMEMBER_INDEX
		lda #0
		sta partyMemberStatsChanged,x
		lda partyMembers_lo,x
		sta CURRENT_PARTYMEMBER+0
		lda partyMembers_hi,x
		sta CURRENT_PARTYMEMBER+1
		cpx #4
		bcc :+
			ldy #PartyMember::status
			lda (CURRENT_PARTYMEMBER),y
			bne :+
				rts
		:

		ldx XPOS
		ldy YPOS
		jsr _setupDestination
		clc
		lda #PartyMember::name
		adc CURRENT_PARTYMEMBER+0
		sta SRC+0
		lda #0
		adc CURRENT_PARTYMEMBER+1
		sta SRC+1
		lda #$0
		sta textColor
		lda #$fc
		sta textBackground
		sta textBackgroundAlt

		ldy #PartyMember::status
		lda (CURRENT_PARTYMEMBER),y
		and #PARTYMEMBER_STATUS_POISONED
		beq notPoisoned
			lda #$5
			sta textColor
		notPoisoned:

		lda #0
		sta TMP

		ldy #PartyMember::activeSpells+0
		lda (CURRENT_PARTYMEMBER),y
		and #SPELL_SHIELD
		beq :+
			lda TMP
			ora #1
			sta TMP
		:
		lda (CURRENT_PARTYMEMBER),y
		and #SPELL_HASTE|SPELL_AID|SPELL_SLOW_POISON
		beq :+
			lda TMP
			ora #2
			sta TMP
		:
		iny
		lda (CURRENT_PARTYMEMBER),y
		and #>(SPELL_PROTECTION_FROM_EVIL|SPELL_MAGICAL_VESTMENT)
		beq :+
			lda TMP
			ora #1
			sta TMP
		:
		lda (CURRENT_PARTYMEMBER),y
		and #>SPELL_UNKNOWN
		beq :+
			lda TMP
			ora #2
			sta TMP
		:

		lda TMP
		cmp #1
		bne :+
			ldx #$ac
			stx textBackground
			stx textBackgroundAlt
		:
		cmp #2
		bne :+
			ldx #$7c
			stx textBackground
			stx textBackgroundAlt
		:
		cmp #3
		bne :+
			ldx #$ac
			stx textBackground
			ldx #$7c
			stx textBackgroundAlt			
		:


		lda swappingFlipFlop
		beq :+
			lda CURRENT_PARTYMEMBER_INDEX
			cmp SWAPPING_MEMBER_INDEX
			bne :+
				lda #2
				sta textColor
				lda #<swappingTxt
				sta SRC+0
				lda #>swappingTxt
				sta SRC+1
		:
		jsr drawName

		jsr renderPortraitOverview

		ldy #INVENTORY_LEFT_HAND
		jsr renderHandOverview
		
		ldy #INVENTORY_RIGHT_HAND
		jsr renderHandOverview

		jsrf renderHealthBar
		rts

swappingTxt: .byte "SWAPPING"
.endproc

.proc showNormal
		lda #<-1
		sta MEMBER_INVENTORY_OPEN
		lda #0
		sta showingStats

		bitblt characters_gfx, 18,22, 22,0

		lda #0
		ldx #23
		ldy #1
		jsr renderPartyMemberOverview

		lda #1
		ldx #32
		ldy #1
		jsr renderPartyMemberOverview

		lda #2
		ldx #23
		ldy #8
		jsr renderPartyMemberOverview

		lda #3
		ldx #32
		ldy #8
		jsr renderPartyMemberOverview

		ldx #4
		jsr renderBackdrop
		lda #4
		ldx #23
		ldy #15
		jsr renderPartyMemberOverview

		ldx #5
		jsr renderBackdrop
		lda #5
		ldx #32
		ldy #15
		jsr renderPartyMemberOverview

		rts
.endproc

.export refreshCompass
.proc refreshCompass
		lda shouldUpdateCompass
		beq :+
		lda SPELLBOOK_MEMBER_INDEX
		bmi :++
		:
		rts
		:
		jsrf renderCompass
		rts
.endproc

.export refreshUI
.proc refreshUI
	ldy MEMBER_INVENTORY_OPEN
	bmi :++
		lda #0
		sta partyCompositionChanged
		lda partyMemberStatsChanged,y
		beq :+
			jsr renderInventoryForPartyMember
		:
		rts
	:

	lda partyCompositionChanged
	beq :+
		lda #0
		sta partyCompositionChanged
		jmp showNormal
	:

	lda partyMemberStatsChanged+0
	beq :+
		lda #0
		ldx #23
		ldy #1
		jsr renderPartyMemberOverview
	:
	lda partyMemberStatsChanged+1
	beq :+
		lda #1
		ldx #32
		ldy #1
		jsr renderPartyMemberOverview
	:
	lda partyMemberStatsChanged+2
	beq :+	
		lda #2
		ldx #23
		ldy #8
		jsr renderPartyMemberOverview
	:
	lda partyMemberStatsChanged+3
	beq :+
		lda #3
		ldx #32
		ldy #8
		jsr renderPartyMemberOverview
	:
	lda partyMemberStatsChanged+4
	beq :+
		lda #4
		ldx #23
		ldy #15
		jsr renderPartyMemberOverview
	:
	lda partyMemberStatsChanged+5
	beq :+
		lda #5
		ldx #32
		ldy #15
		jsr renderPartyMemberOverview
	:

	rts
.endproc

.proc renderInventoryForPartyMember
		lda #0
		ldy MEMBER_INVENTORY_OPEN
		sta partyMemberStatsChanged,y
		lda partyMembers_lo,y
		sta CURRENT_PARTYMEMBER+0
		lda partyMembers_hi,y
		sta CURRENT_PARTYMEMBER+1
		ldy #PartyMember::picture
		lda (CURRENT_PARTYMEMBER),y
		sta ARGS+0
		lda #0
		sta ARGS+1
		ldx #22
		ldy #0
		jsrf showPortrait

		ldx #27
		ldy #0
		jsr _setupDestination
		clc
		lda #PartyMember::name
		adc CURRENT_PARTYMEMBER+0
		sta SRC+0
		lda #0
		adc CURRENT_PARTYMEMBER+1
		sta SRC+1
		lda #$0
		sta textColor
		lda #$fc
		sta textBackground
		sta textBackgroundAlt
		jsr drawName

		jsrf renderBigBars

		lda showingStats
		beq :+
			jsrf renderStatsForPartyMember
			rts
		:

		ldx #INVENTORY_RIGHT_RING
		loop:
			txa
			pha

			; Skip Quiver
			ldy item_x,x
			bmi prevSlot

			asl
			clc
			adc #PartyMember::inventory
			tay
			lda (CURRENT_PARTYMEMBER),y
			sta CUR_ITEM+0
			iny
			lda (CURRENT_PARTYMEMBER),y
			sta CUR_ITEM+1
			ora CUR_ITEM+0
			beq clear
				clc
				lda partySpells+0
				and #SPELL_DETECT_MAGIC
				beq :+
					ldy #Item::flags
					jsr lda_CUR_ITEM_y
					and #ITEM_FLAGS_MAGICAL
					beq :+
						sec
				:
				php
				ldy #Item::picture
				jsr lda_CUR_ITEM_y
				sta ICONINDEX
				lda item_x,x
				ldy item_y,x
				tax
				lda #0
				sta ARGS+0
				plp
				jsrf drawIcon
				jmp prevSlot
			clear:
				lda item_x,x
				ldy item_y,x
				tax
				jsrf clearIcon
			prevSlot:
			pla
			tax
			dex
			bmi :+
		jmp loop
		:

		; Quiver
		ldy #PartyMember::inventory + INVENTORY_QUIVER*2
		lda (CURRENT_PARTYMEMBER),y
		sta TOP_ITEM+0
		iny
		lda (CURRENT_PARTYMEMBER),y
		sta TOP_ITEM+1
		lda #<-1
		sta ARGS+2
		jsrf items_countItems

		; Render quiver count
		ldx #29
		ldy #6
		jsr _setupDestination
		inc DST+0

		lda #1
		sta textColor
		lda #0
		sta textBackground
		jsr drawValue

		rts

item_x:		.byte 29,37,23,26,23,26,23,26,23,26,23,26,23,26,23,26,<-1,29,29,37,37,33,37,37,37,29,29 
item_y:		.byte 19,19, 5, 5, 7, 7, 9, 9,11,11,13,13,15,15,17,17,<-1, 8,11, 5, 8,19,11,13,15,14,16
.endproc

.proc drawValue
		jsrf htd

 		lda TMP+0
 		lsr
		lsr
		lsr
		lsr
		beq :+
			clc
			adc #'0'
			jmp :++
		:
			lda #' '
		:
		jsr render3x6Char
 		lda TMP+0
 		and #$0f
		clc
		adc #'0'
 		jsr render3x6Char
		rts
.endproc

.proc source3x6Char
		and #$7f
		tay
		lda asciiToFontIndex,y

		ldy #0
		sty SRC2+1
		asl
		rol SRC2+1
		asl
		rol SRC2+1
		asl
		rol SRC2+1
		clc
		adc #<fontGfx
		sta SRC2+0
		lda #>fontGfx
		adc SRC2+1
		sta SRC2+1
		rts

asciiToFontIndex:
		.byte $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b
		.byte $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1b
		.byte $1b, $1b, $1b, $1b, $1b, $1b, $1b, $1a, $1b, $1b, $1c, $1d, $1e, $1f, $20, $21
		.byte $22, $23, $24, $25, $26, $27, $28, $29, $2a, $2b, $2c, $2d, $00, $2e, $00, $00
		.byte $00, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0a, $0b, $0c, $0d, $0e
		.byte $0f, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $00, $00, $00, $00, $00
		.byte $00, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0a, $0b, $0c, $0d, $0e
		.byte $0f, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $00, $00, $00, $00, $00
.endproc

.proc render3x6Char_ext
		txa
.endproc

.proc render3x6Char
		jsr source3x6Char

		lda transparentText
		beq :+
			jsr transparent
			jmp forwardPlain
		:

		ldy #5
		:
			lda (SRC2),y
			ora #$55
			sta (DST),y
			dey
		bpl :-

forward:
		ldy #0
		lda textBackground
		sta (DST2),y
		inc DST2+0
		bne :+
			inc DST2+1
		:

forwardPlain:
		ldy #0
		lda textColor
		sta (DST3),y
		inc DST3+0
		bne :+
			inc DST3+1
		:

		clc
		lda DST+0
		adc #8
		sta DST+0
		lda DST+1
		adc #0
		sta DST+1

		rts

.pushseg
.segment "MEMCODE_RW"
transparent:
		ldy #5
		:
			ldx #$35
			stx 1
			lda (DST),y
			ldx #$37
			stx 1
			ora (SRC2),y
			sta (DST),y
			dey
		bpl :-
		rts
.popseg
.endproc

.proc render3x6CharPlain
		jsr source3x6Char
		jsr draw
		jmp render3x6Char::forwardPlain
.pushseg
.segment "START"
draw:
		lda #$34
		sta 1
		ldy #5
		:
			lda (DST),y
			dec 1
			ora (SRC2),y
			inc 1
			sta (DST),y
			dey
		bpl :-
		lda #$37
		sta 1
		rts
.popseg
.endproc

; a = char
; x = x-offset
; y = row
.proc render3x6CharAtRow
		sty YREG
		jsr source3x6Char
		ldy YREG
		lda rows_lo,y
		sta TMP2+0
		lda rows_hi,y
		sta TMP2+1
		ldy #0
		jmp (TMP2)
row0:		.repeat 6,I
		.scope
			ypos = 0*7+I
			lda (SRC2),y
			eor TMP
			sta vic_bitmap+9*8+(16+ypos/8)*320+(ypos&7),x
			iny
		.endscope
		.endrep
		jmp forward
row1:		.repeat 6,I
		.scope
			ypos = 1*7+I
			lda (SRC2),y
			eor TMP
			sta vic_bitmap+9*8+(16+ypos/8)*320+(ypos&7),x
			iny
		.endscope
		.endrep
		jmp forward
row2:		.repeat 6,I
		.scope
			ypos = 2*7+I
			lda (SRC2),y
			eor TMP
			sta vic_bitmap+9*8+(16+ypos/8)*320+(ypos&7),x
			iny
		.endscope
		.endrep
		jmp forward
row3:		.repeat 6,I
		.scope
			ypos = 3*7+I
			lda (SRC2),y
			eor TMP
			sta vic_bitmap+9*8+(16+ypos/8)*320+(ypos&7),x
			iny
		.endscope
		.endrep
		jmp forward
row4:		.repeat 6,I
		.scope
			ypos = 4*7+I
			lda (SRC2),y
			eor TMP
			sta vic_bitmap+9*8+(16+ypos/8)*320+(ypos&7),x
			iny
		.endscope
		.endrep
		jmp forward
row5:		.repeat 6,I
		.scope
			ypos = 5*7+I
			lda (SRC2),y
			eor TMP
			sta vic_bitmap+9*8+(16+ypos/8)*320+(ypos&7),x
			iny
		.endscope
		.endrep
forward:	ldy YREG
		clc
		txa
		adc #8
		tax
		rts
rows_lo:	.byte <row0,<row1,<row2,<row3,<row4,<row5
rows_hi:	.byte >row0,>row1,>row2,>row3,>row4,>row5
.endproc

fontGfx:	.incbin "converters/ui/font-bitmap.bin"

.export drawName
.proc drawName
		ldy #0
		:
			tya
			pha
			lda (SRC),y
			jsr render3x6Char

			ldx textBackground
			ldy textBackgroundAlt
			stx textBackgroundAlt
			sty textBackground

			pla
			tay
			iny
			cpy #8
		bne :-
		rts
.endproc

.export drawNamePlain
.proc drawNamePlain
		ldy #0
		:
			tya
			pha
			lda (SRC),y
			beq :+
			jsr render3x6CharPlain
			pla
			tay
			iny
			cpy #8
		bne :-
		rts
		:
		pla
		rts
.endproc

; a=0 => cleric, a!=0 => mage
; x = spellindex
; y = row
.proc drawSpellName
		sty YREG
		cmp #0
		beq :+
			jsrf getCLERICscroll
			jmp :++
		:
			jsrf getMAGEscroll
		:
		ldx #0
		ldy #0
loop:		sty TMP+1
		lda ARGS,y
		ldy YREG
		jsr render3x6CharAtRow
		ldy TMP+1
		iny
		cpy #13
		bne loop
		rts
.endproc

.if 0
.proc renderSpace
		lda #$55
		ldy #5
		:
			sta (DST),y
			dey
		bpl :-
		jmp render3x6Char::forward
.endproc 
.endif

; x = member index
.proc showInventory
		lda partyMembers_lo,x
		sta CURRENT_PARTYMEMBER+0
		lda partyMembers_hi,x
		sta CURRENT_PARTYMEMBER+1

		ldy #PartyMember::status
		lda (CURRENT_PARTYMEMBER),y
		beq :++
			lda MEMBER_INVENTORY_OPEN
			bpl :+
				stx MEMBER_INVENTORY_OPEN
				bitblt inventory_gfx, 18,22, 22,0
				jmp renderInventoryForPartyMember
			:
			stx MEMBER_INVENTORY_OPEN
			jmp renderInventoryForPartyMember
		:
		ldx CURRENT_PARTYMEMBER_INDEX
		lda partyMembers_lo,x
		sta CURRENT_PARTYMEMBER+0
		lda partyMembers_hi,x
		sta CURRENT_PARTYMEMBER+1
		rts
.endproc

.pushseg
.segment "BSS"
	spellBookMemberRow: .res 6
.popseg

; y = member index
; x = level
.proc renderSpellBook
		.pushseg
		.segment "BSS"
		currentRow: .res 1
		memberRow: .res 1
		.popseg

		bitblt spellbook_gfx, 14,7, 8,15

		ldx SPELLBOOK_LEVEL_INDEX
		dex
		ldy levelHighLight,x
		lda #<(vic_screen+9+15*40)
		sta TMP+0
		lda #>(vic_screen+9+15*40)
		sta TMP+1
		lda #(13<<4)|12
		sta (TMP),y
		iny
		sta (TMP),y

		ldx SPELLBOOK_MEMBER_INDEX
		lda partyMembers_lo,x
		sta CURRENT_PARTYMEMBER+0
		lda partyMembers_hi,x
		sta CURRENT_PARTYMEMBER+1

		lda #PartyMember::mageSpells
		ldx SPELLBOOK_TYPE
		beq :+
			lda #PartyMember::clericSpells
		:
		clc
		ldx SPELLBOOK_LEVEL_INDEX
		dex
		adc mul6,x
		clc
		adc CURRENT_PARTYMEMBER+0
		sta CURRENT_PARTYMEMBER+0
		bcc :+
			inc CURRENT_PARTYMEMBER+1
		:

		ldx #5
		lda #<-1
		:
			sta spellBookMemberRow,x
			dex
		bpl :-

		ldy #0
		sty currentRow
		nextRow:
			sty memberRow
			ldy currentRow
			lda #$ff
			cpy SPELLBOOK_SELECTED_ROW
			bne :+
				eor #$aa
			:
			sta TMP

			ldy memberRow
			lax (CURRENT_PARTYMEMBER),y
			bmi :+
			beq :+
				tya
				ldy currentRow
				sta spellBookMemberRow,y
				lda SPELLBOOK_TYPE
				jsr drawSpellName
				inc currentRow
			:

			ldy memberRow
			iny
			cpy #6
		bne nextRow
		
		rts
levelHighLight:	.byte 0,3,5,8,11
mul6:		.byte 0,6,12,18,24

spellbook_gfx:
			.incbin "converters/ui/spellbook-bitmap.bin"
			.incbin "converters/ui/spellbook-screen.bin"
			.incbin "converters/ui/spellbook-d800.bin"
.endproc

.segment "GUI4"
.proc renderControls
		bitblt controls_gfx, 22,7, 0,15
		lda #1
		sta shouldUpdateCompass
		rts

controls_gfx:
		.incbin "converters/ui/controls-bitmap.bin"
		.incbin "converters/ui/controls-screen.bin"
		.incbin "converters/ui/controls-d800.bin"
.endproc
.export gui_renderControls = renderControls

.proc renderStatsForPartyMember
	jsrf showStatsGfx

	lda #$01
	sta textColor
	inc transparentText

	ldy #PartyMember::alignment
	lax (CURRENT_PARTYMEMBER),y
	lda alignmentTxt_lo,x
	sta SRC+0
	lda alignmentTxt_hi,x
	sta SRC+1
	ldx #23
	ldy #5
	jsr drawString

	ldy #PartyMember::race
	lax (CURRENT_PARTYMEMBER),y
	lda raceTxt_lo,x
	sta SRC+0
	lda raceTxt_hi,x
	sta SRC+1
	ldx #23
	ldy #6
	jsr drawString

	ldx #0
	:
		lda stringsLo,x
		sta SRC+0
		lda stringsHi,x
		sta SRC+1
		ora SRC+0
		beq :+
		txa
		pha
		ldy stringsYpos,x
		lda stringsXpos,x
		tax
		jsr drawString
		pla
		tax
		inx
	jmp :-
	:

	ldy #PartyMember::class
	lax (CURRENT_PARTYMEMBER),y
	lda classTxt_lo,x
	sta SRC+0
	lda classTxt_hi,x
	sta SRC+1
	ldx #23
	ldy #17
	jsr drawString

	lda #$03
	sta textColor

	ldx #0
	:
		stx TMP3

		clc
		txa
		adc #8
		tay
		ldx #37
		jsr setDest

		ldx TMP3
		ldy statOffset,x
		lda (CURRENT_PARTYMEMBER),y
		jsr drawNumber

		ldx TMP3
		inx
		cpx #8
	bne :-

	.repeat 3,I
		ldx #38
		ldy #17+I
		jsr setDest
		ldy #PartyMember::level+I
		lda (CURRENT_PARTYMEMBER),y
		beq :+
			jsr drawNumber
			ldx #31
			ldy #17+I
			jsr setDest
			ldy #PartyMember::experience+4*I
			jsr drawExperience
		:
	.endrep

	dec transparentText
	rts

drawExperience:
	ldx #0
	:
		lda (CURRENT_PARTYMEMBER),y
		sta ARGS+0,x
		iny
		inx
		cpx #4
	bne :-
	lda ARGS+0
	ora ARGS+1
	ora ARGS+2
	ora ARGS+3
	bne :+
		rts
	:
	jsrf htd32

	ldx #0
	stx TMP3
	:
		txa
		pha
		ldy RESULT+4,x
		beq :+
			inc TMP3
		:
		ldx digits,y
		lda TMP3
		beq :+
			jsr drawChar
		:
		pla
		tax
		inx
		cpx #6
	bne :---
	rts

statOffset:
	.byte PartyMember::hp
	.byte PartyMember::strengthCurrent
	.byte PartyMember::intelligenceCurrent
	.byte PartyMember::wisdomCurrent
	.byte PartyMember::dexterityCurrent
	.byte PartyMember::constitutionCurrent
	.byte PartyMember::charismaCurrent
	.byte PartyMember::ac

stringsLo:
	.byte <s0,<s1,<s2,<s3,<s4,<s5,<s6,<s7,<s8,0
stringsHi:
	.byte >s0,>s1,>s2,>s3,>s4,>s5,>s6,>s7,>s8,0
stringsXpos:
	.byte  23, 23, 23, 23, 23, 23, 23, 23, 31
stringsYpos:
	.byte  8,  9, 10, 11, 12, 13, 14, 15, 16

setDest:
	jsrf setupDestination
	rts

drawChar:
	jsrf render3x6Char_ext
	rts

drawString:
	stx XPOS
	sty YPOS
	jsr setDest
	ldy #0
	charLoop:
		tya
		pha
		lda (SRC),y
		bne :+
			pla
			rts
		:
		cmp #13
		bne :+
			ldx XPOS
			ldy YPOS
			iny
			sty YPOS
			jsr setDest
			jmp skipChar
		:
		
		tax
		jsr drawChar
		
		skipChar:
		pla
		tay
		iny
	jmp charLoop

drawNumber:
	sta ARGS+0
	bpl :+
		eor #$ff
		clc
		adc #1
		sta ARGS+0
		ldx #'-'
		jsr drawChar
	:
	jsrf htd

	lda TMP+0
	pha

	; 100s
	lda TMP+1
	and #$0f
	beq :+
		tay
		ldx digits,y
		jsr drawChar
	:

	; 10s
	pla
	pha
	lsr
	lsr
	lsr
	lsr
	beq :+
		tay
		ldx digits,y
		jsr drawChar
	:

	; 1s
	pla
	and #$0f
	tay
	ldx digits,y
	jsr drawChar

	rts
digits:		.byte "0123456789"

s0:		.byte "HEALTH POINTS",0
s1:		.byte "STRENGTH",0
s2:		.byte "INTELLIGENCE",0
s3:		.byte "WISDOM",0
s4:		.byte "DEXTERITY",0
s5:		.byte "CONSTITUTION",0
s6:		.byte "CHARISMA",0
s7:		.byte "ARMOR CLASS",0
s8:		.byte "EXP	L",0

alignmentTxt_lo: .byte <a0,<a1,<a2,<a3,<a4,<a5,<a6,<a7,<a8
alignmentTxt_hi: .byte >a0,>a1,>a2,>a3,>a4,>a5,>a6,>a7,>a8
a0: 	.byte "LAWFUL GOOD",0
a1: 	.byte "NEUTRAL GOOD",0
a2: 	.byte "CHAOTIC GOOD",0
a3: 	.byte "LAWFUL NEUTRAL",0
a4: 	.byte "TRUE NEUTRAL",0
a5: 	.byte "CHAOTIC NEUTRAL",0
a6: 	.byte "LAWFUL EVIL",0
a7: 	.byte "NEUTRAL EVIL",0
a8: 	.byte "CHAOTIC EVIL",0

raceTxt_lo: .byte <r0,<r1,<r2,<r3,<r4,<r5,<r6,<r7,<r8,<r9,<r10,<r11
raceTxt_hi: .byte >r0,>r1,>r2,>r3,>r4,>r5,>r6,>r7,>r8,>r9,>r10,>r11
r0: 	.byte "HUMAN MALE",0
r1: 	.byte "HUMAN FEMALE",0
r2: 	.byte "ELF MALE",0
r3: 	.byte "ELF FEMALE",0
r4: 	.byte "HALF-ELF MALE",0
r5: 	.byte "HALF-ELF FEMALE",0
r6: 	.byte "DWARF MALE",0
r7: 	.byte "DWARF FEMALE",0
r8: 	.byte "GNOME MALE",0
r9: 	.byte "GNOME FEMALE",0
r10: 	.byte "HALFLING MALE",0
r11: 	.byte "HALFLING FEMALE",0

classTxt_lo: .byte <c0,<c1,<c2,<c3,<c4,<c5,<c6,<c7,<c8,<c9,<c10,<c11,<c12,<c13,<c14
classTxt_hi: .byte >c0,>c1,>c2,>c3,>c4,>c5,>c6,>c7,>c8,>c9,>c10,>c11,>c12,>c13,>c14
c0: 	.byte "FIGHTER",0
c1: 	.byte "RANGER",0
c2: 	.byte "PALADIN",0
c3: 	.byte "MAGE",0
c4: 	.byte "CLERIC",0
c5: 	.byte "THIEF",0
c6: 	.byte "FIGHTER",13,"CLERIC",0
c7: 	.byte "FIGHTER",13,"THIEF",0
c8: 	.byte "FIGHTER",13,"MAGE",0
c9: 	.byte "FIGHTER",13,"MAGE",13,"THIEF",0
c10: 	.byte "THIEF",13,"MAGE",0
c11: 	.byte "CLERIC",13,"THIEF",0
c12: 	.byte "FIGHTER",13,"CLERIC",13,"MAGE",0
c13: 	.byte "RANGER",13,"CLERIC",0
c14: 	.byte "CLERIC",13,"MAGE",0
.endproc

.segment "GUI"
.proc renderCompass
		lda #0
		sta shouldUpdateCompass

		ldx partyDirection
		lda left_bitmap_lo,x
		sta SRC+0
		lda left_bitmap_hi,x
		sta SRC+1
		lda left_screen_lo,x
		sta SRC2+0
		lda left_screen_hi,x
		sta SRC2+1
		lda left_d800_lo,x
		sta SRC3+0
		lda left_d800_hi,x
		sta SRC3+1
		lda #(2*8)
		sta TMP2+0
		lda #(2)
		sta TMP2+1
		ldx #10
		ldy #20
		lda #1-1
		jsr _bitblt

		ldx partyDirection
		lda center_bitmap_lo,x
		sta SRC+0
		lda center_bitmap_hi,x
		sta SRC+1
		lda center_screen_lo,x
		sta SRC2+0
		lda center_screen_hi,x
		sta SRC2+1
		lda center_d800_lo,x
		sta SRC3+0
		lda center_d800_hi,x
		sta SRC3+1
		lda #(3*8)
		sta TMP2+0
		lda #(3)
		sta TMP2+1
		ldx #14
		ldy #16
		lda #2-1
		jsr _bitblt

		ldx partyDirection
		lda right_bitmap_lo,x
		sta SRC+0
		lda right_bitmap_hi,x
		sta SRC+1
		lda right_screen_lo,x
		sta SRC2+0
		lda right_screen_hi,x
		sta SRC2+1
		lda right_d800_lo,x
		sta SRC3+0
		lda right_d800_hi,x
		sta SRC3+1
		lda #(2*8)
		sta TMP2+0
		lda #(2)
		sta TMP2+1
		ldx #19
		ldy #20
		lda #1-1
		jsr _bitblt

		rts

left_bitmap_lo:	.byte <north_left_gfx
		.byte <east_left_gfx
		.byte <south_left_gfx
		.byte <west_left_gfx
left_bitmap_hi:	.byte >north_left_gfx
		.byte >east_left_gfx
		.byte >south_left_gfx
		.byte >west_left_gfx
left_screen_lo:	.byte <(north_left_gfx+2*1*8)
		.byte <(east_left_gfx+2*1*8)
		.byte <(south_left_gfx+2*1*8)
		.byte <(west_left_gfx+2*1*8)
left_screen_hi:	.byte >(north_left_gfx+2*1*8)
		.byte >(east_left_gfx+2*1*8)
		.byte >(south_left_gfx+2*1*8)
		.byte >(west_left_gfx+2*1*8)
left_d800_lo:	.byte <(north_left_gfx+2*1*8+2*1)
		.byte <(east_left_gfx+2*1*8+2*1)
		.byte <(south_left_gfx+2*1*8+2*1)
		.byte <(west_left_gfx+2*1*8+2*1)
left_d800_hi:	.byte >(north_left_gfx+2*1*8+2*1)
		.byte >(east_left_gfx+2*1*8+2*1)
		.byte >(south_left_gfx+2*1*8+2*1)
		.byte >(west_left_gfx+2*1*8+2*1)

center_bitmap_lo:.byte <north_center_gfx
		.byte <east_center_gfx
		.byte <south_center_gfx
		.byte <west_center_gfx
center_bitmap_hi:.byte >north_center_gfx
		.byte >east_center_gfx
		.byte >south_center_gfx
		.byte >west_center_gfx
center_screen_lo:.byte <(north_center_gfx+3*2*8)
		.byte <(east_center_gfx+3*2*8)
		.byte <(south_center_gfx+3*2*8)
		.byte <(west_center_gfx+3*2*8)
center_screen_hi:.byte >(north_center_gfx+3*2*8)
		.byte >(east_center_gfx+3*2*8)
		.byte >(south_center_gfx+3*2*8)
		.byte >(west_center_gfx+3*2*8)
center_d800_lo:	.byte <(north_center_gfx+3*2*8+3*2)
		.byte <(east_center_gfx+3*2*8+3*2)
		.byte <(south_center_gfx+3*2*8+3*2)
		.byte <(west_center_gfx+3*2*8+3*2)
center_d800_hi:	.byte >(north_center_gfx+3*2*8+3*2)
		.byte >(east_center_gfx+3*2*8+3*2)
		.byte >(south_center_gfx+3*2*8+3*2)
		.byte >(west_center_gfx+3*2*8+3*2)

right_bitmap_lo:.byte <north_right_gfx
		.byte <east_right_gfx
		.byte <south_right_gfx
		.byte <west_right_gfx
right_bitmap_hi:.byte >north_right_gfx
		.byte >east_right_gfx
		.byte >south_right_gfx
		.byte >west_right_gfx
right_screen_lo:.byte <(north_right_gfx+2*1*8)
		.byte <(east_right_gfx+2*1*8)
		.byte <(south_right_gfx+2*1*8)
		.byte <(west_right_gfx+2*1*8)
right_screen_hi:.byte >(north_right_gfx+2*1*8)
		.byte >(east_right_gfx+2*1*8)
		.byte >(south_right_gfx+2*1*8)
		.byte >(west_right_gfx+2*1*8)
right_d800_lo:	.byte <(north_right_gfx+2*1*8+2*1)
		.byte <(east_right_gfx+2*1*8+2*1)
		.byte <(south_right_gfx+2*1*8+2*1)
		.byte <(west_right_gfx+2*1*8+2*1)
right_d800_hi:	.byte >(north_right_gfx+2*1*8+2*1)
		.byte >(east_right_gfx+2*1*8+2*1)
		.byte >(south_right_gfx+2*1*8+2*1)
		.byte >(west_right_gfx+2*1*8+2*1)

north_left_gfx:		.incbin "converters/ui/north_left-bitmap.bin"
			.incbin "converters/ui/north_left-screen.bin"
			.incbin "converters/ui/north_left-d800.bin"
north_center_gfx:	.incbin "converters/ui/north_center-bitmap.bin"
			.incbin "converters/ui/north_center-screen.bin"
			.incbin "converters/ui/north_center-d800.bin"
north_right_gfx:	.incbin "converters/ui/north_right-bitmap.bin"
			.incbin "converters/ui/north_right-screen.bin"
			.incbin "converters/ui/north_right-d800.bin"

east_left_gfx:		.incbin "converters/ui/east_left-bitmap.bin"
			.incbin "converters/ui/east_left-screen.bin"
			.incbin "converters/ui/east_left-d800.bin"
east_center_gfx:	.incbin "converters/ui/east_center-bitmap.bin"
			.incbin "converters/ui/east_center-screen.bin"
			.incbin "converters/ui/east_center-d800.bin"
east_right_gfx:		.incbin "converters/ui/east_right-bitmap.bin"
			.incbin "converters/ui/east_right-screen.bin"
			.incbin "converters/ui/east_right-d800.bin"

south_left_gfx:		.incbin "converters/ui/south_left-bitmap.bin"
			.incbin "converters/ui/south_left-screen.bin"
			.incbin "converters/ui/south_left-d800.bin"
south_center_gfx:	.incbin "converters/ui/south_center-bitmap.bin"
			.incbin "converters/ui/south_center-screen.bin"
			.incbin "converters/ui/south_center-d800.bin"
south_right_gfx:	.incbin "converters/ui/south_right-bitmap.bin"
			.incbin "converters/ui/south_right-screen.bin"
			.incbin "converters/ui/south_right-d800.bin"

west_left_gfx:		.incbin "converters/ui/west_left-bitmap.bin"
			.incbin "converters/ui/west_left-screen.bin"
			.incbin "converters/ui/west_left-d800.bin"
west_center_gfx:	.incbin "converters/ui/west_center-bitmap.bin"
			.incbin "converters/ui/west_center-screen.bin"
			.incbin "converters/ui/west_center-d800.bin"
west_right_gfx:		.incbin "converters/ui/west_right-bitmap.bin"
			.incbin "converters/ui/west_right-screen.bin"
			.incbin "converters/ui/west_right-d800.bin"
.endproc

.segment "GUI2"
pixelsPerBig_lo:	.byte 0
		.repeat 255,I
			.byte <((20*256)/(I+1))
		.endrep
pixelsPerBig_hi:	.byte 0
		.repeat 255,I
			.byte >((20*256)/(I+1))
		.endrep

.proc renderBigBars
		jsr renderBigHpBar
		jsr renderBigFoodBar
		rts
.endproc

.proc renderBigHpBar
		ldx #31
		ldy #1
		jsrf setupDestination
		ldy #PartyMember::hpCurrent+1
		lda (CURRENT_PARTYMEMBER),y
		bpl :+
			ldy #0
			jmp :+++
		:

		sec
		ldy #PartyMember::hp+0
		lda (CURRENT_PARTYMEMBER),y
		ldy #PartyMember::hpCurrent+0
		sbc (CURRENT_PARTYMEMBER),y
		ldy #PartyMember::hp+1
		lda (CURRENT_PARTYMEMBER),y
		ldy #PartyMember::hpCurrent+1
		sbc (CURRENT_PARTYMEMBER),y
		bpl :+
			ldy #20
			jmp :++
		:

		ldy #PartyMember::hp+0
		lax (CURRENT_PARTYMEMBER),y
		lda pixelsPerBig_lo,x
		sta NUM1+0
		lda pixelsPerBig_hi,x
		sta NUM1+1
		ldy #PartyMember::hpCurrent+0
		lda (CURRENT_PARTYMEMBER),y
		sta NUM2
		jsr multiply
		cmp #$80
		bcc :+
			iny
		:
		jmp renderBigBar
.endproc

.proc renderBigFoodBar
		ldx #31
		ldy #2
		jsrf setupDestination

		sec
		ldy #PartyMember::food
		lda #100
		sbc (CURRENT_PARTYMEMBER),y
		bpl :+
			ldy #20
			jmp :++
		:

		ldx #100
		lda pixelsPerBig_lo,x
		sta NUM1+0
		lda pixelsPerBig_hi,x
		sta NUM1+1

		lda (CURRENT_PARTYMEMBER),y
		sta NUM2
		jsr multiply
		cmp #$80
		bcc :+
			iny
		:
		jmp renderBigBar
.endproc

.proc renderBigBar
		; Render big bar 0..19 px
		ldx mul5,y
		tya
		pha

		.repeat 5,I
			ldy #3+I*8
			lda hpPattern+I,x
			sta (DST),y
			iny
			sta (DST),y
		.endrep
		pla
		tax
		lda hpColors,x
		ldy #4
		:
			sta (DST3),y
			dey
		bpl :-

		rts

hpColors:	.byte $0
			.byte $2
			.byte $2
			.byte $2
			.byte $2
			.byte $a
			.byte $a
			.byte $a
			.byte $a
			.byte $7
			.byte $7
			.byte $7
			.byte $7
			.byte $3
			.byte $3
			.byte $3
			.byte $3
			.byte $5
			.byte $5
			.byte $5
			.byte $5

mul5:		.repeat 21,I
				.byte I*5
			.endrep

hpPattern:	.byte %00000000,%00000000,%00000000,%00000000,%00000000
			.byte %11000000,%00000000,%00000000,%00000000,%00000000
			.byte %11110000,%00000000,%00000000,%00000000,%00000000
			.byte %11111100,%00000000,%00000000,%00000000,%00000000
			.byte %11111111,%00000000,%00000000,%00000000,%00000000
			.byte %11111111,%11000000,%00000000,%00000000,%00000000
			.byte %11111111,%11110000,%00000000,%00000000,%00000000
			.byte %11111111,%11111100,%00000000,%00000000,%00000000
			.byte %11111111,%11111111,%00000000,%00000000,%00000000
			.byte %11111111,%11111111,%11000000,%00000000,%00000000
			.byte %11111111,%11111111,%11110000,%00000000,%00000000
			.byte %11111111,%11111111,%11111100,%00000000,%00000000
			.byte %11111111,%11111111,%11111111,%00000000,%00000000
			.byte %11111111,%11111111,%11111111,%11000000,%00000000
			.byte %11111111,%11111111,%11111111,%11110000,%00000000
			.byte %11111111,%11111111,%11111111,%11111100,%00000000
			.byte %11111111,%11111111,%11111111,%11111111,%00000000
			.byte %11111111,%11111111,%11111111,%11111111,%11000000
			.byte %11111111,%11111111,%11111111,%11111111,%11110000
			.byte %11111111,%11111111,%11111111,%11111111,%11111100
			.byte %11111111,%11111111,%11111111,%11111111,%11111111
.endproc

.segment "GUI3"
.proc renderHealthBar
		; Render health bar 0..15 px
		clc
		lda XPOS
		adc #3
		tax
		lda YPOS
		adc #5
		tay
		jsrf setupDestination
		ldy #PartyMember::hpCurrent+1
		lda (CURRENT_PARTYMEMBER),y
		bpl :+
			ldy #0
			jmp :+++
		:

		sec
		ldy #PartyMember::hp+0
		lda (CURRENT_PARTYMEMBER),y
		ldy #PartyMember::hpCurrent+0
		sbc (CURRENT_PARTYMEMBER),y
		ldy #PartyMember::hp+1
		lda (CURRENT_PARTYMEMBER),y
		ldy #PartyMember::hpCurrent+1
		sbc (CURRENT_PARTYMEMBER),y
		bpl :+
			ldy #16
			jmp :++
		:

		ldy #PartyMember::hp+0
		lax (CURRENT_PARTYMEMBER),y
		lda pixelsPerHp_lo,x
		sta NUM1+0
		lda pixelsPerHp_hi,x
		sta NUM1+1
		ldy #PartyMember::hpCurrent+0
		lda (CURRENT_PARTYMEMBER),y
		sta NUM2
		jsr multiply
		cmp #$80
		bcc :+
			iny
		:

		tya
		pha
		asl
		asl
		tax

		.repeat 4,I
			ldy #3+I*8
			lda hpPattern+I,x
			sta (DST),y
			iny
			sta (DST),y
		.endrep
		pla
		tax
		lda hpColors,x
		ldy #3
		:
			sta (DST3),y
			dey
		bpl :-

		rts

hpColors:	.byte $0
			.byte $2
			.byte $2
			.byte $2
			.byte $a
			.byte $a
			.byte $a
			.byte $7
			.byte $7
			.byte $7
			.byte $7
			.byte $3
			.byte $3
			.byte $3
			.byte $5
			.byte $5
			.byte $5

hpPattern:	.byte %00000000,%00000000,%00000000,%00000000
			.byte %11000000,%00000000,%00000000,%00000000
			.byte %11110000,%00000000,%00000000,%00000000
			.byte %11111100,%00000000,%00000000,%00000000
			.byte %11111111,%00000000,%00000000,%00000000
			.byte %11111111,%11000000,%00000000,%00000000
			.byte %11111111,%11110000,%00000000,%00000000
			.byte %11111111,%11111100,%00000000,%00000000
			.byte %11111111,%11111111,%00000000,%00000000
			.byte %11111111,%11111111,%11000000,%00000000
			.byte %11111111,%11111111,%11110000,%00000000
			.byte %11111111,%11111111,%11111100,%00000000
			.byte %11111111,%11111111,%11111111,%00000000
			.byte %11111111,%11111111,%11111111,%11000000
			.byte %11111111,%11111111,%11111111,%11110000
			.byte %11111111,%11111111,%11111111,%11111100
			.byte %11111111,%11111111,%11111111,%11111111

pixelsPerHp_lo:	.byte 0
		.repeat 255,I
			.byte <((16*256)/(I+1))
		.endrep
pixelsPerHp_hi:	.byte 0
		.repeat 255,I
			.byte >((16*256)/(I+1))
		.endrep
.endproc

.macro DEFKEY key,key2,callback,value
	.byte key, key2, <callback, >callback, value
.endmacro

.export gui_processInputEvents
.proc gui_processInputEvents
		jsr processKeyEvents
		jsrf processMouseEvents
		rts
.endproc

.proc processKeyEvents
		lda KEYEVENT+KeyEvent::keyPressed
		bne :+
			rts
		:

		lda ASK_FOR_SPELL_RECEIVER_GUI
		bne :+
			jsr checkKeys
			jmp :++
		:
			lda mouseType
			cmp #MOUSETYPE_NONE
			bne :+
				jsr modalCheckKeys
		:

		ldx #$ff
		jsrf updatePointerWithKeyboardOnlyControls

		lda #0
		sta KEYEVENT+KeyEvent::keyPressed
		sta KEYEVENT+KeyEvent::normalChar
		sta KEYEVENT+KeyEvent::specialChar
		sta KEYEVENT+KeyEvent::modifiers
		rts
	
checkKeys:
.scope
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
match:			lda keyboardActionList+2,x
				sta INDJMP+0
				lda keyboardActionList+3,x
				sta INDJMP+1
				lda keyboardActionList+4,x
				tax
				jmp (INDJMP)

noMatch:	inx
	   		inx
	   		inx
			inx
			inx
			lda keyboardActionList+0,x
			ora keyboardActionList+1,x
		bne nextKey
		rts
.endscope

modalCheckKeys:
.scope
		lda KEYEVENT+KeyEvent::modifiers
		and #$80
		beq :+
			jmp modalEscape
		:

		ldx #$00
		nextKey:
			lda keyboardActionListModal+1,x
			beq :+
				cmp KEYEVENT+KeyEvent::specialChar
				beq match
			:
			lda KEYEVENT+KeyEvent::normalChar
			cmp keyboardActionListModal+0,x
			bne noMatch
match:			lda keyboardActionListModal+2,x
				sta INDJMP+0
				lda keyboardActionListModal+3,x
				sta INDJMP+1
				lda keyboardActionListModal+4,x
				tax
				jmp (INDJMP)

noMatch:	inx
	   		inx
	   		inx
			inx
			inx
			lda keyboardActionListModal+0,x
			ora keyboardActionListModal+1,x
		bne nextKey
		rts
.endscope

keyboardActionList:
		DEFKEY $17, 0, moveForward, 0
		DEFKEY $13, 0, moveBackward, 0
		DEFKEY $11, 0, turnLeft, 0
		DEFKEY $05, 0, turnRight, 0
		DEFKEY $01, 0, moveLeft, 0
		DEFKEY $04, 0, moveRight, 0
		DEFKEY $12, 0, debugPrintPartyPosition, 0
		DEFKEY $07, 0, toggleGodMode, 0
		DEFKEY $31, 0, digitPressed, 1
		DEFKEY $32, 0, digitPressed, 2
		DEFKEY $33, 0, digitPressed, 3
		DEFKEY $34, 0, digitPressed, 4
		DEFKEY $35, 0, digitPressed, 5
		DEFKEY $36, 0, digitPressed, 6
		DEFKEY $37, 0, digitPressed, 7
		DEFKEY $38, 0, digitPressed, 8
		DEFKEY $39, 0, digitPressed, 9
		DEFKEY $30, 0, digitPressed, 10
		DEFKEY $2c, 0, digitPressed, 11
		DEFKEY $2e, 0, digitPressed, 12
		DEFKEY $1f, 0, escape, 0
		DEFKEY $03, 0, camp, 0
		DEFKEY $08, 0, help, 0
		DEFKEY $20, 0, space, 0
		DEFKEY 0, $02, enter, 0
		DEFKEY $02, 0, bestiary, 0
		DEFKEY 0, $10, f1, 0
		DEFKEY 0, $20, f3, 0
		DEFKEY $0d, 0, automap, 0
	   	DEFKEY 0, $80, cursorUpDown, 0
	   	DEFKEY 0, $04, cursorLeftRight, 0
		.word 0

keyboardActionListModal:
	   	DEFKEY 0, $80, cursorUpDown, 0
	   	DEFKEY 0, $04, cursorLeftRight, 0
		DEFKEY $20, 0, space, 0
		DEFKEY 0, $02, enter, 0
		DEFKEY $1f, 0, modalEscape, 0
		.word 0

.proc modalEscape
		ldx #<-1
		jsrf characterSelectedClickHandler
		rts
.endproc

.endproc

.pushseg
.segment "GUI4"
.export setGameScreenMode
.proc setGameScreenMode
	txa
	cmp #SCREEN_MODE_normal
	bne :+
		jmp toNormalMode
	:
	cmp #SCREEN_MODE_text
	bne :+
		jmp toTextMode
	:
	cmp #SCREEN_MODE_mainmenu
	bne :+
		jmp toMainMenuMode
	:
	cmp #SCREEN_MODE_text_header
	bne :+
		jmp toTextModeWithHeader
	:
	rts

	toTextModeWithHeader:
		jsrf clearTextScreen
		memcpy_ram d800_backup, $d800+15*40, 10*40

		ldx #<screenModeGameDocumentWithHeader
		ldy #>screenModeGameDocumentWithHeader
		jmp fallThrough

	toTextMode:
		jsrf clearTextScreen
		memcpy_ram d800_backup, $d800+15*40, 10*40

		ldx #<screenModeGameDocument
		ldy #>screenModeGameDocument
fallThrough:
		sec
		jsr setScreenMode

		lda #1
		ldx #0
		:
			sta $d800+15*40,x
			inx
		bne :-
		:
			sta $d800+15*40+256,x
			inx
			cpx #<400
		bne :-
		sec
		rts

	toNormalMode:
		jsrf clearTextScreen
		memcpy_ram $d800+15*40, d800_backup, 10*40

		sec
		ldx #<screenModeGameNormal
		ldy #>screenModeGameNormal
		jsr setScreenMode

		sec
		rts

	toMainMenuMode:
		memcpy_ram $d800+15*40, d800_backup, 10*40

		sec
		ldx #<screenModeMainMenu
		ldy #>screenModeMainMenu
		jsr setScreenMode

		sec
		rts
.endproc
.popseg

.proc f3
		jsrf :+
		rts
.pushseg
.segment "GUI4"
		:
		lda #1
		sta textColor
		ldx #<twomhzString
		ldy #>twomhzString
		jsr text_writeNullString

		lda #7
		sta textColor

		ldx #<disabledString
		ldy #>disabledString
		
		lda twomhzMode
		eor #1
		sta twomhzMode
		beq :+
			ldx #<enabledString
			ldy #>enabledString
		:
		jsr text_writeNullString

		jsrf text_newLine

		sec
		rts
twomhzString: .byte "2Mhz mode: ",0
enabledString: .byte "enabled",0
disabledString:.byte "disabled",0
.popseg
.endproc


.proc f1
		jsrf :+
		rts
.pushseg
.segment "GUI4"
		:

		lda #1
		sta textColor
		ldx #<mouseString
		ldy #>mouseString
		jsr text_writeNullString

		lda #7
		sta textColor
		ldy mouseType
		ldx mouseTypes_lo,y
		lda mouseTypes_hi,y
		tay
		jsr text_writeNullString

		jsrf text_newLine

		sec
		rts
		
mouseString: .byte "Mouse driver: ",0
mouseTypes_lo: .byte <m0,<m1,<m2
mouseTypes_hi: .byte >m0,>m1,>m2
m0:		.byte "1351 (Acc)",0
m1:		.byte "1351 (No Acc)",0
;m2:		.byte "Joy-mouse",0
m2:		.byte "None",0
.popseg
.endproc

.proc clickAllPopups
		lda MEMBER_INVENTORY_OPEN
		bmi :+
			jsrf showNormal
		:
		lda SPELLBOOK_MEMBER_INDEX
		bmi :+
			lda #<-1
			sta SPELLBOOK_MEMBER_INDEX
			jsrf renderControls
		:
		rts
.endproc

.proc camp
		jsr clickAllPopups
		jsrf camp_do
		sec
		rts
.endproc

.proc bestiary
		lda #0
		sta screenEnabled
		jsr clickAllPopups
		jsrf bestiaryGo
		sec
		rts
.endproc

.proc automap
		lda #0
		sta screenEnabled
		jsr clickAllPopups
		jsrf automapGo
		sec
		rts
.endproc

.proc help
		lda #0
		sta screenEnabled
		jsr clickAllPopups
		jsrf helpGo
		sec
		rts
.endproc

.proc cursorUpDown
		lda KEYEVENT+KeyEvent::modifiers
		and #$50
		beq :+
up:			ldx #0
			jmp checkKeyboardOnlyControls
		:
down:	ldx #1
		jmp checkKeyboardOnlyControls
.endproc
cursorUp = cursorUpDown::up
cursorDown = cursorUpDown::down

.proc cursorLeftRight
		lda KEYEVENT+KeyEvent::modifiers
		and #$50
		beq :+
left:		ldx #2
			jmp checkKeyboardOnlyControls
		:
right:	ldx #3
		jmp checkKeyboardOnlyControls
.endproc
cursorLeft = cursorLeftRight::left
cursorRight = cursorLeftRight::right

.proc checkKeyboardOnlyControls
		lda mouseType
		cmp #MOUSETYPE_NONE
		bne :+
			jsrf updatePointerWithKeyboardOnlyControls
		:
		sec
		rts
.endproc

.proc _fakeMouseEvent
		jsrf fakeMouseEvent
		jsrf processMouseEvents
dont:	ldx #$ff
		jmp checkKeyboardOnlyControls
.endproc
.proc space
.if 0
		ldx #0
		lda partyMembers_lo,x
		sta CURRENT_PARTYMEMBER+0
		lda partyMembers_hi,x
		sta CURRENT_PARTYMEMBER+1

		ldy #(PartyMember::inventory + INVENTORY_BACKPACK*2)
		lda #<(items+$17e*.sizeof(Item))
		sta (CURRENT_PARTYMEMBER),y
		iny
		lda #>(items+$17e*.sizeof(Item))
		sta (CURRENT_PARTYMEMBER),y
		iny

		lda #<(items+$17f*.sizeof(Item))
		sta (CURRENT_PARTYMEMBER),y
		iny
		lda #>(items+$17f*.sizeof(Item))
		sta (CURRENT_PARTYMEMBER),y
		iny

		lda #<(items+$180*.sizeof(Item))
		sta (CURRENT_PARTYMEMBER),y
		iny
		lda #>(items+$180*.sizeof(Item))
		sta (CURRENT_PARTYMEMBER),y
		iny

		sec
		rts
.endif

.if 0
		lda #3
		sta ARGS+0
		lda #<15
		sta ARGS+1
		lda #>15
		sta ARGS+2
		jsrf gameState_giveMemberDamage
		lda #0
		sta ARGS+0
		lda #<15
		sta ARGS+1
		lda #>15
		sta ARGS+2
		jsrf gameState_giveMemberDamage
.endif

.if 0
		ldx #0
		lda partyMembers_lo,x
		sta CURRENT_PARTYMEMBER+0
		lda partyMembers_hi,x
		sta CURRENT_PARTYMEMBER+1

		ldy #(PartyMember::inventory + INVENTORY_BACKPACK*2)

		lda #<(items+$040*.sizeof(Item))
		sta (CURRENT_PARTYMEMBER),y
		iny
		lda #>(items+$040*.sizeof(Item))
		sta (CURRENT_PARTYMEMBER),y
		iny

		lda #<(items+$152*.sizeof(Item))
		sta (CURRENT_PARTYMEMBER),y
		iny
		lda #>(items+$152*.sizeof(Item))
		sta (CURRENT_PARTYMEMBER),y
		iny

		lda #<(items+$153*.sizeof(Item))
		sta (CURRENT_PARTYMEMBER),y
		iny
		lda #>(items+$153*.sizeof(Item))
		sta (CURRENT_PARTYMEMBER),y
		iny

		lda #<(items+$154*.sizeof(Item))
		sta (CURRENT_PARTYMEMBER),y
		iny
		lda #>(items+$154*.sizeof(Item))
		sta (CURRENT_PARTYMEMBER),y
		iny

		lda #<(items+$155*.sizeof(Item))
		sta (CURRENT_PARTYMEMBER),y
		iny
		lda #>(items+$155*.sizeof(Item))
		sta (CURRENT_PARTYMEMBER),y
		iny

		lda #<(items+$156*.sizeof(Item))
		sta (CURRENT_PARTYMEMBER),y
		iny
		lda #>(items+$156*.sizeof(Item))
		sta (CURRENT_PARTYMEMBER),y
		iny
.endif
;		jsrf renderer_drawBurningHands

;		sec
;		rts

;		ldx #9
;		jsrf npc_runDialogue

;		jsrf gameState_useWandOfSilvias
;		sec
;		rts

		lda mouseType
		cmp #MOUSETYPE_NONE
		bne _fakeMouseEvent::dont
		ldx #1
		jmp _fakeMouseEvent
.endproc
.proc enter
		lda mouseType
		cmp #MOUSETYPE_NONE
		bne _fakeMouseEvent::dont
		ldx #2
		jmp _fakeMouseEvent
.endproc

.proc toggleGodMode
	lda godMode
	eor #1
	sta godMode
	sta $d020
	sec
	rts
.endproc

.proc loadLevel
	jsrf gameState_loadLevel
	sec
	rts
.endproc

.proc digitPressed
	lda KEYEVENT+KeyEvent::modifiers
	and #$02
	beq skipC128digits
		cpx #7
		bne :+
			jmp turnLeft
		:
		cpx #8
		bne :+
			jmp moveForward
		:
		cpx #9
		bne :+
			jmp turnRight
		:
		cpx #4
		bne :+
			jmp moveLeft
		:
		cpx #5
		bne :+
			jmp moveBackward
		:
		cpx #6
		bne :+
			jmp moveRight
		:
		sec
		rts
	skipC128digits:

	lda KEYEVENT+KeyEvent::modifiers
	and #$50
	beq :++
		lda godMode
		beq :+
			jmp loadLevel
		:
		dex
		jsrf toggleSwapping
		sec
		rts
	:

	lda SPELLBOOK_MEMBER_INDEX
	bmi :++
		cpx #6
		bcs :+
			cpx SPELLBOOK_LEVEL_INDEX
			beq :+
				lda #<-1
				sta SPELLBOOK_SELECTED_ROW
				stx SPELLBOOK_LEVEL_INDEX
				jsrf renderSpellBook
		:
		rts
	:

	cpx #7
	bcc :+
		rts
	:
	dex

	lda MEMBER_INVENTORY_OPEN
	bmi :+
		cpx MEMBER_INVENTORY_OPEN
		bne :+
			jsrf toggleStats
			jmp :++
	:
		jsrf showInventory
	:
	sec
	rts
.endproc

.proc escape
	lda MEMBER_INVENTORY_OPEN
	bmi :+
		jsrf showNormal
	:
	lda SPELLBOOK_MEMBER_INDEX
	bmi :+
		lda #<-1
		sta SPELLBOOK_MEMBER_INDEX
		jsrf renderControls
	:
	sec
	rts
.endproc

.export debugPrintPartyPosition
.proc debugPrintPartyPosition
	lda #3
	sta textColor
	lda partyPosition+0
	sta ARGS+0
	lda partyPosition+1
	sta ARGS+1
	jsrf text_writeHexShort
	ldx #':'
	jsrf text_putChar
	lda partyDirection
	sta ARGS+0
	jsrf text_writeDecimalByte
	jsrf text_newLine
	sec
	rts
.endproc

; x = direction
.proc moveInDirection
	txa
	and #3
	sta ARGS+2
	lda partyPosition+0
	sta ARGS+0
	lda partyPosition+1
	sta ARGS+1
	jsrf gameState_getFreePositionInDirection
	lda ARGS+1
	bpl :+
		jsrf gameState_wayBlocked
		sec
		rts
	:
	jsrf gameState_teleportParty
	jsrf text_newLine
	sec
	rts
.endproc

.proc moveForward
	lda KEYEVENT+KeyEvent::modifiers
	and #4
	beq :+
		jmp cursorUp
	:
	ldx partyDirection
	jmp moveInDirection
.endproc

.proc moveBackward
	lda KEYEVENT+KeyEvent::modifiers
	and #4
	beq :+
		jmp cursorDown
	:
	ldx partyDirection
	inx
	inx
	jmp moveInDirection
.endproc

.proc moveLeft
	lda KEYEVENT+KeyEvent::modifiers
	and #4
	beq :+
		jmp cursorLeft
	:
	ldx partyDirection
	dex
	jmp moveInDirection
.endproc

.proc moveRight
	lda KEYEVENT+KeyEvent::modifiers
	and #4
	beq :+
		jmp cursorRight
	:
	ldx partyDirection
	inx
	jmp moveInDirection
.endproc

.proc turnLeft
	ldx partyDirection
	dex
	txa
	and #3
	sta partyDirection
	lda #1
	sta shouldUpdateCompass
	sta SHOULDRENDER
	inc DIDTELEPORT
	sec
	rts
.endproc

.proc turnRight
	ldx partyDirection
	inx
	txa
	and #3
	sta partyDirection
	lda #1
	sta shouldUpdateCompass
	sta SHOULDRENDER
	inc DIDTELEPORT
	sec
	rts
.endproc

.export printTakenMessage
.proc printTakenMessage
		lda POINTER_ITEM+0
		ora POINTER_ITEM+1
		bne :+
			rts
		:

		lda #1
		sta textColor

		lda POINTER_ITEM+0
		sta CUR_ITEM+0
		lda POINTER_ITEM+1
		sta CUR_ITEM+1

		ldx #0
		lda godMode
		bne :+
			ldy #Item::flags
			jsr lda_CUR_ITEM_y
			and #ITEM_FLAGS_IDENTIFIED
			beq :++
			:
				inx
		:
		stx ARGS+0
		jsrf printItemName

		ldx #<takenString
		ldy #>takenString
		jsr text_writeNullString
		rts

takenString:	.byte " taken.",$a,0
.endproc

.proc _moveClickHandler
		lda lo,x
		sta INDJMP+0
		lda hi,x
		sta INDJMP+1
		jmp (INDJMP)
lo:		.byte <turnLeft,<moveForward,<turnRight,<moveLeft,<moveBackward,<moveRight
hi:		.byte >turnLeft,>moveForward,>turnRight,>moveLeft,>moveBackward,>moveRight
.endproc

.segment "GUI2"
throwSubpos:
	.byte 0,1,3,2
	.byte 1,3,2,0
	.byte 2,0,1,3
	.byte 3,2,0,1

.proc moveClickHandler
	jsrf _moveClickHandler
	jmp regionDone
.endproc

.proc floorClickHandler
		.pushseg
		.segment "BSS"
			mazePosition: .res 2
			subpos: .res 1
		.popseg

		;mazePosition = GameState.partyPosition;   *2 because we want to perform item look ups later
		lda partyPosition+0
		asl
		sta mazePosition+0
		lda partyPosition+1
		rol
		sta mazePosition+1

		cpx #2
		bcc :+
			;if (cr.parameter>=2)

			;mazePosition = GameState.partyPosition + clickFloorMazeOffsets[GameState.partyDirection];
			ldy partyDirection
			clc
			lda mazePosition+0
			adc offsets_lo,y
			sta mazePosition+0
			sta TMP+0
			lda mazePosition+1
			adc offsets_hi,y
			and #$07
			sta mazePosition+1
			asl TMP+0
			rol
			sta TMP+1
			clc
			lda #<maze
			adc TMP+0
			sta TMP+0
			lda #>maze
			adc TMP+1
			sta TMP+1

			;int wmi = mb.walls[GameState.renderer.relativeDirectionSouth];
			ldy relativeDirectionSouth
			lda (TMP),y
			tay
			
			;WallMapping wm = GameState.inf.wallMappings.get(wmi);
						;if ((wm.flags & 0xb) == 0)
			lda wallFlags,y
			and #(WALLFLAG_ISDOOR|WALLFLAG_PASSPARTY|WALLFLAG_PASSSMALL)
			bne :+
				sec
				jmp regionDone
		:

		;MazeBlock mb = GameState.inf.maze.mazeBlocks[mazePosition];
		clc
		lda mazePosition+0
		adc #<topItemPtrs
		sta TMP+0
		lda mazePosition+1
		adc #>topItemPtrs
		sta TMP+1
		ldy #0
		lda (TMP),y
		sta TOP_ITEM+0
		iny
		lda (TMP),y
		sta TOP_ITEM+1

		;int subpos = clickFloorMazeSubPositions[GameState.partyDirection][cr.parameter];
		txa
		asl
		asl
		adc partyDirection
		tax
		lda throwSubpos,x
		sta subpos

		; if (GameState.pointerItem==0)
		lda POINTER_ITEM+0
		ora POINTER_ITEM+1
		beq emptyPointer
		jmp nonEmptyPointer
		emptyPointer:
			;int itemIndex = GameState.unlinkItem(topItemIndexPtr, subpos, -1);
			lda subpos
			sta ARGS+0
			jsrf items_unlinkItemBySubPos

			;mb.topItemIndex = topItemIndexPtr[0];
			ldy #0
			lda TOP_ITEM+0
			sta (TMP),y
			iny
			lda TOP_ITEM+1
			sta (TMP),y

			;if (itemIndex == 0)
			lda CUR_ITEM+0
			ora CUR_ITEM+1
			bne :+
				sec
				jmp regionDone
			:

			lda CUR_ITEM+0
			pha
			lda CUR_ITEM+1
			pha

			; ScriptParser.executeTrigger(mazePosition, ScriptParser.TRIGGER_ITEMTAKEN);
			lda mazePosition+1
			lsr
			sta ARGS+1
			lda mazePosition+0
			ror
			sta ARGS+0
			lda #TRIGGER_ITEMTAKEN
			sta ARGS+2
			jsrf scriptParser_executeTrigger
			
			; GameState.pointerItem = itemIndex;
			pla
			sta POINTER_ITEM+1
			pla
			sta POINTER_ITEM+0
			jsrf printTakenMessage
			jmp exit

		nonEmptyPointer:
			;GameState.linkItem(topItemIndexPtr, mazePosition, GameState.pointerItem, subpos);
			lda subpos
			sta ARGS+2
			lda mazePosition+1
			lsr
			sta ARGS+1
			lda mazePosition+0
			ror
			sta ARGS+0
			lda POINTER_ITEM+0
			sta CUR_ITEM+0
			lda POINTER_ITEM+1
			sta CUR_ITEM+1
			jsrf items_linkItem

			;mb.topItemIndex = topItemIndexPtr[0];
			ldy #0
			lda TOP_ITEM+0
			sta (TMP),y
			iny
			lda TOP_ITEM+1
			sta (TMP),y

			;ScriptParser.executeTrigger(mazePosition, ScriptParser.TRIGGER_ITEMDROPPED);
			lda mazePosition+1
			lsr
			sta ARGS+1
			lda mazePosition+0
			ror
			sta ARGS+0
			lda #TRIGGER_ITEMDROPPED
			sta ARGS+2
			jsrf scriptParser_executeTrigger

			;GameState.pointerItem = 0;
			lda #0
			sta POINTER_ITEM+0
			sta POINTER_ITEM+1

exit:		jsrf updatePointerSprite
		inc SHOULDRENDER
		sec
		jmp regionDone

offsets_lo:	.byte <(-32*2),<(1*2),<(32*2),<(-1*2)
offsets_hi:	.byte >(-32*2),>(1*2),>(32*2),>(-1*2)
.endproc

.proc throwItemHandler
		lda POINTER_ITEM+0
		ora POINTER_ITEM+1
		bne :+
			sec
			jmp regionDone
		:

		lda MEMBER_INVENTORY_OPEN
		sta ARGS+0
		lda POINTER_ITEM+0
		sta ARGS+1
		lda POINTER_ITEM+1
		sta ARGS+2
		lda partyPosition+0
		sta ARGS+3
		lda partyPosition+1
		sta ARGS+4

		;throwSubpos[GameState.partyDirection][cr.parameter];
		txa
		asl
		asl
		adc partyDirection
		tax
		lda throwSubpos,x
		sta ARGS+5

		lda partyDirection
		sta ARGS+6

		ldy #Item::type
		;lda (POINTER_ITEM),y
		jsr lda_POINTER_ITEM_y
		sta ARGS+7

		jsrf gameState_throwItem
		lda ARGS+0
		beq :+
			lda #0
			sta POINTER_ITEM+0
			sta POINTER_ITEM+1
			jsrf updatePointerSprite
		:

		sec
		jmp regionDone
.endproc

forwardDeltaLo:
	.byte <-32, <1, <32, <-1
forwardDeltaHi:
	.byte >-32, >1, >32, >-1

.proc useFrontWall
		;ScriptParser.triggerMask = ScriptParser.TRIGGER_PLAYERUSESWALL;
		lda #TRIGGER_PLAYERUSESWALL
		sta TRIGGER_MASK

		;int mazePosition = GameState.getForwardPosition(GameState.partyPosition, GameState.partyDirection);
		ldx partyDirection
		stx ARGS+2
		clc
		lda partyPosition+0
		adc forwardDeltaLo,x
		sta ARGS+0
		lda partyPosition+1
		adc forwardDeltaHi,x
		and #$03
		sta ARGS+1

		jsrf scriptParser_doHandleWallEvent
		lsr ARGS+0
		rts
.endproc

.proc frontWallClickHandler
		jsr useFrontWall
		jmp regionDone
.endproc

.proc characterSelectedClickHandler
	cpx #<-1
	bne :+
		lda #1
		sta textColor
		ldx #<abortedTxt
		ldy #>abortedTxt
		jsr text_writeNullString
		jmp return
		abortedTxt:.byte $a,"Spell aborted.",$a,0
	:

	stx AFFECTED_MEMBER_INDEX
	lda partyMembers_lo,x
	sta AFFECTED_MEMBER+0
	sta CURRENT_PARTYMEMBER+0
	lda partyMembers_hi,x
	sta AFFECTED_MEMBER+1
	sta CURRENT_PARTYMEMBER+1

	ldy #PartyMember::status
	lda (AFFECTED_MEMBER),y
	and #1
	bne :+
		sec
		jmp regionDone
	:

	lda #3
	sta textColor
	jsrf printMemberName
	jsrf text_newLine

	jsrf _castSpell

return:
	lda SPELLCASTER_USING_SCROLL
	beq :+
		jsrf gameState_spellCastSetMemberTimeout
;		jmp :++
	:
;		lda #<-1
;		sta SPELLBOOK_MEMBER_INDEX
;		jsrf renderControls
;	:

	lda #0
	sta ASK_FOR_SPELL_RECEIVER_GUI
	sec
	jmp regionDone

.pushseg
.segment "SPELLS"
_castSpell:
	ldx castSpellIndex
	lda spellTypes_lo,x
	sta CURRENT_SPELLTYPE+0
	lda spellTypes_hi,x
	sta CURRENT_SPELLTYPE+1

	ldy #SpellType::soundEffect
	lax (CURRENT_SPELLTYPE),y
	jsrf audio_playSoundEffect

	ldx AFFECTED_MEMBER_INDEX
	jsrf gameState_drawMagicalGlimmerAnimation

	ldy #SpellType::castFunc
	lda (CURRENT_SPELLTYPE),y
	sta INDJMP+0
	iny
	lda (CURRENT_SPELLTYPE),y
	sta INDJMP+1
	ora INDJMP+0
	beq :+
		ldx #<.bank(spellBank)
		jmp indirectJsr
	:
	rts
.popseg
.endproc

.proc abortSwapping
	txa
	pha

	lda #<swappingMemberTimer
	sta CURRENT_TIMER+0
	lda #>swappingMemberTimer
	sta CURRENT_TIMER+1
	lda #0
	ldy #Timer::active
	sta (CURRENT_TIMER),y
	ldx SWAPPING_MEMBER_INDEX
	inc partyMemberStatsChanged,x

	pla
	bmi noSwapping
	cmp SWAPPING_MEMBER_INDEX
	beq noSwapping
		cmp SPELLBOOK_MEMBER_INDEX
		beq :+
		cpx SPELLBOOK_MEMBER_INDEX
		bne :++
		:
			pha
			lda #<-1
			sta SPELLBOOK_MEMBER_INDEX
			jsrf renderControls
			pla
		:
		tax
		inc partyMemberStatsChanged,x
		ldy SWAPPING_MEMBER_INDEX
		jsrf gameState_swapMembers
	noSwapping:

	lda #<-1
	sta SWAPPING_MEMBER_INDEX
	rts
.endproc

.proc abortSwappingClickHandler
	jsr abortSwapping
	sec
	jmp regionDone
.endproc

.proc toggleSwapping
	lda partyMembers_lo,x
	sta CURRENT_PARTYMEMBER+0
	lda partyMembers_hi,x
	sta CURRENT_PARTYMEMBER+1
	ldy #PartyMember::status
	lda (CURRENT_PARTYMEMBER),y
	bne :+
		clc
		rts
	:

	lda SWAPPING_MEMBER_INDEX
	cmp #<-1
	bne :+
		lda #1
		sta swappingFlipFlop
		stx SWAPPING_MEMBER_INDEX
		inc partyMemberStatsChanged,x
		lda #<swappingMemberTimer
		sta CURRENT_TIMER+0
		lda #>swappingMemberTimer
		sta CURRENT_TIMER+1
		jsrf resetTimer
		lda #1
		ldy #Timer::active
		sta (CURRENT_TIMER),y
		jmp :++
	:
		jsr abortSwapping
	:
	sec
	rts
.endproc

.proc characterNameClickHandler
	jsr toggleSwapping
	jmp regionDone
.endproc

.proc characterPortraitClickHandler
	cpx #0
	bpl :+
		jsrf showNormal
		jmp :++
	:
		jsrf showInventory
	:
	sec
	jmp regionDone
.endproc

.proc previousCharacterClickHandler
	ldy #PartyMember::status
	ldx MEMBER_INVENTORY_OPEN
	:
		dex
		bpl :+
			ldx #5
		:
		lda partyMembers_lo,x
		sta CURRENT_PARTYMEMBER+0
		lda partyMembers_hi,x
		sta CURRENT_PARTYMEMBER+1
		lda (CURRENT_PARTYMEMBER),y
	beq :--
	stx MEMBER_INVENTORY_OPEN
	jsrf renderInventoryForPartyMember
	jmp regionDone
.endproc

.proc nextCharacterClickHandler
	ldy #PartyMember::status
	ldx MEMBER_INVENTORY_OPEN
	:
		inx
		cpx #6
		bne :+
			ldx #0
		:
		lda partyMembers_lo,x
		sta CURRENT_PARTYMEMBER+0
		lda partyMembers_hi,x
		sta CURRENT_PARTYMEMBER+1
		lda (CURRENT_PARTYMEMBER),y
	beq :--
	stx MEMBER_INVENTORY_OPEN
	jsrf renderInventoryForPartyMember
	jmp regionDone
.endproc

.proc toggleStats
	lda showingStats
	bne :+
		inc showingStats
		ldx MEMBER_INVENTORY_OPEN
		jsrf renderInventoryForPartyMember
		rts
	:
	lda #0
	sta showingStats
	ldx MEMBER_INVENTORY_OPEN
	lda #<-1
	sta MEMBER_INVENTORY_OPEN
	jsrf showInventory
	rts
.endproc

.proc showStatsGfx
	bitblt stats_gfx, 18,18, 22,4
	rts
stats_gfx:	.incbin "converters/ui/stats-bitmap.bin"
			.incbin "converters/ui/stats-screen.bin"
			.incbin "converters/ui/stats-d800.bin"
.endproc

.proc statsClickHandler
	jsr toggleStats
	jmp regionDone
.endproc

.proc inventoryItemClickHandler
		lda showingStats
		bne :+++
			ldy MEMBER_INVENTORY_OPEN
			sty CURRENT_PARTYMEMBER_INDEX
			lda partyMembers_lo,y
			sta CURRENT_PARTYMEMBER+0
			lda partyMembers_hi,y
			sta CURRENT_PARTYMEMBER+1

			lda MOUSEEVENT+MouseEvent::buttons
			cmp #1
			bne :+
				jsrf gameState_swapItemInInventory
			:
			cmp #2
			bne :+
				jsr useItemInInventory
			:
		:
		sec
		jmp regionDone
.endproc

.proc characterLeftHandHandler
		ldy #0
		jmp characterHandHandler
.endproc

.proc characterRightHandHandler
		ldy #1
		jmp characterHandHandler
.endproc

; x = partyMemberIndex
; y = handIndex
.proc characterHandHandler
		handIndex = ATTACKING_HAND_INDEX

		tya
		pha

		lda partyMembers_lo,x
		sta CURRENT_PARTYMEMBER+0
		lda partyMembers_hi,x
		sta CURRENT_PARTYMEMBER+1
		ldy #PartyMember::status
		lda (CURRENT_PARTYMEMBER),y
		bne :+
			pla
			clc
			jmp regionDone
		:

		pla
		tay

		lda MOUSEEVENT+MouseEvent::buttons
		cmp #2
		beq :+
			; Swap item
			stx TMP
			stx CURRENT_PARTYMEMBER_INDEX
			lda partyMembers_lo,x
			sta CURRENT_PARTYMEMBER+0
			lda partyMembers_hi,x
			sta CURRENT_PARTYMEMBER+1
			tya
			tax
			ldy TMP
			jsrf gameState_swapItemInInventory
			sec
			jmp regionDone
		:

		stx ATTACKING_PARTYMEMBER_INDEX
		sty handIndex

		;if (!(getMemberStatus(memberIndex, 13))) 
		ldy #IS_ACTIVE|IS_CONSCIOUS|NOT_PARALYZED
		jsrf gameState_getMemberStatus
		bcs :+
			rts
		:
		
		;int di = member.inventory[handIndex];
		lda handIndex
		clc
		asl
		adc #PartyMember::inventory
		tay
		lda (CURRENT_PARTYMEMBER),y
		sta CUR_ITEM+0
		iny
		lda (CURRENT_PARTYMEMBER),y
		sta CUR_ITEM+1

		;if (!mayUseItemInHand(memberIndex, handIndex))
		ldx handIndex
		jsrf gameState_mayUseItemInHand
		cpx #1
		beq :+
			rts
		:

		;if ((member.disabledHands & (1<<handIndex))!=0)
		lda handIndex
		clc
		adc #1
		ldy #PartyMember::disabledHands
		and (CURRENT_PARTYMEMBER),y
		beq :+
			rts
		:

		;if (!mayMemberUseItem(memberIndex, di))
		jsrf mayMemberUseItem
		bcs :+
			lda #1
			sta textColor
			jsrf printMemberName
			ldx #<cantUseTxt
			ldy #>cantUseTxt
			jsr text_writeNullString
			rts
			cantUseTxt:.byte " can not use this item.",$a,0
		:

		; Now if it is bare hands CUR_ITEM will be 0 here. Let it point to the first item instead to represent the hands
		lda CUR_ITEM+0
		ora CUR_ITEM+1
		bne :+
			lda #<items
			sta CUR_ITEM+0
			lda #>items
			sta CUR_ITEM+1
		:

		;ItemType itemType = items.itemTypes[item.type];
		ldy #Item::type
		jsr lda_CUR_ITEM_y

		;int usage = itemType.usage&0x7f;
		tay
		jsr items_fetchItemType

		lda itemType+ItemType::usage
		and #$7f

		;switch(usage)
		cmp #19
		bcc :+
			jmp default
		:
		tax
		lda cases_lo,x
		sta TMP+0
		lda cases_hi,x
		sta TMP+1
		jmp (TMP)

case0:
case16:
		lda #1
		sta textColor
		ldx #<thisItemIsAutomaticallyUsedWhenWorn
		ldy #>thisItemIsAutomaticallyUsedWhenWorn
		jsr text_writeNullString
		jmp break
		thisItemIsAutomaticallyUsedWhenWorn:.byte "This item is automatically used when worn.",$a,0
case1:
case2:
case3:
		; Normal attack
		ldx ATTACKING_PARTYMEMBER_INDEX
		ldy ATTACKING_HAND_INDEX
		jsrf gameState_normalAttack
		jmp break
case4:
case8:
case12:
case13:
case15:
		lda #1
		sta textColor
		ldx #<thisItemIsNotUsedInThisWay
		ldy #>thisItemIsNotUsedInThisWay
		jsr text_writeNullString
		jmp break
		thisItemIsNotUsedInThisWay:.byte "This item is not used in this way.",$a,0
case5:		lda #0
		jmp :+
case6:		lda #1
		:
		cmp SPELLBOOK_TYPE
		beq :+
			ldx #1
			stx SPELLBOOK_LEVEL_INDEX
		:
		sta SPELLBOOK_TYPE ; 0=mage, 1=cleric

		; Open Spellbook
		lda #<-1
		sta SPELLBOOK_SELECTED_ROW
		lda ATTACKING_PARTYMEMBER_INDEX
		sta SPELLBOOK_MEMBER_INDEX
		jsrf renderSpellBook
		jmp break
case7:
		; Eat food
		ldx ATTACKING_PARTYMEMBER_INDEX
		ldy ATTACKING_HAND_INDEX
		jsrf gameState_eatFood
		jmp break
case9:		
		; Use mage scroll
		ldy #Item::value
		jsr lda_CUR_ITEM_y
		sta ARGS+0
		ldx ATTACKING_PARTYMEMBER_INDEX
		ldy ATTACKING_HAND_INDEX
		jsrf gameState_useScroll
		jmp break
case10:
		; Use cleric scroll (+25)
		ldy #Item::value
		jsr lda_CUR_ITEM_y
		clc
		adc #25
		sta ARGS+0
		ldx ATTACKING_PARTYMEMBER_INDEX
		ldy ATTACKING_HAND_INDEX
		jsrf gameState_useScroll
		jmp break
case11:
		; Read parchment
		lda timersEnabled
		pha
		lda #0
		sta timersEnabled
		ldx #SCREEN_MODE_text
		jsrf setGameScreenMode
		ldy #Item::value
		jsr lax_CUR_ITEM_y
		dex
		sec
		jsrf text_writeTextMessage
		ldx #SCREEN_MODE_normal
		jsrf setGameScreenMode
		pla
		sta timersEnabled
		inc SHOULDRENDER
		jmp break
case14:
		; Drink potion
		ldx ATTACKING_PARTYMEMBER_INDEX
		ldy ATTACKING_HAND_INDEX
		jsrf gameState_drinkPotion
		jmp break
case18:
		; Use wand
		ldx ATTACKING_PARTYMEMBER_INDEX
		ldy ATTACKING_HAND_INDEX
		jsrf gameState_useWand
		jmp break
case17:
default:
break:
		sec
		jmp regionDone

cases_lo:	.byte <case0
		.byte <case1
		.byte <case2
		.byte <case3
		.byte <case4
		.byte <case5
		.byte <case6
		.byte <case7
		.byte <case8
		.byte <case9
		.byte <case10
		.byte <case11
		.byte <case12
		.byte <case13
		.byte <case14
		.byte <case15
		.byte <case16
		.byte <case17
		.byte <case18
cases_hi:	.byte >case0
		.byte >case1
		.byte >case2
		.byte >case3
		.byte >case4
		.byte >case5
		.byte >case6
		.byte >case7
		.byte >case8
		.byte >case9
		.byte >case10
		.byte >case11
		.byte >case12
		.byte >case13
		.byte >case14
		.byte >case15
		.byte >case16
		.byte >case17
		.byte >case18
.endproc

; x=inventory_index
; y=member index
; CURRENT_PARTYMEMBER
.proc useItemInInventory
		handIndex = ATTACKING_HAND_INDEX

		; Don't allow to use hands directly from inventory
		cpx #2
		bcs :+
			rts
		:

		sty ATTACKING_PARTYMEMBER_INDEX
		stx handIndex ; Actually Inventory index

		; Alive an well?
		ldx ATTACKING_PARTYMEMBER_INDEX
		ldy #IS_ACTIVE|IS_CONSCIOUS|NOT_PARALYZED
		jsrf gameState_getMemberStatus
		bcs :+
			rts
		:

		; Fetch item		
		lda handIndex ; Actually Inventory index
		clc
		asl
		adc #PartyMember::inventory
		tay
		lda (CURRENT_PARTYMEMBER),y
		sta CUR_ITEM+0
		iny
		lda (CURRENT_PARTYMEMBER),y
		sta CUR_ITEM+1

		; Empty slot?
		lda CUR_ITEM+0
		ora CUR_ITEM+1
		bne :+
			rts
		:

		;if (!mayMemberUseItem(memberIndex, di))
		jsrf mayMemberUseItem
		bcs :+
			lda #1
			sta textColor
			jsrf printMemberName
			ldx #<cantUseTxt
			ldy #>cantUseTxt
			jsr text_writeNullString
			rts
			cantUseTxt:.byte " can not use this item.",$a,0
		:

		; Fetch itemType.usae
		ldy #Item::type
		jsr lda_CUR_ITEM_y
		tay
		jsr items_fetchItemType
		lda itemType+ItemType::usage
		and #$7f

		;switch(usage)
		cmp #19
		bcc :+
			jmp default
		:
		tax
		lda cases_lo,x
		sta TMP+0
		lda cases_hi,x
		sta TMP+1
		jmp (TMP)

case0:
case16:
		lda #1
		sta textColor
		ldx #<thisItemIsAutomaticallyUsedWhenWorn
		ldy #>thisItemIsAutomaticallyUsedWhenWorn
		jsr text_writeNullString
		jmp break
		thisItemIsAutomaticallyUsedWhenWorn:.byte "This item is automatically used when worn.",$a,0
case1:
case2:
case3:
		jmp break
case4:
case8:
case12:
case13:
case15:
		lda #1
		sta textColor
		ldx #<thisItemIsNotUsedInThisWay
		ldy #>thisItemIsNotUsedInThisWay
		jsr text_writeNullString
		jmp break
		thisItemIsNotUsedInThisWay:.byte "This item is not used in this way.",$a,0
case5:
case6:
		jmp break
case7:
		; Eat food
		ldx ATTACKING_PARTYMEMBER_INDEX
		ldy ATTACKING_HAND_INDEX
		jsrf gameState_eatFood
		;jmp break
case9:
		; Use mage scroll
		;jmp break
case10:
		; Use cleric scroll (+25)
		jmp break
case11:
		; Read parchment
		lda timersEnabled
		pha
		lda #0
		sta timersEnabled
		ldx #SCREEN_MODE_text
		jsrf setGameScreenMode
		ldy #Item::value
		jsr lax_CUR_ITEM_y
		dex
		sec
		jsrf text_writeTextMessage
		ldx #SCREEN_MODE_normal
		jsrf setGameScreenMode
		pla
		sta timersEnabled
		inc SHOULDRENDER
		jmp break
case14:
		; Drink potion
		ldx ATTACKING_PARTYMEMBER_INDEX
		ldy ATTACKING_HAND_INDEX
		jsrf gameState_drinkPotion
		;jmp break
case18:
		jmp break
case17:
default:
break:
		sec
		jmp regionDone

cases_lo:	.byte <case0
		.byte <case1
		.byte <case2
		.byte <case3
		.byte <case4
		.byte <case5
		.byte <case6
		.byte <case7
		.byte <case8
		.byte <case9
		.byte <case10
		.byte <case11
		.byte <case12
		.byte <case13
		.byte <case14
		.byte <case15
		.byte <case16
		.byte <case17
		.byte <case18
cases_hi:	.byte >case0
		.byte >case1
		.byte >case2
		.byte >case3
		.byte >case4
		.byte >case5
		.byte >case6
		.byte >case7
		.byte >case8
		.byte >case9
		.byte >case10
		.byte >case11
		.byte >case12
		.byte >case13
		.byte >case14
		.byte >case15
		.byte >case16
		.byte >case17
		.byte >case18
.endproc

.proc selectSpellBookLevel
	lda #<-1
	sta SPELLBOOK_SELECTED_ROW
	stx SPELLBOOK_LEVEL_INDEX
	jsrf renderSpellBook
	sec
	jmp regionDone 
.endproc

; y=memberRow
.proc castSpellFromSpellBook
	ldx SPELLBOOK_MEMBER_INDEX
	lda partyMembers_lo,x
	sta SELECTED_SPELL+0
	lda partyMembers_hi,x
	sta SELECTED_SPELL+1

	lda #PartyMember::mageSpells
	ldx SPELLBOOK_TYPE
	beq :+
		lda #PartyMember::clericSpells
	:
	clc
	ldx SPELLBOOK_LEVEL_INDEX
	dex
	adc mul6,x
	clc
	adc SELECTED_SPELL+0
	sta SELECTED_SPELL+0
	bcc :+
		inc SELECTED_SPELL+1
	:
	clc
	tya
	adc SELECTED_SPELL+0
	sta SELECTED_SPELL+0
	bcc :+
		inc SELECTED_SPELL+1
	:

	ldy #0
	sty SPELLCASTER_USING_SCROLL
	ldx SPELLBOOK_TYPE
	lda (SELECTED_SPELL),y
	cpx #0
	beq :+
		clc
		adc #25
	:
	sta ARGS+0
	jsrf spells_castSpell

	rts
mul6:	.byte 0,6,12,18,24
.endproc

.proc selectSpell
	ldy spellBookMemberRow,x
	bmi exit

	cpx SPELLBOOK_SELECTED_ROW
	bne :+
		jsr castSpellFromSpellBook
;		bcc exit
		ldx #<-1
;		lda #<-1
;		sta SPELLBOOK_MEMBER_INDEX
;		jsrf renderControls
;		jmp exit
	:
	stx SPELLBOOK_SELECTED_ROW
	jsrf renderSpellBook

exit:	sec
	jmp regionDone 
.endproc

.proc abortSpell
	lda #<-1
	sta SPELLBOOK_MEMBER_INDEX
	jsrf renderControls
	sec
	jmp regionDone
.endproc

.proc campClickedHandler
	jsrf camp
	jmp regionDone
.endproc

.macro clickRegion _x, _y, _width, _height, _handler, _parameter
	.scope	
		x0=_x
		y0=_y
		x1=_x+_width
		y1=_y+_height
	.byte <x0,>x0,<x1,>x1,y0,y1, <(_handler), >(_handler), <_parameter
	.endscope
.endmacro

.macro clickRegionsCharacter dx,dy,index
	clickRegion dx+0, dy+8, 32,32,characterPortraitClickHandler, index
.endmacro

.macro clickRegionsCharacterName dx,dy,index
	clickRegion dx+0, dy+0, 64,8, characterNameClickHandler, index
.endmacro

.proc clickRegionsDungeon
	clickRegion 24,  8, 128,96, frontWallClickHandler, 1
	clickRegion  0,102,  88,18, floorClickHandler, 0
	clickRegion 89,102,  88,18, floorClickHandler, 1
	clickRegion  0, 72,  88,29, floorClickHandler, 2
	clickRegion 89, 72,  88,29, floorClickHandler, 3
	clickRegion  0,  8,  88,48, throwItemHandler, 0
	clickRegion 88,  8,  88,48, throwItemHandler, 1
	clickRegion  5,127,  20,17, moveClickHandler, 0
	clickRegion 25,127,  20,17, moveClickHandler, 1
	clickRegion 45,127,  20,17, moveClickHandler, 2
	clickRegion  5,144,  20,17, moveClickHandler, 3
	clickRegion 25,144,  20,17, moveClickHandler, 4
	clickRegion 45,144,  20,17, moveClickHandler, 5
	clickRegion 0, (22*8), 320,24, campClickedHandler, 0
.endproc

.proc clickRegionsSwapping
	clickRegionsCharacterName 184,8,0
	clickRegionsCharacterName 256,8,1
	clickRegionsCharacterName 184,64,2
	clickRegionsCharacterName 256,64,3
	clickRegionsCharacterName 184,120,4
	clickRegionsCharacterName 256,120,5
	clickRegion 184,0, 136,176, abortSwappingClickHandler, -1
.endproc

.proc clickRegionsControls
.endproc

.proc clickRegionsSpellBook
	clickRegion 34*2, 15*8+7*0, 20,7, selectSpellBookLevel, 1
	clickRegion 45*2, 15*8+7*0, 20,7, selectSpellBookLevel, 2
	clickRegion 56*2, 15*8+7*0, 20,7, selectSpellBookLevel, 3
	clickRegion 67*2, 15*8+7*0, 20,7, selectSpellBookLevel, 4
	clickRegion 78*2, 15*8+7*0, 20,7, selectSpellBookLevel, 5
	clickRegion 34*2, 15*8+7*1, 54*2,7, selectSpell, 0
	clickRegion 34*2, 15*8+7*2, 54*2,7, selectSpell, 1
	clickRegion 34*2, 15*8+7*3, 54*2,7, selectSpell, 2
	clickRegion 34*2, 15*8+7*4, 54*2,7, selectSpell, 3
	clickRegion 34*2, 15*8+7*5, 54*2,7, selectSpell, 4
	clickRegion 34*2, 15*8+7*6, 54*2,7, selectSpell, 5
	clickRegion 34*2, 15*8+7*7, 54*2,7, abortSpell, 0
.endproc

.proc clickRegionsAskForSpellReceiver
	clickRegion 184,   0, 64,48, characterSelectedClickHandler, 0
	clickRegion 256,   0, 64,48, characterSelectedClickHandler, 1
	clickRegion 184,  56, 64,48, characterSelectedClickHandler, 2
	clickRegion 256,  56, 64,48, characterSelectedClickHandler, 3
	clickRegion 184, 112, 64,48, characterSelectedClickHandler, 4
	clickRegion 256, 112, 64,48, characterSelectedClickHandler, 5
	clickRegion 34*2, 15*8+7*7, 54*2,7, characterSelectedClickHandler, -1
.endproc

.proc clickRegionsNormal
	clickRegionsCharacter 184,8,0
	clickRegionsCharacter 256,8,1
	clickRegionsCharacter 184,64,2
	clickRegionsCharacter 256,64,3
	clickRegionsCharacter 184,120,4
	clickRegionsCharacter 256,120,5
	clickRegionsCharacterName 184,8,0
	clickRegionsCharacterName 256,8,1
	clickRegionsCharacterName 184,64,2
	clickRegionsCharacterName 256,64,3
	clickRegionsCharacterName 184,120,4
	clickRegionsCharacterName 256,120,5

	clickRegion 216,16,32,16,characterLeftHandHandler, 0
	clickRegion 288,16,32,16,characterLeftHandHandler, 1
	clickRegion 216,72,32,16,characterLeftHandHandler, 2
	clickRegion 288,72,32,16,characterLeftHandHandler, 3
	clickRegion 216,128,32,16,characterLeftHandHandler, 4
	clickRegion 288,128,32,16,characterLeftHandHandler, 5
	clickRegion 216,16+16,32,16,characterRightHandHandler, 0
	clickRegion 288,16+16,32,16,characterRightHandHandler, 1
	clickRegion 216,72+16,32,16,characterRightHandHandler, 2
	clickRegion 288,72+16,32,16,characterRightHandHandler, 3
	clickRegion 216,128+16,32,16,characterRightHandHandler, 4
	clickRegion 288,128+16,32,16,characterRightHandHandler, 5
.endproc

.proc clickRegionsInventory
	clickRegion 22*8,0,32,32,characterPortraitClickHandler,-1
	clickRegion 37*8,0,24,16,nextCharacterClickHandler,-1
	clickRegion 37*8,16,24,16,previousCharacterClickHandler,-1
	clickRegion 22*8+3,158, 44,10, statsClickHandler,-1
	clickRegion 29*8,19*8, 16,16, inventoryItemClickHandler, INVENTORY_LEFT_HAND
	clickRegion 37*8,19*8, 16,16, inventoryItemClickHandler, INVENTORY_RIGHT_HAND
	clickRegion 23*8, 5*8, 16,16, inventoryItemClickHandler, INVENTORY_BACKPACK+0
	clickRegion 26*8, 5*8, 16,16, inventoryItemClickHandler, INVENTORY_BACKPACK+1
	clickRegion 23*8, 7*8, 16,16, inventoryItemClickHandler, INVENTORY_BACKPACK+2
	clickRegion 26*8, 7*8, 16,16, inventoryItemClickHandler, INVENTORY_BACKPACK+3
	clickRegion 23*8, 9*8, 16,16, inventoryItemClickHandler, INVENTORY_BACKPACK+4
	clickRegion 26*8, 9*8, 16,16, inventoryItemClickHandler, INVENTORY_BACKPACK+5
	clickRegion 23*8,11*8, 16,16, inventoryItemClickHandler, INVENTORY_BACKPACK+6
	clickRegion 26*8,11*8, 16,16, inventoryItemClickHandler, INVENTORY_BACKPACK+7
	clickRegion 23*8,13*8, 16,16, inventoryItemClickHandler, INVENTORY_BACKPACK+8
	clickRegion 26*8,13*8, 16,16, inventoryItemClickHandler, INVENTORY_BACKPACK+9
	clickRegion 23*8,15*8, 16,16, inventoryItemClickHandler, INVENTORY_BACKPACK+10
	clickRegion 26*8,15*8, 16,16, inventoryItemClickHandler, INVENTORY_BACKPACK+11
	clickRegion 23*8,17*8, 16,16, inventoryItemClickHandler, INVENTORY_BACKPACK+12
	clickRegion 26*8,17*8, 16,16, inventoryItemClickHandler, INVENTORY_BACKPACK+13
	clickRegion 29*8, 5*8, 16,16, inventoryItemClickHandler, INVENTORY_QUIVER
	clickRegion 29*8, 8*8, 16,16, inventoryItemClickHandler, INVENTORY_BREAST_ARMOUR
	clickRegion 29*8,11*8, 16,16, inventoryItemClickHandler, INVENTORY_BRACERS
	clickRegion 37*8, 5*8, 16,16, inventoryItemClickHandler, INVENTORY_HELMET
	clickRegion 37*8, 8*8, 16,16, inventoryItemClickHandler, INVENTORY_NECKLACE
	clickRegion 33*8,19*8, 16,16, inventoryItemClickHandler, INVENTORY_BOOTS
	clickRegion 37*8,11*8, 16,16, inventoryItemClickHandler, INVENTORY_BELT+0
	clickRegion 37*8,13*8, 16,16, inventoryItemClickHandler, INVENTORY_BELT+1
	clickRegion 37*8,15*8, 16,16, inventoryItemClickHandler, INVENTORY_BELT+2
	clickRegion 29*8,14*8, 16,16, inventoryItemClickHandler, INVENTORY_LEFT_RING
	clickRegion 29*8,16*8, 16,16, inventoryItemClickHandler, INVENTORY_RIGHT_RING
.endproc

; x=action. 0=up, 1=down, 2=left, 3=right, ff=none
.proc updatePointerWithKeyboardOnlyControls
		.pushseg
		.segment "BSS"
			_index:	.res 1
			_currentCoordinates: .res 1
		.popseg

		stx TMP2

		; Modify dungeon center
		lda triggerRect
		bmi :+
			lda #1
			sta keyboardOnlyCoordinatesInventory
			sta keyboardOnlyCoordinatesNormal
			; (x+w/2)/2
			lda triggerRect+0 ;x
			asl
			asl
			asl
			sta TMP
			lda triggerRect+2 ;w
			asl
			asl
			adc TMP
			lsr
			sta keyboardOnlyCoordinatesNormal+1
			sta keyboardOnlyCoordinatesInventory+1

			; y+h/2
			lda triggerRect+1 ;y
			asl
			asl
			asl
			sta TMP
			lda triggerRect+3 ;h
			asl
			asl
			adc TMP
			sta keyboardOnlyCoordinatesNormal+2
			sta keyboardOnlyCoordinatesInventory+2
			jmp :++
		:
			lda #0
			sta keyboardOnlyCoordinatesInventory
			sta keyboardOnlyCoordinatesNormal
		:

		lda SWAPPING_MEMBER_INDEX
		bmi notSwapping
			; Disable members not active in the coordinate list
			ldx #5
			:
				lda partyMembers_lo,x
				sta CURRENT_PARTYMEMBER+0
				lda partyMembers_hi,x
				sta CURRENT_PARTYMEMBER+1
				ldy #PartyMember::status
				lda (CURRENT_PARTYMEMBER),y
				ldy mul7,x
				sta keyboardOnlyCoordinatesSwapping+0,y
				dex
			bpl :-
			ldx #<keyboardOnlyCoordinatesSwapping
			ldy #>keyboardOnlyCoordinatesSwapping
			lda #1
			jmp done
		notSwapping:

		lda ASK_FOR_SPELL_RECEIVER_GUI
		beq notSpellReceiver
			; Disable members not active in the coordinate list
			ldx #5
			:
				lda partyMembers_lo,x
				sta CURRENT_PARTYMEMBER+0
				lda partyMembers_hi,x
				sta CURRENT_PARTYMEMBER+1
				ldy #PartyMember::status
				lda (CURRENT_PARTYMEMBER),y
				ldy mul7,x
				sta keyboardOnlyCoordinatesAskForSpellReceiver+0,y
				dex
			bpl :-

			ldx #<keyboardOnlyCoordinatesAskForSpellReceiver
			ldy #>keyboardOnlyCoordinatesAskForSpellReceiver
			lda #2
			jmp done
		notSpellReceiver:

		lda MEMBER_INVENTORY_OPEN
		bmi :+
			ldx #<keyboardOnlyCoordinatesInventory
			ldy #>keyboardOnlyCoordinatesInventory
			lda #3
			jmp done
		:

		lda SPELLBOOK_MEMBER_INDEX
		bmi :+
			ldx #<keyboardOnlyCoordinatesSpellBook
			ldy #>keyboardOnlyCoordinatesSpellBook
			lda #4
			jmp done
		:

		; Disable members not active in the coordinate list
		ldx #5
		:
			lda partyMembers_lo,x
			sta CURRENT_PARTYMEMBER+0
			lda partyMembers_hi,x
			sta CURRENT_PARTYMEMBER+1
			ldy #PartyMember::status
			lda (CURRENT_PARTYMEMBER),y
			pha
			lda mul7,x
			asl
			tay
			pla
			sta keyboardOnlyCoordinatesNormal_members+0,y
			sta keyboardOnlyCoordinatesNormal_members+7,y
			dex
		bpl :-
		ldx #<keyboardOnlyCoordinatesNormal
		ldy #>keyboardOnlyCoordinatesNormal
		lda #0

done:	stx TMP+0
		sty TMP+1

		cmp _currentCoordinates
		beq onSameCoordinateSheet
			ldx _index
			stx TMP2+1 ;old index
			ldx #0
			stx _index
			ldx #$ff
			stx TMP2
			ldy _currentCoordinates ;src coords
			sta _currentCoordinates ;dst coords

			checkToNormal:
			cmp #0
			bne :++
				; going to normal
				cpy #3
				bne :++
					; from inventory 0->3
					lda TMP2+1
					cmp #7*7
					bcs :+
						sta _index
					:
					jmp onSameCoordinateSheet
			:
			cmp #3
			bne :++
				lda #7*7
				sta _index
				; going to inventory
				cpy #0
				bne :++
					; from normal 3->0
					lda TMP2+1
					cmp #7*7
					bcs :+
						sta _index
					:
					jmp onSameCoordinateSheet
			:
		onSameCoordinateSheet:

		lda TMP2
		bmi :+
redo:		clc
			adc #3 ;up/down/left/right
			adc _index
			tay
			jsr lda_TMP_y
			cmp _index
			beq exit
			sta _index
		:

		ldy _index
		jsr lda_TMP_y ;disabled
		bne :+
			lda TMP2
			jmp redo
		:
		iny
		lda #0
		sta currentxpos+1
		jsr lda_TMP_y ;<x
		asl
		sta currentxpos+0
		rol currentxpos+1
		iny
		jsr lda_TMP_y ;<y
		sta currentypos+0
		lda #0
		sta currentypos+1

exit:	rts
mul7:	.repeat 37,I
			.byte I*7
		.endrep

.macro coordinate _enabled, _x, _y, _up, _down, _left, _right
	.byte _enabled, <((_x)/2), <(_y), (_up)*7, (_down)*7, (_left)*7, (_right)*7
.endmacro

.macro clickRegionCoord _enabled, _x, _y, _w, _h, _up, _down, _left, _right
	.byte _enabled, <(((_x)+(_w)/2)/2), <((_y)+(_h)/2), (_up)*7, (_down)*7, (_left)*7, (_right)*7
.endmacro

.pushseg
.segment "HIRAM"
keyboardOnlyCoordinatesSwapping:
	coordinate 1, 184+32, 8+4,	 4,2,1,1
	coordinate 1, 256+32, 8+4,	 5,3,0,0
	coordinate 1, 184+32, 64+4,	 0,4,3,3
	coordinate 1, 256+32, 64+4,	 1,5,2,2
	coordinate 1, 184+32, 120+4, 2,0,5,5
	coordinate 1, 256+32, 120+4, 3,1,4,4

keyboardOnlyCoordinatesAskForSpellReceiver:
	coordinate 1, 184+16,   0+32, 4,2,1,1
	coordinate 1, 256+16,   0+32, 5,3,0,0
	coordinate 1, 184+16,  56+32, 0,4,3,3
	coordinate 1, 256+16,  56+32, 1,5,2,2
	coordinate 1, 184+16, 112+32, 2,0,5,5
	coordinate 1, 256+16, 112+32, 3,1,4,4

keyboardOnlyCoordinatesInventory:
;	trt	00 01 16    19
;	c	02 03 17    
;	c	04 05 17    20
;	trf	06 07 18    22
;	brf	08 09 25    23
;	brf	10 11 26    24
;	brf	12 13 14 21 15
	clickRegionCoord 1, 24,  8, 128,96,  5, 3, 0, 2+7		 ; 0: center wall (overlay)
	clickRegionCoord 1,  0,102,  88,18,  3, 1, 1, 2 		 ; 1: bl floor
	clickRegionCoord 1, 89,102,  88,18,  4, 2, 1, 8+7		 ; 2: br floor
	clickRegionCoord 1,  0, 72,  88,29,  0, 1, 3, 4			 ; 3: tl floor
	clickRegionCoord 1, 89, 72,  88,29,  0, 2, 3, 6+7		 ; 4: tr floor
	clickRegionCoord 1,  0,  8,  88,48,  5, 0, 5, 6 		 ; 5: tl throw
	clickRegionCoord 1, 88,  8,  88,48,  6, 0, 5, 0+7		 ; 6: tr throw
	clickRegionCoord 1,23*8, 5*8, 16,16, 12+7, 2+7, 6  , 1+7 ; 0: INVENTORY_BACKPACK+0
	clickRegionCoord 1,26*8, 5*8, 16,16, 13+7, 3+7, 0+7,16+7 ; 1: INVENTORY_BACKPACK+1
	clickRegionCoord 1,23*8, 7*8, 16,16,  0+7, 4+7, 0  , 3+7 ; 2: INVENTORY_BACKPACK+2
	clickRegionCoord 1,26*8, 7*8, 16,16,  1+7, 5+7, 2+7,17+7 ; 3: INVENTORY_BACKPACK+3
	clickRegionCoord 1,23*8, 9*8, 16,16,  2+7, 6+7, 0  , 5+7 ; 4: INVENTORY_BACKPACK+4
	clickRegionCoord 1,26*8, 9*8, 16,16,  3+7, 7+7, 4+7,17+7 ; 5: INVENTORY_BACKPACK+5
	clickRegionCoord 1,23*8,11*8, 16,16,  4+7, 8+7, 4  , 7+7 ; 6: INVENTORY_BACKPACK+6
	clickRegionCoord 1,26*8,11*8, 16,16,  5+7, 9+7, 6+7,18+7 ; 7: INVENTORY_BACKPACK+7
	clickRegionCoord 1,23*8,13*8, 16,16,  6+7,10+7, 2  , 9+7 ; 8: INVENTORY_BACKPACK+8
	clickRegionCoord 1,26*8,13*8, 16,16,  7+7,11+7, 8+7,25+7 ; 9: INVENTORY_BACKPACK+9
	clickRegionCoord 1,23*8,15*8, 16,16,  8+7,12+7, 2  ,11+7 ; 10:INVENTORY_BACKPACK+10
	clickRegionCoord 1,26*8,15*8, 16,16,  9+7,13+7,10+7,26+7 ; 11:INVENTORY_BACKPACK+11
	clickRegionCoord 1,23*8,17*8, 16,16, 10+7, 0+7, 2  ,13+7 ; 12:INVENTORY_BACKPACK+12
	clickRegionCoord 1,26*8,17*8, 16,16, 11+7, 1+7,12+7,14+7 ; 13:INVENTORY_BACKPACK+13
	clickRegionCoord 1,29*8,19*8, 16,16, 26+7,16+7,13+7,21+7 ; 14:INVENTORY_LEFT_HAND
	clickRegionCoord 1,37*8,19*8, 16,16, 24+7,19+7,21+7,24+7 ; 15:INVENTORY_RIGHT_HAND
	clickRegionCoord 1,29*8, 5*8, 16,16, 14+7,17+7, 1+7,19+7 ; 16:INVENTORY_QUIVER
	clickRegionCoord 1,29*8, 8*8, 16,16, 16+7,18+7, 3+7,20+7 ; 17:INVENTORY_BREAST_ARMOUR
	clickRegionCoord 1,29*8,11*8, 16,16, 17+7,25+7, 7+7,22+7 ; 18:INVENTORY_BRACERS
	clickRegionCoord 1,37*8, 5*8, 16,16, 15+7,20+7,16+7,19+7 ; 19:INVENTORY_HELMET
	clickRegionCoord 1,37*8, 8*8, 16,16, 19+7,22+7,17+7,20+7 ; 20:INVENTORY_NECKLACE
	clickRegionCoord 1,33*8,19*8, 16,16, 21+7,21+7,14+7,15+7 ; 21:INVENTORY_BOOTS
	clickRegionCoord 1,37*8,11*8, 16,16, 20+7,23+7,18+7,22+7 ; 22:INVENTORY_BELT+0
	clickRegionCoord 1,37*8,13*8, 16,16, 22+7,24+7,25+7,23+7 ; 23:INVENTORY_BELT+1
	clickRegionCoord 1,37*8,15*8, 16,16, 23+7,15+7,26+7,24+7 ; 24:INVENTORY_BELT+2
	clickRegionCoord 1,29*8,14*8, 16,16, 18+7,26+7, 9+7,23+7 ; 25:INVENTORY_LEFT_RING
	clickRegionCoord 1,29*8,16*8, 16,16, 25+7,14+7,11+7,24+7 ; 26:INVENTORY_RIGHT_RING

keyboardOnlyCoordinatesSpellBook:
	clickRegionCoord 1, 34*2, 15*8+7*1, 54*2,7, 6,1,0,0 ; selectSpell, 0
	clickRegionCoord 1, 34*2, 15*8+7*2, 54*2,7, 0,2,1,1 ; selectSpell, 1
	clickRegionCoord 1, 34*2, 15*8+7*3, 54*2,7, 1,3,2,2 ; selectSpell, 2
	clickRegionCoord 1, 34*2, 15*8+7*4, 54*2,7, 2,4,3,3 ; selectSpell, 3
	clickRegionCoord 1, 34*2, 15*8+7*5, 54*2,7, 3,5,4,4 ; selectSpell, 4
	clickRegionCoord 1, 34*2, 15*8+7*6, 54*2,7, 4,6,5,5 ; selectSpell, 5
	clickRegionCoord 1, 34*2, 15*8+7*7, 54*2,7, 5,0,6,6 ; abortSpell, 0

keyboardOnlyCoordinatesNormal:
	clickRegionCoord 1, 24,  8, 128,96,  5, 3, 0,11	; 0: center wall (overlay)
	clickRegionCoord 1,  0,102,  88,18,  3, 1, 1, 2 ; 1: bl floor
	clickRegionCoord 1, 89,102,  88,18,  4, 2, 1,12	; 2: br floor
	clickRegionCoord 1,  0, 72,  88,29,  0, 1, 3, 4	; 3: tl floor
	clickRegionCoord 1, 89, 72,  88,29,  0, 2, 3,11	; 4: tr floor
	clickRegionCoord 1,  0,  8,  88,48,  5, 0, 5, 6 ; 5: tl throw
	clickRegionCoord 1, 88,  8,  88,48,  6, 0, 5, 7	; 6: tr throw
keyboardOnlyCoordinatesNormal_members:
	coordinate 1, 216+16, 16 +8, 		 7, 8, 6, 9	; 7: 0 left hand
	coordinate 1, 216+16, 16 +24,		 7,11, 6,10	; 8: 0 right hand
	coordinate 1, 288+16, 16 +8, 		 9,10, 7, 9	; 9: 1 left hand
	coordinate 1, 288+16, 16 +24,		 9,13, 8,10	;10: 1 right hand
	coordinate 1, 216+16, 72 +8, 		 8,12, 4,13	;11: 2 left hand
	coordinate 1, 216+16, 72 +24,		11,15, 2,14	;12: 2 right hand
	coordinate 1, 288+16, 72 +8, 		10,14,11,13	;13: 3 left hand
	coordinate 1, 288+16, 72 +24,		13,17,12,14	;14: 3 right hand
	coordinate 1, 216+16, 128+8, 		12,16, 2,17	;15: 4 left hand
	coordinate 1, 216+16, 128+24,		15,16, 2,18	;16: 4 right hand
	coordinate 1, 288+16, 128+8, 		14,18,15,17	;17: 5 left hand
	coordinate 1, 288+16, 128+24,		17,18,16,18	;18: 5 right hand
.popseg
.endproc

.proc handleClickRegions
		.pushseg
		.segment "BSS"
			_nbrClickRegions:	.res 1
		.popseg

		sta _nbrClickRegions
		stx CUR_REGION+0
		sty CUR_REGION+1
		regionLoop:
			; if mouse_xpos < cur_region.x0 then nextRegion
			ldy #0
			sec
			lda MOUSEEVENT+MouseEvent::xpos+0
			sbc (CUR_REGION),y
			lda MOUSEEVENT+MouseEvent::xpos+1
			iny
			sbc (CUR_REGION),y
			bcc nextRegion

			; if mouse_xpos >= cur_region.x1 then nextRegion
			iny
			sec
			lda MOUSEEVENT+MouseEvent::xpos+0
			sbc (CUR_REGION),y
			lda MOUSEEVENT+MouseEvent::xpos+1
			iny
			sbc (CUR_REGION),y
			bcs nextRegion

			; if mouse_ypos < cur_region.y0 then nextRegion
			iny
			sec
			lda MOUSEEVENT+MouseEvent::ypos
			sbc (CUR_REGION),y
			bcc nextRegion

			; if mouse_ypos >= cur_region.y1 then nextRegion
			iny
			sec
			lda MOUSEEVENT+MouseEvent::ypos
			sbc (CUR_REGION),y
			bcs nextRegion
				; Match
				iny
				lda (CUR_REGION),y
				sta regionHandler+0
				iny
				lda (CUR_REGION),y
				sta regionHandler+1
				iny
				lax (CUR_REGION),y
				jmp (regionHandler)
				.export regionDone
				regionDone:
					bcc :+
						rts
					:
			nextRegion:
			clc
			lda CUR_REGION+0
			adc #9
			sta CUR_REGION+0
			bcc :+
				inc CUR_REGION+1
			:
			dec _nbrClickRegions
		bne regionLoop
		clc
		rts
.endproc

.proc processMouseEvents
		lda MOUSEEVENT+MouseEvent::buttons
		bpl :+
			rts
		:

		lda SWAPPING_MEMBER_INDEX
		bmi :+
			lda #<(.sizeOf(clickRegionsSwapping) / 9)
			ldx #<clickRegionsSwapping
			ldy #>clickRegionsSwapping
			jsr handleClickRegions
			jmp done
		:

		lda ASK_FOR_SPELL_RECEIVER_GUI
		beq :+
			lda #<(.sizeOf(clickRegionsAskForSpellReceiver) / 9)
			ldx #<clickRegionsAskForSpellReceiver
			ldy #>clickRegionsAskForSpellReceiver
			jsr handleClickRegions
			jmp done
		:

		lda MEMBER_INVENTORY_OPEN
		bmi :+
			lda #<(.sizeOf(clickRegionsInventory) / 9)
			ldx #<clickRegionsInventory
			ldy #>clickRegionsInventory
			jsr handleClickRegions
			bcs done
			jmp :++
		:
			lda #<(.sizeOf(clickRegionsNormal) / 9)
			ldx #<clickRegionsNormal
			ldy #>clickRegionsNormal
			jsr handleClickRegions
			bcs done
		:

		lda #<(.sizeOf(clickRegionsDungeon) / 9)
		ldx #<clickRegionsDungeon
		ldy #>clickRegionsDungeon
		jsr handleClickRegions
		bcs done

		lda SPELLBOOK_MEMBER_INDEX
		bmi :+
			lda #<(.sizeOf(clickRegionsSpellBook) / 9)
			ldx #<clickRegionsSpellBook
			ldy #>clickRegionsSpellBook
			jsr handleClickRegions
			bcs done
		:

done:		lda #0
		sta MOUSEEVENT+MouseEvent::xpos+0
		sta MOUSEEVENT+MouseEvent::xpos+1
		sta MOUSEEVENT+MouseEvent::ypos
		lda #$ff
		sta MOUSEEVENT+MouseEvent::buttons
		rts
.endproc

.segment "FONT"
	; 0 11
	; b 01
	; c 00
	; f 10
.export font8
.proc font8
	.byte %00000000 ;0
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00001010
	.byte %00000110
	.byte %00000100
	.byte %00000100

	.byte %00000100 ;1
	.byte %00000100
	.byte %00000100
	.byte %00000100
	.byte %00000100
	.byte %00000100
	.byte %00000100
	.byte %00000100

	.byte %00000100 ;2
	.byte %00000100
	.byte %00000101
	.byte %00000101
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000

	.byte %00000000 ;3
	.byte %00000000
	.byte %01010101
	.byte %01010101
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000

	.byte %00100000 ;4
	.byte %00100000
	.byte %01100000
	.byte %01010000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000

	.byte %00100000 ;5
	.byte %00100000
	.byte %00100000
	.byte %00100000
	.byte %00100000
	.byte %00100000
	.byte %00100000
	.byte %00100000

	.byte %00000000 ;6
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %10100000
	.byte %10100000
	.byte %00100000
	.byte %00100000

	.byte %00000000 ;7
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %10101010
	.byte %10101010
	.byte %00000000
	.byte %00000000

	.byte %00000000 ;8
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000101
	.byte %00001001
	.byte %00001000
	.byte %00001000

	.byte %00001000 ;9
	.byte %00001000
	.byte %00001000
	.byte %00001000
	.byte %00001000
	.byte %00001000
	.byte %00001000
	.byte %00001000

	.byte %00001000 ;a
	.byte %00001000
	.byte %00001010
	.byte %00001010
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000

	.byte %00000000 ;b
	.byte %00000000
	.byte %10101010
	.byte %10101010
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000

	.byte %00010000 ;c
	.byte %00010000
	.byte %10010000
	.byte %10100000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000

	.byte %00010000 ;d
	.byte %00010000
	.byte %00010000
	.byte %00010000
	.byte %00010000
	.byte %00010000
	.byte %00010000
	.byte %00010000

	.byte %00000000 ;e
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %01010000
	.byte %01010000
	.byte %00010000
	.byte %00010000

	.byte %00000000 ;f
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %01010101
	.byte %01010101
	.byte %00000000
	.byte %00000000

	.byte %00011000 ;10
	.byte %00011000
	.byte %00111100
	.byte %00111100
	.byte %01111110
	.byte %01111110
	.byte %11111111
	.byte %11111111

	.byte %11111111 ;11
	.byte %11111111
	.byte %01111110
	.byte %01111110
	.byte %00111100
	.byte %00111100
	.byte %00011000
	.byte %00011000

	.byte %00110000 ;12
	.byte %00111000
	.byte %00111100
	.byte %00111110
	.byte %00111100
	.byte %00111000
	.byte %00110000
	.byte %00000000

helpButtonBracketLeft:
	.byte %00000001
	.byte %00000011
	.byte %00000011
	.byte %00000011
	.byte %00000011
	.byte %00000011
	.byte %00000011
	.byte %00000011

helpButtonBracketRight:
	.byte %10000000
	.byte %11000000
	.byte %11000000
	.byte %11000000
	.byte %11000000
	.byte %11000000
	.byte %11000000
	.byte %11000000
.endproc

	.assert .sizeof(font8) < 31*8, error, "Too many custom chars in font"

	.res 31*8 - .sizeof(font8), $ff

	.incbin "converters/font/font8.bin", 31*8, (128-31)*8

.segment "GUI4"
.export askForSpellReceiver
.proc askForSpellReceiver
		lda MEMBER_INVENTORY_OPEN
		bmi :+
			jsrf showNormal
		:
		lda #1
		sta textColor
		ldx #<txt
		ldy #>txt
		jsr text_writeNullString
		ldx #79
		jsrf audio_playSoundEffect
		inc ASK_FOR_SPELL_RECEIVER_GUI
		rts
txt:		.byte "Cast spell on which character? ",0
.endproc

