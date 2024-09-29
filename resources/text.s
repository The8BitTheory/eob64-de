.include "global.inc"

stringBuffer = scratchArea

.segment "BSS"
.export textColor
textColor:	.res 1
.export textBackground
textBackground:	.res 1
.export textBackgroundAlt
textBackgroundAlt:.res 1
textCol:	.res 1
pendingReturn:	.res 1
pendingReturn3:	.res 1
textCol3:	.res 1

.export textColor2
textColor2:	.res 1
.export textCol2
textCol2:	.res 1
.export textRow2
textRow2:	.res 1
textIndent2:	.res 1
.export textRows2
textRows2: .res 1

.segment "TEXTUTIL"
.export text_init
.proc text_init
		lda #8
		sta textRows2
		lda #0
		sta textCol
		sta pendingReturn
		jsr clearTextScreen
		lda #1
		sta textColor
		sta textColor2
		jsr scrollUp
		jsr scrollUp
		jsr scrollUp
		rts
.endproc

.export text_writeHexByte
.proc text_writeHexByte
		value = ARGS+0

		lda #'$'
		jsr putChar
noDollar:	lda value
		jsr writeHexByte
		rts
.endproc

.export text_writeHexByteNoDollar
text_writeHexByteNoDollar = text_writeHexByte::noDollar

.export htd32
.proc htd32
		HTD_IN = ARGS+0		; 32-bit input
		HTD_OUT = RESULT	; 10-digit output, one digit per ZP-loc

		ldy #0 ;digit position
		:
			; Count digits on position y
			ldx #<-1 ;digit value
			:
				inx
				sec
				lda HTD_IN+0
				sbc table0,y
				sta HTD_IN+0
				lda HTD_IN+1
				sbc table1,y
				sta HTD_IN+1
				lda HTD_IN+2
				sbc table2,y
				sta HTD_IN+2
				lda HTD_IN+3
				sbc table3,y
				sta HTD_IN+3
			bcs :-

			; Rollback underflowed result
			clc
			lda HTD_IN+0
			adc table0,y
			sta HTD_IN+0
			lda HTD_IN+1
			adc table1,y
			sta HTD_IN+1
			lda HTD_IN+2
			adc table2,y
			sta HTD_IN+2
			lda HTD_IN+3
			adc table3,y
			sta HTD_IN+3

			txa
			sta HTD_OUT,y
			iny
			cpy #10
		bne :--

		rts

table0:	.byte <(1000000000>> 0),<(100000000>> 0),<(10000000>> 0),<(1000000>> 0),<(100000>> 0),<(10000>> 0),<(1000>> 0),<(100>> 0),<(10>> 0),<(1>> 0)
table1:	.byte <(1000000000>> 8),<(100000000>> 8),<(10000000>> 8),<(1000000>> 8),<(100000>> 8),<(10000>> 8),<(1000>> 8),<(100>> 8),<(10>> 8),<(1>> 8)
table2:	.byte <(1000000000>>16),<(100000000>>16),<(10000000>>16),<(1000000>>16),<(100000>>16),<(10000>>16),<(1000>>16),<(100>>16),<(10>>16),<(1>>16)
table3:	.byte <(1000000000>>24),<(100000000>>24),<(10000000>>24),<(1000000>>24),<(100000>>24),<(10000>>24),<(1000>>24),<(100>>24),<(10>>24),<(1>>24)
.endproc

.export htd
.proc htd
		HTD_IN = ARGS+0
		HTD_OUT = TMP

		lda #0
		sta HTD_OUT+1
		sta HTD_OUT+0
	        ldx #8
htd1: 		asl HTD_IN
	        rol HTD_OUT
	        rol HTD_OUT+1
	        dex
	        beq htd3
	        lda HTD_OUT
	        and #$0F
	        cmp #5
        	bmi htd2
        	clc
	        lda HTD_OUT
        	adc #3
        	sta HTD_OUT
 htd2: 		lda HTD_OUT
        	cmp #$50
        	bmi htd1
        	clc
        	adc #$30
	        sta HTD_OUT
	        jmp htd1
 htd3:
 		rts	
.endproc

.export text_writeDecimalByte
.proc text_writeDecimalByte
		jsr htd

 		; 100s
 		lda TMP+1
 		and #$0f
 		beq :+
	 		tax
 			lda digits,x
 			jsr putChar
 		:

 		; 10s
 		lda TMP+0
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
 		lda TMP+0
 		and #$0f
 		tax
		lda digits,x
 		jsr putChar

 		rts

digits:		.byte "0123456789"
.endproc

.export text_writeHexShort
.proc text_writeHexShort
		value = ARGS+0

		lda #'$'
		jsr putChar
		lda value+1
		jsr writeHexByte
		lda value+0
		jsr writeHexByte

		rts
.endproc

.export text_newLine
.proc text_newLine
		lda #$0a
		jsr putChar
		rts
.endproc

; Fetch string into RAM since strings most often print a string from another segment than TEXT
.pushseg
.segment "MEMCODE_RO"
.proc fetchNullString
		stx SRC+0
		sty SRC+1
		lda #<stringBuffer
		sta DST+0
		lda #>stringBuffer
		sta DST+1

		ldy #0
		:
			lda (SRC),y
			sta (DST),y
			beq :+
			iny
			bne :-
				inc SRC+1
				inc DST+1
		jmp :-
		:

		rts
.endproc

.export text_writeString
.proc text_writeString
		textPointer = ARGS+0
		length = ARGS+2

		ldy #0
		:
			lda (textPointer),y
			sta stringBuffer,y
			iny
			cpy length
		bne :-
		lda #0
		sta stringBuffer,y

		jsrf _writeNullString
		rts
.endproc

.export text_writeNullString
.proc text_writeNullString
		jsr fetchNullString
		jsrf _writeNullString
		rts
.endproc

.export text_writeNullString2
.proc text_writeNullString2
		php
		jsr fetchNullString
		plp
		jsrf _writeNullString2
		rts
.endproc

.export text_writeNullString2_noWordWrap
.proc text_writeNullString2_noWordWrap
		php
		jsr fetchNullString
		plp
		jsrf _writeNullString2_noWordWrap
		rts
.endproc

.export text_writeNullString3
.proc text_writeNullString3
		jsr fetchNullString
		jsrf _writeNullString3
		rts
.endproc

.popseg

.proc scanToNextWhiteSpace
		ldy #0
		:
			lda (SRC),y
			iny
			cmp #33
		bcs :-
		rts
.endproc

.proc _writeNullString
		lda #<stringBuffer
		sta SRC+0
		lda #>stringBuffer
		sta SRC+1

nextWord:
		jsr scanToNextWhiteSpace
		sty COUNT

		; Does the word fit on the line?
		clc
		tya
		adc textCol
		cmp #40-4
		bcc :+
			; No, line break and print word
			lda #$0a
			jsr putChar
		:

		ldy #0
		:
			lda (SRC),y
			beq done
			jsr putChar
			iny
			cpy COUNT
		bne :-

		clc
		tya
		adc SRC+0
		sta SRC+0
		bcc :+
			inc SRC+1
		:
		jmp nextWord

done:
		rts
.endproc

; pagination boolean in c
; text indent in a
.proc _writeNullString2
		lda textCol2
		sta textIndent2
		lda #0
		rol
		sta TMP3
		lda #<stringBuffer
		sta SRC+0
		lda #>stringBuffer
		sta SRC+1

nextWord:
		jsr scanToNextWhiteSpace
		sty COUNT

		; Does the word fit on the line?
		clc
		tya
		adc textCol2
		cmp #40+1
		bcc :+
			; No, line break and print word
			lda #$0a
			sta CUR_CHAR
			jsr putChar2
		:

		ldy #0
		:
			lda (SRC),y
			beq done
			sta CUR_CHAR
			jsr putChar2
			iny
			cpy COUNT
		bne :-
	
		clc
		tya
		adc SRC+0
		sta SRC+0
		bcc :+
			inc SRC+1
		:
		jmp nextWord

done:
		lda TMP3
		beq :+
			clc
			jsr more
		:
		rts
.endproc

; pagination boolean in c
; text indent in a
.proc _writeNullString2_noWordWrap
		lda textCol2
		sta textIndent2
		lda #0
		rol
		sta TMP3
		lda #<stringBuffer
		sta SRC+0
		lda #>stringBuffer
		sta SRC+1

		ldy #0
		:
			lda (SRC),y
			beq done
			sta CUR_CHAR
			jsr putChar2
			iny
		bne :-

done:
		lda TMP3
		beq :+
			clc
			jsr more
		:
		rts
.endproc

; This is like writeNullString but emits on the text screen
.proc _writeNullString3
		lda #<stringBuffer
		sta SRC+0
		lda #>stringBuffer
		sta SRC+1

		ldy #0
		:
			lda (SRC),y
			beq :+
			sta CUR_CHAR
			jsr putChar3
			iny
			bne :-
			inc SRC+1
		jmp :-
		:
		rts
.endproc

.export text_putChar
.proc text_putChar
		stx CUR_CHAR
		jmp putChar+2
.endproc

; Private functions
.proc writeHexByte
		pha
		lsr
		lsr
		lsr
		lsr
		tax
		lda hexToChar,x
		jsr putChar
		pla
		and #$0f
		tax
		lda hexToChar,x
		jsr putChar
		rts
hexToChar:	.byte "0123456789ABCDEF"
.endproc

.proc putChar
		sta CUR_CHAR
		pha
		txa
		pha
		tya
		pha

		lda pendingReturn
		beq :+
			jsr scrollUp
			lda #0
			sta pendingReturn
		:

		lda CUR_CHAR
		cmp #$0a
		bne :+
			inc pendingReturn
			lda #0
			sta textCol
			jmp return
		:

		ldx textCol
		sta vic_screen+24*40,x
		lda SCREENMODE+0
		cmp #<screenModeGameNormal
		bne :+
		lda SCREENMODE+1
		cmp #>screenModeGameNormal
		bne :+
			lda textColor
			sta $d800+24*40,x
			jmp :++
		:
			lda textColor
			sta d800_backup+(24-15)*40,x
		:
		inx
		cpx #40-6
		bne :+
			inc pendingReturn
			ldx #0
		:
		stx textCol

return:
		pla
		tay
		pla
		tax
		pla
		rts
.endproc

.export putChar2
.proc putChar2
		pha
		txa
		pha
		tya
		pha

		lda CUR_CHAR
		cmp #$0a
		bne :+
			jsr return
			jmp exit
		:
		
		ldy textCol2
		lda CUR_CHAR
		sta (TEXT2_SCREEN),y
		lda textColor2
		sta (TEXT2_D800),y
		iny
		cpy #40
		bne :+
			jsr return
			jmp exit
		:
		sty textCol2

exit:		pla
		tay
		pla
		tax
		pla
		rts

return:		lda textIndent2
		sta textCol2
		ldx textRow2
		inx
		cpx textRows2
		bne :+
			sec
			jsr more
			jsr clearTextScreen
			ldx #8
			jmp clearTextScreenColors
		:
		stx textRow2
setRow:		lda textRow2_lo,x
		sta TEXT2_SCREEN+0
		lda textRow2_hi,x
		sta TEXT2_SCREEN+1
		lda textRow2_d800_lo,x
		sta TEXT2_D800+0
		lda textRow2_d800_hi,x
		sta TEXT2_D800+1
		rts
.endproc

.proc putChar3
		pha
		txa
		pha
		tya
		pha

		lda pendingReturn3
		beq :+
			jsr scrollUp3
			lda #0
			sta pendingReturn3
		:

		lda CUR_CHAR
		cmp #$0a
		bne :+
			inc pendingReturn3
			lda #0
			sta textCol3
			jmp return
		:

		ldx textCol3
		sta vic_screen2+24*40,x
		lda textColor
		sta $d800+24*40,x
		inx
		cpx #40
		bne :+
			inc pendingReturn3
			ldx #0
		:
		stx textCol3

return:
		pla
		tay
		pla
		tax
		pla
		rts
.endproc

; x,y
.proc text_setPosition
.export text_setPosition
	stx textCol2
	sty textRow2
	lda textRow2_lo,y
	sta TEXT2_SCREEN+0
	lda textRow2_hi,y
	sta TEXT2_SCREEN+1
	lda textRow2_d800_lo,y
	sta TEXT2_D800+0
	lda textRow2_d800_hi,y
	sta TEXT2_D800+1
	rts
.endproc

.proc more
		ldx textRows2
		jsr putChar2::setRow
		ldy #39
		ldx #3
		loop:
			bcc :+
				lda moreStr,x
				sta (TEXT2_SCREEN),y
				lda #6
				sta (TEXT2_D800),y
				jmp :++
			:
				lda doneStr,x
				sta (TEXT2_SCREEN),y
				lda #7
				sta (TEXT2_D800),y
			:
			dey
			dex
		bpl loop

		jsrf waitAnyKey

		rts
moreStr:	.byte "MEHR"
doneStr:	.byte " OK "
.endproc

.export clearTextScreen
.proc clearTextScreen
	lda #<(vic_screen2+15*40)
	sta TEXT2_SCREEN+0
	lda #>(vic_screen2+15*40)
	sta TEXT2_SCREEN+1
	lda #<($d800+15*40)
	sta TEXT2_D800+0
	lda #>($d800+15*40)
	sta TEXT2_D800+1
	lda #0
	sta textCol2
	sta textRow2
	sta pendingReturn3
	sta textCol3

	lda #' '
	ldx #0
	:
		sta vic_screen2+15*40,x
		inx
	bne :-
	:
		sta vic_screen2+15*40+256,x
		inx
		cpx #<400
	bne :-

	rts
.endproc

.export clearTextScreenColors
.proc clearTextScreenColors
	txa
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
	rts
.endproc

.export text_drawBox
; x xpos
; y ypos
.proc text_drawBox
	width = ARGS+0
	height = ARGS+1

	stx DST+0
	clc
	lda textRow2_lo,y
	adc DST+0
	sta DST+0
	sta DST2+0
	php
	lda textRow2_hi,y
	adc #0
	sta DST+1
	plp
	lda textRow2_d800_hi,y
	adc #0
	sta DST2+1

	ldy #0
	lda #0
	sta (DST),y
	lda #$18
	sta (DST2),y

	ldx height
	:
		clc
		lda DST+0
		adc #40
		sta DST+0
		sta DST2+0
		bcc :+
			inc DST+1
			inc DST2+1
		:
		dex
		bmi :+
		lda #1
		sta (DST),y
		lda #$18
		sta (DST2),y
	jmp :--
	:

	lda #2
	sta (DST),y
	lda #$18
	sta (DST2),y

	ldx width
	:
		iny
		dex
		bmi :+
		lda #3
		sta (DST),y
		lda #$18
		sta (DST2),y
	jmp :-
	:

	lda #4
	sta (DST),y
	lda #$18
	sta (DST2),y

	ldx height
	:
		sec
		lda DST+0
		sbc #40
		sta DST+0
		sta DST2+0
		bcs :+
			dec DST+1
			dec DST2+1
		:
		dex
		bmi :+
		lda #5
		sta (DST),y
		lda #$18
		sta (DST2),y
	jmp :--
	:

	lda #6
	sta (DST),y
	lda #$18
	sta (DST2),y
	
	ldx width
	:
		dey
		dex
		bmi :+
		lda #7
		sta (DST),y
		lda #$18
		sta (DST2),y
	jmp :-
	:

	ldx height
	clear:
		dex
		bpl :+
			rts
		:

		clc
		lda DST+0
		adc #40
		sta DST+0
		sta DST2+0
		bcc :+
			inc DST+1
			inc DST2+1
		:

		ldy #0
		:
			iny
			cpy width
			beq :+
				bcs clear
			:
			lda #' '
			sta (DST),y
			lda #$8
			sta (DST2),y
		jmp :--
	jmp clear
.endproc

.export text_drawBoxPressed
; x xpos
; y ypos
.proc text_drawBoxPressed
	width = ARGS+0
	height = ARGS+1

	stx DST+0
	clc
	lda textRow2_lo,y
	adc DST+0
	sta DST+0
	sta DST2+0
	php
	lda textRow2_hi,y
	adc #0
	sta DST+1
	plp
	lda textRow2_d800_hi,y
	adc #0
	sta DST2+1

	ldy #0
	lda #8
	sta (DST),y
	lda #$18
	sta (DST2),y

	ldx height
	:
		clc
		lda DST+0
		adc #40
		sta DST+0
		sta DST2+0
		bcc :+
			inc DST+1
			inc DST2+1
		:
		dex
		bmi :+
		lda #9
		sta (DST),y
		lda #$18
		sta (DST2),y
	jmp :--
	:

	lda #10
	sta (DST),y
	lda #$18
	sta (DST2),y

	ldx width
	:
		iny
		dex
		bmi :+
		lda #11
		sta (DST),y
		lda #$18
		sta (DST2),y
	jmp :-
	:

	lda #12
	sta (DST),y
	lda #$18
	sta (DST2),y

	ldx height
	:
		sec
		lda DST+0
		sbc #40
		sta DST+0
		sta DST2+0
		bcs :+
			dec DST+1
			dec DST2+1
		:
		dex
		bmi :+
		lda #13
		sta (DST),y
		lda #$18
		sta (DST2),y
	jmp :--
	:

	lda #14
	sta (DST),y
	lda #$18
	sta (DST2),y
	
	ldx width
	:
		dey
		dex
		bmi :+
		lda #15
		sta (DST),y
		lda #$18
		sta (DST2),y
	jmp :-
	:

	ldx height
	clear:
		dex
		bpl :+
			rts
		:

		clc
		lda DST+0
		adc #40
		sta DST+0
		sta DST2+0
		bcc :+
			inc DST+1
			inc DST2+1
		:

		ldy #0
		:
			iny
			cpy width
			beq :+
				bcs clear
			:
			lda #' '
			sta (DST),y
			lda #8
			sta (DST2),y
		jmp :--
	jmp clear
.endproc

; x xpos
; y ypos
.export text_drawButton
.proc text_drawButton
	width = ARGS+0

	stx DST+0
	clc
	lda textRow2_lo,y
	adc DST+0
	sta DST+0
	lda textRow2_hi,y
	adc #0
	sta DST+1

	; Draw left side
	lda #0
	ldy #0
	sta (DST),y
	lda #1
	ldy #40
	sta (DST),y
	lda #2
	ldy #80
	sta (DST),y

	; Draw top and bottom
	ldx width
	:
		inc DST+0
		bne :+
			inc DST+1
		:
		ldy #0
		lda #7
		sta (DST),y
		lda #3
		ldy #80
		sta (DST),y
		dex
	bne :--

	inc DST+0
	bne :+
		inc DST+1
	:

	; Draw right side
	ldy #0
	lda #6
	sta (DST),y
	ldy #40
	lda #5
	sta (DST),y
	ldy #80
	lda #4
	sta (DST),y

	rts
.endproc

; x xpos
; y ypos
.export text_drawButtonPressed
.proc text_drawButtonPressed
	width = ARGS+0

	stx DST+0
	clc
	lda textRow2_lo,y
	adc DST+0
	sta DST+0
	lda textRow2_hi,y
	adc #0
	sta DST+1

	lda #8
	ldy #0
	sta (DST),y
	lda #9
	ldy #40
	sta (DST),y
	lda #10
	ldy #80
	sta (DST),y

	ldx width
	:
		inc DST+0
		bne :+
			inc DST+1
		:
		ldy #0
		lda #15
		sta (DST),y
		lda #11
		ldy #80
		sta (DST),y
		dex
	bne :--

	inc DST+0
	bne :+
		inc DST+1
	:

	ldy #0
	lda #14
	sta (DST),y
	ldy #40
	lda #13
	sta (DST),y
	ldy #80
	lda #12
	sta (DST),y

	rts
.endproc

textRow2_lo:	.repeat 10,I
			.byte <(vic_screen2+(15+I)*40)
		.endrep
textRow2_hi:	.repeat 10,I
			.byte >(vic_screen2+(15+I)*40)
		.endrep
textRow2_d800_lo:.repeat 10,I
			.byte <($d800+(15+I)*40)
		.endrep
textRow2_d800_hi:.repeat 10,I
			.byte >($d800+(15+I)*40)
		.endrep

.pushseg
.segment "MEMCODE_RO"
.proc scrollUp
		lda SCREENMODE+0
		cmp #<screenModeGameNormal
		bne :++
		lda SCREENMODE+1
		cmp #>screenModeGameNormal
		bne :++
			ldx #39
			:
				lda vic_screen+23*40,x
				sta vic_screen+22*40,x
				lda $d800+23*40,x
				sta $d800+22*40,x
				lda vic_screen+24*40,x
				sta vic_screen+23*40,x
				lda $d800+24*40,x
				sta $d800+23*40,x
				lda #' '
				sta vic_screen+24*40,x
				dex
			bpl :-
			rts
		:
		lda 1
		pha
		lda #$35
		sta 1
		ldx #39
		:
			lda vic_screen+23*40,x
			sta vic_screen+22*40,x
			lda d800_backup+(23-15)*40,x
			sta d800_backup+(22-15)*40,x
			lda vic_screen+24*40,x
			sta vic_screen+23*40,x
			lda d800_backup+(24-15)*40,x
			sta d800_backup+(23-15)*40,x
			lda #' '
			sta vic_screen+24*40,x
			dex
		bpl :-
		pla
		sta 1
		rts		
.endproc

.proc scrollUp3
		lda 1
		pha
		lda #$35
		sta 1
		ldx #39
		:
			lda vic_screen2+23*40,x
			sta vic_screen2+22*40,x
			lda $d800+23*40,x
			sta $d800+22*40,x
			lda vic_screen2+24*40,x
			sta vic_screen2+23*40,x
			lda $d800+24*40,x
			sta $d800+23*40,x
			lda #' '
			sta vic_screen2+24*40,x
			dex
		bpl :-
		pla
		sta 1
		rts		
.endproc
.popseg