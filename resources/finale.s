.include "global.inc"
.segment "FINALE"

.export finale
.proc finale
		lda #0
		sta timersEnabled
		inc SHOULDRENDER
		jsrf render
		jsrf deathanim
noDeathAnim:
		ldx #SCREEN_MODE_text
		jsrf setGameScreenMode
		lda #$b
		sta $d022
		lda #$f
		sta $d023

		ldx #51
		sec
		jsr writeTextMessage

;		lda #<$1ffe
;		sta scriptFlags+0
;		lda #>$1ffe
;		sta scriptFlags+1

		lda scriptFlags+0
		and #<$1ffe
		cmp #<$1ffe
		bne :+
		lda scriptFlags+1
		and #>$1ffe
		cmp #>$1ffe
		bne :+
			jsr clear
			ldx #<msg
			ldy #>msg
			lda #1
			sta textColor2
			sec
			jsr text_writeNullString2
		:
		clc
		ldx #<screenModeNoop
		ldy #>screenModeNoop
		jsr setScreenMode
		lda #0
		sta $d020
		sta $d011
		sta $d015
		jsrf ending
		jmp restart

msg:
		.byte "Gratulation zur Vollendung aller 12",$a,"Beholder Boni.",$a,$a
		.byte "Die Namen der Helden, die",$a,"das vollbracht haben, sind:"
		.byte 0
pwd:
		.byte "Passwort: ",0
.endproc
.export finaleNoDeathAnim = finale::noDeathAnim

.proc clear
		pha
		txa
		pha
		tya
		pha
		jsrf clearTextScreen
		ldx #8
		jsrf clearTextScreenColors
		pla
		tay
		pla
		tax
		pla
		rts
.endproc

.proc writeTextMessage
		dex
		jsrf text_writeTextMessage
		rts
.endproc

