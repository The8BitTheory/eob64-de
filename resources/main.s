.include "global.inc"

.segment "MAIN"
.export main
.proc main
	jsr systemDetect

	lda #$4c ;jmp abs
	sta INDJMPOP

	lda #0
	sta BANK
	sta screenEnabled
	sta musicEnabled
	sta pointerEnabled
	sta $d011
	sta $d021
	lda #$18
	sta $d016
	lda $dd00
	and #$fc
	sta $dd00
	lda #$38
	sta $d018
	jsrf automapVDC_init

	lda #$ff
	sta KEYEVENT+0
	sta KEYEVENT+1
	sta KEYEVENT+2
	sta MOUSEEVENT+0

	; Initialize PNRG
	lda #$cf
	sta seed

	; Other setup
	ldx #MOUSETYPE_1351_ACC
	jsrf mouse_init
	jsrf keyboard_init
	jsr _audio_init
	jsr install_irq
jmp :+
	jsrf ending
:
    jsrf intro
	clc
	jsr copyFont
	jsrf automapVDC_copyFont

	ldx #MOUSETYPE_1351_ACC
	jsrf mouse_init

	jsr go_mainmenu
	; x=0 Chargen
	; x=1 Load
	; x=2 Default party
;	jsrf directLoadFlashSave
	cpx #0
	bne :+
		jsr go_chargen
		ldx #0
	:
	cpx #2
	bne :+
		ldx #0
	:

	jmp go_game
.endproc

.export reinstallFont
.proc reinstallFont
	clc
	jsr copyFont
	rts
.endproc

.proc install_irq
	jsr initScreenMode

	; Reset tick
	lda #0
	ldx #3
	:
		sta _tick,x
		sta latchTick,x
		dex
	bpl :-

	jsr restore_irq
	cli	
	rts
.endproc

.proc go_chargen
	sec
	jsr copyFont
	jsrf chargen
	rts
.endproc

.proc go_mainmenu
	jsrf drawLogoAtTop
	jsr prepareRuntime
	lda #7
	sta screenModeGame_bgcolor
	jsrf mainmenu_do
	txa
	pha
	sec
	ldx #<screenModeNoop
	ldy #>screenModeNoop
	jsr setScreenMode
	lda #0
	sta $d020
	sta $d011
	sta $d015
	pla
	tax
	rts
.endproc

.proc go_game
	txa
	pha
	jsr prepareRuntime
	pla
	tax

	; Game just created if X=0, so we need to create the party inventory
	bne :+
		jsrf gameStateInit
		jmp :++
	:
		jsrf updatePointerSprite
	:

	jmpf game
.endproc

.proc prepareRuntime
	txa
	pha
	memset __BSS_RUN__, $00, __BSS_SIZE__
	jsr _audio_init
	lda #0
	sta muted
	sta screenModeGame_bgcolor
	sta screenEnabled
	jsrf clearVICSprites
	pla
	tax
	jsr installMemCode
	rts
.endproc

.proc _audio_init
	jsrf audio_init
	rts
.endproc

.export game
.proc game
	lda #1
	ldx #20
	:
		sta affectedSpriteColumns,x
		dex
	bpl :-
	jsrf clearFrameBuffer

	clc
	ldx #<screenModeGameNormal
	ldy #>screenModeGameNormal
	jsr setScreenMode
	jsrf guiInit

	ldx levelIndex
	bne :+
		ldx #1
		jsrf gameState_loadLevel
		jmp :++
	:
		jsrf loadMazeFromSave
		jsrf render
		lda #$ff
		sta screenEnabled
	:

	lda #1
	sta textColor
	ldx #<helpMsg
	ldy #>helpMsg
	jsr text_writeNullString

	lda vdcPresent
	beq :+
		lda #3
		sta textColor
		ldx #<c128Msg
		ldy #>c128Msg
		jsr text_writeNullString
	:

.if 0
	ldx #1
	stx dungeonSpriteColors
	inx
	stx dungeonSpriteColorD025
	inx
	stx dungeonSpriteColorD026
	ldx #0
	:
		txa
		.repeat 10,I
			sta frameBuffer_sprites+I*$100,x
		.endrep
		inx
	bne :-
	ldx #79
	:
		txa
		sta frameBuffer_sprites+10*$100,x
		dex
	bpl :-
	lda #1
	ldx #20
	:
		sta affectedSpriteColumns,x
		dex
	bpl :-
	jsrf showFrameBuffer
.endif

	;jsrf benchmark

	lda #1
	sta timersEnabled
	lda #0
	sta SHOULDRENDER
	sta DIDTELEPORT
	jsrf automapVDCUpdate

	mainLoop:
		lda #POINTERENABLED_ANY
		sta pointerEnabled
		jsr rnd

		lda timersEnabled
		beq :++
			lda SHOULDEVALUATETIMERS
			beq :+
				jsr timer_evaluate
				jsrf shouldCastTurnUndead
				sec
				jsrf checkPartyStatus
			:
			jsrf levelTimerHandler
		:

		jsrf gui_processInputEvents

		lda timersEnabled
		pha
		lda #0
		sta timersEnabled
		jsrf refreshUI
		pla
		sta timersEnabled

		lda SHOULDRENDER
		beq :+
			jsrf render
		:

		lda DIDTELEPORT
		beq :+
			jsrf automapVDCUpdate
			lda #0
			sta DIDTELEPORT
		:

		jsrf refreshCompass
	jmp mainLoop

	helpMsg: .byte "'h' ",126,"ffnet die Hilfe.",$a,0
	c128Msg: .byte "C128 erkannt. F3 aktiviert Turbo (Problematisch mit 1541U2).",$a,0
.endproc

.export waitAnyKey
.proc waitAnyKey
	jsr consumeEvents
	:
		lda KEYEVENT+KeyEvent::keyPressed
		bne consumeEvents
		lda MOUSEEVENT+MouseEvent::buttons
		bpl consumeEvents
	bmi :-

consumeEvents:
	lda #0
	sta KEYEVENT+KeyEvent::keyPressed
	sta KEYEVENT+KeyEvent::normalChar
	sta KEYEVENT+KeyEvent::specialChar
	sta KEYEVENT+KeyEvent::modifiers
	lda #$ff
	sta MOUSEEVENT+MouseEvent::buttons
	rts
.endproc
