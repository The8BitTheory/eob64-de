.include "global.inc"

.segment "BSS"
callStack_lo:	.res 10
callStack_hi:	.res 10

.export triggerRect
triggerRect:	.res 4 ;x,y,w,h

.segment "SCRIPTPARSER"
.proc clickedOnTriggerRect
.if 0
		lda triggerRect+0
		jsr debug_a
		lda triggerRect+1
		jsr debug_a
		lda triggerRect+2
		jsr debug_a
		lda triggerRect+3
		jsr debug_a
		jsrf text_newLine
.endif

		lda triggerRect
		bmi false
		lda MOUSEEVENT+MouseEvent::xpos+1
		bne false

		lda MOUSEEVENT+MouseEvent::xpos+0
		lsr
		lsr
		lsr
		cmp triggerRect+0
		bcc false
		tax
		clc
		lda triggerRect+0
		adc triggerRect+2
		sta TMP
		cpx TMP
		bcs false

		lda MOUSEEVENT+MouseEvent::ypos
		lsr
		lsr
		lsr
		cmp triggerRect+1
		bcc false
		tay
		clc
		lda triggerRect+1
		adc triggerRect+3
		sta TMP
		cpy TMP
		bcs false

		sec
		rts
false:
		clc
		rts
.endproc

.export scriptParser_doHandleWallEvent
.proc scriptParser_doHandleWallEvent
		position = ARGS+0
		direction = ARGS+2

		;direction^=2;
		lda direction
		eor #2
		sta direction

		;MazeBlock mb = GameState.inf.maze.mazeBlocks[position];
		lda position+0
		asl
		sta TMP2+0
		lda position+1
		rol
		asl TMP2+0
		rol
		sta TMP2+1
		clc
		lda TMP2+0
		adc #<maze
		sta TMP2+0
		lda TMP2+1
		adc #>maze
		sta TMP2+1

		;int wmi = mb.walls[direction];
		ldy direction
		lax (TMP2),y

		;int wallEvent = wm.event;
		;if (wallEvent == 0)
		lda wallEventMask,x
		bne :+
			lda #0
			jmp return
		:

		;int triggerBits = mb.triggerBitsAndMonsterCount;
		clc
		lda position+0
		adc #<triggerBitsAndMonsterCount
		sta TMP+0
		lda position+1
		adc #>triggerBitsAndMonsterCount
		sta TMP+1
		ldy #0
		lda (TMP),y
		lsr
		lsr
		lsr
		ora #$e0

		;if ((triggerBits & triggerMask)==0)
		and TRIGGER_MASK
		bne :+
			rts
		:

		;wallEvent--;
		ldy wallEventMask,x
		dey
		;if (wallEvent<0)
		bpl :+
			lda #0
			jmp return
		:

		cpy #10
		bcc :+
			inc $d020
			jmp *-3
		:

		lda wallEventHandlers_lo,y
		sta TMP+0
		lda wallEventHandlers_hi,y
		sta TMP+1
		jmp (TMP)

return:		sta ARGS+0
		rts
wallEventHandlers_lo:
		.byte <doorKnobHandler
		.byte <simpleTouchHandler
		.byte <handleOnHandler
		.byte <handleOffHandler
		.byte <forceDoorHandler
		.byte <failedForceDoorHandler
		.byte <noHandler
		.byte <noHandler
		.byte <noHandler
		.byte <compartmentHandler
wallEventHandlers_hi:
		.byte >doorKnobHandler
		.byte >simpleTouchHandler
		.byte >handleOnHandler
		.byte >handleOffHandler
		.byte >forceDoorHandler
		.byte >failedForceDoorHandler
		.byte >noHandler
		.byte >noHandler
		.byte >noHandler
		.byte >compartmentHandler
.endproc

.macro setRegion x0,y0,x1,y1
		lda #<x0
		sta REGION_X0+0
		lda #>x0
		sta REGION_X0+1
		lda #<x1
		sta REGION_X1+0
		lda #>x1
		sta REGION_X1+1
		lda #y0
		sta REGION_Y0
		lda #y1
		sta REGION_Y1
.endmacro

.proc clickInRegion
		; if mouse_xpos < REGION_X0 then nextRegion
		sec
		lda MOUSEEVENT+MouseEvent::xpos+0
		sbc REGION_X0+0
		lda MOUSEEVENT+MouseEvent::xpos+1
		sbc REGION_X0+1
		bcc false

		; if mouse_xpos >= REGION_X1 then nextRegion
		sec
		lda MOUSEEVENT+MouseEvent::xpos+0
		sbc REGION_X1+0
		lda MOUSEEVENT+MouseEvent::xpos+1
		sbc REGION_X1+1
		bcs false

		; if mouse_ypos < REGION_Y0 then nextRegion
		sec
		lda MOUSEEVENT+MouseEvent::ypos
		sbc REGION_Y0
		bcc false

		; if mouse_ypos >= REGION_Y1 then nextRegion
		sec
		lda MOUSEEVENT+MouseEvent::ypos
		sbc REGION_Y1
		bcs false

		sec
		rts
false:		clc
		rts
.endproc

.proc failedForceDoor
		position = ARGS+0
		direction = ARGS+2
		mazeBlock = TMP2

		setRegion 40,16,136,88
		jsr clickInRegion
		bcc returnFalse

		ldy direction
		lax (mazeBlock),y
		lda wallFlags,x
		and #WALLFLAG_DOORCLOSED
		beq returnFalse

		lda #1
		sta textColor
		ldx #<noPryTxt
		ldy #>noPryTxt
		jsr text_writeNullString
		lda #1
		sta ARGS+0
		rts
returnFalse:
		lda #0
		sta ARGS+0
		rts

noPryTxt:	.byte "No one is able to pry this door open.",$a,0
.endproc

.proc noHandler
		lda #0
		sta ARGS+0
		rts	
.endproc

.proc doorKnobHandler
		position = ARGS+0
		direction = ARGS+2
		mazeBlock = TMP2

		ldy direction
		lax (mazeBlock),y
		lda wallMappingToDoorType,x

		jsr clickedOnTriggerRect
		bcs clickedOnKnob

		lda TRIGGER_MASK
		cmp #TRIGGER_PLAYERUSESWALL
		beq :+
clickedOnKnob:
			lda mazeBlock+0
			sta TMP+0
			lda mazeBlock+1
			sta TMP+1
			ldy direction
			clc
			lda position+0
			adc #<triggerBitsAndMonsterCount
			sta TMP2+0
			lda position+1
			adc #>triggerBitsAndMonsterCount
			sta TMP2+1
			ldx #6
			jsrf audio_playSoundEffect
			ldy #<-1
			jsr toggleDoor
			lda #1
			sta ARGS+0
			rts
		:

		jmp failedForceDoor
wallMappingToDoorType:
	.byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1
.endproc

.proc simpleTouchHandler
		position = ARGS+0
		direction = ARGS+2
		mazeBlock = TMP2

		jsr clickedOnTriggerRect
		bcs :+
			lda #0
			jmp return
		:
		lda TRIGGER_MASK
		sta ARGS+2
		jsr scriptParser_executeTrigger

		lda #1
return:		sta ARGS+0
		rts
.endproc

.proc handleOnHandler
		position = ARGS+0
		direction = ARGS+2
		mazeBlock = TMP2

		jsr clickedOnTriggerRect
		bcs :+
			lda #0
			jmp return
		:

		ldy direction
		lax (TMP2),y
		inx
		txa
		sta (TMP2),y

		inc SHOULDRENDER

		lda TRIGGER_MASK
		sta ARGS+2
		jsr scriptParser_executeTrigger

		lda #1
return:		sta ARGS+0
		rts
.endproc

.proc handleOffHandler
		position = ARGS+0
		direction = ARGS+2
		mazeBlock = TMP2

		jsr clickedOnTriggerRect
		bcs :+
			lda #0
			jmp return
		:

		ldy direction
		lax (TMP2),y
		dex
		txa
		sta (TMP2),y

		inc SHOULDRENDER

		lda TRIGGER_MASK
		sta ARGS+2
		jsr scriptParser_executeTrigger

		lda #1
return:		sta ARGS+0
		rts
.endproc

.proc forceDoorHandler
		position = ARGS+0
		direction = ARGS+2
		mazeBlock = TMP2

		.pushseg
		.segment "BSS"
			strongestMemberIndex: .res 1
			maxStrength: .res 1
		.popseg

		lda TRIGGER_MASK
		and #TRIGGER_PLAYERUSESWALL
		beq :+
			setRegion 40,16,136,88
			jsr clickInRegion
			bcs :+
			jmp returnFalse
		:

		;int strongestMemberIndex = -1;
		lda #<-1
		sta strongestMemberIndex
		lda #0
		sta maxStrength
		ldx #0
		loop:
			lda partyMembers_lo,x
			sta TMP+0
			lda partyMembers_hi,x
			sta TMP+1

			; (GameState.party[i].status&1)==0
			ldy #PartyMember::status
			lda (TMP),y
			and #1
			beq next

			; GameState.party[i].hpCurrent<=0
			ldy #PartyMember::hpCurrent+1
			lda (TMP),y
			bmi next
			dey
			lda (TMP),y
			beq next
				; strongestMemberIndex == -1
				lda strongestMemberIndex
				bmi :+

				; GameState.party[i].strengthCurrent+GameState.party[i].extraStrengthCurrent
				ldy #PartyMember::strengthCurrent
				clc
				lda (TMP),y	
				ldy #PartyMember::extraStrengthCurrent
				adc (TMP),y
				cmp maxStrength
				bcc next
				:
					stx strongestMemberIndex
					sta maxStrength
			next:
			inx
			cpx #6
		bne loop

		;if (strongestMemberIndex==-1)
		lda strongestMemberIndex
		cmp #<-1
		bne :+
			lda #1
			sta textColor
			ldx #<noBody
			ldy #>noBody
			jsr text_writeNullString
			jmp returnTrue
		:

		;Dice dice = new Dice(1,20,0);
		lda #1
		sta DICE_ROLLS
		lda #20
		sta DICE_SIDES
		lda #0
		sta DICE_BASE
		jsrf rollDice

		;forceDoorStrengthModifier[GameState.party[strongestMemberIndex].strengthCurrent]
		ldx strongestMemberIndex
		lda partyMembers_lo,x
		sta TMP+0
		lda partyMembers_hi,x
		sta TMP+1
		ldy #PartyMember::strengthCurrent
		lax (TMP),y
		lda forceDoorStrengthModifier,x

		; dice.roll() > forceDoorStrengthModifier[GameState.party[strongestMemberIndex].strengthCurrent]
		cmp DICE_RESULT
		bcs :+
			lda #1
			sta textColor
			ldx #<partyFail
			ldy #>partyFail
			jsr text_writeNullString
			jmp returnTrue
		:

		lda #1
		sta textColor
		ldx #<theParty
		ldy #>theParty
		jsr text_writeNullString
		clc
		lda TMP+0
		adc #<PartyMember::name
		tax
		lda TMP+1
		adc #>PartyMember::name
		tay
		jsr text_writeNullString
		ldx #<forcesTheDoor
		ldy #>forcesTheDoor
		jsr text_writeNullString
		
		;if (wmi == 0x1e)
		ldy direction
		lda (mazeBlock),y
		cmp #$1e
		bne :+
			lda #8
			sta (mazeBlock),y
			iny
			iny
			tya
			and #3
			tay
			lda #8
			sta (mazeBlock),y
			jmp :++
		:
			lda #18
			sta (mazeBlock),y
			iny
			iny
			tya
			and #3
			tay
			lda #18
			sta (mazeBlock),y
		:

		lda mazeBlock+0
		sta TMP+0
		lda mazeBlock+1
		sta TMP+1
		clc
		lda position+0
		adc #<triggerBitsAndMonsterCount
		sta TMP2+0
		lda position+1
		adc #>triggerBitsAndMonsterCount
		sta TMP2+1
		jsr openDoor

		jmp returnTrue

returnFalse:	lda #0
		beq return
returnTrue:	lda #1
return:		sta ARGS+0
		rts

noBody: .byte "Nobody is able to force the door.",$a,0
theParty: .byte "The party forces the door.",$a,0
forcesTheDoor: .byte " forces the door!",$a,0
partyFail: .byte "The party tries to force the door and fails.",$a,0

forceDoorStrengthModifier: .byte 1,1,2,3,3,4,4,5,5,6,6,7,7,8,8,9,10,11,12,13
.endproc

.proc failedForceDoorHandler
		position = ARGS+0
		direction = ARGS+2
		mazeBlock = TMP2

		jmp failedForceDoor
.endproc

; Didn't implement tick-mode because it's unused in EOB1
.export levelTimerHandler
.proc levelTimerHandler
		; if ((_scriptTimersMode & 2) && _stepsUntilScriptCall && _stepCounter > _stepsUntilScriptCall) {
		lda timerMode
		and #TIMERMODE_STEPS
		bne :+
			rts
		:
		lda stepsUntilScriptCall+0
		ora stepsUntilScriptCall+1
		bne :+
			rts
		:

		sec
		lda stepsUntilScriptCall+0
		sbc stepCounter+0
		lda stepsUntilScriptCall+1
		sbc stepCounter+1
		bmi :+
			rts
		:		

		;_inf->run(0, 0x20);
		lda #0
		sta ARGS+0
		sta ARGS+1
		lda #TRIGGER_CREATEMONSTER
		sta ARGS+2
		jsr scriptParser_executeTrigger

		;_stepCounter = 0;
		lda #0
		sta stepCounter+0
		sta stepCounter+1

		; EoBEngine::updateScriptTimersExtra()
		; int cnt = 0;
		ldy #0
		; for (int i = 1; i < 30; i++)
		ldx #1
		monsterLoop:
			;if (_monsters[i].hitPointsCur <= 0)
			lda monster_hp_hi,x
			bpl :+
				iny
				jmp :++
			:
			ora monster_hp_lo,x
			bne :+
				iny
			:
			inx
			cpx #30
		bne monsterLoop

		;if (!cnt)
		cpy #0
		beq :+
			rts
		:

		;for (int i = 1; i < 30; i++) {    
		ldx #1
		monsterLoop2:
            ;if (getBlockDistance(_monsters[i].block, _currentBlock) > 3) {
			lda partyPosition+0
			sta ARGS+0
			lda partyPosition+1
			sta ARGS+1
			lda monster_position_lo,x
			sta ARGS+2
			lda monster_position_hi,x
			sta ARGS+3
			jsrf gameState_getDistance
			lda ARGS+0
			cmp #3
			bcc :+
				;killMonster(&_monsters[i], true);
				jsrf killMonster
				;break
				rts
			:
			inx
			cpx #30
		bne monsterLoop2
		rts
.endproc

; Clobbers TMP
; Returns in TOP_ITEM
.export getTopItem
.proc getTopItem
		position = ARGS+0

		; TMP = position*2
		lda position+0
		asl
		sta TMP+0
		lda position+1
		rol
		sta TMP+1

		; TMP += topItemPtrs
		clc
		lda TMP+0
		adc #<topItemPtrs
		sta TMP+0
		lda TMP+1
		adc #>topItemPtrs
		sta TMP+1

		; topItem = *TMP
		ldy #0
		lda (TMP),y
		sta TOP_ITEM+0
		iny
		lda (TMP),y
		sta TOP_ITEM+1
		rts
.endproc

.proc compartmentHandler
		position = ARGS+0
		direction = ARGS+2
		mazeBlock = TMP2

		jsr clickedOnTriggerRect
		bcs :+
			lda #0
			jmp return
		:

		; if (gameState.pointerItem==0)
		lda POINTER_ITEM+0
		ora POINTER_ITEM+1
		beq takeFromCompartment
		jmp dropIntoCompartment

		takeFromCompartment:
			lda position+0
			pha
			lda position+1
			pha

			;int itemIndex = [gameState unlinkItem:&(mb->topItemIndex) atSubPosition:8 withItemIndex:-1];
			jsr getTopItem

			lda #COMPARTMENT
			sta ARGS+0
			jsrf items_unlinkItemBySubPos
			ldy #0
			lda TOP_ITEM+0
			sta (TMP),y
			iny
			lda TOP_ITEM+1
			sta (TMP),y

			pla
			sta position+1
			pla
			sta position+0

			;if (itemIndex == 0)
			lda CUR_ITEM+0
			ora CUR_ITEM+1
			bne :+
				jmp returnTrue
			:

			lda CUR_ITEM+0
			pha
			lda CUR_ITEM+1
			pha

			;[gameState.inf.script executeTriggerAtPosition:position usingTriggerMask:TRIGGER_ITEMTAKEN];
			lda #TRIGGER_ITEMTAKEN
			sta ARGS+2
			jsr scriptParser_executeTrigger

			;gameState.pointerItem=itemIndex;
			pla
			sta POINTER_ITEM+1
			pla
			sta POINTER_ITEM+0

			jsrf printTakenMessage
			jsrf updatePointerSprite

			inc SHOULDRENDER

			jmp returnTrue

		dropIntoCompartment:
			;if (GameState.renderer.itemPictureTo3dItemIndex[item.picture]<=14
			ldy #Item::picture
			jsr lda_POINTER_ITEM_y
			tax
			lda itemPictureTo3dItemIndex,x
			cmp #15
			bcs :+
				lda #1
				sta textColor
				ldx #<tooLarge
				ldy #>tooLarge
				jsr text_writeNullString
				jmp returnTrue
			:

			;GameState.linkItem(topItemIndexPtr, position, GameState.pointerItem, 8);
			jsr getTopItem
			lda #COMPARTMENT
			sta ARGS+2
			lda POINTER_ITEM+0
			sta CUR_ITEM+0
			lda POINTER_ITEM+1
			sta CUR_ITEM+1
			jsrf items_linkItem
			ldy #0
			lda TOP_ITEM+0
			sta (TMP),y
			iny
			lda TOP_ITEM+1
			sta (TMP),y			

			;[gameState.inf.script executeTriggerAtPosition:position usingTriggerMask:TRIGGER_ITEMDROPPED];
			lda #TRIGGER_ITEMDROPPED
			sta ARGS+2
			jsr scriptParser_executeTrigger

			;GameState.pointerItem=0;
			lda #0
			sta POINTER_ITEM+0
			sta POINTER_ITEM+1
			jsrf updatePointerSprite
			inc SHOULDRENDER

returnTrue:	lda #1
return:		sta ARGS+0
		rts
tooLarge:	.byte "The item is too large to fit.",$a,0
.endproc

.export scriptParser_executeTrigger
.proc scriptParser_executeTrigger
		position = ARGS+0
		_triggerMask = ARGS+2

		lda #0
		sta CALLSTACK_PTR

		clc
		lda position+0
		sta TRIGGER_POSITION+0
		adc #<triggerIndices
		sta TMP+0
		lda position+1
		sta TRIGGER_POSITION+1
		adc #>triggerIndices
		sta TMP+1

		ldy #0
		lda (TMP),y
		and #$7f
		bne :+
			rts
		:
		tax
		dex

		; Found trigger!
		;triggerBits = mb.triggerBitsAndMonsterCount&0xf8;
		sec
		lda TMP+1
		sbc #>$0400
		sta TMP+1
		lda (TMP),y
		;triggerBits>>=3;
		lsr
		lsr
		lsr
		;triggerBits|=0xe0;
		ora #$e0
		; if ((triggerBits & _triggerMask)==0)
		and _triggerMask
		bne :+
			rts
		:
		lda _triggerMask
		sta TRIGGER_MASK

		jsr levels_setInfPointer
		lda #0
		sta SCRIPT_DONE

		; while(!scriptDone)
		scriptLoop:
			lda INF_POINTER+0
			sta LAST_INF_POINTER+0
			lda INF_POINTER+1
			sta LAST_INF_POINTER+1
			jsr levels_infReadByte

			tay
			sec
			sbc #$e6
			bpl :+
				jmp scriptNoImpl
			:
			tax
			lda scriptFunctions_lo,x
			sta TMP+0
			lda scriptFunctions_hi,x
			sta TMP+1
			jmp (TMP)
scriptContinue:
			lda SCRIPT_DONE
		beq scriptLoop
		rts

scriptNoImpl:	sty ARGS+0
		ldx #<opCodeNotImplemented
		ldy #>opCodeNotImplemented
		jmp error

opCodeNotImplemented: 	.byte "Opcode not implemented: ",0

scriptFunctions_lo:
		.byte <parseEncounter		;e6
		.byte <parseIdentifyItems	;e7
		.byte <parseTurn		;e8
		.byte <parseLauncher		;e9
		.byte <parseNewItem		;ea
		.byte <parseGiveExperience	;eb
		.byte <parseChangeLevel		;ec
		.byte <parseConsume		;ed
		.byte <parseConditional		;ee
		.byte <parseCall		;ef
		.byte <parseReturn		;f0
		.byte <parseEnd			;f1
		.byte <parseJump		;f2
		.byte <parseDamage		;f3
		.byte <parseHeal		;f4
		.byte <parseClearFlag		;f5
		.byte <parseSound		;f6
		.byte <parseSetFlag		;f7
		.byte <parseMessage		;f8
		.byte <parseStealItem		;f9
		.byte <parseTeleport		;fa
		.byte <parseCreateMonster	;fb
		.byte <parseCloseDoor		;fc
		.byte <parseOpenDoor		;fd
		.byte <parseChangeWall		;fe
		.byte <parseSetWall		;ff

scriptFunctions_hi:
		.byte >parseEncounter
		.byte >parseIdentifyItems
		.byte >parseTurn
		.byte >parseLauncher
		.byte >parseNewItem
		.byte >parseGiveExperience
		.byte >parseChangeLevel
		.byte >parseConsume
		.byte >parseConditional
		.byte >parseCall
		.byte >parseReturn
		.byte >parseEnd
		.byte >parseJump
		.byte >parseDamage
		.byte >parseHeal
		.byte >parseClearFlag
		.byte >parseSound
		.byte >parseSetFlag
		.byte >parseMessage
		.byte >parseStealItem
		.byte >parseTeleport
		.byte >parseCreateMonster
		.byte >parseCloseDoor
		.byte >parseOpenDoor
		.byte >parseChangeWall
		.byte >parseSetWall
.endproc

.proc error
		lda #10
		sta textColor
		jsr text_writeNullString
		lda #7
		sta textColor
		jsrf text_writeHexByte
		lda #14
		sta textColor
		ldx #<byteCode
		ldy #>byteCode
		jsr text_writeNullString

		lda #1
		sta textColor
		lda LAST_INF_POINTER+0
		sta INF_POINTER+0
		lda LAST_INF_POINTER+1
		sta INF_POINTER+1
		ldx #34
		stx TMP
		:
			jsr levels_infReadByte
			sta ARGS+0
			jsrf text_writeHexByteNoDollar
			lda textColor
			eor #$e
			sta textColor
			dec TMP
		bne :-

		:
			lda _tick
			lsr
			lsr
			lsr
			and #1
			cmp TMP
			beq :-
			sta TMP
			asl
			sta $d020
		jmp :-
byteCode:	.byte " Byte code:",$a,0
.endproc

.proc parseSetWall
	;int subcode = readByte();
	jsr levels_infReadByte
	cmp #$f7
	bne :++
		;int position = readWord()*4;
		jsr readMazePosition

		;byte to = (byte)readByte();
		jsr levels_infReadByte

		;for (int i=0; i<4; i++)
		;	GameState.inf.maze.mazeBlocks[position].walls[i]=to;
		ldy #3
		:
			sta (TMP),y
			dey
		bpl :-
		jmp checkIfNeedToRepaint
	:
	cmp #$e9
	bne :+
		;int position = readWord()*4;
		jsr readMazePosition

		;int side = readByte();
		jsr levels_infReadByte
		tay

		;byte to = (byte)readByte();
		jsr levels_infReadByte

		;GameState.inf.maze.mazeBlocks[position].walls[side]=to;
		sta (TMP),y
		jmp checkIfNeedToRepaint
	:
	cmp #$ed
	bne :+
		;GameState.partyDirection = readByte();
		jsr levels_infReadByte
		sta partyDirection
		inc shouldUpdateCompass
		jmp scriptParser_executeTrigger::scriptContinue
	:

	jmp scriptParser_executeTrigger::scriptContinue
checkIfNeedToRepaint:
	lda MAZEPOSITION+0
	sta ARGS+0
	lda MAZEPOSITION+1
	sta ARGS+1
	jsrf gameState_isPositionVisible
	jmp scriptParser_executeTrigger::scriptContinue
.endproc

; MazeBlock in TMP
; TriggerBitsAndMonsterCounts in TMP2
; y = direction
.proc toggleDoor
	;if (GameState.doorTimer.active)
	ldx #Timer::active
	lda doorTimer,x
	beq :+++
		;if (GameState.doorTimer.argument != position)
		ldx #Timer::argument
		lda doorTimer,x
		cmp TMP+0
		bne :+
		inx
		lda doorTimer,x
		cmp TMP+1
		beq :++
		:
			rts
		:

		; GameState.doorTimer.parameter *= -1;
		ldx #Timer::parameter
		lda doorTimer,x
		eor #$ff
		clc
		adc #1
		sta doorTimer,x
		rts
	:

	; If no direction is set, then figure out if it is a north-south-door or a west-east-door.
	cpy #<-1
	bne :+
		ldy #0
		lax (TMP),y
		lda wallFlags,x
		and #WALLFLAG_ISDOOR
		bne :+
			iny
	:
	tya
	and #1
	tay

	;int doorMoveDirection = ((wm.flags&WallMapping.PASSPARTY)==0)?1:-1;
	lax (TMP),y
	lda wallFlags,x
	and #WALLFLAG_PASSPARTY
	bne :+
		ldx #1
		jmp :++
	:
		ldx #<-1
	:

	;if (doorMoveDirection == -1)
	cpx #<-1
	bne :+
		;if ((mb.triggerBitsAndMonsterCount&7)!=0)
		ldy #0
		lda (TMP2),y
		and #7
		beq :+
			rts
	:

	;GameState.doorTimer.resetWithParameters(position, doorMoveDirection, true);
	lda #<doorTimer
	sta CURRENT_TIMER+0
	lda #>doorTimer
	sta CURRENT_TIMER+1
	ldy #Timer::argument
	lda TMP+0
	pha
	sta ARGS+0
	sta doorTimer,y
	iny
	lda TMP+1
	pha
	sta ARGS+1
	sta doorTimer,y
	ldy #Timer::parameter
	txa
	pha
	sta doorTimer,y
	ldy #Timer::active
	lda #1
	sta doorTimer,y
	jsrf resetTimer

	;GameState.doorTimer.timerHandler.execute(position);
	jsrf doorTimerHandler

	;GameState.doorTimer.resetWithParameters(position, doorMoveDirection, true);
	ldy #Timer::active
	lda #1
	sta doorTimer,y
	ldy #Timer::parameter
	pla
	sta doorTimer,y
	ldy #Timer::argument+1
	pla
	sta doorTimer,y
	dey
	pla
	sta doorTimer,y
	jsrf resetTimer

	rts
.endproc

; MazeBlock in TMP
; TriggerBitsAndMonsterCounts in TMP2
.proc openDoor
	;if (GameState.doorTimer.active)
	ldx #Timer::active
	lda doorTimer,x
	beq :+++
		;if (GameState.doorTimer.argument != position)
		ldx #Timer::argument
		lda doorTimer,x
		cmp TMP+0
		bne :+
		inx
		lda doorTimer,x
		cmp TMP+1
		beq :++
		:
			rts
		:

		; GameState.doorTimer.parameter = 1;
		ldx #Timer::parameter
		lda #1
		sta doorTimer,x
		rts
	:

	ldy #0
	lax (TMP),y
	lda wallFlags,x
	and #WALLFLAG_ISDOOR
	bne :+
		iny
	:
	tya

	;if ((wm.flags&WallMapping.PASSPARTY)!=0)
	lax (TMP),y
	lda wallFlags,x
	and #WALLFLAG_PASSPARTY
	beq :+
		rts
	:

	;GameState.doorTimer.resetWithParameters(position, 1, true);
	lda #<doorTimer
	sta CURRENT_TIMER+0
	lda #>doorTimer
	sta CURRENT_TIMER+1
	ldy #Timer::argument
	lda TMP+0
	pha
	sta ARGS+0
	sta doorTimer,y
	iny
	lda TMP+1
	pha
	sta ARGS+1
	sta doorTimer,y
	ldy #Timer::parameter
	lda #1
	sta doorTimer,y
	ldy #Timer::active
	sta doorTimer,y
	jsrf resetTimer

	;GameState.doorTimer.timerHandler.execute(position);
	jsrf doorTimerHandler

	;GameState.doorTimer.resetWithParameters(position, 1, true);
	ldy #Timer::active
	lda #1
	sta doorTimer,y
	ldy #Timer::parameter
	sta doorTimer,y
	ldy #Timer::argument+1
	pla
	sta doorTimer,y
	dey
	pla
	sta doorTimer,y
	jsrf resetTimer

	rts
.endproc

; MazeBlock in TMP
; TriggerBitsAndMonsterCounts in TMP2
.proc closeDoor
	;if ((mb.triggerBitsAndMonsterCount&7)!=0)
	ldy #0
	lda (TMP2),y
	and #7
	beq :+
		rts
	:

	;if (GameState.doorTimer.active)
	ldx #Timer::active
	lda doorTimer,x
	beq :+++
		;if (GameState.doorTimer.argument != position)
		ldx #Timer::argument
		lda doorTimer,x
		cmp TMP+0
		bne :+
		inx
		lda doorTimer,x
		cmp TMP+1
		beq :++
		:
			rts
		:

		; GameState.doorTimer.parameter = 1;
		ldx #Timer::parameter
		lda #<-1
		sta doorTimer,x
		rts
	:

	ldy #0
	lax (TMP),y
	lda wallFlags,x
	and #WALLFLAG_ISDOOR
	bne :+
		iny
	:

	;if ((wm.flags&WallMapping.PASSPARTY)==0)
	lax (TMP),y
	lda wallFlags,x
	and #WALLFLAG_PASSPARTY
	bne :+
		rts
	:

	;GameState.doorTimer.resetWithParameters(position, -1, true);
	lda #<doorTimer
	sta CURRENT_TIMER+0
	lda #>doorTimer
	sta CURRENT_TIMER+1
	ldy #Timer::argument
	lda TMP+0
	pha
	sta ARGS+0
	sta doorTimer,y
	iny
	lda TMP+1
	pha
	sta ARGS+1
	sta doorTimer,y
	ldy #Timer::parameter
	lda #<-1
	sta doorTimer,y
	ldy #Timer::active
	lda #1
	sta doorTimer,y
	jsrf resetTimer

	;GameState.doorTimer.timerHandler.execute(position);
	jsrf doorTimerHandler

	;GameState.doorTimer.resetWithParameters(position, -1, true);
	ldy #Timer::active
	lda #1
	sta doorTimer,y
	ldy #Timer::parameter
	lda #<-1
	sta doorTimer,y
	ldy #Timer::argument+1
	pla
	sta doorTimer,y
	dey
	pla
	sta doorTimer,y
	jsrf resetTimer

	rts
.endproc

.proc parseChangeWall
	jsr levels_infReadByte
	cmp #$ea
	bne :+
		jsr readMazePosition
		ldy #<-1
		jsr toggleDoor
		jmp scriptParser_executeTrigger::scriptContinue
	:
	cmp #$f7
	bne :+++
		jsr readMazePosition
		jsr levels_infReadByte
		sta DST
		jsr levels_infReadByte
		sta SRC
		ldy #0
		lda DST
		cmp (TMP),y
		bne :+
			lda SRC
		:
		ldy #3
		:
			sta (TMP),y
			dey
		bpl :-

		lda MAZEPOSITION+0
		sta ARGS+0
		lda MAZEPOSITION+1
		sta ARGS+1
		jsrf gameState_isPositionVisible
	:
	jmp scriptParser_executeTrigger::scriptContinue
.endproc

.proc parseOpenDoor
	jsr readMazePosition
	jsr openDoor
	jmp scriptParser_executeTrigger::scriptContinue
.endproc

.proc parseCloseDoor
	jsr readMazePosition
	jsr closeDoor
	jmp scriptParser_executeTrigger::scriptContinue
.endproc

.proc parseCreateMonster
	.pushseg
	.segment "BSS"
		monsterCreate: .res .sizeof(MonsterCreate)
	.popseg
	monsterCreate_ptr = ARGS+1

	lda #<monsterCreate
	sta monsterCreate_ptr+0
	lda #>monsterCreate
	sta monsterCreate_ptr+1

	jsr levels_infReadByte

	ldy #MonsterCreate::levelType
	jsr levels_infReadByte
	sta (monsterCreate_ptr),y ;levelType
	iny
	jsr levels_infReadByte
	sta (monsterCreate_ptr),y ;position lo
	pha
	iny
	jsr levels_infReadByte
	sta (monsterCreate_ptr),y ;position hi
	pha
	iny
	jsr levels_infReadByte
	sta (monsterCreate_ptr),y ;subpos
	iny
	jsr levels_infReadByte
	sta (monsterCreate_ptr),y ;direction
	iny
	jsr levels_infReadByte
	sta (monsterCreate_ptr),y ;type
	iny
	jsr levels_infReadByte
	sta (monsterCreate_ptr),y ;picture
	iny
	jsr levels_infReadByte
	sta (monsterCreate_ptr),y ;phase
	iny
	jsr levels_infReadByte
	sta (monsterCreate_ptr),y ;pause
	iny
	jsr levels_infReadByte
	sta (monsterCreate_ptr),y ;pocket_item lo
	jsr levels_infReadByte ;skip pocket_item hi
	iny
	jsr levels_infReadByte
	sta (monsterCreate_ptr),y ;weapon lo
	jsr levels_infReadByte ;skip weapon hi

	pla
	sta TMP2+1
	pla
	clc
	adc #<triggerBitsAndMonsterCount
	sta TMP2+0
	lda TMP2+1
	adc #>triggerBitsAndMonsterCount
	sta TMP2+1

	ldy #0
	lda (TMP2),y
	and #7
	beq :+
		jmp scriptParser_executeTrigger::scriptContinue
	:

	ldx #0
	:
		lda monster_hp_hi,x
		bmi slotFound
		bne :+
			lda monster_hp_lo,x
			beq slotFound
		:
		inx
		cpx #30
	bne :--
	jmp scriptParser_executeTrigger::scriptContinue

	slotFound:
	stx ARGS+0
	jsrf monster_createFromMonsterCreate
	jmp scriptParser_executeTrigger::scriptContinue
.endproc

.proc parseTeleport
	mode = TMP

	lda #<-1
	sta SRC+0
	lda #>-1
	sta SRC+1

	;if (mode!=0xf1)
	jsr levels_infReadByte
	sta mode
	cmp #$f1
	beq :+
		;srcPosition = [self readWord];
		jsr levels_infReadByte
		sta SRC+0
		jsr levels_infReadByte
		sta SRC+1
	:

	;int dstPosition = [self readWord];
	jsr levels_infReadByte
	sta DST+0
	jsr levels_infReadByte
	sta DST+1

	lda mode
	cmp #$e8
	bne :+
		; move party to DST
		lda DST+0
		sta ARGS+0
		lda DST+1
		sta ARGS+1
		jsrf gameState_teleportParty
		jmp scriptParser_executeTrigger::scriptContinue
	:
	cmp #$f5
	beq moveItem
		jmp notF5
	moveItem:
		; move item
		lda SRC+0
		sta ARGS+0
		lda SRC+1
		sta ARGS+1
		jsr getTopItem
		lda TOP_ITEM+0
		ora TOP_ITEM+1
		bne :+
			jmp allItemsMoved
		:

		ldy #Item::subpos
		jsr lda_TOP_ITEM_y
		pha

		lda TOP_ITEM+0
		sta ARGS+0
		lda TOP_ITEM+1
		sta ARGS+1
		jsrf items_unlinkItemByItemIndex

		ldy #0
		lda TOP_ITEM+0
		sta (TMP),y
		iny
		lda TOP_ITEM+1
		sta (TMP),y

		lda DST+0
		sta ARGS+0
		lda DST+1
		sta ARGS+1
		jsr getTopItem

		pla
		sta ARGS+2
		jsrf items_linkItem

		ldy #0
		lda TOP_ITEM+0
		sta (TMP),y
		iny
		lda TOP_ITEM+1
		sta (TMP),y
		jmp moveItem

	allItemsMoved:
		lda #<thrown
		sta TMP+0
		lda #>thrown
		sta TMP+1
		:
			ldy #Thrown::active
			lda (TMP),y
			cmp #1
			bne :+
				ldy #Thrown::position+0
				lda (TMP),y
				cmp SRC+0
				bne :+
					iny
					lda (TMP),y
					cmp SRC+1
					bne :+
						lda DST+1
						sta (TMP),y
						dey
						lda DST+0
						sta (TMP),y
			:
			clc
			lda TMP+0
			adc #<.sizeof(Thrown)
			sta TMP+0
			tax
			lda TMP+1
			adc #>.sizeof(Thrown)
			sta TMP+1
			cmp #>(thrown+.sizeof(Thrown)*10)
			bne :--
			cpx #<(thrown+.sizeof(Thrown)*10)
		bne :--

		jmp scriptParser_executeTrigger::scriptContinue
	notF5:

	cmp #$f3
	bne :+++
		; move monsters from SRC to DST
		ldx #30
		:
			lda SRC+0
			pha
			lda SRC+1
			pha
			lda monster_position_lo,x
			cmp SRC+0
			bne :+
			lda monster_position_hi,x
			cmp SRC+1
			bne :+
				lda DST+0
				sta TMP+0
				lda DST+1
				sta TMP+1
				lda monster_subpos,x
				sta TMP2+0
				stx ARGS+0
				jsrf monster_updatePositionAndDirection
				ldx ARGS+0
			:
			pla
			sta SRC+1
			pla
			sta SRC+0
			dex
		bpl :--
	:

	jmp scriptParser_executeTrigger::scriptContinue
.endproc

.proc parseStealItem
	subpos = ARGS+2

	jsr levels_infReadByte
	cmp #<-1
	bne :++
		:jsr rnd
		and #7
		cmp #6
		beq :-
		cmp #7
		beq :-
	:
	tax
	:
		ldy #IS_ACTIVE
		jsrf gameState_getMemberStatus
		bcs :+
		inx
		cpx #6
		bne :-
		ldx #0
	jmp :-
	:
	stx CURRENT_PARTYMEMBER_INDEX

	jsr readMazePosition
	jsr levels_infReadByte
	sta subpos

	:
		jsr rnd
		and #31
		cmp #INVENTORY_SIZE
	bcs :-
	tax
	asl
	clc
	adc #PartyMember::inventory
	tay
	lda #27
	sta COUNT
	lda #0
	sta CUR_ITEM+0
	sta CUR_ITEM+1
	nextSlot:
		lda (CURRENT_PARTYMEMBER),y
		sta CUR_ITEM+0
		iny
		lda (CURRENT_PARTYMEMBER),y
		sta CUR_ITEM+1
		iny
		ora CUR_ITEM+0
		beq :+
			cpx #INVENTORY_QUIVER
			beq :+
				sty YREG
				ldy #Item::picture
				jsr lda_CUR_ITEM_y
				ldy YREG
				tax
				lda itemPictureTo3dItemIndex,x
				cmp #15
				bcs break
		:

		cpy #PartyMember::inventory + INVENTORY_SIZE*2
		bne :+
			ldy #PartyMember::inventory
		:
		inx
		dec COUNT
	bne nextSlot
break:
	lda COUNT
	beq :+
		lda CUR_ITEM+0
		ora CUR_ITEM+1
		beq :+
			lda #0
			dey
			sta (CURRENT_PARTYMEMBER),y
			dey
			sta (CURRENT_PARTYMEMBER),y
			lda MAZEPOSITION+0
			sta ARGS+0
			lda MAZEPOSITION+1
			sta ARGS+1

			jsr getTopItem

			jsrf items_linkItem

			ldy #0
			lda TOP_ITEM+0
			sta (TMP),y
			iny
			lda TOP_ITEM+1
			sta (TMP),y

			ldx CURRENT_PARTYMEMBER_INDEX
			inc partyMemberStatsChanged,x
	:

	jmp scriptParser_executeTrigger::scriptContinue
.endproc

.proc parseMessage
	jsr levels_infReadByte
	sta ARGS+2
	jsr levels_infReadByte
	sta ARGS+0
	jsr levels_infReadByte
	sta ARGS+1
	jsr levels_infReadByte
	sta textColor
	jsr levels_writeString
	jmp scriptParser_executeTrigger::scriptContinue
.endproc

bits_lo:	.byte $01,$02,$04,$08,$10,$20,$40,$80,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
bits_hi:	.byte $00,$00,$00,$00,$00,$00,$00,$00,$01,$02,$04,$08,$10,$20,$40,$80,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
bits_hi2:	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$02,$04,$08,$10,$20,$40,$80,$00,$00,$00,$00,$00,$00,$00,$00
bits_hi3:	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$02,$04,$08,$10,$20,$40,$80

.proc parseSetFlag
	jsr levels_infReadByte
	cmp #$e4
	bne :+
		;byte_30E56 = true;
		jmp scriptParser_executeTrigger::scriptContinue
	:
	cmp #$ef
	bne :+
		jsr levels_infReadByte
		tay
		ldx levelIndex
		dex
		lda levelFlags_lo,x
		ora bits_lo,y
		sta levelFlags_lo,x
		lda levelFlags_hi,x
		ora bits_hi,y
		sta levelFlags_hi,x
		lda levelFlags_hi2,x
		ora bits_hi2,y
		sta levelFlags_hi2,x
		lda levelFlags_hi3,x
		ora bits_hi3,y
		sta levelFlags_hi3,x
		jmp scriptParser_executeTrigger::scriptContinue
	:
	cmp #$f0
	bne :+
		jsr levels_infReadByte
		tay
		lda globalFlags_lo
		ora bits_lo,y
		sta globalFlags_lo
		lda globalFlags_hi
		ora bits_hi,y
		sta globalFlags_hi
		lda globalFlags_hi2
		ora bits_hi2,y
		sta globalFlags_hi2
		lda globalFlags_hi3
		ora bits_hi3,y
		sta globalFlags_hi3
		jmp scriptParser_executeTrigger::scriptContinue
	:
	cmp #$f3
	bne :+
		jsr levels_infReadByte
		pha
		jsr levels_infReadByte
		tax
		pla
		tay
		lda monster_flags,x
		ora bits_lo,y
		sta monster_flags,x
		lda #0
		sta monster_phase,x
		jmp scriptParser_executeTrigger::scriptContinue
	:
	jmp scriptParser_executeTrigger::scriptContinue
.endproc

.proc parseSound
	jsr levels_infReadByte
	tax
	sta ARGS+0
	jsr levels_infReadByte
	sta ARGS+1
	jsr levels_infReadByte
	sta ARGS+2
	ora ARGS+1
	bne :+
		jsrf audio_playSoundEffect
		jmp scriptParser_executeTrigger::scriptContinue
	:
	jsrf audio_playSound
	jmp scriptParser_executeTrigger::scriptContinue
.endproc

.proc parseClearFlag
	jsr levels_infReadByte
	cmp #$ef
	bne :+
		jsr levels_infReadByte
		tay
		ldx levelIndex
		dex
		lda bits_lo,y
		eor #$ff
		and levelFlags_lo,x
		sta levelFlags_lo,x
		lda bits_hi,y
		eor #$ff
		and levelFlags_hi,x
		sta levelFlags_hi,x
		lda bits_hi2,y
		eor #$ff
		and levelFlags_hi2,x
		sta levelFlags_hi2,x
		lda bits_hi3,y
		eor #$ff
		and levelFlags_hi3,x
		sta levelFlags_hi3,x
		jmp scriptParser_executeTrigger::scriptContinue
	:
	cmp #$f0
	bne :+
		jsr levels_infReadByte
		tay
		lda bits_lo,y
		eor #$ff
		and globalFlags_lo
		sta globalFlags_lo
		lda bits_hi,y
		eor #$ff
		and globalFlags_hi
		sta globalFlags_hi
		lda bits_hi2,y
		eor #$ff
		and globalFlags_hi2
		sta globalFlags_hi2
		lda bits_hi3,y
		eor #$ff
		and globalFlags_hi3
		sta globalFlags_hi3
		jmp scriptParser_executeTrigger::scriptContinue
	:
	jmp scriptParser_executeTrigger::scriptContinue
.endproc

.proc parseHeal
	; Not used in EotB I
	jmp scriptParser_executeTrigger::scriptNoImpl
.endproc

.proc readDice
	jsr levels_infReadByte
	sta DICE_ROLLS
	jsr levels_infReadByte
	sta DICE_SIDES
	jsr levels_infReadByte
	sta DICE_BASE
	rts
.endproc

.proc parseDamage
	whom = TMP

	jsr levels_infReadByte
	sta whom
	jsr readDice

	lda whom
	cmp #$ff
	bne damageSingle
		; Damage all
		ldx #0
		:
			stx ARGS+0
			txa
			pha
			lda #RING_OF_FEATHER_FALL
			sta ARGS+1
			jsrf gameState_memberHaveRing
			lsr ARGS+1
			bcs :+
				jsr getDamage
				jsrf gameState_giveMemberDamage
			:
			pla
			tax
			inx
			cpx #6
		bne :--
		jmp scriptParser_executeTrigger::scriptContinue

	damageSingle:
	sta ARGS+0
	jsr getDamage
	jsrf gameState_giveMemberDamage
	jmp scriptParser_executeTrigger::scriptContinue

getDamage:
	lda #0
	sta ARGS+2
	lda DICE_ROLLS
	sta ARGS+1
	lda DICE_SIDES
	beq :+
		jsrf rollDice
		lda DICE_RESULT+0
		sta ARGS+1
		lda DICE_RESULT+1
		sta ARGS+2
		jmp :++
	:
		lda #0
		sta ARGS+1
		sta ARGS+2
	:
	rts
.endproc

.proc parseJump
	jsr levels_infReadByte
	pha
	jsr levels_infReadByte
	sta INF_POINTER+1
	pla
	sta INF_POINTER+0
	jmp scriptParser_executeTrigger::scriptContinue
.endproc

.proc parseEnd
	inc SCRIPT_DONE
	jmp scriptParser_executeTrigger::scriptContinue
.endproc

.proc parseReturn
	dec CALLSTACK_PTR
	ldx CALLSTACK_PTR
	lda callStack_lo,x
	sta INF_POINTER+0
	lda callStack_hi,x
	sta INF_POINTER+1	
	jmp scriptParser_executeTrigger::scriptContinue
.endproc

.proc parseCall
	ldx CALLSTACK_PTR
	clc
	lda INF_POINTER+0
	adc #2
	sta callStack_lo,x
	lda INF_POINTER+1
	adc #0
	sta callStack_hi,x
	inx
	stx CALLSTACK_PTR
	cpx #10
	bne :+
		ldx #<outOfCallStack
		ldy #>outOfCallStack
		jsr text_writeNullString
		inc $d020
		jmp *-3
	:
	jmp parseJump
outOfCallStack:
	.byte "No call stack space left.",0
.endproc

.proc readMazePosition
		jsr levels_infReadByte
		sta TMP+0
		sta TMP2+0
		sta MAZEPOSITION+0
		jsr levels_infReadByte
		sta TMP2+1
		sta MAZEPOSITION+1
		asl TMP+0
		rol
		asl TMP+0
		rol
		sta TMP+1

		;TMP+=maze
		clc
		lda TMP+0
		adc #<maze
		sta TMP+0
		lda TMP+1
		adc #>maze
		sta TMP+1

		;TMP2+=triggerBitsAndMonsterCount
		clc
		lda TMP2+0
		adc #<triggerBitsAndMonsterCount
		sta TMP2+0
		lda TMP2+1
		adc #>triggerBitsAndMonsterCount
		sta TMP2+1
		rts
.endproc

.proc parseConditional
	tsx
	stx SP_ENTRY
	loop:
		;int conditionalToken = readByte();
		jsr levels_infReadByte
		bmi :+
			;0..127 = push value
			pha
			jmp loop
		:

		; Conditional operator
		tay
		sec
		sbc #startOperatorValue
		bpl :+
			inc $d021
			jmp *-3
		:
		tax
		lda op_lo,x
		sta TMP+0
		lda op_hi,x
		sta TMP+1
		jmp (TMP)
	jmp loop

op_containsClass:				;cc
	jmp op_unknown

op_containsAlignment:				;ce
	jmp op_unknown

op_isPartyVisible:				;da
	lda #<party
	sta TMP+0
	lda #>party
	sta TMP+1
	:
		ldy #PartyMember::status
		lda (TMP),y
		beq :+
			ldy #PartyMember::activeSpells+0
			lda (TMP),y
			and #SPELL_INVISIBLE
			bne :+
				lda #0
				pha
				jmp loop			
		:
		clc
		lda TMP+0
		adc #<.sizeof(PartyMember)
		sta TMP+0
		lda TMP+1
		adc #>.sizeof(PartyMember)
		sta TMP+1
		cmp #>(party+6*.sizeof(PartyMember))
		bne :--
		lda TMP+0
		cmp #<(party+6*.sizeof(PartyMember))
	bne :--
	lda #1
	pha
	jmp loop

op_rollDice:					;db
	jsr levels_infReadByte
	sta DICE_ROLLS
	jsr levels_infReadByte
	sta DICE_SIDES
	jsr levels_infReadByte
	sta DICE_BASE
	jsrf rollDice
	lda DICE_RESULT
	pha
	jmp loop

op_hasClass:					;dc
	jsr levels_infReadByte
	sta TMP2
	lda #<party
	sta TMP+0
	lda #>party
	sta TMP+1
	:
		ldy #PartyMember::status
		lda (TMP),y
		beq :+
			ldy #PartyMember::class
			lda (TMP),y
			tax
			lda classProfessions,x
			and TMP2
			beq :+
				lda #1
				pha
				jmp loop			
		:
		clc
		lda TMP+0
		adc #<.sizeof(PartyMember)
		sta TMP+0
		lda TMP+1
		adc #>.sizeof(PartyMember)
		sta TMP+1
		cmp #>(party+6*.sizeof(PartyMember))
		bne :--
		lda TMP+0
		cmp #<(party+6*.sizeof(PartyMember))
	bne :--
	lda #0
	pha
	jmp loop

;classProfessions: .byte 1, 1, 5, 2, 4, 8, 5, 9, 3,11,10,12, 7, 5, 6
classProfessions:
	.byte IS_FIGHTER | 0         | 0         ;MULTICLASS_FIGHTER
	.byte IS_FIGHTER | 0         | 0         ;MULTICLASS_RANGER
	.byte IS_FIGHTER | IS_CLERIC | 0         ;MULTICLASS_PALADIN
	.byte IS_MAGE    | 0         | 0         ;MULTICLASS_MAGE
	.byte IS_CLERIC  | 0         | 0         ;MULTICLASS_CLERIC
	.byte IS_THIEF   | 0         | 0         ;MULTICLASS_THIEF
	.byte IS_FIGHTER | IS_CLERIC | 0         ;MULTICLASS_FIGHTER_CLERIC
	.byte IS_FIGHTER | IS_THIEF  | 0         ;MULTICLASS_FIGHTER_THIEF
	.byte IS_FIGHTER | IS_MAGE   | 0         ;MULTICLASS_FIGHTER_MAGE
	.byte IS_FIGHTER | IS_MAGE   | IS_THIEF  ;MULTICLASS_FIGHTER_MAGE_THIEF
	.byte IS_MAGE    | IS_THIEF  | 0         ;MULTICLASS_THIEF_MAGE
	.byte IS_CLERIC  | IS_THIEF  | 0         ;MULTICLASS_CLERIC_THIEF
	.byte IS_FIGHTER | IS_MAGE   | IS_CLERIC ;MULTICLASS_FIGHTER_CLERIC_MAGE
	.byte IS_FIGHTER | IS_CLERIC | 0         ;MULTICLASS_RANGER_CLERIC
	.byte IS_MAGE    | IS_CLERIC | 0         ;MULTICLASS_CLERIC_MAGE

op_hasRace:					;dd
	jsr levels_infReadByte
	sta TMP2
	lda #<party
	sta TMP+0
	lda #>party
	sta TMP+1
	:
		ldy #PartyMember::status
		lda (TMP),y
		beq :+
			ldy #PartyMember::race
			lda (TMP),y
			lsr
			cmp TMP2
			bne :+
				lda #1
				pha
				jmp loop			
		:
		clc
		lda TMP+0
		adc #<.sizeof(PartyMember)
		sta TMP+0
		lda TMP+1
		adc #>.sizeof(PartyMember)
		sta TMP+1
		cmp #>(party+6*.sizeof(PartyMember))
		bne :--
		lda TMP+0
		cmp #<(party+6*.sizeof(PartyMember))
	bne :--
	lda #0
	pha
	jmp loop

op_pushTriggerMask:				;e0
	lda TRIGGER_MASK
	pha
	jmp loop

op_queryPointerItem:				;e7
	lda POINTER_ITEM+0
	sta TMP+0
	lda POINTER_ITEM+1
	sta TMP+1

	jsr levels_infReadByte
	cmp #$e1
	bne :+
		ldy #Item::type
		jsr lda_TMP_y
		pha
		jmp loop
	:
	cmp #$f5
	bne :+
		lda TMP+0
		pha
		jmp loop
	:
	ldy #Item::value
	jsr lda_TMP_y
	pha
	jmp loop

op_pushWallSide:				;e9
	jsr levels_infReadByte
	pha
	jsr readMazePosition
	pla
	tay
	lda (TMP),y
	pha
	jmp loop

op_pushPartyDirection:				;ed
	lda partyDirection
	pha
	jmp loop

op_pushLevelFlag:				;ef
	jsr levels_infReadByte
	tay
	ldx levelIndex
	dex
	lda bits_lo,y
	and levelFlags_lo,x
	sta TMP
	lda bits_hi,y
	and levelFlags_hi,x
	ora TMP
	sta TMP
	lda bits_hi2,y
	and levelFlags_hi2,x
	ora TMP
	sta TMP
	lda bits_hi3,y
	and levelFlags_hi3,x
	ora TMP
	beq :+
		lda #1
	:
	pha
	jmp loop

op_pushGlobalFlag:				;f0
	jsr levels_infReadByte
	tay
	lda bits_lo,y
	and globalFlags_lo
	sta TMP
	lda bits_hi,y
	and globalFlags_hi
	ora TMP
	sta TMP
	lda bits_hi2,y
	and globalFlags_hi2
	ora TMP
	sta TMP
	lda bits_hi3,y
	and globalFlags_hi3
	ora TMP
	beq :+
		lda #1
	:
	pha
	jmp loop

op_checkPartyPosition:				;f1
	jsr levels_infReadByte
	sta TMP+0
	jsr levels_infReadByte
	sta TMP+1
	ldx #0
	cmp partyPosition+1
	bne :+
	lda TMP+0
	cmp partyPosition+0
	bne :+
		ldx #1
	:
	txa
	pha
	jmp loop

op_countMonstersOnPosition:			;f3
	jsr readMazePosition
	ldy #0
	ldx #0
	lda (TMP2),y
	and #7
	beq :+
		inx
	:
	txa
	pha
	jmp loop

op_countItemsOnPosition:			;f5
	jsr levels_infReadByte
	sta ARGS+2

	jsr levels_infReadByte
	sta TMP+0
	jsr levels_infReadByte
	asl TMP+0
	rol
	sta TMP+1

	clc
	lda TMP+0
	adc #<topItemPtrs
	sta TMP+0
	lda TMP+1
	adc #>topItemPtrs
	sta TMP+1

	ldy #0
	lda (TMP),y
	sta TOP_ITEM+0
	iny
	lda (TMP),y
	sta TOP_ITEM+1

	jsrf items_countItems

	lda ARGS+0
	pha
	jmp loop

op_pushNorthWall:			;f7
	jsr readMazePosition
	ldy #0
	lda (TMP),y
	pha
	jmp loop

op_or:					;f8
	pla
	sta TMP
	pla
	bne pushTrue
	lda TMP
	bne pushTrue
	jmp pushFalse

op_and:					;f9
	pla
	sta TMP
	pla
	beq pushFalse
	lda TMP
	beq pushFalse
	jmp pushTrue

.macro binaryCompare
	pla
	sta TMP
	pla
	cmp TMP
.endmacro

op_gte:	binaryCompare			;fa
	bcs pushTrue
	jmp pushFalse

op_gt:	binaryCompare			;fb
	beq :+
		bcs pushTrue 
	:
	jmp pushFalse

op_lte:	binaryCompare			;fc
	bcc pushTrue
	beq pushTrue
	jmp pushFalse

op_lt:	binaryCompare			;fd
	bcc pushTrue
	jmp pushFalse

op_neq:	binaryCompare			;fe
	bne pushTrue
	jmp pushFalse

op_eq:	binaryCompare			;ff
	bne pushFalse

pushTrue:
	lda #1
	pha
	jmp loop

pushFalse:
	lda #0
	pha
	jmp loop

op_done:
	jsr levels_infReadByte
	sta TMP+0
	jsr levels_infReadByte
	sta TMP+1
	pla
	bne :+
		lda TMP+0
		sta INF_POINTER+0
		lda TMP+1
		sta INF_POINTER+1
	:

	tsx
	cpx SP_ENTRY
	beq :+
		lda #10
		sta textColor
		ldx #<stackCorrupted
		ldy #>stackCorrupted
		jsr text_writeNullString
		tsx
		stx ARGS+0
		lda #7
		sta textColor
		jsrf text_writeHexByte
		ldx #<notEquals
		ldy #>notEquals
		jsr text_writeNullString
		ldx SP_ENTRY
		stx ARGS+0
		ldx #<null
		ldy #>null
		jmp error
	:

	jmp scriptParser_executeTrigger::scriptContinue
stackCorrupted:	.byte "Stack corrupted! ",0
notEquals:	.byte " != ",0
null:		.byte 0

op_unknown:	sty ARGS+0
		ldx #<unknownConditional
		ldy #>unknownConditional
		jmp error
unknownConditional: .byte "Unknown conditional: ",0

startOperatorValue = $cc

op_lo:
.byte <op_containsClass		;cc
.byte <op_unknown		;cd
.byte <op_unknown		;ce
.byte <op_unknown		;cf
.byte <op_unknown		;d0
.byte <op_unknown		;d1
.byte <op_unknown		;d2
.byte <op_unknown		;d3
.byte <op_unknown		;d4
.byte <op_unknown		;d5
.byte <op_unknown		;d6
.byte <op_unknown		;d7
.byte <op_unknown		;d8
.byte <op_unknown		;d9
.byte <op_isPartyVisible	;da(0)
.byte <op_rollDice		;db(1)
.byte <op_hasClass		;dc(2)
.byte <op_hasRace		;dd(3)
.byte <op_unknown		;de
.byte <op_unknown		;df
.byte <op_pushTriggerMask	;e0(6)
.byte <op_unknown		;e1
.byte <op_unknown		;e2
.byte <op_unknown		;e3
.byte <op_unknown		;e4
.byte <op_unknown		;e5
.byte <op_unknown		;e6
.byte <op_queryPointerItem	;e7(13)
.byte <op_unknown		;e8
.byte <op_pushWallSide		;e9(15)
.byte <op_unknown		;ea
.byte <op_unknown		;eb
.byte <op_unknown		;ec
.byte <op_pushPartyDirection	;ed(19)
.byte <op_done			;ee
.byte <op_pushLevelFlag		;ef(21)
.byte <op_pushGlobalFlag	;f0(22)
.byte <op_checkPartyPosition	;f1(23)
.byte <op_unknown	;f2(24)
.byte <op_countMonstersOnPosition	;f3(25)
.byte <op_unknown		;f4
.byte <op_countItemsOnPosition	;f5(27)
.byte <op_unknown		;f6
.byte <op_pushNorthWall		;f7(29)
.byte <op_or			;f8(30)
.byte <op_and			;f9(31)
.byte <op_gte			;fa(32)
.byte <op_gt			;fb(33)
.byte <op_lte			;fc(34)
.byte <op_lt			;fd(35)
.byte <op_neq			;fe(36)
.byte <op_eq			;ff(37)

op_hi:
.byte >op_containsClass		;cc
.byte >op_unknown		;cd
.byte >op_unknown		;ce
.byte >op_unknown		;cf
.byte >op_unknown		;d0
.byte >op_unknown		;d1
.byte >op_unknown		;d2
.byte >op_unknown		;d3
.byte >op_unknown		;d4
.byte >op_unknown		;d5
.byte >op_unknown		;d6
.byte >op_unknown		;d7
.byte >op_unknown		;d8
.byte >op_unknown		;d9
.byte >op_isPartyVisible	;da
.byte >op_rollDice		;db
.byte >op_hasClass		;dc
.byte >op_hasRace		;dd
.byte >op_unknown		;de
.byte >op_unknown		;df
.byte >op_pushTriggerMask	;e0
.byte >op_unknown		;e1
.byte >op_unknown		;e2
.byte >op_unknown		;e3
.byte >op_unknown		;e4
.byte >op_unknown		;e5
.byte >op_unknown		;e6
.byte >op_queryPointerItem	;e7
.byte >op_unknown		;e8
.byte >op_pushWallSide		;e9
.byte >op_unknown		;ea
.byte >op_unknown		;eb
.byte >op_unknown		;ec
.byte >op_pushPartyDirection	;ed
.byte >op_done			;ee
.byte >op_pushLevelFlag		;ef
.byte >op_pushGlobalFlag	;f0
.byte >op_checkPartyPosition	;f1
.byte >op_unknown		;f2
.byte >op_countMonstersOnPosition	;f3
.byte >op_unknown		;f4
.byte >op_countItemsOnPosition	;f5
.byte >op_unknown		;f6
.byte >op_pushNorthWall		;f7
.byte >op_or			;f8
.byte >op_and			;f9
.byte >op_gte			;fa
.byte >op_gt			;fb
.byte >op_lte			;fc
.byte >op_lt			;fd
.byte >op_neq			;fe
.byte >op_eq			;ff
.endproc

.proc parseConsume
	jsr levels_infReadByte
	cmp #$ff
	bne :+
		lda #0
		sta ARGS+0
		lda #<-1
		sta ARGS+1
		jsrf gameState_consumeItem
		jmp scriptParser_executeTrigger::scriptContinue
	:
	cmp #$fe
	bne :+
		lda #<-1
	:
	sta ARGS+2
	jsr levels_infReadByte
	sta ARGS+0
	jsr levels_infReadByte
	sta ARGS+1
	jsrf gameState_consumeItemOnPosition
	jmp scriptParser_executeTrigger::scriptContinue
.endproc

.proc parseChangeLevel
	jsr levels_infReadByte ; Unknown

	jsr levels_infReadByte
	pha

	jsr levels_infReadByte
	sta partyPosition+0
	jsr levels_infReadByte
	sta partyPosition+1

	jsr levels_infReadByte
	cmp #$ff
	beq :+
		sta partyDirection
		inc shouldUpdateCompass
	:

	lda #0
	sta timersEnabled

	jsrf gameState_deactivateMonstersThrownDoors

	pla
	tax
	jsrf loadMaze

	lda partyPosition+0
	sta ARGS+0
	lda partyPosition+1
	sta ARGS+1
	jsrf gameState_teleportParty

	lda #1
	sta timersEnabled

	inc SCRIPT_DONE
	jmp scriptParser_executeTrigger::scriptContinue
.endproc

.proc parseGiveExperience
	jsr levels_infReadByte
	cmp #$e2
	bne :+
		jsr levels_infReadByte
		sta ARGS+0
		jsr levels_infReadByte
		sta ARGS+1
		jsrf gameState_givePartyExperience
	:
	jmp scriptParser_executeTrigger::scriptContinue
.endproc

.proc parseNewItem
	;int baseIndex = readWord();
	jsr levels_infReadByte
	pha
	jsr levels_infReadByte
	pha

	;int position = readWord();
	jsr levels_infReadByte
	sta ARGS+0
	jsr levels_infReadByte
	sta ARGS+1

	;int newItemIndex = GameState.items.createItem(baseIndex);
	pla
	tay
	pla
	tax
	jsrf items_createItemFromBase

	;if (newItemIndex == 0)
	lda CUR_ITEM+0
	ora CUR_ITEM+1
	bne :+
		jsr levels_infReadByte
		jmp scriptParser_executeTrigger::scriptContinue
	:

	;if (position == 0xffff)
	lda ARGS+0
	cmp #$ff
	bne :++
	lda ARGS+1
	cmp #$ff
	bne :++
		lda POINTER_ITEM+0
		ora POINTER_ITEM+1
		bne :+
			lda CUR_ITEM+0
			sta POINTER_ITEM+0
			lda CUR_ITEM+1
			sta POINTER_ITEM+1
			jsrf updatePointerSprite
		:
		jsr levels_infReadByte
		jmp scriptParser_executeTrigger::scriptContinue
	:

	;int subpos = readByte();
	jsr levels_infReadByte	
	sta ARGS+2

	;GameState.linkItem(topItemIndexPtr,position,newItemIndex, subpos);
	jsr getTopItem
	jsrf items_linkItem

	;mb.topItemIndex = topItemIndexPtr[0];
	ldy #0
	lda TOP_ITEM+0
	sta (TMP),y
	iny
	lda TOP_ITEM+1
	sta (TMP),y

	jmp scriptParser_executeTrigger::scriptContinue
.endproc

.proc parseLauncher
	; Read Type
	jsr levels_infReadByte
	pha

	; Read itemOrSpellIndex
	jsr levels_infReadByte
	tax
	jsr levels_infReadByte
	tay

	pla
	cmp #$df
	bne isItem
		jmp isSpell
	isItem:
		jsrf items_createItemFromBase
		lda CUR_ITEM+0
		ora CUR_ITEM+1
		bne :+
			jmp scriptParser_executeTrigger::scriptContinue
		:

		; Caster
		lda #<-1
		sta ARGS+0

		; Item
		lda CUR_ITEM+0
		sta ARGS+1
		lda CUR_ITEM+1
		sta ARGS+2

		; Read position
		jsr levels_infReadByte
		sta ARGS+3
		jsr levels_infReadByte
		sta ARGS+4

		; Read direction
		jsr levels_infReadByte		
		sta ARGS+6
		sta TMP

		; Read facing and with direction calculate subpos
		jsr levels_infReadByte
		asl TMP
		ora TMP
		tax
		lda thrownDirectionAndFacingToSubpos,x
		sta ARGS+5
		
		; Item type
		ldy #Item::type
		jsr lda_CUR_ITEM_y
		sta ARGS+7

		jsrf gameState_throwItem
		lsr ARGS+0
		bcs :+
			ldy #Item::position
			lda #<-1
			jsr sta_CUR_ITEM_y
			iny
			lda #>-1
			jsr sta_CUR_ITEM_y
		:

		jmp scriptParser_executeTrigger::scriptContinue
	isSpell:
		; itemOrSpellIndex
		stx ARGS+1
		sty ARGS+2

		; Caster
		lda #<-1
		sta ARGS+0

		; Read position
		jsr levels_infReadByte
		sta ARGS+2
		jsr levels_infReadByte
		sta ARGS+3

		; Read direction
		jsr levels_infReadByte		
		sta ARGS+5
		sta TMP

		; Read facing and with direction calculate subpos
		jsr levels_infReadByte
		asl TMP
		ora TMP
		tax
		lda thrownDirectionAndFacingToSubpos,x
		sta ARGS+4

		jsrf spells_throwSpell
		jmp scriptParser_executeTrigger::scriptContinue
thrownDirectionAndFacingToSubpos: .byte 2, 3, 0, 2, 1, 0, 3, 1
.endproc

.proc parseTurn
	amount = TMP2
	jsr levels_infReadByte
	pha
	jsr levels_infReadByte
	sta amount
	pla
	cmp #$f1
	bne :+
		; Turn party
		clc
		lda partyDirection
		adc amount
		and #3
		sta partyDirection
		inc shouldUpdateCompass
		inc SHOULDRENDER
		jmp scriptParser_executeTrigger::scriptContinue
	:
	cmp #$f5
	beq :+
		jmp scriptParser_executeTrigger::scriptContinue
	:

	; Turn thrown
	lda #<thrown
	sta TMP+0
	lda #>thrown
	sta TMP+1
	:
		ldy #Thrown::active
		lda (TMP),y
		beq :+
			ldy #Thrown::position+0
			lda (TMP),y
			cmp TRIGGER_POSITION+0
			bne :+
				iny
				lda (TMP),y
				cmp TRIGGER_POSITION+1
				bne :+
					ldy #Thrown::direction
					clc
					lda (TMP),y
					adc amount
					and #3
					sta (TMP),y
		:
		clc
		lda TMP+0
		adc #<.sizeof(Thrown)
		sta TMP+0
		tax
		lda TMP+1
		adc #>.sizeof(Thrown)
		sta TMP+1
		cmp #>(thrown+.sizeof(Thrown)*10)
		bne :--
		cpx #<(thrown+.sizeof(Thrown)*10)
	bne :--
	jmp scriptParser_executeTrigger::scriptContinue
.endproc

.proc parseIdentifyItems
	jsr readMazePosition
	lda MAZEPOSITION+0
	cmp partyPosition+0
	bne notPartyPosition
	lda MAZEPOSITION+1
	cmp partyPosition+1
	bne notPartyPosition
		ldx #0
		nextMember:
			txa
			pha

			ldy #IS_ACTIVE
			jsrf gameState_getMemberStatus
			bcc continue
				ldy #PartyMember::inventory
				nextSlot:
					lda (CURRENT_PARTYMEMBER),y
					sta CUR_ITEM+0
					iny
					lda (CURRENT_PARTYMEMBER),y
					sta CUR_ITEM+1
					iny
					ora CUR_ITEM+0
					beq :+
						sty YREG
						ldy #Item::flags
						jsr lda_CUR_ITEM_y
						ora #ITEM_FLAGS_IDENTIFIED
						jsr sta_CUR_ITEM_y
						ldy YREG
					:
					cpy #PartyMember::inventory+INVENTORY_SIZE*2
				bne nextSlot
			continue:

			ldy #PartyMember::inventory+INVENTORY_QUIVER*2
			lda (CURRENT_PARTYMEMBER),y
			sta TOP_ITEM+0
			iny
			lda (CURRENT_PARTYMEMBER),y
			sta TOP_ITEM+1
			jsrf items_identifyLinkedItems

			pla
			tax
			inx
			cpx #6
		bne nextMember
		jmp scriptParser_executeTrigger::scriptContinue

	notPartyPosition:
		lda MAZEPOSITION+0
		sta ARGS+0
		lda MAZEPOSITION+1
		sta ARGS+1
		jsr getTopItem
		jsrf items_identifyLinkedItems
		jmp scriptParser_executeTrigger::scriptContinue
.endproc

.proc parseEncounter
	jsr levels_infReadByte
	tax
	jsrf npc_runDialogue
	bcc :+
		inc SCRIPT_DONE
	:
	jmp scriptParser_executeTrigger::scriptContinue

.if 0
	pha
	lda #1
	sta textColor
	ldx #<txt
	ldy #>txt
	jsr text_writeNullString
	lda #3
	sta textColor
	pla
	jsrf text_writeHexByte
	jsrf text_newLine

	jmp scriptParser_executeTrigger::scriptContinue
txt:	.byte "Play encounter ",0
.endif
.endproc

