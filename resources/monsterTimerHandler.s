.include "global.inc"

.macro debug
	php
	inc textColor
	jsr debug_a
	dec textColor
	plp
.endmacro

.segment "MONSTERTIMERHANDLER"

; x is intact
; return relative octant in ARGS+0
; Called getNextMonsterDirection in ScummVM
.export monsterTimerHandler_getRelativeOctant
.proc monsterTimerHandler_getRelativeOctant
	monsterPosition = ARGS+0
	otherPosition = ARGS+2

	.pushseg
	.segment "BSS"
		monsterX: .res 1
		monsterY: .res 1
		otherX: .res 1
		otherY: .res 1
		dx: .res 1
		adx: .res 1
		dy: .res 1
		ady: .res 1
		ndx: .res 1
		ndy: .res 1
	.popseg

	lda monsterPosition+0
	sta monsterY
	and #$1f
	sta monsterX

	lda otherPosition+0
	sta otherY
	and #$1f
	sta otherX

	ldy #4
	:
		lsr monsterPosition+1
		ror monsterY
		lsr otherPosition+1
		ror otherY
		dey
	bpl :-

	sec
	lda monsterY
	sbc otherY
	sta dy
	bpl :+
		clc
		eor #$ff
		adc #1
	:
	sta ady
	asl dy

	sec
	lda #0
	sbc dy
	sta ndy

	sec
	lda otherX
	sbc monsterX
	sta dx
	bpl :+
		clc
		eor #$ff
		adc #1
	:
	sta adx
	asl dx

	sec
	lda #0
	sbc dx
	sta ndx

	lda #0

	ldy dy
	cpy adx
	sec
	bpl :+
		clc
	:
	rol

	ldy ndy
	cpy adx
	sec
	bpl :+
		clc
	:
	rol

	ldy dx
	cpy ady
	sec
	bpl :+
		clc
	:
	rol

	ldy ndx
	cpy ady
	sec
	bpl :+
		clc
	:
	rol

	tay
	lda relativeOctants,y
	sta ARGS+0

	rts

relativeOctants: .byte <-1, 6, 2, <-1, 4, 5, 3, <-1, 0, 7, 1, <-1, <-1, <-1, <-1, <-1

.endproc

; x = monsterIndex
; CURRENT_PARTYMEMBER
; return boolean in C
.export monsterTimerHandler_willMonsterHit
.proc monsterTimerHandler_willMonsterHit
	txa
	pha

	lda #1
	sta DICE_ROLLS
	lda #20
	sta DICE_SIDES
	lda #0
	sta DICE_BASE
	jsrf rollDice

	lda DICE_RESULT+0
	cmp #20
	beq :++
		ldy #PartyMember::activeSpells+1
		lda (CURRENT_PARTYMEMBER),y
		and #<SPELL_PROTECTION_FROM_EVIL
		beq :+
			sec
			lda DICE_RESULT+0
			sbc #2
			sta DICE_RESULT+0
		:

		ldy #PartyMember::activeSpells+0
		lda (CURRENT_PARTYMEMBER),y
		and #<SPELL_PRAYER
		beq :+
			sec
			lda DICE_RESULT+0
			sbc #1
			sta DICE_RESULT+0
	:

	pla
	tax

	ldy monster_typeIndex,x
	sec
	lda monsterType_tach0,y
	ldy #PartyMember::ac
	sbc (CURRENT_PARTYMEMBER),y
	cmp DICE_RESULT+0
	bcs :+
		sec
		rts
	:

	lda DICE_RESULT+0
	cmp #20
	beq :+
		clc
		rts
	:
	sec
	rts
.endproc

; x = monsterIndex
; x is preserved
.proc checkIfMonsterShouldMoveForward
	distance = TMP+0

	lda monster_phase,x
	cmp #MONSTER_STATE_READYHIT
	bpl return ; >= MONSTER_STATE_READYHIT, MONSTER_STATE_INACTIVE, MONSTER_STATE_LEGWALK, MONSTER_STATE_ISHIT

	lda monster_hp_hi,x
	bmi return
	ora monster_hp_lo,x
	beq return

	lda monster_position_lo,x
	sta ARGS+0
	lda monster_position_hi,x
	sta ARGS+1
	lda partyPosition+0
	sta ARGS+2
	lda partyPosition+1
	sta ARGS+3
	jsrf gameState_getDistance
	lda ARGS+0
	cmp #4
	bcc shouldMove
return: rts

shouldMove:
	sta TMP+1

	lda monster_position_lo,x
	sta ARGS+0
	jsr monsterTimerHandler_getRelativeOctant

	lda monster_direction,x
	asl
	sta TMP

	sec
	lda ARGS+0
	sbc TMP
	sec
	sbc #3
	bpl :+
		clc
		adc #8
	:
	cmp #2
	bpl :+
	lda TMP+1
	cmp #2
	bcc :+
		rts
	:

	lda #MONSTER_STATE_FORWARD
	sta monster_phase,x
	lda partyPosition+0
	sta monster_goalPosition_lo,x
	lda partyPosition+1
	sta monster_goalPosition_hi,x

	rts
.endproc

; x is preserved
.proc setAllMonstersTowardsParty
	txa
	pha

	ldy #<-1
	ldx #0
	:
		lda monster_phase,x
		cmp #MONSTER_STATE_INACTIVE
		bne :+
			lda #MONSTER_STATE_FORWARD
			sta monster_phase,x
			lda partyPosition+0
			sta monster_goalPosition_lo,x
			lda partyPosition+1
			sta monster_goalPosition_hi,x
			txa
			tay
		:
		inx
		cpx #30
	bne :--

	tya
	tax
	bmi :++
		lda monster_typeIndex,x
		cmp #7
		bne :+
			lda #$04
			ora scriptFlags+2
			sta scriptFlags+2
		:
		cmp #12
		bne :+
			lda #$08
			ora scriptFlags+3
			sta scriptFlags+3
	:

	pla
	tax

	rts
.endproc
.export monsterTimerHandler_setAllMonstersTowardsParty = setAllMonstersTowardsParty

; x = monsterIndex
; x is preserved
.proc setMonsterTowardsParty
	lda monster_flags,x
	and #MONSTER_FLAGS_ACTIVE
	beq return

	lda monster_phase,x
	cmp #MONSTER_STATE_ISHIT
	beq return

	cmp #MONSTER_STATE_INACTIVE
	beq setAllMonstersTowardsParty

	lda #MONSTER_STATE_FORWARD
	sta monster_phase,x
	lda partyPosition+0
	sta monster_goalPosition_lo,x
	lda partyPosition+1
	sta monster_goalPosition_hi,x

return:	rts
.endproc

; x = monsterIndex
; bool if reach in c
.proc checkMonsterReach
	lda monster_subpos,x
	cmp #4
	bne :+
		sec
		rts
	:

	lda monster_direction,x
	asl
	asl
	clc
	adc monster_subpos,x
	tay
	lda monsterReachTable,y
	cmp #1
	rts

monsterReachTable:
	.byte 1,1,0,0
	.byte 0,1,0,1
	.byte 0,0,1,1
	.byte 1,0,1,0
.endproc

; x = monster Index
.proc moveMonsterToBetterSubpos
	clc
	lda monster_position_lo,x
	adc #<triggerBitsAndMonsterCount
	sta TMP2+0
	lda monster_position_hi,x
	adc #>triggerBitsAndMonsterCount
	sta TMP2+1
	ldy #0
	lda (TMP2),y
	and #7
	cmp #1
	bne :+
		lda #4
		sta monster_subpos,x
		jmp :++
	:
		lda monster_position_lo,x
		sta ARGS+0
		lda monster_position_hi,x
		sta ARGS+1
		jsr findFreeSubposInDirection
		bmi :+
			sta monster_subpos,x
	:

	lda monster_position_lo,x
	sta ARGS+0
	lda monster_position_hi,x
	sta ARGS+1
	jsrf gameState_isPositionVisible
	rts
.endproc

; x = monster Index / CURRENT_MONSTER_INDEX
.proc performMonsterAttackRPG
	lda monster_subpos,x
	cmp #4
	beq :+
		asl
		asl
		ora monster_direction,x
		tay
		lda alternativOrderTableWhenMonsterNotInCenter,y
		jmp :++
	:
		jsr rnd
		and #1
	:
	beq :+
		lda #6
	:
	sta TMP

	lda partyDirection
	asl
	asl
	ora monster_direction,x
	tay
	clc
	lda monsterAttackTargetOrderIndexTable,y
	adc TMP
	clc
	adc #<monsterAttackTargetOrders
	sta MONSTER_ATTACK_TARGET_ORDER+0
	lda #0
	adc #>monsterAttackTargetOrders
	sta MONSTER_ATTACK_TARGET_ORDER+1

	ldy #0
	memberLoop:
		tya
		pha
		lax (MONSTER_ATTACK_TARGET_ORDER),y
		stx CURRENT_PARTYMEMBER_INDEX
		lda partyMembers_lo,x
		sta CURRENT_PARTYMEMBER+0
		lda partyMembers_hi,x
		sta CURRENT_PARTYMEMBER+1

		ldy #PartyMember::status
		lda (CURRENT_PARTYMEMBER),y
		and #PARTYMEMBER_STATUS_ACTIVE
		beq continue

		ldy #PartyMember::hpCurrent
		lda (CURRENT_PARTYMEMBER),y
		cmp #<-10
		bne :+
		iny
		lda (CURRENT_PARTYMEMBER),y
		cmp #>-10
		beq continue
		:

		ldy #PartyMember::activeSpells+0
		lda (CURRENT_PARTYMEMBER),y
		and #SPELL_INVISIBLE
		beq :+
			dice 1,20,0
			jsrf rollDice
			lda DICE_RESULT+0
			cmp #5
			bcs continue
		:
			pla
			jmp foundTarget
	
		continue:
		pla
		tay
		iny
		cpy #6
		beq noneAttacked
	jmp memberLoop
noneAttacked:
	rts

foundTarget:
	lda #0
	sta DAMAGE+0
	sta DAMAGE+1

	ldx CURRENT_MONSTER_INDEX
	ldy monster_typeIndex,x
	ldx #0
	lda monsterType_attackCount,y
	bne attackLoop
		jmp noAttacks
	attackLoop:
		stx COUNT

		ldx CURRENT_MONSTER_INDEX
		jsrf monsterTimerHandler_willMonsterHit
		bcs monsterDidHit
			jmp continue2
		monsterDidHit:

		ldy monster_typeIndex,x
		ldx COUNT
		cpx #0
		bne :+
			lda monsterType_attackDice0Rolls,y
			sta DICE_ROLLS
			lda monsterType_attackDice0Sides,y
			sta DICE_SIDES
			lda monsterType_attackDice0Base,y
			sta DICE_BASE
			jmp rollAttackDice
		:
		cpx #1
		bne :+
			lda monsterType_attackDice1Rolls,y
			sta DICE_ROLLS
			lda monsterType_attackDice1Sides,y
			sta DICE_SIDES
			lda monsterType_attackDice1Base,y
			sta DICE_BASE
			jmp rollAttackDice
		:
		lda monsterType_attackDice2Rolls,y
		sta DICE_ROLLS
		lda monsterType_attackDice2Sides,y
		sta DICE_SIDES
		lda monsterType_attackDice2Base,y
		sta DICE_BASE

		rollAttackDice:
		jsrf rollDice
		clc
		lda DAMAGE+0
		adc DICE_RESULT+0
		sta DAMAGE+0
		lda DAMAGE+1
		adc DICE_RESULT+1
		sta DAMAGE+1

		jsrf gameState_checkMemberAttackSaves
		bcc :+
			lda #0
			sta DAMAGE+0
			sta DAMAGE+1
		:

		continue2:
		ldx CURRENT_MONSTER_INDEX
		ldy monster_typeIndex,x
		ldx COUNT
		inx
		txa
		cmp monsterType_attackCount,y
		beq :+
	jmp attackLoop
	:

noAttacks:
	lda DAMAGE+0
	ora DAMAGE+1
	bne :+
		rts
	:

	ldx CURRENT_MONSTER_INDEX
	ldy monster_typeIndex,x
	lda monsterType_typeBits_lo,y
	and #MONSTER_TYPEBITS_LO_STEAL
	bne :+
		jmp noStealing
	:
		jsr rnd
		and #3
		cmp #3
		bne :+
			jmp noStealing
		:
			dice 1,27,0
			jsrf rollDice
			ldx DICE_RESULT+0
			dex
			txa
			asl
			adc #PartyMember::inventory
			tay
			lda #27
			sta COUNT
			itemLoop:
				lda (CURRENT_PARTYMEMBER),y
				sta CUR_ITEM+0
				iny
				lda (CURRENT_PARTYMEMBER),y
				sta CUR_ITEM+1
				iny
				ora CUR_ITEM+0
				bne tryStealItem
					jmp continue3
				tryStealItem:
					sty YREG
					ldy #Item::type
					jsr lda_CUR_ITEM_y
					tay
					jsr items_fetchItemType
					lda itemType+ItemType::usage
					ldy YREG
					and #$80
					bne stealItem
						jmp continue3
					stealItem:
						lda #0
						dey
						sta (CURRENT_PARTYMEMBER),y
						dey
						sta (CURRENT_PARTYMEMBER),y

						lda #1
						sta textColor
						jsrf printMemberName

						ldy #PartyMember::race
						lda (CURRENT_PARTYMEMBER),y
						and #1
						beq :+
							ldx #<her
							ldy #>her
							jmp :++
						:
							ldx #<his
							ldy #>his
						:
						jsr text_writeNullString

						lda #0
						sta ARGS+0
						jsrf printItemName

						ldx #<ret
						ldy #>ret
						jsr text_writeNullString

						jmp noStealing
						her: .byte " has lost her ",0
						his: .byte " has lost his ",0
						ret: .byte ".",$a,0
				continue3:
				cpy #(PartyMember::inventory+27*2)
				bne :+
					ldy #PartyMember::inventory
				:
				dec COUNT
			beq noStealing
			jmp itemLoop
	noStealing:

	lda CURRENT_PARTYMEMBER_INDEX
	sta ARGS+0
	lda DAMAGE+0
	sta ARGS+1
	lda DAMAGE+1
	sta ARGS+2
	jsrf gameState_giveMemberDamage

	ldx CURRENT_MONSTER_INDEX
	ldy monster_typeIndex,x
	lda monsterType_typeBits_lo,y
	and #MONSTER_TYPEBITS_LO_POISONUS
	beq :+
		ldx CURRENT_PARTYMEMBER_INDEX
		lda #PARTYMEMBER_STATUS_POISONED
		sta ARGS+0
		lda #<poisoned
		sta ARGS+1
		lda #>poisoned
		sta ARGS+2
		lda #0
		sta ARGS+3
		lda #1
		sta ARGS+4
		lda #PMTA_POISONED_CHARACTER
		sta ARGS+5
		sec
		jsrf gameState_setMemberStatus
	:

	ldx CURRENT_MONSTER_INDEX
	ldy monster_typeIndex,x
	lda monsterType_typeBits_lo,y
	and #MONSTER_TYPEBITS_LO_MINDBLAST
	beq :+
		ldx CURRENT_PARTYMEMBER_INDEX
		lda #PARTYMEMBER_STATUS_PARALYZED
		sta ARGS+0
		lda #<paralyzed
		sta ARGS+1
		lda #>paralyzed
		sta ARGS+2
		lda #2
		sta ARGS+3
		lda #5
		sta ARGS+4
		lda #PMTA_RESET_PARALYZED
		sta ARGS+5
		sec
		jsrf gameState_setMemberStatus
	:

	rts
.pushseg
.segment "GAMESTATE"
	poisoned:.byte "poisoned",0
	paralyzed:.byte "paralyzed",0
.popseg

monsterAttackTargetOrderIndexTable:
	.byte 2*12, 3*12, 0*12, 1*12
	.byte 1*12, 2*12, 3*12, 0*12
	.byte 0*12, 1*12, 2*12, 3*12
	.byte 3*12, 0*12, 1*12, 2*12

alternativOrderTableWhenMonsterNotInCenter: ;Transposed for efficiency
	.byte 0, 0, 1, 1
	.byte 1, 0, 0, 1
	.byte 0, 1, 1, 0
	.byte 1, 1, 0, 0

monsterAttackTargetOrders:
	.byte 1, 0, 3, 2, 5, 4
	.byte 0, 1, 2, 3, 4, 5
	.byte 5, 3, 1, 4, 2, 0
	.byte 1, 3, 5, 0, 2, 4
	.byte 4, 5, 2, 3, 0, 1
	.byte 5, 4, 3, 2, 1, 0
	.byte 0, 2, 4, 1, 3, 5
	.byte 4, 2, 0, 5, 3, 1
.endproc

; x = monster Index
; bool if attacked in c
.proc monsterTryAttack
	position = ARGS+0

	stx CURRENT_MONSTER_INDEX

	lda position+0
	cmp #<-1
	bne :+
		lda position+1
		cmp #>-1
		bne :+
			lda monster_position_lo,x
			sta ARGS+0
			lda monster_position_hi,x
			sta ARGS+1
			lda monster_direction,x
			sta ARGS+2
			jsrf gameState_getForwardPosition
	:

	lda position+0
	cmp partyPosition+0
	bne :+
	lda position+1
	cmp partyPosition+1
	bne :+
		jmp :++
	:
		clc
		rts
	:

	ldx CURRENT_MONSTER_INDEX
	jsr checkMonsterReach
	bcs :+
		jsr moveMonsterToBetterSubpos
		sec
		rts
	:

	lda monster_flags,x
	eor #MONSTER_FLAGS_FLIP
	sta monster_flags,x
	bne :+
		sec
		rts
	:

	lda monster_position_lo,x
	sta ARGS+0
	lda monster_position_hi,x
	sta ARGS+1
	jsrf gameState_isPositionVisible
	php
	ldx CURRENT_MONSTER_INDEX
	bcs monsterVisible
		jmp monsterNotVisible
	monsterVisible:
		lda #0
		sta timersEnabled

		ldy monster_typeIndex,x
		cpy #4
		bne :++
			lda monsterType_attackSound,y
			bmi :+
				sta ARGS+0
				jsrf audio_playSoundAtPartyPosition
			:
			ldx CURRENT_MONSTER_INDEX
		:

		lda #<MONSTER_STATE_PREPARE_ATTACK
		sta monster_state,x
		inc SHOULDRENDER
		; renderPause 0
		txa
		pha
		jsrf render
		pla
		tax
		stx CURRENT_MONSTER_INDEX

		lda #<MONSTER_STATE_ATTACK
		sta monster_state,x

		ldy monster_typeIndex,x
		cpy #4
		beq :++
			lda monsterType_attackSound,y
			bmi :+
				sta ARGS+0
				jsrf audio_playSoundAtPartyPosition
			:
			ldx CURRENT_MONSTER_INDEX
		:

		inc SHOULDRENDER
		; renderPause 8
		txa
		pha
		jsrf render
		pla
		tax
		stx CURRENT_MONSTER_INDEX

		jmp attackCompleted
	monsterNotVisible:
		ldy monster_typeIndex,x
		lda monsterType_attackSound,y
		bmi :+
			sta ARGS+0
			jsrf audio_playSoundAtPartyPosition
		:
		ldx CURRENT_MONSTER_INDEX
	attackCompleted:

	jsr performMonsterAttackRPG
	ldx CURRENT_MONSTER_INDEX

	plp
	bcs :+
		sec
		rts
	:

	lda #MONSTER_STATE_FORWARD
	sta monster_state,x
	lda monster_legState,x
	eor #1
	sta monster_legState,x
	inc SHOULDRENDER
	; renderPause 8
	lda #1
	sta timersEnabled

	sec
	rts
.endproc

; x = monsterIndex / CURRENT_MONSTER_INDEX
; returns action taken in c
.proc doDistanceActionTowardsParty
	ldy monster_typeIndex,x
	lda monsterType_typeBits_lo,y
	and #MONSTER_TYPEBITS_LO_RANGEATTACK
	bne :+
returnFalse:	clc
		rts
	:

	lda monster_thCount,x
	beq returnFalse

	lda monster_ticksUntilMove,x
	beq :+
		sec
		sbc #1
		sta monster_ticksUntilMove,x
		jmp returnFalse
	:

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
	cmp #4
	bcs returnFalse

	lda monster_position_lo,x
	sta ARGS+0
	lda monster_position_hi,x
	sta ARGS+1
	lda partyPosition+0
	sta ARGS+2
	lda partyPosition+1
	sta ARGS+3
	jsr monsterTimerHandler_getRelativeOctant
	sta TMP
	lda monster_direction,x
	asl
	cmp TMP
	bne returnFalse

	lda monster_position_lo,x
	sta ARGS+0
	lda monster_position_hi,x
	sta ARGS+1
	lda monster_direction,x
	sta ARGS+2
	checkLineOfSight:
		lda ARGS+0
		cmp partyPosition+0
		bne :+
			lda ARGS+1
			cmp partyPosition+1
			beq freeLineOfSight
		:
		jsrf gameState_getForwardPosition

		lda ARGS+0
		sta TMP+0
		lda ARGS+1
		asl TMP+0
		rol
		asl TMP+0
		rol
		and #$0f
		sta TMP+1
		clc
		lda #<maze
		adc TMP+0
		sta TMP+0
		lda #>maze
		adc TMP+1
		sta TMP+1

		lda ARGS+2
		eor #2
		tay
		lax (TMP),y
		lda wallFlags,x
		and #(WALLFLAG_PASSPARTY|WALLFLAG_PASSSMALL)
		bne :+
			jmp returnFalse
		:

		clc
		lda ARGS+0
		adc #<triggerBitsAndMonsterCount
		sta TMP+0
		lda ARGS+1
		adc #>triggerBitsAndMonsterCount
		sta TMP+1
		ldy #0
		lda (TMP),y
		and #7
		beq :+
			jmp returnFalse
		:
	jmp checkLineOfSight

freeLineOfSight:
	ldx CURRENT_MONSTER_INDEX
	lda monster_typeIndex,x
	cmp #4
	bne :+
		; Kuotoa - Throw spell 9
		ldy #9
		jsrf spells_throwSpellFromMonster
		lda #$1f
		sta ARGS+0
		lda monster_position_lo,x
		sta ARGS+1
		lda monster_position_hi,x
		sta ARGS+2
		jsrf audio_playSound
		jmp break
	:
	cmp #14
	bne :+
		; Mage - Throw mage spells (6,5,2,2,0,0,0,0). A mage has a thcount of 8. The following tables shows what spells they cast at each thcount-tick.
		ldy monster_thCount,x
		lda monsterMageSpells,y
		tay
		jsrf spells_throwSpellFromMonster
		ldy monster_thCount,x
		lda monsterMageSounds,y
		sta ARGS+0
		lda monster_position_lo,x
		sta ARGS+1
		lda monster_position_hi,x
		sta ARGS+2
		jsrf audio_playSound
		jmp break
		monsterMageSpells: .byte 0,  0,  0,  0,  0,  2,  2,  5,  6
		monsterMageSounds: .byte 0, 85, 85, 85, 85, 98, 98, 31, 98
	:
	cmp #15
	bne :++
		; Drider - Throw spear
		ldx #<$003c
		ldy #>$003c
		jsrf items_createItemFromBase
		lda CUR_ITEM+0
		ora CUR_ITEM+1
		beq :+
			lda #<-1
			sta ARGS+0
			lda CUR_ITEM+0
			sta ARGS+1
			lda CUR_ITEM+1
			sta ARGS+2
			ldx CURRENT_MONSTER_INDEX
			lda monster_position_lo,x
			sta ARGS+3
			lda monster_position_hi,x
			sta ARGS+4
			lda monster_subpos,x
			sta ARGS+5
			lda monster_direction,x
			sta ARGS+6
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
		jmp break
	:
	cmp #16
	bne :+
		; Kenku - Throw spell 0
		ldy #0
		jsrf spells_throwSpellFromMonster
		lda #$55
		sta ARGS+0
		lda monster_position_lo,x
		sta ARGS+1
		lda monster_position_hi,x
		sta ARGS+2
		jsrf audio_playSound
		jmp break
	:
	cmp #17
	beq :+
		jmp :++
	:
		; Mindflayer - Mindblast
		lda #$53
		sta ARGS+0
		lda monster_position_lo,x
		sta ARGS+1
		lda monster_position_hi,x
		sta ARGS+2
		jsrf audio_playSound
		lda #1
		sta textColor
		ldx #<mindBlast
		ldy #>mindBlast
		jsr text_writeNullString

		ldx #0
		loop:
			lda #PARTYMEMBER_STATUS_PARALYZED
			sta ARGS+0
			lda #<paralyzed
			sta ARGS+1
			lda #>paralyzed
			sta ARGS+2
			lda #1
			sta ARGS+3
			lda #5
			sta ARGS+4
			lda #PMTA_RESET_PARALYZED
			sta ARGS+5
			sec
			jsrf gameState_setMemberStatus
			ldx CURRENT_PARTYMEMBER_INDEX
			inx
			cpx #6
		bne loop

		jmp break
		.pushseg
		.segment "GAMESTATE"
		paralyzed:.byte "paralyzed",0
		.popseg
		mindBlast:.byte "The party is hit with a psychic mind blast!",$a,0
	:
	cmp #21
	beq :+
		jmp break
	:
		; Xanathar
		jsr rnd
		and #3
		cmp #3
		bcs :+
			pha
			txa
			pha
			tay
			lda monsterMageSounds,y
			sta ARGS+0
			lda monster_position_lo,x
			sta ARGS+1
			lda monster_position_hi,x
			sta ARGS+2
			jsrf audio_playSound
			pla
			tax
			pla
			tay
			lda monsterXanatharSpells,y
			tay
			jsrf spells_throwSpellFromMonster
			jmp break
		:
		ldx #0
		loop2:
			ldy #IS_ACTIVE|IS_ALIVE
			stx CURRENT_PARTYMEMBER_INDEX
			jsrf gameState_getMemberStatus
			bcc :+
				lda #1
				sta textColor
				jsrf printMemberName
				ldx #<causeSerious
				ldy #>causeSerious
				jsr text_writeNullString
				dice 2,8,1
				jsrf rollDice
				ldx CURRENT_PARTYMEMBER_INDEX
				stx ARGS+0
				lda DICE_RESULT+0
				sta ARGS+1
				lda DICE_RESULT+1
				sta ARGS+2
				jsrf gameState_giveMemberDamage
			:
			ldx CURRENT_PARTYMEMBER_INDEX
			inx
			cpx #6
			beq break
		jmp loop2
		causeSerious: .byte " is hit by a cause serious wounds spell!",$a,0
		monsterXanatharSpells: .byte 2,10,11
		monsterXanatharSpellSounds: .byte 98, 83, 64

break:
	inc SHOULDRENDER
	ldx CURRENT_MONSTER_INDEX
	lda monster_thCount,x
	cmp #<-1
	beq :+
		sec
		sbc #1
		sta monster_thCount,x
	:
	lda #5
	sta monster_ticksUntilMove,x

	sec
	rts
.endproc


.proc moveMonsterForward
	.pushseg
	.segment "BSS"
		moveMonsterFlipFlop: .res 1
		relativeOctant: .res 1
		di: .res 1
		si: .res 1
	.popseg

	jsr doDistanceActionTowardsParty
	bcc :+
		rts
	:

	lda monster_position_lo,x
	sta ARGS+0
	lda monster_position_hi,x
	sta ARGS+1
	lda monster_direction,x
	sta ARGS+2
	jsrf gameState_getForwardPosition
	ldx CURRENT_MONSTER_INDEX
	jsr monsterTryAttack
	bcc :+
		rts
	:

	lda #MONSTER_STATE_FORWARD
	sta monster_state,x

	;moveMonsterTowardsPosition(monster, monster.goalPosition)
	lda monster_turnDirection,x
	cmp #6+1 ;TODO: Double check this. See ScummVM sprites_eob.cpp:1070
	bmi :+
		lda #0
		sta monster_turnDirection,x
		lda moveMonsterFlipFlop
		eor #1
		sta moveMonsterFlipFlop
	:

	lda moveMonsterFlipFlop
	bne :+
		lda #<monsterTurnTable0
		sta TMP3+0
		lda #>monsterTurnTable0
		sta TMP3+1
		jmp :++
	:
		lda #<monsterTurnTable1
		sta TMP3+0
		lda #>monsterTurnTable1
		sta TMP3+1
	:

	lda monster_direction,x
	asl
	sta si

	lda monster_position_lo,x
	sta ARGS+0
	lda monster_position_hi,x
	sta ARGS+1
	lda monster_goalPosition_lo,x
	sta ARGS+2
	lda monster_goalPosition_hi,x
	sta ARGS+3
	jsr monsterTimerHandler_getRelativeOctant
	lda ARGS+0
	cmp #<-1
	bne :+
		rts
	:

	sta relativeOctant

	lda monster_flags,x
	and #MONSTER_FLAGS_TURNED
	beq :+
		lda relativeOctant
		eor #4
		sta relativeOctant
	:
	lda relativeOctant

	sec
	sbc si
	and #7
	sta di

	lda relativeOctant
	and #1
	bne needToAlignWithParty
		lda relativeOctant
		lsr
		tay
		clc
		lda monster_position_lo,x
		adc positionDirectionDeltas_lo,y
		sta TMP+0
		lda monster_position_hi,x
		adc positionDirectionDeltas_hi,y
		and #>$3ff
		sta TMP+1

		lda TMP+0
		cmp partyPosition+0
		bne needToAlignWithParty
		lda TMP+1
		cmp partyPosition+1
		bne needToAlignWithParty
			lda di
			beq :++
				cmp #5
				bmi :+
					ldy monster_direction,x
					dey
					sty si
					jmp :++
				:
				ldy monster_direction,x
				iny
				sty si
			:
			
			lda si
			and #3
			tay
			lda #<-1
			sta ARGS+0
			lda #>-1
			sta ARGS+1
			jmp tryMoveMonster
	needToAlignWithParty:

	lda di
	beq diZero
		lda relativeOctant
		and #1
		beq evenOctant
			lda di
			cmp #5
			bmi :+
				dec si
				jmp boundSi
			:
				inc si
				jmp boundSi
		evenOctant:
			lda di
			cmp #5
			bmi :+
				dec si
				dec si
				jmp boundSi
			:
				inc si
				inc si
		boundSi:
		lda si
		and #7
		sta si
	diZero:

	ldy #7

	loop:
		lda #0
		sta ARGS+0
		sta ARGS+1

		lda (TMP3),y
		clc
		adc si
		and #7
		sta si

		and #1
		bne :+
			lda monster_position_lo,x
			sta ARGS+0
			lda monster_position_hi,x
			sta ARGS+1
			lda si
			lsr
			sta ARGS+2
			jsrf gameState_getForwardPosition
			ldx CURRENT_MONSTER_INDEX
		:

		lda ARGS+0
		ora ARGS+1
		beq continue
			tya
			pha
			lda si
			lsr
			tay
			jsr tryMoveMonster
			pla
			tay
			bcs return
		continue:
		dey
	bpl loop
return:

	; Direct attack after move for some monster types
	ldx CURRENT_MONSTER_INDEX
	ldy monster_typeIndex,x
	lda monsterType_typeBits_lo,y
	and #MONSTER_TYPEBITS_LO_TWOATTACKS
	beq :+
		lda #<-1
		sta ARGS+0
		lda #>-1
		sta ARGS+1
		jsr monsterTryAttack
	:

	rts

monsterTurnTable0: .byte < 7, <-6, < 5, <-4, < 3, <-2, < 1, < 0 
monsterTurnTable1: .byte <-7, < 6, <-5, < 4, <-3, < 2, <-1, < 0
positionDirectionDeltas_lo: .byte <-32,<1,<32,<-1
positionDirectionDeltas_hi: .byte >-32,>1,>32,>-1
.endproc

; x = monster
; y = direction
; returns success in c
.proc tryMoveMonster
	newPosition = ARGS+0

	.pushseg
	.segment "BSS"
		oldPosition: .res 2
		direction: .res 1
	.popseg

	lda monster_position_lo,x
	sta oldPosition+0
	lda monster_position_hi,x
	sta oldPosition+1
	sty direction
	stx CURRENT_MONSTER_INDEX

	lda newPosition+0
	cmp #<-1
	bne :+
	lda newPosition+1
	cmp #>-1
	beq toSkip
	:
	cmp oldPosition+1
	bne :+
	lda newPosition+0
	cmp oldPosition+0
	bne :+
		toSkip: jmp skip
	:
		lda newPosition+0
		asl
		sta TMP2+0
		lda newPosition+1
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

		lda direction
		cmp #<-1
		bne :+
			lda monster_direction,x
		:
		sta direction

		eor #2
		tay
		lda (TMP2),y
		tay
		lda wallFlags,y
		and #WALLFLAG_BLOCKMONSTER
		bne :+
			clc
			rts
		:

		lda newPosition+0
		cmp partyPosition+0
		bne :+
		lda newPosition+1
		cmp partyPosition+1
		bne :+
			clc
			rts
		:

		clc
		lda newPosition+0
		adc #<triggerBitsAndMonsterCount
		sta TMP2+0
		lda newPosition+1
		adc #>triggerBitsAndMonsterCount
		sta TMP2+1
		ldy #0
		lda (TMP2),y
		and #7
		beq :++
			lda newPosition+0
			ora newPosition+1
			beq :++
				jsr findFreeSubposInDirection
				bpl :+
					clc
					rts
				:
				sta monster_subpos,x
		:

		lda newPosition+0
		sta TMP+0
		lda newPosition+1
		sta TMP+1
		lda direction
		sta TMP2+0
		stx ARGS+0

		jsrf monster_updatePositionAndDirection
		ldx CURRENT_MONSTER_INDEX
		lda #<-1
		sta direction
	skip:

	lda direction
	cmp #<-1
	beq :+
		sta monster_direction,x
	:

	lda oldPosition+0
	sta ARGS+0
	lda oldPosition+1
	sta ARGS+1
	jsrf gameState_isPositionVisible
	ldx CURRENT_MONSTER_INDEX

	ldy monster_typeIndex,x
	lda monsterType_walkSound,y
	bmi :+
		sta ARGS+0
		lda monster_position_lo,x
		sta ARGS+1
		lda monster_position_hi,x
		sta ARGS+2
		jsrf audio_playSound
	:

	ldx CURRENT_MONSTER_INDEX
	sec
	rts
.endproc

; x = monsterIndex
.proc findFreeSubposInDirection
	position = ARGS+0

	ldy monster_typeIndex,x
	lda monsterType_specialYoffset,y
	beq :+
		jmp returnNeg
	:

	jsr getOccupiedSubpos
	bpl :+
		jmp returnNeg
	:
	sta TMP

	lda monster_direction,x
	sta TMP2+0
	and #1
	asl
	asl
	sta TMP2+1

	ldx #0
	:
		clc
		txa
		adc TMP2+1
		tay
		lda subposTable,y
		eor TMP2+0
		tay
		lda bits,y
		and TMP
		bne :+
			ldx CURRENT_MONSTER_INDEX
			tya
			rts
		:
		inx
		cpx #4
	bne :--

returnNeg:
	ldx CURRENT_MONSTER_INDEX
	lda #<-1
	rts

bits:	.byte 1,2,4,8,16,32,64,128
subposTable:
	.byte 0,1,2,3
	.byte 0,2,1,3
.endproc

; Returns result in A
.proc getOccupiedSubpos
	position = ARGS+0

	.pushseg
	.segment "BSS"
		monsterCount: .res 1
	.popseg

	clc
	lda position+0
	adc #<triggerBitsAndMonsterCount
	sta TMP2+0
	lda position+1
	adc #>triggerBitsAndMonsterCount
	sta TMP2+1
	ldy #0
	lda (TMP2),y
	and #7
	sta monsterCount
	cmp #4
	bne :+
		ldx CURRENT_MONSTER_INDEX
		lda #<-2
		rts
	:

	lda #0
	sta TMP

	ldx #0
	monsterLoop:
		lda monster_position_lo,x
		cmp position+0
		bne continue
		lda monster_position_hi,x
		cmp position+1
		bne continue

			ldy monster_typeIndex,x
			lda monsterType_specialYoffset,y
			beq :+
				ldx CURRENT_MONSTER_INDEX
				lda #<-1
				rts
			:

			lda monster_subpos,x
			cmp #4
			bne :+
				ldy monster_direction,x
				lda centerDirectionToSubpos,y
				sta monster_subpos,x
			:

			tay
			lda subposToBit,y
			ora TMP
			sta TMP

			dec monsterCount
			beq break
		continue:
		inx
		cpx #30
	bne monsterLoop

break:	ldx CURRENT_MONSTER_INDEX
	lda TMP
	rts

subposToBit: .byte 1,2,4,8,16,32,64,128
centerDirectionToSubpos: .byte 0,1,3,2
.endproc

; x = monster index
; a = relativeDirection
.proc moveMonster
	pha
	lda monster_position_lo,x
	sta ARGS+0
	lda monster_position_hi,x
	sta ARGS+1
	lda monster_direction,x
	sta ARGS+2
	jsrf gameState_getForwardPosition
	ldx CURRENT_MONSTER_INDEX

	ldy #<-1
	jsr tryMoveMonster
	bcc :+
		pla
		rts
	:

	clc
	pla
	adc monster_direction,x
	and #3
	sta monster_direction,x
	tay
	lda #<-1
	sta ARGS+0
	lda #>-1
	sta ARGS+1
	jmp tryMoveMonster
.endproc

; x = monster index
; a = relativeDirection
.proc turnMonster
	.pushseg
	.segment "BSS"
		newDirection: .res 1
		relativeDirection: .res 1
	.popseg
	sta relativeDirection

	lda monster_turnDirection,x
	bpl :+
	jmp negTurnDirection
	:
		bne :+
			lda relativeDirection
			eor #$ff
			clc
			adc #1
			jsr moveMonster
		:

		clc
		lda relativeDirection
		adc monster_direction,x
		and #3
		sta newDirection

		sta ARGS+2
		lda monster_position_lo,x
		sta ARGS+0
		lda monster_position_hi,x
		sta ARGS+1
		jsrf gameState_getForwardPosition
		ldx CURRENT_MONSTER_INDEX

		lda ARGS+0
		asl
		sta TMP2+0
		lda ARGS+1
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

		ldy newDirection
		lda relativeDirectionsSouth,y
		tay
		lda (TMP2),y
		tay
		lda wallFlags,y
		and #WALLFLAG_BLOCKMONSTER
		ldy monster_turnDirection,x
		bne :++
			cmp #0
			bne :+
				lda #<-1
				sta monster_turnDirection,x
			:
			rts
		:
		cmp #0
		beq :+
			ldy newDirection
			lda #<-1
			sta ARGS+0
			lda #>-1
			sta ARGS+1
			jsr tryMoveMonster
			lda #<-1
			sta monster_turnDirection,x
			rts
		:
	negTurnDirection:

	lda monster_direction,x
	sta ARGS+2
	lda monster_position_lo,x
	sta ARGS+0
	lda monster_position_hi,x
	sta ARGS+1
	jsrf gameState_getForwardPosition
	ldx CURRENT_MONSTER_INDEX
	ldy #<-1
	jsr tryMoveMonster
	bcc :+
		lda #1
		sta monster_turnDirection,x
		rts
	:

	sec
	lda monster_direction,x
	sbc relativeDirection
	and #3
	tay
	lda #<-1
	sta ARGS+0
	lda #>-1
	sta ARGS+1
	jsr tryMoveMonster
	bcc :+
		lda #0
		sta monster_turnDirection,x
	:
	rts

relativeDirectionsSouth: .byte SOUTH, WEST,  NORTH, EAST

.endproc

.proc awakenMonster
	rts
.endproc

.export monsterTimerHandler
.proc monsterTimerHandler
	.pushseg
	.segment "BSS"
		levelType: .res 1
	.popseg

	lda ARGS+0
	sta levelType

	ldx #0
	monsterLoop:
		stx CURRENT_MONSTER_INDEX

		lda levelType
		cmp monster_levelType,x
		beq :+
			cc:jmp continue
		:

		; ScummVM also skips if m->flags & 0x20
		lda monster_hp_hi,x
		bmi cc
		ora monster_hp_lo,x
		beq cc

		; ScummVM also have this (only used in conjunction with setBlockMonsterDirection, not part of EOB1):
		; if (m->directionChanged) {
		;	 m->directionChanged = 0;
		;	 continue;
		; }

		jsr checkIfMonsterShouldMoveForward ;updateMonsterDest in ScummVM

		lda monster_phase,x ; m->mode in ScummVM
		cmp #MONSTER_STATE_FORWARD
		beq :+
		bmi :+
			jsr setMonsterTowardsParty ;updateMonsterAttackMode in ScummVM
		:

		; SWITCH
		lda monster_phase,x
		cmp #MONSTER_STATE_FORWARD ;0
		bne :+
			jsr moveMonsterForward ;updateMoveMonster in ScummVM
			jmp break
		:

		cmp #MONSTER_STATE_BACKWARD ;1
		bne :+
			lda #2
			jsr moveMonster ;updateMonsterFollowPath in ScummVM
			jmp break
		:

		cmp #MONSTER_STATE_LEFT ;2
		bne :+
			lda #<-1
			jsr moveMonster ;updateMonsterFollowPath in ScummVM
			jmp break
		:

		cmp #MONSTER_STATE_RIGHT ;3
		bne :+
			lda #1
			jsr moveMonster ;updateMonsterFollowPath in ScummVM
			jmp break
		:

		cmp #MONSTER_STATE_TURNLEFT ;5
		bne :+
			lda #<-1
			jsr turnMonster ;updateMonstersStraying in ScummVM
			jmp break
		:

		cmp #MONSTER_STATE_TURNRIGHT ;6
		bne :+
			lda #1
			jsr turnMonster ;updateMonstersStraying in ScummVM
			jmp break
		:

		cmp #MONSTER_STATE_READYHIT ;7
		beq :+
		cmp #MONSTER_STATE_ISHIT ;10
		bne :++
		:
			jsr awakenMonster ;updateMonstersSpellStatus in ScummVM
			jmp break
		:

		break:

		lda monster_phase,x ;m->mode in ScummVM
		cmp #MONSTER_STATE_ADJTURN ;4
		beq continue
		cmp #MONSTER_STATE_READYHIT ;7
		beq continue
		cmp #MONSTER_STATE_INACTIVE ;8
		beq continue
			lda monster_legState,x ;m->animStep
			eor #1
			sta monster_legState,x
		continue:

		;In ScummVM this also exists:
		;if (_monsterProps[m->type].u30 == 1) .u30 is not part of EOB1
		;	setBlockMonsterDirection(m->block, m->dir);

		ldx CURRENT_MONSTER_INDEX
		inx
		cpx #30
		bne :+
			jsrf checkAllThrownHit
			rts
		:
	jmp monsterLoop
.endproc
