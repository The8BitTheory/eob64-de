.include "global.inc"

.segment "GAMESTATE"
startPosition_lo:
	.byte <(10+15*32)
	.byte <(19+23*32)
	.byte <(26+4*32)
	.byte <(16+20*32)
	.byte <(10+24*32)
	.byte <(24+1*32)
	.byte <(1+19*32)
	.byte <(7+28*32)
	.byte <(21+4*32)
	.byte <(1+11*32)
	.byte <(15+11*32)
	.byte <(15+11*32)
startPosition_hi:
	.byte >(10+15*32)
	.byte >(19+23*32)
	.byte >(26+4*32)
	.byte >(16+20*32)
	.byte >(10+24*32)
	.byte >(24+1*32)
	.byte >(1+19*32)
	.byte >(7+28*32)
	.byte >(21+4*32)
	.byte >(1+11*32)
	.byte >(15+11*32)
	.byte >(15+11*32)
startDirection:
	.byte NORTH
	.byte EAST
	.byte WEST
	.byte SOUTH
	.byte NORTH;WEST
	.byte WEST
	.byte NORTH
	.byte WEST
	.byte EAST
	.byte NORTH
	.byte EAST
	.byte EAST

.segment "BSS"
.export swappingFlipFlop
swappingFlipFlop:	.res 1
.export godMode
godMode:	.res 1
.export seed
seed:		.res 1

.export teleportFlarePhase
teleportFlarePhase:	.res 1
.export visibleTeleporters
visibleTeleporters:	.res 1

.export thrown
.proc thrown
	.res 10*.sizeof(Thrown)
.endproc

.segment "GAMESTATEM"
.export pointerItemSaved
pointerItemSaved:
	.res 2

.export partyDirection
partyDirection:
	.res 1

.export partyPosition
partyPosition:
	.res 2

.export levelIndex
levelIndex:
	.res 1

.export loadedLevels
loadedLevels:
	.res 12

.export party
.proc party
	.res 6*.sizeof(PartyMember)
.endproc

.export partySpells
partySpells: .word 0

.export levelFlags_lo
levelFlags_lo:
	.res 12

.export levelFlags_hi
levelFlags_hi:
	.res 12

.export levelFlags_hi2
levelFlags_hi2:
	.res 12

.export levelFlags_hi3
levelFlags_hi3:
	.res 12

.export globalFlags_lo
globalFlags_lo:
	.res 1

.export globalFlags_hi
globalFlags_hi:
	.res 1

.export globalFlags_hi2
globalFlags_hi2:
	.res 1

.export globalFlags_hi3
globalFlags_hi3:
	.res 1

.export scriptFlags
;scriptFlags:	.res 4
scriptFlags = globalFlags_lo

.segment "MEMCODE_RO"
.export partyMembers_lo
partyMembers_lo:
.repeat 6,I
	.byte <(party+I*.sizeof(PartyMember))
.endrep

.export partyMembers_hi
partyMembers_hi:
.repeat 6,I
	.byte >(party+I*.sizeof(PartyMember))
.endrep

.segment "GAMESTATE"
.export gameStateInit
.proc gameStateInit
	lda #0
	sta godMode
	sta teleportFlarePhase
	lda #<-1
	sta SWAPPING_MEMBER_INDEX

	lda #0
	sta POINTER_ITEM+0
	sta POINTER_ITEM+1

	getPartyMember 0
	jsr createInventory
	jsr calculateMemberAC
	getPartyMember 1
	jsr createInventory
	jsr calculateMemberAC
	getPartyMember 2
	jsr createInventory
	jsr calculateMemberAC
	getPartyMember 3
	jsr createInventory
	jsr calculateMemberAC

	lda #0
	tax
	:
		sta thrown,x
		inx
		cpx #.sizeof(thrown)
	bne :-
	ldx #11
	:
		sta loadedLevels,x
		dex
	bpl :-

	; Clear visited tiles
	memset_underio visitedTiles, $00, $600

	rts
.endproc

.proc fillInventory
	lda #PartyMember::inventory+INVENTORY_BACKPACK*2
	sta TMP
	:
		jsr rnd
		tax
		ldy #0
		jsrf items_getItemPointer
		ldy TMP
		lda CUR_ITEM+0
		sta (CURRENT_PARTYMEMBER),y
		iny
		lda CUR_ITEM+1
		sta (CURRENT_PARTYMEMBER),y
		iny
		sty TMP
		cpy #PartyMember::inventory+(INVENTORY_BACKPACK+14)*2
	bne :-
	rts
.endproc

.export createInventory
.proc createInventory
	ldy #PartyMember::inventory
	:
		tya
		pha
		lda (CURRENT_PARTYMEMBER),y
		sta TMP+0
		iny
		lda (CURRENT_PARTYMEMBER),y
		sta TMP+1
		ora TMP+0
		bne :+
			pla
			tay
			iny
			iny
			jmp skipped
		:
		ldx TMP+0
		ldy TMP+1
		jsrf items_createItemFromBase
		pla
		tay
		lda CUR_ITEM+0
		sta (CURRENT_PARTYMEMBER),y
		iny
		lda CUR_ITEM+1
		sta (CURRENT_PARTYMEMBER),y
		iny
skipped:	cpy #PartyMember::inventory+26*2
	bne :--
	rts
.endproc

.export gameState_deactivateMonstersThrownDoors
.proc gameState_deactivateMonstersThrownDoors
	ldx #29
	lda #0
	:
		sta monster_state,x
		dex
	bpl :-

	lda #<thrown
	sta CURRENT_THROWN+0
	lda #>thrown
	sta CURRENT_THROWN+1
	loop:
		ldy #Thrown::active
		lda (CURRENT_THROWN),y
		cmp #1
		bne :+
			ldy #Thrown::itemOrSpellIndex
			lda (CURRENT_THROWN),y
			sta CUR_ITEM+0
			iny
			lda (CURRENT_THROWN),y
			sta CUR_ITEM+1
			ldy #Item::subpos
			jsr lda_CUR_ITEM_y
			and #3
			jsr sta_CUR_ITEM_y
		:
		ldy #Thrown::active
		lda #0
		sta (CURRENT_THROWN),y
		clc
		lda CURRENT_THROWN+0
		adc #.sizeof(Thrown)
		sta CURRENT_THROWN+0
		tax
		lda CURRENT_THROWN+1
		adc #0
		sta CURRENT_THROWN+1
		cmp #>(thrown+.sizeof(Thrown)*10)
		bne loop
		cpx #<(thrown+.sizeof(Thrown)*10)
	bne loop

	ldx #Timer::active
	lda doorTimer,x
	bne :+
		rts
	:

	ldx #Timer::parameter
	lda doorTimer,x
	sta TMP2

	ldx #Timer::argument
	lda doorTimer,x
	sta TMP+0
	inx
	lda doorTimer,x
	sta TMP+1

	ldy #0
	lax (TMP),y
	lda wallFlags,x
	and #WALLFLAG_ISDOOR
	bne :+
		iny
	:	

	moveDoor:
		lax (TMP),y
		lda wallFlags,x
		and #(WALLFLAG_DOOROPEN|WALLFLAG_DOORCLOSED)
		beq :+
			lda #0
			ldx #Timer::active
			sta doorTimer,x
			rts
		:

		txa
		clc
		adc TMP2
		sta (TMP),y
		iny
		iny
		lda (TMP),y
		clc
		adc TMP2
		sta (TMP),y
		dey
		dey
	jmp moveDoor
.endproc

.export gameState_saveLevel
.proc gameState_saveLevel
	lda POINTER_ITEM+0
	sta pointerItemSaved+0
	lda POINTER_ITEM+1
	sta pointerItemSaved+1
	ldy #Timer::active
	lda #0
	sta swappingMemberTimer,y
	jsr gameState_deactivateMonstersThrownDoors
	jsrf saveMaze

	lda #<visitedTiles
	sta MSRC+0
	lda #>visitedTiles
	sta MSRC+1
	lda #<SAVEGAME_START
	sta MDST+0
	lda #>SAVEGAME_START
	sta MDST+1
	lda #<$600
	ldx #>$600
	jsr _memcpy_pureram

	rts
.endproc

.export gameState_didLoadLevel
.proc gameState_didLoadLevel
	lda pointerItemSaved+0
	sta POINTER_ITEM+0
	lda pointerItemSaved+1
	sta POINTER_ITEM+1
	jsrf fixTimerHandlers

	lda #<SAVEGAME_START
	sta MSRC+0
	lda #>SAVEGAME_START
	sta MSRC+1
	lda #<visitedTiles
	sta MDST+0
	lda #>visitedTiles
	sta MDST+1
	lda #<$600
	ldx #>$600
	jsr _memcpy_pureram

	ldx #3
	:
		lda _tick,x
		sta latchTick,x
		sta tick,x
		dex
	bpl :-

	jsrf checkItemsBase

	rts
.endproc

.export gameState_loadLevel
.proc gameState_loadLevel
	lda timersEnabled
	pha
	lda #0
	sta screenEnabled
	sta timersEnabled

	txa
	pha
	jsr gameState_deactivateMonstersThrownDoors
	pla
	tax
	jsrf loadMaze

	ldx levelIndex
	dex
	lda startPosition_lo,x
	sta ARGS+0
	lda startPosition_hi,x
	sta ARGS+1
	lda startDirection,x
	sta partyDirection
	jsrf gameState_teleportParty

	jsrf render
	lda #$ff
	sta screenEnabled
	pla
	sta timersEnabled
	rts
.endproc

; In ScummVM this function is called int EoBCoreEngine::validateWeaponSlotItem(int index, int slot)
; CURRENT_PARTYMEMBER
; x=handIndex
; return in x 0=false 1=true
;public static boolean mayUseItemInHand(int memberIndex, int handIndex)
.export gameState_mayUseItemInHand
.proc gameState_mayUseItemInHand
		txa
		pha

		ldy #PartyMember::inventory + INVENTORY_LEFT_HAND*2
		lda (CURRENT_PARTYMEMBER),y
		sta MAY_ITEM+0
		iny
		lda (CURRENT_PARTYMEMBER),y
		sta MAY_ITEM+1
		iny
		lda (CURRENT_PARTYMEMBER),y
		sta RIGHT_ITEM+0
		iny
		lda (CURRENT_PARTYMEMBER),y
		sta RIGHT_ITEM+1

		ldy #MAY_ITEM
		jsr mayMemberUseItem_anyZP

		pla
		bne :+
			lda MAY_ITEM+0
			ora MAY_ITEM+1
			beq returnTrue
			bcs returnTrue
			bcc returnFalse
		:

		ldy #Item::type
		jsr lda_MAY_ITEM_y
		tay
		jsr items_fetchItemType

		lda MAY_ITEM+0
		ora MAY_ITEM+1
		beq :+
			lda itemType+ItemType::doubleHanded
			cmp #2
			beq returnFalse
		:

		lda RIGHT_ITEM+0
		ora RIGHT_ITEM+1
		beq returnTrue

		ldy #Item::type
		jsr lda_RIGHT_ITEM_y
		tay
		jsr items_fetchItemType
		lda itemType+ItemType::usage
		and #$7f
		beq returnMayUseRightItem
		cmp #3+1
		bcs returnMayUseRightItem

		lda itemType+ItemType::doubleHanded
		bne returnFalse

returnMayUseRightItem:
		ldy #RIGHT_ITEM
		jsr mayMemberUseItem_anyZP
		bcc returnFalse
returnTrue:
		ldx #1
		rts
returnFalse:
		ldx #0
		rts
.endproc

; Y=ZP pointer to Item
.proc mayMemberUseItem_anyZP
		lda CUR_ITEM+0
		pha
		lda CUR_ITEM+1
		pha
		lda $00,y
		sta CUR_ITEM+0
		lda $01,y
		sta CUR_ITEM+1
		jsr mayMemberUseItem
		pla
		sta CUR_ITEM+1
		pla
		sta CUR_ITEM+0
		rts
.endproc

; ATTACKING_PARTY_MEMBER_INDEX
; return monster index in A or -1
.proc getMonsterToAttack
	position = ARGS+0

	jsr getMonstersOnLocation

	lda MONSTERS_ON_LOC+4
	bmi :+
		rts
	:

	lda partyDirection
	asl
	asl
	asl
	sta TMP
	lda ATTACKING_PARTYMEMBER_INDEX
	and #1
	asl
	asl
	ora TMP
	tax

	ldy partyDirectionCasterSubPositionTables,x
	lda MONSTERS_ON_LOC,y
	bmi :+
		rts
	:
	inx
	ldy partyDirectionCasterSubPositionTables,x
	lda MONSTERS_ON_LOC,y
	bmi :+
		rts
	:
	inx
	ldy partyDirectionCasterSubPositionTables,x
	lda MONSTERS_ON_LOC,y
	bmi :+
		rts
	:
	inx
	ldy partyDirectionCasterSubPositionTables,x
	lda MONSTERS_ON_LOC,y
	bmi :+
		rts
	:

	lda #<-1
	rts

partyDirectionCasterSubPositionTables:
	.byte 2,3,0,1
	.byte 3,2,1,0
	.byte 0,2,1,3
	.byte 2,0,3,1
	.byte 1,0,3,2
	.byte 0,1,2,3
	.byte 3,1,0,2
	.byte 1,3,2,0
.endproc

; Called findBlockMonsters in ScummVM
; Returns
; MONSTERS_ON_LOC populated
; y=# monsters found
.export gameState_getMonstersOnPosition
.proc gameState_getMonstersOnPosition
	position = ARGS+0
	subpos = ARGS+2
	direction = ARGS+3
	includeAllOnPosition = ARGS+4
	firstMonsterOnPosition = ARGS+5

	lda #<-1
	sta MONSTERS_ON_LOC+0
	sta MONSTERS_ON_LOC+1
	sta MONSTERS_ON_LOC+2
	sta MONSTERS_ON_LOC+3
	sta MONSTERS_ON_LOC+4

	lda includeAllOnPosition
	beq :++
		ldy #0
		ldx #0
		l1:
			lda monster_position_lo,x
			cmp position+0
			bne :+
			lda monster_position_hi,x
			cmp position+1
			bne :+
				txa
				sta MONSTERS_ON_LOC,y
				iny
			:
			inx
			cpx #29
		bne l1
		rts
	:

	lda firstMonsterOnPosition
	beq :++
		jsr getFirstMonsterOnPosition
		bpl :+
			ldy #0
			rts
		:
		ldy #1
		sta MONSTERS_ON_LOC
		rts
	:

	ldy #0
	ldx #0
	l2:
		jsr monster_occupiesPosition
		bcc :+
			txa
			sta MONSTERS_ON_LOC,y
			iny
		:
		inx
		cpx #29
	bne l2

	rts
.endproc

.proc getFirstMonsterOnPosition
	position = ARGS+0
	subpos = ARGS+2
	direction = ARGS+3
	subposTable = TMP

	.pushseg
	.segment "BSS"
		monsterIndex: .res 1
		subPositionIndex: .res 1
	.popseg

	lda #<-1
	sta monsterIndex
	lda #5
	sta subPositionIndex

	;int subposTable[] = subPositionScanOrderTable[direction][subpos];

	lda direction
	asl
	asl
	adc subpos
	asl
	asl
	adc #<subPositionScanOrderTable
	sta subposTable+0
	lda #>subPositionScanOrderTable
	adc #0
	sta subposTable+1

	ldy #0
	ldx #0
	loop:
		lda monster_position_lo,x
		cmp position+0
		bne continue
		lda monster_position_hi,x
		cmp position+1
		bne continue

		lda monster_subpos,x
		cmp subpos
		bne :+
			txa
			rts
		:

		ldy #0
		:
			lda monster_subpos,x
			cmp (subposTable),y
			bne :+
				cpy subPositionIndex
				bcs :+
					sty subPositionIndex
					stx monsterIndex
			:
			iny
			cpy #4
		bne :--

		continue:
		inx
		cpx #29
	bne loop

	lda monsterIndex

	rts

subPositionScanOrderTable:
	.byte 4, 2, 1, 3
	.byte 4, 3, 0, 2
	.byte 4, 0, 3, 1
	.byte 4, 1, 2, 0
	
	.byte 4, 1, 2, 3
	.byte 4, 0, 3, 2
	.byte 4, 3, 0, 1
	.byte 4, 2, 1, 0
	
	.byte 4, 2, 1, 3
	.byte 4, 3, 0, 2
	.byte 4, 0, 3, 1
	.byte 4, 1, 2, 0

	.byte 4, 1, 2, 3
	.byte 4, 0, 3, 2
	.byte 4, 3, 0, 1
	.byte 4, 2, 1, 0
.endproc

; x = monsterIndex
.proc monster_occupiesPosition
	position = ARGS+0
	subpos = ARGS+2

	lda monster_position_lo,x
	cmp position+0
	bne false
	lda monster_position_hi,x
	cmp position+1
	bne false
	lda monster_subpos,x
	cmp subpos
	beq true
	cmp #4
	beq true

false:	clc
	rts
true:	sec
	rts
.endproc

.export gameState_dealBurningHandsDamage
.proc gameState_dealBurningHandsDamage
	position = ARGS+0
	damageRolls = ARGS+2
	damageSides = ARGS+3
	damageBase = ARGS+4
	damageReductionMode = ARGS+5

	jsr getMonstersOnLocation

	ldx MONSTERS_ON_LOC+4
	bmi :+
		jsr damageMonster
		jmp return
	:

	lda partyDirection
	asl
	tax
	ldy subPositionsAffectedByBurningHands+0,x
	ldx MONSTERS_ON_LOC,y
	bmi :+
		jsr damageMonster
	:

	lda partyDirection
	asl
	tax
	ldy subPositionsAffectedByBurningHands+1,x
	ldx MONSTERS_ON_LOC,y
	bmi :+
		jsr damageMonster
	:

return:
	rts

damageMonster:
	lda damageRolls
	sta DICE_ROLLS
	lda damageSides
	sta DICE_SIDES
	lda damageBase
	sta DICE_BASE
	jsr giveMonsterRandomDamage
	rts

subPositionsAffectedByBurningHands:
	.byte 2, 3
	.byte 0, 2
	.byte 0, 1
	.byte 1, 3
.endproc

; x = monsterIndex
; DICE_*
.proc giveMonsterRandomDamage
	damageReductionMode = ARGS+5

	stx XREG
	jsr rollDice
	lda DICE_RESULT+0
	pha

	lda damageReductionMode
	beq :++
		ldx XREG
		jsr gameState_shouldReduceDamageOnMonster
		bcc :++
			lda damageReductionMode
			cmp #1
			bne :+
				pla
				lsr
				pha
				jmp :++
			:
				pla
				lda #0
				pha
	:

	ldx XREG
	jsr gameState_checkMonsterSpellImmunity
	pla
	bcc :+
		rts
	:

	ldx XREG
	jsr giveMonsterDamage
	rts
.endproc

.proc getMonstersOnLocation
	position = ARGS+0

	lda #<-1
	sta MONSTERS_ON_LOC+0
	sta MONSTERS_ON_LOC+1
	sta MONSTERS_ON_LOC+2
	sta MONSTERS_ON_LOC+3
	sta MONSTERS_ON_LOC+4

	ldx #29
	loop:
		lda monster_position_lo,x
		cmp position+0
		bne :+
		lda monster_position_hi,x
		cmp position+1
		bne :+
			ldy monster_subpos,x
			txa
			sta MONSTERS_ON_LOC,y
		:
		dex
	bpl loop

	rts
.endproc

; CURRENT_PARTYMEMBER
; returns modifier in A
.proc getStrengthModifier
	ldy #PartyMember::strengthCurrent
	lax (CURRENT_PARTYMEMBER),y
	dex
	lda strengthModifierTable,x
	pha

	ldy #PartyMember::extraStrengthCurrent
	lax (CURRENT_PARTYMEMBER),y
	bne :+
		pla
		rts
	:
	pla

	cpx #1
	bcc :+
		lda #1
	:
	cpx #51
	bcc :+
		lda #2
	:
	cpx #76
	bcc :+
		lda #2
	:
	cpx #91
	bcc :+
		lda #2
	:
	cpx #100
	bcc :+
		lda #3
	:

	rts

strengthModifierTable: .byte <-4, <-3, <-3, <-2, <-2, <-1, <-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 3, 3, 4, 4, 5, 6, 7
.endproc

; CURRENT_PARTYMEMBER
; returns modifier in A
.proc getStrengthModifier2
	ldy #PartyMember::strengthCurrent
	lax (CURRENT_PARTYMEMBER),y
	dex
	lda strengthModifierTable,x
	pha

	ldy #PartyMember::extraStrengthCurrent
	lax (CURRENT_PARTYMEMBER),y
	bne :+
		pla
		rts
	:
	pla

	cpx #1
	bcc :+
		lda #3
	:
	cpx #51
	bcc :+
		lda #3
	:
	cpx #76
	bcc :+
		lda #4
	:
	cpx #91
	bcc :+
		lda #5
	:
	cpx #100
	bcc :+
		lda #6
	:

	rts

strengthModifierTable: .byte <-4, <-2, <-1, <-1, <-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 2, 7, 8, 9, 10, 11, 12, 14
.endproc

; CURRENT_PARTYMEMBER
; returns modifier in A
.proc getDexterityModifier
	ldy #PartyMember::dexterityCurrent
	lax (CURRENT_PARTYMEMBER),y
	dex
	lda dexterityModifierTable,x
	rts

dexterityModifierTable: .byte <5, <-4, <-3, <-2, <-1, 0, 0, 0, 0, 0, 0, 1, 2, 2, 3, 3, 4, 4, 4, 5, 5, 5, 5
.endproc

.export gameState_getDexterityModifier2
.proc gameState_getDexterityModifier2
	jsr getDexterityModifier2
	sta ARGS+0
	rts
.endproc

; CURRENT_PARTYMEMBER
; returns modifier in A
.proc getDexterityModifier2
	ldy #PartyMember::dexterityCurrent
	lax (CURRENT_PARTYMEMBER),y
	dex
	lda dexterityModifierTable,x
	sta ARGS+0
	rts

dexterityModifierTable: .byte 5, 5, 5, 4, 3, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, <-1, <-2, <-3, <-4, <-4, <-5, <-5, <-5, <-6, <-6
.endproc

; CURRENT_PARTYMEMBER
; a = otherAC
.proc getACModifier
	sta TMP+1

	ldy #PartyMember::class
	lax (CURRENT_PARTYMEMBER),y
	ldy #PartyMember::level
	lda classModifierTable,x
	bne :+
		; 0. lvl
		lda (CURRENT_PARTYMEMBER),y
		jmp done
	:
	cmp #1
	bne :+
		; 1. lvl/3
		jsr div3
		jmp done
div3:	lda (CURRENT_PARTYMEMBER),y
		sta NUM1+0
		lda #0
		sta NUM1+1
		lda #$56
		sta NUM2
		jsr multiply
		tya
		rts
	:
	cmp #2
	bne :+
		; 2. (lvl/3)*2
		jsr div3
		asl
		jmp done
	:
		; 3. lvl/2
		lda (CURRENT_PARTYMEMBER),y
		lsr

done:
	sta TMP+0
	sec
	lda #20
	sbc TMP+0
	sec
	sbc TMP+1
	rts

classModifierTable:
	.byte 0, 0, 0, 1, 2, 3, 0, 0, 0, 0, 3, 2, 0, 0, 2
.endproc

; bool EoBCoreEngine::characterAttackHitTest(int charIndex, int monsterIndex, int item, int attackType) {
; ATTACKING_PARTYMEMBER_INDEX
; CUR_ITEM
; x = Monster index
; c = baseSkillOnDexterityOrStrength
;
; return c=1, true. c=0, false.
.export gameState_willCasterHitMonster
gameState_willCasterHitMonster = willCasterHitMonster
.proc willCasterHitMonster
	stx XREG
	php

	ldx ATTACKING_PARTYMEMBER_INDEX
	lda partyMembers_lo,x
	sta CURRENT_PARTYMEMBER+0
	lda partyMembers_hi,x
	sta CURRENT_PARTYMEMBER+1

	;int itemType = (itemIndex==0) ? 0 : GameState.items.get(itemIndex).type;
	lda CUR_ITEM+0
	ora CUR_ITEM+1
	beq :+
		ldy #Item::type
		jsr lda_CUR_ITEM_y
	:

	;int itemValue = ((itemType<1) || (itemType>3)) ? 0 : GameState.items.get(itemIndex).value;
	cmp #1
	bcc :+
	cmp #4
	bcs :+
		ldy #Item::value
		jsr lda_CUR_ITEM_y
		jmp :++
	:
		lda #0
	:

	sta TMP2
	plp
	;if (baseSkillOnDexterityOrStrength!=0)
	bcc :+
		jsr getStrengthModifier
		jmp :++
	:
		jsr getDexterityModifier
	:
	clc
	adc TMP2
	sta TMP2 ; d

	;monster.flags |= Monster.FLAGS_ACTIVE;
	ldx XREG
	lda monster_flags,x
	ora #MONSTER_FLAGS_ACTIVE
	sta monster_flags,x

	;int acModifier = getACModifier(caster, monster.type.ac);
	ldy monster_typeIndex,x
	lda monsterType_ac,y
	jsr getACModifier

	;acModifier-=itemValue;
	sec
	sbc TMP2
	sta TMP2

	;Dice dice = new Dice(1,20,0);
	dice 1,20,0
	jsr rollDice

	;if ((partySpells&0x30)!=0)
	lda partySpells
	and #(SPELL_PRAYER|SPELL_BLESS)
	beq :+
		inc DICE_RESULT+0
	:

	;if ((member.activeSpells&0x40)!=0)
	ldy #PartyMember::activeSpells+0
	lda (CURRENT_PARTYMEMBER),y
	and #<SPELL_AID
	beq :+
		inc DICE_RESULT+0
	:

	;if (diceValue>20)
	lda DICE_RESULT+0
	cmp #20
	bcc :+
		lda #20
	:

	;if (diceValue<1)
	cmp #0
	bne :+
		lda #1
	:

	;return diceValue>=acModifier;
	cmp TMP2
	rts
.endproc

.export gameState_getMaxExpLevelIndex
.proc gameState_getMaxExpLevelIndex
	lda ARGS+0
	jsr getMaxExpLevelIndex
	sta ARGS+0
	rts
.endproc

; Called getCharacterLevelIndex in ScummVM
; a = multiclass
; y = class
.proc getMaxExpLevelIndex
	sty YREG
	sta TMP2

	lda TMP2
	ldx #0
	jsr multiClassToClass
	cmp YREG
	bne :+
		lda #0
		rts
	:

	lda TMP2
	ldx #1
	jsr multiClassToClass
	cmp YREG
	bne :+
		lda #1
		rts
	:

	lda TMP2
	ldx #2
	jsr multiClassToClass
	cmp YREG
	bne :+
		lda #2
		rts
	:

	lda #<-1
	rts
.endproc

; x = partyMemberIndex
.export gameState_getSpellCasterDamageMultiplier
.proc gameState_getSpellCasterDamageMultiplier
	lda SPELLCASTER_USING_SCROLL
	beq :+
		ret9:lda #9
		sta ARGS+0
		rts
	:

	; Is spell not from a member
	cpx #<-1
	bne :+
		lda levelIndex
		cmp #7
		bcs ret9
		lda #5
		sta ARGS+0
		rts
	:

	lda partyMembers_lo,x
	sta CURRENT_PARTYMEMBER+0
	lda partyMembers_hi,x
	sta CURRENT_PARTYMEMBER+1
	ldy #PartyMember::class
	lda (CURRENT_PARTYMEMBER),y
	ldy #CLASS_MAGE
	jsr getMaxExpLevelIndex
	cmp #<-1
	bmi :+
	beq :+
		clc
		adc #PartyMember::level
		tay
		lda (CURRENT_PARTYMEMBER),y
		sta ARGS+0
		rts
	:
	lda #1
	sta ARGS+0
	rts
.endproc

; x = partyMemberIndex
.export gameState_getLevelACModifier
.proc gameState_getLevelACModifier
	lda SPELLCASTER_USING_SCROLL
	beq :+
		ret9:lda #9
		sta ARGS+0
		rts
	:

	cpx #<-1
	bne :+
		lda levelIndex
		cmp #7
		bcc :+
			lda #5
			sta ARGS+0
			rts
		jmp ret9
	:

	lda partyMembers_lo,x
	sta CURRENT_PARTYMEMBER+0
	lda partyMembers_hi,x
	sta CURRENT_PARTYMEMBER+1
	ldy #PartyMember::class
	lda (CURRENT_PARTYMEMBER),y
	ldy #CLASS_CLERIC
	jsr getMaxExpLevelIndex
	cmp #<-1
	bmi :+
	beq :+
		clc
		adc #PartyMember::level
		tay
		lda (CURRENT_PARTYMEMBER),y
		sta ARGS+0
		rts
	:

	ldy #PartyMember::class
	lda (CURRENT_PARTYMEMBER),y
	ldy #CLASS_PALADIN
	jsr getMaxExpLevelIndex
	cmp #<-1
	bpl :+
		ret1: lda #1
		sta ARGS+0
		rts
	:

	clc
	adc #PartyMember::level
	tay
	lda (CURRENT_PARTYMEMBER),y
	cmp #9
	bmi ret1

	sbc #8
	sta ARGS+0
	rts
.endproc

; In ScummVM this function is called int EoBCoreEngine::closeDistanceAttack(int charIndex, Item item)
; ATTACKING_PARTYMEMBER_INDEX
; CUR_ITEM
.proc meleeAttack
		;if (memberIndex>=2)
		lda ATTACKING_PARTYMEMBER_INDEX
		cmp #2
		bcc :+
			lda #<ATTACK_CANTREACH
			rts
		:

		;int attackPosition = getForwardPosition(GameState.partyPosition, GameState.partyDirection);
		lda partyPosition+0
		sta ARGS+0
		lda partyPosition+1
		sta ARGS+1
		lda partyDirection+0
		sta ARGS+2
		jsr gameState_getForwardPosition

		jsr getMonsterToAttack
		bpl :++
			lda ARGS+0
			asl
			rol ARGS+1
			asl
			rol ARGS+1
			clc
			adc #<maze
			sta TMP+0
			lda ARGS+1
			adc #>maze
			sta TMP+1
			ldy relativeDirectionSouth
			lax (TMP),y
			lda wallEventMask,x
			cmp #$ff
			bne :+
				inx
				txa
				sta (TMP),y
				tya
				eor #2
				tay
				txa
				sta (TMP),y
				inc SHOULDRENDER
				lda #<ATTACK_HACK
				rts
			:
			lda #<ATTACK_MISS
			rts
		:
		sta TARGETED_MONSTER_INDEX

		sec
		ldx TARGETED_MONSTER_INDEX
		jsr willCasterHitMonster
		bcs :+
			lda #<ATTACK_MISS
			rts
		:

		ldy #Item::type
		jsr lda_CUR_ITEM_y
		pha
		bpl :+ ; Custom item types, aka spells, are >=$80
			ldx TARGETED_MONSTER_INDEX
			jsr gameState_checkMonsterSpellImmunity
			bcc :+
				pla
				lda #<ATTACK_MISS
				rts
		:
		
		pla
		pha
		sec
		ldx TARGETED_MONSTER_INDEX
		jsr calculateItemDamage
		sta TMP

		pla
		ldx TARGETED_MONSTER_INDEX
		jsr getMonsterDamageReduction
		bcc :+
			lsr TMP
		:
		
		lda TMP
		rts
.endproc

; x = monsterIndex
; a = itemTypeIndex
.proc getMonsterDamageReduction
	tay
	lda monster_typeIndex,x
	tax
	lda monsterType_typeBits_lo,x
	and #MONSTER_TYPEBITS_LO_REDUCEDAMAGE
	beq :+
		clc
		rts
	:
	jsr items_fetchItemType
	lda itemType+ItemType::classBits
	and #4
	sec
	bne :+
		clc
	:
	rts
.endproc

.if 0

		ldx TARGETED_MONSTER_INDEX
		lda monster_flags,x
		ora #MONSTER_FLAGS_HIT
		sta monster_flags,x
		inc SHOULDRENDER

		jsr rnd
		and #31
		rts
.endif

; In ScummVM this function is bool EoBCoreEngine::calcDamageCheckItemType(int itemType)
; y = itemTypeIndex
; return c=0 false, c=1 true
.proc isItemAThrowableOrRangeWeapon
	;ItemType itemType = items.itemTypes[itemTypeIndex];
	;int v = itemType.usage&0x7f;
	jsr items_fetchItemType
	lda itemType+ItemType::usage
	and #$7f
	;(v==2) || (v==3)
	cmp #ITEMUSAGE_THROWABLES
	beq true
	cmp #ITEMUSAGE_RANGE_WEAPONS
	beq true
false:	clc
	rts
true:	sec
	rts
.endproc

; In ScummVM this function is int EoBCoreEngine::calcDamageModifers(int charIndex, EoBMonsterInPlay *m, int item, int itemType, int useStrModifier)
; CURRENT_PARTYMEMBER
; x = Monster Index (-1 == null)
; CUR_ITEM
; a = itemType
; c = applyStrengthModifyer 
;
; returns damage in A
.proc calculateItemDamage
	stx XREG

	tay
	jsr items_fetchItemType

	;int value = 0;

	;if ((applyStrenghModifier) && (caster!=-1))
	lda #0
	bcc :+
		lda CURRENT_PARTYMEMBER
		ora CURRENT_PARTYMEMBER
		beq :+
			;value = getStrengthModifier2(caster);
			jsr getStrengthModifier2
	:
	sta TMP2

	;if (itemIndex!=0)
	sec
	lda #<items
	sbc CUR_ITEM+0
	lda #>items
	sbc CUR_ITEM+1
	bpl noItem
		; value += items.get(itemIndex).value;
		ldy #Item::value
		jsr lda_CUR_ITEM_y
		clc
		adc TMP2
		sta TMP2

		;if ((monster!=null) && ((monster.type.typeBits&MonsterType.TYPEBITS_BIGSIZE)!=0))
		ldx XREG
		cpx #<-1
		beq vsSmall
		ldy monster_typeIndex,x
		lda monsterType_typeBits_lo,y
		and #MONSTER_TYPEBITS_LO_BIGSIZE
		beq vsSmall
			; Damage vs big
			lda itemType+ItemType::damageVsBig+0
			sta DICE_ROLLS
			lda itemType+ItemType::damageVsBig+1
			sta DICE_SIDES
			lda itemType+ItemType::damageVsBig+2
			sta DICE_BASE
			jmp rollIt
		vsSmall: ; else
			; Damage vs small
			lda itemType+ItemType::damageVsSmall+0
			sta DICE_ROLLS
			lda itemType+ItemType::damageVsSmall+1
			sta DICE_SIDES
			lda itemType+ItemType::damageVsSmall+2
			sta DICE_BASE
			jmp rollIt
	noItem:
		lda #1
		sta DICE_ROLLS
		lda #2
		sta DICE_SIDES
		lda #0
		sta DICE_BASE

rollIt:
	jsr rollDice

	; Sign extend TMP2
	ldx #0
	lda TMP2+0
	bpl :+
		dex
	:
	stx TMP2+1

	clc
	lda TMP2+0
	adc DICE_RESULT+0
	sta TMP2+0
	lda TMP2+1
	adc DICE_RESULT+1
	sta TMP2+1

	;if (value<0)
	bpl :+
		lda #0
		sta TMP2+0
	:
	lda TMP2+0
	rts
.endproc

; ATTACKING_PARTYMEMBER_INDEX
; ATTACKING_HAND_INDEX
; CUR_ITEM
; ARGS+7 prepopulated with item.type
.proc rangeAttack
		.pushseg
		.segment "BSS"
			ammoItemType: .res 1
		.popseg

		ldx ATTACKING_PARTYMEMBER_INDEX
		lda partyMembers_lo,x
		sta CURRENT_PARTYMEMBER+0
		lda partyMembers_hi,x
		sta CURRENT_PARTYMEMBER+1

		;int ammoItemType = weaponItemTypeToAmmoItemType[item.type-7];
		sec
		lda ARGS+7
		sbc #7
		tax
		lda weaponItemTypeToAmmoItemType,x
		sta ammoItemType

		cmp #16
		beq needArrow
		jmp notArrow
		needArrow:
			; Got an arrow in the left hand?
			ldy #PartyMember::inventory+INVENTORY_LEFT_HAND*2
			lda (CURRENT_PARTYMEMBER),y
			sta TMP+0
			iny
			lda (CURRENT_PARTYMEMBER),y
			sta TMP+1
			ora TMP+0
			beq :+
				ldy #Item::type
				jsr lda_TMP_y
				cmp #$10
				bne :+
					ldy #PartyMember::inventory+INVENTORY_LEFT_HAND*2
					lda #0
					sta (CURRENT_PARTYMEMBER),y
					iny
					sta (CURRENT_PARTYMEMBER),y
					jmp gotAmmo
			:

			; Got an arrow in the right hand then?
			ldy #PartyMember::inventory+INVENTORY_RIGHT_HAND*2
			lda (CURRENT_PARTYMEMBER),y
			sta TMP+0
			iny
			lda (CURRENT_PARTYMEMBER),y
			sta TMP+1
			ora TMP+0
			beq :+
				ldy #Item::type
				jsr lda_TMP_y
				cmp #$10
				bne :+
					ldy #PartyMember::inventory+INVENTORY_RIGHT_HAND*2
					lda #0
					sta (CURRENT_PARTYMEMBER),y
					iny
					sta (CURRENT_PARTYMEMBER),y
					jmp gotAmmo
			:

			; Any thing in the quiver?
			ldy #PartyMember::inventory+INVENTORY_QUIVER*2
			lda (CURRENT_PARTYMEMBER),y
			sta TOP_ITEM+0
			iny
			lda (CURRENT_PARTYMEMBER),y
			sta TOP_ITEM+1
			ora TOP_ITEM+0
			beq noAmmo
			lda #0
			sta ARGS+0
			jsrf items_unlinkItemBySubPos
			ldy #PartyMember::inventory+INVENTORY_QUIVER*2
			lda TOP_ITEM+0
			sta (CURRENT_PARTYMEMBER),y
			iny
			lda TOP_ITEM+1
			sta (CURRENT_PARTYMEMBER),y
			lda CUR_ITEM+0
			sta TMP+0
			lda CUR_ITEM+1
			sta TMP+1
			jmp gotAmmo

		notArrow:
			ldy #PartyMember::inventory
			nextItemSlot:
				lda (CURRENT_PARTYMEMBER),y
				sta TMP+0
				iny
				lda (CURRENT_PARTYMEMBER),y
				sta TMP+1
				iny
				sty YREG
				ora TMP+0
				beq continue
					ldy #Item::type
					jsr lda_TMP_y
					cmp ammoItemType
					bne continue
						lda #0
						ldy YREG
						dey
						sta (CURRENT_PARTYMEMBER),y
						dey
						sta (CURRENT_PARTYMEMBER),y
						jmp gotAmmo
				continue:
				ldy YREG
				cpy #PartyMember::inventory+27*2
			bne nextItemSlot

noAmmo:
		lda #<ATTACK_NOAMMO
		rts

gotAmmo:	
		; Throw ITEM here
		; int subpos = memberIndex<=3 ? memberIndex : memberIndex-2;
		lda ATTACKING_PARTYMEMBER_INDEX
		sta ARGS+0
		cmp #4
		bcc :+
			sbc #2
		:
		asl
		asl
		ora partyDirection
		tax
		lda throwSubpos,x
		sta ARGS+5

		lda TMP+0
		sta ARGS+1
		lda TMP+1
		sta ARGS+2

		lda partyPosition+0
		sta ARGS+3
		lda partyPosition+1
		sta ARGS+4

		lda partyDirection
		sta ARGS+6

		jsr gameState_throwItem

		;Audio.playSoundEffect((item.type==7) ? 26 : 11);
		;NO! gameState_throwItem already does this!

		lda #0
		rts

weaponItemTypeToAmmoItemType: .byte 16,<-1,<-1,<-1,<-1,<-1,<-1,18,17,0,1,18,19

.endproc

throwSubpos:
	.byte 0,1,3,2
	.byte 1,3,2,0
	.byte 2,0,1,3
	.byte 3,2,0,1

; ATTACKING_PARTYMEMBER_INDEX
; ATTACKING_HAND_INDEX
; CUR_ITEM
; ARGS+7 prepopulated with item.type
.proc throwAttack
		; int subpos = memberIndex<=3 ? memberIndex : memberIndex-2;
		lda ATTACKING_PARTYMEMBER_INDEX
		sta ARGS+0
		cmp #4
		bcc :+
			sbc #2
		:
		asl
		asl
		ora partyDirection
		tax
		lda throwSubpos,x
		sta ARGS+5

		lda CUR_ITEM+0
		sta ARGS+1
		lda CUR_ITEM+1
		sta ARGS+2

		lda partyPosition+0
		sta ARGS+3
		lda partyPosition+1
		sta ARGS+4

		lda partyDirection
		sta ARGS+6

		jsr gameState_throwItem
		lsr ARGS+0
		bcc return
			;Audio.playSoundEffect(11);
			;NO! gameState_throwItem already does this!

			;PartyMember member = party[memberIndex];
			ldx ATTACKING_PARTYMEMBER_INDEX
			stx ARGS+0
			lda partyMembers_lo,x
			sta CURRENT_PARTYMEMBER+0
			lda partyMembers_hi,x
			sta CURRENT_PARTYMEMBER+1

			;member.inventory[handIndex]=0
			lda ATTACKING_HAND_INDEX
			sta ARGS+1
			asl
			adc #PartyMember::inventory
			tay
			lda #0
			sta (CURRENT_PARTYMEMBER),y
			iny
			sta (CURRENT_PARTYMEMBER),y

			;grabAmmo(memberIndex, handIndex/*, -1, false*/);
			jsr gameState_grabAmmo

return:		lda #0
		rts
.endproc

.proc gameState_grabAmmo
	memberIndex = ARGS+0
	handIndex = ARGS+1

	ldx memberIndex
	lda partyMembers_lo,x
	sta CURRENT_PARTYMEMBER+0
	lda partyMembers_hi,x
	sta CURRENT_PARTYMEMBER+1

	ldy #PartyMember::inventory + (INVENTORY_BELT+2)*2
	:
		tya
		pha

		;int beltItemIndex = member.inventory[i];
		lda (CURRENT_PARTYMEMBER),y
		sta TMP+0
		iny
		lda (CURRENT_PARTYMEMBER),y
		sta TMP+1

		;if (beltItemIndex==0)
		ora TMP+0
		beq continue

		;member.inventory[i] = 0;
		lda #0
		sta (CURRENT_PARTYMEMBER),y
		dey
		sta (CURRENT_PARTYMEMBER),y
		
		;member.inventory[handIndex] = beltItemIndex;
		lda handIndex
		asl
		adc #PartyMember::inventory
		tay
		lda TMP+0
		sta (CURRENT_PARTYMEMBER),y
		iny
		lda TMP+1
		sta (CURRENT_PARTYMEMBER),y
		pla
		rts

		continue:
		pla
		tay
		cpy #PartyMember::inventory + INVENTORY_BELT*2
		beq :+
		dey
		dey
	jmp :-
	:
	rts
.endproc

; x = Monster index
.export killMonster
.proc killMonster
	.pushseg
	.segment "BSS"
		monsterIndex: .res 1
	.popseg
	dropSubPos = ARGS+2

	stx monsterIndex
	jsr gameState_isPositionVisible

	ldx monsterIndex
	lda monster_subpos,x
	cmp #4
	bne :+
		jsr rnd
		and #3
	:
	sta dropSubPos

	;if ((monster.weapon!=0) && (dropWeaponDice.roll()==1)) ;dropWeapinDice = 1T2+0
	lda monster_weapon,x
	beq :+
		dice 1,2,0
		jsr rollDice
		lda DICE_RESULT
		cmp #1
		bne :+
			lda monster_weapon,x
			jsr dropItem
	:

	;if (monster.pocketItem!=0)
	ldx monsterIndex
	lda monster_pocketItem,x
	beq :+
		jsr dropItem
	:

	; givePartyExperience
	ldx monsterIndex
	ldy monster_typeIndex,x
	lda monsterType_experience_lo,y
	sta ARGS+0
	lda monsterType_experience_hi,y
	sta ARGS+1
	jsr gameState_givePartyExperience

	ldx monsterIndex
	stx ARGS+0
	lda #0
	sta TMP+0
	sta TMP+1
	lda #<-1
	sta TMP2+0
	jsrf monster_updatePositionAndDirection

	; check ending
	ldx monsterIndex
	lda monster_typeIndex,x
	cmp #21
	bne :+
		lda #0
		sta timersEnabled
		inc SHOULDRENDER
		jsrf render
		jmpf finaleNoDeathAnim
	:

	lda monster_phase,x
	cmp #MONSTER_STATE_INACTIVE
	bne :+
		jsrf monsterTimerHandler_setAllMonstersTowardsParty
	:

	ldx monsterIndex
	rts

dropItem:
	tax
	ldy #0
	jsrf items_createItemFromBase
	lda CUR_ITEM+0
	ora CUR_ITEM+1
	beq :+
		ldx monsterIndex
		lda monster_position_lo,x
		sta ARGS+0
		lda monster_position_hi,x
		sta ARGS+1
		jsrf getTopItem
		jsrf items_linkItem
		ldy #0
		lda TOP_ITEM+0
		sta (TMP),y
		iny
		lda TOP_ITEM+1
		sta (TMP),y
	:
	rts
.endproc

; x = Monster index
; a = Damage
.export gameState_giveMonsterDamage
.proc gameState_giveMonsterDamage
	lda ARGS+0
.endproc
; Fall through
.proc giveMonsterDamage
.pushseg
.segment "MEMCODE_RW"
	preventMonsterFlash: .byte 0
.popseg
	bne :+
		rts
	:
	sta TMP+0
	sec
	lda monster_hp_lo,x
	sbc TMP
	sta monster_hp_lo,x
	lda monster_hp_hi,x
	sbc #0
	sta monster_hp_hi,x

	lda monster_flags,x
	and #MONSTER_FLAGS_TURNED^$ff
	ora #MONSTER_FLAGS_ACTIVE
	sta monster_flags,x

	lda monster_position_lo,x
	sta ARGS+0
	lda monster_position_hi,x
	sta ARGS+1
	txa
	pha
	jsr gameState_isPositionVisible
	pla
	tax
	bcc :+
		lda preventMonsterFlash
		bne :+
			lda monster_flags,x
			ora #MONSTER_FLAGS_HIT
			sta monster_flags,x
			txa
			pha
			jsrf render
			pla
			tax
	:

	lda monster_hp_hi,x
	bmi :+
	ora monster_hp_lo,x
	beq :+
	rts
	:jmp killMonster
.endproc

; x=ATTACKING_PARTYMEMBER_INDEX
; y=ATTACKING_HAND_INDEX
; CURRENT_PARTYMEMBER
; CUR_ITEM
.export gameState_normalAttack
.proc gameState_normalAttack
		; Expire invisibility when attacking
		ldy #PartyMember::activeSpells+0
		lda (CURRENT_PARTYMEMBER),y
		and #(<SPELL_INVISIBLE)^$ff
		sta (CURRENT_PARTYMEMBER),y

		; Get item type usage
		ldy #0 ; Item Type 0 for hands
		sec
		lda #<items
		sbc CUR_ITEM+0
		lda #>items
		sbc CUR_ITEM+1
		bpl :+
			ldy #Item::type
			jsr lda_CUR_ITEM_y
			tay
		:
		sty ARGS+7 ; throwAttack needs this
		jsr items_fetchItemType
		lda itemType+ItemType::usage
		and #$7f

		cmp #1
		bne :++
			jsr meleeAttack
			bne :+
				lda #<ATTACK_MISS
			:
			pha
			ldx #32
			jsrf audio_playSoundEffect
			pla
			jmp attackDone
		:
		cmp #2
		bne :+
			jsr throwAttack
			jmp attackDone
		:
		cmp #3
		bne :+
			jsr rangeAttack
			jmp attackDone
		:
		lda #0
attackDone:	sta TMP
		bmi noDamage
		beq noDamage
			sec
			lda #<items
			sbc CUR_ITEM+0
			lda #>items
			sbc CUR_ITEM+1
			bpl skipSpecialForHands
				ldy #Item::flags
				jsr lda_CUR_ITEM_y
				and #ITEM_FLAGS_REGAINHP
				beq :+
					clc
					ldy #PartyMember::hpCurrent
					lda (CURRENT_PARTYMEMBER),y
					adc TMP
					sta (CURRENT_PARTYMEMBER),y
					iny
					lda (CURRENT_PARTYMEMBER),y
					adc #0
					sta (CURRENT_PARTYMEMBER),y
					bmi :+
					bne cap
					dey
					lda (CURRENT_PARTYMEMBER),y
					bpl :+
					cap:
						lda #127
						sta (CURRENT_PARTYMEMBER),y
						iny
						lda #0
						sta (CURRENT_PARTYMEMBER),y
				:

				ldy #Item::flags
				jsr lda_CUR_ITEM_y
				and #ITEM_FLAGS_CONSUMABLE
				beq :+
					lda ATTACKING_HAND_INDEX
					asl
					adc #PartyMember::inventory
					tay
					lda #0
					sta (CURRENT_PARTYMEMBER),y
					iny
					sta (CURRENT_PARTYMEMBER),y
				:
			skipSpecialForHands:

			ldx TARGETED_MONSTER_INDEX
			lda TMP
			pha
			jsr giveMonsterDamage
			pla
			sta TMP
		noDamage:

		ldx ATTACKING_PARTYMEMBER_INDEX
		lda partyMembers_lo,x
		sta CURRENT_PARTYMEMBER+0
		lda partyMembers_hi,x
		sta CURRENT_PARTYMEMBER+1

		;member.disabledHands |= 1<<handIndex;
		ldx ATTACKING_HAND_INDEX
		inx
		txa
		ldy #PartyMember::disabledHands
		ora (CURRENT_PARTYMEMBER),y
		sta (CURRENT_PARTYMEMBER),y

		;member.hitInfo[handIndex] = damage;
		lda ATTACKING_HAND_INDEX
		asl
		adc #PartyMember::hitInfo
		tay
		lda TMP
		sta (CURRENT_PARTYMEMBER),y
		bpl :+
			lda #$ff
			jmp :++
		:
			lda #$00
		:
		iny
		sta (CURRENT_PARTYMEMBER),y

		ldx ATTACKING_PARTYMEMBER_INDEX
		lda #18
		sta ARGS+0
		lda #0
		sta ARGS+1
		sta ARGS+2
		sta ARGS+3

		lda TMP
		cmp #<-2
		bpl :+
			lda #0
			jmp :++
		:
			; >=-2
			lda #2
		:
		clc
		adc ATTACKING_HAND_INDEX
		sta ARGS+4
		lda #1
		sta ARGS+5
		jsr gameState_setMemberTimeout

		ldx ATTACKING_PARTYMEMBER_INDEX
		inc partyMemberStatsChanged,x

		rts
.endproc

;public static boolean mayMemberUseItem(int memberIndex, int itemIndex)
; CURRENT_PARTYMEMBER
; CUR_ITEM
.export mayMemberUseItem
.proc mayMemberUseItem
		lda CUR_ITEM+0
		ora CUR_ITEM+1
		beq returnTrue

		ldy #Item::type
		jsr lda_CUR_ITEM_y
		tay
		jsr items_fetchItemType
		lda itemType+ItemType::classBits
		ldy #PartyMember::class
		pha
		lda (CURRENT_PARTYMEMBER),y
		tay
		pla
		and classProfessions,y
		beq returnFalse

returnTrue:	sec
		rts
returnFalse:	clc
		rts
.endproc

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

; DICE_*
.export gameState_causeWounds
.proc gameState_causeWounds
		.pushseg
		.segment "BSS"
			diceRolls: .res 1
			diceSides: .res 1
			diceBase: .res 1
			monsterIndex: .res 1
		.popseg

		lda DICE_ROLLS
		sta diceRolls
		lda DICE_SIDES
		sta diceSides
		lda DICE_BASE
		sta diceBase

		ldx SPELLBOOK_MEMBER_INDEX
		lda partyMembers_lo,x
		sta CURRENT_PARTYMEMBER+0
		lda partyMembers_hi,x
		sta CURRENT_PARTYMEMBER+1

		lda frontRanks,x
		bne :+
			lda #1
			sta textColor
			jsrf printMemberName
			ldx #<msg1
			ldy #>msg1
			jsr text_writeNullString
			rts
			msg1: .byte " must be in the front ranks to hit!",$a,0
		:

		lda partyPosition+0
		sta ARGS+0
		lda partyPosition+1
		sta ARGS+1
		lda partyDirection
		sta ARGS+2
		jsr gameState_getForwardPosition

		lda SPELLBOOK_MEMBER_INDEX
		sta ATTACKING_PARTYMEMBER_INDEX
		jsr getMonsterToAttack
		bpl :+
			lda #1
			sta textColor
			jsrf printMemberName
			ldx #<msg2
			ldy #>msg2
			jsr text_writeNullString
			rts
			msg2: .byte " missed the monster.",$a,0
		:
		tax
		stx monsterIndex

		lda #0
		sta CUR_ITEM+0
		sta CUR_ITEM+1
		sec
		jsr gameState_willCasterHitMonster
		bcs :+
			rts
		:

		ldx monsterIndex
		jsr gameState_checkMonsterSpellImmunity
		bcc :+
			rts
		:

		lda diceRolls
		sta DICE_ROLLS
		lda diceSides
		sta DICE_SIDES
		lda diceBase
		sta DICE_BASE
		jsr rollDice

		ldx monsterIndex
		lda DICE_RESULT+0
		jmp giveMonsterDamage

frontRanks: .byte 1,1,0,0,0,0
.endproc

; X = member index
.export gameState_healMember
.proc gameState_healMember
		hpToHeal = ARGS+0 ;+1 16-bit
		
		inc partyMemberStatsChanged,x

		ldy #IS_ACTIVE|IS_ALIVE
		jsr gameState_getMemberStatus
		bcs :+
			rts
		:

		ldy #PartyMember::hpCurrent
		clc
		lda ARGS+0
		adc (CURRENT_PARTYMEMBER),y
		sta (CURRENT_PARTYMEMBER),y
		lda ARGS+1
		adc (CURRENT_PARTYMEMBER),y
		sta (CURRENT_PARTYMEMBER),y

		ldy #PartyMember::hpCurrent+0
		sec
		lda (CURRENT_PARTYMEMBER),y
		ldy #PartyMember::hp+0
		sbc (CURRENT_PARTYMEMBER),y
		ldy #PartyMember::hpCurrent+1
		lda (CURRENT_PARTYMEMBER),y
		ldy #PartyMember::hp+1
		sbc (CURRENT_PARTYMEMBER),y
		bmi :+
			ldy #PartyMember::hp+0
			lda (CURRENT_PARTYMEMBER),y
			ldy #PartyMember::hpCurrent+0
			sta (CURRENT_PARTYMEMBER),y
			ldy #PartyMember::hp+1
			lda (CURRENT_PARTYMEMBER),y
			ldy #PartyMember::hpCurrent+1
			sta (CURRENT_PARTYMEMBER),y
		:

		rts
.endproc

; CURRENT_PARTYMEMBER
.proc removeMemberTimeout
	action = ARGS+0

	ldx #0
	:
		txa
		clc
		adc #PartyMember::timeoutActions
		tay
		lda (CURRENT_PARTYMEMBER),y
		cmp action
		bne skip
			lda #0
			sta (CURRENT_PARTYMEMBER),y
			txa
			asl
			asl
			clc
			adc #PartyMember::timeouts
			tay
			lda #0
			sta (CURRENT_PARTYMEMBER),y
			iny
			sta (CURRENT_PARTYMEMBER),y
			iny
			sta (CURRENT_PARTYMEMBER),y
			iny
			sta (CURRENT_PARTYMEMBER),y
		skip:
		inx
		cpx #10
	bne :-



	rts
.endproc

.export gameState_updateMembersTimersWithClosestEvent
.proc gameState_updateMembersTimersWithClosestEvent
	lda CURRENT_PARTYMEMBER+0
	pha
	lda CURRENT_PARTYMEMBER+1
	pha

	ldx #0
	:
		lda partyMembers_lo,x
		sta CURRENT_PARTYMEMBER+0
		lda partyMembers_hi,x
		sta CURRENT_PARTYMEMBER+1
		ldy #IS_ACTIVE
		txa
		pha
		jsr gameState_getMemberStatus
		pla
		tax
		bcc :+
			txa
			pha
			jsr updateMemberTimersWithClosestEvent
			pla
			tax
		:
		inx
		cpx #6
	bne :--

	pla
	sta CURRENT_PARTYMEMBER+1
	pla
	sta CURRENT_PARTYMEMBER+0
	rts
.endproc

; x=partyMemberIndex
; CURRENT_PARTYMEMBER
.proc updateMemberTimersWithClosestEvent
	lda partyMemberTimer_lo,x
	sta CURRENT_TIMER+0
	lda partyMemberTimer_hi,x
	sta CURRENT_TIMER+1

	; closestTimeout = MAX_INT
	lda #$ff
	sta TMP+0
	sta TMP+1
	sta TMP+2
	sta TMP+3

	ldy #PartyMember::timeouts
	:
		;member.timeouts[i]!=0
		sty YREG
		lda (CURRENT_PARTYMEMBER),y
		iny
		ora (CURRENT_PARTYMEMBER),y
		iny
		ora (CURRENT_PARTYMEMBER),y
		iny
		ora (CURRENT_PARTYMEMBER),y
		iny
		cmp #0
		beq skip
		
		;member.timeouts[i]<closestTimeout
		ldy YREG
		sec
		lda (CURRENT_PARTYMEMBER),y
		iny
		sbc TMP+0
		lda (CURRENT_PARTYMEMBER),y
		iny
		sbc TMP+1
		lda (CURRENT_PARTYMEMBER),y
		iny
		sbc TMP+2
		lda (CURRENT_PARTYMEMBER),y
		iny
		sbc TMP+3
		bcs skip
			ldy YREG
			lda (CURRENT_PARTYMEMBER),y
			sta TMP+0
			iny
			lda (CURRENT_PARTYMEMBER),y
			sta TMP+1
			iny
			lda (CURRENT_PARTYMEMBER),y
			sta TMP+2
			iny
			lda (CURRENT_PARTYMEMBER),y
			sta TMP+3
			iny
		skip:
		cpy #PartyMember::timeouts+4*10
	bne :-

	lda #$ff
	cmp TMP+0
	bne :+
	cmp TMP+1
	bne :+
	cmp TMP+2
	bne :+
	cmp TMP+3
	bne :+
		ldy #Timer::active
		lda #0
		sta (CURRENT_TIMER),y
		rts
	:

	ldy #Timer::timeout
	sec
	lda TMP+0
	sbc tick+0
	sta (CURRENT_TIMER),y
	iny
	lda TMP+1
	sbc tick+1
	sta (CURRENT_TIMER),y
	iny
	lda TMP+2
	sbc tick+2
	sta (CURRENT_TIMER),y
	iny
	lda TMP+3
	sbc tick+3
	sta (CURRENT_TIMER),y

	ldy #Timer::active
	lda #1
	sta (CURRENT_TIMER),y

	jsrf resetTimer

	rts
.endproc

; x = partyMemberIndex
; CURRENT_PARTYMEMBER
.export neutralizePoison 
.proc neutralizePoison
		ldy #PartyMember::status
		lda (CURRENT_PARTYMEMBER),y
		and #(PARTYMEMBER_STATUS_POISONED^$ff)
		sta (CURRENT_PARTYMEMBER),y

		ldy #PartyMember::activeSpells
		lda (CURRENT_PARTYMEMBER),y
		and #(SPELL_SLOW_POISON^$ff)
		sta (CURRENT_PARTYMEMBER),y

		lda #<-34
		sta ARGS+0
		jsr removeMemberTimeout

		rts
.endproc

; x=ATTACKING_PARTYMEMBER_INDEX
; y=ATTACKING_HAND_INDEX
; CURRENT_PARTYMEMBER
; CUR_ITEM
.export gameState_drinkPotion
.proc gameState_drinkPotion
		.pushseg
		.segment "BSS"
			potionIndex: .res 1
		.popseg
		stx ARGS+0
		sty ARGS+1
		jsr gameState_consumeItem
		lda ARGS+0
		sta potionIndex

		ldx #10
		jsrf audio_playSoundEffect

		ldx potionIndex
		dex
		cpx #8
		bcc :+
			inc $d020
			jmp *-3
		:
		lda switch_lo,x
		sta TMP+0
		lda switch_hi,x
		sta TMP+1
		jmp (TMP)

case0: ; Giant strength
			ldx ATTACKING_PARTYMEMBER_INDEX
			jsr gameState_drawMagicalGlimmerAnimation

			ldy #PartyMember::strengthCurrent
			lda #22
			sta (CURRENT_PARTYMEMBER),y
			ldy #PartyMember::extraStrengthCurrent
			lda #0
			sta (CURRENT_PARTYMEMBER),y

			jsr rnd
			and #3
			clc
			adc #1
			sta NUM2
			lda #<546
			sta NUM1+0
			lda #>546
			sta NUM1+1
			jsr multiply

			sta ARGS+0
			sty ARGS+1
			lda #0
			sta ARGS+2
			sta ARGS+3
			lda #PMTA_RESET_GIANT_STRENGTH
			sta ARGS+4
			lda #1
			sta ARGS+5
			ldx ATTACKING_PARTYMEMBER_INDEX
			jsr gameState_setMemberTimeout
		jmp break

case1: ; Healing
			ldx ATTACKING_PARTYMEMBER_INDEX
			jsr gameState_drawMagicalGlimmerAnimation
			lda #2
			sta DICE_ROLLS
			lda #4
			sta DICE_SIDES
			lda #2
			sta DICE_BASE
			jsr rollDice					
			lda DICE_RESULT+0
			sta ARGS+0
			lda DICE_RESULT+1
			sta ARGS+1
			ldx ATTACKING_PARTYMEMBER_INDEX
			jsr gameState_healMember
		jmp break

case2: ; Extra healing
			ldx ATTACKING_PARTYMEMBER_INDEX
			jsr gameState_drawMagicalGlimmerAnimation
			lda #3
			sta DICE_ROLLS
			lda #8
			sta DICE_SIDES
			lda #3
			sta DICE_BASE
			lda DICE_RESULT+0
			sta ARGS+0
			lda DICE_RESULT+1
			sta ARGS+1
			ldx ATTACKING_PARTYMEMBER_INDEX
			jsr gameState_healMember
		jmp break

case3: ; Poison
			ldx ATTACKING_PARTYMEMBER_INDEX
			lda #PARTYMEMBER_STATUS_POISONED
			sta ARGS+0
			lda #<poisonedMsg
			sta ARGS+1
			lda #>poisonedMsg
			sta ARGS+2
			lda #0
			sta ARGS+3
			lda #1
			sta ARGS+4
			lda #PMTA_POISONED_CHARACTER
			sta ARGS+5
			sec
			jsr gameState_setMemberStatus 

			ldy #PartyMember::activeSpells+0
			lda (CURRENT_PARTYMEMBER),y
			and #(SPELL_SLOW_POISON^$ff)
			sta (CURRENT_PARTYMEMBER),y
		jmp break
poisonedMsg:	.byte "poisoned",0

case4: ; Food
			ldx ATTACKING_PARTYMEMBER_INDEX
			jsr gameState_drawMagicalGlimmerAnimation
			ldy #PartyMember::food
			lda #100
			sta (CURRENT_PARTYMEMBER),y
		jmp break

case5: ; Haste
			ldx ATTACKING_PARTYMEMBER_INDEX
			jsr gameState_drawMagicalGlimmerAnimation
			ldy #PartyMember::activeSpells
			lda (CURRENT_PARTYMEMBER),y
			ora #SPELL_HASTE
			sta (CURRENT_PARTYMEMBER),y
		jmp break

case6: ; Invisibility		
			ldx ATTACKING_PARTYMEMBER_INDEX
			jsr gameState_drawMagicalGlimmerAnimation
			ldy #PartyMember::activeSpells
			lda (CURRENT_PARTYMEMBER),y
			ora #SPELL_INVISIBLE
			sta (CURRENT_PARTYMEMBER),y
		jmp break

case7: ; Neutralize poison
			ldx ATTACKING_PARTYMEMBER_INDEX
			jsr gameState_drawMagicalGlimmerAnimation
			jsr neutralizePoison
		jmp break

break:		lda potionIndex
		cmp #3+1
		beq skipMessage
		ldy #PartyMember::status
		lda (CURRENT_PARTYMEMBER),y
		and #2
		bne skipMessage
			lda #1
			sta textColor
			jsr printMemberName
			ldx #<feels
			ldy #>feels
			jsr text_writeNullString
			ldx potionIndex
			ldy feelings_hi,x
			lda feelings_lo,x
			tax
			jsr text_writeNullString
			ldx #<ret
			ldy #>ret
			jsr text_writeNullString

skipMessage:
		ldx ATTACKING_PARTYMEMBER_INDEX
		inc partyMemberStatsChanged,x
		rts

switch_lo:	.byte <case0
		.byte <case1
		.byte <case2
		.byte <case3
		.byte <case4
		.byte <case5
		.byte <case6
		.byte <case7

switch_hi:	.byte >case0
		.byte >case1
		.byte >case2
		.byte >case3
		.byte >case4
		.byte >case5
		.byte >case6
		.byte >case7

feels:		.byte " feels ",0
ret:		.byte "!",$a,0

f00: .byte "",0
f01: .byte "much stronger",0
f02: .byte "better",0
f03: .byte "much better",0
f04: .byte "ill for a moment",0
f05: .byte "no longer hungry",0
f06: .byte "fast and agile",0
f07: .byte "transparent",0
f08: .byte "better",0
feelings_lo: .byte <f00,<f01,<f02,<f03,<f04,<f05,<f06,<f07,<f08
feelings_hi: .byte >f00,>f01,>f02,>f03,>f04,>f05,>f06,>f07,>f08
.endproc

; x=ATTACKING_PARTYMEMBER_INDEX
; y=ATTACKING_HAND_INDEX
; CURRENT_PARTYMEMBER
; CUR_ITEM
.export gameState_useWand
.proc gameState_useWand
		stx XREG
		sty YREG

		ldy #Item::value
		jsr lda_CUR_ITEM_y
		bne :+
			jmp noEffect
		:

		cmp #5
		beq :+
			tax
			lda wandIndexToSpellIndex,x
			sta ARGS+0
			ldx XREG
			ldy YREG
			jmp gameState_useScroll
		:

wandOfSilvias:
		; Wand of Silvias
		ldx #$62
		jsrf audio_playSoundEffect
		jsr gameState_drawBigMagicalGlimmerAnimation
		lda partyPosition+0
		sta ARGS+0
		lda partyPosition+1
		sta ARGS+1
		lda partyDirection
		sta ARGS+2

		; TMP = triggerBitsAndMonsterCount[frontPosition]
		jsr gameState_getForwardPosition
		clc
		lda ARGS+0
		adc #<triggerBitsAndMonsterCount
		sta TMP+0
		lda ARGS+1
		adc #>triggerBitsAndMonsterCount
		sta TMP+1

		; TMP2 = triggerBitsAndMonsterCount[frontFrontPosition]
		; TMP3 = mazeBlock[frontFrontPosition]
		jsr gameState_getForwardPosition
		lda ARGS+1
		sta TMP2+1
		sta TMP3+1
		lda ARGS+0
		sta TMP2+0
		asl
		rol TMP3+1
		asl
		rol TMP3+1
		sta TMP3+0
		clc
		lda TMP2+0
		adc #<triggerBitsAndMonsterCount
		sta TMP2+0
		lda TMP2+1
		adc #>triggerBitsAndMonsterCount
		sta TMP2+1
		clc
		lda TMP3+0
		adc #<maze
		sta TMP3+0
		lda TMP3+1
		adc #>maze
		sta TMP3+1

		lda partyDirection
		eor #2
		tay
		lax (TMP3),y
		lda wallFlags,x
		and #WALLFLAG_BLOCKMONSTER
		bne :+
			jmp noEffect
		:

		ldy #0
		lda (TMP2),y
		and #7
		beq :+
			jmp noEffect
		:

		ldy #0
		lda (TMP),y
		and #7
		bne :+
			jmp noEffect
		:

		lda partyPosition+0
		sta ARGS+0
		lda partyPosition+1
		sta ARGS+1
		lda partyDirection
		sta ARGS+2
		jsr gameState_getForwardPosition
		lda ARGS+0
		sta TMP3+0
		lda ARGS+1
		sta TMP3+1
		jsr gameState_getForwardPosition
		lda ARGS+0
		sta TMP+0
		lda ARGS+1
		sta TMP+1
		lda #<-1
		sta TMP2+0
		ldx #0
		nextMonster:
			lda monster_position_lo,x
			cmp TMP3+0
			bne continue
			lda monster_position_hi,x
			cmp TMP3+1
			bne continue
				txa
				pha
				sta ARGS+0
				jsrf monster_updatePositionAndDirection
				pla
				tax
				inc SHOULDRENDER
			continue:
			inx
			cpx #30
		bne nextMonster

		rts

noEffect:
		lda #1
		sta textColor
		ldx #<noEffectTxt
		ldy #>noEffectTxt
		jsr text_writeNullString
		rts

noEffectTxt: .byte "The wand has no apparent magical effect.",$a,0
wandIndexToSpellIndex: .byte 0, 18, 24, 40, 13, <-1, 4
.endproc
.export gameState_useWandOfSilvias = gameState_useWand::wandOfSilvias

; x=ATTACKING_PARTYMEMBER_INDEX
; y=ATTACKING_HAND_INDEX
; CURRENT_PARTYMEMBER
; CUR_ITEM
.export gameState_eatFood
.proc gameState_eatFood
		ldy #IS_ACTIVE|IS_CONSCIOUS
		jsr gameState_getMemberStatus
		bcs :+
			lda #1
			sta textColor
			jsr printMemberName
			ldx #<notCapable
			ldy #>notCapable
			jsr text_writeNullString
			rts
		:

		clc
		ldy #Item::value
		jsr lda_CUR_ITEM_y
		ldy #PartyMember::food
		adc (CURRENT_PARTYMEMBER),y
		bcc :+
			lda #100
		:
		cmp #101
		bcc :+
			lda #100
		:
		sta (CURRENT_PARTYMEMBER),y

		ldx #9
		jsrf audio_playSoundEffect

		ldx ATTACKING_PARTYMEMBER_INDEX
		inc partyMemberStatsChanged,x
		stx ARGS+0
		ldy ATTACKING_HAND_INDEX
		sty ARGS+1
		jsr gameState_consumeItem

		rts

notCapable:	.byte " isn't capable of eating food!",$a,0
.endproc

; x=ATTACKING_PARTYMEMBER_INDEX
; y=ATTACKING_HAND_INDEX
; CURRENT_PARTYMEMBER
; CUR_ITEM
.export gameState_useScroll
.proc gameState_useScroll
		spellIndex = ARGS+0

		lda SPELLBOOK_TYPE
		sta BACKUP_SPELLBOOK_TYPE
		lda SPELLBOOK_MEMBER_INDEX
		sta BACKUP_SPELLBOOK_MEMBER_INDEX

		tya
		clc
		adc #1
		sta SPELLCASTER_USING_SCROLL
		stx SPELLBOOK_MEMBER_INDEX
		
		lda spellIndex
		cmp #$19-1
		lda #0
		rol
		sta SPELLBOOK_TYPE

		jsrf spells_castSpell

		rts
.endproc

.export printMemberName
.proc printMemberName
		clc
		lda CURRENT_PARTYMEMBER+0
		adc #<PartyMember::name
		tax
		lda CURRENT_PARTYMEMBER+1
		adc #>PartyMember::name
		tay
		jmp text_writeNullString
.endproc

.export gameState_drawBigMagicalGlimmerAnimation
.proc gameState_drawBigMagicalGlimmerAnimation
	lda timersEnabled
	pha
	lda #0
	sta timersEnabled

	; Make sure the frame buffer is in OK condition (shared by the scratch area!)
	jsrf render

	ldx #0
	loop:
		txa
		pha
		ldy #0 ; 0=draw
		jsrf drawBigMagicalGlimmerFrame
		jsrf showFrameBuffer
		pla
		pha
		tax
		ldy #4 ; 4=restore
		jsrf drawBigMagicalGlimmerFrame
		pla
		tax
		inx
		cpx #10
	bne loop

	pla
	sta timersEnabled

	rts
.endproc

; x=member index (-1 == all)
.export gameState_drawMagicalGlimmerAnimation
.proc gameState_drawMagicalGlimmerAnimation
	.pushseg
	.segment "BSS"
		memberBits: .res 2
	.popseg
	cpx #$ff
	beq :+
		lda bits,x
		tax
	:
	lda MEMBER_INVENTORY_OPEN
	bmi :+
		ldx #64 ;6th bit denotes inventory face position
	:
	stx memberBits+1

	lda timersEnabled
	pha
	lda #0
	sta timersEnabled

	ldy #0
	frameLoop:
		bit $d011
		bpl *-3
		bit $d011
		bmi *-3
		lda memberBits+1
		sta memberBits+0
		.repeat 7,I
			lsr memberBits
			bcc :+
				.if I = 6
					ldx MEMBER_INVENTORY_OPEN
					bmi :+
						lda partyMembers_lo,x
						sta CURRENT_PARTYMEMBER+0
						lda partyMembers_hi,x
						sta CURRENT_PARTYMEMBER+1
						tya
						pha
						ldy #PartyMember::status
						lda (CURRENT_PARTYMEMBER),y
						tax
						pla
						tay
						txa
				.else
					lda party+I*.sizeof(PartyMember)+PartyMember::status
				.endif
				and #1
				beq :+
					ldx #I
					jsr doGlimmer
			:
		.endrep
		iny
		cpy #$21
		beq :+
	jmp frameLoop
	:

	pla
	sta timersEnabled

	rts
doGlimmer:
	jsrf magicalGlimmer
	rts

bits:	.byte 1,2,4,8,16,32
.endproc

;boolean mayPutItemInInventory(int itemIndex, int memberIndex, int inventoryPosition)
; CUR_ITEM
; x = inventoryPosition
; CURRENT_MEMBER
.proc mayPutItemInInventory
		ldy #Item::flags
		jsr lda_INV_ITEM_y
		and #ITEM_FLAGS_CURSED
		bne returnFalse

		lda CUR_ITEM+0
		ora CUR_ITEM+1
		beq returnTrue

		cpx #17
		bne :+
			jsr mayMemberUseItem
			bcs :+
				lda #1
				sta textColor
				jsr printMemberName
				ldx #<cantWearThatTypeOfArmor
				ldy #>cantWearThatTypeOfArmor
				jsr text_writeNullString
				jmp returnFalse
		:

		ldy #Item::type
		jsr lda_CUR_ITEM_y
		tay
		jsr items_fetchItemType
		lda inventoryUsage_lo,x
		and itemType+ItemType::inventoryBits+0
		bne returnTrue
		lda inventoryUsage_hi,x
		and itemType+ItemType::inventoryBits+1
		bne returnTrue
		lda #1
		sta textColor
		ldx #<youCantPutThatItemThere
		ldy #>youCantPutThatItemThere
		jsr text_writeNullString

returnFalse:	clc
		rts
returnTrue:	sec
		rts
cantWearThatTypeOfArmor: .byte " can't wear that type of armor.",$a,0
youCantPutThatItemThere: .byte "You can't put that item there.",$a,0
inventoryUsage_hi:
		.byte $00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$FF,$00,$00,$01,$01
inventoryUsage_lo:
		.byte $08,$08,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$01,$02,$04,$20,$40,$10,$FF,$80,$80,$00,$00
.endproc

; Returns item.value in ARGS+0
.export gameState_consumeItem
.proc gameState_consumeItem
		memberIndex = ARGS+0
		inventorySlot = ARGS+1

		;if (inventorySlot==-1)
		lda inventorySlot
		cmp #<-1
		bne :+
			;itemIndex = GameState.pointerItem;
			lda POINTER_ITEM+0
			sta TMP+0
			lda POINTER_ITEM+1
			sta TMP+1
			jmp :++
		:
			;itemIndex = GameState.party[memberIndex].inventory[inventorySlot];
			ldx memberIndex
			lda partyMembers_lo,x
			sta TMP2+0
			lda partyMembers_hi,x
			sta TMP2+1
			lda inventorySlot
			asl
			clc
			adc #PartyMember::inventory
			tay
			lda (TMP2),y
			sta TMP+0
			iny
			lda (TMP2),y
			sta TMP+1
		:

		;item.position=-1;
		ldy #Item::position
		lda #<-1
		jsr sta_TMP_y
		iny
		lda #>-1
		jsr sta_TMP_y

		;if (inventorySlot==-1)
		lda inventorySlot
		cmp #<-1
		bne :+
			;GameState.pointerItem=0;
			lda #0
			sta POINTER_ITEM+0
			sta POINTER_ITEM+1
			jsrf updatePointerSprite
			jmp :++
		:
			;GameState.party[memberIndex].inventory[inventorySlot]=0;
			lda inventorySlot
			asl
			clc
			adc #PartyMember::inventory
			tay
			lda #0
			sta (TMP2),y
			iny
			sta (TMP2),y
			ldx memberIndex
			inc partyMemberStatsChanged,x
		:

		ldy #Item::value
		jsr lda_TMP_y
		sta ARGS+0
		rts
.endproc

.export gameState_consumeItemOnPosition
.proc gameState_consumeItemOnPosition
		position = ARGS+0
		itemType = ARGS+2

		lda itemType
		sta TMP2

		;int topItemIndex = mb.topItemIndex;
		jsrf getTopItem

		;if (topItemIndex==0)
		lda TOP_ITEM+0
		ora TOP_ITEM+1
		bne :+
			rts
		:

		;mb.topItemIndex = 0;
		lda #0
		tay
		sta (TMP),y
		iny
		sta (TMP),y

		;int currentItemIndex = topItemIndex;
		lda TOP_ITEM+0
		sta END_ITEM+0
		sta CUR_ITEM+0
		lda TOP_ITEM+1
		sta END_ITEM+1
		sta CUR_ITEM+1

		loop:
			; Iterate next now and save it
			ldy #Item::next
			jsr lda_CUR_ITEM_y
			pha
			iny
			jsr lda_CUR_ITEM_y
			pha

			;if ((item.type == itemType) || (itemType==-1))
			lda TMP2
			cmp #<-1
			beq match
			ldy #Item::type
			jsr lda_CUR_ITEM_y
			cmp TMP2
			bne noMatch
			match:
				ldy #Item::position
				lda #<-1
				jsr sta_CUR_ITEM_y
				iny
				lda #>-1
				jsr sta_CUR_ITEM_y
				ldy #Item::level
				lda #0
				jsr sta_CUR_ITEM_y
				ldy #Item::next
				jsr sta_CUR_ITEM_y
				iny
				jsr sta_CUR_ITEM_y
				ldy #Item::previous
				jsr sta_CUR_ITEM_y
				iny
				jsr sta_CUR_ITEM_y
				jmp next

			;else if (itemType!=-1)
			noMatch:
				lda #0
				ldy #Item::next
				jsr sta_CUR_ITEM_y
				iny
				jsr sta_CUR_ITEM_y
				ldy #Item::previous
				jsr sta_CUR_ITEM_y
				iny
				jsr sta_CUR_ITEM_y

				jsrf getTopItem
				;[self linkItem:&(mb->topItemIndex) atPosition:position withItemIndex:itemIndex atSubPosition:item.subpos];
				ldy #Item::subpos
				jsr lda_CUR_ITEM_y
				sta ARGS+2
				jsrf items_linkItem
				;mb.topItemIndex = topItemIndexPtr[0];
				ldy #0
				lda TOP_ITEM+0
				sta (TMP),y
				iny
				lda TOP_ITEM+1
				sta (TMP),y

		next:
			; Restore saved next and assign to CUR_ITEM
			pla
			sta CUR_ITEM+1
			pla
			sta CUR_ITEM+0

			lda CUR_ITEM+0
			cmp END_ITEM+0
			bne :+
			lda CUR_ITEM+1
			cmp END_ITEM+1
			bne :+
				rts
			:
		jmp loop
.endproc

; x=inventory_index
; y=member index
; CURRENT_PARTYMEMBER
.export gameState_swapItemInInventory
.proc gameState_swapItemInInventory
		sty SWAPPING_IN_INVENTORY_MEMBER
		lda POINTER_ITEM+0
		sta CUR_ITEM+0
		lda POINTER_ITEM+1
		sta CUR_ITEM+1

		txa
		clc
		asl
		adc #PartyMember::inventory
		tay
		pha
		lda (CURRENT_PARTYMEMBER),y
		sta INV_ITEM+0
		iny
		lda (CURRENT_PARTYMEMBER),y
		sta INV_ITEM+1

		jsr mayPutItemInInventory
		bcs :+
			pla
			rts
		:

		cpx #INVENTORY_QUIVER
		beq quiverHandling
		jmp normalSwap
		quiverHandling:
			;int topItemIndexPtr[] = {member.inventory[16]};
			lda INV_ITEM+0
			sta TOP_ITEM+0
			lda INV_ITEM+1
			sta TOP_ITEM+1
	
			lda POINTER_ITEM+0
			ora POINTER_ITEM+1
			bne putArrowInQuiver
			grabArrowFromQuiver:
				;GameState.pointerItem = GameState.unlinkItem(topItemIndexPtr, 0, -1);
				lda #0
				sta ARGS+0
				jsrf items_unlinkItemBySubPos
				lda CUR_ITEM+0
				sta POINTER_ITEM+0
				lda CUR_ITEM+1
				sta POINTER_ITEM+1
				jsrf printTakenMessage
				jmp quiverDone
				
			putArrowInQuiver:
				;GameState.linkItem(topItemIndexPtr, -2, GameState.pointerItem, 0);
				lda #<-2
				sta ARGS+0
				lda #>-2
				sta ARGS+1
				lda POINTER_ITEM+0
				sta CUR_ITEM+0
				lda POINTER_ITEM+1
				sta CUR_ITEM+1
				lda #0
				sta ARGS+2
				jsrf items_linkItem

				;GameState.pointerItem=0;
				lda #0
				sta POINTER_ITEM+0
				sta POINTER_ITEM+1

			quiverDone:
				;member.inventory[16] = topItemIndexPtr[0];
				pla
				tay
				lda TOP_ITEM+0
				sta (CURRENT_PARTYMEMBER),y
				iny
				lda TOP_ITEM+1
				sta (CURRENT_PARTYMEMBER),y
				jmp render
		normalSwap:
			; Perform swap
			lda INV_ITEM+0
			sta POINTER_ITEM+0
			lda INV_ITEM+1
			sta POINTER_ITEM+1
			pla
			tay
			lda CUR_ITEM+0
			sta (CURRENT_PARTYMEMBER),y
			iny
			lda CUR_ITEM+1
			sta (CURRENT_PARTYMEMBER),y
			jsr calculateMemberAC
			jsrf printTakenMessage

render:		ldx SWAPPING_IN_INVENTORY_MEMBER
		inc partyMemberStatsChanged,x
		jsrf updatePointerSprite
		rts
.endproc

; CURRENT_PARTYMEMBER
.export calculateMemberAC
.proc calculateMemberAC
		jsrf calculateAC

		ldx #4
		slotLoop:
			stx XREG

			;int itemIndex = member.inventory[inventoryPositionsAffectingAC[i]];
			ldy inventoryPositionsAffectingAC,x
			lda (CURRENT_PARTYMEMBER),y
			sta CUR_ITEM+0
			iny
			lda (CURRENT_PARTYMEMBER),y
			sta CUR_ITEM+1
			;if (itemIndex==0)
			ora CUR_ITEM+0
			beq continue 

			;if ((i==2) && (!mayUseItemInHand(memberIndex, 1)))
			cpx #2
			bne :+
				ldx #1
				jsr gameState_mayUseItemInHand
				beq continue
				ldx XREG
			:

			;ItemType itemType = items.itemTypes[item.type];
			ldy #Item::type
			jsr lda_CUR_ITEM_y
			tay

			;if ((itemType.usage&0x7f)!=0)
			jsr items_fetchItemType
			lda itemType+ItemType::usage
			and #$7f
			bne continue

			;int profAllowed = itemType.classBits;
			jsr items_fetchItemType

			;int profMask = (profAllowed & classProfessions[party[memberIndex].clazz]);
			ldy #PartyMember::class
			lda (CURRENT_PARTYMEMBER),y
			tay
			lda itemType+ItemType::classBits
			and classProfessions,y

			;if (profMask==0)
			beq continue

			ldy #Item::type
			jsr lda_CUR_ITEM_y
			tay

			;if ((i>=1) && (i<=2) && (item.type!=0x1b))
			cpx #1
			bcc :+
			cpx #3
			bcs :+
				cpy #ITEMTYPE_SHIELD
				bne continue
			:

			;member.ac += itemType.armourClassModifier;
			jsr items_fetchItemType
			lda itemType+ItemType::armourClassModifier
			ldx XREG
			clc
			ldy #PartyMember::ac
			adc (CURRENT_PARTYMEMBER),y
			pha

			;member.ac -= item.value;
			ldy #Item::value
			jsr lda_CUR_ITEM_y
			sta TMP
			pla
			sec
			sbc TMP
			ldy #PartyMember::ac
			sta (CURRENT_PARTYMEMBER),y

			continue:
			ldx XREG
			dex
		bpl slotLoop

		; If the armor doesnt affect AC through value, then apply ac-modifying rings
		ldy #PartyMember::inventory + INVENTORY_BREAST_ARMOUR*2
		lda (CURRENT_PARTYMEMBER),y
		sta CUR_ITEM+0
		iny
		lda (CURRENT_PARTYMEMBER),y
		sta CUR_ITEM+1
		ora CUR_ITEM+0
		beq :+
		ldy #Item::value
		jsr lda_CUR_ITEM_y
		cmp #0
		bne skipRings
			:
			ldy #PartyMember::inventory + INVENTORY_LEFT_RING*2
			jsr getItemValue
			sta TMP
			ldy #PartyMember::inventory + INVENTORY_RIGHT_RING*2
			jsr getItemValue
			cmp TMP
			bcs :+
				; A < TMP
				lda TMP
			:
			sta TMP

			ldy #PartyMember::ac
			sec
			lda (CURRENT_PARTYMEMBER),y
			sbc TMP
			sta (CURRENT_PARTYMEMBER),y
		skipRings:

		ldy #PartyMember::field_DF
		lda (CURRENT_PARTYMEMBER),y
		beq skipDF
		bmi skipDF
			jsrf getDexterityModifier
			clc
			txa
			adc #6
			ldy #PartyMember::ac
			cmp (CURRENT_PARTYMEMBER),y
			bcs :+
				sta (CURRENT_PARTYMEMBER),y
				jmp skipDF
			:
			lda #0
			ldy #PartyMember::field_DF
			sta (CURRENT_PARTYMEMBER),y
		skipDF:

		ldy #PartyMember::activeSpells+0
		lda (CURRENT_PARTYMEMBER),y
		and #SPELL_SHIELD
		beq :+
			ldy #PartyMember::ac
			lda (CURRENT_PARTYMEMBER),y
			cmp #4
			bcc :+
				lda #4
				sta (CURRENT_PARTYMEMBER),y
		:

		ldy #PartyMember::activeSpells+1
		lda (CURRENT_PARTYMEMBER),y
		and #>SPELL_MAGICAL_VESTMENT
		beq skipMV
			ldx CURRENT_PARTYMEMBER_INDEX
			jsr gameState_getLevelACModifier
			lda #5
			sta TMP
			lda ARGS+0
			cmp #5
			bcc :+
				sbc #5
				tax
				lda div3,x
				clc
				adc TMP
				sta TMP
			:

			lda TMP
			ldy #PartyMember::ac
			cmp (CURRENT_PARTYMEMBER),y
			bcs :+
				sta (CURRENT_PARTYMEMBER),y
			:
		skipMV:

		rts
div3:	.repeat 20,I
			.byte I/3
		.endrep

getItemValue:
		lda (CURRENT_PARTYMEMBER),y
		sta CUR_ITEM+0
		iny
		lda (CURRENT_PARTYMEMBER),y
		sta CUR_ITEM+1
		bne :+
zero:		lda #0
			rts
		:
		ldy #Item::type
		jsr lda_CUR_ITEM_y
		tay
		jsr items_fetchItemType
		lda itemType+ItemType::usage
		and #$7f
		bne zero
		ldy #Item::value
		jsr lda_CUR_ITEM_y
		rts

inventoryPositionsAffectingAC:
		.byte PartyMember::inventory + INVENTORY_BREAST_ARMOUR*2
		.byte PartyMember::inventory + INVENTORY_LEFT_HAND*2
		.byte PartyMember::inventory + INVENTORY_RIGHT_HAND*2
		.byte PartyMember::inventory + INVENTORY_BRACERS*2
		.byte PartyMember::inventory + INVENTORY_HELMET*2
.endproc


; NUM1*NUM2 = y.a (NUM1 = 16bit)
.pushseg
.segment "START"
.export multiply
.proc multiply
	lda #$00
	tay
	beq :+

	doAdd:
		clc
		adc NUM1+0
		tax
		tya
		adc NUM1+1
		tay
		txa
	loop:
		asl NUM1+0
		rol NUM1+1
		:lsr NUM2
		bcs doAdd
	bne loop

	rts
.endproc

; QOUTIENT.REMAINDER = DIVIDEND/DIVISOR
.export divide
.proc divide
	lda #0	        ;preset remainder to 0
	sta REMAINDER
	sta REMAINDER+1
	ldx #16	        ;repeat for each bit: ...
	loop:
		asl DIVIDEND	;dividend lb & hb*2, msb -> Carry
		rol DIVIDEND+1	
		rol REMAINDER	;remainder lb & hb * 2 + msb from carry
		rol REMAINDER+1
		lda REMAINDER
		sec
		sbc DIVISOR	;substract divisor to see if it fits in
		tay	        ;lb result -> Y, for we may need it later
		lda REMAINDER+1
		sbc DIVISOR+1
		bcc :+	;if carry=0 then divisor didnt fit in yet
			sta REMAINDER+1	;else save substraction result as new remainder,
			sty REMAINDER	
			inc QUOTIENT	;and INCrement result cause divisor fit in 1 times
		:
		dex
	bne loop
	rts
.endproc

.export rnd
.proc rnd
	lda seed
        beq doEor
        asl
        beq noEor ;if the input was $80, skip the EOR
        bcc noEor
doEor:  eor #$1d
noEor:  sta seed
	eor $d012
	eor $dc04
	rts
.endproc
.popseg

;[0..255]*1

.export rollDice
.proc rollDice
	lda DICE_BASE
	sta DICE_RESULT+0
	lda #0
	sta DICE_RESULT+1
	lda DICE_ROLLS
	bne :+
		rts
	:
	sta TMP
	lda DICE_SIDES
	bne :+
		rts
	:

	txa
	pha
	:
		jsr rnd

		; Multiply by DICE_SIDES. Result in y.a
		sta NUM1+0
		lda #0
		sta NUM1+1
		lda DICE_SIDES
		sta NUM2
		jsr multiply

		; y+1 so result is [1..DICE_SIDES]
		iny
		tya

		; Accumulate hibyte into DICE_RESULT
		clc
		adc DICE_RESULT+0
		sta DICE_RESULT+0
		lda DICE_RESULT+1
		adc #0
		sta DICE_RESULT+1

		dec TMP
	bne :-

	pla
	tax
	rts
.endproc

.export gameState_wayBlocked
.proc gameState_wayBlocked
	ldx #<wayBlockedString
	ldy #>wayBlockedString
	lda #1
	sta textColor
	jsr text_writeNullString
	ldx #29
	jsrf audio_playSoundEffect
	rts

wayBlockedString:
	.byte "You can't go that way.",$a,0
.endproc

; boolean in ARGS+1
.export gameState_memberHaveRing
.proc gameState_memberHaveRing
	memberIndex = ARGS+0
	ringIndex = ARGS+1

	ldx memberIndex
	lda partyMembers_lo,x
	sta TMP+0
	lda partyMembers_hi,x
	sta TMP+1

	ldy #PartyMember::inventory+INVENTORY_LEFT_RING*2
	jsr checkRing
	bcs returnTrue
	ldy #PartyMember::inventory+INVENTORY_RIGHT_RING*2
	jsr checkRing
	bcs returnTrue
	jmp returnFalse

checkRing:
	lda (TMP),y
	sta CUR_ITEM+0
	iny
	lda (TMP),y
	sta CUR_ITEM+1
	ora CUR_ITEM+0
	beq :++
		ldy #Item::type
		jsr lda_CUR_ITEM_y
		cmp #ITEMTYPE_RING2
		beq :+
		cmp #ITEMTYPE_RING
		bne :++
		:
			ldy #Item::value
			jsr lda_CUR_ITEM_y
			cmp ringIndex
			bne :+
				sec
				rts
	:
	clc
	rts

returnTrue:
	lda #1
	bne return
returnFalse:
	lda #0
return:
	sta ARGS+1
	rts
.endproc

.export gameState_removeCharacterEffect
.proc gameState_removeCharacterEffect
	spell = ARGS+0
	memberIndex = ARGS+1
	showWarning = ARGS+2

	rts
.endproc

.export gameState_removeAllCharacterEffects
.proc gameState_removeAllCharacterEffects
	memberIndex = ARGS+0
	ldx memberIndex
	lda partyMembers_lo,x
	sta CURRENT_PARTYMEMBER+0
	lda partyMembers_hi,x
	sta CURRENT_PARTYMEMBER+1
	rts
.endproc

.export gameState_giveMemberDamage
.proc gameState_giveMemberDamage
	memberIndex = ARGS+0
	damage = ARGS+1 ;+2 16-bit

	.pushseg
	.segment "BSS"
		_memberIndex: .res 1
	.popseg

	lda godMode
	beq :+
		rts
	:

	ldx memberIndex
	stx _memberIndex
	ldy #IS_ACTIVE|IS_ALIVE
	jsr gameState_getMemberStatus
	bcs :+
		rts
	:

	; First reduce temporary HP
	ldy #PartyMember::temporaryHP
	lda (CURRENT_PARTYMEMBER),y
	beq :+
		sec
		sbc damage+0
		bpl :+
			lda #0
	:
	sta (CURRENT_PARTYMEMBER),y
	
	; Reduce HP
	ldy #PartyMember::hpCurrent+0
	sec
	lda (CURRENT_PARTYMEMBER),y
	sbc damage+0
	sta (CURRENT_PARTYMEMBER),y
	iny
	lda (CURRENT_PARTYMEMBER),y
	sbc damage+1
	sta (CURRENT_PARTYMEMBER),y

	; Set damage info
	ldy #PartyMember::damageInfo+0
	lda damage+0
	sta (CURRENT_PARTYMEMBER),y
	iny
	lda damage+1
	sta (CURRENT_PARTYMEMBER),y

	; Check death (>-10)
	ldy #PartyMember::hpCurrent+0
	lda (CURRENT_PARTYMEMBER),y
	sbc #<-9
	iny
	lda (CURRENT_PARTYMEMBER),y
	sbc #>-9
	bpl :+
		lda #>-10
		sta (CURRENT_PARTYMEMBER),y
		dey
		lda #<-10
		sta (CURRENT_PARTYMEMBER),y
		ldy #PartyMember::food
		lda #0
		sta (CURRENT_PARTYMEMBER),y
		ldy #PartyMember::status
		lda (CURRENT_PARTYMEMBER),y
		and #PARTYMEMBER_STATUS_ACTIVE
		sta (CURRENT_PARTYMEMBER),y
		ldx _memberIndex
		inc partyMemberStatsChanged,x
		sta ARGS+0
		jsr gameState_removeAllCharacterEffects
		ldx #22
		jsrf audio_playSoundEffect
	:
	ldx #21
	jsrf audio_playSoundEffect

	; First reduce field_DF
	ldy #PartyMember::field_DF
	lda (CURRENT_PARTYMEMBER),y
	beq :+
		sec
		sbc damage+0
		bpl :+
			lda #1
			sta ARGS+0
			lda _memberIndex
			sta ARGS+1
			lda #1
			sta ARGS+2
			jsrf gameState_removeCharacterEffect
			lda #0
	:
	sta (CURRENT_PARTYMEMBER),y

	ldx _memberIndex
	inc partyMemberStatsChanged,x

	; If unconscious, the close open spellbook if any
	ldy #PartyMember::hpCurrent+1
	lda (CURRENT_PARTYMEMBER),y
	bmi :+
	dey
	lda (CURRENT_PARTYMEMBER),y
	beq :+
	jmp :++
	:
		cpx SPELLBOOK_MEMBER_INDEX
		bne :+
			lda #<-1
			sta SPELLBOOK_MEMBER_INDEX
			jsrf gui_renderControls
	:

	ldx _memberIndex
	lda #18
	sta ARGS+0
	lda #0
	sta ARGS+1
	sta ARGS+2
	sta ARGS+3
	lda #PMTA_RESET_DAMAGE_INFO
	sta ARGS+4
	lda #1
	sta ARGS+5
	jsr gameState_setMemberTimeout

	rts
.endproc

.export gameState_teleportParty
.proc gameState_teleportParty
	position = ARGS+0

	;MonsterTimerHandler.updateMonsters();

	;int oldPosition = GameState.partyPosition;
	lda partyPosition+0
	sta TMP+0
	lda partyPosition+1
	sta TMP+1

	;GameState.partyPosition = newPosition;
	lda ARGS+0
	sta partyPosition+0
	lda ARGS+1
	sta partyPosition+1

	;ScriptParser.executeTrigger(oldPosition, ScriptParser.TRIGGER_PLAYERLEAVE);
	lda TMP+0
	sta ARGS+0
	lda TMP+1
	sta ARGS+1
	lda #TRIGGER_PLAYERLEAVE
	sta ARGS+2
	jsrf scriptParser_executeTrigger

	;ScriptParser.executeTrigger(newPosition, ScriptParser.TRIGGER_PLAYERENTER);
	lda partyPosition+0
	sta ARGS+0
	lda partyPosition+1
	sta ARGS+1
	lda #TRIGGER_PLAYERENTER
	sta ARGS+2
	jsrf scriptParser_executeTrigger

	;MonsterTimerHandler.updateMonsters();
	;levelTimer++;
	;byte_30292=-1;

	inc stepCounter+0
	bne :+
		inc stepCounter+1
	:

	inc SHOULDRENDER
	inc DIDTELEPORT

	;ThrownTimerHandler.checkAllThrownHit();
	jsrf checkAllThrownHit

	rts
.endproc

.export gameState_getForwardPosition
.proc gameState_getForwardPosition
	position = ARGS+0
	direction = ARGS+2

	ldx direction
	clc
	lda position+0
	adc forwardDeltaLo,x
	sta ARGS+0
	lda position+1
	adc forwardDeltaHi,x
	and #$03
	sta ARGS+1

	rts

forwardDeltaLo:
	.byte <-32, <1, <32, <-1
forwardDeltaHi:
	.byte >-32, >1, >32, >-1
.endproc

; boolean true in c if visible
.export gameState_isPositionVisible
.proc gameState_isPositionVisible
	position = ARGS+0
	ldx #17
	:
		lda position+0
		cmp scannedMazePositionsLo,x
		bne :++
		lda position+1
		cmp scannedMazePositionsHi,x
		bne :++
			jsrf calculateWallClip
			bcs :+
				inc SHOULDRENDER
				sec
				rts
			:
			clc
			rts
		:
		dex
	bpl :---
	clc
	rts
.endproc

.export gameState_getFreePositionInDirection
.proc gameState_getFreePositionInDirection
	position = ARGS+0
	direction = ARGS+2

	jsr gameState_getForwardPosition

	lda godMode
	beq :+
		rts
	:

	; TMP = (position*4)&0xfff
	lda position+0
	sta TMP+0
	lda position+1
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

	ldx direction
	ldy forwardFrontWallSide,x
	lax (TMP),y
	lda wallFlags,x
	and #WALLFLAG_PASSPARTY
	bne :+
returnNeg:	lda #<-1
		sta position+0
		lda #>-1
		sta position+1
		rts
	:

	clc
	lda #<triggerBitsAndMonsterCount
	adc position+0
	sta TMP+0
	lda #>triggerBitsAndMonsterCount
	adc position+1
	sta TMP+1
	ldy #0
	lda (TMP),y
	and #7
	bne returnNeg

	rts

forwardFrontWallSide: .byte 2,3,0,1
.endproc

; CURRENT_PARTYMEMBER
; boolean in c if saved or not
.export gameState_checkMemberAttackSaves
.proc gameState_checkMemberAttackSaves
	ldy #PartyMember::attackSaves
	lda (CURRENT_PARTYMEMBER),y
	bne :+
		clc
		rts
	:
	sec
	sbc #1
	sta (CURRENT_PARTYMEMBER),y
	sec
	rts
.endproc

; x = monsterIndex
; return boolean in c
.export gameState_checkMonsterSpellImmunity
.proc gameState_checkMonsterSpellImmunity
	txa
	pha

	ldy monster_typeIndex,x
	lda monsterType_spellImmunity,y
	pha

	lda #1
	sta DICE_ROLLS
	lda #100
	sta DICE_SIDES
	lda #0
	sta DICE_BASE
	jsr rollDice

	pla
	cmp DICE_RESULT+0

	pla
	tax
	rts
.endproc

; CURRENT_THROWN
.export gameState_handleExplosion
.proc gameState_handleExplosion
	lda CURRENT_THROWN+0
	pha
	lda CURRENT_THROWN+1
	pha
	ldy #Thrown::active
	lda #0
	sta (CURRENT_THROWN),y
	jsrf render
	pla
	sta CURRENT_THROWN+1
	pla
	sta CURRENT_THROWN+0
	ldy #Thrown::active
	lda #2
	sta (CURRENT_THROWN),y

	ldx #17
	:
		lda scannedMazePositionsLo,x
		ldy #Thrown::position+0
		cmp (CURRENT_THROWN),y
		bne continue
		lda scannedMazePositionsHi,x
		iny
		cmp (CURRENT_THROWN),y
		beq found
		continue:
		dex
	bpl :-
exit:rts

found:
	cpx #15
	beq exit
	cpx #17
	beq exit

	lda deltaX,x
	txa
	pha
	jsrf calculateWallClip
	pla
	tax
	bcs exit

	txa
	pha
	ldx #$23
	jsrf audio_playSoundEffect
	pla
	tax
	
	clc
	lda deltaX,x
	adc #88
	tay

	lda invertedObjectScanPositionZoom,x
	tax

	jsrf renderExplosion

	rts

deltaX: .byte <-120,<-80 ,<-40 , 0  , 40 , 80 , 120,<-118,<-59 , 0  , 59 , 118,<-98 , 0  , 98 ,<-128, 0 , 128
invertedObjectScanPositionZoom:  .byte 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 1, 1, 1, 0, 0, 0
.endproc

; CURRENT_THROWN is defined
.export gameState_thrownHitParty
.proc gameState_thrownHitParty
	ldy #Thrown::itemOrSpellIndex
	lda (CURRENT_THROWN),y
	sta CUR_ITEM+0
	iny
	lda (CURRENT_THROWN),y
	sta CUR_ITEM+1

	ldy #Thrown::spellGfxIndexOrItemType
	lda (CURRENT_THROWN),y
	tay
	jsr isItemAThrowableOrRangeWeapon
	lda #1
	bcc :+
		ldx #<-1
		clc
		tya
		jsr calculateItemDamage
	:
	sta ARGS+1

	ldy #Thrown::subpos
	lda (CURRENT_THROWN),y
	and #3
	asl
	asl
	ora partyDirection
	tax
	lda objectRelativeSubPositions,x
	pha

	ldx #4
	ldy #IS_ACTIVE
	jsr gameState_getMemberStatus
	bcs :+
	ldx #5
	ldy #IS_ACTIVE
	jsr gameState_getMemberStatus
	bcc :++
	:
		jsr rnd
		and #1
		beq :+
			pla
			clc
			adc #2
			pha
	:
	
	pla
	sta ARGS+0
	tax
	ldy #IS_ACTIVE
	jsr gameState_getMemberStatus
	bcs :+
		lda #0
		bcc return
	:

	lda #0
	sta ARGS+2
	jsr gameState_giveMemberDamage

	lda #1
return:	sta ARGS+0
	rts
.endproc

.export gameState_thrownHitMonster
.proc gameState_thrownHitMonster
	; CURRENT_THROWN is defined
	monsterIndex = ARGS+0

	; if thrown.caster == -1
	ldy #Thrown::caster
	lda (CURRENT_THROWN),y
	cmp #<-1
	bne :+
		lda #0
		sta CURRENT_PARTYMEMBER+0
		sta CURRENT_PARTYMEMBER+1
		jmp hit
	:
	
	; || willCasterHitMonster(thrown.caster, monster, thrown.itemOrSpellIndex, 0)
	sta ATTACKING_PARTYMEMBER_INDEX
	tay
	lda partyMembers_lo,y
	sta CURRENT_PARTYMEMBER+0
	lda partyMembers_hi,y
	sta CURRENT_PARTYMEMBER+1	

	ldy #Thrown::itemOrSpellIndex
	lda (CURRENT_THROWN),y
	sta CUR_ITEM+0
	iny
	lda (CURRENT_THROWN),y
	sta CUR_ITEM+1
	ldx monsterIndex
	clc
	jsr willCasterHitMonster
	bcs hit
	jmp noHit

hit:	; Saving CURRENT_THROWN here. When a monster is damaged there will be an immedate render that will cause this variable to reset
	lda CURRENT_THROWN+0
	pha
	lda CURRENT_THROWN+1
	pha

	;damage = calculatedItemDamage(thrown.spellGfxIndexOrItemTypeIndex) ?
	; calculateItemDamage(thrown.caster, monster, thrown.itemOrSpellIndex, thrown.spellGfxIndexOrItemTypeIndex, false) : 1
	ldy #Thrown::spellGfxIndexOrItemType
	lda (CURRENT_THROWN),y
	tay
	jsr isItemAThrowableOrRangeWeapon
	lda #1
	bcc :+
		ldx monsterIndex
		clc
		tya
		jsr calculateItemDamage
	:
	ldx monsterIndex
	jsr giveMonsterDamage

	pla
	sta CURRENT_THROWN+1
	pla
	sta CURRENT_THROWN+0

	lda #1
	jmp return

noHit:	lda #0
return:	sta ARGS+0
	rts
.endproc

; Result in TMP2
; CURRENT_PARTYMEMBER
.export generateCharacterHitpointsByLevel
.proc generateCharacterHitpointsByLevel
	levelIndex = ARGS+0
	h = DIVIDEND
	m = TMP2
	i = DST

	lda levelIndex
	pha

	ldy #PartyMember::class
	lda (CURRENT_PARTYMEMBER),y
	tax
	ldy #PartyMember::constitutionCurrent
	lda (CURRENT_PARTYMEMBER),y
	tay
	jsr getClassAndConstHitpointsModifier
	sta m

	lda #0
	sta h

	ldx #0
	loop:
		txa
		pha

		lda levelIndex
		lsr levelIndex
		and #1
		beq continue

		clc
		lda h
		adc m
		sta h

		clc
		txa
		adc #PartyMember::level
		tay

		clc
		txa
		stx TMP ;Backup X here, needed below
		adc #6
		tax

		lda (CURRENT_PARTYMEMBER),y
		cmp hpIncrPerLevel,x
		beq _lte
		bcc _lte
		_gt:
			clc
			lda TMP
			adc #12
			tax

			clc
			lda h
			adc hpIncrPerLevel,x
			sta h
			jmp continue
		_lte:
			ldy #PartyMember::class
			lda (CURRENT_PARTYMEMBER),y
			tax
			ldy TMP
			jsr getCharacterClassType
			bmi continue
				lda hpIncrPerLevel,y
				sta DICE_SIDES
				lda #1
				sta DICE_ROLLS
				lda #0
				sta DICE_BASE
				jsr rollDice
				clc
				lda h
				adc DICE_RESULT+0
				sta h
		continue:
		pla
		tax
		inx
		cpx #3
	bne loop

	ldy #PartyMember::class
	lda (CURRENT_PARTYMEMBER),y
	tay
	lda classExperienceDivisor,y
	sta DIVISOR+0

	lda #0
	sta DIVISOR+1
	sta DIVIDEND+1
	jsr divide

	lda QUOTIENT
	bne :+
		lda #1
	:
	sta TMP2

	pla
	sta levelIndex

	rts

hpIncrPerLevel: .byte 10, 4, 8, 6, 10, 10, 9, 10, 9, 10, 9, 9, 3, 1, 2, 2, 3, 3
.endproc

; x = class
; y = levelIndex
; returns in y
.export getCharacterClassType
.proc getCharacterClassType
	; a = x*3+y
	stx TMP
	asl TMP
	txa
	adc TMP
	sty TMP
	adc TMP
	tay
	lda characterClassType,y
	tay
	rts

characterClassType:
	; levelIndex
	.byte <0, <-1, <-1  ;MULTICLASS_FIGHTER
	.byte <5, <-1, <-1  ;MULTICLASS_RANGER
	.byte <4, <-1, <-1  ;MULTICLASS_PALADIN
	.byte <1, <-1, <-1  ;MULTICLASS_MAGE
	.byte <2, <-1, <-1  ;MULTICLASS_CLERIC
	.byte <3, <-1, <-1  ;MULTICLASS_THIEF
	.byte <0, < 2, <-1  ;MULTICLASS_FIGHTER_CLERIC
	.byte <0, < 3, <-1  ;MULTICLASS_FIGHTER_THIEF
	.byte <0, < 1, <-1  ;MULTICLASS_FIGHTER_MAGE
	.byte <0, < 1, < 3  ;MULTICLASS_FIGHTER_MAGE_THIEF
	.byte <3, < 1, <-1  ;MULTICLASS_THIEF_MAGE
	.byte <2, < 3, <-1  ;MULTICLASS_CLERIC_THIEF
	.byte <0, < 2, < 1  ;MULTICLASS_FIGHTER_CLERIC_MAGE
	.byte <5, < 2, <-1  ;MULTICLASS_RANGER_CLERIC
	.byte <2, < 1, <-1  ;MULTICLASS_CLERIC_MAGE
.endproc

; x = class
; y = constitution
; returns in a and x same value
.export getClassAndConstHitpointsModifier
.proc getClassAndConstHitpointsModifier
	lda hpConstModifiers,y
	cmp #3
	bmi :++
		tay
		lda classProfessions,x
		and #IS_FIGHTER
		bne :+
			ldy #2
		:
		tya
	:
	tax
	rts
	hpConstModifiers: .byte <-1, <-3, <-2, <-2, <-1, <-1, <-1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 6, 6, 7, 7
.endproc

; Result in TMP2
.export getHPmodifier
.proc getHPmodifier
	class = ARGS+0
	constitution = ARGS+1
	levelA = ARGS+2
	levelB = ARGS+3
	levelC = ARGS+4

	dec constitution

	;int si = levelAdice.roll() + levelBdice.roll() + levelCdice.roll();
	lda #0
	sta DICE_BASE
	sta TMP2+0
	sta TMP2+1

	lda levelA
	beq :+
		sta DICE_ROLLS
		ldx class
		lda levelAdiceSides,x
		sta DICE_SIDES
		jsr rollDice
		lda DICE_RESULT+0
		sta TMP2+0
		lda DICE_RESULT+1
		sta TMP2+1
	:

	lda levelB
	beq :+
		sta DICE_ROLLS
		ldx class
		lda levelBdiceSides,x
		sta DICE_SIDES
		jsr rollDice
		clc
		lda TMP2+0
		adc DICE_RESULT+0
		sta TMP2+0
		lda TMP2+1
		adc DICE_RESULT+1
		sta TMP2+1
	:

	lda levelC
	beq :+
		sta DICE_ROLLS
		ldx class
		lda levelCdiceSides,x
		sta DICE_SIDES
		jsr rollDice
		clc
		lda TMP2+0
		adc DICE_RESULT+0
		sta TMP2+0
		lda TMP2+1
		adc DICE_RESULT+1
		sta TMP2+1
	:

	;constitution = constitutionModifierTable[constitution];
	ldx constitution
	lda constitutionModifierTable,x

	;if ((constitution > 2) && (levelAdiceSides[clazz] < 10))
	cmp #3
	bmi :+
		ldx class
		ldy levelAdiceSides,x
		cpy #10
		bcs :+
			lda #2
	:
	sta constitution

	;si += levelA*constitution;
	lda levelA
	jsr mulAndAdd

	; if ((levelB!=0) && (levelBdiceSides[clazz]!=0)) 
	lda levelB
	beq :+
		ldx class
		lda levelBdiceSides,x
		beq :+
			lda levelB
			jsr mulAndAdd
	:

	; if ((levelC!=0) && (levelCdiceSides[clazz]!=0)) 
	lda levelC
	beq :+
		ldx class
		lda levelCdiceSides,x
		beq :+
			lda levelC
			jsr mulAndAdd
	:

	lda #3
	sta DIVISOR+0
	lda #0
	sta DIVISOR+1
	ldx class
	lda levelCdiceSides,x
	bne :++
		lda levelBdiceSides,x
		bne :+
			rts
		:
		lda #2
		sta DIVISOR+0
	:
	lda TMP2+0
	sta DIVIDEND+0
	lda TMP2+1
	sta DIVIDEND+1
	jsr divide
	lda QUOTIENT+0
	sta TMP2+0
	lda QUOTIENT+1
	sta TMP2+1

	rts

mulAndAdd:
	sta NUM1+0
	lda constitution
	sta NUM2
	lda #0
	sta NUM1+1
	jsr multiply
	clc
	adc TMP2+0
	sta TMP2+0
	tya
	adc TMP2+1
	sta TMP2+1
	rts

levelAdiceSides: .byte 10,10,10, 4, 8, 6,10,10,10,10, 4, 8,10,10, 8
levelBdiceSides: .byte  0, 0, 0, 0, 0, 0, 8, 6, 4, 4, 6, 6, 8, 8, 4
levelCdiceSides: .byte  0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 4, 0, 0
constitutionModifierTable: .byte <-3, <-2, <-2, <-1, <-1, <-1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 6, 6, 7, 7
.endproc

; CURRENT_PARTYMEMBER
; x = level index
.proc levelUp
	clc
	txa
	adc #PartyMember::level
	tay
	lda (CURRENT_PARTYMEMBER),y
	clc
	adc #1
	sta (CURRENT_PARTYMEMBER),y

	ldy #PartyMember::class
	lda (CURRENT_PARTYMEMBER),y
	sta ARGS+0
	ldy #PartyMember::constitutionCurrent
	lda (CURRENT_PARTYMEMBER),y
	sta ARGS+1
	lda #0
	sta ARGS+2
	sta ARGS+3
	sta ARGS+4
	inc ARGS+2,x	
	jsr getHPmodifier

	ldy #PartyMember::hpCurrent
	lda (CURRENT_PARTYMEMBER),y
	adc TMP2+0
	sta (CURRENT_PARTYMEMBER),y
	iny
	lda (CURRENT_PARTYMEMBER),y
	adc TMP2+1
	sta (CURRENT_PARTYMEMBER),y

	ldy #PartyMember::hp
	lda (CURRENT_PARTYMEMBER),y
	adc TMP2+0
	sta (CURRENT_PARTYMEMBER),y
	iny
	lda (CURRENT_PARTYMEMBER),y
	adc TMP2+1
	sta (CURRENT_PARTYMEMBER),y
	
	ldx MEMBER_GETTING_XP
	inc partyMemberStatsChanged,x

	ldx #$17
	jsrf audio_playSoundEffect

	lda #4
	sta textColor
	jsr printMemberName
	ldx #<gainedLevelTxt
	ldy #>gainedLevelTxt
	jmp text_writeNullString

	rts

gainedLevelTxt: .byte " has gained a level of experience.",$a,0
.endproc

; CURRENT_PARTYMEMBER
; a = real class
; x = level index
.proc checkLevelUp
	.pushseg
	.segment "BSS"
		xpNeededToLevel:	.res 4
		currentXp:		.res 4
	.popseg

	sta YREG

	; Get current level-1 => TMP2
	stx TMP
	clc
	lda #PartyMember::level
	adc TMP
	tay
	lda (CURRENT_PARTYMEMBER),y
	sta TMP2

	; Get xpNeededToLevel
	ldy YREG
	lda experienceLevelPointers_0_lo,y
	sta TMP+0
	lda experienceLevelPointers_0_hi,y
	sta TMP+1
	ldy TMP2
	lda (TMP),y
	sta xpNeededToLevel+0
	ldy YREG
	lda experienceLevelPointers_1_lo,y
	sta TMP+0
	lda experienceLevelPointers_1_hi,y
	sta TMP+1
	ldy TMP2
	lda (TMP),y
	sta xpNeededToLevel+1
	ldy YREG
	lda experienceLevelPointers_2_lo,y
	sta TMP+0
	lda experienceLevelPointers_2_hi,y
	sta TMP+1
	ldy TMP2
	lda (TMP),y
	sta xpNeededToLevel+2
	ldy YREG
	lda experienceLevelPointers_3_lo,y
	sta TMP+0
	lda experienceLevelPointers_3_hi,y
	sta TMP+1
	ldy TMP2
	lda (TMP),y
	sta xpNeededToLevel+3

	;if (experienceNeededToLevel == -1)
	bpl :+
		rts
	:

	;if (partyMember.experience[levelIndex] > experienceNeededToLevel)
	sec
	lda currentXp+0
	sbc xpNeededToLevel+0
	sta currentXp+0
	lda currentXp+1
	sbc xpNeededToLevel+1
	sta currentXp+1
	lda currentXp+2
	sbc xpNeededToLevel+2
	sta currentXp+2
	lda currentXp+3
	sbc xpNeededToLevel+3
	sta currentXp+3
	bmi :+ ;<
	ora currentXp+2
	ora currentXp+1
	ora currentXp+0
	beq :+ ;<=
		;>
		txa
		pha
		jsr levelUp
		pla
		tax
	:
	rts

experienceLevelPointers_0_lo: 	.byte <fighterExperienceLevels_0
				.byte <mageExperienceLevels_0
				.byte <clericExperienceLevels_0
				.byte <theifExperienceLevels_0
				.byte <paladinExperienceLevels_0
				.byte <paladinExperienceLevels_0
experienceLevelPointers_0_hi: 	.byte >fighterExperienceLevels_0
				.byte >mageExperienceLevels_0
				.byte >clericExperienceLevels_0
				.byte >theifExperienceLevels_0
				.byte >paladinExperienceLevels_0
				.byte >paladinExperienceLevels_0

experienceLevelPointers_1_lo: 	.byte <fighterExperienceLevels_1
				.byte <mageExperienceLevels_1
				.byte <clericExperienceLevels_1
				.byte <theifExperienceLevels_1
				.byte <paladinExperienceLevels_1
				.byte <paladinExperienceLevels_1
experienceLevelPointers_1_hi: 	.byte >fighterExperienceLevels_1
				.byte >mageExperienceLevels_1
				.byte >clericExperienceLevels_1
				.byte >theifExperienceLevels_1
				.byte >paladinExperienceLevels_1
				.byte >paladinExperienceLevels_1

experienceLevelPointers_2_lo: 	.byte <fighterExperienceLevels_2
				.byte <mageExperienceLevels_2
				.byte <clericExperienceLevels_2
				.byte <theifExperienceLevels_2
				.byte <paladinExperienceLevels_2
				.byte <paladinExperienceLevels_2
experienceLevelPointers_2_hi: 	.byte >fighterExperienceLevels_2
				.byte >mageExperienceLevels_2
				.byte >clericExperienceLevels_2
				.byte >theifExperienceLevels_2
				.byte >paladinExperienceLevels_2
				.byte >paladinExperienceLevels_2

experienceLevelPointers_3_lo: 	.byte <fighterExperienceLevels_3
				.byte <mageExperienceLevels_3
				.byte <clericExperienceLevels_3
				.byte <theifExperienceLevels_3
				.byte <paladinExperienceLevels_3
				.byte <paladinExperienceLevels_3
experienceLevelPointers_3_hi: 	.byte >fighterExperienceLevels_3
				.byte >mageExperienceLevels_3
				.byte >clericExperienceLevels_3
				.byte >theifExperienceLevels_3
				.byte >paladinExperienceLevels_3
				.byte >paladinExperienceLevels_3

mageExperienceLevels_0:	   .byte (0>>0)&$ff,(2500>>0)&$ff,(5000>>0)&$ff,(10000>>0)&$ff,(20000>>0)&$ff,(40000>>0)&$ff,(60000>>0)&$ff,( 90000>>0)&$ff,(135000>>0)&$ff,(250000>>0)&$ff,(375000>>0)&$ff,$ff
fighterExperienceLevels_0: .byte (0>>0)&$ff,(2000>>0)&$ff,(4000>>0)&$ff,( 8000>>0)&$ff,(16000>>0)&$ff,(32000>>0)&$ff,(64000>>0)&$ff,(125000>>0)&$ff,(250000>>0)&$ff,(500000>>0)&$ff,(750000>>0)&$ff,$ff
clericExperienceLevels_0:  .byte (0>>0)&$ff,(1500>>0)&$ff,(3000>>0)&$ff,( 6000>>0)&$ff,(13000>>0)&$ff,(27500>>0)&$ff,(55000>>0)&$ff,(110000>>0)&$ff,(225000>>0)&$ff,(450000>>0)&$ff,$ff
theifExperienceLevels_0:   .byte (0>>0)&$ff,(1250>>0)&$ff,(2500>>0)&$ff,( 5000>>0)&$ff,(10000>>0)&$ff,(20000>>0)&$ff,(40000>>0)&$ff,( 70000>>0)&$ff,(110000>>0)&$ff,(160000>>0)&$ff,(220000>>0)&$ff,$ff
paladinExperienceLevels_0: .byte (0>>0)&$ff,(2250>>0)&$ff,(4500>>0)&$ff,( 9000>>0)&$ff,(18000>>0)&$ff,(36000>>0)&$ff,(75000>>0)&$ff,(150000>>0)&$ff,(300000>>0)&$ff,(600000>>0)&$ff,(900000>>0)&$ff,$ff
mageExperienceLevels_1:	   .byte (0>>8)&$ff,(2500>>8)&$ff,(5000>>8)&$ff,(10000>>8)&$ff,(20000>>8)&$ff,(40000>>8)&$ff,(60000>>8)&$ff,( 90000>>8)&$ff,(135000>>8)&$ff,(250000>>8)&$ff,(375000>>8)&$ff,$ff
fighterExperienceLevels_1: .byte (0>>8)&$ff,(2000>>8)&$ff,(4000>>8)&$ff,( 8000>>8)&$ff,(16000>>8)&$ff,(32000>>8)&$ff,(64000>>8)&$ff,(125000>>8)&$ff,(250000>>8)&$ff,(500000>>8)&$ff,(750000>>8)&$ff,$ff
clericExperienceLevels_1:  .byte (0>>8)&$ff,(1500>>8)&$ff,(3000>>8)&$ff,( 6000>>8)&$ff,(13000>>8)&$ff,(27500>>8)&$ff,(55000>>8)&$ff,(110000>>8)&$ff,(225000>>8)&$ff,(450000>>8)&$ff,$ff
theifExperienceLevels_1:   .byte (0>>8)&$ff,(1250>>8)&$ff,(2500>>8)&$ff,( 5000>>8)&$ff,(10000>>8)&$ff,(20000>>8)&$ff,(40000>>8)&$ff,( 70000>>8)&$ff,(110000>>8)&$ff,(160000>>8)&$ff,(220000>>8)&$ff,$ff
paladinExperienceLevels_1: .byte (0>>8)&$ff,(2250>>8)&$ff,(4500>>8)&$ff,( 9000>>8)&$ff,(18000>>8)&$ff,(36000>>8)&$ff,(75000>>8)&$ff,(150000>>8)&$ff,(300000>>8)&$ff,(600000>>8)&$ff,(900000>>8)&$ff,$ff
mageExperienceLevels_2:	   .byte (0>>16)&$ff,(2500>>16)&$ff,(5000>>16)&$ff,(10000>>16)&$ff,(20000>>16)&$ff,(40000>>16)&$ff,(60000>>16)&$ff,( 90000>>16)&$ff,(135000>>16)&$ff,(250000>>16)&$ff,(375000>>16)&$ff,$ff
fighterExperienceLevels_2: .byte (0>>16)&$ff,(2000>>16)&$ff,(4000>>16)&$ff,( 8000>>16)&$ff,(16000>>16)&$ff,(32000>>16)&$ff,(64000>>16)&$ff,(125000>>16)&$ff,(250000>>16)&$ff,(500000>>16)&$ff,(750000>>16)&$ff,$ff
clericExperienceLevels_2:  .byte (0>>16)&$ff,(1500>>16)&$ff,(3000>>16)&$ff,( 6000>>16)&$ff,(13000>>16)&$ff,(27500>>16)&$ff,(55000>>16)&$ff,(110000>>16)&$ff,(225000>>16)&$ff,(450000>>16)&$ff,$ff
theifExperienceLevels_2:   .byte (0>>16)&$ff,(1250>>16)&$ff,(2500>>16)&$ff,( 5000>>16)&$ff,(10000>>16)&$ff,(20000>>16)&$ff,(40000>>16)&$ff,( 70000>>16)&$ff,(110000>>16)&$ff,(160000>>16)&$ff,(220000>>16)&$ff,$ff
paladinExperienceLevels_2: .byte (0>>16)&$ff,(2250>>16)&$ff,(4500>>16)&$ff,( 9000>>16)&$ff,(18000>>16)&$ff,(36000>>16)&$ff,(75000>>16)&$ff,(150000>>16)&$ff,(300000>>16)&$ff,(600000>>16)&$ff,(900000>>16)&$ff,$ff
mageExperienceLevels_3:	   .byte (0>>24)&$ff,(2500>>24)&$ff,(5000>>24)&$ff,(10000>>24)&$ff,(20000>>24)&$ff,(40000>>24)&$ff,(60000>>24)&$ff,( 90000>>24)&$ff,(135000>>24)&$ff,(250000>>24)&$ff,(375000>>24)&$ff,$ff
fighterExperienceLevels_3: .byte (0>>24)&$ff,(2000>>24)&$ff,(4000>>24)&$ff,( 8000>>24)&$ff,(16000>>24)&$ff,(32000>>24)&$ff,(64000>>24)&$ff,(125000>>24)&$ff,(250000>>24)&$ff,(500000>>24)&$ff,(750000>>24)&$ff,$ff
clericExperienceLevels_3:  .byte (0>>24)&$ff,(1500>>24)&$ff,(3000>>24)&$ff,( 6000>>24)&$ff,(13000>>24)&$ff,(27500>>24)&$ff,(55000>>24)&$ff,(110000>>24)&$ff,(225000>>24)&$ff,(450000>>24)&$ff,$ff
theifExperienceLevels_3:   .byte (0>>24)&$ff,(1250>>24)&$ff,(2500>>24)&$ff,( 5000>>24)&$ff,(10000>>24)&$ff,(20000>>24)&$ff,(40000>>24)&$ff,( 70000>>24)&$ff,(110000>>24)&$ff,(160000>>24)&$ff,(220000>>24)&$ff,$ff
paladinExperienceLevels_3: .byte (0>>24)&$ff,(2250>>24)&$ff,(4500>>24)&$ff,( 9000>>24)&$ff,(18000>>24)&$ff,(36000>>24)&$ff,(75000>>24)&$ff,(150000>>24)&$ff,(300000>>24)&$ff,(600000>>24)&$ff,(900000>>24)&$ff,$ff

.endproc

; x = member index
; ARGS+0 = experience
.export gameState_giveExperience
.proc gameState_giveExperience
	experience = ARGS+0

	.pushseg
	.segment "BSS"
		xpToGive: .res 2
	.popseg

	stx MEMBER_GETTING_XP
	lda partyMembers_lo,x
	sta CURRENT_PARTYMEMBER+0
	lda partyMembers_hi,x
	sta CURRENT_PARTYMEMBER+1

	lda experience+0
	sta DIVIDEND+0
	lda experience+1
	sta DIVIDEND+1
	lda #0
	sta DIVISOR+1
	ldy #PartyMember::class
	lax (CURRENT_PARTYMEMBER),y
	lda classExperienceDivisor,x
	sta DIVISOR+0
	jsr divide
	lda QUOTIENT+0
	sta xpToGive+0
	lda QUOTIENT+1
	sta xpToGive+1

	.if 0
	ldx MEMBER_GETTING_XP
	inx
	stx textColor
	lda xpToGive+0
	sta ARGS+0
	lda xpToGive+1
	sta ARGS+1
	jsrf text_writeHexShort
	.endif

	ldx #0
	loop:
		ldy #PartyMember::class
		lda (CURRENT_PARTYMEMBER),y
		jsr multiClassToClass
		bmi continue
			pha
			;partyMember.experience[levelIndex] += experience;
			txa
			asl
			asl
			clc
			adc #PartyMember::experience
			tay
			clc
			lda (CURRENT_PARTYMEMBER),y
			adc xpToGive+0
			sta (CURRENT_PARTYMEMBER),y
			sta checkLevelUp::currentXp+0
			iny
			lda (CURRENT_PARTYMEMBER),y
			adc xpToGive+1
			sta (CURRENT_PARTYMEMBER),y
			sta checkLevelUp::currentXp+1
			iny
			lda (CURRENT_PARTYMEMBER),y
			adc #0
			sta (CURRENT_PARTYMEMBER),y
			sta checkLevelUp::currentXp+2
			iny
			lda (CURRENT_PARTYMEMBER),y
			adc #0
			sta (CURRENT_PARTYMEMBER),y
			sta checkLevelUp::currentXp+3

			pla
			jsr checkLevelUp
		continue:
		inx
		cpx #3
	bne loop

	rts
.endproc
classExperienceDivisor: .byte 1, 1, 1, 1, 1, 1, 2, 2, 2, 3, 2, 2, 3, 2, 2

; Called getCharacterClassType in ScummVM
; a = class
; x = levelIndex
; returns real class in a
.proc multiClassToClass
	stx TMP
	asl
	asl
	ora TMP
	tay
	lda multiClassToClassTable,y
	rts

; Called _characterClassType in ScummVM
multiClassToClassTable:
	.byte <0,<-1,<-1,<-1
	.byte <5,<-1,<-1,<-1
	.byte <4,<-1,<-1,<-1
	.byte <1,<-1,<-1,<-1
	.byte <2,<-1,<-1,<-1
	.byte <3,<-1,<-1,<-1
	.byte <0,< 2,<-1,<-1
	.byte <0,< 3,<-1,<-1
	.byte <0,< 1,<-1,<-1
	.byte <0,< 1,< 3,<-1
	.byte <3,< 1,<-1,<-1
	.byte <2,< 3,<-1,<-1
	.byte <0,< 2,< 1,<-1
	.byte <5,< 2,<-1,<-1
	.byte <2,< 1,<-1,<-1
.endproc

.export gameState_givePartyExperience
.proc gameState_givePartyExperience
	.pushseg
	.segment "BSS"
		xpToGive: .res 2
	.popseg
	experience = ARGS+0

	lda experience+0
	sta DIVIDEND+0
	lda experience+1
	sta DIVIDEND+1
	lda #0
	sta DIVISOR+0
	sta DIVISOR+1

	; How many should the XP be split on?
	ldx #0
	:
		txa
		pha
		ldy #IS_ACTIVE|IS_ALIVE
		jsr gameState_getMemberStatus
		pla
		tax
		bcc :+
			inc DIVISOR
		:
		inx
		cpx #6
	bne :--

	jsr divide
	lda QUOTIENT+0
	sta xpToGive+0
	lda QUOTIENT+1
	sta xpToGive+1

	ldx #0
	:
		txa
		pha
		ldy #IS_ACTIVE|IS_ALIVE
		jsr gameState_getMemberStatus
		pla
		tax
		bcc :+
			txa
			pha
			lda xpToGive+0
			sta ARGS+0
			lda xpToGive+1
			sta ARGS+1
			jsr gameState_giveExperience
			pla
			tax
		:
		inx
		cpx #6
	bne :--

	rts
.endproc

.export shouldCastTurnUndead
.proc shouldCastTurnUndead
.pushseg
.segment "BSS"
openBookCasterLevel: .res 1
.popseg
	lda #0
	sta openBookCasterLevel

	lda levelIndex
	cmp #2
	beq :+
	lda levelIndex
	cmp #7
	beq :+
		rts
	:

	lda SPELLBOOK_MEMBER_INDEX
	sta BACKUP_SPELLBOOK_MEMBER_INDEX

	ldx #0
	characterLoop:
		stx CURRENT_PARTYMEMBER_INDEX

		ldy #(IS_ACTIVE|IS_CONSCIOUS|NOT_PARALYZED)
		jsr gameState_getMemberStatus
		bcs :+
			jmp continue
		:

		ldy #(PartyMember::inventory+INVENTORY_LEFT_HAND*2)
		jsr checkHolySymbol
		beq :+
			ldy #(PartyMember::inventory+INVENTORY_RIGHT_HAND*2)
			jsr checkHolySymbol
			beq :+
				jmp continue
		:

		ldy #PartyMember::class
		lda (CURRENT_PARTYMEMBER),y
		sta ARGS+0
		ldy #CLASS_PALADIN
		jsrf gameState_getMaxExpLevelIndex
		lda ARGS+0
		bmi :+
			; It's a paladin!
			clc
			adc #PartyMember::level
			tay
			lda (CURRENT_PARTYMEMBER),y
			cmp openBookCasterLevel
			bmi :+
			beq :+
				sta openBookCasterLevel
				lda CURRENT_PARTYMEMBER_INDEX
				sta SPELLBOOK_MEMBER_INDEX
			jmp skipCheckCleric
		:

		ldy #PartyMember::class
		lda (CURRENT_PARTYMEMBER),y
		sta ARGS+0
		ldy #CLASS_CLERIC
		jsrf gameState_getMaxExpLevelIndex
		lda ARGS+0
		bmi :+
			; It's a cleric!
			clc
			adc #PartyMember::level
			tay
			sec
			lda (CURRENT_PARTYMEMBER),y
			sbc #2
			cmp openBookCasterLevel
			bmi :+
			beq :+
				sta openBookCasterLevel
				lda CURRENT_PARTYMEMBER_INDEX
				sta SPELLBOOK_MEMBER_INDEX
		:
skipCheckCleric:

continue:
		ldx CURRENT_PARTYMEMBER_INDEX
		inx
		cpx #6
		beq :+
	jmp characterLoop
	:

	lda openBookCasterLevel
	beq :+
		jsr turnUndeadSpellHandler	
	:

	lda BACKUP_SPELLBOOK_MEMBER_INDEX
	sta SPELLBOOK_MEMBER_INDEX

	rts

checkHolySymbol:
	lda (CURRENT_PARTYMEMBER),y
	sta CUR_ITEM+0
	iny
	lda (CURRENT_PARTYMEMBER),y
	sta CUR_ITEM+1
	ora CUR_ITEM+0
	bne :+
		lda #1
		rts
	:

	ldy #Item::type
	jsr lda_CUR_ITEM_y
	tay
	jsr items_fetchItemType
	lda itemType+ItemType::usage
	and #$7f
	cmp #ITEMUSAGE_HOLYSYMBOLS
	rts

.if 0
.pushseg
.segment "BSS"
	envAudioTimer: .res 4
.popseg

	lda levelIndex
	cmp #2
	bne bail

	sec
	.repeat 4,I
		lda envAudioTimer+I
		sbc tick+I
	.endrep
	bcc :+
bail:	rts
	:

	dice 1,10,3
	jsrf rollDice

	; *=18
	lda DICE_RESULT+0
	asl
	sta TMP+0
	lda DICE_RESULT+1
	rol
	sta TMP+1
	lda TMP+0
	asl
	rol DICE_RESULT+1
	asl
	rol DICE_RESULT+1
	asl
	rol DICE_RESULT+1
	clc
	adc TMP+0
	sta TMP+0
	lda TMP+1
	adc DICE_RESULT+1
	sta TMP+1

	clc
	lda tick+0
	adc TMP+0
	sta envAudioTimer+0
	lda tick+1
	adc TMP+1
	sta envAudioTimer+1
	lda tick+2
	adc #0
	sta envAudioTimer+2
	lda tick+3
	adc #0
	sta envAudioTimer+3

	rts
.endif
.endproc

.proc turnUndeadSpellHandler
.pushseg
.segment "BSS"
	hit: .res 1
.popseg
	; Find the position in front of the party
	lda partyPosition+0
	sta ARGS+0
	lda partyPosition+1
	sta ARGS+1
	lda partyDirection+0
	sta ARGS+2
	jsr gameState_getForwardPosition

	; Check if there are monsters
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
	bne :+
		rts
	:

	; Yup, got monsters. Can we turn them?!?!?
	lda #CENTER
	sta ARGS+2
	lda #4
	sta ARGS+3
	lda #1
	sta ARGS+4
	sta ARGS+5
	jsr gameState_getMonstersOnPosition

	ldx #0
	stx hit
	monsterLoop:
		txa
		pha

		lda MONSTERS_ON_LOC,x
		bpl :+
			pla
			jmp break
		:

		tax
		ldy monster_typeIndex,x
		lda monsterType_typeBits_lo,y
		and #MONSTER_TYPEBITS_LO_REDUCEDAMAGE
		beq nextMonsterOnLocation

		lda monster_flags,x
		and #MONSTER_FLAGS_TRYCASTTURNSPELL
		bne nextMonsterOnLocation
			; TODO: Check that the skeleton warriors on level 7 actually triggers here
			lda monster_flags,x
			ora #MONSTER_FLAGS_TRYCASTTURNSPELL
			sta monster_flags,x

			jsr turnUndeadHit
			bcc :+
				inc hit
			:

		nextMonsterOnLocation:
		pla
		tax
		inx
		cpx #5
	bne monsterLoop
break:

	lda hit
	beq :+
		ldx SPELLBOOK_MEMBER_INDEX
		lda partyMembers_lo,x
		sta CURRENT_PARTYMEMBER+0
		lda partyMembers_hi,x
		sta CURRENT_PARTYMEMBER+1

		lda #9
		sta textColor
		jsr printMemberName
		ldx #<message
		ldy #>message
		jsr text_writeNullString

		jsr gameState_drawBigMagicalGlimmerAnimation

		ldx #95
		jsrf audio_playSoundEffect
	:

	rts
message:
	.byte " uses the power to turn undead!",$a,0

turnUndeadHit:
	stx TARGETED_MONSTER_INDEX
	; y = monsterTypeIndex

	;	uint8 e = _turnUndeadEffect[_monsterProps[m->type].tuResist * 14 + MIN(casterLevel, 14)];
	ldx monsterType_tuResist,y
	lda mul14,x
	ldx shouldCastTurnUndead::openBookCasterLevel
	cpx #14
	bcc :+
		ldx #14
		clc
	:
	stx TMP
	adc TMP
	tax
	lda turnUndeadEffect,x
	sta TMP
	;	if (e == 0xFF)
	bpl :+
		inc giveMonsterDamage::preventMonsterFlash
		;Deal 500 dmg
		txa
		pha
		lda #250
		jsr giveMonsterDamage
		pla
		tax
		lda #250
		jsr giveMonsterDamage
		dec giveMonsterDamage::preventMonsterFlash
		sec
		rts
	:

	; else if (hitChance < e) {
	dice 1,20,0
	jsrf rollDice
	lda DICE_RESULT
	cmp TMP
	bcs :+
		clc
		rts
	:

	ldx TARGETED_MONSTER_INDEX
	lda #0
	sta monster_state,x
	lda monster_flags,x
	ora #MONSTER_FLAGS_TURNED
	sta monster_flags,x
	lda #40
	sta monster_pause,x
	;m->dir = (getNextMonsterDirection(m->block, _currentBlock) ^ 4) >> 1;
	lda monster_position_lo,x
	sta ARGS+0
	lda monster_position_hi,x
	sta ARGS+1
	lda partyPosition+0
	sta ARGS+2
	lda partyPosition+1
	sta ARGS+3
	jsrf monsterTimerHandler_getRelativeOctant
	lda ARGS+0
	eor #4
	lsr
	sta monster_direction,x
	
	sec
	rts

turnUndeadEffect:
	.byte $0A,$07,$04,$00,$00,$FF,$FF,$FF
	.byte $FF,$FF,$FF,$FF,$FF,$FF,$0D,$0A
	.byte $07,$04,$00,$00,$FF,$FF,$FF,$FF
	.byte $FF,$FF,$FF,$FF,$10,$0D,$0A,$07
	.byte $04,$00,$00,$FF,$FF,$FF,$FF,$FF
	.byte $FF,$FF,$13,$10,$0D,$0A,$07,$04
	.byte $00,$00,$FF,$FF,$FF,$FF,$FF,$FF
	.byte $14,$13,$10,$0D,$0A,$07,$04,$00
	.byte $00,$FF,$FF,$FF,$FF,$FF,$63,$14
	.byte $13,$10,$0D,$0A,$07,$04,$00,$00
	.byte $00,$FF,$FF,$00,$63,$63,$14,$13
	.byte $10,$0D,$0A,$07,$04,$00,$00,$00
	.byte $00,$FF,$63,$63,$63,$14,$13,$10
	.byte $0D,$0A,$07,$04,$04,$00,$00,$00
	.byte $63,$63,$63,$63,$14,$13,$10,$0D
	.byte $0A,$07,$07,$04,$04,$00,$63,$63
	.byte $63,$63,$63,$14,$13,$10,$0D,$0A
	.byte $0A,$07,$07,$04

mul14:
	.repeat 8,I
		.byte I*14
	.endrep
.endproc

; x = memberIndex
; y = flags
; returns c=0 == false, c=1 == true
.export gameState_getMemberStatus
.proc gameState_getMemberStatus
	txa
	bmi false
	pha

	lda partyMembers_lo,x
	sta CURRENT_PARTYMEMBER+0
	lda partyMembers_hi,x
	sta CURRENT_PARTYMEMBER+1
	tya
	tax
	
	and #IS_ACTIVE
	beq :+
		ldy #PartyMember::status
		lda (CURRENT_PARTYMEMBER),y
		and #PARTYMEMBER_STATUS_ACTIVE
		beq false
	:

	txa
	and #IS_ALIVE
	beq :+
		ldy #PartyMember::hpCurrent
		lda (CURRENT_PARTYMEMBER),y
		cmp #<-10
		bne :+
		iny
		lda (CURRENT_PARTYMEMBER),y
		cmp #>-10
		bne :+
		jmp false
	:

	txa
	and #IS_CONSCIOUS
	beq :+
		ldy #PartyMember::hpCurrent+1
		lda (CURRENT_PARTYMEMBER),y
		bmi false
		dey
		lda (CURRENT_PARTYMEMBER),y
		beq false
	:

	txa
	and #NOT_PARALYZED
	beq :+
		ldy #PartyMember::status
		lda (CURRENT_PARTYMEMBER),y
		and #PARTYMEMBER_STATUS_PARALYZED
		bne false
	:

	txa
	and #NOT_POISONED
	beq :+
		ldy #PartyMember::status
		lda (CURRENT_PARTYMEMBER),y
		and #PARTYMEMBER_STATUS_POISONED
		bne false
	:

	pla
	tax
	sec
	rts
false:
	pla
	tax
	clc
	rts
.endproc

.export gameState_throwItem
.proc gameState_throwItem
	caster 		= ARGS+0
	itemIndex 	= ARGS+1
	position 	= ARGS+3
	subpos 		= ARGS+5
	direction 	= ARGS+6
	itemType 	= ARGS+7

	.pushseg
	.segment "BSS"
		_itemIndex:	.res 2
		_caster:	.res 2
	.popseg
	lda itemIndex+0
	sta _itemIndex+0
	lda itemIndex+1
	sta _itemIndex+1

	lda caster
	sta _caster

	lda #<thrown
	sta CURRENT_THROWN+0
	lda #>thrown
	sta CURRENT_THROWN+1
	ldy #Thrown::active
	:
		lda (CURRENT_THROWN),y
		beq :+
		clc
		lda CURRENT_THROWN+0
		adc #<.sizeof(Thrown)
		sta CURRENT_THROWN+0
		tax
		lda CURRENT_THROWN+1
		adc #>.sizeof(Thrown)
		sta CURRENT_THROWN+1

		cmp #>(thrown+.sizeof(Thrown)*10)
		bne :-
		cpx #<(thrown+.sizeof(Thrown)*10)
	bne :-
		; return false;
		clc
		jmp return
	:

	;MazeBlock mb = inf.maze.mazeBlocks[position];
	lda position+0
	asl
	sta TMP+0
	lda position+1
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

	lda _itemIndex+0
	sta CUR_ITEM+0
	lda _itemIndex+1
	sta CUR_ITEM+1

	lda position+0
	sta ARGS+0
	lda position+1
	sta ARGS+1
	lda subpos
	ora #FLYING
	sta ARGS+2

	jsrf items_linkItem

	ldy #0
	lda TOP_ITEM+0
	sta (TMP),y
	iny
	lda TOP_ITEM+1
	sta (TMP),y

	ldy #Thrown::active
	lda #1
	sta (CURRENT_THROWN),y
	ldy #Thrown::movedOnlyWithinSubpos
	sta (CURRENT_THROWN),y
	ldy #Thrown::position
	lda position+0
	sta (CURRENT_THROWN),y
	iny
	lda position+1
	sta (CURRENT_THROWN),y
	ldy #Thrown::subpos
	lda subpos
	sta (CURRENT_THROWN),y
	ldy #Thrown::direction
	lda direction
	sta (CURRENT_THROWN),y
	ldy #Thrown::itemOrSpellIndex
	lda _itemIndex+0
	sta (CURRENT_THROWN),y
	iny
	lda _itemIndex+1
	sta (CURRENT_THROWN),y

	ldy #Thrown::spellGfxIndexOrItemType
	lda itemType
	pha
	sta (CURRENT_THROWN),y
	ldy #Thrown::caster
	lda _caster
	sta (CURRENT_THROWN),y
	ldy #Thrown::flags
	lda #0
	sta (CURRENT_THROWN),y
	ldy #Thrown::range
	lda #12
	sta (CURRENT_THROWN),y
	ldy #Thrown::spellHandler
	lda #0
	sta (CURRENT_THROWN),y
	iny
	sta (CURRENT_THROWN),y
	pla
	ldx #26
	cmp #7
	beq :+
		ldx #11
	:
	jsrf audio_playSoundEffect

	inc SHOULDRENDER

	sec
	lda #0
return:	adc #0
	sta ARGS+0
	rts
.endproc

; x = member index
; c = noForceChange
.export gameState_setMemberStatus
.proc gameState_setMemberStatus
	newStatus = ARGS+0
	message = ARGS+1 ;+2
	multiplier = ARGS+3
	nbrHalfMinutes = ARGS+4
	timeoutAction = ARGS+5

	stx CURRENT_PARTYMEMBER_INDEX
	lda partyMembers_lo,x
	sta CURRENT_PARTYMEMBER+0
	lda partyMembers_hi,x
	sta CURRENT_PARTYMEMBER+1

	;if (((member.status&newStatus)!=0) && noForceChange)
	bcc :+
		ldy #PartyMember::status
		lda (CURRENT_PARTYMEMBER),y
		and newStatus
		beq :+
			rts
	:

	ldy #IS_ACTIVE|IS_ALIVE
	jsr gameState_getMemberStatus
	bcs :+
		rts
	:

	jsr getMemberDamageReductionModifier
	bcc :+
		rts
	:

	ldy #PartyMember::status
	lda newStatus
	ora (CURRENT_PARTYMEMBER),y
	sta (CURRENT_PARTYMEMBER),y
	ldx CURRENT_PARTYMEMBER_INDEX
	inc partyMemberStatsChanged,x

	lda nbrHalfMinutes
	sta NUM2
	lda #<546
	sta NUM1+0
	lda #>546
	sta NUM1+1
	jsr multiply

	lda message+0
	pha
	lda message+1
	pha

	sta ARGS+0
	sty ARGS+1
	lda #0
	sta ARGS+2
	sta ARGS+3
	lda timeoutAction
	sta ARGS+4
	lda #1
	sta ARGS+5
	ldx CURRENT_PARTYMEMBER_INDEX
	jsr gameState_setMemberTimeout

	lda #1
	sta textColor
	jsr printMemberName
	ldx #<_is
	ldy #>_is
	jsr text_writeNullString
	pla
	tay
	pla
	tax
	jsr text_writeNullString
	ldx #<_ret
	ldy #>_ret
	jsr text_writeNullString

	rts
_is:	.byte " is ",0
_ret:	.byte "!",$a,0
.endproc

; CURRENT_PARTYMEMBER
; returns boolean in c
.export gameState_getMemberDamageReductionModifier
gameState_getMemberDamageReductionModifier = getMemberDamageReductionModifier
.proc getMemberDamageReductionModifier
	multiplier = ARGS+3

	.pushseg
	.segment "BSS"
		class: .res 1
	.popseg

	ldy #PartyMember::class
	lda (CURRENT_PARTYMEMBER),y
	ldx #0
	jsr multiClassToClass

	cmp #CLASS_CLERIC
	beq :+
	cmp #CLASS_RANGER
	beq :+
	jmp :++
	:
		lda #CLASS_FIGHTER
	:
	sta class

	ldy #PartyMember::level
	lax (CURRENT_PARTYMEMBER),y
	ldy class
	jsr getDamageReductionModifier
	tax

	ldy #PartyMember::race
	lda (CURRENT_PARTYMEMBER),y
	lsr
	cmp #RACE_DWARF
	beq :+
	cmp #RACE_HALFLING
	beq :+
	jmp :++
	:
		cpx #4
		beq reduceWithConstitution
		cpx #0
		beq reduceWithConstitution
		cpx #11
		beq reduceWithConstitution
		jmp dontReduceWithConstitution
	:
	cmp #RACE_GNOME
	bne :+
		cpx #4
		beq reduceWithConstitution
		cpx #1
		beq reduceWithConstitution
		jmp dontReduceWithConstitution
	:
	jmp dontReduceWithConstitution
reduceWithConstitution:
	txa
	sec
	ldy #PartyMember::constitutionCurrent
	sbc (CURRENT_PARTYMEMBER),y
	tax
dontReduceWithConstitution:
	stx XREG

	lda #1
	sta DICE_ROLLS
	lda #20
	sta DICE_SIDES
	lda #0
	sta DICE_BASE
	jsr rollDice

	lda DICE_RESULT
	cmp XREG
	rts

damageReductionConstitutionModifierTable:
	.byte 0, 0, 0, 0, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5
.endproc

; x = monsterIndex
.export gameState_fearMonster
.proc gameState_fearMonster
		txa
		pha

		lda monster_phase,x
		cmp #MONSTER_STATE_READYHIT
		beq return
		cmp #MONSTER_STATE_INACTIVE
		beq return
		cmp #MONSTER_STATE_ISHIT
		beq return

		lda #MONSTER_STATE_FORWARD
		sta monster_phase,x

		lda monster_position_lo,x
		sta ARGS+0
		lda monster_position_hi,x
		sta ARGS+1
		lda partyPosition+0
		sta ARGS+2
		lda partyPosition+1
		sta ARGS+3
		jsrf monsterTimerHandler_getRelativeOctant
		lda ARGS+0
		eor #4
		lsr
		sta monster_direction,x

		lda monster_flags,x
		ora #MONSTER_FLAGS_TURNED
		sta monster_flags,x

return:		pla
		tax
		rts
.endproc

; x = monsterIndex
; return boolean in c
.export gameState_shouldReduceDamageOnMonster
.proc gameState_shouldReduceDamageOnMonster
		txa
		pha

		;int damageReductionModifier = getDamageReductionModifier(0, monster.type.hpDices, 4);
		ldy monster_typeIndex,x
		ldx monsterType_hpDices,y
		ldy #0
		jsr getDamageReductionModifier
		sta TMP

		lda #1
		sta DICE_ROLLS
		lda #20
		sta DICE_SIDES
		lda #0
		sta DICE_BASE
		jsr rollDice

		lda DICE_RESULT
		cmp TMP

		pla
		tax
		rts
.endproc

; y = class
; x = level
; return modifier in A
.proc getDamageReductionModifier
	multiplier = ARGS+3

	lda classDamageReductionTables_lo,y
	sta TMP+0
	lda classDamageReductionTables_hi,y
	sta TMP+1

	lda maxLevelPerClass,y
	stx DIVIDEND+0
	cmp DIVIDEND+0
	bcs :+
		sta DIVIDEND+0
	:
	lda #0
	sta DIVIDEND+1
	sta DIVISOR+1

	lda classLevelDivisors,y
	sta DIVISOR+0
	sty YREG
	jsr divide
	ldy YREG

	lda multiplier
	sta NUM1+0
	lda #0
	sta NUM1+1
	lda classMultipliers,y
	sta NUM2
	jsr multiply

	clc
	adc QUOTIENT+0
	tay
	lda (TMP),y

	rts

maxLevelPerClass:  .byte 17, 21, 19, 21, 17, 17
classLevelDivisors: .byte 2, 5, 3, 4, 2, 2
classMultipliers:   .byte 10, 5, 7, 6, 10, 10

clericDamageReductionTable: .byte 10, 9, 7, 6, 5, 4, 2, 14, 13, 11, 10, 9, 8, 6, 13, 12, 10, 9, 8, 7, 5, 16, 15, 13, 12, 11, 10, 8, 15, 14, 12, 11, 10, 9, 7
thiefDamageReductionTable: .byte 13, 12, 11, 10, 9, 8, 14, 12, 10, 8, 6, 7, 12, 11, 10, 9, 8, 4, 16, 15, 14, 13, 12, 11, 15, 13, 11, 9, 7, 5
fighterDamageReductionTable: .byte 16, 14, 13, 11, 10, 8, 7, 5, 4, 3, 18, 16, 15, 13, 12, 10, 9, 7, 6, 5, 17, 15, 14, 12, 11, 9, 8, 6, 5, 4, 20, 17, 16, 13, 12, 9, 8, 5, 4, 4, 19, 17, 16, 14, 13, 11, 10, 8, 7, 6
mageDamageReductionTable: .byte 14, 13, 11, 10, 8, 11, 9, 7, 5, 3, 13, 11, 9, 7, 5, 15, 13, 11, 9, 7, 12, 10, 8, 6, 4

classDamageReductionTables_lo:
	.byte <fighterDamageReductionTable
	.byte <mageDamageReductionTable
	.byte <clericDamageReductionTable
	.byte <thiefDamageReductionTable
	.byte <fighterDamageReductionTable
	.byte <fighterDamageReductionTable

classDamageReductionTables_hi:
	.byte >fighterDamageReductionTable
	.byte >mageDamageReductionTable
	.byte >clericDamageReductionTable
	.byte >thiefDamageReductionTable
	.byte >fighterDamageReductionTable
	.byte >fighterDamageReductionTable
.endproc

; AFFECTED_MEMBER_INDEX
.export gameState_setSpellTimeout
.proc gameState_setSpellTimeout
	timeoutA = ARGS+0
	timeoutMultiplier = ARGS+1 ;2
	timeoutB = ARGS+3
	spellIndex = ARGS+4
	reuseSameActionSpot = ARGS+5

	lda timeoutMultiplier+0
	sta NUM1+0
	lda timeoutMultiplier+1
	sta NUM1+1
	clc
	lda timeoutA
	adc timeoutB
	sta NUM2
	jsr multiply
	sta ARGS+0
	sty ARGS+1
	lda #0
	sta ARGS+2
	sta ARGS+3

	sec
	lda #0
	sbc spellIndex
	sta spellIndex

	ldx AFFECTED_MEMBER_INDEX
	jmp gameState_setMemberTimeout
.endproc

; X = partyMemberIndex
.export gameState_setMemberTimeout
.proc gameState_setMemberTimeout
	timeout = ARGS+0
	action = ARGS+4
	reuseSameActionSpot = ARGS+5

	stx TMP
	lda partyMembers_lo,x
	sta CURRENT_PARTYMEMBER+0
	lda partyMembers_hi,x
	sta CURRENT_PARTYMEMBER+1
	lda partyMemberTimer_lo,x
	sta CURRENT_TIMER+0
	lda partyMemberTimer_hi,x
	sta CURRENT_TIMER+1

	;if (!timer.active)
	ldy #Timer::active
	lda (CURRENT_TIMER),y
	bne :+
		;timer.timeout = timeout;
		ldy #Timer::timeout
		lda timeout+0
		sta (CURRENT_TIMER),y
		iny
		lda timeout+1
		sta (CURRENT_TIMER),y
		iny
		lda timeout+2
		sta (CURRENT_TIMER),y
		iny
		lda timeout+3
		sta (CURRENT_TIMER),y

		;[timer reset];
		jsrf resetTimer

		clc
		lda timeout+0
		adc tick+0
		sta timeout+0
		lda timeout+1
		adc tick+1
		sta timeout+1
		lda timeout+2
		adc tick+2
		sta timeout+2
		lda timeout+3
		adc tick+3
		sta timeout+3

		;member.timeouts[0] = timeout+tick;
		ldy #PartyMember::timeouts
		lda timeout+0
		sta (CURRENT_PARTYMEMBER),y
		iny
		lda timeout+1
		sta (CURRENT_PARTYMEMBER),y
		iny
		lda timeout+2
		sta (CURRENT_PARTYMEMBER),y
		iny
		lda timeout+3
		sta (CURRENT_PARTYMEMBER),y
		
		;member.timeoutActions[0] = action;
		ldy #PartyMember::timeoutActions
		lda action
		sta (CURRENT_PARTYMEMBER),y

		;timer.active=YES;
		ldy #Timer::active
		lda #1
		sta (CURRENT_TIMER),y

		rts
	:

	;timeout = tick+timeout;
	clc
	lda timeout+0
	adc tick+0
	sta timeout+0
	lda timeout+1
	adc tick+1
	sta timeout+1
	lda timeout+2
	adc tick+2
	sta timeout+2
	lda timeout+3
	adc tick+3
	sta timeout+3

	;if (timer.absoluteTickTimeout>timeout)
	sec
	ldy #Timer::absoluteTimeout
	lda (CURRENT_TIMER),y
	sbc timeout+0
	iny
	lda (CURRENT_TIMER),y
	sbc timeout+1
	iny
	lda (CURRENT_TIMER),y
	sbc timeout+2
	iny
	lda (CURRENT_TIMER),y
	sbc timeout+3
	bcc :+
		;timer.absoluteTickTimeout = timeout;
		ldy #Timer::absoluteTimeout
		lda timeout+0
		sta (CURRENT_TIMER),y
		iny
		lda timeout+1
		sta (CURRENT_TIMER),y
		iny
		lda timeout+2
		sta (CURRENT_TIMER),y
		iny
		lda timeout+3
		sta (CURRENT_TIMER),y
	:

	;if (reuseSameActionSpot)
	lda reuseSameActionSpot
	beq :++
		; Try find a slot with the same action
		ldx #0
		ldy #PartyMember::timeoutActions
		:
			lda (CURRENT_PARTYMEMBER),y
			cmp action
			beq assignSlot
			inx
			iny
			cpx #10
		bne :-
	:

	; Try find a free slot
	ldx #0
	ldy #PartyMember::timeouts
	:
		lda (CURRENT_PARTYMEMBER),y
		iny
		ora (CURRENT_PARTYMEMBER),y
		iny
		ora (CURRENT_PARTYMEMBER),y
		iny
		ora (CURRENT_PARTYMEMBER),y
		beq assignSlot
		iny
		inx
		cpx #10
	bne :-

	; No slot found
	rts

assignSlot:
	;member.timeouts[i] = timeout;
	txa
	asl
	asl
	clc
	adc #PartyMember::timeouts
	tay
	lda timeout+0
	sta (CURRENT_PARTYMEMBER),y
	iny
	lda timeout+1
	sta (CURRENT_PARTYMEMBER),y
	iny
	lda timeout+2
	sta (CURRENT_PARTYMEMBER),y
	iny
	lda timeout+3
	sta (CURRENT_PARTYMEMBER),y

	;member.timeoutActions[i] = action;
	txa
	clc
	adc #PartyMember::timeoutActions
	tay
	lda action
	sta (CURRENT_PARTYMEMBER),y

	rts
.endproc

.export gameState_getDistance
.proc gameState_getDistance
	positionA = ARGS+0
	positionB = ARGS+2
	ax = TMP+0
	ay = TMP+1
	bx = TMP2+0
	by = TMP2+1
	dx = TMP+0
	dy = TMP+1

	; ax = positionA&0x1f;
	; ay = positionA/0x20;
	lda positionA+0
	sta ay
	and #$1f
	sta ax
	lda positionA+1
	lsr
	ror ay
	lsr
	ror ay
	lsr
	ror ay
	lsr
	ror ay
	lsr
	ror ay

	; bx = positionB&0x1f;
	; by = positionB/0x20;
	lda positionB+0
	sta by
	and #$1f
	sta bx
	lda positionB+1
	lsr
	ror by
	lsr
	ror by
	lsr
	ror by
	lsr
	ror by
	lsr
	ror by
	
	;dx = Math.abs(ax-bx);
	sec
	lda ax
	sbc bx
	bpl :+
		clc
		eor #$ff
		adc #1
	:
	sta dx

	;dy = Math.abs(ay-by);
	sec
	lda ay
	sbc by
	bpl :+
		clc
		eor #$ff
		adc #1
	:
	sta dy

	; '5433345'
	; '4322234'
	; '3211123'
	; '321P123'
	; '3211123'
	; '4322234'
	; '5433345'

	;return Math.max(dx,dy) + Math.min(dx,dy)/2;
	cmp dx
	bcc :+
		; dy >= dx
		lda dx
		lsr
		clc
		adc dy
		sta ARGS+0
		rts
	:
	; dy < dx
	lda dy
	lsr
	clc
	adc dx
	sta ARGS+0
	rts
.endproc

; CURRENT_PARTYMEMBER
; Return in A & ARGS+0
.export gameState_getClassProfession
.proc gameState_getClassProfession
	ldy #PartyMember::class
	lda (CURRENT_PARTYMEMBER),y
	tay
	lda classProfessions,y
	sta ARGS+0
	rts
.endproc

; x = character index
.export gameState_dropCharacter
.proc gameState_dropCharacter
	inc partyMemberStatsChanged,x
	inc partyCompositionChanged

	stx CURRENT_PARTYMEMBER_INDEX
	ldy #IS_ACTIVE
	jsr gameState_getMemberStatus
	bcs :+
		rts
	:

	lda #0
	ldy #PartyMember::status
	sta (CURRENT_PARTYMEMBER),y

	; Drop everything except arrows in the quiver
	ldy #PartyMember::inventory
	:
		sty YREG
		cpy #PartyMember::inventory+INVENTORY_QUIVER*2
		beq :+
			lda (CURRENT_PARTYMEMBER),y
			sta CUR_ITEM+0
			iny
			lda (CURRENT_PARTYMEMBER),y
			sta CUR_ITEM+1
			ora CUR_ITEM+0
			beq :+
				jsrf items_dropItemAtPartyPosition
				ldy YREG
				lda #0
				sta (CURRENT_PARTYMEMBER),y
				iny
				sta (CURRENT_PARTYMEMBER),y
		:
		ldy YREG
		iny
		iny
		cpy #PartyMember::inventory+INVENTORY_SIZE*2
	bne :--

	; Drop arrows in the quiver
	:
		ldy #PartyMember::inventory+INVENTORY_QUIVER*2
		lda (CURRENT_PARTYMEMBER),y
		sta TOP_ITEM+0
		iny
		lda (CURRENT_PARTYMEMBER),y
		sta TOP_ITEM+1
		ora TOP_ITEM+0
		beq :+

		; Unlink
		lda #0
		sta ARGS+0
		jsrf items_unlinkItemBySubPos

		; Update top item
		ldy #PartyMember::inventory+INVENTORY_QUIVER*2
		lda TOP_ITEM+0
		sta (CURRENT_PARTYMEMBER),y
		iny
		lda TOP_ITEM+1
		sta (CURRENT_PARTYMEMBER),y

		; Drop to floor
		jsrf items_dropItemAtPartyPosition
	jmp :-
	:
	ldy #PartyMember::inventory+INVENTORY_QUIVER*2
	lda #0
	sta (CURRENT_PARTYMEMBER),y
	iny
	sta (CURRENT_PARTYMEMBER),y

	; Deactivate member
	ldx CURRENT_PARTYMEMBER_INDEX
	lda partyMemberTimer_lo,x
	sta CURRENT_TIMER+0
	lda partyMemberTimer_hi,x
	sta CURRENT_TIMER+1
	ldy #Timer::active
	lda #0
	sta (CURRENT_TIMER),y

	;if (charIndex < 5)
	cpx #5
	bcs :++
		;exchangeCharacters(charIndex, testCharacter(5, 1) ? 5 : 4);
		txa
		pha
		ldx #5
		ldy #IS_ACTIVE
		jsrf gameState_getMemberStatus
		ldy #4
		bcc :+
			iny
		:
		pla
		tax
		jsr gameState_swapMembers
	:

	rts
.endproc

; x = memberA
; y = memberB
.export gameState_swapMembers 
.proc gameState_swapMembers
	inc partyCompositionChanged

	txa
	pha
	tya
	pha

	; Swap PartyMember structs
	lda partyMembers_lo,x
	sta MSRC+0
	lda partyMembers_hi,x
	sta MSRC+1
	lda partyMembers_lo,y
	sta MDST+0
	lda partyMembers_hi,y
	sta MDST+1

	inc partyMemberStatsChanged,x
	tya
	tax
	inc partyMemberStatsChanged,x

	ldy #1 ;Skip position
	:
		lax (MSRC),y
		lda (MDST),y
		sta (MSRC),y
		txa
		sta (MDST),y
		iny
		cpy #.sizeof(PartyMember)
	bne :-

	; Swap relevant timer stuff (timeout, active and absolute timeout)
	pla
	tay
	pla
	tax

	lda partyMemberTimer_lo,x
	sta MSRC+0
	lda partyMemberTimer_hi,x
	sta MSRC+1
	lda partyMemberTimer_lo,y
	sta MDST+0
	lda partyMemberTimer_hi,y
	sta MDST+1
	ldy #Timer::timeout
	ldx #3
	stx COUNT
	:
		lax (MSRC),y
		lda (MDST),y
		sta (MSRC),y
		txa
		sta (MDST),y
		iny
		dec COUNT
	bpl :-

	ldy #Timer::active ;+absolutTimeout
	ldx #4
	stx COUNT
	:
		lax (MSRC),y
		lda (MDST),y
		sta (MSRC),y
		txa
		sta (MDST),y
		iny
		dec COUNT
	bpl :-

	rts
.endproc

partyMemberTimer_lo:
	.byte <partyMemberTimer0
	.byte <partyMemberTimer1
	.byte <partyMemberTimer2
	.byte <partyMemberTimer3
	.byte <partyMemberTimer4
	.byte <partyMemberTimer5

partyMemberTimer_hi:
	.byte >partyMemberTimer0
	.byte >partyMemberTimer1
	.byte >partyMemberTimer2
	.byte >partyMemberTimer3
	.byte >partyMemberTimer4
	.byte >partyMemberTimer5
