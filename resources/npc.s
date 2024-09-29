.include "global.inc"

.macro clickregion x0,y0,w,h,callback,key,special,modifier,parameter
	.word callback
	.byte x0,y0,x0+w,y0+h
	.byte key,special,modifier
	.byte parameter
.endmacro

.segment "NPC"

; Called by the script engine
; x = sequence index
; Returns boolean in c if script should continue or not
.export npc_runDialogue
.proc npc_runDialogue
	.pushseg
	.segment "BSS"
		.export npcSubSequence
		npcSubSequence: .res 1
	.popseg

	lda #<-1
	sta npcSubSequence

	cpx #8 ; Portal animation
	bne :+
		jsrf portalAnimation
		clc
		jmp done
	:
	cpx #9 ; Ending
	bne :+
		jmpf finale
	:
	cpx #10 ; Password protect
	bne :+
		clc
		jmp done
	:

	txa
	pha
	jsrf render_npcScene
	lda #$ff
	sta screenEnabled
	pla
	tax
	jsr runNpcDialogue
	clc

done:
;	php
	inc SHOULDRENDER
;	jsrf render
;	plp
	rts
.endproc

.proc runNpcDialogue
		.pushseg
		.segment "BSS"
			tmp: .res 1
		.popseg
		lda #0
		sta tmp

		lda #$ff
		sta MOUSEEVENT+MouseEvent::buttons
		sta KEYEVENT+KeyEvent::normalChar

		lda timersEnabled
		pha
		txa
		pha

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

		ldx #SCREEN_MODE_text
		jsrf setGameScreenMode

		lda #$b
		sta $d022
		lda #$f
		sta $d023

		pla
		tax
		cpx #0
		bne :+
			jmp dialogue0
		:
		cpx #1
		bne :+
			jmp dialogue1
		:
		cpx #2
		bne :+
			jmp dialogue2
		:
		cpx #3
		bne :+
			jmp dialogue3
		:
		cpx #4
		bne :+
			jmp dialogue4
		:
		cpx #5
		bne :+
			jmp dialogue5
		:
		cpx #6
		bne :+
			jmp dialogue6
		:
		cpx #7
		bne :+
			jmp dialogue7
		:

done:
		ldx #SCREEN_MODE_normal
		jsrf setGameScreenMode
		jsrf updatePointerSprite
		pla
		sta timersEnabled
		rts


; Dwarf at level 4
dialogue0:
		jsr clear

		ldx #8
		clc
		jsr writeTextMessage
		lda scriptFlags+1
		and #$20
		bne :+
			ldx #<npc11
			ldy #>npc11
			jsr choice
			jmp :++
		:
			ldx #<npc12
			ldy #>npc12
			jsr choice
		:
		cpx #0
		bne :+
			;int npcIndex, int queryJoinTextId, int confirmJoinTextId, int noJoinTextId
			;6, 12, 23, 2

			lda #6
			sta ARGS+0
			lda #12
			sta ARGS+1
			lda #23
			sta ARGS+2
			lda #2
			sta ARGS+3
			jsr npcJoinDialogue

			lda scriptFlags+1
			ora #$40
			sta scriptFlags+1
			jmp done
		:
		cpx #1
		bne :+
			jsr clear
			ldx #1
			sec
			jsr writeTextMessage
			lda scriptFlags+1
			ora #$20
			sta scriptFlags+1
			jmp dialogue0
		:
		jmp done


; Dwarven leader and king at level 5
dialogue1:
		jsr clear

		; Ask the party for help and possibly let Dohrum join the party
		lda scriptFlags+2
		and #$01
		beq part1
			jmp part2
		part1:
			lda scriptFlags+1
			and #$80
			beq :+
				lda #13
				pha
				ldx #0
				jmp :++
			:
				lda scriptFlags+1
				ora #$80
				sta scriptFlags+1
				ldx #3
				clc
				jsr writeTextMessage
				ldx #<npc21
				ldy #>npc21
				jsr choice
				lda #4
				pha
			:

			pla
			cpx #0
			bne :+
				pha
				jsr clear
				pla
				tax
				clc
				jsr writeTextMessage
				ldx #<npc22
				ldy #>npc22
				jsr choice
			:

			jsr clear
			cpx #0
			bne :++
				lda #5
				sta COUNT
				:
					ldx #<55
					ldy #>55
					jsrf items_createItemsFromBaseAtPartyPosition
					dec COUNT
				bpl :-
				ldx #<62
				ldy #>62
				jsrf items_createItemsFromBaseAtPartyPosition
				lda scriptFlags+2
				ora #$1
				sta scriptFlags+2
				ldx #6
				sec
				jsr writeTextMessage
				lda #7
				sta ARGS+0
				lda #7
				sta ARGS+1
				lda #29
				sta ARGS+2
				lda #30
				sta ARGS+3
				jsr npcJoinDialogue
				jmp :++
			:
				ldx #5
				sec
				jsr writeTextMessage		
			:
			inc tmp

		; Check if the party has found Kiergar
		part2:
		lda scriptFlags+2
		cmp #$08
		bne part3
			ldx #0
			:
				lda partyMembers_lo,x
				sta TMP+0
				lda partyMembers_hi,x
				sta TMP+1
				ldy #PartyMember::status
				lda (TMP),y
				and #IS_ACTIVE
				beq :+
					ldy #PartyMember::picture
					lda (TMP),y
					cmp #52
					beq keirgarFound
				:
				inx
				cpx #6
			bne :--
			jmp part3
			keirgarFound:
				jsr clear
				ldx #25
				sec
				jsr writeTextMessage
				jsr clear
				ldx #26
				sec
				jsr writeTextMessage
				lda scriptFlags+2
				ora #$08
				sta scriptFlags+2
				inc tmp

		; Check if the party has the dwarven potion to heal the king
		part3:
		lda scriptFlags+2
		and #$10
		bne part4
			lda #ITEMTYPE_DWARVEN_POTION
			sta ARGS+0
			lda #<-1
			sta ARGS+1
			jsrf items_deletePartyItems
			bcc :+
				lda #0
				sta npcSubSequence
				ldx #1
				jsrf render_npcScene
				jsr clear
				ldx #28
				sec
				jsr writeTextMessage

				ldx #<32
				ldy #>32
				jsrf items_createItemsFromBaseAtPartyPosition

				lda scriptFlags+2
				ora #$10
				sta scriptFlags+2
				inc tmp
			:

		part4:
		lda tmp
		bne :+
			jsr clear
			lda #1
			sta textColor
			ldx #<_wishLuck
			ldy #>_wishLuck
			sec
			jsr text_writeNullString2
		:

		jmp done


; Dwarven cleric at level 5
dialogue2:
		jsr clear
		lda scriptFlags+2
		and #$01
		beq clericTooTired
			lda scriptFlags+2
			and #$02
			beq :+
				ldx #11
				sec
				jsr writeTextMessage
				jmp done
			:

			jsr populateCharacterMenuWithResurrectables
			php
			jsr clear
			ldx #9
			clc
			jsr writeTextMessage
			plp
			bcs :+
				ldx #<npc31
				ldy #>npc31
				jsr choice
				jmp :++
			:
				ldx #<npc32
				ldy #>npc32
				jsr choice
			:
			cpx #0
			bne :+
				jsr healParty
				lda scriptFlags+2
				ora #$02
				sta scriptFlags+2
				jmp done
			:
			cpx #1
			bne :+
				jsr resurrectionSelectDialogue
				lda scriptFlags+2
				ora #$02
				sta scriptFlags+2
				jmp done
			:
			jmp done
		clericTooTired:
		ldx #24
		sec
		jsr writeTextMessage
		jmp done

dialogue3:
		jsr clear
		ldx #18
		clc
		jsr writeTextMessage
		ldx #<npc4
		ldy #>npc4
		jsr choice

		cpx #0
		bne bribe
			; Attack
			lda scriptFlags+2
			ora #$40
			sta scriptFlags+2
			lda scriptFlags+3
			ora #$08
			sta scriptFlags+3
			ldx #0
			:
				lda monster_phase,x
				cmp #MONSTER_STATE_INACTIVE
				bne :+
					lda #MONSTER_STATE_TURNLEFT
					sta monster_phase,x
				:
				inx
				cpx #30
			bne :--
			jmp done

		; Bribe
		bribe:
		lda #ITEMTYPE_EGG
		sta ARGS+0
		lda #<-1
		sta ARGS+1
		jsrf items_deletePartyItems
		lda COUNT
		bcc :+
			; Bribe ok
			jsr clear
			ldx #20
			sec
			jsr writeTextMessage
			lda scriptFlags+2
			ora #$40
			sta scriptFlags+2
			jmp done
		:

		; Bribe failed
		jsr clear
		ldx #19
		sec
		jsr writeTextMessage
		jmp done

dialogue4:
		jsr clear
		ldx #14
		clc
		jsr writeTextMessage
		ldx #<npc5
		ldy #>npc5
		jsr choice
		cpx #0
		bne :+
			; Kill her
			lda scriptFlags+2
			ora #$20
			sta scriptFlags+2
			jmp :++
		:
		cpx #1
		bne :+
			; Hear her
			jsr clear
			ldx #15
			sec
			jsr writeTextMessage
		:
		lda scriptFlags+2
		ora #$80
		sta scriptFlags+2
		jmp done

dialogue5:
		jsr clear
		ldx #16
		clc
		jsr writeTextMessage
		ldx #<npc6
		ldy #>npc6
		jsr choice
		lda scriptFlags+3
		ora #$02
		sta scriptFlags+3
		cpx #0
		beq :+
			jmp done
		:
		jsr clear
		ldx #17
		sec
		jsr writeTextMessage

		ldx #0
		:
			ldy #IS_ACTIVE
			stx CURRENT_PARTYMEMBER_INDEX
			jsrf gameState_getMemberStatus
			bcc :+
				txa
				pha
				ldy #PartyMember::inventory+INVENTORY_LEFT_HAND*2
				jsr maybeDropItem
				ldy #PartyMember::inventory+INVENTORY_RIGHT_HAND*2
				jsr maybeDropItem
				pla
				tax
			:
			inx
			cpx #6
		bne :--
		jmp done
		maybeDropItem:
			lda (CURRENT_PARTYMEMBER),y
			sta CUR_ITEM+0
			iny
			lda (CURRENT_PARTYMEMBER),y
			sta CUR_ITEM+1
			dey
			ora CUR_ITEM+0
			bne :+
				rts
			:
			sty YREG
			ldy #Item::type
			jsr lda_CUR_ITEM_y
			bpl :+
				rts
			:
			ldy YREG
			lda #0
			sta (CURRENT_PARTYMEMBER),y
			iny
			sta (CURRENT_PARTYMEMBER),y
			jsrf items_dropItemAtPartyPosition
			ldx CURRENT_PARTYMEMBER_INDEX
			inc partyMemberStatsChanged,x
			rts

dialogue6:
		jsr clear
		ldx #21
		sec
		jsr writeTextMessage
		lda scriptFlags+3
		ora #$01
		sta scriptFlags+3
		jmp done

dialogue7:
		jsr clear
		ldx #22
		clc
		jsr writeTextMessage
		ldx #<npc7
		ldy #>npc7
		jsr choice
		cpx #2
		bcs :+++
			cpx #0
			bne :+
				lda #8
				sta ARGS+0
				lda #27
				sta ARGS+1
				lda #44
				sta ARGS+2
				lda #45
				sta ARGS+3
				jsr npcJoinDialogue
				jmp :++
			:
				jsr clear
				ldx #31
				sec
				jsr writeTextMessage
			:
			lda scriptFlags+3
			ora #$04
			sta scriptFlags+3
		:
		jmp done

npc11:	.word _tendHisWounds
		.word _talk
		.word _leave

npc12:	.word _tendHisWounds
		.word 0
		.word _leave

npc21:	.word _hearProposal
		.word 0
		.word _leave

npc22:	.word _helpHim
		.word 0
		.word _leave

npc31:	.word _healParty
		.word 0
		.word _leave

npc32:	.word _healParty
		.word _resurrectDead
		.word _leave

npc4:	.word _attack
		.word 0
		.word _bribe

npc5:	.word _killHer
		.word _hearHerOut
		.word _letHerGo

npc6:	.word _surrender
		.word 0
		.word _attack

npc7:	.word _freeHim
		.word _killHim
		.word _leave

_wishLuck:		.byte $a, "     I wish you luck in your travels.",0
_tendHisWounds:	.byte "Tend Wounds",0
_talk:			.byte "    Talk"   ,0
_leave:			.byte "   Leave"   ,0
_hearProposal:	.byte "  Hear Him" ,0
_helpHim:		.byte "  Help Him" ,0
_healParty:		.byte "Heal Party" ,0
_resurrectDead:	.byte " Resurrect" ,0
_attack:		.byte "  Attack!"  ,0
_bribe:			.byte "   Bribe"   ,0
_killHer:		.byte "  Kill Her" ,0
_hearHerOut:	.byte "  Hear Her" ,0
_letHerGo:		.byte " Let Her Go",0
_surrender:		.byte " Surrender" ,0
_freeHim:		.byte " Free Him"  ,0
_killHim:		.byte "  Kill Him" ,0
.endproc

.pushseg
.segment "BSS"
	rrCount: .res 1
	rrIndex: .res 10 ;0..5=party members. $80-$89=npcs
.popseg

.proc healParty
		; Roll 1T3+2
		:jsr rnd
		and #3
		beq :-
		clc
		adc #1
		sta COUNT

		ldx #0
		loop:
			lda COUNT
			bne :+
				rts
			:
			ldy #IS_ACTIVE|IS_ALIVE
			jsrf gameState_getMemberStatus
			bcc continue
				inc partyMemberStatsChanged,x

				ldy #PartyMember::status
				lda (CURRENT_PARTYMEMBER),y
				and #(PARTYMEMBER_STATUS_PARALYZED^$ff)
				sta (CURRENT_PARTYMEMBER),y

				txa
				pha
				jsrf neutralizePoison
				pla
				tax

				sec
				ldy #PartyMember::hpCurrent+0
				lda (CURRENT_PARTYMEMBER),y
				ldy #PartyMember::hp+0
				sbc (CURRENT_PARTYMEMBER),y
				ldy #PartyMember::hpCurrent+1
				lda (CURRENT_PARTYMEMBER),y
				ldy #PartyMember::hp+1
				sbc (CURRENT_PARTYMEMBER),y
				bpl continue

				dec COUNT

				; 1T8 + 9
				jsr rnd
				and #7
				clc
				adc #10
				clc
				ldy #PartyMember::hpCurrent+0
				adc (CURRENT_PARTYMEMBER),y
				sta (CURRENT_PARTYMEMBER),y
				iny
				lda (CURRENT_PARTYMEMBER),y
				adc #0
				sta (CURRENT_PARTYMEMBER),y

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
					ldy #PartyMember::hp+0
					lda (CURRENT_PARTYMEMBER),y
					ldy #PartyMember::hpCurrent+0
					sta (CURRENT_PARTYMEMBER),y
					ldy #PartyMember::hp+1
					lda (CURRENT_PARTYMEMBER),y
					ldy #PartyMember::hpCurrent+1
					sta (CURRENT_PARTYMEMBER),y
				:
			continue:
			inx
			cpx #6
			beq :+
		jmp loop
		:
		rts
.endproc

.proc resurrectionSelectDialogue
.if 0
		ldx #0
		lda #3
		sta rrIndex,x
		inx
		lda #1
		sta rrIndex,x
		inx
		lda #0
		sta rrIndex,x
		inx
		lda #2
		sta rrIndex,x
		inx
		lda #$80
		sta rrIndex,x
		inx
		lda #$83
		sta rrIndex,x
		inx
		lda #$85
		sta rrIndex,x
		inx
		lda #$87
		sta rrIndex,x
		inx
		stx rrCount
.endif

		ldx #<_whichShould
		ldy #>_whichShould
		jsr selectCharacter
		bcs :+
			rts
		:

		bpl :+
			and #$7f
			sta npcSubSequence
			clc
			adc #1 ;+1 because bone item values are 1-indexed compared to the NPC indices
			sta ARGS+1
			lda #ITEMTYPE_BONES
			sta ARGS+0
			jsrf items_deletePartyItems
			ldx #2
			jsrf render_npcScene

			lda npcSubSequence
			sta ARGS+0
			asl
			clc
			adc #32
			sta ARGS+1
			adc #1
			sta ARGS+3
			lda #<-1
			sta ARGS+2
			jsr npcJoinDialogue

			jmp :++
		:
			tax
			lda partyMembers_lo,x
			sta TMP+0
			lda partyMembers_hi,x
			sta TMP+1
			inc partyMemberStatsChanged,x
			ldy #PartyMember::hp+0
			lda (TMP),y
			ldy #PartyMember::hpCurrent+0
			sta (TMP),y
			ldy #PartyMember::hp+1
			lda (TMP),y
			ldy #PartyMember::hpCurrent+1
			sta (TMP),y
		:

		rts
_whichShould:   .byte "Which should be resurrected?",0
.endproc

.proc selectCharacter
		.pushseg
		.segment "BSS"
			page: 	.res 1
			pages:  .res 1
			header: .res 2
		.popseg

		stx header+0
		sty header+1

		lda #0
		sta page
		ldx #1
		lda rrCount
		cmp #7
		bcc :+
			inx
		:
		stx pages

repaint:
		jsr clear
		ldx header+0
		ldy header+1
		lda #1
		sta textColor
		clc
		jsr text_writeNullString2

		ldx #0
		jsr getName
		bcs controls
		ldx #0
		ldy #1
		lda #11
		jsr buttonHelper

		ldx #1
		jsr getName
		bcs controls
		ldx #13
		ldy #1
		lda #12
		jsr buttonHelper

		ldx #2
		jsr getName
		bcs controls
		ldx #27
		ldy #1
		lda #11
		jsr buttonHelper

		ldx #3
		jsr getName
		bcs controls
		ldx #0
		ldy #4
		lda #11
		jsr buttonHelper

		ldx #4
		jsr getName
		bcs controls
		ldx #13
		ldy #4
		lda #12
		jsr buttonHelper

		ldx #5
		jsr getName
		bcs controls
		ldx #27
		ldy #4
		lda #11
		jsr buttonHelper

controls:
		lda page
		cmp #1
		bcc :+
			lda #<previous
			sta TMP+0
			lda #>previous
			sta TMP+1
			ldx #0
			ldy #7
			lda #6
			jsr buttonHelper
			lda #7
			sta $d800+2+23*40
		:

		lda #<cancelTxt
		sta TMP+0
		lda #>cancelTxt
		sta TMP+1
		ldx #15
		ldy #7
		lda #8
		jsr buttonHelper
		lda #7
		sta $d800+17+23*40

		ldx page
		inx
		cpx pages
		bcs :+
			lda #<next
			sta TMP+0
			lda #>next
			sta TMP+1
			ldx #32
			ldy #7
			lda #6
			jsr buttonHelper
			lda #7
			sta $d800+34+23*40
		:

		lda #<clickAreas
		sta CURRENT_CAMP_MENU+0
		lda #>clickAreas
		sta CURRENT_CAMP_MENU+1
		menu:
			jsrf handleMenu
		bcc menu
			cpx #6
			bne :+
				lda page
				beq menu
				dec page
				jmp repaint
			:
			cpx #7
			bne :+
				clc
				rts
			:
			cpx #8
			bne :+
				ldx page
				inx
				cpx pages
				bcs menu
				inc page
				jmp repaint
			:

			txa
			ldx page
			clc
			adc mul6,x
			cmp rrCount
		bcs menu

		tax
		lda rrIndex,x
		sec
		rts

getName:
		txa
		pha

		ldx page
		clc
		adc mul6,x
		tax
		cpx rrCount
		bcs done

		lda rrIndex,x
		bmi :+
			tax
			lda partyMembers_lo,x
			adc #PartyMember::name
			sta TMP+0
			lda partyMembers_hi,x
			adc #0
			sta TMP+1
			jmp :++
		:
			and #$7f
			tax
			lda npcPresets_lo,x
			adc #PartyMember::name
			sta TMP+0
			lda npcPresets_hi,x
			adc #0
			sta TMP+1
		:
		clc
done:
		pla
		tax
		rts
mul6:	.byte 0,6,12,18

previous:		.byte " Prev",0
next:			.byte " Next",0

.pushseg
.segment "CAMP"
clickAreas:
		clickregion  0, 1, 13, 3, chosen, $31,$ff,$00, 0
		clickregion 13, 1, 14, 3, chosen, $32,$ff,$00, 1
		clickregion 27, 1, 13, 3, chosen, $33,$ff,$00, 2
		clickregion  0, 4, 13, 3, chosen, $34,$ff,$00, 3
		clickregion 13, 4, 14, 3, chosen, $35,$ff,$00, 4
		clickregion 27, 4, 13, 3, chosen, $36,$ff,$00, 5
		clickregion  0, 7, 8,  3, chosen, $10,$ff,$00, 6
		clickregion 15, 7, 10, 3, chosen, $03,$ff,$00, 7
		clickregion 32, 7, 8,  3, chosen, $0e,$ff,$00, 8
chosen:	tax ;Callback must be local to the CAMP segment. A-register is not retained cross bank-JSRs so move the result to x
		rts
.popseg
.endproc

.proc populateCharacterMenuWithActiveMembers
		lda #0
		sta rrCount

		ldx #0
		characterLoop:
			ldy #IS_ACTIVE
			jsrf gameState_getMemberStatus
			bcc nextCharacter
				ldy rrCount
				txa
				sta rrIndex,y
				inc rrCount
			nextCharacter:
			inx
			cpx #6
		bne characterLoop

		lda rrCount
		rts
.endproc

.proc populateCharacterMenuWithResurrectables
		lda #0
		sta rrCount

		ldx #0
		characterLoop:
			ldy #IS_ACTIVE
			jsrf gameState_getMemberStatus
			bcc nextCharacter
				; Is the member dead?
				ldy #IS_ALIVE
				jsrf gameState_getMemberStatus
				bcs :+
					ldy rrCount
					txa
					sta rrIndex,y
					inc rrCount
				:

				; Does the member have any bones on them?
				ldy #PartyMember::inventory
				:
					lda (CURRENT_PARTYMEMBER),y
					sta CUR_ITEM+0
					iny
					lda (CURRENT_PARTYMEMBER),y
					sta CUR_ITEM+1
					iny
					sty YREG
					jsr checkItem
					ldy YREG
					cpy #PartyMember::inventory+27*2
				bne :-
			nextCharacter:
			inx
			cpx #6
		bne characterLoop

.if 0 ; Remove pointer item check because the consume bones code will not consume the pointer item => bones duplicated
		; Check pointer item for bones
		lda POINTER_ITEM+0
		sta CUR_ITEM+0
		lda POINTER_ITEM+1
		sta CUR_ITEM+1
		ora CUR_ITEM+0
		beq :+
			jsr checkItem
		:
.endif

		lda rrCount
		cmp #1
		rts

checkItem:
		ldy #Item::type
		jsr lda_CUR_ITEM_y
		tay
		jsr items_fetchItemType
		lda itemType+ItemType::usage
		and #$7f
		cmp #ITEMUSAGE_BONES
		bne :+
			ldy #Item::value
			jsr lda_CUR_ITEM_y
			sec
			sbc #1
			ldy rrCount
			ora #$80
			sta rrIndex,y
			inc rrCount
		:
		rts
.endproc

; Button choice returned in X
.proc choice
		lda ARGS+0
		pha
		stx TMP+0
		sty TMP+1

		ldy #0
		lda (TMP),y
		iny
		ora (TMP),y
		beq :+
			ldx #0
			ldy #7
			lda #11
			sta ARGS+0
			jsrf text_drawButton
			ldx #1
			ldy #8
			jsrf text_setPosition
			lda #1
			sta textColor2
			ldy #0
			lax (TMP),y
			iny
			lda (TMP),y
			tay
			jsr text_writeNullString2
		:

		ldy #2
		lda (TMP),y
		iny
		ora (TMP),y
		beq :+
			ldx #13
			ldy #7
			lda #12
			sta ARGS+0
			jsrf text_drawButton
			ldx #14
			ldy #8
			jsrf text_setPosition
			lda #1
			sta textColor2
			ldy #2
			lax (TMP),y
			iny
			lda (TMP),y
			tay
			jsr text_writeNullString2
		:

		ldy #4
		lda (TMP),y
		iny
		ora (TMP),y
		beq :+
			ldx #27
			ldy #7
			lda #11
			sta ARGS+0
			jsrf text_drawButton
			ldx #28
			ldy #8
			jsrf text_setPosition
			lda #1
			sta textColor2
			ldy #4
			lax (TMP),y
			iny
			lda (TMP),y
			tay
			jsr text_writeNullString2
		:

		lda #<clickAreas
		sta CURRENT_CAMP_MENU+0
		lda #>clickAreas
		sta CURRENT_CAMP_MENU+1
		:
			jsrf handleMenu
		bcc :-

		pla
		sta ARGS+0

		rts

; Hijacking the menu system created in the camp menu. Not the nicest code reuse but what the fuck...
.pushseg
.segment "CAMP"
clickAreas:
		clickregion  0, 6, 13, 3, chosen, $31,$ff,$00, 0
		clickregion 13, 6, 13, 3, chosen, $32,$ff,$00, 1
		clickregion 27, 6, 13, 3, chosen, $33,$ff,$00, 2
		.word 0
chosen:	tax ;Callback must be local to the CAMP segment. A-register is not retained cross bank-JSRs so move the result to x
		rts
.popseg
.endproc

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

.proc npcJoinDialogue
				npcIndex = ARGS+0
				queryJoinTextId = ARGS+1
				confirmJoinTextId = ARGS+2
				noJoinTextId = ARGS+3

				lda textRows2
				pha
				lda #6
				sta textRows2

				jsr clear
				ldx queryJoinTextId
				clc
				jsr writeTextMessage
				ldx #<buttons
				ldy #>buttons
				jsr choice

				cpx #0
				bne noJoin
				join:
					lda confirmJoinTextId
					bpl :+
						jsr clear
						ldx npcIndex
						clc
						lda npcPresets_lo,x
						adc #PartyMember::name
						pha
						lda npcPresets_hi,x
						adc #0
						tay
						pla
						tax
						lda #1
						sta textColor2
						clc
						jsr text_writeNullString2
						ldx #<joinsTxt
						ldy #>joinsTxt
						sec
						jsr text_writeNullString2
						jmp :++
					:
						jsr clear
						sec
						ldx confirmJoinTextId
						jsr writeTextMessage
					:

					lda npcIndex
					pha
					clc
					adc #1 ;+1 because bone item values are 1-indexed compared to the NPC indices
					sta ARGS+1
					lda #33
					sta ARGS+0
					jsr prepareForNewPartyMember
					pla
					bcc :+
						tax
						jsr initNpc
					:

					ldx #1
					jmp done
				
				noJoin:
					jsr clear
					sec
					ldx noJoinTextId
					jsr writeTextMessage
					ldx #0

done:			pla
				sta textRows2
				rts
joinsTxt:		.byte " joins the party.",$a,0
buttons:		.word yesTxt
				.word 0
				.word noTxt
yesTxt:			.byte "    Yes",0
noTxt:			.byte "    No",0
.endproc

; x=xpos
; y=ypos
; a=width
; TMP=txt
.proc buttonHelper
		sta ARGS+0
		txa
		pha
		tya
		pha
		jsrf text_drawButton
		pla
		tay
		iny
		pla
		tax
		inx
		jsrf text_setPosition
		lda #1
		sta textColor2
		ldx TMP+0
		ldy TMP+1
		jmp text_writeNullString2
.endproc

.proc prepareForNewPartyMember
		itemType = ARGS+0
		itemValue = ARGS+1

		jsr populateCharacterMenuWithActiveMembers
		cmp #6
		bne :+
			lda itemType
			pha
			lda itemValue
			pha
			jsr partyFull
			pla
			sta itemValue
			pla
			sta itemType
			bcs :+
				clc
				rts
		:

		; Remove bones for NPC if any
		jsrf items_deletePartyItems

		sec
		rts
.endproc

; Character dropped: c=1
; <=4 members: c=0, x=0
;  >4 members but canceled: c=0, x!=0
.export npc_dropMemberMenu
.proc npc_dropMemberMenu
		jsr populateCharacterMenuWithActiveMembers
		cmp #5
		bcs :+
			ldx #0
			clc
			rts
		:

		ldx #<select
		ldy #>select
		jsr selectCharacter
		bcs :+
			ldx #1
			clc
			rts
		:
		jmp drop

select: .byte "Select the character you wish to drop:",0
.endproc

; Returns true in c if a member was dropped, otherwise false
.proc partyFull
		jsr populateCharacterMenuWithActiveMembers

		ldx #<maxTxt
		ldy #>maxTxt
		jsr selectCharacter
		bcs :+
			rts
		:
		jmp drop

maxTxt: .byte "Party full! Select character to drop:",0
.endproc

.proc drop
		jsrf gameState_dropCharacter
		sec
		rts
.endproc

cancelTxt:.byte " Cancel",0

.proc writeTextMessage
		dex
		jsrf text_writeTextMessage
		rts
.endproc

.proc npc0
		.byte $00, $01
		.byte "Anya",0,0,0,0,0,0,0
        .byte 18, 18, 59, 59,  5,  5, 11, 11, 14, 14, 16, 16,  9,  9
        .word 45, 45
        .byte 10, 0
        .byte RACE_HUMAN*2+SEX_FEMALE, MULTICLASS_FIGHTER, ALIGNMENT_CHAOTIC_GOOD
        .byte 44, 100
        .byte 4, 0, 0
        .dword $00001F40, $00000000, $00000000
        .byte 0
        .res 5*6*2,0
        .dword $00000000
        .res 28*2,0
.endproc

.proc npc1
		.byte $00, $01
		.byte "Beohram",0,0,0,0
        .byte 17, 17,  0,  0,  9,  9, 15, 15, 13, 13, 18, 18, 17, 17
        .word 55, 55
        .byte 10, 0
        .byte RACE_HUMAN*2+SEX_MALE, MULTICLASS_PALADIN, ALIGNMENT_LAWFUL_GOOD
        .byte 45, 100
        .byte 7, 0, 0
        .dword $000130B0, $00000000, $00000000
        .byte 0
        .res 5*6*2,0
        .dword $00000000
        .res 28*2,0
.endproc

.proc npc2
		.byte $00, $01
		.byte "Kirath",0,0,0,0,0
        .byte 11, 11,  0,  0, 17, 17, 13, 13, 18, 18,  8,  8, 12, 12
        .word 20, 20
        .byte 10, 0
        .byte RACE_HALFELF*2+SEX_MALE, MULTICLASS_MAGE, ALIGNMENT_TRUE_NEUTRAL
        .byte 46, 100
        .byte 7, 0, 0
        .dword $00011170, $00000000, $00000000
        .byte 0
        .res 5*6*2,0
        .dword $00B3126B
        .res 28*2,0
.endproc

.proc npc3
		.byte $00, $01
		.byte "Ileria",0,0,0,0,0
        .byte 10, 10,  0,  0, 12, 12,  9,  9, 15, 15, 17, 17, 17, 17
        .word 52, 52
        .byte 10, 0
        .byte RACE_HALFELF*2+SEX_MALE, MULTICLASS_CLERIC, ALIGNMENT_LAWFUL_GOOD
        .byte 47, 100
        .byte 6, 0, 0
        .dword $00004E20, $00000000, $00000000
        .byte 0
        .res 5*6*2,0
        .dword $FFFFFFFF
        .res 28*2,0
.endproc

.proc npc4
		.byte $00, $01
		.byte "Tyrra",0,0,0,0,0,0
		.byte 16, 16,  0,  0, 14, 14, 16, 16, 18, 18, 17, 17,  7,  7
        .word 45, 45
        .byte 10, 0
        .byte RACE_HUMAN*2+SEX_FEMALE, MULTICLASS_RANGER, ALIGNMENT_CHAOTIC_GOOD
        .byte 48, 100
        .byte 6, 0, 0
        .dword $0000CF08, $00000000, $00000000
        .byte 0
        .res 5*6*2,0
        .dword $00000000
        .res 28*2,0
.endproc

.proc npc5
		.byte $00, $01
		.byte "Tod Uphill",0
		.byte 17, 17,  0,  0, 11, 11, 14, 14, 19, 19, 18, 18, 16, 16
        .word 32, 32
        .byte 10, 0
        .byte RACE_HALFLING*2+SEX_MALE, MULTICLASS_THIEF, ALIGNMENT_CHAOTIC_NEUTRAL
        .byte 49, 100
        .byte 5, 0, 0
        .dword $00002D3F, $00000000, $00000000
        .byte 0
        .res 5*6*2,0
        .dword $00000000
        .res 28*2,0
.endproc

.proc npc6
		.byte $00, $01
		.byte "Taghor",0,0,0,0,0
		.byte 17, 17,  0,  0, 11, 11, 15, 15, 15, 15, 19, 19,  9,  9
        .word 3, 45
        .byte 3, 0
        .byte RACE_DWARF*2+SEX_MALE, MULTICLASS_FIGHTER, ALIGNMENT_NEUTRAL_GOOD
        .byte 50, 25
        .byte 5, 0, 0
        .dword $00003F6A, $00000000, $00000000
        .byte 0
        .res 5*6*2,0
        .dword $00000000
        .word $0024, $0000, $0000, $0000, $0000, $0000, $0000, $0000
        .word $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000
        .word $0000, $0029, $0000, $002B, $0000, $0000, $0000, $0000
        .word $0000, $0000, $0000, $0000
.endproc

.proc npc7
		.byte $00, $01
		.byte "Dohrum",0,0,0,0,0
		.byte 18, 18, 29, 29, 13, 13, 11, 11, 16, 16, 17, 17, 14, 14
        .word 28, 28
        .byte 10, 0
        .byte RACE_DWARF*2+SEX_MALE, MULTICLASS_FIGHTER, ALIGNMENT_LAWFUL_GOOD
        .byte 51, 100
        .byte 3, 0, 0
        .dword $000013A0, $00000000, $00000000
        .byte 0
        .res 5*6*2,0
        .dword $00000000
        .word $0024, $0000, $0030, $0037, $0037, $0000, $0000, $0000
        .word $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000
        .word $0000, $0029, $0000, $002B, $0000, $0000, $0000, $0000
        .word $0000, $0000, $0000, $0000
.endproc

.proc npc8
		.byte $00, $01
		.byte "Keirgar",0,0,0,0
		.byte 18, 18, 92, 92, 15, 15, 15, 15, 12, 12, 19, 19, 17, 17
        .word 3, 45
        .byte 3, 0
        .byte RACE_DWARF*2+SEX_MALE, MULTICLASS_FIGHTER, ALIGNMENT_NEUTRAL_GOOD
        .byte 52, 25
        .byte 5, 0, 0
        .dword $00001F40, $00000000, $00000000
        .byte 0
        .res 5*6*2,0
        .dword $00000000
        .res 28*2,0
.endproc

; x = npcIndex
.proc initNpc
		txa
		pha

		ldx #0
		:
			lda partyMembers_lo,x
			sta CURRENT_PARTYMEMBER+0
			lda partyMembers_hi,x
			sta CURRENT_PARTYMEMBER+1
			stx CURRENT_PARTYMEMBER_INDEX
			ldy #PartyMember::status
			lda (CURRENT_PARTYMEMBER),y
			and #IS_ACTIVE
			beq :+
			inx
			cpx #6
		bne :-
			pla
			rts
		:

		ldy #0
		lda #0
		:
			sta (CURRENT_PARTYMEMBER),y
			iny
			cpy #<.sizeof(PartyMember)
		bne :-

		pla
		tax
		lda npcPresets_lo,x
		sta TMP+0
		lda npcPresets_hi,x
		sta TMP+1
		ldy #0
		:
			lda (TMP),y
			sta (CURRENT_PARTYMEMBER),y
			iny
			cpy #<.sizeof(npc0)
		bne :-

		ldx CURRENT_PARTYMEMBER_INDEX
		inc partyMemberStatsChanged,x
		txa
		ldy #PartyMember::position
		sta (CURRENT_PARTYMEMBER),y

		jsrf createInventory
		jsrf calculateMemberAC

		inc partyCompositionChanged

		rts
.endproc

npcPresets_lo:	.byte <npc0,<npc1,<npc2,<npc3,<npc4,<npc5,<npc6,<npc7,<npc8
npcPresets_hi:	.byte >npc0,>npc1,>npc2,>npc3,>npc4,>npc5,>npc6,>npc7,>npc8

.assert .sizeof(npc0) = .sizeof(npc1), error, "0!=1 Wrong NPC size"
.assert .sizeof(npc1) = .sizeof(npc2), error, "1!=2 Wrong NPC size"
.assert .sizeof(npc2) = .sizeof(npc3), error, "2!=3 Wrong NPC size"
.assert .sizeof(npc3) = .sizeof(npc4), error, "3!=4 Wrong NPC size"
.assert .sizeof(npc4) = .sizeof(npc5), error, "4!=5 Wrong NPC size"
.assert .sizeof(npc5) = .sizeof(npc6), error, "5!=6 Wrong NPC size"
.assert .sizeof(npc6) = .sizeof(npc7), error, "6!=7 Wrong NPC size"
.assert .sizeof(npc7) = .sizeof(npc8), error, "7!=8 Wrong NPC size"
