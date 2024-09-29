.include "global.inc"

.segment "MAIN"

; CURRENT_PARTYMEMBER
; a = spellIndex
.proc spellExpired
		pha

		lda #1
		sta textColor
		jsrf printMemberName

		ldx #<text1
		ldy #>text1
		jsr text_writeNullString

		pla
		pha
		tax
		jsrf printSpellName

		ldx #<text2
		ldy #>text2
		jsr text_writeNullString

		lda CURRENT_PARTYMEMBER_INDEX
		sta AFFECTED_MEMBER_INDEX
		lda CURRENT_PARTYMEMBER+0
		sta AFFECTED_MEMBER+0
		lda CURRENT_PARTYMEMBER+1
		sta AFFECTED_MEMBER+1

		pla
		tax
		ldy #SpellType::expiredFunc
		jsrf spells_callSpellHandler

		rts
text1:		.byte "'s ",0
text2:		.byte " Zauber verwirkt.",$a,0
.endproc

; x = mode
; y = handIndex
.proc resetDisabledHandsAndHitInfo
		tya
		pha

		; if ((mode==0) || (mode==2))
		cpx #1
		beq :+
			;member.disabledHands ^= (1<<handIndex);
			clc
			adc #1
			eor #3 ; switched to &= ~x for robustness
			ldy #PartyMember::disabledHands
			and (CURRENT_PARTYMEMBER),y
			sta (CURRENT_PARTYMEMBER),y
		:

		;member.hitInfo[handIndex] = 0;
		pla
		asl
		adc #PartyMember::hitInfo
		tay
		lda #0
		sta (CURRENT_PARTYMEMBER),y
		iny
		sta (CURRENT_PARTYMEMBER),y

		rts
.endproc

.export partyMemberTimerHandler
.proc partyMemberTimerHandler
		.pushseg
		.segment "BSS"
			xreg: .res 1
			currentTimer: .res 2
			partyMemberIndex: .res 1
		.popseg

		ldx ARGS+0
		stx partyMemberIndex
		stx CURRENT_PARTYMEMBER_INDEX
		lda partyMembers_lo,x
		sta CURRENT_PARTYMEMBER+0
		lda partyMembers_hi,x
		sta CURRENT_PARTYMEMBER+1

		lda CURRENT_TIMER+0
		sta currentTimer+0
		lda CURRENT_TIMER+1
		sta currentTimer+1

		ldx #0
		nextActionSlot:
			stx xreg
			txa
			asl
			asl
			clc
			adc #PartyMember::timeouts
			sta TMP
			tay

			; if (member.timeouts[i]==0)
			lda (CURRENT_PARTYMEMBER),y
			iny
			ora (CURRENT_PARTYMEMBER),y
			iny 
			ora (CURRENT_PARTYMEMBER),y
			iny 
			ora (CURRENT_PARTYMEMBER),y
			beq continue

			; if (member.timeouts[i]==GameState.tick)
			ldy TMP
			lda (CURRENT_PARTYMEMBER),y
			cmp tick+0
			bne :+
			iny
			lda (CURRENT_PARTYMEMBER),y
			cmp tick+1
			bne :+
			iny
			lda (CURRENT_PARTYMEMBER),y
			cmp tick+2
			bne :+
			iny
			lda (CURRENT_PARTYMEMBER),y
			cmp tick+3
			beq :++

			; if (member.timeouts[i]>GameState.tick)
			:ldy TMP
			sec
			lda (CURRENT_PARTYMEMBER),y
			sbc tick+0
			iny
			lda (CURRENT_PARTYMEMBER),y
			sbc tick+1
			iny
			lda (CURRENT_PARTYMEMBER),y
			sbc tick+2
			iny
			lda (CURRENT_PARTYMEMBER),y
			sbc tick+3
			bcs continue
			:

			; member.timeouts[i]=0;
			ldy TMP
			lda #0
			sta (CURRENT_PARTYMEMBER),y
			iny
			sta (CURRENT_PARTYMEMBER),y
			iny
			sta (CURRENT_PARTYMEMBER),y
			iny
			sta (CURRENT_PARTYMEMBER),y
			
			txa
			clc
			adc #PartyMember::timeoutActions
			tay

			;if (member.timeoutActions[i]<0)
			lda (CURRENT_PARTYMEMBER),y
			bpl :+
				;spellExpired(memberIndex, -member.timeoutActions[i]);
				eor #$ff
				clc
				adc #1
				jsr spellExpired
				jmp continue
			:

			ldx partyMemberIndex
			inc partyMemberStatsChanged,x

			tax
			lda pmtaHandler_lo,x
			sta TMP+0
			lda pmtaHandler_hi,x
			sta TMP+1
			jmp (TMP)

			continue:

			; Restore context after handler call
			ldx partyMemberIndex
			stx CURRENT_PARTYMEMBER_INDEX
			lda partyMembers_lo,x
			sta CURRENT_PARTYMEMBER+0
			lda partyMembers_hi,x
			sta CURRENT_PARTYMEMBER+1
			lda currentTimer+0
			sta CURRENT_TIMER+0
			lda currentTimer+1
			sta CURRENT_TIMER+1

			ldx xreg
			inx
			cpx #10
			beq :+
		jmp nextActionSlot
		:

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

		;GameState.partyMemberTimers[memberIndex].timeout = closestTimeout-GameState.tick;
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
		
		;if (closestTimeout==0x7fffffff)
		lda #$ff
		cmp TMP+0
		bne :+
		cmp TMP+1
		bne :+
		cmp TMP+2
		bne :+
		cmp TMP+3
		bne :+
			;GameState.partyMemberTimers[memberIndex].active = false;
			ldy #Timer::active
			lda #0
			sta (CURRENT_TIMER),y
		:

		rts

PMTA_RESET_DISABLED_LEFT_HAND_handler:
		ldx #0
		ldy #0
		jsr resetDisabledHandsAndHitInfo
		jmp continue
PMTA_RESET_DISABLED_RIGHT_HAND_handler:
		ldx #0
		ldy #1
		jsr resetDisabledHandsAndHitInfo
		jmp continue
PMTA_RESET_HIT_INFO_LEFT_HAND_handler:
		ldx #1
		ldy #0
		jsr resetDisabledHandsAndHitInfo
		lda #PMTA_RESET_DISABLED_AND_HIT_INFO_LEFT_HAND
		sta ARGS+4
		jsr setNewTimeout
		jmp continue
PMTA_RESET_HIT_INFO_RIGHT_HAND_handler:
		ldx #1
		ldy #1
		jsr resetDisabledHandsAndHitInfo
		lda #PMTA_RESET_DISABLED_AND_HIT_INFO_RIGHT_HAND
		sta ARGS+4
		jsr setNewTimeout
		jmp continue

setNewTimeout:	;int timeout = ((member.activeSpells&PartyMember.SPELL_HASTE)==0) ? 36 : 18;
		ldx #36
		ldy #PartyMember::activeSpells+0
		lda (CURRENT_PARTYMEMBER),y
		and #<SPELL_HASTE
		beq :+
			ldx #18
		:
		lda partyMemberIndex
		pha

		;GameState.setMemberTimeout(memberIndex, timeout, GameState.PMTA_RESET_DISABLED_AND_HIT_INFO_LEFT/RIGHT_HAND, true);
		stx ARGS+0
		pla
		tax
		lda #0
		sta ARGS+1
		sta ARGS+2
		sta ARGS+3
		lda #1
		sta ARGS+5
		jsrf gameState_setMemberTimeout
		rts

PMTA_RESET_DISABLED_AND_HIT_INFO_LEFT_HAND_handler:
		ldx #2
		ldy #0
		jsr resetDisabledHandsAndHitInfo
		jmp continue

PMTA_RESET_DISABLED_AND_HIT_INFO_RIGHT_HAND_handler:
		ldx #2
		ldy #1
		jsr resetDisabledHandsAndHitInfo
		jmp continue

PMTA_RESET_DAMAGE_INFO_handler:
		ldy #PartyMember::damageInfo
		lda #0
		sta (CURRENT_PARTYMEMBER),y
		iny
		sta (CURRENT_PARTYMEMBER),y
		jmp continue

PMTA_RESET_GIANT_STRENGTH_handler:
		ldy #PartyMember::strength
		lda (CURRENT_PARTYMEMBER),y
		ldy #PartyMember::strengthCurrent
		sta (CURRENT_PARTYMEMBER),y
		ldy #PartyMember::extraStrength
		lda (CURRENT_PARTYMEMBER),y
		ldy #PartyMember::extraStrengthCurrent
		sta (CURRENT_PARTYMEMBER),y
		ldx partyMemberIndex
		inc partyMemberStatsChanged,x

		lda #1
		sta textColor
		jsrf printMemberName
		ldx #<noLongerHasGiantStrength
		ldy #>noLongerHasGiantStrength
		jsr text_writeNullString

		jmp continue
noLongerHasGiantStrength: .byte " hat keine Riesenkr",$7d,"fte mehr.",$a,0

PMTA_POISONED_CHARACTER_handler:
		ldy #PartyMember::status
		lda (CURRENT_PARTYMEMBER),y
		and #PARTYMEMBER_STATUS_POISONED
		bne :+
			jmp continue
		:

		lda partyMemberIndex
		sta ARGS+0
		pha

		ldy #PartyMember::activeSpells+0
		lda (CURRENT_PARTYMEMBER),y
		and #SPELL_SLOW_POISON
		bne :+
			lda #5
			sta ARGS+1
			lda #0
			sta ARGS+2
			jsrf gameState_giveMemberDamage
		:

		;GameState.setMemberTimeout(memberIndex, 546, GameState.PMTA_POISONED_CHARACTER, true);
		lda #<546
		sta ARGS+0
		lda #>546
		sta ARGS+1
		lda #0
		sta ARGS+2
		sta ARGS+3
		lda #PMTA_POISONED_CHARACTER
		sta ARGS+4
		lda #1
		sta ARGS+5
		pla
		tax
		jsrf gameState_setMemberTimeout

		lda #1
		sta textColor
		jsrf printMemberName
		ldx #<feelsTheEffectOfPoison
		ldy #>feelsTheEffectOfPoison
		jsr text_writeNullString

		jmp continue
feelsTheEffectOfPoison: .byte " f",$7f,"hlt die Wirkung des Gifts!",$a,0

PMTA_RESET_PARALYZED_handler:
		ldy #PartyMember::status
		lda (CURRENT_PARTYMEMBER),y
		and #(PARTYMEMBER_STATUS_PARALYZED^$ff)
		sta (CURRENT_PARTYMEMBER),y
		ldx partyMemberIndex
		inc partyMemberStatsChanged,x

		lda #1
		sta textColor
		jsrf printMemberName
		ldx #<isNoLongerParalyzed
		ldy #>isNoLongerParalyzed
		jsr text_writeNullString

		jmp continue
isNoLongerParalyzed: .byte " ist nicht mehr gel",$7d,"hmt!",$a,0

pmtaHandler_lo:	.byte <PMTA_RESET_DISABLED_LEFT_HAND_handler
		.byte <PMTA_RESET_DISABLED_RIGHT_HAND_handler
		.byte <PMTA_RESET_HIT_INFO_LEFT_HAND_handler
		.byte <PMTA_RESET_HIT_INFO_RIGHT_HAND_handler
		.byte <PMTA_RESET_DISABLED_AND_HIT_INFO_LEFT_HAND_handler
		.byte <PMTA_RESET_DISABLED_AND_HIT_INFO_RIGHT_HAND_handler
		.byte <PMTA_RESET_DAMAGE_INFO_handler
		.byte <PMTA_RESET_GIANT_STRENGTH_handler
		.byte <PMTA_POISONED_CHARACTER_handler
		.byte <PMTA_RESET_PARALYZED_handler

pmtaHandler_hi:	.byte >PMTA_RESET_DISABLED_LEFT_HAND_handler
		.byte >PMTA_RESET_DISABLED_RIGHT_HAND_handler
		.byte >PMTA_RESET_HIT_INFO_LEFT_HAND_handler
		.byte >PMTA_RESET_HIT_INFO_RIGHT_HAND_handler
		.byte >PMTA_RESET_DISABLED_AND_HIT_INFO_LEFT_HAND_handler
		.byte >PMTA_RESET_DISABLED_AND_HIT_INFO_RIGHT_HAND_handler
		.byte >PMTA_RESET_DAMAGE_INFO_handler
		.byte >PMTA_RESET_GIANT_STRENGTH_handler
		.byte >PMTA_POISONED_CHARACTER_handler
		.byte >PMTA_RESET_PARALYZED_handler
.endproc
