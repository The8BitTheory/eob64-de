.include "global.inc"

.segment "BSS"
needRest: .res 1
didLoad: .res 1
inMainMenu: .res 1

.export partyResting
partyResting: .res 1

.segment "CAMP"

.macro button xpos, ypos, width, keyhintpos, text
	.byte width
	.byte xpos
	.byte ypos
	.byte keyhintpos
	.word text
.endmacro

.macro clickregion x0,y0,w,h,callback,key,special,modifier,parameter
	.word callback
	.byte x0,y0,x0+w,y0+h
	.byte key,special,modifier
	.byte parameter
.endmacro

.macro defineMenu
	lda #<menu
	sta TMP+0
	lda #>menu
	sta TMP+1
	jsr drawMenu
	lda #<cr
	sta CURRENT_CAMP_MENU+0
	lda #>cr
	sta CURRENT_CAMP_MENU+1
.endmacro

.macro redrawMenu
	lda #<menu
	sta TMP+0
	lda #>menu
	sta TMP+1
	jsr drawMenu
.endmacro

.macro confirm message,message2,message3,message4,message5
	.scope
		lda #<__cont__
		sta ARGS+0
		lda #>__cont__
		sta ARGS+1
		ldx #<__msg__
		ldy #>__msg__
		jsr showModalConfirmBox
		rts
		__msg__:
			.byte message
			.ifnblank message2
				.byte message2
			.endif
			.ifnblank message3
				.byte message3
			.endif
			.ifnblank message4
				.byte message4
			.endif
			.ifnblank message5
				.byte message5
			.endif
			.byte 0
		__cont__:
	.endscope
.endmacro

.macro notify message,message2,message3,message4,message5
	.scope
		ldx #<__msg__
		ldy #>__msg__
		jsr showModalBox
		jmp __cont__
		__msg__:
			.byte message
			.ifnblank message2
				.byte message2
			.endif
			.ifnblank message3
				.byte message3
			.endif
			.ifnblank message4
				.byte message4
			.endif
			.ifnblank message5
				.byte message5
			.endif
			.byte 0
		__cont__:
	.endscope
.endmacro

exitTxt:	  .byte " Exit",0

.proc f_text_setPosition
	jsrf text_setPosition
	rts
.endproc


; ---------------------------------------------------------------------------------
; Top menu
; ---------------------------------------------------------------------------------
.proc showTopMenu
		defineMenu
		rts

menu:		button  0, 1, 18, 5, restPartyTxt
		button 20, 1, 18, 2, prayForSpellsTxt
		button  0, 4, 18, 2, memorizeSpellsTxt
		button 20, 4, 18, 3, scribeScrollsTxt
		button  0, 7, 18, 3, dropCharacterTxt
		button 20, 7, 10, 4, gameOptionsTxt
		button 32, 7,  6, 2, exitTxt
		.byte 0
		.byte 18,"Camp",0

cr:		clickregion  0,1,20,3, topMenuHandler, $12,$ff,$00, 0 ;rest
		clickregion 20,1,20,3, topMenuHandler, $10,$ff,$00, 1 ;pray
		clickregion  0,4,20,3, topMenuHandler, $0d,$ff,$00, 2 ;memo
		clickregion 20,4,20,3, topMenuHandler, $13,$ff,$00, 3 ;scri
		clickregion  0,7,20,3, topMenuHandler, $04,$ff,$00, 4 ;drop
		clickregion 20,7,12,3, topMenuHandler, $07,$ff,$00, 5 ;game
		clickregion 32,7, 8,3, topMenuHandler, $05,$ff,$00, 6 ;exit
		clickregion 32,7, 8,3, topMenuHandler, $1f,$ff,$00, 6 ;exit
		clickregion 32,7, 8,3, topMenuHandler, $00,$00,$80, 6 ;exit
		.word 0

restPartyTxt: 	  .byte "    Rest Party",0
memorizeSpellsTxt:.byte " Memorize Spells",0
prayForSpellsTxt: .byte " Pray for Spells",0
scribeScrollsTxt: .byte "  Scribe Scrolls",0
dropCharacterTxt: .byte "  Drop Character",0
gameOptionsTxt:	  .byte "   Game",0
.endproc

.proc topMenuHandler
		bne :+
			jsr showRest
			rts
		:
		cmp #1
		bne :++
			ldx #<showPrayMemorizeMenu
			ldy #>showPrayMemorizeMenu
			lda #IS_CLERIC
			jsr showSelectCharMenu
			bcs :+
				notify "You don't have any Cleric able to",$a,"pray in your party."
				jmp showTopMenu
			:
			rts
		:
		cmp #2
		bne :++
			ldx #<showPrayMemorizeMenu
			ldy #>showPrayMemorizeMenu
			lda #IS_MAGE
			jsr showSelectCharMenu
			bcs :+
				notify "You don't have any Mage able to",$a,"learn spells."
				jmp showTopMenu
			:
			rts
		:
		cmp #3
		bne :+
			jmp showScribeScrolls
		:
		cmp #4
		bne :+++
			jsrf npc_dropMemberMenu
			bcs :++
				cpx #0
				bne :+
					ldx #<noLess
					ldy #>noLess
					jsr showModalBox
				:
				jmp showTopMenu
			:
			jmp exit
		:
		cmp #5
		bne :+
			jmp showGameOptions
		:
		cmp #6
		bne :++
			lda needRest
			beq :+
				notify "Your party needs to rest to gain",$a,"spells."
			:

exit:		lda #0
			sta CURRENT_CAMP_MENU+0
			sta CURRENT_CAMP_MENU+1
			rts
		:
		rts
noLess: .byte "You cannot have less than four",$a,"characters.",0
.endproc


; Dialogues:
; 23 = select character (pray)
; 26 = select character (memorize)
; 49 = select character (scribe)
; 53 = select character (drop)

; Strings:
; 23     "\r\r Select a character\r from your party who\r would like to learn\r spells.",
; 24     "\r Your Paladin is \r too low a level\r for spells.",
; 25     "\r\r The Mage has no\r Spell Book!",
; 26     "\r\r\r Select a character\r from your party\r who would like to\r pray for spells.",
; 27     "\r You don't have\r any Cleric able\r to pray in your\r party.",
; 28     "\r You don't have\r any Mage able to\r learn spells.",
; 29     " An unconscious\r or dead Mage\r cannot memorize\r spells.",
; 30     " An unconscious\r or dead Cleric\r cannot pray for\r spells.",
; 31     "1",
; 32     "2",
; 33     "3",
; 34     "4",
; 35     "5",
; 36     "Clear",
; 37     "Spells Available:",
; 38     "Yes",
; 39     "No"

; 40    "\rWill your healers\rheal the party?",
; 41    " Someone is still\r injured. Rest\r until healed?",
; 42    "Resting party.",
; 43    "\r All characters\r are fully\r rested.",
; 44    " Your party needs\r to rest to gain\r spells."

; 45    " You cannot have\r less than four\r characters."
; 46    " Are you sure you\r wish to exit the\r game?"

; 47    " Your party is\r starving. Do you\r wish to continue\r resting?"

; 48     "Select the scroll(s)\ryou wish to scribe.",
; 49     "\r\r\r Select a Mage\r from your party\r who would like to\r scribe spells.",
; 50     " You don't have\r any scolls to be\r scribed.",
; 51     " You don't have\r any scrolls that\r this Mage needs.",
; 52     "\r You don't have\r any Mage able to\r scribe scrolls."

; 53     " Select the\r character you\r wish to drop."

; 55     " Are you sure you\r wish to SAVE the\r game?"

; 56     "Camp:",
; 57     "Preferences:",
; 58     "Game Options:"

; 59     "Poisoned party\rmembers will die!\rRest anyway?"

; "\rYou can't rest here, monsters are near."


; ---------------------------------------------------------------------------------
; Select character menu 
; ---------------------------------------------------------------------------------
.proc showSelectCharMenu
		.pushseg
		.segment "BSS"
			listedCharacters: .res 6
			nbrCharacters: .res 1
			menuToShow: .res 2
			professionFilter: .res 1
		.popseg

		sta professionFilter
		stx menuToShow+0
		sty menuToShow+1

		ldx #5
		lda #<-1
		:
			sta listedCharacters
			dex
		bpl :-
		lda #0
		sta nbrCharacters

		ldx #0
		:
			lda partyMembers_lo,x
			sta CURRENT_PARTYMEMBER+0
			lda partyMembers_hi,x
			sta CURRENT_PARTYMEMBER+1
			stx CURRENT_PARTYMEMBER_INDEX

			ldy #IS_ACTIVE
			jsr getMemberStatus
			bcc :+

			jsrf gameState_getClassProfession
			lda ARGS+0
			and professionFilter
			beq :+

				ldy nbrCharacters
				lda CURRENT_PARTYMEMBER_INDEX
				sta listedCharacters,y
				iny
				sty nbrCharacters

			:
			ldx CURRENT_PARTYMEMBER_INDEX
			inx
			cpx #6
		bne :--

		lda nbrCharacters
		bne :+
			clc
			rts
		:
		cmp #1
		bne :+
			ldy #0
			jsr clickChar
			sec
			rts
		:

		defineMenu

		ldy #0
		:
			tya
			pha

			ldx listedCharacters,y
			lda partyMembers_lo,x
			sta CURRENT_PARTYMEMBER+0
			lda partyMembers_hi,x
			sta CURRENT_PARTYMEMBER+1
			stx CURRENT_PARTYMEMBER_INDEX

			iny
			tya
			pha
			iny
			ldx #15
			jsr f_text_setPosition

			ldy textCol2
			clc
			pla
			adc #'0'
			sta (TEXT2_SCREEN),y
			iny
			lda #'.'
			sta (TEXT2_SCREEN),y

			ldy textCol2
			lda #7
			sta (TEXT2_D800),y
			iny
			sta (TEXT2_D800),y

			iny
			iny
			sty textCol2

			lda #1
			sta textColor2

			clc
			lda #PartyMember::name
			adc CURRENT_PARTYMEMBER+0
			tax
			lda #0
			adc CURRENT_PARTYMEMBER+1
			tay
			clc
			jsr text_writeNullString2 

			pla
			tay
			iny
			cpy nbrCharacters
		bne :-
		sec
		rts

menu:		button 32, 7,  6, 2, exitTxt
		.byte 0
		.byte 11,"Select a character",0

cr:		clickregion 32,7, 8, 3,  back, 		$05,$ff,$00, 0
		clickregion 32,7, 8, 3,  back, 		$1f,$ff,$00, 0
		clickregion 32,7, 8, 3,  back, 		$00,$00,$80, 0
		clickregion 15,2,15, 6,  clickChar, $ff,$ff,$00, 0
		clickregion $ff,$ff,0,0, keyChar, 	$31,$ff,$00, 0
		clickregion $ff,$ff,0,0, keyChar, 	$32,$ff,$00, 1
		clickregion $ff,$ff,0,0, keyChar, 	$33,$ff,$00, 2
		clickregion $ff,$ff,0,0, keyChar, 	$34,$ff,$00, 3
		clickregion $ff,$ff,0,0, keyChar, 	$35,$ff,$00, 4
		clickregion $ff,$ff,0,0, keyChar, 	$36,$ff,$00, 5
		.word 0

back:		jmp showTopMenu

clickChar:	cpy nbrCharacters
		bcs :+
			ldx listedCharacters,y
			lda partyMembers_lo,x
			sta CURRENT_PARTYMEMBER+0
			lda partyMembers_hi,x
			sta CURRENT_PARTYMEMBER+1
			stx CURRENT_PARTYMEMBER_INDEX
			lda menuToShow+0
			sta INDJMP+0
			lda menuToShow+1
			sta INDJMP+1
			jmp indirectJsr_nobank
		:
		rts

keyChar:	tay
		jmp clickChar
.endproc



; ---------------------------------------------------------------------------------
; Pray/Memorize menu
; ---------------------------------------------------------------------------------
.proc showPrayMemorizeMenu
		.pushseg
		.segment "BSS"
			spellsPerPage: .res 5
			knownSpells: .res 4
			selectedPage: .res 1
			spellsOnSelectedPage: .res 8
			spellCntOnSelectedPage: .res 8
			nbrSpellsOnSelectedPage: .res 1
			spellsLeft: .res 1
		.popseg

		jsr isAlive
		bcc :+
			jmp :+++++
		:
			jsr getSelectedClass
			cmp #CLASS_MAGE
			bne :+
				notify "An unconscious or dead Mage",$a,"cannot memorize spells."
			:
			cmp #CLASS_CLERIC
			bne :+
				notify "An unconscious or dead Cleric",$a,"cannot memorize spells."
			:
			cmp #CLASS_PALADIN
			bne :+
				notify "An unconscious or dead Paladin",$a,"cannot memorize spells."
			:
			jmp showTopMenu
		:

		jsr getSelectedClass
		cmp #CLASS_MAGE
		bne :++
			jsr hasHolySymbol
			bne :+
				notify "The Mage has no Spell Book!"
				jmp showTopMenu
			:
			jmp :++
		:
		cmp #CLASS_PALADIN
		bne :+
			ldy #PartyMember::level
			lda (CURRENT_PARTYMEMBER),y
			cmp #9
			bcs :+
				notify "Your Paladin is too low a level for",$a,"spells."
				jmp showTopMenu
		:

		jsr getSpellsPerPage

		defineMenu
		jsr drawSelectedCharName

		lda #0
		sta selectedPage
		jsr selectPage
		rts

isAlive:
		ldx CURRENT_PARTYMEMBER_INDEX
		ldy #IS_ACTIVE|IS_CONSCIOUS|NOT_PARALYZED
		jsr getMemberStatus
		rts

getSelectedClass:
		lda showSelectCharMenu::professionFilter
		cmp #IS_MAGE
		bne :+
			lda #CLASS_MAGE
			rts
		:
		ldy #PartyMember::class
		lda (CURRENT_PARTYMEMBER),y
		sta ARGS+0
		ldy #CLASS_CLERIC
		jsrf gameState_getMaxExpLevelIndex
		lda ARGS+0
		bpl :+
			lda #CLASS_PALADIN
			rts
		:
		lda #CLASS_CLERIC
		rts

hasHolySymbol:
		lda #ITEMTYPE_SPELLBOOK
		sta ARGS+0
		lda #<-1
		sta ARGS+1
		jsrf items_checkInventoryForItemByTypeAndValue
		lda ARGS+0
		rts

menu:	button 25,1,1,1,page1Txt
		button 28,1,1,1,page2Txt
		button 31,1,1,1,page3Txt
		button 34,1,1,1,page4Txt
		button 37,1,1,1,page5Txt
		button 25,7,5,1,clearTxt
		button 32,7,6,2,exitTxt
		.byte 0
		.byte 9,"Spells Available",0,0

page1Txt:	.byte "1",0
page2Txt:	.byte "2",0
page3Txt:	.byte "3",0
page4Txt:	.byte "4",0
page5Txt:	.byte "5",0
clearTxt:	.byte "Clear",0

		;X,Y, Width,Height, Callback, Keychar,KeyMod, Param
cr:		clickregion 32,7,  8,3, back,        $05,$ff,$00, 0
		clickregion 32,7,  8,3, back, 		 $1f,$ff,$00, 0
		clickregion 32,7,  8,3, back, 		 $00,$00,$80, 0
		clickregion 25,1,  3,3, selectPage,  $31,$ff,$41, 0
		clickregion 28,1,  3,3, selectPage,  $32,$ff,$41, 1
		clickregion 31,1,  3,3, selectPage,  $33,$ff,$41, 2
		clickregion 34,1,  3,3, selectPage,  $34,$ff,$41, 3
		clickregion 37,1,  3,3, selectPage,  $35,$ff,$41, 4
		clickregion  0,1, 24,1, selectSpell, $31,$ff,$00, 0
		clickregion  0,2, 24,1, selectSpell, $32,$ff,$00, 1
		clickregion  0,3, 24,1, selectSpell, $33,$ff,$00, 2
		clickregion  0,4, 24,1, selectSpell, $34,$ff,$00, 3
		clickregion  0,5, 24,1, selectSpell, $35,$ff,$00, 4
		clickregion  0,6, 24,1, selectSpell, $36,$ff,$00, 5
		clickregion  0,7, 24,1, selectSpell, $37,$ff,$00, 6
		clickregion  0,8, 24,1, selectSpell, $38,$ff,$00, 7
		clickregion 25,7,  7,3, clearSpells, $03,$ff,$00, 0
		.word 0

.proc back
		lda showSelectCharMenu::professionFilter
		cmp #IS_CLERIC
		beq :+
			lda #PartyMember::mageSpells
			jmp :++
		:
			lda #PartyMember::clericSpells
		:
		clc
		adc CURRENT_PARTYMEMBER+0
		sta TMP+0
		lda CURRENT_PARTYMEMBER+1
		adc #0
		sta TMP+1 

		ldy #29
		:
			lda (TMP),y
			bpl :+
				lda #1
				sta needRest
				jmp showTopMenu
			:
			dey
		bpl :--

		jmp showTopMenu
.endproc

.proc getMemorizedSpellsBase
		lda showSelectCharMenu::professionFilter
		cmp #IS_CLERIC
		clc
		beq :+
			lda #PartyMember::mageSpells
			jmp :++
		:
			lda #PartyMember::clericSpells
		:
		ldy selectedPage
		adc mul6,y

		clc
		adc CURRENT_PARTYMEMBER+0
		sta TMP+0
		lda CURRENT_PARTYMEMBER+1
		adc #0
		sta TMP+1 
		rts
.endproc

.proc clearSpells
		jsr getMemorizedSpellsBase
		lda #0
		tay
		:
			sta (TMP),y
			iny
			cpy #6
		bne :-
		lda selectedPage
		jmp selectPage 
.endproc

.proc selectSpell
		cmp nbrSpellsOnSelectedPage
		bcc :+
			rts
		:
		tax
		lda spellsLeft
		bne :+
			rts
		:

		jsr getMemorizedSpellsBase

		lda spellsOnSelectedPage,x
		eor #$ff
		tax
		inx
		ldy #0
		:
			lda (TMP),y
			bne :+
				txa
				sta (TMP),y
				lda selectedPage
				jmp selectPage
			:
			iny
			cpy #6
		bne :--

		rts
.endproc

.proc selectPage
		; Unselect previous choice
		cmp selectedPage
		beq samePage
			pha
			ldy selectedPage
			clc
			lda mul3,y
			adc #25
			tax
			ldy #1
			lda #1
			sta ARGS+0
			jsrf text_drawButton

			; Clear spells area
			ldx #1
			ldy #1
			jsr f_text_setPosition
			ldx #8
			:
				lda #' '
				ldy #22
				:
					sta (TEXT2_SCREEN),y
					dey
				bne :-
				clc
				lda TEXT2_SCREEN
				adc #40
				sta TEXT2_SCREEN
				bcc :+
					inc TEXT2_SCREEN+1
				:
				dex
			bne :---

			pla
		samePage:

		; Select current choice
		sta selectedPage
		tay
		clc
		lda mul3,y
		adc #25
		tax
		ldy #1
		lda #1
		sta ARGS+0
		jsrf text_drawButtonPressed

		; Filter out spells and calculate spell count
		jsr getSpellsOnSelectedPage

		; Present remaining and total
		ldx #27
		ldy #5
		jsr f_text_setPosition
		lda #3
		sta textColor2
		ldx #<remaining
		ldy #>remaining
		clc
		jsr text_writeNullString2

		ldx #27
		ldy #5
		jsr f_text_setPosition

		clc
		lda spellsLeft
		adc #'0'
		jsr putCharWrapper

		lda #27+5
		sta textCol2
		ldx selectedPage
		lda spellsPerPage,x
		clc
		adc #'0'
		jsr putCharWrapper

		lda showSelectCharMenu::professionFilter
		cmp #IS_MAGE
		beq :+
			ldx selectedPage
			lda spellsPerPage,x
			bne :+
				rts
			:

		; Spell text base
		lda showSelectCharMenu::professionFilter
		cmp #IS_CLERIC
		beq :+
			lda #0
			jmp :++
		:
			lda #25
		:
		sta TMP	

		; Print spells and counts
		ldx #0
		:
			cpx nbrSpellsOnSelectedPage
			bne :+
				rts
			:
			txa
			pha

			tay
			iny
			ldx #1
			jsr f_text_setPosition

			pla
			tax

			lda #7
			sta textColor2
			txa
			clc
			adc #'1'
			jsr putCharWrapper
			lda #'.'
			jsr putCharWrapper
			lda #1
			sta textColor2
			inc textCol2

			txa
			pha
			clc
			lda spellsOnSelectedPage,x
			adc TMP
			tax
			jsrf printSpellName2
			pla
			tax

			lda #22
			sta textCol2
			clc
			lda spellCntOnSelectedPage,x
			adc #'0'
			jsr putCharWrapper

			inx
		jmp :--

remaining:	.byte "  of   Left",0
mul3:		.byte 0,3,6,9,12
.endproc

.proc getSpellsOnSelectedPage
		; Filter out the correct spells for the selected page
		lda showSelectCharMenu::professionFilter
		cmp #IS_CLERIC
		beq :+
			lda #<mageSpellPage
			sta TMP+0
			lda #>mageSpellPage
			sta TMP+1
			jmp :++
		:
			lda #<clericSpellPage
			sta TMP+0
			lda #>clericSpellPage
			sta TMP+1
		:

		lax #0
		tay
		sta nbrSpellsOnSelectedPage
		:
			lda bits,y
			php
			and knownSpells,x
			beq :+
				lda (TMP),y
				cmp selectedPage
				bne :+
					tya
					clc
					adc #1

					stx XREG
					ldx nbrSpellsOnSelectedPage
					sta spellsOnSelectedPage,x
					inx
					stx nbrSpellsOnSelectedPage
					ldx XREG
			:
			plp
			bpl :+
				inx
			:
			iny
			cpy #$19
		bne :---

		; Count spells memorized/prayed for
		lda showSelectCharMenu::professionFilter
		cmp #IS_CLERIC
		clc
		beq :+
			lda #PartyMember::mageSpells
			jmp :++
		:
			lda #PartyMember::clericSpells
		:
		ldx selectedPage
		adc mul6,x
		sta YREG

		ldx #0
		loop:
			cpx nbrSpellsOnSelectedPage
			beq breakLoop
			stx XREG
			lda spellsOnSelectedPage,x
			sta TMP

			lda #0
			sta COUNT
			ldx #6
			ldy YREG
			innerLoop:
				lda (CURRENT_PARTYMEMBER),y
				beq :++
					bpl :+
						eor #$ff
						clc
						adc #1
					:
					cmp TMP
					bne :+
						inc COUNT
				:
				iny
				dex
			bne innerLoop
		
			ldx XREG
			lda COUNT
			sta spellCntOnSelectedPage,x
			inx
		bne loop
		breakLoop:

		; Calculate spell points left to place
		lda #0
		tax 
		:
			cpx nbrSpellsOnSelectedPage
			beq :+
			clc
			adc spellCntOnSelectedPage,x
			inx
		bne :-
		:
		sta TMP
		sec
		ldx selectedPage
		lda spellsPerPage,x
		sbc TMP
		bpl :+
			lda #0
		:
		sta spellsLeft

		rts
.endproc

.proc getSpellsPerPage
		lda #0
		ldx #4
		:
			sta spellsPerPage,x
			dex
		bpl :-

		lda showSelectCharMenu::professionFilter
		cmp #IS_CLERIC
		beq pray
		jmp memorize

pray:		ldy #PartyMember::class
		lda (CURRENT_PARTYMEMBER),y
		sta ARGS+0
		ldy #CLASS_CLERIC
		jsrf gameState_getMaxExpLevelIndex
		lda ARGS+0
		bpl isCleric

		ldy #PartyMember::class
		lda (CURRENT_PARTYMEMBER),y
		sta ARGS+0
		ldy #CLASS_PALADIN
		jsrf gameState_getMaxExpLevelIndex
		lda ARGS+0
		bpl isPaladin
		rts

isCleric:
		jsr getSpellsPerPagePerLevelOffset
		ldx #0
		:
			lda nbrClericSpellsPerPagePerLevel,y
			sta spellsPerPage,x
			iny
			inx
			cpx #5
		bne :-

		; max(0, wisdomCurrent-12)*5
		ldy #PartyMember::wisdomCurrent
		lda (CURRENT_PARTYMEMBER),y
		sec
		sbc #12
		bpl :+
			lda #0
		:
		tax
		ldy mul5,x
		ldx #0
		:
			clc
			lda spellsPerPage,x
			beq :+
				adc nbrExtraClericSpellsPerPageBasedOnWisdom,y
				sta spellsPerPage,x
			:
			iny
			inx
			cpx #5
		bne :--

		lda #<(CLERIC_KNOWN_SPELLS>>0)
		sta knownSpells+0
		lda #<(CLERIC_KNOWN_SPELLS>>8)
		sta knownSpells+1
		lda #<(CLERIC_KNOWN_SPELLS>>16)
		sta knownSpells+2
		lda #<(CLERIC_KNOWN_SPELLS>>24)
		sta knownSpells+3
		rts

isPaladin:
		lda #0
		jsr getSpellsPerPagePerLevelOffset
		ldx #0
		:
			lda nbrPaladinSpellsPerPagePerLevel,y
			sta spellsPerPage,x
			iny
			inx
			cpx #5
		bne :-
		lda #<(PALADIN_KNOWN_SPELLS>>0)
		sta knownSpells+0
		lda #<(PALADIN_KNOWN_SPELLS>>8)
		sta knownSpells+1
		lda #<(PALADIN_KNOWN_SPELLS>>16)
		sta knownSpells+2
		lda #<(PALADIN_KNOWN_SPELLS>>24)
		sta knownSpells+3
		rts

memorize:	ldy #PartyMember::class
		lda (CURRENT_PARTYMEMBER),y
		sta ARGS+0
		ldy #CLASS_MAGE
		jsrf gameState_getMaxExpLevelIndex
		lda ARGS+0
		bpl isMage
		rts

isMage:		jsr getSpellsPerPagePerLevelOffset
		ldx #0
		:
			lda nbrMageSpellsPerPagePerLevel,y
			sta spellsPerPage,x
			iny
			inx
			cpx #5
		bne :-

		lda CURRENT_PARTYMEMBER_INDEX
		sta ARGS+0
		lda #RING_OF_WIZARDRY
		sta ARGS+1
		jsrf gameState_memberHaveRing
		lda ARGS+1
		lsr
		bcc :+
			asl spellsPerPage+3
			asl spellsPerPage+4
		:

		ldx #0
		ldy #PartyMember::scribedScrolls
		:
			lda (CURRENT_PARTYMEMBER),y
			sta knownSpells,x
			iny
			inx
			cpx #4
		bne :-
		rts

getSpellsPerPagePerLevelOffset:
		clc
		adc #PartyMember::level
		tay
		lax (CURRENT_PARTYMEMBER),y
		dex
		bpl :+
			ldx #0
		:
		ldy mul5,x
		rts
.endproc

mul5:	.byte 0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80
mul6:	.byte 0,6,12,18,24

mageSpellPage:
	.byte 0, 0, 0, 0, 0, 0, 0
	.byte 1, 1, 1, 1
	.byte 2, 2, 2, 2, 2, 2, 2, 2
	.byte 3, 3, 3
	.byte 4, 4, 4

clericSpellPage:
	.byte 0, 0, 0, 0, 0
	.byte 1, 1, 1, 1
	.byte 2, 2, 2, 2, 2
	.byte 3, 3, 3, 3, 3
	.byte 4, 4, 4, 4
	.byte 0
	.byte <-1

nbrClericSpellsPerPagePerLevel:
	;Page 1  2  3  4  5 Lvl
	.byte 1, 0, 0, 0, 0 ;1
	.byte 2, 0, 0, 0, 0 ;2
	.byte 2, 1, 0, 0, 0 ;3
	.byte 3, 2, 0, 0, 0 ;4
	.byte 3, 3, 1, 0, 0 ;5
	.byte 3, 3, 2, 0, 0 ;6
	.byte 3, 3, 2, 1, 0 ;7
	.byte 3, 3, 3, 2, 0 ;8
	.byte 4, 4, 3, 2, 1 ;9
	.byte 4, 4, 3, 3, 2 ;10
	.byte 4, 4, 3, 3, 2 ;10 buffer overflow handling
	.byte 4, 4, 3, 3, 2 ;10 buffer overflow handling
	.byte 4, 4, 3, 3, 2 ;10 buffer overflow handling

nbrExtraClericSpellsPerPageBasedOnWisdom:
	;Page 1  2  3  4  5 Wis
	.byte 0, 0, 0, 0, 0 ;12
	.byte 1, 0, 0, 0, 0 ;13
	.byte 2, 0, 0, 0, 0 ;14
	.byte 2, 1, 0, 0, 0 ;15
	.byte 2, 2, 0, 0, 0 ;16
	.byte 2, 2, 1, 0, 0 ;17
	.byte 2, 2, 1, 1, 0 ;18
	.byte 3, 2, 1, 2, 0 ;19
	.byte 3, 2, 1, 2, 0 ;19 buffer overflow handling
	.byte 3, 2, 1, 2, 0 ;19 buffer overflow handling
	.byte 3, 2, 1, 2, 0 ;19 buffer overflow handling

nbrPaladinSpellsPerPagePerLevel:
	;Page 1  2  3  4  5 Lvl
	.byte 1, 0, 0, 0, 0 ;1
	.byte 1, 0, 0, 0, 0 ;2
	.byte 1, 0, 0, 0, 0 ;3
	.byte 1, 0, 0, 0, 0 ;4
	.byte 1, 0, 0, 0, 0 ;5
	.byte 1, 0, 0, 0, 0 ;6
	.byte 1, 0, 0, 0, 0 ;7
	.byte 1, 0, 0, 0, 0 ;8
	.byte 1, 0, 0, 0, 0 ;9
	.byte 2, 0, 0, 0, 0 ;10
	.byte 2, 1, 0, 0, 0 ;11
	.byte 2, 2, 0, 0, 0 ;12
	.byte 2, 2, 0, 0, 0 ;12 buffer overflow handling
	.byte 2, 2, 0, 0, 0 ;12 buffer overflow handling
	.byte 2, 2, 0, 0, 0 ;12 buffer overflow handling

nbrMageSpellsPerPagePerLevel:
	;Page 1  2  3  4  5 Lvl
	.byte 1, 0, 0, 0, 0 ;1
	.byte 2, 0, 0, 0, 0 ;2
	.byte 2, 1, 0, 0, 0 ;3
	.byte 3, 2, 0, 0, 0 ;4
	.byte 4, 2, 1, 0, 0 ;5
	.byte 4, 2, 2, 0, 0 ;6
	.byte 4, 3, 2, 1, 0 ;7
	.byte 4, 3, 3, 2, 0 ;8
	.byte 4, 3, 3, 2, 1 ;9
	.byte 4, 4, 3, 2, 2 ;10
	.byte 4, 4, 4, 3, 3 ;11
	.byte 4, 4, 4, 3, 3 ;11 buffer overflow handling
	.byte 4, 4, 4, 3, 3 ;11 buffer overflow handling
	.byte 4, 4, 4, 3, 3 ;11 buffer overflow handling
.endproc



; ---------------------------------------------------------------------------------
; Game options menu 
; ---------------------------------------------------------------------------------
.pushseg 
.segment "BSS"
.export preferredDevice
preferredDevice: .res 1
.popseg

.proc showGameOptions
		defineMenu
		jsr highlightPreferredDevice
		rts

highlightPreferredDevice:
		lda #1
		ldx #10
		:
			sta $d800+23*40+7,x
			dex
		bpl :-
		lda preferredDevice
		and #3
		tax
		clc
		adc #8
		sta preferredDevice
		ldy deviceOffsets,x
		lda #6
		sta $d800+23*40,y
		iny
		sta $d800+23*40,y

		rts

load:	jsrf io_readDirectory
		cpx #0
		beq :+
			ldx #<driveErrorString
			ldy #>driveErrorString
			jsr showModalBox
			jmp showGameOptions
		:

		lda nbrFilesFound
		bne :+
			ldx #<noSavesFound
			ldy #>noSavesFound
			jsr showModalBox
			jmp showGameOptions
		:
		ldx #<showGameOptions
		ldy #>showGameOptions
		jmp showFilePicker

noSavesFound: .byte "NO SAVEGAMES FOUND.",0

save:	jsrf io_save
		cpx #0
		beq :+
			ldx #<driveErrorString
			ldy #>driveErrorString
			jsr showModalBox
		:
		jmp showGameOptions

flashLoad:
		jsr isFlashSaveAvailable
		cpx #$ff
		bne :+
			ldx #<noSavesFound
			ldy #>noSavesFound
			jsr showModalBox
			jmp showGameOptions			
		:
		jsrf io_flashLoad
		lda #0
		sta screenEnabled
		inc didLoad
		ldx levelIndex
		jsrf loadMazeFromSave
		lda #0
		sta CURRENT_CAMP_MENU+0
		sta CURRENT_CAMP_MENU+1
		rts

flashSave:
		jsrf io_flashSave
		jmp showGameOptions

device:	inc preferredDevice
		jsr highlightPreferredDevice
		rts

menu:	button  0, 1, 18, 2, flashLoadTxt
		button 20, 1, 18, 2, flashSaveTxt
		button  0, 4, 18, 2, diskLoadTxt
		button 20, 4, 18, 2, diskSaveTxt
		button  0, 7, 18, 2, deviceTxt
		button 32, 7,  6, 2, exitTxt
		.byte 0
		.byte 14,"Game Options",0

cr:		clickregion  0,1,20,3, flashLoad,	$31,$ff,$00, 0
		clickregion 20,1,20,3, flashSave,  	$32,$ff,$00, 0
		clickregion  0,4,20,3, load,  		$33,$ff,$00, 0
		clickregion 20,4,20,3, save,  		$34,$ff,$00, 0
		clickregion  0,7,20,3, device,		$04,$ff,$00, 0
		clickregion 32,7, 8,3, back,  		$05,$ff,$00, 0
		clickregion 32,7, 8,3, back, 		$1f,$ff,$00, 0
		clickregion 32,7, 8,3, back, 		$00,$00,$80, 0
		.word 0

flashLoadTxt: 	 .byte " 1 Flash Load",0
flashSaveTxt:    .byte " 2 Flash Save",0
diskLoadTxt:     .byte " 3 Disk Load",0
diskSaveTxt:     .byte " 4 Disk Save",0
deviceTxt:		 .byte " Dev: 08 09 10 11",0
deviceOffsets:	 .byte 7,10,13,16

back:		jmp showTopMenu
.endproc

.proc showLimitedGameOptions
		defineMenu
		jsr showGameOptions::highlightPreferredDevice
		rts

load:	jsrf io_readDirectory
		cpx #0
		beq :+
			ldx #<driveErrorString
			ldy #>driveErrorString
			jsr showModalBox
			jmp showLimitedGameOptions
		:

		lda nbrFilesFound
		bne :+
			ldx #<showGameOptions::noSavesFound
			ldy #>showGameOptions::noSavesFound
			jsr showModalBox
			jmp showLimitedGameOptions
		:
		ldx #<showLimitedGameOptions
		ldy #>showLimitedGameOptions
		jmp showFilePicker

flashLoad:
		jsr isFlashSaveAvailable
		cpx #$ff
		bne :+
			ldx #<showGameOptions::noSavesFound
			ldy #>showGameOptions::noSavesFound
			jsr showModalBox
			jmp showLimitedGameOptions			
		:
		jsrf io_flashLoad
		lda #0
		sta screenEnabled
		inc didLoad
		ldx levelIndex
		jsrf loadMazeFromSave
		lda #0
		sta CURRENT_CAMP_MENU+0
		sta CURRENT_CAMP_MENU+1
		rts

menu:	button  0, 1, 18, 2, flashLoadTxt
		button 20, 1, 18, 2, diskLoadTxt
		button  0, 7, 18, 2, deviceTxt
		button 32, 7,  6, 2, exitTxt
		.byte 0
		.byte 14,"Game Options",0

cr:		clickregion  0,1,20,3, flashLoad,					$31,$ff,$00, 0
		clickregion 20,1,20,3, load, 	 					$32,$ff,$00, 0
		clickregion  0,7,20,3, showGameOptions::device,		$04,$ff,$00, 0
		clickregion 32,7, 8,3, back,  						$05,$ff,$00, 0
		clickregion 32,7, 8,3, back, 						$1f,$ff,$00, 0
		clickregion 32,7, 8,3, back, 						$00,$00,$80, 0
		.word 0

flashLoadTxt: 	 .byte " 1 Flash Load",0
diskLoadTxt:     .byte " 2 Disk Load",0
deviceTxt:		 .byte " Dev: 08 09 10 11",0
deviceOffsets:	 .byte 7,10,13,16

back:	ldx #SCREEN_MODE_mainmenu
		jsrf setGameScreenMode
		jmp showMainMenu		
.endproc

.export directLoadFlashSave
.proc directLoadFlashSave
		jsrf io_flashLoad

.if 0
		getPartyMember 3
		ldy #PartyMember::level
		lda #10
		sta (CURRENT_PARTYMEMBER),y
		iny
		sta (CURRENT_PARTYMEMBER),y
		iny
		sta (CURRENT_PARTYMEMBER),y
		ldy #PartyMember::scribedScrolls
		lda #$ff
		sta (CURRENT_PARTYMEMBER),y		
		iny
		sta (CURRENT_PARTYMEMBER),y		
		iny
		sta (CURRENT_PARTYMEMBER),y		
		iny
		sta (CURRENT_PARTYMEMBER),y		
.endif

		ldx #1
		rts
.endproc

.proc flashScreen
		bit $d011
		bpl *-3
		bit $d011
		bmi *-3
		inc documentTextModeBackground
		bit $d011
		bpl *-3
		bit $d011
		bmi *-3
		dec documentTextModeBackground
		rts
.endproc

; ---------------------------------------------------------------------------------
; File picker menu 
; ---------------------------------------------------------------------------------
.proc showFilePicker
		.pushseg
		.segment "BSS"
			scrolly: .res 1
			selectedFile: .res 1
			backMenu: .res 3
		.popseg

		lda #$4c ;jmp
		sta backMenu+0
		stx backMenu+1
		sty backMenu+2

		defineMenu

		lda #0
		sta scrolly
		sta selectedFile

		lda #1
		sta textColor2
		jsr drawFiles

		lda #6
		sta textColor2
		ldy selectedFile
		jsr colorizeRow

		rts

colorizeRow:
		tya
		clc
		adc #2
		tay
		ldx #3
		jsr f_text_setPosition

		; Draw right selection arrow
		lda #$12
		ldx textColor2
		cpx #1
		bne :+
			lda #' '
		:
		jsr putCharWrapper

		lda textColor2
		ldy textCol2
		:
			sta (TEXT2_D800),y
			iny
			cpy #27
		bne :-
		rts

drawFiles:
		lda #<fileBuffer
		sta TMP2+0
		lda #>fileBuffer
		sta TMP2+1

		ldy #0
		fileLoop:
			ldx #3
			iny
			iny
			jsr f_text_setPosition
			dey
			dey

			tya
			pha

			jsr printEntry

			pla
			tay

			iny
			cpy nbrFilesFound
			beq :+
			cpy #7
		bne fileLoop
		:
		rts

up:		lda nbrFilesFound
		clc
		lda selectedFile
		adc scrolly
		cmp #1
		bcs :+
			jmp flashScreen
		:

		lda #1
		sta textColor2
		ldy selectedFile
		jsr colorizeRow

		ldy selectedFile
		bne :+
			dec scrolly
			jsr scrollUp
			ldx #3
			ldy #2
			jsr f_text_setPosition
			jsr printCurrentEntry
			jmp :++
		:
			dec selectedFile
		:
		lda #6
		sta textColor2
		ldy selectedFile
		jmp colorizeRow

down:	lda nbrFilesFound
		sec
		lda selectedFile
		adc scrolly
		cmp nbrFilesFound
		bcc :+
			jmp flashScreen
		:

		lda #1
		sta textColor2
		ldy selectedFile
		jsr colorizeRow

		ldy selectedFile
		cpy #6
		bne :+
			inc scrolly
			jsr scrollDown
			ldx #3
			ldy #8
			jsr f_text_setPosition
			jsr printCurrentEntry
			jmp :++
		:
			inc selectedFile
		:

		lda #6
		sta textColor2
		ldy selectedFile
		jmp colorizeRow

movePtrs:
		clc
		txa
		adc MSRC+0
		sta MSRC+0
		tya
		adc MSRC+1
		sta MSRC+1
		clc
		txa
		adc MDST+0
		sta MDST+0
		tya
		adc MDST+1
		sta MDST+1
		rts

scrollUp:
		lda #<(vic_screen2+4+22*40)
		sta MSRC+0
		lda #>(vic_screen2+4+22*40)
		sta MSRC+1
		lda #<(vic_screen2+4+23*40)
		sta MDST+0
		lda #>(vic_screen2+4+23*40)
		sta MDST+1

		ldy #0
		:
			tya
			pha

			lda #<23
			ldx #>23
			jsr _memcpy_ram

			ldx #<-40
			ldy #>-40
			jsr movePtrs

			pla
			tay
			iny
			cpy #6
		bne :-
		rts

scrollDown:
		lda #<(vic_screen2+4+18*40)
		sta MSRC+0
		lda #>(vic_screen2+4+18*40)
		sta MSRC+1
		lda #<(vic_screen2+4+17*40)
		sta MDST+0
		lda #>(vic_screen2+4+17*40)
		sta MDST+1

		ldy #0
		:
			tya
			pha

			lda #<23
			ldx #>23
			jsr _memcpy_ram

			ldx #<40
			ldy #>40
			jsr movePtrs

			pla
			tay
			iny
			cpy #6
		bne :-
		rts

select:	cpy selectedFile
		beq choose
		tya
		pha
		lda #1
		sta textColor2
		ldy selectedFile
		jsr colorizeRow
		pla
		tay
		sty selectedFile
		lda #6
		sta textColor2
		jsr colorizeRow
		rts

choose: ; Compose filename
		jsr getCurrentEntry
		ldx #0
		:
			lda prefix,x
			beq :+
			sta filename,x
			inx
		bne :-
		:
		ldy #0
		:
			lda (TMP2),y
			sta filename,x
			inx
			iny
			cpy #8
		bne :-
		jsrf io_load
		cpx #0
		beq :+
			ldx #<driveErrorString
			ldy #>driveErrorString
			jsr showModalBox
			rts
		:
		lda #0
		sta screenEnabled
		inc didLoad

		lda inMainMenu
		bne :+
			ldx levelIndex
			jsrf loadMazeFromSave
		:

		lda #0
		sta CURRENT_CAMP_MENU+0
		sta CURRENT_CAMP_MENU+1
		rts

prefix:	.byte "EOBSAVE/",0

menu:	button  0, 1, 1, 1, upTxt
		button  0, 7, 1, 1, downTxt
		button 32, 7, 6, 2, exitTxt
		.byte 0
		.byte 8,"Select savegame to load",0

cr:		clickregion  32,  7, 8, 3, backMenu,$05,$ff,$00, 0
		clickregion  32,  7, 8, 3, backMenu,$1f,$ff,$00, 0
		clickregion  32,  7, 8, 3, backMenu,$00,$00,$80, 0
		clickregion   0,  1, 3, 3, up,	   	$40,$80,$10, 0
		clickregion   0,  7, 3, 3, down,   	$40,$80,$00, 0
		clickregion   4,  2,23, 7, select, 	$40,$ff,$00, 0
		clickregion $ff,$ff, 0, 0, choose, 	$40,$02,$00, 0
		.word 0

upTxt:	.byte $10,0
downTxt:.byte $11,0

getCurrentEntry:
		clc
		lda #0
		sta TMP2+1
		lda scrolly
		adc selectedFile
		asl
		rol TMP2+1
		asl
		rol TMP2+1
		asl
		rol TMP2+1
		adc #<fileBuffer
		sta TMP2+0
		lda TMP2+1
		adc #>fileBuffer
		sta TMP2+1
		rts

printCurrentEntry:
		jsr getCurrentEntry

printEntry:
		lda #' '
		jsr putCharWrapper

		ldy #0
		:
			lda (TMP2),y
			jsr putCharWrapper
			iny
			cpy #8
		bne :-

		lda #' '
		jsr putCharWrapper
		lda #'-'
		jsr putCharWrapper
		lda #' '
		jsr putCharWrapper
		jsr hhmmss

		clc
		lda TMP2+0
		adc #8
		sta TMP2+0
		bcc :+
			inc TMP2+1
		:
		rts
.endproc

.proc div32
	SIZE_BYTES = 4

	; Zero-Page             		; INPUT     ; OUPUT
	zNumerator		= NUMERATOR32 	; n         ; q = n/d
	zDenominator	= DENOMINATOR32 ; / d	  	;
	zTemp			= RESULT+20		;
	zRemainder		= REMAINDER32 	; 00000000  ; r = q - (n/d)*d

	ldx #SIZE_BYTES-1   ; N bytes for each number

	; Clear remainder
	lda #0
	:
		sta zRemainder,x
		dex
	bpl :-

	; --- Setup ---
	ldx #SIZE_BYTES*8   ; Bits remaining to divide
	divide:
		; Shift high bits of Numerator into Remainder
		asl zNumerator+0
		rol zNumerator+1
		rol zNumerator+2
		rol zNumerator+3
		rol zRemainder+0
		rol zRemainder+1
		rol zRemainder+2
		rol zRemainder+3

		; Temp = Remainder - Denominator
		sec
		lda zRemainder+0
		sbc zDenominator+0
		sta zTemp+0
		lda zRemainder+1
		sbc zDenominator+1
		sta zTemp+1
		lda zRemainder+2
		sbc zDenominator+2
		sta zTemp+2
		lda zRemainder+3
		sbc zDenominator+3
		sta zTemp+3

		; Remainder > Divisor?
		bcc :++
			; Yes
			inc zNumerator

			ldy #SIZE_BYTES-1 ; Update Remainder = Temp
   			:
   				lda zTemp,y
	   			sta zRemainder,y
				dey
			bpl :-
		:

		; No, done dividing all bits?
		dex
	bne divide

	rts
.endproc

; Input time as ASCII in (TMP2)
; Prints 000H 00M 00S
.proc hhmmss
		jsr parseASCII

		; 1 tick is 55ms
		; 1s = 1000/55 ticks = 18 ticks per second
		; 1m = (1000*60)/55 ticks = 1090 ticks per minute
		; 1h = (1000*60*60)/55 ticks = 65454 ticks per hour
		
		; test case
		.if 0
		time = (26*3600 + 23*60 + 34)*1000 / 55
		lda #<(time>>24)
		sta RESULT+3
		lda #<(time>>16)
		sta RESULT+2
		lda #<(time>>8)
		sta RESULT+1
		lda #<(time>>0)
		sta RESULT+0
		.endif

		lda RESULT+0
		sta NUMERATOR32+0
		lda RESULT+1
		sta NUMERATOR32+1
		lda RESULT+2
		sta NUMERATOR32+2
		lda RESULT+3
		sta NUMERATOR32+3

		; hours   = v/65454 ;3600000/55
		lda #<65454
		sta DENOMINATOR32+0
		lda #>65454
		sta DENOMINATOR32+1
		lda #0
		sta DENOMINATOR32+2
		sta DENOMINATOR32+3
		jsr div32

		; QUOTIENT now holds hours, copy to ARGS as expected by htd32
		lda QUOTIENT32+0
		sta ARGS+0
		lda QUOTIENT32+1
		sta ARGS+1
		lda QUOTIENT32+2
		sta ARGS+2
		lda QUOTIENT32+3
		sta ARGS+3
		jsrf htd32

		; Combine the digits into nybbles as printNumber expects
		lda RESULT+8
		asl
		asl
		asl
		asl
		ora RESULT+9
		sta TMP+0
		lda RESULT+7
		sta TMP+1

		lda #'H'
		ldx #3
		jsr printNumberWithSuffix

		; Put remainder into dividend
		lda REMAINDER32+0
		sta NUMERATOR32+0
		lda REMAINDER32+1
		sta NUMERATOR32+1
		lda REMAINDER32+2
		sta NUMERATOR32+2
		lda REMAINDER32+3
		sta NUMERATOR32+3

		; minutes = v/1090
		lda #<1090
		sta DENOMINATOR32+0
		lda #>1090
		sta DENOMINATOR32+1
		lda #0
		sta DENOMINATOR32+2
		sta DENOMINATOR32+3
		jsr div32

		; QUOTIENT now holds minutes
		lda QUOTIENT32+0
		sta ARGS+0
		jsr htdWrapper
		lda #'M'
		ldx #2
		jsr printNumberWithSuffix

		; Put remainder into dividend
		lda REMAINDER32+0
		sta NUMERATOR32+0
		lda REMAINDER32+1
		sta NUMERATOR32+1
		lda REMAINDER32+2
		sta NUMERATOR32+2
		lda REMAINDER32+3
		sta NUMERATOR32+3

		; seconds = v/18
		lda #<18
		sta DENOMINATOR32+0
		lda #>18
		sta DENOMINATOR32+1
		lda #0
		sta DENOMINATOR32+2
		sta DENOMINATOR32+3
		jsr div32

		; QUOTIENT now holds minutes
		lda QUOTIENT32+0
		sta ARGS+0
		jsr htdWrapper
		lda #'S'
		ldx #2
		jsr printNumberWithSuffix

		rts

htdWrapper:
		jsrf htd
		rts

printNumberWithSuffix:
		pha
		jsr printNumber
		pla
		jsr putCharWrapper
		lda #' '
		jmp putCharWrapper

printNumber:
		cpx #3
		bcc :+
			lda TMP+1
			and #15
			jsr putDigit
		:
		lda TMP+0
		lsr
		lsr
		lsr
		lsr
		jsr putDigit

		lda TMP+0
		and #15

putDigit:
		clc
		adc #'0'
		jmp putCharWrapper

parseASCII:
		; TMP2:   "01234567"
		; RESULT: 67 45 23 01
		ldy #0
		ldx #3

		lp:
			lda (TMP2),y
			cmp #'A'
			bcc :+
				sbc #'A'
				clc
				adc #10
				jmp :++
			:
				sec
				sbc #'0'
			:
			asl
			asl
			asl
			asl
			sta RESULT,x
			iny
			lda (TMP2),y
			cmp #'A'
			bcc :+
				sbc #'A'
				clc
				adc #10
				jmp :++
			:
				sec
				sbc #'0'
			:
			ora RESULT,x
			sta RESULT,x
			iny
			dex
		bpl lp
		rts
.endproc
.export printNumber = hhmmss::printNumber

; ---------------------------------------------------------------------------------
; Scribe scrolls menu 
; ---------------------------------------------------------------------------------
.proc showScribeScrolls
		.pushseg
		.segment "BSS"
			numScrolls: .res 1
			scrollInvSlot=frameBuffer_d800
			scrollCharacter=frameBuffer_d800+32
			menuItems: .res 6
			nbrMenuItems: .res 1
		.popseg

		; Go through all inventories and check what available spells to possibly scribe
		lda #0
		sta numScrolls
		ldy #1
		nextSpell:
			tya
			pha
			ldx #0
			nextMember:
				lda partyMembers_lo,x
				sta CURRENT_PARTYMEMBER+0
				lda partyMembers_hi,x
				sta CURRENT_PARTYMEMBER+1
				stx CURRENT_PARTYMEMBER_INDEX

				sty ARGS+1
				lda #34
				sta ARGS+0
				jsrf items_checkInventoryForItemByTypeAndValue
				ldy ARGS+1

				lda ARGS+0
				sta scrollInvSlot-1,y
				beq :+
					inc numScrolls
					lda CURRENT_PARTYMEMBER_INDEX
					sta scrollCharacter-1,y
					jmp break
				:

				ldx CURRENT_PARTYMEMBER_INDEX
				inx
				cpx #6
			bne nextMember
			break:
			pla
			tay
			iny
			cpy #33
		bne nextSpell

		lda numScrolls
		bne :+
			notify "You don't have any scrolls to be",$a,"scribed."
			jmp showTopMenu
		:

		; Select character to scribe scroll with
		ldx #<charSelected
		ldy #>charSelected
		lda #IS_MAGE
		jsr showSelectCharMenu
		bcs :+
			notify "You don't have any mage able to",$a,"learn spells."
			jmp showTopMenu
		:
		rts

charSelected:	jsr populateUnknownSpells
		defineMenu
		jsr drawSelectedCharName
		lda nbrMenuItems
		bne :+
			notify "You don't have any scrolls that this",$a,"mage needs."
			jmp showTopMenu
		:

redrawLoop:
		redrawMenu
		jsr drawSelectedCharName

		ldx #1
		ldy #2
		jsr f_text_setPosition

		; Draw populated spells here
		ldy #0
		:
			lda #7
			sta textColor2
			tya
			clc
			adc #'1'
			jsr putCharWrapper
			lda #'.'
			jsr putCharWrapper
			inc textCol2
			lda #1
			sta textColor2

			ldx menuItems,y
			jsrf printSpellName2
			lda #$a
			jsr putCharWrapper

			sec
			lda textCol2
			sbc #3
			sta textCol2

			iny
			cpy nbrMenuItems
		bne :-

		rts

populateUnknownSpells:
		; Find at most 6 non-scribed spells among the ones found in the inventories
		lda #0
		sta nbrMenuItems
		ldy #PartyMember::scribedScrolls
		ldx #0
		loop:
			lda scrollInvSlot,x
			beq noScrollForThisSpell
				lda bits,x
				and (CURRENT_PARTYMEMBER),y
				beq :+
					lda #0
					sta scrollInvSlot,x
					jmp noScrollForThisSpell
				:
				stx XREG
					inx
					txa
					ldx nbrMenuItems
					sta menuItems,x
					inx
					stx nbrMenuItems
					cpx #6
					beq breakPopulate
				ldx XREG
			noScrollForThisSpell:
				lda bits,x
				bpl :+
					iny
				:
			inx
			cpx #32
		bne loop
breakPopulate:	rts

clickSpell:	cpy nbrMenuItems
		bcc :+
			rts
		:

		ldx menuItems,y
		dex
		stx XREG

		; Remove the scroll from the inventory
		ldy scrollInvSlot,x
		lda scrollCharacter,x
		tax
		lda partyMembers_lo,x
		sta TMP+0
		lda partyMembers_hi,x
		sta TMP+1
		lda #0
		sta (TMP),y
		iny
		sta (TMP),y

		; Remove the menu options
		ldx XREG
		lda #0
		sta scrollInvSlot,x

		; Scribe the scroll
		txa
		lsr
		lsr
		lsr
		clc
		adc #PartyMember::scribedScrolls
		tay
		lda (CURRENT_PARTYMEMBER),y
		ora bits,x
		sta (CURRENT_PARTYMEMBER),y

		; Redraw
		ldx CURRENT_PARTYMEMBER_INDEX
		inc partyMemberStatsChanged,x
		jsr populateUnknownSpells
		lda nbrMenuItems
		bne :+
			jmp showTopMenu
		:
		jmp redrawLoop

keySpell:	tay
		jmp clickSpell

menu:		button 32, 7,  6, 2, exitTxt
		.byte 0
		.byte 1,"Scribe Scrolls",0

cr:		clickregion 32,7, 8,3,   back, 		$05,$ff,$00, 5
		clickregion 32,7, 8,3,   back,		$1f,$ff,$00, 5
		clickregion 32,7, 8,3,   back,		$00,$00,$80, 5
		clickregion 1,2,20, 6,   clickSpell,$ff,$ff,$00, 0
		clickregion $ff,$ff,0,0, keySpell, 	$31,$ff,$00, 0
		clickregion $ff,$ff,0,0, keySpell, 	$32,$ff,$00, 1
		clickregion $ff,$ff,0,0, keySpell, 	$33,$ff,$00, 2
		clickregion $ff,$ff,0,0, keySpell, 	$34,$ff,$00, 3
		clickregion $ff,$ff,0,0, keySpell, 	$35,$ff,$00, 4
		clickregion $ff,$ff,0,0, keySpell, 	$36,$ff,$00, 5
		.word 0

back:		jmp showTopMenu

.endproc



; ---------------------------------------------------------------------------------
; Rest
; ---------------------------------------------------------------------------------
.proc showRest
		.pushseg
		.segment "BSS"
			useHealers: .res 1
			hours: .res 2
			regenerateSpellsOnMember: .res 6 	;%01=cleric spells. %10=mage spells
			crs: .res 6
			everySix: .res 1
			restPartyElapsedTime: .res 4
			hourInDay: .res 1
			injured: .res 1
			haveAskedToContinue: .res 1
		.popseg

		; Reset timeouts and total rest time
		lda #0
		sta haveAskedToContinue
		sta hours+0
		sta hours+1
		sta hourInDay
		sta restPartyElapsedTime+0
		sta restPartyElapsedTime+1
		sta restPartyElapsedTime+2
		sta restPartyElapsedTime+3
		sta everySix
		ldx #5
		:
			sta crs,x
			dex
		bpl :-

		; Make sure no monsters are near
		ldx #5
		jsr updateMonsters
		bcc :+
			lda #0
			sta CURRENT_CAMP_MENU+0
			sta CURRENT_CAMP_MENU+1
			rts
		:

		; Ok to rest initially
		defineMenu

		; Check for poisoned characters
		ldx #0
		:
			ldy #IS_ACTIVE
			jsr getMemberStatus
			bcc :++
				ldy #NOT_POISONED
				jsr getMemberStatus
				bcs :++
					confirm "Poisoned party members will die!",$a,"Rest anyway?"
					cmp #1
					beq :+
						jmp showTopMenu
					:
						redrawMenu
					jmp :++
			:
			inx
			cpx #6
		bne :---
		:

		; Anybody injured?
		lda #0
		sta useHealers
		jsr getCharacterWithLowestHp
		stx injured
		bpl somebodyInjured
			jmp nobodyInjured
		somebodyInjured:
			; Look for healers
			ldx #0
			:
				txa
				pha

				; Make sure they are capable
				ldy #IS_ACTIVE|IS_CONSCIOUS|NOT_PARALYZED
				jsr getMemberStatus
				bcs :+
					jmp continue
				:

				; And that it is either a cleric or a paladin
				ldy #PartyMember::class
				lda (CURRENT_PARTYMEMBER),y
				sta ARGS+0
				ldy #CLASS_CLERIC
				jsrf gameState_getMaxExpLevelIndex
				lda ARGS+0
				bpl :+
					ldy #PartyMember::class
					lda (CURRENT_PARTYMEMBER),y
					sta ARGS+0
					ldy #CLASS_PALADIN
					jsrf gameState_getMaxExpLevelIndex
					lda ARGS+0
					bpl :+
						jmp continue
				:

				; And that they have their holy symbol
				lda #ITEMTYPE_HOLYSYMBOL
				sta ARGS+0
				lda #<-1
				sta ARGS+1
				jsrf items_checkInventoryForItemByTypeAndValue
				lda ARGS+0
				beq continue

				; And that they have healing spells
				jsr checkHealSpells
				bcc continue
					pla
					confirm "Will your healers heal the party?"
					sta useHealers
					redrawMenu
					jsr showText
					jmp nobodyInjured
				continue:				
				pla
				tax
				inx
				cpx #6
				beq nobodyInjured
			jmp :---
		nobodyInjured:

		; Figure out which members are subject to spell regeneration
		ldx #0
		whoShouldSpellRegenLoop:
			lda #0
			sta regenerateSpellsOnMember,x

			txa
			pha
			ldy #IS_ACTIVE
			jsr getMemberStatus
			pla
			tax
			bcs :+
				jmp nextMember
			:

			checkCleric:
				lda #ITEMTYPE_HOLYSYMBOL
				sta ARGS+0
				lda #<-1
				sta ARGS+1
				txa
				pha
				jsrf items_checkInventoryForItemByTypeAndValue
				pla
				tax
				lda ARGS+0
				beq checkMage

				ldy #PartyMember::clericSpells
				:
					lda (CURRENT_PARTYMEMBER),y
					beq :+
						inc regenerateSpellsOnMember,x
						jmp checkMage
					:
					iny
					cpy #(PartyMember::clericSpells+30)
				bne :--

			checkMage:
				lda #ITEMTYPE_SPELLBOOK
				sta ARGS+0
				lda #<-1
				sta ARGS+1
				txa
				pha
				jsrf items_checkInventoryForItemByTypeAndValue
				pla
				tax
				lda ARGS+0
				beq nextMember

				ldy #PartyMember::mageSpells
				:
					lda (CURRENT_PARTYMEMBER),y
					beq :+
						lda regenerateSpellsOnMember,x
						ora #2
						sta regenerateSpellsOnMember,x
						jmp nextMember
					:
					iny
					cpy #(PartyMember::mageSpells+30)
				bne :--

			nextMember:
			inx
			cpx #6
			beq :+
		jmp whoShouldSpellRegenLoop
		:

		jsr showText
		jmp restLoop

showText:	ldx #1
		ldy #2
		jsr f_text_setPosition
		lda #3
		sta textColor2
		ldx #<restingParty
		ldy #>restingParty
		clc
		jsr text_writeNullString2
		lda #1
		sta textColor2
		ldx #<hoursRested
		ldy #>hoursRested
		clc
		jsr text_writeNullString2
		jsr updateRestTime
		rts

		restLoop:
			jsr regenerateSpells

			lda useHealers
			beq :+
				jsr healPartyMembers
			:

			ldx everySix
			inx
			cpx #6
			bne notEverySix
				jsr hourTick
				bcc :++
					confirm "Your party is starving.",$a,"Do you wish to continue resting?"
					cmp #0
					bne :+
						jmp abortRest
					:
					redrawMenu
					jsr showText
				:

				jsr getCharacterWithLowestHp
				stx injured

				ldx #3
				jsr updateMonsters
				bcc :+
					jmp abortRest
				:
				ldx #0
			notEverySix:
			stx everySix

			jsrf refreshUI

			; If all spells are learned but party still injured, ask if we should continue resting
			lda haveAskedToContinue
			bne :+
				lda injured
				bmi :+
					jsr checkSpellsToLearn
					bcs :+
						inc haveAskedToContinue
						confirm "Someone is still injured.",$a,"Rest until healed?"
						cmp #0
						beq abortRest
						redrawMenu
						jsr showText
			:

			; Check party status here
			clc
			jsr checkPartyStatus
			bcs abortRest

			jsr checkSpellsToLearn
			bcs :+
				lda injured
				bmi :++
			:

			jsr pollAnyEvent
			bcs :+
		jmp restLoop
		:

		lda injured
		bpl :+
			notify "All characters are fully rested."
		:

abortRest:
		clc
		lda _tick+0
		adc restPartyElapsedTime+0
		sta _tick+0
		lda _tick+1
		adc restPartyElapsedTime+1
		sta _tick+1
		lda _tick+2
		adc restPartyElapsedTime+2
		sta _tick+2
		lda _tick+3
		adc restPartyElapsedTime+3
		sta _tick+3

		lda #0
		sta CURRENT_CAMP_MENU+0
		sta CURRENT_CAMP_MENU+1
		rts

waitForStarveConfirm:


restingParty:	.byte "Resting party.",$a,$a,0
hoursRested:	.byte "Hours rested:",0

menu:		.byte 0
		.byte 18,"Rest",0
cr:		.word 0


.proc checkSpellsToLearn
		ldx #0
		checkLoop:
			inc crs,x

			lda regenerateSpellsOnMember,x
			bne :+
				jmp nextMember
			:

			txa
			pha
			ldy #IS_ACTIVE|IS_ALIVE
			jsr getMemberStatus
			pla
			tax
			bcs :+
				jmp nextMember
			:

			checkClericSpells:
				txa
				pha
				lda regenerateSpellsOnMember,x
				and #1
				beq clericSpellsDone
					ldx #0
					ldy #PartyMember::clericSpells
					:
						lda (CURRENT_PARTYMEMBER),y
						bpl :+
							pla
							sec
							rts
						:
						iny
						inx
						cpx #30
					bne :--
				clericSpellsDone:
				pla
				tax

			regenerateMageSpells:
				txa
				pha
				lda regenerateSpellsOnMember,x
				and #2
				beq mageSpellsDone
					ldx #0
					ldy #PartyMember::mageSpells
					:
						lda (CURRENT_PARTYMEMBER),y
						bpl :+
							pla
							sec
							rts
						:
						iny
						inx
						cpx #30
					bne :--
				mageSpellsDone:
				pla
				tax

			nextMember:
			inx
			cpx #6
			beq :+
		jmp checkLoop
		:

		clc
		rts
.endproc

.export checkPartyStatus
; c:boolean: handle death
;
; Returns:
; c:boolean: party dead
.proc checkPartyStatus
		php

		; Count characters alive
		lda #0
		sta TMP
		ldx #0
		:
			ldy #NOT_PARALYZED|IS_CONSCIOUS|IS_ACTIVE
			jsrf gameState_getMemberStatus
			bcc :+
				inc TMP
			:
			inx
			cpx #6
		bne :--

		lda TMP
		beq :+
			; >0 characters alive
			plp
false:		clc
			rts
		:

		; Party is dead
		plp
		bcc false ; But lets not handle death just now

		jsrf gui_refreshWholeParty
		jsrf refreshUI

		lda #0
		sta timersEnabled
		ldx #SCREEN_MODE_text
		jsrf setGameScreenMode
		ldx #51
		sec
		jsrf text_writeTextMessage
		jmp restart
.endproc

.proc hourTick
		.pushseg
		.segment "BSS"
			starving: .res 1
		.popseg

		lda #0
		sta starving

		inc hours+0
		bne :+
			inc hours+1
		:
		ldx hourInDay
		inx
		cpx #24
		bne :+	
			ldx #0
		:
		stx hourInDay

		jsr updateRestTime

		clc
		lda restPartyElapsedTime+0
		adc #<(32760>>0)
		sta restPartyElapsedTime+0
		lda restPartyElapsedTime+1
		adc #<(32760>>8)
		sta restPartyElapsedTime+1
		lda restPartyElapsedTime+2
		adc #<(32760>>16)
		sta restPartyElapsedTime+2
		lda restPartyElapsedTime+3
		adc #<(32760>>24)
		sta restPartyElapsedTime+3

		; Update poisoning
		.scope
		ldx #0
		:
			ldy #IS_ACTIVE
			jsr getMemberStatus
			bcc continue
			ldy #NOT_POISONED
			jsr getMemberStatus
			bcs continue

			stx ARGS+0
			lda #<10
			sta ARGS+1
			lda #>10
			sta ARGS+2

			txa
			pha
			jsrf gameState_giveMemberDamage
			pla
			tax

			continue:

			inx
			cpx #6
		bne :-
		.endscope

		; Check starving every 8th hour
		lda hours
		and #7
		beq :+
			jmp dontCheckStarving
		:
			.scope
			ldx #0
			starveLoop:
				txa
				pha

				; Add Lay on Hands for paladins
				ldy #PartyMember::class
				lda (CURRENT_PARTYMEMBER),y
				cmp #MULTICLASS_PALADIN
				bne notPaladin
					ldy #PartyMember::clericSpells
					ldx #10
					lda #24
					jsr memchr_current_partymember
					bcs notPaladin
						ldy #PartyMember::clericSpells
						ldx #10
						lda #<-24
						jsr memchr_current_partymember
						bcc :+
							lda #24
							sta (CURRENT_PARTYMEMBER),y
							jmp notPaladin
						:
						ldy #PartyMember::clericSpells
						ldx #10
						lda #0
						jsr memchr_current_partymember
						lda #24
						sta (CURRENT_PARTYMEMBER),y						
				notPaladin:

				; Update hitpoints and food status
				ldy #IS_ACTIVE|IS_ALIVE
				jsr getMemberStatus
				pla
				tax
				bcs :+
					jmp continue
				:
					ldy #PartyMember::food
					lda (CURRENT_PARTYMEMBER),y
					beq noFood
						; Regain 1hp every 8th hour
						sec
						ldy #PartyMember::hpCurrent+0
						lda (CURRENT_PARTYMEMBER),y
						ldy #PartyMember::hp+0
						sbc (CURRENT_PARTYMEMBER),y
						ldy #PartyMember::hpCurrent+1
						lda (CURRENT_PARTYMEMBER),y
						ldy #PartyMember::hp+1
						sbc (CURRENT_PARTYMEMBER),y
						bpl :+
							clc
							ldy #PartyMember::hpCurrent+0
							clc
							lda (CURRENT_PARTYMEMBER),y
							adc #1
							sta (CURRENT_PARTYMEMBER),y
							iny							
							lda (CURRENT_PARTYMEMBER),y
							adc #0
							sta (CURRENT_PARTYMEMBER),y
							inc partyMemberStatsChanged,x
						:

						; Loose 5 food unless there is a Ring of Sustenance
						stx ARGS+0
						lda #RING_OF_SUSTENANCE
						sta ARGS+1
						jsrf gameState_memberHaveRing
						lsr ARGS+1
						bcs :+++
							ldy #PartyMember::food
							lda (CURRENT_PARTYMEMBER),y
							sec
							sbc #5
							beq :+
							bcs :++
							:
								inc starving
								lda #0
							:
							sta (CURRENT_PARTYMEMBER),y
							inc partyMemberStatsChanged,x
						:

						jmp continue
					noFood:
						; Only inflict damage due to starvation at 08:00 and 16:00 each day
						lda hourInDay
						beq continue
						stx ARGS+0
						lda #<1
						sta ARGS+1
						lda #>1
						sta ARGS+2
						txa
						pha
						jsrf gameState_giveMemberDamage
						pla
						tax
						inc starving
				continue:
				inx
				cpx #6
			beq :+
			jmp starveLoop
			:
			.endscope
		dontCheckStarving:
		
		lda starving
		cmp #1
		rts
.endproc

; a = value to look for
; x = max items
; y = start offset
; return c=1=found, y=ptr. c=0=notfound
.proc memchr_current_partymember
	sta TMP
	:
		lda (CURRENT_PARTYMEMBER),y
		cmp TMP
		bne :+
			sec
			rts
		:
		iny
		dex
	bne :--
	clc
	rts
.endproc

.proc healPartyMembers
		ldx #0
		nextMember:
			lda injured
			bpl :+
				rts
			:
			lda regenerateSpellsOnMember,x
			and #1
			bne :+
				jmp continue
			:
				lda partyMembers_lo,x
				sta CURRENT_PARTYMEMBER+0
				lda partyMembers_hi,x
				sta CURRENT_PARTYMEMBER+1

				lda crs,x
				cmp #48
				bcc tryHealSpells
					ldy #PartyMember::clericSpells
					loopSpells2:
						lda (CURRENT_PARTYMEMBER),y
						cmp #<-2
						bne :+
							rts
						:
						cmp #<-15
						bne :+
							rts
						:
						cmp #<-20
						bne :+
							rts
						:
						iny
						cpy #(PartyMember::clericSpells+30)
					bne loopSpells2

				tryHealSpells:
					ldy #PartyMember::clericSpells
					loopSpells:
						lda (CURRENT_PARTYMEMBER),y
						cmp #2 ; Cure Light Wounds
						bne :+
							dice 1,8,0
							jmp castSpell
						:cmp #15 ; Cure Serious Wounds
						bne :+
							dice 2,8,1
							jmp castSpell
						:cmp #20 ; Cure Critical Wounds
						beq :+
							jmp continueSpell
						:
							dice 3,8,3
							castSpell:
								lda (CURRENT_PARTYMEMBER),y
								eor #$ff
								clc
								adc #1
								sta (CURRENT_PARTYMEMBER),y
								lda #0
								sta crs,x

								txa
								pha
								tya
								pha

								lda #1
								sta textColor
								jsr printMemberName
								ldx #<castsTxt
								ldy #>castsTxt
								jsr text_writeNullString3
								ldx injured
								inc partyMemberStatsChanged,x
								lda partyMembers_lo,x
								sta CURRENT_PARTYMEMBER+0
								lda partyMembers_hi,x
								sta CURRENT_PARTYMEMBER+1
								jsr printMemberName
								ldx #<periodReturnTxt
								ldy #>periodReturnTxt
								jsr text_writeNullString3

								jsrf rollDice
								ldy #PartyMember::hpCurrent
								clc
								lda DICE_RESULT+0
								adc (CURRENT_PARTYMEMBER),y
								sta (CURRENT_PARTYMEMBER),y
								iny
								lda DICE_RESULT+1
								adc (CURRENT_PARTYMEMBER),y
								sta (CURRENT_PARTYMEMBER),y

								sec
								ldy #PartyMember::hpCurrent+0
								lda (CURRENT_PARTYMEMBER),y
								ldy #PartyMember::hp+0
								sbc (CURRENT_PARTYMEMBER),y
								ldy #PartyMember::hpCurrent+1
								lda (CURRENT_PARTYMEMBER),y
								ldy #PartyMember::hp+1
								sbc (CURRENT_PARTYMEMBER),y
								bmi :+
									lda (CURRENT_PARTYMEMBER),y
									ldy #PartyMember::hpCurrent+1
									sta (CURRENT_PARTYMEMBER),y
									ldy #PartyMember::hp+0
									lda (CURRENT_PARTYMEMBER),y
									ldy #PartyMember::hpCurrent+0
									sta (CURRENT_PARTYMEMBER),y
								:

								pla
								tay
								pla
								tax
								lda partyMembers_lo,x
								sta CURRENT_PARTYMEMBER+0
								lda partyMembers_hi,x
								sta CURRENT_PARTYMEMBER+1
						continueSpell:
						iny
						cpy #(PartyMember::clericSpells+30)
					beq continue
					jmp loopSpells
			continue:
			inx
			cpx #6
			bne :+
				rts
			:
		jmp nextMember
castsTxt: .byte " casts healing on ",0
.endproc

.proc regenerateSpells
		ldx #0
		regenLoop:
			inc crs,x

			lda regenerateSpellsOnMember,x
			bne :+
				jmp nextMember
			:

			txa
			pha
			ldy #IS_CONSCIOUS ; Don't have to check IS_ACTIVE here since that is included in the regenerateSpellsOnMember-cache
			jsr getMemberStatus
			pla
			tax
			bcs :+
				jmp nextMember
			:

			ldy #PartyMember::food
			lda (CURRENT_PARTYMEMBER),y
			bne :+
				jmp nextMember
			:

			regenerateClericSpells:
				lda crs,x
				sta TMP
				txa
				pha
				lda regenerateSpellsOnMember,x
				and #1
				beq clericSpellsDone
					ldx #0
					ldy #PartyMember::clericSpells
					:
						lda clericSpellsPauseTable,x
						cmp TMP
						bcs clericSpellsDone
						lda (CURRENT_PARTYMEMBER),y
						bpl :+
							eor #$ff
							clc
							adc #1
							sta (CURRENT_PARTYMEMBER),y
							ldx #<gainedText
							ldy #>gainedText
							sec
							jsr printStatus
							lda #48
							sta TMP
							jmp clericSpellsDone
						:
						iny
						inx
						cpx #30
					bne :--
				clericSpellsDone:
				pla
				tax
				lda TMP
				sta crs,x

			regenerateMageSpells:
				lda crs,x
				sta TMP
				txa
				pha
				lda regenerateSpellsOnMember,x
				and #2
				beq mageSpellsDone
					ldx #0
					ldy #PartyMember::mageSpells
					:
						lda mageSpellsPauseTable,x
						cmp TMP
						bcs mageSpellsDone

						lda (CURRENT_PARTYMEMBER),y
						bpl :+
							eor #$ff
							clc
							adc #1
							sta (CURRENT_PARTYMEMBER),y
							ldx #<memorizedText
							ldy #>memorizedText
							clc
							jsr printStatus
							lda #48
							sta TMP
							jmp mageSpellsDone
						:
						iny
						inx
						cpx #30
					bne :--
				mageSpellsDone:
				pla
				tax
				lda TMP
				sta crs,x

			nextMember:
			inx
			cpx #6
			beq :+
		jmp regenLoop
		:
		rts

printStatus:	php
		pha
		txa
		pha
		tya
		pha
		lda #1
		sta textColor
		jsr printMemberName
		pla
		tay
		pla
		tax
		jsr text_writeNullString3
		pla
		plp
		bcc :+
			clc
			adc #25
		:
		tax
		jsrf printSpellName3
		ldx #<periodReturnTxt
		ldy #>periodReturnTxt
		jsr text_writeNullString3
		rts

gainedText:	.byte " gained ",0
memorizedText:	.byte " memorized ",0

mageSpellsPauseTable:
		.repeat 30,I
			.byte I/6+48
		.endrep
clericSpellsPauseTable:
		.repeat 30,I
			.byte I/10+48
		.endrep
.endproc

periodReturnTxt:.byte ".",$a,0

.proc updateRestTime
		.pushseg
		.segment "BSS"
			digits: .res 5
		.popseg

		lda hours+0
		sta DIVIDEND+0
		lda hours+1
		sta DIVIDEND+1
		lda #10
		sta DIVISOR+0
		lda #0
		sta DIVISOR+1

		.repeat 5,I
			jsr divide
			lda REMAINDER+0
			sta digits+4-I
		.endrep

		lda #15
		sta textCol2
		ldx #0
		:
			lda digits,x
			bne :+
			inx
			cpx #4
		bne :-
		:
			clc
			lda digits,x
			adc #'0'
			jsr putCharWrapper
			inx
			cpx #5
		bne :-

		rts
.endproc

; CURRENT_PARTYMEMBER
.proc checkHealSpells
		ldy #PartyMember::clericSpells
		ldx #0
		:
			lda (CURRENT_PARTYMEMBER),y
			cmp #<2  ;Cure Light Wounds
			beq true
			cmp #<-2
			beq true
			cmp #<15 ;Cure Serious Wounds
			beq true
			cmp #<-15
			beq true
			cmp #<20 ;Cure Critical Wounds
			beq true
			cmp #>-20
			beq true
			iny
			inx
			cpx #30
		bne :-
		clc
		rts
true:		sec
		rts
.endproc

; Return character index in x or -1
; If x>=0 then CURRENT_PARTYMEMBER is set accordingly
.proc getCharacterWithLowestHp
		lda #<900
		sta TMP+0
		lda #>900
		sta TMP+1
		lda #<-1
		sta TMP2

		ldx #0
		:
			ldy #IS_ACTIVE|IS_ALIVE
			jsr getMemberStatus
			bcc :+

			sec
			ldy #PartyMember::hpCurrent+0
			lda (CURRENT_PARTYMEMBER),y
			ldy #PartyMember::hp+0
			sbc (CURRENT_PARTYMEMBER),y
			ldy #PartyMember::hpCurrent+1
			lda (CURRENT_PARTYMEMBER),y
			ldy #PartyMember::hp+1
			sbc (CURRENT_PARTYMEMBER),y
			bpl :+

			sec
			ldy #PartyMember::hpCurrent+0
			lda (CURRENT_PARTYMEMBER),y
			sbc TMP+0
			iny
			lda (CURRENT_PARTYMEMBER),y
			sbc TMP+1
			bpl :+

				lda (CURRENT_PARTYMEMBER),y
				sta TMP+1
				dey
				lda (CURRENT_PARTYMEMBER),y
				sta TMP+0
				dey
				stx TMP2

			:
			inx
			cpx #6
		bne :--

		ldx TMP2
		rts
.endproc

; Return boolean in c if monsters are nearby
.proc updateMonsters
		;ldx #5
		:
			txa
			pha

			inc partyResting
			lda #0
			sta ARGS+0
			jsrf monsterTimerHandler
			lda #1
			sta ARGS+0
			jsrf monsterTimerHandler
			lda #0
			sta ARGS+0
			jsrf thrownTimerHandler
			dec partyResting

			ldy #0
			:
				lda monster_phase,y
				cmp #MONSTER_STATE_INACTIVE
				beq :+
					lda partyPosition+0
					sta ARGS+0
					lda partyPosition+1
					sta ARGS+1
					lda monster_position_lo,y
					sta ARGS+2
					lda monster_position_hi,y
					sta ARGS+3
					jsrf gameState_getDistance
					lda ARGS+0
					cmp #2
					bcs :+
						pla
						notify "You can't rest here, monsters are",$a,"near."
						jmp returnTrue
				:
				iny
				cpy #30
			bne :--

			pla
			tax
			dex
			beq :+
		jmp :---
		:

returnFalse:	clc
		bcc :+
returnTrue:	sec
		:
		rts
.endproc

.endproc

; 40    "\rWill your healers\rheal the party?",
; 41    " Someone is still\r injured. Rest\r until healed?",
; 42    "Resting party.",
; 43    "\r All characters\r are fully\r rested.",
; 44    " Your party needs\r to rest to gain\r spells."

; 59     "Poisoned party\rmembers will die!\rRest anyway?"

; "\rYou can't rest here, monsters are near."

.export mainmenu_do
.proc mainmenu_do
		lda #POINTERENABLED_REAL
		sta pointerEnabled
		lda #1
		sta inMainMenu
		lda #$ff
		sta MOUSEEVENT+MouseEvent::buttons
		lda #$00
		sta KEYEVENT+KeyEvent::keyPressed
		sta POINTER_ITEM+0
		sta POINTER_ITEM+1
		sta didLoad
		jsrf updatePointerSprite

		clc
		ldx #<screenModeMainMenu
		ldy #>screenModeMainMenu
		jsr setScreenMode
		lda #$b
		sta $d022
		lda #$f
		sta $d023
		lda #6
		sta $d020

		jsr showMainMenu
		loop:
			jsr handleMenu
			lda CURRENT_CAMP_MENU+0
			ora CURRENT_CAMP_MENU+1
		bne loop
		ldx didLoad
		rts
.endproc

; ---------------------------------------------------------------------------------
; Main menu
; ---------------------------------------------------------------------------------
.proc showMainMenu
		lda #$1
		sta textColor2
		lda #<menu
		sta TMP+0
		lda #>menu
		sta TMP+1
		clc
		jsr drawButtons
		lda #<cr
		sta CURRENT_CAMP_MENU+0
		lda #>cr
		sta CURRENT_CAMP_MENU+1
		
		ldx #0
		ldy #9
		jsr f_text_setPosition
		lda #$7
		sta textColor2
		ldx #<f1txt
		ldy #>f1txt
		clc
		jsr text_writeNullString2
		lda #$1
		sta textColor2

		ldx mouseType
		ldy mouseTypes_hi,x
		lda mouseTypes_lo,x
		tax
		clc
		jsr text_writeNullString2

		; Calculate total string length
		ldx #<version
		ldy #>version
		jsr strlen
		sty COUNT
		ldx system
		ldy systemTypes_hi,x
		lda systemTypes_lo,x
		tax
		jsr strlen
		tya
		clc
		adc COUNT
		sta COUNT
		ldx sidModel
		ldy sidModels_hi,x
		lda sidModels_lo,x
		tax
		jsr strlen
		tya
		clc
		adc COUNT
		sta COUNT

		; Right align
		sec
		lda #40
		sbc COUNT
		tax
		ldy #9
		jsr f_text_setPosition
		lda #7
		sta textColor2

		ldx #<version
		ldy #>version
		clc
		jsr text_writeNullString2_noWordWrap

		ldx system
		ldy systemTypes_hi,x
		lda systemTypes_lo,x
		tax
		clc
		jsr text_writeNullString2_noWordWrap

		ldx sidModel
		ldy sidModels_hi,x
		lda sidModels_lo,x
		tax
		clc
		jsr text_writeNullString2_noWordWrap

		lda #$ff
		sta screenEnabled

		rts

strlen:	stx TMP+0
		sty TMP+1
		ldy #0
		:
			lda (TMP),y
			bne :+
				rts
			:
			iny
		bne :--
		rts

f1txt:	.byte "F1: ",0

mouseTypes_lo: .byte <m0,<m1,<m2
mouseTypes_hi: .byte >m0,>m1,>m2

m0:		.byte "1351 (Acc)   ",0
m1:		.byte "1351 (No Acc)",0
m2:		.byte "None         ",0

systemTypes_lo: .byte <t0,<t1,<t2,<t3
systemTypes_hi: .byte >t0,>t1,>t2,>t3

t0:		.byte "/PAL",0
t1:		.byte "/NEW NTSC",0
t2:		.byte "/OLD NTSC",0
t3:		.byte "/DREAN",0

sidModels_lo: .byte <s0,<s1
sidModels_hi: .byte >s0,>s1

s0:		.byte "/6581",0
s1:		.byte "/8580",0

version:.byte "v",VERSION,0

menu:	button  8, 2, 23, 2, loadTxt
		button  8, 4, 23, 4, startTxt
		button  8, 5, 23, 5, quickStartTxt
		button  8, 6, 23, 7, watchIntroTxt
		.byte 0

cr:		clickregion 9,3,24,1, handler, $0c,$ff,$00, 0
		clickregion 9,5,24,1, handler, $13,$ff,$00, 1
		clickregion 9,6,24,1, handler, $11,$ff,$00, 2
		clickregion 9,7,24,1, handler, $17,$ff,$00, 3
		.word 0

loadTxt: 	  	.byte " Load game in progress",0
startTxt:		.byte "   Start a new party  ",0
quickStartTxt:	.byte "    Quickstart game   ",0
watchIntroTxt:	.byte "      Watch intro     ",0

handler:
		cmp #0
		bne :+
			ldx #SCREEN_MODE_text
			jsrf setGameScreenMode
			jmp showLimitedGameOptions
		:
		cmp #1
		bne :+
			lda #0
			sta CURRENT_CAMP_MENU+0
			sta CURRENT_CAMP_MENU+1
			rts
		:
		cmp #2
		bne :+
			jsrf createDefaultParty
			lda #0
			sta CURRENT_CAMP_MENU+0
			sta CURRENT_CAMP_MENU+1
			inc didLoad
			inc didLoad
			rts
		:

		clc
		ldx #<screenModeNoop
		ldy #>screenModeNoop
		jsr setScreenMode
		lda #0
		sta $d015
		sta screenEnabled
		jmp restart
.endproc

.export camp_do
.proc camp_do
		lda #POINTERENABLED_REAL
		sta pointerEnabled
		lda #0
		sta inMainMenu
		lda #$ff
		sta MOUSEEVENT+MouseEvent::buttons
		lda #$00
		sta KEYEVENT+KeyEvent::keyPressed

		lda muted
		pha
		lda #1
		sta muted
		lda #0
		sta $d418

		lda timersEnabled
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

		jsrf darkenFrameBuffer

		ldx #SCREEN_MODE_text
		jsrf setGameScreenMode

		lda #$b
		sta $d022
		lda #$f
		sta $d023

		lda #0
		sta needRest
		sta partyResting
		sta didLoad

		jsr showTopMenu
		loop:
			jsr handleMenu
			lda CURRENT_CAMP_MENU+0
			ora CURRENT_CAMP_MENU+1
		bne loop

		lda #1
		ldx #20
		:
			sta affectedSpriteColumns,x
			dex
		bpl :-

		lda didLoad
		beq :+
			clc
			ldx #<screenModeGameNormal
			ldy #>screenModeGameNormal
			jsr setScreenMode
			jsrf guiInit
			inc SHOULDRENDER
			jsrf render
			jmp :++
		:
			ldx #SCREEN_MODE_normal
			jsrf setGameScreenMode
		:
		jsrf updatePointerSprite
		lda #$ff
		sta screenEnabled
		pla
		sta timersEnabled
		pla
		sta muted

		sec
		jsr checkPartyStatus

		; Make sure member 4&5 have their health bars updated properly.
		inc partyMemberStatsChanged+4
		inc partyMemberStatsChanged+5

		sec
		rts
.endproc

.proc pollAnyEvent
		clc
		lda MOUSEEVENT+MouseEvent::buttons
		bmi :+
			lda #$ff
			sta MOUSEEVENT+MouseEvent::buttons
			sec
		:
		lda KEYEVENT+KeyEvent::keyPressed
		beq :+
			lda #0
			sta KEYEVENT+KeyEvent::keyPressed
			sec
		:
		rts
.endproc

.export handleMenu
.proc handleMenu
		loop:
			lda MOUSEEVENT+MouseEvent::buttons
			bmi :+
				jmp handleMouseEvent
			:
			lda KEYEVENT+KeyEvent::keyPressed
			beq :+
				jmp handleKeyEvent
			:
		jmp loop
.endproc

.proc handleKeyEvent
		lda KEYEVENT+KeyEvent::normalChar
		sta TMP2+0
		lda KEYEVENT+KeyEvent::specialChar
		sta TMP2+1
		lda CURRENT_CAMP_MENU+0
		sta TMP+0
		lda CURRENT_CAMP_MENU+1
		sta TMP+1
		loop:
			ldy #0
			lda (TMP),y
			sta INDJMP+0
			iny
			lda (TMP),y
			sta INDJMP+1
			ora INDJMP+0
			beq done

			ldy #6
			lda (TMP),y
			cmp TMP2+0
			beq :+
			iny
			lda (TMP),y
			cmp TMP2+1
			bne continue
			:

			ldy #8
			lda (TMP),y
			beq :+
				and KEYEVENT+KeyEvent::modifiers
				beq continue
			:

			lda #0
			sta KEYEVENT+KeyEvent::keyPressed

			iny
			lda (TMP),y
			ldx #$ff
			ldy #$ff

			; x = deltaX
			; y = deltaY
			; a = buttonIndex
			jsr indirectJsr_nobank
			sec
			jmp doneOK

			continue:
			clc
			lda TMP+0
			adc #10
			sta TMP+0
			bcc :+
				inc TMP+1
			:
		jmp loop	

done:	clc
doneOK:	lda #0
		sta KEYEVENT+KeyEvent::keyPressed
		rts	
.endproc

.proc handleMouseEvent
		lda MOUSEEVENT+MouseEvent::xpos+0
		sta TMP2+0
		lda MOUSEEVENT+MouseEvent::xpos+1
		lsr
		ror TMP2+0
		lsr
		ror TMP2+0
		lsr
		ror TMP2+0

		sec
		lda MOUSEEVENT+MouseEvent::ypos
		sbc #1
		lsr
		lsr
		lsr
		sec
		sbc #15
		sta TMP2+1

		lda CURRENT_CAMP_MENU+0
		sta TMP+0
		lda CURRENT_CAMP_MENU+1
		sta TMP+1
		loop:
			ldy #0
			lda (TMP),y
			sta INDJMP+0
			iny
			lda (TMP),y
			sta INDJMP+1
			iny
			ora INDJMP+0
			beq done

			; x0
			lda (TMP),y
			sta TMP3+0
			iny
			cmp TMP2+0
			beq :+
				bcs continue
			:
			; y0
			lda (TMP),y
			sta TMP3+1
			iny
			cmp TMP2+1
			beq :+
				bcs continue
			:
			; x1
			lda (TMP),y
			iny
			cmp TMP2+0
			beq continue
			bcc continue
			; y1
			lda (TMP),y
			iny
			cmp TMP2+1
			beq continue
			bcc continue

			iny
			iny
			iny

			lda (TMP),y
			pha
			sec
			lda TMP2+0
			sbc TMP3+0
			tax
			sec
			lda TMP2+1
			sbc TMP3+1
			tay
			lda #$ff
			sta MOUSEEVENT+MouseEvent::buttons	
			pla

			; x = deltaX
			; y = deltaY
			; a = buttonIndex
			jsr indirectJsr_nobank
			jmp done

			continue:
			clc
			lda TMP+0
			adc #10
			sta TMP+0
			bcc :+
				inc TMP+1
			:
		jmp loop	

done:		lda #$ff
		sta MOUSEEVENT+MouseEvent::buttons
		rts	
.endproc

; c=1 draw border, c=0 do not draw border
.proc drawButtons
		php
		loop:
			ldy #0
			lda (TMP),y
			bne :+
				plp
				rts
			:

			pha
			ldy #4
			lda (TMP),y
			sta ARGS+0
			iny
			lda (TMP),y
			sta ARGS+1
			ldy #1
			lax (TMP),y
			iny
			lda (TMP),y
			tay
			pla
			
			plp
			jsr drawButton
			php

			clc
			ldy #1
			lda (TMP),y
			ldy #3
			adc (TMP),y
			bcs :+
				tay
				lda #7
				sta (TEXT2_D800),y
			:

			clc
			lda TMP+0
			adc #6
			sta TMP+0
			bcc :+
				inc TMP+1
			:
		jmp loop
.endproc

; TMP = menuPtr
.proc drawMenu
		jsrf clearTextScreen
		ldx #$18
		jsrf clearTextScreenColors
		lda #1
		sta textColor2
		sec
		jsr drawButtons

		lda #6
		sta textColor2
		iny
		lax (TMP),y
		ldy #0
		jsr f_text_setPosition

		clc
		lda TMP+0
		adc #2
		tax
		lda TMP+1
		adc #0
		tay
		clc
		jsr text_writeNullString2

		rts
.endproc

; x xpos
; y ypos
; a width
; c=1 draw frame, c=0 do not draw frame
.proc drawButton
		php
		message = ARGS+0
		sta TMP2

		bcc :+
			txa
			pha
			tya
			pha

			lda ARGS+0
			pha

			lda TMP2
			sta ARGS+0
			jsrf text_drawButton

			pla
			sta ARGS+0

			pla
			tay
			pla
			tax
		:

		iny
		inx
		jsr f_text_setPosition

		ldx message+0
		ldy message+1
		clc
		jsr text_writeNullString2

		plp
		rts
.endproc

.proc emitModalBox
		txa
		pha
		tya
		pha
		ldx #0
		ldy #1
		lda #38
		sta ARGS+0
		lda #7
		sta ARGS+1
		bcs :+
			jsrf text_drawBox
			jmp :++
		:
			jsrf text_drawBoxPressed
		:

		ldx #2
		ldy #3
		jsr f_text_setPosition
		lda #1
		sta textColor2
		pla
		tay
		pla
		tax
		clc
		jsr text_writeNullString2
		rts
.endproc

.proc showModalBox
		clc
		jsr emitModalBox
		jsrf waitAnyKey
		rts
.endproc

.proc showModalConfirmBox
		.pushseg
		.segment "BSS"
			callback: .res 2
		.popseg

		lda ARGS+0
		sta callback+0
		lda ARGS+1
		sta callback+1

		sec
		jsr emitModalBox

		lda #<buttons
		sta TMP+0
		lda #>buttons
		sta TMP+1
		sec
		jsr drawButtons

		lda #<cr
		sta CURRENT_CAMP_MENU+0
		lda #>cr
		sta CURRENT_CAMP_MENU+1

		rts

buttons:button  1, 6, 5, 2, yesTxt
		button 33, 6, 4, 2, noTxt
		.byte 0

select:		pha
		lda callback+0
		sta INDJMP+0
		lda callback+1
		sta INDJMP+1
		pla
		jmp (INDJMP)

cr:		clickregion  1,6,7,3, select, $19,$ff,$00, 1
		clickregion 33,6,6,3, select, $0e,$ff,$00, 0
		.word 0

yesTxt:		.byte " Yes",0
noTxt:		.byte " No",0
.endproc

.proc drawSelectedCharName
		lda #6
		sta textColor2

		inc textCol2
		lda #'('
		jsr putCharWrapper
		
		clc
		lda #PartyMember::name
		adc CURRENT_PARTYMEMBER+0
		tax
		lda #0
		adc CURRENT_PARTYMEMBER+1
		tay
		clc
		jsr text_writeNullString2

		lda #')'
		jsr putCharWrapper
		rts
.endproc

.proc putCharWrapper
		sta CUR_CHAR
		txa
		pha
		tya
		pha
		jsrf putChar2
		pla
		tay
		pla
		tax
		rts
.endproc

.proc getMemberStatus
		txa
		pha
		jsrf gameState_getMemberStatus
		pla
		tax
		rts
.endproc

.proc printMemberName
		clc
		lda CURRENT_PARTYMEMBER+0
		adc #<PartyMember::name
		tax
		lda CURRENT_PARTYMEMBER+1
		adc #>PartyMember::name
		tay
		jmp text_writeNullString3
.endproc

bits:	.byte $01,$02,$04,$08,$10,$20,$40,$80
		.byte $01,$02,$04,$08,$10,$20,$40,$80
		.byte $01,$02,$04,$08,$10,$20,$40,$80
		.byte $01,$02,$04,$08,$10,$20,$40,$80

