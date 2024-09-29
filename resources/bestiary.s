.include "global.inc"

.segment "GAMESTATEM"
encounteredMonsters: .res 19

.segment "BESTIARY"

renderTextScreen = vic_bitmap+15*320

.export bestiaryGo
.proc bestiaryGo
		lda #POINTERENABLED_REAL
		sta pointerEnabled
		lda #$ff
		sta MOUSEEVENT+MouseEvent::buttons
		lda #$00
		sta KEYEVENT+KeyEvent::keyPressed

		lda timersEnabled
		pha
		lda #0
		sta timersEnabled
		sta screenEnabled
		lda #1
		jsr wait
		ldx #SPRITE_COL1
		stx $d027
		stx $d029
		stx $d02a
		stx $d02b
		stx $d02c
		stx $d02d
		stx $d02e
		stx $d02f

		memset vic_bitmap, $ff, (15*320)
		memset $d800, $6, (15*40)

		ldy #0
		jsrf clearSpritesWithData
		jsr printHeader

		lda #1
		jsr wait

		ldx #SCREEN_MODE_text_header
		jsrf setGameScreenMode
		lda #$ff
		sta screenEnabled

		jsr setMonster

		jsrf processKeyEvents

		jsrf installDrawMonsterHelper

		lda #1
		jsr wait
		lda #0
		sta screenEnabled

		jsrf clearSprites

		lda #1
		ldx #20
		:
			sta affectedSpriteColumns,x
			dex
		bpl :-
		jsrf guiInit

		lda #1
		jsr wait
		sec
		ldx #<screenModeGameNormal
		ldy #>screenModeGameNormal
		jsr setScreenMode
		jsrf reinstallFont
		jsrf clearSprites

		inc SHOULDRENDER
		jsrf render

		lda #$ff
		sta screenEnabled

		pla
		sta timersEnabled

		lda #0
		sta KEYEVENT+KeyEvent::keyPressed
		sta KEYEVENT+KeyEvent::normalChar
		sta KEYEVENT+KeyEvent::specialChar
		sta KEYEVENT+KeyEvent::modifiers
		lda #$ff
		sta MOUSEEVENT+MouseEvent::buttons

		rts
.endproc

.proc printHeader
		ldx #39
		:
			lda str,x
			sta scratchArea,x
			dex
		bpl :-
		jsrf invertButtons
		ldx #39
		:
			lda scratchArea,x
			sta vic_screen2,x
			lda d800,x
			sta $d800,x
			dex
		bpl :-
		rts

;              0123456789012345678901234567890123456789
str:	.byte "<-EXIT          Bestiary          NEXT->"
d800:	.res 16,$0
		.res  8,$7
		.res 16,$0
.endproc

.proc setMonster
		jsr renderBestiaryInfo
		jmp animateMonster
.endproc

.proc animateMonster
		ldy bestiaryIndex
		lda bestiary_to_monster_mapping,y
		and #1
		sta monsterPicture
		lda bestiary_to_monster_mapping,y
		lsr
		tax
		jsrf installDrawMonsterHelperDirect


; c=0:normal
; c=1:flipped
; x=picture 0/1
; DRAW_MONSTER_HIT
; xpos = ARGS+0
; ypos = ARGS+1
; zoom = ARGS+2 ;0..2
; state= ARGS+3 ;0..5

		jsrf clearFrameBuffer
		memset frameBuffer_bitmap, $ff, (22*15*8)
		memset frameBuffer_d800, $6, (22*15)
		lda #0
		.repeat 7,I
			sta frameBuffer_d800+I*15
		.endrep
		lda #7
		.repeat 22-7,I
			sta frameBuffer_d800+(I+7)*15
		.endrep

		lda #(88+4)/8
		sta ARGS+0
		lda #(88)/8+3
		sta ARGS+1
		lda #0
		sta ARGS+2
		sta DRAW_MONSTER_HIT
		ldx animFrame
		lda sequence_state,x
		sta ARGS+3
		lda sequence_carry,x
		cmp #1
		ldx monsterPicture
		jsrf render_bestiary

		ldx animFrame
		inx
		txa
		and #3
		sta animFrame

		rts
sequence_carry: .byte 1,0,1,1
sequence_state: .byte 0,1,3,1

.pushseg
.segment "MEMCODE_RW"
animFrame: .byte 0
monsterPicture: .byte 0
.popseg
.endproc

.pushseg
.segment "MEMCODE_RW"
bestiaryIndex: .byte 0
lineIndex: .byte 0
lines: .byte 0
.popseg

.proc renderBestiaryInfo
		memset renderTextScreen, ' ', 20*40

		lda #0
		sta lines
		sta lineIndex

		lda #<renderTextScreen
		sta TEXT2_SCREEN+0
		lda #>renderTextScreen
		sta TEXT2_SCREEN+1

		ldx bestiaryIndex
		lda info_lo,x
		sta SRC+0
		lda info_hi,x
		sta SRC+1

		ldy #1
		sty textCol2
		nextWord:
			ldy #0

			; Scan to the next white space
			:
				lda (SRC),y
				iny
				cmp #33
			bcs :-
			sty COUNT

			; Does the word fit on the line?
			clc
			tya
			adc textCol2
			cmp #40+1
			bcc :++
				; No, line break
				clc
				lda TEXT2_SCREEN+0
				adc #40
				sta TEXT2_SCREEN+0
				bcc :+
					inc TEXT2_SCREEN+1
				:
				lda #1
				sta textCol2
				inc lines
			:

			; Print word
			ldx #0
			:
				txa
				tay
				lda (SRC),y
				beq done
				ldy textCol2
				sta (TEXT2_SCREEN),y
				inc textCol2
				inx
				cpx COUNT
			bne :-

			; Skip past the current word
			clc
			lda SRC+0
			adc COUNT
			sta SRC+0
			bcc :+
				inc SRC+1
			:
		jmp nextWord
		done:

		rts
.endproc

.proc showBestiaryInfo
		ldx #1
		ldy #0
		jsrf text_setPosition
		lda #6
		sta textColor2
		ldx bestiaryIndex
		lda name_lo,x
		ldy name_hi,x
		tax
		clc
		jsr text_writeNullString2

		ldx textCol2
		lda #' '
		:
			sta vic_screen2+15*40,x
			inx
			cpx #40
		bne :-

		ldx lineIndex
		lda lineSource_lo,x
		sta MSRC+0
		lda lineSource_hi,x
		sta MSRC+1
		lda #<(vic_screen2+16*40)
		sta MDST+0
		lda #>(vic_screen2+16*40)
		sta MDST+1
		lda #<(8*40)
		ldx #>(8*40)
		jsr _memcpy_pureram
		rts

lineSource_lo:
		.repeat 10,I
			.byte <(renderTextScreen+I*40)
		.endrep
lineSource_hi:
		.repeat 10,I
			.byte >(renderTextScreen+I*40)
		.endrep
.endproc

.macro DEFKEY key,key2,callback,value
	.byte key, key2, <callback, >callback, value
.endmacro

.segment "BESTIARY2"
.proc processKeyEvents
		lda #0
		sta KEYEVENT+KeyEvent::keyPressed
		sta KEYEVENT+KeyEvent::normalChar
		sta KEYEVENT+KeyEvent::specialChar
		sta KEYEVENT+KeyEvent::modifiers
		lda #$ff
		sta MOUSEEVENT+MouseEvent::buttons

		jsrf showBestiaryInfo
		:
			lda #10
			jsr waitWithAbort
			bne :+
			jsrf animateMonster
		jmp :-
		:

		lda MOUSEEVENT+MouseEvent::buttons
		bmi :+
			jsr mouseEvent
			bne :+
				bcc escape
				jmp right
		:
		lda KEYEVENT+KeyEvent::keyPressed
		beq processKeyEvents

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
        	match:
			lda keyboardActionList+2,x
                sta INDJMP+0
                lda keyboardActionList+3,x
    	        sta INDJMP+1
            	lda keyboardActionList+4,x
                tax
                jmp (INDJMP)
	        noMatch:
		        inx
	       		inx
	       		inx
	        	inx
	        	inx
	        	lda keyboardActionList+0,x
	        	ora keyboardActionList+1,x
	  	bne nextKey

	  	jmp processKeyEvents

escape:
		rts

; Returns A=!0 if no event, otherwise 0. C=0 indicate left, C=1 indicate right 
mouseEvent:
		; First row?
		lda MOUSEEVENT+MouseEvent::ypos
		lsr
		lsr
		lsr
		beq :+
			rts
		:

		; TMP = xpos/8
		lda MOUSEEVENT+MouseEvent::xpos+0
		sta TMP+0
		lda MOUSEEVENT+MouseEvent::xpos+1
		lsr
		ror TMP+0
		lsr
		ror TMP+0
		lsr
		ror TMP+0
		lda TMP+0
		cmp #7
		bcs :+
			lda #0
			; Left
			rts
		:
		cmp #33
		bcc :+
			; Right
			lda #0
			rts
		:
			lda #1
			rts

cursorUpDown:
		lda KEYEVENT+KeyEvent::modifiers
		and #$50
		beq down
		up:
			lda lineIndex
			beq :+
				dec lineIndex
			:
			jmp processKeyEvents
		down:
			sec
			lda lines
			sbc #8
			cmp lineIndex
			bmi :+
				inc lineIndex
			:
			jmp processKeyEvents

cursorLeftRight:
		lda KEYEVENT+KeyEvent::modifiers
		and #$50
		beq right
		left:
			lda bestiaryIndex
			beq :+
				dec bestiaryIndex
				jsrf setMonster
			:
			jmp processKeyEvents
		right:
			lda bestiaryIndex
			cmp #18
			beq :+
				inc bestiaryIndex
				jsrf setMonster
			:
			jmp processKeyEvents

keyboardActionList:
	   DEFKEY $1f, 0, escape, 0
       DEFKEY $02, 0, escape, 0
       DEFKEY 0, $80, cursorUpDown, 0
       DEFKEY 0, $04, cursorLeftRight, 0
       .word 0
.endproc


.segment "BESTIARY"

;.byte >mon_kobold,	>mon_leech
;.byte >mon_zombie, >mon_skeleton
;.byte >mon_kuotoa, >mon_flind
;.byte >mon_spider, >mon_spider
;.byte >mon_dwarf, 	>mon_spider
;.byte >mon_kenku, 	>mon_mage
;.byte >mon_drowelf,>mon_skelwar
;.byte >mon_drider, >mon_hellhnd
;.byte >mon_rust, 	>mon_disbeast
;.byte >mon_shindia,>mon_mantis
;.byte >mon_xorn, 	>mon_mflayer
;.byte >mon_xanath, >mon_golem

bestiary_to_monster_mapping:
		.byte 0,1
		.byte 2,3
		.byte 4,5
		.byte 6
		.byte 8
		.byte 10
		.byte 12
		.byte 14,15
		.byte 16,17
		.byte 19
		.byte 20
		.byte 21
		.byte 22
		.byte 23

; $ff denotes monster-NPC
monster_to_bestiary_mapping:
		.byte 0,1
		.byte 2,3
		.byte 4,5
		.byte 6,6
		.byte 7,6
		.byte 8,$ff
		.byte 9,6
		.byte 10,11
		.byte 12,13
		.byte $ff,14
		.byte 15,16
		.byte 17,18

name_lo:
		.byte <name_kobold
		.byte <name_leech
		.byte <name_zombie
		.byte <name_skeleton
		.byte <name_kuotoa
		.byte <name_flind
		.byte <name_spider
		.byte <name_dwarf
		.byte <name_kenku
		.byte <name_drowelf
		.byte <name_drider
		.byte <name_hellhnd
		.byte <name_rust
		.byte <name_disbeast
		.byte <name_mantis
		.byte <name_xorn
		.byte <name_mflayer
		.byte <name_xanath
		.byte <name_golem

name_hi:
		.byte >name_kobold
		.byte >name_leech
		.byte >name_zombie
		.byte >name_skeleton
		.byte >name_kuotoa
		.byte >name_flind
		.byte >name_spider
		.byte >name_dwarf
		.byte >name_kenku
		.byte >name_drowelf
		.byte >name_drider
		.byte >name_hellhnd
		.byte >name_rust
		.byte >name_disbeast
		.byte >name_mantis
		.byte >name_xorn
		.byte >name_mflayer
		.byte >name_xanath
		.byte >name_golem

info_lo:
		.byte <info_kobold
		.byte <info_leech
		.byte <info_zombie
		.byte <info_skeleton
		.byte <info_kuotoa
		.byte <info_flind
		.byte <info_spider
		.byte <info_dwarf
		.byte <info_kenku
		.byte <info_drowelf
		.byte <info_drider
		.byte <info_hellhnd
		.byte <info_rust
		.byte <info_disbeast
		.byte <info_mantis
		.byte <info_xorn
		.byte <info_mflayer
		.byte <info_xanath
		.byte <info_golem

info_hi:
		.byte >info_kobold
		.byte >info_leech
		.byte >info_zombie
		.byte >info_skeleton
		.byte >info_kuotoa
		.byte >info_flind
		.byte >info_spider
		.byte >info_dwarf
		.byte >info_kenku
		.byte >info_drowelf
		.byte >info_drider
		.byte >info_hellhnd
		.byte >info_rust
		.byte >info_disbeast
		.byte >info_mantis
		.byte >info_xorn
		.byte >info_mflayer
		.byte >info_xanath
		.byte >info_golem

name_xanath:
.byte "Beholder",0
info_xanath:
.byte "Also known as Eye Tyrant or Sphere of Many Eyes, this solitary horror is most often found underground. Beholder have a globular body and move with an innate levitation. Atop the beholder's spherical body are ten eye-stalks, and in the center is a single large eye and a gaping maw adorned with several rows of razor-sharp teeth. Each of the eye-stalks has a unique magical ability - the beholder can cast a different spell with each. Fortunately, not all of the eyes can be brought to bear on a given target. Beholder are covered with hard, chitinous armor, making them relatively tough to hit in combat.",0

name_disbeast:
.byte "Displacer Beast",0
info_disbeast:
.byte "The displacer beast resembled a blue-black puma with two powerful tentacles growing from its shoulders. The tentacles are tipped with sharp, horny protuberances that can punch through even steel armor. The beasts range in size from 8 to 12 feet in length and can reach upwards of 500 pounds. The beast's name described its most dangerous feature - an ability to ",$22,"displace",$22," its image up to three feet from its actual location. This makes the displacer beast very difficult to hit in combat.",0

name_drider:
.byte "Drider",0
info_drider:
.byte "This strange creature has the head and torso of a drow and the lower body of a giant spider. When drow of exceptional ability reach the sixth-level, they are subjected to a test by the drow's dark goddess. Those who pass the test are elevated to special services. Those who fail are transformed into driders and cast out of drow society. Driders can generally cast spells as well as fight with weapons.",0

name_drowelf:
.byte "Drow",0
info_drowelf:
.byte "In their long past history, the drow were part of the elf community that still roams the world's forests. Something turned these elves evil, and drove them from the sunlight into their present subterranean caves and cities. Drow are shorter and more lightly built than men. They have black skin and pale, usually white hair. All drow can cast some magic spells, and they often carry swords of adamite alloy.",0

name_dwarf:
.byte "Drawf",0
info_dwarf:
.byte "These stocky demi-humans stand between 4 and 4 1/2 feet tall and weigh between 130 and 170 pounds. Dwarves are very tough warriors, and are resistant to both venomous and magical attacks.",0

name_flind:
.byte "Flind",0
info_flind:
.byte "This humanoid creature vaguely resembles a heavily muscled human with a canine head. Flind are of average intelligence, and can be found above ground and in caverns.",0

name_golem:
.byte "Golem",0
info_golem:
.byte "A golem is an artificial creature animated by an elemental spirit. The process of creating a golem begins with the creation of a body made of stone, iron, clay, or even flesh. After the body has been assembled, the creator must undergo a lengthy ritual to bind the elemental to the body, and subordinate its will to the new master. The ritual is a closely guarded secret of a handful of powerful wizards. Golems are utterly fearless, and battle with single-minded determination.",0

name_hellhnd:
.byte "Hell Hound",0
info_hellhnd:
.byte "The hell hound is a very large, rust-red or brown beast with burning red eyes. The beast's markings, teeth, and tongue are soot-black. The baying of a hell hound has been described as ",$22,"eerie",$22,", ",$22,"hollow",$22,", and ",$22,"disturbing",$22,". The beast attacks with flaming breath and piercing teeth.",0

name_kenku:
.byte "Kenku",0
info_kenku:
.byte "These beings resembled humanoid hawks, with both arms and wings. Their height ranges from five to seven feet, their feathers are predominantly brown with white under-feathers, and their eyes are brilliant yellow. Kenku all have natural thieving abilities, and take perverse pleasure in annoying and inconveniencing human and demi-human races.",0

name_kobold:
.byte "Kobold",0
info_kobold:
.byte "Well known for cowardice and a wide sadistic streak, kobolds are usually only dangerous in large groups. Kobolds stand about three feet tall, and their dark rust hides are somewhat scaly. The smell of a kobold has been described as ",$22,"a cross between a wet dog and a swamp",$22,". They have an ancestral hatred for gnomes, and attack them on sight. Kobolds generally prefer to attach other opponents en masse, or from a distance.",0

name_kuotoa:
.byte "Kuo-Toa",0
info_kuotoa:
.byte "This in an ancient race of fish-men that now dwell in subterranean caverns. The kuo-toa harbor an age-old hatred for surface dwellers and their sunlit world. These creatures have a vaguely human body, topped with a wide-mouthed fish head. They have short legs and long, three-fingered hands. Kuo-toa wear no clothing, only a leather harness for their weapons and few belongings.",0

name_leech:
.byte "Leech, Giant",0
info_leech:
.byte "This is a slimy, slug-like parasite that feeds on its victim's bodily fluids. Leeches wait in the mud, sludge, and slime of swamps and sewers for unsuspecting prey. The giant leech rears up from the ooze when it detects the presence of a living creature, and attempts to subdue its prey.",0

name_mantis:
.byte "Mantis Warrior",0
info_mantis:
.byte "Also called Thri-Kreen, this is a carnivorous insect man. A mantis warrior resembled a man-sized preying mantis. Mantis warriors are often armed with a peculiar viciously bladed polearm. The name for this weapon has never been translated into the Common tongue, but the ferocious reputation of the deadly thing, and the warriors who wield it, has spread far.",0

name_mflayer:
.byte "Mind Flayer",0
info_mflayer:
.byte "Also called Illithid, this is one of the most feared of the subterranean dwellers. Mind flayers feed on the very minds of sentient beings. Mind flasyers are a slime mauve color, stand about six feet tall, and adorn themselves with flowing robes decorated with images of suffering, death, and despair. Their faces resembles octopi with two large, white, pupilless eyes.",0

name_rust:
.byte "Rust Monster",0
info_rust:
.byte "This is a subterranean creature with an appetite for metals of all kinds. Rust monsters are about five feet in length, and have a long, armored tail and two prehensile antennae. The tail is tipped with a strange paddle-like appendage. The creature is normally placid and inoffensive - until it smells metal. Whenever the creature detects the scent of food (armor, weapons, and the like) it charges. Whenever the monster hits metal with its antennae, the metal corrodes and ",$22,"feeds",$22," the creature. Even magical weapons can be susceptible to the rust monster's attack.",0

name_skeleton:
.byte "Skeleton",0
info_skeleton:
.byte "This is a magically animated body, created and controlled by a powerful evil wizard or priest. Skeletons are one of the lesser undead monsters, though powerful skeletons have been created from the bodies of powerful warriors. Because they have no intelligence or will, skeletons are immune to spells such as Sleep, Charm, and Hold. They are also utterly fearless, and never retreat from a fight.",0

name_spider:
.byte "Spider, Giant",0
info_spider:
.byte "This predator haunts many regions and helps to keep down the level of pests such as kobolds and adventurers. Giant spiders weave webs for unwary victims, and attack with venomous bites.",0

name_xorn:
.byte "Xorn",0
info_xorn:
.byte "This creature is a native to the elemental plane of Earth, and feeds on previous metal deep underground. The wide body of a xorn is made of a pebbly material, and its mouth is located at the top of its head. Three talon-clawed arms are positioned symmetrically around its body. In the few reported cases of xorn attacks, the creatures seem to have been attracted by jewels or precious metals, rather than malice.",0

name_zombie:
.byte "Zombie",0
info_zombie:
.byte "Like skeletons, zombies are undead creatures that are animated and controlled by powerful wizards or priests. The animation ritual does not alter the conditions of the zombie's body, so most are in sever states of decay, often missing patches of skin and hair, and occasionally whole limbs. Zombies are very slow and clumsy, but they fight with determination and persistence.",0
