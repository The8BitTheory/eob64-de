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
str:	.byte "<-EXIT        Bestiarium          NEXT->"
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
.byte "Auch als Auge des Tyrannen oder Kugel der vielen Augen bekannt, ist dieses einsame Schreckenswesen oft unterirdisch anzutreffen. Beholder haben einen kugelf",$7e,"rmigen K",$7e,"rper und schweben aufgrund angeborener Levitation. Auf ihrem sph",$7d,"rischen K",$7e,"rper sitzen zehn Augenstiele, in der Mitte befindet sich ein gro",$7c,"es Auge, umrahmt von einem Maul mit mehreren Reihen scharfer Z",$7d,"hne. Jeder Augenstiel besitzt eine einzigartige magische F",$7d,"higkeit - der Beholder kann mit jedem Stiel einen anderen Zauber wirken. Zum Gl",$7f,"ck k",$7e,"nnen nicht alle Augen gleichzeitig auf ein Ziel gerichtet werden. Dank ihrer harten, chitinhaltigen Panzerung sind Beholder im Kampf schwer zu treffen.",0

name_disbeast:
.byte "Displacer Beast",0
info_disbeast:
.byte "Das Displacer Beast ",$7d,"hnelte einem blauschwarzen Puma mit kr",$7d,"ftigen Tentakeln von den Schultern. Die Tentakel endeten in scharfen hornartigen Spitzen, die sogar Stahlr",$7f,"stungen durchdringen konnten. Sie waren 8 bis 12 Fu",$7c," lang und wogen ",$7f,"ber 500 Pfund. Sein gef",$7d,"hrlichstes Merkmal war die ",$22,"F",$7d,"higkeit",$22," sein Abbild bis zu drei Fu",$7c," von seiner tats",$7d,"chlichen Position zu verschieben, was es im Kampf ",$7d,"u",$7c,"erst schwer zu treffen machte.",0

name_drider:
.byte "Drider",0
info_drider:
.byte "Diese merkw",$7f,"rdige Kreatur hat den Kopf und Oberk",$7e,"rper eines Dunkelelfen und den unteren K",$7e,"rper einer riesigen Spinne. Begabte Dunkelelfen, die das sechste Level erreichen, werden von der dunklen G",$7e,"ttin der Drow einem Test unterzogen. Wer besteht, wird speziellen Aufgaben zugewiesen. Diejenigen, die versagen, verwandeln sich in Driders und werden aus der Drow-Gesellschaft versto",$7c,"en. Driders k",$7e,"nnen in der Regel zaubern und mit Waffen k",$7d,"mpfen.",0

name_drowelf:
.byte "Drow",0
info_drowelf:
.byte "In ihrer langen Geschichte waren die Drow einst Teil der Elfen-Gemeinschaft, doch etwas wandelte sie ins B",$7e,"se und trieb sie aus dem Sonnenlicht in unterirdische H",$7e,"hlen und St",$7d,"dte. Drow sind kleiner und zierlicher als Menschen, mit schwarzer Haut und meist wei",$7c,"em Haar. Alle Drow k",$7e,"nnen Magie wirken und tragen oft Schwerter aus Adamitlegierung.",0

name_dwarf:
.byte "Zwerg",0
info_dwarf:
.byte "Diese gedrungenen Halbmenschen sind zwischen 4 und 4 1/2 Fu",$7c," gro",$7c," und wiegen zwischen 130 und 170 Pfund. Zwerge sind ",$7d,"u",$7c,"erst robuste Krieger und widerstandsf",$7d,"hig gegen",$7f,"ber sowohl giftigen als auch magischen Angriffen.",0

name_flind:
.byte "Flind",0
info_flind:
.byte "Diese humanoiden Kreaturen ",$7d,"hneln vage einem stark muskul",$7e,"sen Menschen mit einem hunde",$7d,"hnlichen Kopf. Flind besitzen eine durchschnittliche Intelligenz und k",$7e,"nnen sowohl ",$7f,"berirdisch als auch in H",$7e,"hlen gefunden werden.",0

name_golem:
.byte "Golem",0
info_golem:
.byte "Ein Golem ist eine k",$7f,"nstliche Kreatur, die durch einen elementaren Geist zum Leben erweckt wird. Dazu wird ein K",$7e,"rper aus Stein, Eisen, Ton oder Fleisch erschaffen. Der Sch",$7e,"pfer bindet den Geist an den K",$7e,"rper und unterwirft seinen Willen in einem geheimen Ritual das nur wenige m",$7d,"chtige Zauberer kennen. Golems sind furchtlos und k",$7d,"mpfen entschlossen.",0

name_hellhnd:
.byte "Hell Hound",0
info_hellhnd:
.byte "Der Hell Hound ist ein sehr gro",$7c,"es, rostrot oder braun gef",$7d,"rbtes Tier mit brennend roten Augen. Die Markierungen, Z",$7d,"hne und die Zunge des Tieres sind ru",$7c,"schwarz. Das Bellen eines H",$7e,"llenhunds wurde als ",$22,"unheimlich",$22,", ",$22,"hohl",$22,", und ",$22,"verst",$7e,"rend",$22," beschrieben. Das Tier greift mit Flammenatem und durchdringenden Z",$7d,"hnen an.",0

name_kenku:
.byte "Kenku",0
info_kenku:
.byte "Diese Wesen sehen aus wie menschen",$7d,"hnliche Falken mit Armen und Fl",$7f,"geln. Sie sind zwischen f",$7f,"nf und sieben Fu",$7c," gro",$7c,", haben haupts",$7d,"chlich braune Federn mit wei",$7c,"en Unterfedern und leuchtend gelbe Augen. Kenku besitzen nat",$7f,"rliche Diebesf",$7d,"higkeiten und am",$7f,"sieren sich daran, menschliche und halbmenschliche Rassen zu ",$7d,"rgern und zu st",$7e,"ren.",0

name_kobold:
.byte "Kobold",0
info_kobold:
.byte "Kobolde sind bekannt f",$7f,"r ihre Feigheit und sadistischen Neigungen. Sie sind meist nur in gro",$7c,"en Gruppen gef",$7d,"hrlich. Etwa drei Fu",$7c," gro",$7c,", mit dunkler, leicht schuppiger Haut, die nach einen",$22,"Mix aus nassem Hund und Sumpf riecht",$22,". Sie hassen Gnome und attackieren sie sofort. Kobolde bevorzugen Massenangriffe oder Distanzk",$7d,"mpfe.",0

name_kuotoa:
.byte "Kuo-Toa",0
info_kuotoa:
.byte "Eine uralte Rasse von Fischmenschen, die nun in unterirdischen H",$7e,"hlen leben. Kuo-toa hegen einen tiefen Hass gegen",$7f,"ber Oberfl",$7d,"chenbewohnern. Ihr K",$7e,"rper ",$7d,"hnelt grob dem eines Menschen, gekr",$7e,"nt von einem fisch",$7d,"hnlichen Kopf mit weitem Mund. Sie haben kurze Beine, lange dreifingerige H",$7d,"nde und tragen lediglich einen Lederharnisch f",$7f,"r Waffen und wenige Gegenst",$7d,"nde.",0

name_leech:
.byte "Riesen-Blutegel",0
info_leech:
.byte "Ein schmieriger, schneckengleicher Parasit, der sich von den K",$7e,"rperfl",$7f,"ssigkeiten seiner Opfer ern",$7d,"hrt. Blutegel lauern im Schlamm, Schlamm und Schleim von S",$7f,"mpfen und Abwasserkan",$7d,"len auf nichtsahnende Beute. Der Riesen-Blutegel erhebt sich aus dem Schlamm, wenn er die Anwesenheit eines lebenden Wesens bemerkt, und versucht, seine Beute zu ",$7f,"berw",$7d,"ltigen.",0

name_mantis:
.byte "Mantis Krieger",0
info_mantis:
.byte "Auch Thri-Kreen genannt, ist dies ein fleischfressender Insektenmensch. Ein Mantis-Krieger ",$7d,"hnelt einer humanoiden Gottesanbeterin in mannsgro",$7c,"er Form. Sie tragen oft eine eigenartige, b",$7e,"sartig geklingelte Stangenwaffe. Der Name dieser Waffe wurde nie ins Gemeine ",$7f,"bersetzt, aber der gef",$7f,"rchtete Ruf der t",$7e,"dlichen Waffe und der Krieger, die sie f",$7f,"hren, hat sich weit verbreitet.",0

name_mflayer:
.byte "Gehirnfresser",0
info_mflayer:
.byte "Auch Illithid genannt, sind sie unterirdische Bewohner und ",$7d,"u",$7c,"erst gef",$7f,"rchtet. Gehirnfresser ern",$7d,"hren sich von den Gedanken bewusster Wesen. Sie haben mauvefarbene Haut, sind etwa sechs Fu",$7c," gro",$7c," und tragen Gew",$7d,"nder mit Bildern von Leid und Verzweiflung. Ihre Gesichter ",$7d,"hneln Tintenfischen mit gro",$7c,"en, wei",$7c,"en, pupillenlosen Augen.",0

name_rust:
.byte "Rostmonster",0
info_rust:
.byte "Diese unterirdische Kreatur hat Appetit auf Metalle aller Art. Rostmonster sind etwa f",$7f,"nf Fu",$7c," lang, mit einem gepanzerten Schwanz und zwei greifbaren F",$7f,"hlern. Normalerweise sind sie friedlich, bis sie den Geruch von Metall wahrnehmen. Dann st",$7f,"rmen sie los und korrodieren das Metall mit ihren F",$7f,"hlern, um sich zu ",$22,"ern",$7d,"hren",$22," - selbst magische Waffen sind vor ihnen nicht sicher.",0

name_skeleton:
.byte "Skelett",0
info_skeleton:
.byte "Ein magisch belebter K",$7e,"rper, kontrolliert von b",$7e,"sen Zauberern oder Priestern. Skelette sind niedere Untote und mangelnder Intelligenz. Sie sind immun gegen Schlaf, Verzauberung und Festhalten. Skelette sind furchtlos und weichen niemals im Kampf zur",$7f,"ck.",0

name_spider:
.byte "Riesenspinne",0
info_spider:
.byte "Dieser Raubtier durchstreift viele Regionen und hilft dabei, den Bestand an Sch",$7d,"dlingen wie Kobolden und Abenteurern zu reduzieren. Riesenspinnen weben Netze f",$7f,"r ahnungslose Opfer und greifen mit giftigen Bissen an.",0

name_xorn:
.byte "Xorn",0
info_xorn:
.byte "Diese Kreatur stammt aus der Elementarebene der Erde und ern",$7d,"hrt sich von wertvollem Metall unter der Erde. Ein Xorn hat einen breiten, kieselartigen K",$7e,"rper mit einem Mund oben auf dem Kopf. Drei klauengekr",$7e,"nte Arme sind symmetrisch um den K",$7e,"rper angeordnet. Xorn scheinen von Edelsteinen oder wertvollem Metall angezogen zu werden, eher aus Interesse als aus Boshaftigkeit, wie in wenigen berichteten Angriffsf",$7d,"llen.",0

name_zombie:
.byte "Zombie",0
info_zombie:
.byte "Wie Skelette sind Zombies untote Kreaturen, belebt und kontrolliert von m",$7d,"chtigen Zauberern oder Priestern. Die Belebung ver",$7d,"ndert ihren verwesenden Zustand nicht, weshalb sie oft verfallen aussehen, mit fehlender Haut, Haar und sogar Gliedma",$7c,"en. Zombies sind langsam und unbeholfen, k",$7d,"mpfen jedoch entschlossen.",0
