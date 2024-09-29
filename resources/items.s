.include "global.inc"

.segment "BSS"
.export itemType
itemType: .res .sizeof(ItemType)

.segment "GAMESTATEM"
.define UNUSED_AC $e2
custom_itemType_inventoryBits_lo: .res 6
custom_itemType_inventoryBits_hi: .res 6
custom_itemType_armourClassModifier: .res 6
custom_itemType_classBits: .res 6
custom_itemType_doubleHanded: .res 6
custom_itemType_damageVsSmall_rolls: .res 6
custom_itemType_damageVsSmall_sides: .res 6
custom_itemType_damageVsSmall_base: .res 6
custom_itemType_damageVsBig_rolls: .res 6
custom_itemType_damageVsBig_sides: .res 6
custom_itemType_damageVsBig_base: .res 6
custom_itemType_usage: .res 6

.segment "MEMCODE_RO"
; y = type
.export items_fetchItemType
.proc items_fetchItemType
	jsrf fetchItemType
	rts
.endproc

.export lax_ARGS0_y
.proc lax_ARGS0_y
	dec 1
	lax (ARGS+0),y
	php
	inc 1
	plp
	rts
.endproc

.export lda_TMP_y
.proc lda_TMP_y
	dec 1
	dec 1
	lda (TMP),y
	php
	inc 1
	inc 1
	plp
	rts
.endproc

.export lda_RIGHT_ITEM_y
.proc lda_RIGHT_ITEM_y
	dec 1
	lda (RIGHT_ITEM),y
	php
	inc 1
	plp
	rts
.endproc

.export lda_MAY_ITEM_y
.proc lda_MAY_ITEM_y
	dec 1
	lda (MAY_ITEM),y
	php
	inc 1
	plp
	rts
.endproc

.export sta_TMP_y
.proc sta_TMP_y
	dec 1
	sta (TMP),y
	php
	inc 1
	plp
	rts
.endproc

.export lda_POINTER_ITEM_y
.proc lda_POINTER_ITEM_y
	dec 1
	lda (POINTER_ITEM),y
	php
	inc 1
	plp
	rts
.endproc

.export lax_CUR_ITEM_y
.proc lax_CUR_ITEM_y
	dec 1
	lax (CUR_ITEM),y
	php
	inc 1
	plp
	rts
.endproc

.export lda_CUR_ITEM_y
.proc lda_CUR_ITEM_y
	dec 1
	lda (CUR_ITEM),y
	php
	inc 1
	plp
	rts
.endproc

.export sta_CUR_ITEM_y
.proc sta_CUR_ITEM_y
	php
	dec 1
	sta (CUR_ITEM),y
	inc 1
	plp
	rts
.endproc

.export lda_INV_ITEM_y
.proc lda_INV_ITEM_y
	dec 1
	lda (INV_ITEM),y
	php
	inc 1
	plp
	rts
.endproc

.export lda_TOP_ITEM_y
.proc lda_TOP_ITEM_y
	dec 1
	lda (TOP_ITEM),y
	php
	inc 1
	plp
	rts
.endproc

.segment "ITEMFUNCS"
.export initGameState
.proc initGameState
		memset __GAMESTATEM_RUN__, $00, __GAMESTATEM_SIZE__
		ldx #5
		lda #UNUSED_AC
		:
			sta custom_itemType_armourClassModifier,x
			dex
		bpl :-
		jsrf defaultTimers
		rts
.endproc

.export checkItemsBase
.proc checkItemsBase
	dec 1

	; Have item base moved?
	lda #<items
	cmp itemsBaseInSave+0
	bne :+
	lda #>items
	cmp itemsBaseInSave+1
	bne :+
		inc 1
		rts
	:

	; Calulate item base difference
	sec
	lda #<items
	sbc itemsBaseInSave+0
	sta TMP+0
	lda #>items
	sbc itemsBaseInSave+1
	sta TMP+1

	; Update to new items base
	lda #<items
	sta itemsBaseInSave+0
	lda #>items
	sta itemsBaseInSave+1

	; Relocate inventory items
	ldx #0
	memberLoop:
		lda partyMembers_lo,x
		sta CURRENT_PARTYMEMBER+0
		lda partyMembers_hi,x
		sta CURRENT_PARTYMEMBER+1
		ldy #PartyMember::inventory
		inventoryLoop:
			lda (CURRENT_PARTYMEMBER),y
			sta CUR_ITEM+0
			iny
			lda (CURRENT_PARTYMEMBER),y
			sta CUR_ITEM+1
			iny

			ora CUR_ITEM+0
			beq :+
				dey
				dey
				clc
				lda CUR_ITEM+0
				adc TMP+0
				sta (CURRENT_PARTYMEMBER),y
				iny
				lda CUR_ITEM+1
				adc TMP+1
				sta (CURRENT_PARTYMEMBER),y
				iny
			:
			cpy #PartyMember::inventory+27*2
		bne inventoryLoop

		inx
		cpx #6
	bne memberLoop

	; Relocate pointer item
	lda POINTER_ITEM+0
	sta CUR_ITEM+0
	lda POINTER_ITEM+1
	sta CUR_ITEM+1
	ora CUR_ITEM+0
	beq :+
		clc
		lda CUR_ITEM+0
		adc TMP+0
		sta POINTER_ITEM+0
		iny
		lda CUR_ITEM+1
		adc TMP+1
		sta POINTER_ITEM+1
	:

	; Relocate internal item pointers
	lda #<items
	sta CUR_ITEM+0
	lda #>items
	sta CUR_ITEM+1
	itemLoop:
		ldy #Item::previous
		jsr relocateItem
		ldy #Item::next
		jsr relocateItem

		clc
		lda CUR_ITEM+0
		adc #<.sizeof(Item)
		sta CUR_ITEM+0
		lda CUR_ITEM+1
		adc #>.sizeof(Item)
		sta CUR_ITEM+1

		cmp #>lastItem
		bne itemLoop
		lda CUR_ITEM+0
		cmp #<lastItem
	bne itemLoop

	inc 1
	rts

relocateItem:
	lda (CUR_ITEM),y
	sta TMP2+0
	iny
	lda (CUR_ITEM),y
	sta TMP2+1
	ora TMP2+0
	beq :+
		dey
		clc
		lda TMP2+0
		adc TMP+0
		sta (CUR_ITEM),y
		iny
		lda TMP2+1
		adc TMP+1
		sta (CUR_ITEM),y
	:
	rts
.endproc

; x: The new item type
.export items_createCustomItemType 
.proc items_createCustomItemType
		inventoryBits = ARGS+0
		handBits = ARGS+2
		armourClassModifier = ARGS+4
		classBits = ARGS+5
		damageDiceRolls = ARGS+6
		damageDiceSides = ARGS+7
		damageDiceBase = ARGS+8
		usage = ARGS+9

		ldx #0
		:
			lda custom_itemType_armourClassModifier,x
			cmp #UNUSED_AC
			beq :+
			inx
			cpx #6
		bne :-
		:

		lda inventoryBits+0
		sta custom_itemType_inventoryBits_lo,x
		lda inventoryBits+1
		sta custom_itemType_inventoryBits_hi,x
		lda armourClassModifier
		sta custom_itemType_armourClassModifier,x
		lda classBits
		sta custom_itemType_classBits,x
		lda damageDiceRolls
		sta custom_itemType_damageVsSmall_rolls,x
		sta custom_itemType_damageVsBig_rolls,x
		lda damageDiceSides
		sta custom_itemType_damageVsSmall_sides,x
		sta custom_itemType_damageVsBig_sides,x
		lda damageDiceBase
		sta custom_itemType_damageVsSmall_base,x
		sta custom_itemType_damageVsBig_base,x
		lda usage
		sta custom_itemType_usage,x

		txa
		ora #$80
		tax

		rts
.endproc

; CUR_ITEM
.export items_freeCustomItem
.proc items_freeCustomItem
		ldy #Item::position
		lda #<-2
		sta (CUR_ITEM),y
		iny
		lda #>-2
		sta (CUR_ITEM),y

		ldy #Item::level
		lda #<-1
		sta (CUR_ITEM),y

		ldy #Item::type
		dec 1
		lax (CUR_ITEM),y
		inc 1

		lda #UNUSED_AC
		sta custom_itemType_armourClassModifier,x
		rts
.endproc

.proc fetchItemType
	tya
	bmi :+
		lda itemType_inventoryBits_lo,y
		sta itemType+ItemType::inventoryBits+0
		lda itemType_inventoryBits_hi,y
		sta itemType+ItemType::inventoryBits+1
		lda itemType_armourClassModifier,y
		sta itemType+ItemType::armourClassModifier
		lda itemType_classBits,y
		sta itemType+ItemType::classBits
		lda itemType_doubleHanded,y
		sta itemType+ItemType::doubleHanded
		lda itemType_damageVsSmall_rolls,y
		sta itemType+ItemType::damageVsSmall+0
		lda itemType_damageVsSmall_sides,y
		sta itemType+ItemType::damageVsSmall+1
		lda itemType_damageVsSmall_base,y
		sta itemType+ItemType::damageVsSmall+2
		lda itemType_damageVsBig_rolls,y
		sta itemType+ItemType::damageVsBig+0
		lda itemType_damageVsBig_sides,y
		sta itemType+ItemType::damageVsBig+1
		lda itemType_damageVsBig_base,y
		sta itemType+ItemType::damageVsBig+2
		lda itemType_usage,y
		sta itemType+ItemType::usage
		rts
	:
	lda custom_itemType_inventoryBits_lo-$80,y
	sta itemType+ItemType::inventoryBits+0
	lda custom_itemType_inventoryBits_hi-$80,y
	sta itemType+ItemType::inventoryBits+1
	lda custom_itemType_armourClassModifier-$80,y
	sta itemType+ItemType::armourClassModifier
	lda custom_itemType_classBits-$80,y
	sta itemType+ItemType::classBits
	lda custom_itemType_doubleHanded-$80,y
	sta itemType+ItemType::doubleHanded
	lda custom_itemType_damageVsSmall_rolls-$80,y
	sta itemType+ItemType::damageVsSmall+0
	lda custom_itemType_damageVsSmall_sides-$80,y
	sta itemType+ItemType::damageVsSmall+1
	lda custom_itemType_damageVsSmall_base-$80,y
	sta itemType+ItemType::damageVsSmall+2
	lda custom_itemType_damageVsBig_rolls-$80,y
	sta itemType+ItemType::damageVsBig+0
	lda custom_itemType_damageVsBig_sides-$80,y
	sta itemType+ItemType::damageVsBig+1
	lda custom_itemType_damageVsBig_base-$80,y
	sta itemType+ItemType::damageVsBig+2
	lda custom_itemType_usage-$80,y
	sta itemType+ItemType::usage

	rts

itemType_inventoryBits_lo:
	.byte <$0008,<$0008,<$0008,<$0008,<$0088,<$0088,<$0088,<$0008,<$0008,<$0008,<$0008,<$0008,<$0008,<$0008,<$0008,<$0088,<$0089,<$0000,<$0088,<$000a,<$000a,<$0028,<$000a,<$0000,<$000a,<$000a,<$0000,<$0008,<$0088,<$0088,<$0088,<$0088
	.byte <$0018,<$0008,<$0088,<$0088,<$0088,<$0088,<$0088,<$0088,<$0088,<$000a,<$0108,<$000c,<$00c8,<$0000,<$00c8,<$0108,<$0088,<$0088,<$0088
itemType_inventoryBits_hi:
	.byte >$0008,>$0008,>$0008,>$0008,>$0088,>$0088,>$0088,>$0008,>$0008,>$0008,>$0008,>$0008,>$0008,>$0008,>$0008,>$0088,>$0089,>$0000,>$0088,>$000a,>$000a,>$0028,>$000a,>$0000,>$000a,>$000a,>$0000,>$0008,>$0088,>$0088,>$0088,>$0088
	.byte >$0018,>$0008,>$0088,>$0088,>$0088,>$0088,>$0088,>$0088,>$0088,>$000a,>$0108,>$000c,>$00c8,>$0000,>$00c8,>$0108,>$0088,>$0088,>$0088
itemType_armourClassModifier:
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$fa,$fb,$ff,$fe,$00,$f9,$fc,$00,$ff,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
itemType_classBits:
	.byte $09,$09,$09,$0f,$0b,$0b,$0f,$09,$0f,$09,$09,$0d,$0d,$0f,$0d,$0b,$09,$00,$0f,$05,$05,$0d,$0d,$00,$05,$05,$00,$0d,$08,$02,$05,$0f
	.byte $0f,$0f,$02,$04,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$00,$0f,$0f,$02,$0f,$0f
itemType_doubleHanded:
	.byte $01,$01,$00,$00,$00,$00,$00,$02,$00,$01,$02,$01,$01,$02,$02,$00,$00,$00,$00,$01,$01,$00,$01,$00,$01,$01,$00,$00,$00,$00,$00,$00
	.byte $00,$01,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00
itemType_damageVsSmall_rolls:
	.byte $01,$01,$01,$00,$01,$01,$00,$01,$00,$01,$01,$01,$01,$01,$01,$01,$01,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01
itemType_damageVsSmall_sides:
	.byte $08,$08,$06,$00,$03,$04,$00,$06,$00,$06,$0a,$06,$06,$06,$04,$03,$01,$00,$02,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$04
itemType_damageVsSmall_base:
	.byte $00,$00,$00,$00,$05,$00,$00,$00,$00,$00,$00,$01,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
itemType_damageVsBig_rolls:
	.byte $01,$01,$01,$00,$01,$01,$00,$01,$00,$01,$02,$01,$02,$01,$01,$01,$01,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01
itemType_damageVsBig_sides:
	.byte $0a,$0c,$08,$00,$03,$03,$00,$06,$00,$08,$06,$06,$04,$06,$04,$02,$01,$00,$02,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$04
itemType_damageVsBig_base:
	.byte $00,$00,$00,$00,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
itemType_usage:
	.byte $01,$81,$81,$04,$82,$82,$0e,$03,$05,$82,$81,$81,$81,$01,$03,$82,$02,$00,$02,$80,$80,$80,$00,$00,$80,$80,$00,$80,$84,$05,$06,$07
	.byte $00,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$00,$80,$80,$04,$00,$15,$90,$92,$04,$01
.endproc

; CURRENT_PARTYMEMBER
; Return full inventory offset in A & ARGS+0. 0 if not found
.export items_checkInventoryForItemByTypeAndValue
.proc items_checkInventoryForItemByTypeAndValue
	type = ARGS+0
	value = ARGS+1

	dec 1

	ldy #PartyMember::inventory
	loop:
		lda (CURRENT_PARTYMEMBER),y
		sta CUR_ITEM+0
		iny
		lda (CURRENT_PARTYMEMBER),y
		sta CUR_ITEM+1
		iny
		ora CUR_ITEM+0
		beq continue
			lda type
			bmi :+
				sty ARGS+2
				ldy #Item::type
				lda (CUR_ITEM),y
				ldy ARGS+2
				cmp type
				bne continue
			:

			lda value
			bmi found

			ldy #Item::value
			lda (CUR_ITEM),y
			ldy ARGS+2
			cmp value
			beq found
		continue:
		cpy #PartyMember::inventory+27*2
	bne loop
	ldy #2 ;Return 0
found:	dey
	dey
return:	sty ARGS+0
	inc 1
	rts
.endproc

.export items_deletePartyItems 
.proc items_deletePartyItems
	type = ARGS+0
	value = ARGS+1
	dec 1

	lda #0
	sta COUNT

	ldx #0
	nextMember:
		lda partyMembers_lo,x
		sta CURRENT_PARTYMEMBER+0
		lda partyMembers_hi,x
		sta CURRENT_PARTYMEMBER+1
		stx CURRENT_PARTYMEMBER_INDEX

		ldy #PartyMember::inventory
		loop:
			lda (CURRENT_PARTYMEMBER),y
			sta CUR_ITEM+0
			iny
			lda (CURRENT_PARTYMEMBER),y
			sta CUR_ITEM+1
			iny
			sty YREG
			ora CUR_ITEM+0
			beq :+
				jsr checkItem
				bcc :+
					inc COUNT
					ldx CURRENT_PARTYMEMBER_INDEX
					inc partyMemberStatsChanged,x
					ldy YREG
					lda #0
					dey
					sta (CURRENT_PARTYMEMBER),y
					dey
					sta (CURRENT_PARTYMEMBER),y
			:
			ldy YREG
			cpy #PartyMember::inventory+27*2
		bne loop

		ldx CURRENT_PARTYMEMBER_INDEX
		inx
		cpx #6
	bne nextMember

	lda POINTER_ITEM+0
	sta CUR_ITEM+0
	lda POINTER_ITEM+1
	sta CUR_ITEM+1
	ora CUR_ITEM+0
	beq :+
		jsr checkItem
		bcc :+
			inc COUNT
			lda #0
			sta POINTER_ITEM+0
			sta POINTER_ITEM+1
			jsrf updatePointerSprite
	:

	inc 1

	lda COUNT
	cmp #1
	rts

checkItem:
	lda type
	bmi :+
		ldy #Item::type
		lda (CUR_ITEM),y
		cmp type
		bne notFound
	:

	lda value
	bmi found

	ldy #Item::value
	lda (CUR_ITEM),y
	cmp value
	bne notFound
		found:
		sec
		rts
	notFound:
	clc
	rts
.endproc

; x: <index
; y: >index
.export items_getItemPointer
.proc items_getItemPointer
		; MSRC = items + index*sizeof(Item)
		stx NUM1+0
		sty NUM1+1
		lda #.sizeof(Item)
		sta NUM2
		jsr multiply
		clc
		adc #<items
		sta CUR_ITEM+0
		tya
		adc #>items
		sta CUR_ITEM+1
		rts
.endproc

; CUR_ITEM
.export items_dropItemAtPartyPosition
.proc items_dropItemAtPartyPosition
		lda partyPosition+0
		sta ARGS+0
		lda partyPosition+1
		sta ARGS+1
		lda partyDirection
		asl
		asl
		sta ARGS+2
		jsr rnd
		and #1
		ora ARGS+2
		tax
		lda subposTab,x
		sta ARGS+2
		jsrf getTopItem
		jsrf items_linkItem
		ldy #0
		lda TOP_ITEM+0
		sta (TMP),y
		iny
		lda TOP_ITEM+1
		sta (TMP),y

		sec
		rts
subposTab: .byte 0, 1, 2, 3, 1, 3, 0, 2, 3, 2, 1, 0, 2, 0, 3, 1
.endproc

; x: <base
; y: >base
.export items_createItemsFromBaseAtPartyPosition
.proc items_createItemsFromBaseAtPartyPosition
		jsr items_createItemFromBase
		lda CUR_ITEM+0
		ora CUR_ITEM+1
		bne :+
			clc
			rts
		:
		jmp items_dropItemAtPartyPosition
.endproc

; x: <base
; y: >base
; Returns CUR_ITEM
.export items_createItemFromBase
.proc items_createItemFromBase
		dec 1

		; MSRC = items + index*sizeof(Item)
		stx NUM1+0
		sty NUM1+1
		lda #.sizeof(Item)
		sta NUM2
		jsr multiply
		clc
		adc #<items
		sta MSRC+0
		tya
		adc #>items
		sta MSRC+1

		lda #<(items+.sizeof(Item))
		sta CUR_ITEM+0
		lda #>(items+.sizeof(Item))
		sta CUR_ITEM+1
		nextItem:
			ldy #Item::position
			lda (CUR_ITEM),y
			iny
			and (CUR_ITEM),y
			cmp #$ff
			beq break
			clc
			lda CUR_ITEM+0
			adc #<.sizeof(Item)
			sta CUR_ITEM+0
			lda CUR_ITEM+1
			adc #>.sizeof(Item)
			sta CUR_ITEM+1
			cmp #>lastItem
			bne nextItem
			lda CUR_ITEM+0
			cmp #<lastItem
		bne nextItem

		lda #0
		sta CUR_ITEM+0
		sta CUR_ITEM+1
		inc 1
		rts

break:	ldy #.sizeof(Item)-1
		:
			lda (MSRC),y
			sta (CUR_ITEM),y
			dey
		bpl :-

		inc 1

		rts
.endproc

; Return newly created item in CUR_ITEM
.export items_createCustomItem
.proc items_createCustomItem
		flags = ARGS+0
		picture = ARGS+1
		value = ARGS+2
		type = ARGS+3

		dec 1

		lda #<(items + 11*.sizeof(Item))
		sta CUR_ITEM+0
		lda #>(items + 11*.sizeof(Item))
		sta CUR_ITEM+1
		nextItem:
			ldy #Item::position
			lda (CUR_ITEM),y
			cmp #<$fffe
			bne continue
			iny
			lda (CUR_ITEM),y
			cmp #>$fffe
			beq break

			continue:
			clc
			lda CUR_ITEM+0
			adc #<.sizeof(Item)
			sta CUR_ITEM+0
			lda CUR_ITEM+1
			adc #>.sizeof(Item)
			sta CUR_ITEM+1

			cmp #>(items + 17*.sizeof(Item))
			bne nextItem
			lda CUR_ITEM+0
			cmp #<(items + 17*.sizeof(Item))
		bne nextItem

		inc $d021
		jmp *-3

		break:

		ldy #Item::flags
		lda flags
		ora #ITEM_FLAGS_CURSED
		sta (CUR_ITEM),y
		ldy #Item::picture
		lda picture
		sta (CUR_ITEM),y
		ldy #Item::type
		lda type
		sta (CUR_ITEM),y
		ldy #Item::value
		lda value
		sta (CUR_ITEM),y
		lda #0
		ldy #Item::subpos
		sta (CUR_ITEM),y
		ldy #Item::position
		sta (CUR_ITEM),y
		iny
		sta (CUR_ITEM),y
		ldy #Item::nameIdentified
		sta (CUR_ITEM),y
		ldy #Item::nameUnidentified
		sta (CUR_ITEM),y
		ldy #Item::next
		sta (CUR_ITEM),y
		iny
		sta (CUR_ITEM),y
		ldy #Item::previous
		sta (CUR_ITEM),y
		iny
		sta (CUR_ITEM),y

		inc 1
		rts
.endproc

.export items_clearTopItemIndex 
.proc items_clearTopItemIndex
		lda #0
		tax
		:
			sta topItemPtrs+$000,x
			sta topItemPtrs+$100,x
			sta topItemPtrs+$200,x
			sta topItemPtrs+$300,x
			sta topItemPtrs+$400,x
			sta topItemPtrs+$500,x
			sta topItemPtrs+$600,x
			sta topItemPtrs+$700,x
			inx
		bne :-
		rts
.endproc

.export items_linkItemsOnLevel 
.proc items_linkItemsOnLevel
		dec 1
		lda #<items
		sta CUR_ITEM+0
		lda #>items
		sta CUR_ITEM+1
		nextItem:
			ldy #Item::level
			lda (CUR_ITEM),y
			cmp levelIndex
			bne skipItem
				ldy #Item::position+1
				lda (CUR_ITEM),y
				cmp #4
				bcs skipItem
					; (item.levelIndex == levelIndex) && ((unsigned)item.position < 0x400)
					sta ARGS+1
					ldy #Item::position
					lda (CUR_ITEM),y
					sta ARGS+0

					; TMP = itemPosition*2
					asl
					sta TMP+0
					lda ARGS+1
					rol
					sta TMP+1

					; TMP = topItemPtr+TMP
					clc
					lda #<topItemPtrs
					adc TMP+0
					sta TMP+0
					lda #>topItemPtrs
					adc TMP+1
					sta TMP+1

					; TOP_ITEM = *TMP
					ldy #0
					lda (TMP),y
					sta TOP_ITEM+0
					iny
					lda (TMP),y
					sta TOP_ITEM+1

					ldy #Item::subpos
					lda (CUR_ITEM),y
					sta ARGS+2

					inc 1
					jsr linkItem
					dec 1
					
					; *TMP = TOP_ITEM
					ldy #0
					lda TOP_ITEM+0
					sta (TMP),y
					iny
					lda TOP_ITEM+1
					sta (TMP),y
			skipItem:
			clc
			lda CUR_ITEM+0
			adc #<.sizeof(Item)
			sta CUR_ITEM+0
			lda CUR_ITEM+1
			adc #>.sizeof(Item)
			sta CUR_ITEM+1
			cmp #>lastItem
			bne nextItem
			lda CUR_ITEM+0
			cmp #<lastItem
		bne nextItem
		inc 1
		rts
.endproc

; THIS FUNCTION MUST _NOT_ CLOBBER TMP+0 and TMP+1 nor TMP2+0 and TMP2+1
.proc unlinkItem
	; [ ] - [CUR_ITEM] - [ ]

	;int previousItemIndex = currentItem.previous;
	ldy #Item::previous
	lda (CUR_ITEM),y
	sta PREV_ITEM+0
	iny
	lda (CUR_ITEM),y
	sta PREV_ITEM+1

	; [PREV_ITEM] <-> [CUR_ITEM] <-> [ ]

	;int nextItemIndex = currentItem.next;
	ldy #Item::next
	lda (CUR_ITEM),y
	sta NEXT_ITEM+0
	iny
	lda (CUR_ITEM),y
	sta NEXT_ITEM+1

	; [PREV_ITEM] <-> [CUR_ITEM] <-> [NEXT_ITEM]

	;previousItem.next = nextItemIndex;
	ldy #Item::next
	lda NEXT_ITEM+0
	sta (PREV_ITEM),y
	iny
	lda NEXT_ITEM+1
	sta (PREV_ITEM),y

	;              ---------------->
	; [PREV_ITEM] /<- [CUR_ITEM] <-> [NEXT_ITEM]

	;nextItem.previous = previousItemIndex;
	ldy #Item::previous
	lda PREV_ITEM+0
	sta (NEXT_ITEM),y
	iny
	lda PREV_ITEM+1
	sta (NEXT_ITEM),y

	; [PREV_ITEM] <-> [NEXT_ITEM]

	lda #0

	; currentItem.next = 0;
	ldy #Item::next
	sta (CUR_ITEM),y
	iny
	sta (CUR_ITEM),y

	; currentItem.previous = 0;
	ldy #Item::previous
	sta (CUR_ITEM),y
	iny
	sta (CUR_ITEM),y

	; currentItem.position = 0;
	ldy #Item::position
	sta (CUR_ITEM),y
	iny
	sta (CUR_ITEM),y

	;currentItem.level = 0;
	ldy #Item::level
	sta (CUR_ITEM),y

	;if (topItemIndex[0] == currentItemIndex)
	lda TOP_ITEM+0
	cmp CUR_ITEM+0
	bne :+
	lda TOP_ITEM+1
	cmp CUR_ITEM+1
	bne :+
		lda NEXT_ITEM+0
		sta TOP_ITEM+0
		lda NEXT_ITEM+1
		sta TOP_ITEM+1
	:

	;if (topItemIndex[0] == currentItemIndex)
	lda TOP_ITEM+0
	cmp CUR_ITEM+0
	bne :+
	lda TOP_ITEM+1
	cmp CUR_ITEM+1
	bne :+
		lda #0
		sta TOP_ITEM+0
		sta TOP_ITEM+1
	:

	inc 1
	rts
.endproc

; TOP_ITEM (zp-ptr)
; ARGS+0	sub position
; Returns CUR_ITEM
.export items_unlinkItemBySubPos
.proc items_unlinkItemBySubPos
	subPosition = ARGS+0

	lda TOP_ITEM+0
	ora TOP_ITEM+1
	bne :+
		; return 0;
		lda #0
		sta CUR_ITEM+0
		sta CUR_ITEM+1
		rts
	:

	lda TOP_ITEM+0
	sta END_ITEM+0
	sta CUR_ITEM+0
	lda TOP_ITEM+1
	sta END_ITEM+1
	sta CUR_ITEM+1

	dec 1
	loop:
		ldy #Item::subpos
		lda (CUR_ITEM),y
		cmp subPosition
		bne noMatch
			jmp unlinkItem
		noMatch:
	
		; currentItemIndex = currentItem.next;
		ldy #Item::next
		lax (CUR_ITEM),y
		iny
		lda (CUR_ITEM),y
		sta CUR_ITEM+1
		stx CUR_ITEM+0

		;  while (currentItemIndex!=stopItemIndex);
		cmp END_ITEM+1
		bne loop
		cpx END_ITEM+0
	bne loop

	; return 0;
	lda #0
	sta CUR_ITEM+0
	sta CUR_ITEM+1
	inc 1
	rts
.endproc

; TOP_ITEM (zp-ptr)
; ARGS+0 ;+1	itemIndex
.export items_unlinkItemByItemIndex
.proc items_unlinkItemByItemIndex
	itemIndex = ARGS+0

	lda TOP_ITEM+0
	ora TOP_ITEM+1
	bne :+
		; return 0;
		lda #0
		sta CUR_ITEM+0
		sta CUR_ITEM+1
		rts
	:

	lda TOP_ITEM+0
	sta END_ITEM+0
	sta CUR_ITEM+0
	lda TOP_ITEM+1
	sta END_ITEM+1
	sta CUR_ITEM+1

	dec 1
	loop:
		lda CUR_ITEM+0
		cmp itemIndex+0
		bne noMatch
		lda CUR_ITEM+1
		cmp itemIndex+1
		bne noMatch
			jmp unlinkItem
		noMatch:
	
		; currentItemIndex = currentItem.next;
		ldy #Item::next
		lax (CUR_ITEM),y
		iny
		lda (CUR_ITEM),y
		sta CUR_ITEM+1
		stx CUR_ITEM+0

		;  while (currentItemIndex!=stopItemIndex);
		cmp END_ITEM+1
		bne :+
		cpx END_ITEM+0
		bne :+
			; return 0;
			lda #0
			sta CUR_ITEM+0
			sta CUR_ITEM+1
			inc 1
			rts
		:
	jmp loop
.endproc

; TOP_ITEM (zp-ptr)
; ARGS+0	item position lo
; ARGS+1	item position hi
; CUR_ITEM (zp-ptr)
; ARGS+2	sub position
;
; THIS FUNCTION MUST _NOT_ CLOBBER TMP+0 and TMP+1
.export items_linkItem
items_linkItem:
.proc linkItem
	topItem = TOP_ITEM
	itemPosition = ARGS+0
	item = CUR_ITEM
	subPosition = ARGS+2

	; Exit if item == NULL
	lda item+0
	ora item+1
	bne :+
		rts
	:

	dec 1

	; Assign position, subPosition and level on item
	; item.subpos = subPosition
	ldy #Item::subpos
	lda subPosition
	sta (item),y

	; item.position = itemPosition
	ldy #Item::position
	lda itemPosition+0
	sta (item),y
	iny
	lda itemPosition+1
	sta (item),y

	; item.level = itemPosition>=0 ? levelIndex : -1
	bmi :+
		;itemPosition >= 0
		lda levelIndex
		jmp :++
	:
		;itemPosition <0
		lda #<-1
	:
	ldy #Item::level
	sta (item),y

	; Link
	lda topItem+0
	ora topItem+1
	bne topItemNotNull
	topItemIsNull:
		; item.previous = item
		; item.next = item
		; topItem = item
		lda item+0
		ldy #Item::previous
		sta (item),y
		ldy #Item::next
		sta (item),y
		sta topItem+0
		lda item+1
		ldy #Item::previous+1
		sta (item),y
		ldy #Item::next+1
		sta (item),y
		sta topItem+1
		inc 1
		rts

	topItemNotNull:
		; [PREV_ITEM] <-> [TOP_ITEM] <-> [?]

		; PREV_ITEM = topItem.previous
		ldy #Item::previous
		lda (topItem),y
		sta PREV_ITEM+0
		iny
		lda (topItem),y
		sta PREV_ITEM+1

		;             -> [item] ->  
		; [PREV_ITEM] <----------- [TOP_ITEM] <-> [?]

		; item.next = prevItem.next, prevItem.next = item
		ldy #Item::next
		lda (PREV_ITEM),y
		sta (item),y
		lda item+0
		sta (PREV_ITEM),y
		iny
		lda (PREV_ITEM),y
		sta (item),y
		lda item+1
		sta (PREV_ITEM),y

		; [PREV_ITEM] <-> [item] <-> [TOP_ITEM] <-> [?]

		; item.previous = prevItem, topItem.previous = item
		ldy #Item::previous
		lda PREV_ITEM+0
		sta (item),y
		lda item+0
		sta (topItem),y
		iny
		lda PREV_ITEM+1
		sta (item),y
		lda item+1
		sta (topItem),y

		; topItem = item
		lda item+0
		sta topItem+0
		lda item+1
		sta topItem+1

		inc 1
		rts
.endproc

; TOP_ITEM
; itemTypeFilter = ARGS+2
; return ARGS+0,ARGS+1 = count
.export items_countItems
.proc items_countItems
	itemTypeFilter = ARGS+2

	dec 1

	;int count=0;
	lda #0
	sta ARGS+0
	sta ARGS+1

	;int currentItemIndex = topItemIndex;
	lda TOP_ITEM+0
	sta CUR_ITEM+0
	lda TOP_ITEM+1
	sta CUR_ITEM+1

	;if (currentItemIndex==0)
	ora CUR_ITEM+0
	bne :+
		inc 1
		rts
	:

	loop:
		lda itemTypeFilter
		cmp #<-1
		beq :+
			ldy #Item::type
			cmp (CUR_ITEM),y
			bne :++
		:
		inc ARGS+0
		bne :+
			inc ARGS+1
		:

		;currentItemIndex = currentItem.next;
		ldy #Item::next
		lda (CUR_ITEM),y
		pha
		iny
		lda (CUR_ITEM),y
		sta CUR_ITEM+1
		pla
		sta CUR_ITEM+0

		;while(currentItemIndex!=topItemIndex);
		cmp TOP_ITEM+0
		bne loop
		lda CUR_ITEM+1
		cmp TOP_ITEM+1
	bne loop

	inc 1
	rts
.endproc

; TOP_ITEM
;.export countItemsByType
;.proc countItemsByType
;	filterType = ARGS+0
;	rts
;.endproc

; TOP_ITEM
.export items_identifyLinkedItems 
.proc items_identifyLinkedItems
	dec 1

	;int currentItemIndex = topItemIndex;
	lda TOP_ITEM+0
	sta CUR_ITEM+0
	lda TOP_ITEM+1
	sta CUR_ITEM+1

	;if (currentItemIndex==0)
	ora CUR_ITEM+0
	bne :+
		inc 1
		rts
	:

	loop:
		ldy #Item::flags
		lda (CUR_ITEM),y
		ora #ITEM_FLAGS_IDENTIFIED
		sta (CUR_ITEM),y

		;currentItemIndex = currentItem.next;
		ldy #Item::previous
		lda (CUR_ITEM),y
		pha
		iny
		lda (CUR_ITEM),y
		sta CUR_ITEM+1
		pla
		sta CUR_ITEM+0

		;while(currentItemIndex!=topItemIndex);
		cmp TOP_ITEM+0
		bne loop
		lda CUR_ITEM+1
		cmp TOP_ITEM+1
	bne loop

	inc 1
	rts
.endproc

.export items_prepareForSave
.proc items_prepareForSave
	dec 1

	ldy #0
	lda #<pointerItemSaved
	sta CUR_ITEM+0
	lda #>pointerItemSaved
	sta CUR_ITEM+1
	jsr reloc

	

	lda #<items
	sta CUR_ITEM+0
	lda #>items
	sta CUR_ITEM+1
	nextItem:
		ldy #Item::previous
		jsr reloc

		ldy #Item::next
		jsr reloc

		clc
		lda CUR_ITEM+0
		adc #<.sizeof(Item)
		sta CUR_ITEM+0
		lda CUR_ITEM+1
		adc #>.sizeof(Item)
		sta CUR_ITEM+1
		cmp #>lastItem
		bne nextItem
		lda CUR_ITEM+0
		cmp #<lastItem
	bne nextItem

	inc 1
	rts

reloc:
	lda (CUR_ITEM),y
	iny
	ora (CUR_ITEM),y
	beq :+
		dey
		sec
		lda (CUR_ITEM),y
		sbc #<items
		sta (CUR_ITEM),y
		iny
		lda (CUR_ITEM),y
		sbc #>items
		sta (CUR_ITEM),y
	:
	rts
.endproc

.export items_prepareFromLoad
.proc items_prepareFromLoad
	dec 1

	inc 1
	rts
.endproc

; CUR_ITEM
.export printItemName
.proc printItemName
	identified = ARGS+0

	dec 1

	;if (item.nameUnidentified==0xff)
	ldy #Item::nameUnidentified
	lda (CUR_ITEM),y
	cmp #$ff
	bne :+
		jmp return
	:

	;if (identified && item.nameIdentified==0xff)
	lda identified
	beq :+
	ldy #Item::nameIdentified
	lda (CUR_ITEM),y
	cmp #$ff
	bne :+
		jmp return
	:
	
	;if (!identified)
	lda identified
	bne :+
		ldy #Item::nameUnidentified
		jsr print
		jmp return
	:

	;if (item.nameIdentified!=1)
	ldy #Item::nameIdentified
	lda (CUR_ITEM),y
	cmp #1
	beq :+
		ldy #Item::nameIdentified
		jsr print
		jmp return
	:

	;switch (nameTypes[item.type])
	ldy #Item::type
	lax (CUR_ITEM),y
	ldy nameTypes,x
	lda nameHandler_lo,y
	sta TMP+0
	lda nameHandler_hi,y
	sta TMP+1
	jmp (TMP)
plain:
	ldy #Item::nameUnidentified
	jsr print
	jmp return

plus:
	ldy #Item::nameUnidentified
	jsr print
	ldy #Item::value
	lda (CUR_ITEM),y
	bne :+
		jmp return
	:
	bpl :+
		eor #$ff
		clc
		adc #1
		ldx #<minusString
		ldy #>minusString
		jmp :++
	:	
		ldx #<plusString
		ldy #>plusString
	:
	pha
	jsr text_writeNullString
	pla
	sta ARGS+0
	jsrf text_writeDecimalByte
	jmp return

potion:
	ldx #<potionOfString
	ldy #>potionOfString
	jsr text_writeNullString
	ldy #Item::value
	lax (CUR_ITEM),y
	lda potions_lo,x
	pha
	ldy potions_hi,x
	pla
	tax
	jsr text_writeNullString
	jmp return
mage:
	ldx #<mageOfString
	ldy #>mageOfString
	jsr text_writeNullString
	ldy #Item::value
	lax (CUR_ITEM),y
	lda mageScrolls_lo,x
	pha
	ldy mageScrolls_hi,x
	pla
	tax
	jsr text_writeNullString
	jmp return
cleric:
	ldx #<clericOfString
	ldy #>clericOfString
	jsr text_writeNullString
	ldy #Item::value
	lax (CUR_ITEM),y
	lda clericScrolls_lo,x
	pha
	ldy clericScrolls_hi,x
	pla
	tax
	jsr text_writeNullString
	jmp return
ring:
	ldx #<ringOfString
	ldy #>ringOfString
	jsr text_writeNullString
	ldy #Item::value
	lax (CUR_ITEM),y
	lda rings_lo,x
	pha
	ldy rings_hi,x
	pla
	tax
	jsr text_writeNullString
	jmp return
wand:
	ldy #Item::value
	lda (CUR_ITEM),y
	bmi :+
	beq :+
		ldx #<wandOfString
		ldy #>wandOfString
		jsr text_writeNullString
		ldy #Item::value
		lax (CUR_ITEM),y
		lda wands_lo,x
		pha
		ldy wands_hi,x
		pla
		tax
		jsr text_writeNullString	
		jmp return
	:
	ldx #<wandString
	ldy #>wandString
	jsr text_writeNullString
return:	inc 1
	rts

print:	lax (CUR_ITEM),y
	lda itemName_lo,x
	pha
	ldy itemName_hi,x
	pla
	tax
	jsr text_writeNullString
	rts

nameHandler_lo:
	.byte <plain, <plus, <potion, <mage, <cleric, <ring, <wand
nameHandler_hi:
	.byte >plain, >plus, >potion, >mage, >cleric, >ring, >wand
nameTypes:
	.byte 1,1,1,1,1,1,2,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,0,3,4,0,0,0,2,0,1,5,1,0,1,0,5,6,0

plusString:
	.byte " +",0

minusString:
	.byte " -",0

potionOfString:
	.byte "Potion of ",0

mageOfString:
	.byte "Mage Scroll of ",0

clericOfString:
	.byte "Cleric Scroll of ",0

ringOfString:
	.byte "Ring of ",0

wandOfString:
	.byte "Wand of ",0

wandString:
	.byte "Wand",0

rings:
	_ring0:.byte "Adornment",0
	_ring1:.byte "Wizardry",0
	_ring2:.byte "Sustenance",0
	_ring3:.byte "Feather Fall",0
rings_lo:
	.byte <_ring0, <_ring1, <_ring2, <_ring3
rings_hi:
	.byte >_ring0, >_ring1, >_ring2, >_ring3

clericScrolls:
	_cs00: .byte "",0
	_cs01: .byte "Bless",0
	_cs02: .byte "Cure Light Wnds",0
	_cs03: .byte "Cause Light Wnds",0
	_cs04: .byte "Detect Magic",0
	_cs05: .byte "Protect Evil",0
	_cs06: .byte "Aid",0
	_cs07: .byte "Flame Blade",0
	_cs08: .byte "Hold Person",0
	_cs09: .byte "Slow Poison",0
	_cs0a: .byte "Create Food",0
	_cs0b: .byte "Dispel Magic",0
	_cs0c: .byte "Magical Vestment",0
	_cs0d: .byte "Prayer",0
	_cs0e: .byte "Remove Paralysis",0
	_cs0f: .byte "Cure Serious",0
	_cs10: .byte "Cause Serious",0
	_cs11: .byte "Neutral Poison",0
	_cs12: .byte "Protect Evil Ten'",0
	_cs13: .byte "Protect Lightning",0
	_cs14: .byte "Cure Critical",0
	_cs15: .byte "Cause Critical",0
	_cs16: .byte "Flame Strike",0
	_cs17: .byte "Raise Dead",0
	_cs18: .byte "Lay on Hands",0
clericScrolls_lo: .byte <_cs00, <_cs01, <_cs02, <_cs03, <_cs04, <_cs05, <_cs06, <_cs07, <_cs08, <_cs09, <_cs0a, <_cs0b, <_cs0c, <_cs0d, <_cs0e, <_cs0f, <_cs10, <_cs11, <_cs12, <_cs13, <_cs14, <_cs15, <_cs16, <_cs17, <_cs18
clericScrolls_hi: .byte >_cs00, >_cs01, >_cs02, >_cs03, >_cs04, >_cs05, >_cs06, >_cs07, >_cs08, >_cs09, >_cs0a, >_cs0b, >_cs0c, >_cs0d, >_cs0e, >_cs0f, >_cs10, >_cs11, >_cs12, >_cs13, >_cs14, >_cs15, >_cs16, >_cs17, >_cs18

.export getCLERICscroll
.proc getCLERICscroll
	lda CLERICScrolls_lo,x
	sta SRC+0
	lda CLERICScrolls_hi,x
	sta SRC+1
	ldy #12
	:
		lda (SRC),y
		sta ARGS,y
		dey
	bpl :-
	rts
.endproc

CLERICScrolls:
	_CS00: .byte "             ";0
	_CS01: .byte "Bless        ";1
	_CS02: .byte "Cure Lt Wnds ";1
	_CS03: .byte "Cause Lt Wnds";1
	_CS04: .byte "Detect Magic ";1
	_CS05: .byte "Protect Evil ";1
	_CS06: .byte "Aid          ";2
	_CS07: .byte "Flame Blade  ";2
	_CS08: .byte "Hold Person  ";2
	_CS09: .byte "Slow Poison  ";2
	_CS0a: .byte "Create Food  ";3
	_CS0b: .byte "Dispel Magic ";3
	_CS0c: .byte "Magical Vest ";3
	_CS0d: .byte "Prayer       ";3
	_CS0e: .byte "Rem Paralysis";3
	_CS0f: .byte "Cure Serious ";4
	_CS10: .byte "Cause Serious";4
	_CS11: .byte "Remove Poison";4
	_CS12: .byte "Prt Evil Ten ";4
	_CS13: .byte "Prt Lightning";4
	_CS14: .byte "Cure Crit    ";5
	_CS15: .byte "Cause Crit   ";5
	_CS16: .byte "Flame Strike ";5
	_CS17: .byte "Raise Dead   ";5
	_CS18: .byte "Lay on Hands ";1
CLERICScrolls_lo: .byte <_CS00, <_CS01, <_CS02, <_CS03, <_CS04, <_CS05, <_CS06, <_CS07, <_CS08, <_CS09, <_CS0a, <_CS0b, <_CS0c, <_CS0d, <_CS0e, <_CS0f, <_CS10, <_CS11, <_CS12, <_CS13, <_CS14, <_CS15, <_CS16, <_CS17, <_CS18
CLERICScrolls_hi: .byte >_CS00, >_CS01, >_CS02, >_CS03, >_CS04, >_CS05, >_CS06, >_CS07, >_CS08, >_CS09, >_CS0a, >_CS0b, >_CS0c, >_CS0d, >_CS0e, >_CS0f, >_CS10, >_CS11, >_CS12, >_CS13, >_CS14, >_CS15, >_CS16, >_CS17, >_CS18

mageScrolls:
	_ms00: .byte "",0
	_ms01: .byte "Armor",0
	_ms02: .byte "Burning Hands",0
	_ms03: .byte "Detect Magic",0
	_ms04: .byte "Magic Missile",0
	_ms05: .byte "Read Magic",0
	_ms06: .byte "Shield",0
	_ms07: .byte "Shocking Grasp",0
	_ms08: .byte "Invisibility",0
	_ms09: .byte "Knock",0
	_ms0a: .byte "M's Acid Arrow",0
	_ms0b: .byte "Stinking Cloud",0
	_ms0c: .byte "Dispel Magic",0
	_ms0d: .byte "Fireball",0
	_ms0e: .byte "Flame Arrow",0
	_ms0f: .byte "Haste",0
	_ms10: .byte "Hold Person",0
	_ms11: .byte "Invisibility Ten'",0
	_ms12: .byte "Lightning Bolt",0
	_ms13: .byte "Vampiric Touch",0
	_ms14: .byte "Fear",0
	_ms15: .byte "Ice Storm",0
	_ms16: .byte "Stoneskin",0
	_ms17: .byte "Cloudkill",0
	_ms18: .byte "Cone of Cold",0
	_ms19: .byte "Hold Monster",0
mageScrolls_lo: .byte <_ms00, <_ms01, <_ms02, <_ms03, <_ms04, <_ms05, <_ms06, <_ms07, <_ms08, <_ms09, <_ms0a, <_ms0b, <_ms0c, <_ms0d, <_ms0e, <_ms0f, <_ms10, <_ms11, <_ms12, <_ms13, <_ms14, <_ms15, <_ms16, <_ms17, <_ms18, <_ms19
mageScrolls_hi: .byte >_ms00, >_ms01, >_ms02, >_ms03, >_ms04, >_ms05, >_ms06, >_ms07, >_ms08, >_ms09, >_ms0a, >_ms0b, >_ms0c, >_ms0d, >_ms0e, >_ms0f, >_ms10, >_ms11, >_ms12, >_ms13, >_ms14, >_ms15, >_ms16, >_ms17, >_ms18, >_ms19

spellNames_lo:
	.byte <_ms00, <_ms01, <_ms02, <_ms03, <_ms04, <_ms05, <_ms06, <_ms07, <_ms08, <_ms09, <_ms0a, <_ms0b, <_ms0c, <_ms0d, <_ms0e, <_ms0f, <_ms10, <_ms11, <_ms12, <_ms13, <_ms14, <_ms15, <_ms16, <_ms17, <_ms18, <_ms19
	.byte <_cs01, <_cs02, <_cs03, <_cs04, <_cs05, <_cs06, <_cs07, <_cs08, <_cs09, <_cs0a, <_cs0b, <_cs0c, <_cs0d, <_cs0e, <_cs0f, <_cs10, <_cs11, <_cs12, <_cs13, <_cs14, <_cs15, <_cs16, <_cs17, <_cs18

spellNames_hi:
	.byte >_ms00, >_ms01, >_ms02, >_ms03, >_ms04, >_ms05, >_ms06, >_ms07, >_ms08, >_ms09, >_ms0a, >_ms0b, >_ms0c, >_ms0d, >_ms0e, >_ms0f, >_ms10, >_ms11, >_ms12, >_ms13, >_ms14, >_ms15, >_ms16, >_ms17, >_ms18, >_ms19
	.byte >_cs01, >_cs02, >_cs03, >_cs04, >_cs05, >_cs06, >_cs07, >_cs08, >_cs09, >_cs0a, >_cs0b, >_cs0c, >_cs0d, >_cs0e, >_cs0f, >_cs10, >_cs11, >_cs12, >_cs13, >_cs14, >_cs15, >_cs16, >_cs17, >_cs18

; x = spellIndex
.export printSpellName
.proc printSpellName
	pha
	txa
	pha
	tya
	pha
	lda spellNames_lo,x
	ldy spellNames_hi,x
	tax
	jsr text_writeNullString
	pla
	tay
	pla
	tax
	pla
	rts
.endproc

; x = spellIndex
.export printSpellName2
.proc printSpellName2
	pha
	txa
	pha
	tya
	pha
	lda spellNames_lo,x
	ldy spellNames_hi,x
	tax
	clc
	jsr text_writeNullString2
	pla
	tay
	pla
	tax
	pla
	rts
.endproc

; x = spellIndex
.export printSpellName3
.proc printSpellName3
	pha
	txa
	pha
	tya
	pha
	lda spellNames_lo,x
	ldy spellNames_hi,x
	tax
	jsr text_writeNullString3
	pla
	tay
	pla
	tax
	pla
	rts
.endproc

.export getMAGEscroll
.proc getMAGEscroll
	lda MAGEScrolls_lo,x
	sta SRC+0
	lda MAGEScrolls_hi,x
	sta SRC+1
	ldy #12
	:
		lda (SRC),y
		sta ARGS,y
		dey
	bpl :-
	rts
.endproc

MAGEScrolls:
	_MS00: .byte "             ";0
	_MS01: .byte "Armor        ";1
	_MS02: .byte "Burning Hands";1
	_MS03: .byte "Detect Magic ";1
	_MS04: .byte "Magic Missile";1
	_MS05: .byte "Read Magic   ";1
	_MS06: .byte "Shield       ";1
	_MS07: .byte "Shocking Grsp";1
	_MS08: .byte "Invisibility ";2
	_MS09: .byte "Knock        ";2
	_MS0a: .byte "Ms Acid Arrow";2
	_MS0b: .byte "Stinky Cloud ";2
	_MS0c: .byte "Dispel Magic ";3
	_MS0d: .byte "Fireball     ";3
	_MS0e: .byte "Flame Arrow  ";3
	_MS0f: .byte "Haste        ";3
	_MS10: .byte "Hold Person  ";3
	_MS11: .byte "Invisible Ten";3
	_MS12: .byte "Lightning Blt";3
	_MS13: .byte "Vampire Touch";3
	_MS14: .byte "Fear         ";4
	_MS15: .byte "Ice Storm    ";4
	_MS16: .byte "Stoneskin    ";4
	_MS17: .byte "Cloudkill    ";5
	_MS18: .byte "Cone of Cold ";5
	_MS19: .byte "Hold Monster ";5
MAGEScrolls_lo: .byte <_MS00, <_MS01, <_MS02, <_MS03, <_MS04, <_MS05, <_MS06, <_MS07, <_MS08, <_MS09, <_MS0a, <_MS0b, <_MS0c, <_MS0d, <_MS0e, <_MS0f, <_MS10, <_MS11, <_MS12, <_MS13, <_MS14, <_MS15, <_MS16, <_MS17, <_MS18, <_MS19
MAGEScrolls_hi: .byte >_MS00, >_MS01, >_MS02, >_MS03, >_MS04, >_MS05, >_MS06, >_MS07, >_MS08, >_MS09, >_MS0a, >_MS0b, >_MS0c, >_MS0d, >_MS0e, >_MS0f, >_MS10, >_MS11, >_MS12, >_MS13, >_MS14, >_MS15, >_MS16, >_MS17, >_MS18, >_MS19

potions:
	_p0: .byte "",0
	_p1: .byte "Giant Strength",0
	_p2: .byte "Healing",0
	_p3: .byte "Extra Healing",0
	_p4: .byte "Poison",0
	_p5: .byte "Vitality",0
	_p6: .byte "Speed",0
	_p7: .byte "Invisibility",0
	_p8: .byte "Cure Poison",0
potions_lo: .byte <_p0, <_p1, <_p2, <_p3, <_p4, <_p5, <_p6, <_p7, <_p8
potions_hi: .byte >_p0, >_p1, >_p2, >_p3, >_p4, >_p5, >_p6, >_p7, >_p8

wands:
	_w0: .byte "",0
       	_w1: .byte "Lightning",0
       	_w2: .byte "Frost",0
       	_w3: .byte "Curing",0
       	_w4: .byte "Fireball",0
       	_w5: .byte "Silvias",0
       	_w6: .byte "Magic Missile",0
wands_lo: .byte <_w0, <_w1, <_w2, <_w3, <_w4, <_w5, <_w6
wands_hi: .byte >_w0, >_w1, >_w2, >_w3, >_w4, >_w5, >_w6
.endproc
