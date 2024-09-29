.include "global.inc"

.segment "BSS"
  nbrMonsters: .res 1
  currentMemberIndex:
  currentMonsterIndex: .res 1

.segment "SPELLS"

.export spellBank
spellBank:

.macro setSpellTimeout spellIndex, timeoutA, timeoutMultiplier, timeoutB, reuseSameActionSpot
  lda #timeoutA
  sta ARGS+0
  lda #<timeoutMultiplier
  sta ARGS+1
  lda #>timeoutMultiplier
  sta ARGS+2
  lda #timeoutB
  sta ARGS+3
  lda spellIndex
  sta ARGS+4
  lda #reuseSameActionSpot
  sta ARGS+5
  jsrf gameState_setSpellTimeout
.endmacro

.macro setSpellTimeout2 spellIndex, timeoutA, timeoutMultiplier, reuseSameActionSpot
  sta ARGS+3
  lda #timeoutA
  sta ARGS+0
  lda #<timeoutMultiplier
  sta ARGS+1
  lda #>timeoutMultiplier
  sta ARGS+2
  lda spellIndex
  sta ARGS+4
  lda #reuseSameActionSpot
  sta ARGS+5
  jsrf gameState_setSpellTimeout
.endmacro

.macro DefSpellType usage, soundEffect, deactivatesInvisibility, handler
  .byte usage | (deactivatesInvisibility*SPELLTYPEUSAGE_DEACTIVATEINVISIBILITY)
  .byte soundEffect
  .word handler+0
  .word handler+3
  .word handler+6
.endmacro

throwSubpos:
  .byte 0,1,3,2
  .byte 1,3,2,0
  .byte 2,0,1,3
  .byte 3,2,0,1

.proc DefaultSpellHandler
  jmp cast
  jmp hit
  jmp expired

  .proc cast
    rts
  .endproc

  .proc hit
    clc
    rts
  .endproc

  .proc expired
    rts
  .endproc

  ; DICE_*
  ; CURRENT_THROWN
  ; a = Damage Multiplier
  ; return hit boolean in c
  .proc defaultHit
    .pushseg
    .segment "BSS"
      canHitCaster: .res 1
      firstOnPosition: .res 1
      includeAllOnPosition: .res 1
      alwaysHit: .res 1
      damageReduction: .res 1
      gotDamage: .res 1
      damageMultiplier: .res 1
      diceRolls: .res 1
      diceSides: .res 1
      diceBase: .res 1
    .popseg

    cmp #1
    bpl :+
      lda #1
    :
    sta damageMultiplier

    lda DICE_ROLLS
    sta diceRolls
    lda DICE_SIDES
    sta diceSides
    lda DICE_BASE
    sta diceBase

    lda #0
    sta gotDamage

    ldy #Thrown::caster
    lda (CURRENT_THROWN),y
    sta ATTACKING_PARTYMEMBER_INDEX
    lda #0
    sta CUR_ITEM+0
    sta CUR_ITEM+1

    ldy #Thrown::flags
    lax (CURRENT_THROWN),y
    lda #SPELLFLAGS_CANHITCASTER
    sax canHitCaster
    lda #SPELLFLAGS_FIRSTONPOSITION
    sax firstOnPosition
    lda #SPELLFLAGS_ALLONPOSITION
    sax includeAllOnPosition
    lda #SPELLFLAGS_ALWAYSHIT
    sax alwaysHit

    ; Check monster hit
    ldy #Thrown::position
    clc
    lda (CURRENT_THROWN),y
    adc #<triggerBitsAndMonsterCount
    sta TMP+0
    iny
    lda (CURRENT_THROWN),y
    adc #>triggerBitsAndMonsterCount
    sta TMP+1
    ldy #0
    lda (TMP),y
    and #7
    bne monstersToHit
      jmp noMonstersToHit
    monstersToHit:
      lda canHitCaster
      bne :+
        ldy #Thrown::caster
        lda (CURRENT_THROWN),y
        bpl :+
          jmp noMonstersToHit
      :

      ldy #Thrown::position
      lda (CURRENT_THROWN),y
      sta ARGS+0
      iny
      lda (CURRENT_THROWN),y
      sta ARGS+1
      ldy #Thrown::subpos
      lda (CURRENT_THROWN),y
      sta ARGS+2
      ldy #Thrown::direction
      lda (CURRENT_THROWN),y
      sta ARGS+3
      lda includeAllOnPosition
      sta ARGS+4
      lda firstOnPosition
      sta ARGS+5
      jsrf gameState_getMonstersOnPosition
      cpy #0
      bne :+
        jmp return
      :
      sty nbrMonsters

      .scope
      ldy #0
      nextMonster:
        sty currentMonsterIndex

        ldx MONSTERS_ON_LOC,y
        stx XREG
        clc
        ;jsrf gameState_willCasterHitMonster
        ;bcc continue

        ;int damage = damageDice.roll()*damageMultiplier;
        lda diceRolls
        sta DICE_ROLLS
        lda diceSides
        sta DICE_SIDES
        lda diceBase
        sta DICE_BASE
        jsrf rollDice
        lda DICE_RESULT+0
        sta NUM1+0
        lda DICE_RESULT+1
        sta NUM1+1
        lda damageMultiplier
        sta NUM2
        jsr multiply
        pha

        ldx XREG
        jsrf gameState_shouldReduceDamageOnMonster
        pla
        bcc :+
          lsr
        :
        pha

        lda #1
        sta gotDamage

        ldx XREG
        jsrf gameState_checkMonsterSpellImmunity
        pla
        bcs continue

        ldx XREG
        sta ARGS+0
        jsrf gameState_giveMonsterDamage

        continue:
        ldy currentMonsterIndex
        iny
        cpy nbrMonsters
        beq :+
      jmp nextMonster
      :jmp return
      .endscope
    noMonstersToHit:

    ; Check party hit
    ldy #Thrown::position
    lda (CURRENT_THROWN),y
    cmp partyPosition+0
    bne :+
    iny
    lda (CURRENT_THROWN),y
    cmp partyPosition+1
    beq partyPotentiallyHit
    :jmp noPartyHit
    partyPotentiallyHit:
      lda canHitCaster
      bne partyHit
        ldy #Thrown::caster
        lda (CURRENT_THROWN),y
        bmi partyHit
        jmp noPartyHit
      partyHit:

      lda includeAllOnPosition
      bne :+
        jmp dontIncludeAllOnPosition
      :
        .scope
        ldx #0
        nextPartyMember:
          stx currentMemberIndex
          lda partyMembers_lo,x
          sta CURRENT_PARTYMEMBER+0
          lda partyMembers_hi,x
          sta CURRENT_PARTYMEMBER+1

          ldy #PartyMember::status
          lda (CURRENT_PARTYMEMBER),y
          and #1
          bne :+
            jmp continue
          :

          lda alwaysHit
          bne :+
            ldx #0
            jsrf monsterTimerHandler_willMonsterHit
            bcs :+
              jmp continue
          :

          jsr damageOnPartyMember

          ldx currentMemberIndex
          stx ARGS+0
          jsrf gameState_giveMemberDamage

          continue:
          ldx currentMemberIndex
          inx
          cpx #6
          beq :+
        jmp nextPartyMember
        :jmp return
        .endscope
      dontIncludeAllOnPosition:
        .scope
        ldy #Thrown::subpos
        lda (CURRENT_THROWN),y
        and #3
        asl
        asl
        ora partyDirection
        tax
        lda objectRelativeSubPositions,x
        sta currentMemberIndex
        cmp #2
        bcc :++
          ldx #4
          ldy #1
          jsrf gameState_getMemberStatus
          bcs :+
          ldx #5
          ldy #1
          jsrf gameState_getMemberStatus
          bcc :++
          :
            jsr rnd
            and #1
            beq :+
              clc
              lda currentMemberIndex
              adc #2
              sta currentMemberIndex
        :

        ldx currentMemberIndex
        lda partyMembers_lo,x
        sta CURRENT_PARTYMEMBER+0
        lda partyMembers_hi,x
        sta CURRENT_PARTYMEMBER+1

        ldy #Thrown::itemOrSpellIndex
        lda (CURRENT_THROWN),y
        bne :+
          ldy #PartyMember::activeSpells+0
          lda (CURRENT_PARTYMEMBER),y
          and #SPELL_SHIELD
          beq :+
            jmp return
        :
        ldy #PartyMember::status
        lda (CURRENT_PARTYMEMBER),y
        and #1
        beq :+
          jsr damageOnPartyMember
          ldx currentMemberIndex
          stx ARGS+0
          jsrf gameState_giveMemberDamage
        :
        jmp return
        .endscope

damageOnPartyMember:
.scope
    ;int damage = damageDice.roll()*damageMultiplier;
    lda diceRolls
    sta DICE_ROLLS
    lda diceSides
    sta DICE_SIDES
    lda diceBase
    sta DICE_BASE
    jsrf rollDice
    lda DICE_RESULT+0
    sta NUM1+0
    lda DICE_RESULT+1
    sta NUM1+1
    lda damageMultiplier
    sta NUM2
    jsr multiply
    pha

    lda #4
    sta ARGS+3
    jsrf gameState_getMemberDamageReductionModifier
    pla
    bcc :+
      lsr
    :
    sta ARGS+1
    lda #0
    sta ARGS+2

    lda #1
    sta gotDamage

    rts
.endscope


    noPartyHit:
return:   lda gotDamage
    beq :+
      ldy #Thrown::flags
      lda (CURRENT_THROWN),y
      and #SPELLFLAGS_DAMAGEEXPLOSION
      beq :+
        jsrf gameState_handleExplosion
        sec
        rts
    :

    ldy #Thrown::itemOrSpellIndex
    lda (CURRENT_THROWN),y
    cmp #5
    bne :+
      lda #0
      sta gotDamage
    :

    lda gotDamage
    cmp #1
    rts
  .endproc
.endproc

.proc ArmorSpellHandler
  jmp cast
  jmp DefaultSpellHandler::hit
  jmp expired

  .proc cast
    lda AFFECTED_MEMBER+0
    sta CURRENT_PARTYMEMBER+0
    lda AFFECTED_MEMBER+1
    sta CURRENT_PARTYMEMBER+1
    jsrf gameState_getDexterityModifier2
    lda ARGS+0
    clc
    adc #6
    ldy #PartyMember::ac
    cmp (AFFECTED_MEMBER),y
    bcc :+
      lda #1
      sta textColor
      jsrf printMemberName
      ldx #<alreadyHas
      ldy #>alreadyHas
      jsr text_writeNullString
      rts
      alreadyHas: .byte "hat bereits eine hohe R",127,"stungsklasse.",$a,0
    :

    ldx SPELLBOOK_MEMBER_INDEX
    jsrf gameState_getSpellCasterDamageMultiplier
    lda ARGS+0
    clc
    adc #8
    ldy #PartyMember::field_DF
    sta (AFFECTED_MEMBER),y

    lda AFFECTED_MEMBER+0
    sta CURRENT_PARTYMEMBER+0
    lda AFFECTED_MEMBER+1
    sta CURRENT_PARTYMEMBER+1
    lda AFFECTED_MEMBER_INDEX
    sta CURRENT_PARTYMEMBER_INDEX
    jsrf calculateMemberAC

    rts
  .endproc

  .proc expired
    lda AFFECTED_MEMBER+0
    sta CURRENT_PARTYMEMBER+0
    lda AFFECTED_MEMBER+1
    sta CURRENT_PARTYMEMBER+1
    lda AFFECTED_MEMBER_INDEX
    sta CURRENT_PARTYMEMBER_INDEX
    jsrf calculateMemberAC
    rts
  .endproc
.endproc



.proc BurningHandsSpellHandler
  jmp cast
  jmp DefaultSpellHandler::hit
  jmp DefaultSpellHandler::expired

  .proc cast
    jsrf renderer_drawBurningHands

    ldx SPELLBOOK_MEMBER_INDEX
    jsrf gameState_getSpellCasterDamageMultiplier
    lda ARGS+0
    pha

    lda partyPosition+0
    sta ARGS+0
    lda partyPosition+1
    sta ARGS+1
    lda partyDirection
    sta ARGS+2
    jsrf gameState_getForwardPosition

    lda #1
    sta ARGS+2
    lda #3
    sta ARGS+3
    pla
    asl
    sta ARGS+4
    lda #1
    sta ARGS+5
    jsrf gameState_dealBurningHandsDamage

    rts
  .endproc
.endproc



.proc DetectMagicSpellHandler
  jmp cast
  jmp DefaultSpellHandler::hit
  jmp expired

  .proc cast
    lda partySpells+0
    ora #SPELL_DETECT_MAGIC
    sta partySpells+0
    setSpellTimeout castSpellIndex, 0, 546, 2, 1
    jsrf gui_refreshWholeParty
    rts
  .endproc

  .proc expired
    lda partySpells+0
    and #(SPELL_DETECT_MAGIC^$ff)
    sta partySpells+0
    jsrf gui_refreshWholeParty
    rts
  .endproc
.endproc



.proc MagicMissileSpellHandler
  jmp cast
  jmp hit
  jmp DefaultSpellHandler::expired

  .proc cast
    lda SPELLBOOK_MEMBER_INDEX
    sta ARGS+0
    lda #0
    sta ARGS+1
    lda partyPosition+0
    sta ARGS+2
    lda partyPosition+1
    sta ARGS+3
    lda SPELLCASTER_SUBPOS
    sta ARGS+4
    lda partyDirection
    sta ARGS+5
    jsr spells_throwSpell
    rts
  .endproc

  .proc hit
    dice 1,4,1
    ldy #Thrown::caster
    lax (CURRENT_THROWN),y
    jsrf gameState_getSpellCasterDamageMultiplier
    ldx ARGS+0
    dex
    txa
    lsr
    jmp DefaultSpellHandler::defaultHit
  .endproc
.endproc



.proc ShieldSpellHandler
  jmp cast
  jmp DefaultSpellHandler::hit
  jmp expired

  .proc cast
    ldy #PartyMember::activeSpells+0
    lda (AFFECTED_MEMBER),y
    ora #SPELL_SHIELD
    sta (AFFECTED_MEMBER),y
    
    lda AFFECTED_MEMBER+0
    sta CURRENT_PARTYMEMBER+0
    lda AFFECTED_MEMBER+1
    sta CURRENT_PARTYMEMBER+1
    jsrf calculateMemberAC

    setSpellTimeout castSpellIndex, 0, 546, 5, 1

    ldx AFFECTED_MEMBER_INDEX
    inc partyMemberStatsChanged,x

    rts
  .endproc

  .proc expired
    ldy #PartyMember::activeSpells+0
    lda (AFFECTED_MEMBER),y
    and #(SPELL_SHIELD^$ff)
    sta (AFFECTED_MEMBER),y
    jsrf calculateMemberAC
    ldx AFFECTED_MEMBER_INDEX
    inc partyMemberStatsChanged,x
    rts
  .endproc
.endproc

.macro getFreeHand
  ldy #PartyMember::inventory+INVENTORY_RIGHT_HAND*2
  lda (AFFECTED_MEMBER),y
  beq :+
    ldy #PartyMember::inventory+INVENTORY_LEFT_HAND*2
  :
.endmacro

.proc ShockingGraspSpellHandler
  jmp cast
  jmp DefaultSpellHandler::hit
  jmp expired

  .proc cast
    ldx AFFECTED_MEMBER_INDEX
    jsrf gameState_getSpellCasterDamageMultiplier
    lda ARGS+0
    pha

    lda #0
    sta ARGS+0
    sta ARGS+1
    sta ARGS+2
    sta ARGS+3
    sta ARGS+4
    lda #$f
    sta ARGS+5
    lda #1
    sta ARGS+6
    lda #8
    sta ARGS+7
    pla
    sta ARGS+8
    lda #ITEMUSAGE_MELEE_WEAPONS
    sta ARGS+9
    jsrf items_createCustomItemType

    lda #ITEM_FLAGS_CONSUMABLE
    sta ARGS+0
    lda #$52
    sta ARGS+1
    lda #$0
    sta ARGS+2
    stx ARGS+3
    jsrf items_createCustomItem

    getFreeHand
    lda CUR_ITEM+0
    sta (AFFECTED_MEMBER),y
    iny
    lda CUR_ITEM+1
    sta (AFFECTED_MEMBER),y

    setSpellTimeout castSpellIndex, 0, 546, 1, 1
    ldx AFFECTED_MEMBER_INDEX
    inc partyMemberStatsChanged,x

    rts
  .endproc

  .proc expired
    ldy #PartyMember::inventory + INVENTORY_LEFT_HAND*2
    jsr checkActiveHandSpellAndFree
    ldy #PartyMember::inventory + INVENTORY_RIGHT_HAND*2
    jsr checkActiveHandSpellAndFree
    ldx AFFECTED_MEMBER_INDEX
    inc partyMemberStatsChanged,x
    rts
  .endproc

  .proc checkActiveHandSpellAndFree
    sty TMP
    lda (AFFECTED_MEMBER),y
    sta CUR_ITEM+0
    iny
    lda (AFFECTED_MEMBER),y
    sta CUR_ITEM+1
    ldy #Item::type
    jsr lda_CUR_ITEM_y
    bmi :+
      ; Non custom item type
      rts
    :

    ldy TMP
    lda #0
    sta (AFFECTED_MEMBER),y
    iny
    sta (AFFECTED_MEMBER),y
    jsrf items_freeCustomItem

    rts
  .endproc
.endproc



.proc InvisibilitySpellHandler
  jmp cast
  jmp DefaultSpellHandler::hit
  jmp expired

  .proc cast
    ldy #PartyMember::activeSpells+0
    lda (AFFECTED_MEMBER),y
    ora #SPELL_INVISIBLE
    sta (AFFECTED_MEMBER),y
    ldx AFFECTED_MEMBER_INDEX
    inc partyMemberStatsChanged,x
    rts
  .endproc

  .proc expired
    ldy #PartyMember::activeSpells+0
    lda (AFFECTED_MEMBER),y
    and #(SPELL_INVISIBLE^$ff)
    sta (AFFECTED_MEMBER),y
    ldx AFFECTED_MEMBER_INDEX
    inc partyMemberStatsChanged,x
    rts
  .endproc
.endproc



.proc MelfsAcidArrowSpellHandler
  jmp cast
  jmp hit
  jmp DefaultSpellHandler::expired

  .proc cast
    lda SPELLBOOK_MEMBER_INDEX
    sta ARGS+0
    lda #1
    sta ARGS+1
    lda partyPosition+0
    sta ARGS+2
    lda partyPosition+1
    sta ARGS+3
    lda SPELLCASTER_SUBPOS
    sta ARGS+4
    lda partyDirection
    sta ARGS+5
    jsr spells_throwSpell
    rts
  .endproc

  .proc hit
    dice 2,4,0
    ldy #Thrown::caster
    lax (CURRENT_THROWN),y
    jsrf gameState_getSpellCasterDamageMultiplier
    lda ARGS+0
    sta NUM1+0
    lda #0
    sta NUM1+1
    lda #$56
    sta NUM2
    jsr multiply
    tya
    jmp DefaultSpellHandler::defaultHit
  .endproc
.endproc



.proc DispelMagicSpellHandler
  jmp cast
  jmp DefaultSpellHandler::hit
  jmp DefaultSpellHandler::expired

  .proc cast
    lda #1
    sta textColor
    ldx #<text1
    ldy #>text1
    jsr text_writeNullString

    lda AFFECTED_MEMBER+0
    sta CURRENT_PARTYMEMBER+0
    lda AFFECTED_MEMBER+1
    sta CURRENT_PARTYMEMBER+1
    jsrf printMemberName

    ldx #<text2
    ldy #>text2
    jsr text_writeNullString

    ldx #0
    :
      txa
      pha

      clc
      adc #PartyMember::timeoutActions
      tay
      lda (CURRENT_PARTYMEMBER),y
      bpl :+
      beq :+
        eor #$ff
        clc
        adc #1
        tax
        ldy #SpellType::expiredFunc
        jsr spells_callSpellHandler
        pla
        pha
        clc
        adc #PartyMember::timeoutActions
        tay
        lda #0
        sta (CURRENT_PARTYMEMBER),y
        pla
        pha
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
      :

      pla
      tax
      inx
      cpx #10
    bne :--

    ldy #PartyMember::activeSpells+0
    lda (CURRENT_PARTYMEMBER),y
    and #(SPELL_INVISIBLE^$ff)
    sta (CURRENT_PARTYMEMBER),y

    ldy #PartyMember::field_DF
    lda (CURRENT_PARTYMEMBER),y
    bmi :+
    beq :+
      jsr ArmorSpellHandler+6
    :

    ldy #PartyMember::field_DF
    lda #0
    sta (CURRENT_PARTYMEMBER),y

    ldy #PartyMember::activeSpells+0
    sta (CURRENT_PARTYMEMBER),y
    iny
    sta (CURRENT_PARTYMEMBER),y

    ldx AFFECTED_MEMBER_INDEX
    inc partyMemberStatsChanged,x
    
    rts
    text1: .byte "Alle Zauber auf ",0
    text2: .byte " wurden abgewehrt.",$a,0
  .endproc
.endproc



.proc FireBallSpellHandler
  jmp cast
  jmp hit
  jmp DefaultSpellHandler::expired

  .proc cast
    lda SPELLBOOK_MEMBER_INDEX
    sta ARGS+0
    lda #2
    sta ARGS+1
    lda partyPosition+0
    sta ARGS+2
    lda partyPosition+1
    sta ARGS+3
    lda SPELLCASTER_SUBPOS
    sta ARGS+4
    lda partyDirection
    sta ARGS+5
    jsr spells_throwSpell
    rts
  .endproc

  .proc hit
    dice 1,6,0
    ldy #Thrown::caster
    lax (CURRENT_THROWN),y
    jsrf gameState_getSpellCasterDamageMultiplier
    lda ARGS+0
    jmp DefaultSpellHandler::defaultHit
  .endproc
.endproc



.proc FlameArrowSpellHandler
  jmp cast
  jmp hit
  jmp DefaultSpellHandler::expired

  .proc cast
    lda SPELLBOOK_MEMBER_INDEX
    sta ARGS+0
    lda #3
    sta ARGS+1
    lda partyPosition+0
    sta ARGS+2
    lda partyPosition+1
    sta ARGS+3
    lda SPELLCASTER_SUBPOS
    sta ARGS+4
    lda partyDirection
    sta ARGS+5
    jsr spells_throwSpell
    rts
  .endproc

  .proc hit
    dice 5,6,0
    ldy #Thrown::caster
    lax (CURRENT_THROWN),y
    jsrf gameState_getSpellCasterDamageMultiplier
    lda ARGS+0
    jmp DefaultSpellHandler::defaultHit
  .endproc
.endproc



.proc HasteSpellHandler
  jmp cast
  jmp DefaultSpellHandler::hit
  jmp expired

  .proc cast
    ldx #0
    :
      stx AFFECTED_MEMBER_INDEX
      lda partyMembers_lo,x
      sta AFFECTED_MEMBER+0
      lda partyMembers_hi,x
      sta AFFECTED_MEMBER+1

      ldy #PartyMember::status
      lda (AFFECTED_MEMBER),y
      and #1
      beq :+
        ldy #PartyMember::activeSpells+0
        lda (AFFECTED_MEMBER),y
        ora #SPELL_HASTE
        sta (AFFECTED_MEMBER),y
        txa
        pha
        setSpellTimeout castSpellIndex, 3, 546, 1, 1
        pla
        tax
      :
      inc partyMemberStatsChanged,x
      inx
      cpx #6
    bne :--
    rts
  .endproc

  .proc expired
    ldy #PartyMember::activeSpells+0
    lda (AFFECTED_MEMBER),y
    and #(SPELL_HASTE^$ff)
    sta (AFFECTED_MEMBER),y
    ldx AFFECTED_MEMBER_INDEX
    inc partyMemberStatsChanged,x
    rts
  .endproc
.endproc



.proc HoldPersonSpellHandler
  jmp cast
  jmp hit
  jmp DefaultSpellHandler::expired

  .proc cast
    lda SPELLBOOK_MEMBER_INDEX
    sta ARGS+0
    lda #4
    sta ARGS+1
    lda partyPosition+0
    sta ARGS+2
    lda partyPosition+1
    sta ARGS+3
    lda SPELLCASTER_SUBPOS
    sta ARGS+4
    lda partyDirection
    sta ARGS+5
    jsr spells_throwSpell   
    rts
  .endproc

  .proc hit
    ldy #Thrown::position
    lda (CURRENT_THROWN),y
    sta ARGS+0
    iny
    lda (CURRENT_THROWN),y
    sta ARGS+1
    ldy #Thrown::subpos
    lda (CURRENT_THROWN),y
    sta ARGS+2
    ldy #Thrown::direction
    lda (CURRENT_THROWN),y
    sta ARGS+3
    lda #1
    sta ARGS+4
    lda #1
    sta ARGS+5
    jsrf gameState_getMonstersOnPosition
    cpy #0
    bne :+
      jmp return
    :
    sty nbrMonsters

    ldy #0
    :
      sty currentMonsterIndex
      ldx MONSTERS_ON_LOC,y

      jsrf gameState_checkMonsterSpellImmunity
      bcs continue
        ldy monster_typeIndex,x
        lda monsterType_attackBits,y
        and #MONSTER_ATTACKBITS_ISPERSON
        beq continue
          lda #MONSTER_STATE_ISHIT
          sta monster_phase,x
          lda #15
          sta monster_pause,x
      continue:
      ldy currentMonsterIndex
      iny
      cpy nbrMonsters
    bne :-

    return:
    sec
    rts
  .endproc
.endproc



.proc Invisibility10SpellHandler
  jmp cast
  jmp DefaultSpellHandler::hit
  jmp InvisibilitySpellHandler::expired

  .proc cast
    ldx #0
    :
      stx AFFECTED_MEMBER_INDEX
      lda partyMembers_lo,x
      sta AFFECTED_MEMBER+0
      lda partyMembers_hi,x
      sta AFFECTED_MEMBER+1

      ldy #PartyMember::status
      lda (AFFECTED_MEMBER),y
      and #1
      beq :+
        ldy #PartyMember::activeSpells+0
        lda (AFFECTED_MEMBER),y
        ora #SPELL_INVISIBLE
        sta (AFFECTED_MEMBER),y
      :
      inc partyMemberStatsChanged,x
      inx
      cpx #6
    bne :--
    rts
  .endproc
.endproc



.proc LightningBoltSpellHandler
  jmp cast
  jmp hit
  jmp DefaultSpellHandler::expired

  .proc cast
    lda SPELLBOOK_MEMBER_INDEX
    sta ARGS+0
    lda #5
    sta ARGS+1
    lda partyPosition+0
    sta ARGS+2
    lda partyPosition+1
    sta ARGS+3
    lda SPELLCASTER_SUBPOS
    sta ARGS+4
    lda partyDirection
    sta ARGS+5
    jsr spells_throwSpell
    rts
  .endproc

  .proc hit
    dice 1,6,0
    ldy #Thrown::caster
    lax (CURRENT_THROWN),y
    jsrf gameState_getSpellCasterDamageMultiplier
    lda ARGS+0
    jmp DefaultSpellHandler::defaultHit
  .endproc
.endproc



.proc VampiricTouchSpellHandler
  jmp cast
  jmp DefaultSpellHandler::hit
  jmp expired

  .proc cast
    ldx AFFECTED_MEMBER_INDEX
    jsrf gameState_getSpellCasterDamageMultiplier
    lda ARGS+0
    lsr
    pha

    lda #0
    sta ARGS+0
    sta ARGS+1
    sta ARGS+2
    sta ARGS+3
    sta ARGS+4
    lda #$f
    sta ARGS+5
    pla
    sta ARGS+6
    lda #6
    sta ARGS+7
    lda #0
    sta ARGS+8
    lda #ITEMUSAGE_MELEE_WEAPONS
    sta ARGS+9
    jsrf items_createCustomItemType

    lda #ITEM_FLAGS_REGAINHP|ITEM_FLAGS_CONSUMABLE
    sta ARGS+0
    lda #$53
    sta ARGS+1
    lda #$0
    sta ARGS+2
    stx ARGS+3
    jsrf items_createCustomItem

    getFreeHand
    lda CUR_ITEM+0
    sta (AFFECTED_MEMBER),y
    iny
    lda CUR_ITEM+1
    sta (AFFECTED_MEMBER),y

    setSpellTimeout castSpellIndex, 0, 546, 1, 1
    ldx AFFECTED_MEMBER_INDEX
    inc partyMemberStatsChanged,x

    rts
  .endproc

  .proc expired
    sec
    ldy #PartyMember::hpCurrent+0
    lda (AFFECTED_MEMBER),y
    ldy #PartyMember::hp+0
    sbc (AFFECTED_MEMBER),y
    ldy #PartyMember::hpCurrent+1
    lda (AFFECTED_MEMBER),y
    ldy #PartyMember::hp+1
    sbc (AFFECTED_MEMBER),y
    bmi :+
      ldy #PartyMember::hp+0
      lda (AFFECTED_MEMBER),y
      ldy #PartyMember::hpCurrent+0
      sta (AFFECTED_MEMBER),y
      ldy #PartyMember::hp+1
      lda (AFFECTED_MEMBER),y
      ldy #PartyMember::hpCurrent+1
      sta (AFFECTED_MEMBER),y
    :
    jmp ShockingGraspSpellHandler::expired
  .endproc
.endproc



.proc FearSpellHandler
  jmp cast
  jmp DefaultSpellHandler::hit
  jmp DefaultSpellHandler::expired

  .proc cast
    jsrf gameState_drawBigMagicalGlimmerAnimation

    lda partyPosition+0
    sta ARGS+0
    lda partyPosition+1
    sta ARGS+1
    lda partyDirection
    sta ARGS+2
    jsrf gameState_getForwardPosition

    ldx #0
    loop:
      lda monster_position_lo,x
      cmp ARGS+0
      bne :+
      lda monster_position_hi,x
      cmp ARGS+1
      bne :+
      ldy monster_typeIndex,x
      lda monsterType_attackBits,y
      and #3
      beq :+
      jsrf gameState_checkMonsterSpellImmunity
      bcs :+
      jsrf gameState_shouldReduceDamageOnMonster
      bcs :+
        jsrf gameState_fearMonster
      :
      inx
      cpx #30
    bne loop

    rts
  .endproc
.endproc



.proc IceStormSpellHandler
  jmp cast
  jmp hit
  jmp DefaultSpellHandler::expired

  .proc cast
    lda SPELLBOOK_MEMBER_INDEX
    sta ARGS+0
    lda #6
    sta ARGS+1
    lda partyPosition+0
    sta ARGS+2
    lda partyPosition+1
    sta ARGS+3
    lda SPELLCASTER_SUBPOS
    sta ARGS+4
    lda partyDirection
    sta ARGS+5
    jsr spells_throwSpell
    rts
  .endproc

  .proc hit
    dice 1,6,0
    ldy #Thrown::caster
    lax (CURRENT_THROWN),y
    jsrf gameState_getSpellCasterDamageMultiplier
    lda ARGS+0
    jsr DefaultSpellHandler::defaultHit
    bcc miss
      ldx #0
      :
        txa
        pha

        ldy #Thrown::position
        clc
        lda (CURRENT_THROWN),y
        pha
        adc iceStormBlastOffsets_lo,x
        and #<$3ff
        sta (CURRENT_THROWN),y
        iny
        lda (CURRENT_THROWN),y
        pha
        adc iceStormBlastOffsets_hi,x
        and #>$3ff
        sta (CURRENT_THROWN),y

        dice 1,6,0
        ldy #Thrown::caster
        lax (CURRENT_THROWN),y
        jsrf gameState_getSpellCasterDamageMultiplier
        lda ARGS+0
        jsr DefaultSpellHandler::defaultHit

        ldy #Thrown::position+1
        pla
        sta (CURRENT_THROWN),y
        dey
        pla
        sta (CURRENT_THROWN),y

        pla
        tax
        inx
        cpx #4
      bne :-
      sec
    miss:
    rts

iceStormBlastOffsets_lo: .byte <-32, <32, <1, <-1
iceStormBlastOffsets_hi: .byte >-32, >32, >1, >-1
  .endproc
.endproc



.proc StoneSkinSpellHandler
  jmp cast
  jmp DefaultSpellHandler::hit
  jmp DefaultSpellHandler::expired

  .proc cast
    ldx SPELLBOOK_MEMBER_INDEX
    jsrf gameState_getSpellCasterDamageMultiplier

    dice 1,4,0
    jsrf rollDice

    lda ARGS+0
    lsr
    clc
    adc DICE_RESULT+0

    ldy #PartyMember::attackSaves
    sta (AFFECTED_MEMBER),y

    rts
  .endproc
.endproc



.proc ConeOfColdSpellHandler
  jmp cast
  jmp DefaultSpellHandler::hit
  jmp DefaultSpellHandler::expired

  .proc cast
    .pushseg
    .segment "BSS"
      sdm: .res 1
    .popseg

    jsrf renderConeOfCold

    ldx SPELLBOOK_MEMBER_INDEX
    jsrf gameState_getSpellCasterDamageMultiplier
    lda ARGS+0
    sta sdm

    ldx partyDirection
    lda explosionTables_lo,x
    sta TMP2+0
    lda explosionTables_hi,x
    sta TMP2+1

    ldy #0
    loop:
      clc
      lda partyPosition+0
      adc (TMP2),y
      sta ARGS+0
      iny
      lda partyPosition+1
      adc (TMP2),y
      and #>$3ff
      sta ARGS+1
      iny
      tya
      pha

      lda #4
      sta ARGS+2
      lda partyDirection
      sta ARGS+3
      lda #1
      sta ARGS+4
      sta ARGS+5
      jsrf gameState_getMonstersOnPosition
      cpy #0
      beq noMonstersFound
        sty nbrMonsters
        ldy #0
        nextMonster:
          sty currentMonsterIndex

          ldx MONSTERS_ON_LOC,y
          txa
          pha
          jsrf gameState_checkMonsterSpellImmunity
          pla
          tax
          bcs continue

            lda #1
            sta DICE_ROLLS
            lda #4
            sta DICE_SIDES
            lda sdm
            sta DICE_BASE
            jsrf rollDice
            lda DICE_RESULT+0
            sta ARGS+0
            jsrf gameState_giveMonsterDamage
          continue:
          ldy currentMonsterIndex
          iny
          cpy nbrMonsters
        bne nextMonster
      noMonstersFound:
      pla
      tay
      cpy #7*2
      beq :+
        rts
      :
    jmp loop
  .endproc

explosionDeltasNorth: .word (-32)&$ffff, (-64)&$ffff, (-63)&$ffff, (-65)&$ffff, (-96)&$ffff, (-97)&$ffff, (-95)&$ffff
explosionDeltasEast:  .word (  1)&$ffff, (  2)&$ffff, (-30)&$ffff, ( 34)&$ffff, (  3)&$ffff, (-29)&$ffff, ( 35)&$ffff
explosionDeltasSouth: .word ( 32)&$ffff, ( 64)&$ffff, ( 63)&$ffff, ( 65)&$ffff, ( 96)&$ffff, ( 95)&$ffff, ( 97)&$ffff
explosionDeltasWest:  .word ( -1)&$ffff, ( -2)&$ffff, ( 30)&$ffff, (-34)&$ffff, ( -3)&$ffff, ( 29)&$ffff, (-35)&$ffff

explosionTables_lo:
  .byte <explosionDeltasNorth
  .byte <explosionDeltasEast
  .byte <explosionDeltasSouth
  .byte <explosionDeltasWest

explosionTables_hi:
  .byte >explosionDeltasNorth
  .byte >explosionDeltasEast
  .byte >explosionDeltasSouth
  .byte >explosionDeltasWest

.endproc



.proc HoldMonsterSpellHandler
  jmp cast
  jmp hit
  jmp DefaultSpellHandler::expired

  .proc cast
    lda SPELLBOOK_MEMBER_INDEX
    sta ARGS+0
    lda #7
    sta ARGS+1
    lda partyPosition+0
    sta ARGS+2
    lda partyPosition+1
    sta ARGS+3
    lda SPELLCASTER_SUBPOS
    sta ARGS+4
    lda partyDirection
    sta ARGS+5
    jsr spells_throwSpell   
    rts
  .endproc

  .proc hit
    ldy #Thrown::position
    lda (CURRENT_THROWN),y
    sta ARGS+0
    iny
    lda (CURRENT_THROWN),y
    sta ARGS+1
    ldy #Thrown::subpos
    lda (CURRENT_THROWN),y
    sta ARGS+2
    ldy #Thrown::direction
    lda (CURRENT_THROWN),y
    sta ARGS+3
    lda #1
    sta ARGS+4
    lda #1
    sta ARGS+5
    jsrf gameState_getMonstersOnPosition
    cpy #0
    bne :+
      jmp return
    :
    sty nbrMonsters

    ldy #0
    :
      sty currentMonsterIndex
      ldx MONSTERS_ON_LOC,y

      jsrf gameState_checkMonsterSpellImmunity
      bcs continue
        ldy monster_typeIndex,x
        lda monsterType_attackBits,y
        and #MONSTER_ATTACKBITS_ISMONSTER
        beq continue
          lda #MONSTER_STATE_ISHIT
          sta monster_phase,x
          lda #15
          sta monster_pause,x
      continue:
      ldy currentMonsterIndex
      iny
      cpy nbrMonsters
    bne :-

    return:
    sec
    rts
  .endproc
.endproc



.proc BlessSpellHandler
  jmp cast
  jmp DefaultSpellHandler::hit
  jmp expired

  .proc cast
    lda partySpells+0
    and #SPELL_BLESS
    beq :+
      lda #1
      sta textColor
      ldx #<msg
      ldy #>msg
      jsr text_writeNullString
      rts
      msg: .byte "Das Team ist schon gesegnet!",$a,0
    :
    ora #SPELL_BLESS
    sta partySpells+0
    setSpellTimeout castSpellIndex, 6, 546, 0, 1
    rts
  .endproc

  .proc expired
    lda partySpells+0
    and #(SPELL_BLESS^$ff)
    sta partySpells+0
    rts
  .endproc
.endproc



.proc CureLightWoundsSpellHandler
  jmp cast
  jmp DefaultSpellHandler::hit
  jmp DefaultSpellHandler::expired

  .proc cast
    dice 1,8,0
    jsrf rollDice
    ldx AFFECTED_MEMBER_INDEX
    lda DICE_RESULT+0
    sta ARGS+0
    lda DICE_RESULT+1
    sta ARGS+1
    jsrf gameState_healMember
    rts
  .endproc
.endproc



.proc CauseLightWoundsSpellHandler
  jmp cast
  jmp DefaultSpellHandler::hit
  jmp DefaultSpellHandler::expired

  .proc cast
    dice 1,8,0
    jsrf gameState_causeWounds
    rts
  .endproc
.endproc



.proc ProtectionFromEvilSpellHandler
  jmp cast
  jmp DefaultSpellHandler::hit
  jmp expired

  .proc cast
    ldy #PartyMember::activeSpells+1
    lda (AFFECTED_MEMBER),y
    ora #<SPELL_PROTECTION_FROM_EVIL
    sta (AFFECTED_MEMBER),y
    
    setSpellTimeout castSpellIndex, 0, 546, 3, 1

    ldx AFFECTED_MEMBER_INDEX
    inc partyMemberStatsChanged,x

    rts
  .endproc

  .proc expired
    ldy #PartyMember::activeSpells+1
    lda (AFFECTED_MEMBER),y
    and #((>SPELL_PROTECTION_FROM_EVIL)^$ff)
    sta (AFFECTED_MEMBER),y
    ldx AFFECTED_MEMBER_INDEX
    inc partyMemberStatsChanged,x
    rts
  .endproc
.endproc



.proc AidSpellHandler
  jmp cast
  jmp DefaultSpellHandler::hit
  jmp expired

  .proc cast
    ldx AFFECTED_MEMBER_INDEX
    ldy #IS_ACTIVE|IS_ALIVE
    jsrf gameState_getMemberStatus
    bcs :+
      lda #1
      sta textColor
      ldx #<msg
      ldy #>msg
      jsr text_writeNullString
      rts
      msg: .byte "Der Hilfszauber schl",125,"gt fehl!",$a,0
    :

    ldy #PartyMember::activeSpells+0
    lda (CURRENT_PARTYMEMBER),y
    ora #SPELL_AID
    sta (CURRENT_PARTYMEMBER),y

    dice 1,8,0
    jsrf rollDice

    lda DICE_RESULT+0
    ldy #PartyMember::temporaryHP
    sta (CURRENT_PARTYMEMBER),y

    ldy #PartyMember::hpCurrent
    clc
    lda DICE_RESULT+0
    adc (CURRENT_PARTYMEMBER),y
    sta (CURRENT_PARTYMEMBER),y
    iny
    lda (CURRENT_PARTYMEMBER),y
    adc #0
    sta (CURRENT_PARTYMEMBER),y

    setSpellTimeout castSpellIndex, 1, 546, 0, 1

    ldx AFFECTED_MEMBER_INDEX
    inc partyMemberStatsChanged,x

    rts
  .endproc

  .proc expired
    sec
    ldy #PartyMember::hpCurrent
    lda (AFFECTED_MEMBER),y
    ldy #PartyMember::temporaryHP
    sbc (AFFECTED_MEMBER),y
    ldy #PartyMember::hpCurrent
    sta (AFFECTED_MEMBER),y
    iny
    lda (AFFECTED_MEMBER),y
    sbc #0
    sta (AFFECTED_MEMBER),y

    ldy #PartyMember::temporaryHP
    lda #0
    sta (AFFECTED_MEMBER),y

    ldy #PartyMember::activeSpells+0
    lda (AFFECTED_MEMBER),y
    and #(SPELL_AID^$ff)
    sta (AFFECTED_MEMBER),y

    ldx AFFECTED_MEMBER_INDEX
    inc partyMemberStatsChanged,x

    rts
  .endproc
.endproc



.proc FlameBladeSpellHandler
  jmp cast
  jmp DefaultSpellHandler::hit
  jmp ShockingGraspSpellHandler::expired

  .proc cast
    lda #0
    sta ARGS+0
    sta ARGS+1
    sta ARGS+2
    sta ARGS+3
    sta ARGS+4
    lda #$f
    sta ARGS+5
    lda #1
    sta ARGS+6
    lda #4
    sta ARGS+7
    lda #4
    sta ARGS+8
    lda #ITEMUSAGE_MELEE_WEAPONS
    sta ARGS+9
    jsrf items_createCustomItemType

    lda #0
    sta ARGS+0
    lda #$54
    sta ARGS+1
    lda #$0
    sta ARGS+2
    stx ARGS+3
    jsrf items_createCustomItem

    getFreeHand
    lda CUR_ITEM+0
    sta (AFFECTED_MEMBER),y
    iny
    lda CUR_ITEM+1
    sta (AFFECTED_MEMBER),y

    jsrf gameState_getLevelACModifier
    lda ARGS+0
    lsr
    adc #0 ;ceil() to avoid 0
    setSpellTimeout2 castSpellIndex, 0, 546, 1
    ldx AFFECTED_MEMBER_INDEX
    inc partyMemberStatsChanged,x

    rts
  .endproc
.endproc



.proc SlowPoisonSpellHandler
  jmp cast
  jmp DefaultSpellHandler::hit
  jmp expired

  .proc cast
    ldy #PartyMember::status
    lda (AFFECTED_MEMBER),y
    and #PARTYMEMBER_STATUS_POISONED
    bne :+
      lda #1
      sta textColor
      ldx #<msg
      ldy #>msg
      jsr text_writeNullString
      rts
      msg: .byte "Keine Wirkung!",$a,0
    :

    ldy #PartyMember::activeSpells+0
    lda (AFFECTED_MEMBER),y
    ora #SPELL_SLOW_POISON
    sta (AFFECTED_MEMBER),y

    setSpellTimeout castSpellIndex, 1, $7ff8, 0, 1

    ldx AFFECTED_MEMBER_INDEX
    inc partyMemberStatsChanged,x

    rts
  .endproc

  .proc expired
    ldy #PartyMember::activeSpells+0
    lda (AFFECTED_MEMBER),y
    and #(SPELL_SLOW_POISON^$ff)
    sta (AFFECTED_MEMBER),y

    ldx AFFECTED_MEMBER_INDEX
    inc partyMemberStatsChanged,x

    rts
  .endproc
.endproc



.proc CreateFoodSpellHandler
  jmp cast
  jmp DefaultSpellHandler::hit
  jmp DefaultSpellHandler::expired

  .proc cast
    ldx #0
    :
      lda partyMembers_lo,x
      sta CURRENT_PARTYMEMBER+0
      lda partyMembers_hi,x
      sta CURRENT_PARTYMEMBER+1
      ldy #PartyMember::food
      lda #100
      sta (CURRENT_PARTYMEMBER),y
      inc partyMemberStatsChanged,x
      inx
      cpx #6
    bne :-
    rts
  .endproc
.endproc



.proc MagicalVestmentSpellHandler
  jmp cast
  jmp DefaultSpellHandler::hit
  jmp expired

  .proc cast
    ldy #PartyMember::activeSpells+1
    lda (AFFECTED_MEMBER),y
    ora #<SPELL_MAGICAL_VESTMENT
    sta (AFFECTED_MEMBER),y
    
    lda AFFECTED_MEMBER+0
    sta CURRENT_PARTYMEMBER+0
    lda AFFECTED_MEMBER+1
    sta CURRENT_PARTYMEMBER+1
    lda AFFECTED_MEMBER_INDEX
    sta CURRENT_PARTYMEMBER_INDEX
    jsrf calculateMemberAC

    setSpellTimeout castSpellIndex, 0, 546, 5, 1

    ldx AFFECTED_MEMBER_INDEX
    inc partyMemberStatsChanged,x

    rts
  .endproc

  .proc expired
    ldy #PartyMember::activeSpells+1
    lda (AFFECTED_MEMBER),y
    and #((>SPELL_MAGICAL_VESTMENT)^$ff)
    sta (AFFECTED_MEMBER),y
    lda AFFECTED_MEMBER_INDEX

    lda AFFECTED_MEMBER+0
    sta CURRENT_PARTYMEMBER+0
    lda AFFECTED_MEMBER+1
    sta CURRENT_PARTYMEMBER+1
    lda AFFECTED_MEMBER_INDEX
    sta CURRENT_PARTYMEMBER_INDEX
    jsrf calculateMemberAC

    ldx AFFECTED_MEMBER_INDEX
    inc partyMemberStatsChanged,x
    rts
  .endproc
.endproc



.proc PrayerSpellHandler
  jmp cast
  jmp DefaultSpellHandler::hit
  jmp expired

  .proc cast
    lda partySpells+0
    ora #SPELL_PRAYER
    sta partySpells+0
    setSpellTimeout castSpellIndex, 0, 546, 2, 1
    jsrf gui_refreshWholeParty
    rts
  .endproc

  .proc expired
    lda partySpells+0
    and #(SPELL_PRAYER^$ff)
    sta partySpells+0
    jsrf gui_refreshWholeParty
    rts
  .endproc
.endproc



.proc RemoveParalysisSpellHandler
  jmp cast
  jmp DefaultSpellHandler::hit
  jmp DefaultSpellHandler::expired

  .proc cast
    lda #4
    sta TMP
    ldx #0
    :
      lda partyMembers_lo,x
      sta CURRENT_PARTYMEMBER+0
      lda partyMembers_hi,x
      sta CURRENT_PARTYMEMBER+1
      ldy #PartyMember::status
      lda (CURRENT_PARTYMEMBER),y
      and #PARTYMEMBER_STATUS_PARALYZED
      beq :+
        lda (CURRENT_PARTYMEMBER),y
        and #(PARTYMEMBER_STATUS_PARALYZED^$ff)
        sta (CURRENT_PARTYMEMBER),y
        inc partyMemberStatsChanged,x
        dec TMP
        bne :+
          rts
      :
      inx
      cpx #6
    bne :--
    rts
  .endproc
.endproc



.proc CureSeriousWoundsSpellHandler
  jmp cast
  jmp DefaultSpellHandler::hit
  jmp DefaultSpellHandler::expired

  .proc cast
    dice 2,8,1
    jsrf rollDice
    ldx AFFECTED_MEMBER_INDEX
    lda DICE_RESULT+0
    sta ARGS+0
    lda DICE_RESULT+1
    sta ARGS+1
    jsrf gameState_healMember
    rts
  .endproc
.endproc



.proc CauseSeriousWoundsSpellHandler
  jmp cast
  jmp DefaultSpellHandler::hit
  jmp DefaultSpellHandler::expired

  .proc cast
    dice 2,8,1
    jsrf gameState_causeWounds
    rts
  .endproc
.endproc



.proc NeutralizePoisonSpellHandler
  jmp cast
  jmp DefaultSpellHandler::hit
  jmp DefaultSpellHandler::expired

  .proc cast
    ldy #PartyMember::status
    lda (AFFECTED_MEMBER),y
    beq :+
    and #PARTYMEMBER_STATUS_POISONED
    bne :++
    :
      lda #1
      sta textColor
      ldx #<msg
      ldy #>msg
      jsr text_writeNullString
      rts
      msg: .byte "Keine Wirkung!",$a,0
    :

    ldx AFFECTED_MEMBER_INDEX
    inc partyMemberStatsChanged,x
    lda AFFECTED_MEMBER+0
    sta CURRENT_PARTYMEMBER+0
    lda AFFECTED_MEMBER+1
    sta CURRENT_PARTYMEMBER+1
    jsrf neutralizePoison

    rts
  .endproc
.endproc



.proc ProtectionFromEvil10SpellHandler
  jmp cast
  jmp DefaultSpellHandler::hit
  jmp expired

  .proc cast
    ldx #0
    :
      stx AFFECTED_MEMBER_INDEX
      lda partyMembers_lo,x
      sta AFFECTED_MEMBER+0
      lda partyMembers_hi,x
      sta AFFECTED_MEMBER+1

      ldy #PartyMember::status
      lda (AFFECTED_MEMBER),y
      and #1
      beq :+
        ldy #PartyMember::activeSpells+1
        lda (AFFECTED_MEMBER),y
        ora #<SPELL_PROTECTION_FROM_EVIL
        sta (AFFECTED_MEMBER),y
      :
      inc partyMemberStatsChanged,x
      inx
      cpx #6
    bne :--
    
    ldx SPELLBOOK_MEMBER_INDEX
    stx AFFECTED_MEMBER_INDEX
    jsrf gameState_getLevelACModifier
    lda ARGS+0
    setSpellTimeout2 castSpellIndex, 0, 546, 1

    rts
  .endproc

  .proc expired
    ldx #0
    :
      stx AFFECTED_MEMBER_INDEX
      lda partyMembers_lo,x
      sta AFFECTED_MEMBER+0
      lda partyMembers_hi,x
      sta AFFECTED_MEMBER+1

      ldy #PartyMember::activeSpells+1
      lda (AFFECTED_MEMBER),y
      and #((<SPELL_PROTECTION_FROM_EVIL)^$ff)
      sta (AFFECTED_MEMBER),y

      inc partyMemberStatsChanged,x
      inx
      cpx #6
    bne :-
    rts
  .endproc
.endproc



.proc CureCriticalWoundsSpellHandler
  jmp cast
  jmp DefaultSpellHandler::hit
  jmp DefaultSpellHandler::expired

  .proc cast
    dice 3,8,3
    jsrf rollDice
    ldx AFFECTED_MEMBER_INDEX
    lda DICE_RESULT+0
    sta ARGS+0
    lda DICE_RESULT+1
    sta ARGS+1
    jsrf gameState_healMember
    rts
  .endproc
.endproc



.proc CauseCriticalWoundsSpellHandler
  jmp cast
  jmp DefaultSpellHandler::hit
  jmp DefaultSpellHandler::expired

  .proc cast
    dice 3,8,3
    jsrf gameState_causeWounds
    rts
  .endproc
.endproc

.proc FlameStrikeSpellHandler
  jmp cast
  jmp hit
  jmp DefaultSpellHandler::expired

  .proc cast
    lda SPELLBOOK_MEMBER_INDEX
    sta ARGS+0
    lda #8
    sta ARGS+1
    lda partyPosition+0
    sta ARGS+2
    lda partyPosition+1
    sta ARGS+3
    lda SPELLCASTER_SUBPOS
    sta ARGS+4
    lda partyDirection
    sta ARGS+5
    jsr spells_throwSpell
    rts
  .endproc

  .proc hit
    dice 6,8,0
    jmp DefaultSpellHandler::defaultHit
  .endproc
.endproc

.proc RaiseDeadSpellHandler
  jmp cast
  jmp DefaultSpellHandler::hit
  jmp DefaultSpellHandler::expired

  .proc cast
    ldy #PartyMember::hpCurrent
    lda (AFFECTED_MEMBER),y
    cmp #<-10
    bne noEffect
    iny
    lda (AFFECTED_MEMBER),y
    cmp #>-10
    bne noEffect
    ldy #PartyMember::race
    lda (AFFECTED_MEMBER),y
    lsr
    cmp #RACE_ELF
    beq noEffect
      ldy #PartyMember::hpCurrent
      lda #<1
      sta (AFFECTED_MEMBER),y
      iny
      lda #>1
      sta (AFFECTED_MEMBER),y
      ldx AFFECTED_MEMBER_INDEX
      inc partyMemberStatsChanged,x
      rts
    noEffect:
    lda #1
    sta textColor
    ldx #<msg
    ldy #>msg
    jsr text_writeNullString
    rts

msg: .byte "Keine Wirkung!.",$a,0
  .endproc
.endproc



.proc LayOnHandsSpellHandler
  jmp cast
  jmp DefaultSpellHandler::hit
  jmp DefaultSpellHandler::expired

  .proc cast
    ldx SPELLBOOK_MEMBER_INDEX
    lda partyMembers_lo,x
    sta CURRENT_PARTYMEMBER+0
    lda partyMembers_hi,x
    sta CURRENT_PARTYMEMBER+1
    ldy #PartyMember::level
    lda (CURRENT_PARTYMEMBER),y
    asl
    sta ARGS+0
    lda #0
    sta ARGS+1 

    ldx AFFECTED_MEMBER_INDEX
    jsrf gameState_healMember
    rts
  .endproc
.endproc



.proc KuotoaSpellHandler
  jmp DefaultSpellHandler::cast
  jmp hit
  jmp DefaultSpellHandler::expired

  .proc hit
    dice 0,0,12
    lda #1
    jmp DefaultSpellHandler::defaultHit
  .endproc
.endproc



.proc XanatharDisintegrationSpellHandler
  jmp DefaultSpellHandler::cast
  jmp hit
  jmp DefaultSpellHandler::expired

  .proc hit
    .pushseg
    .segment "BSS"
      memberBias: .res 1
    .popseg
    ldy #Thrown::position
    lda (CURRENT_THROWN),y
    cmp partyPosition+0
    beq :+
    iny
    lda (CURRENT_THROWN),y
    cmp partyPosition+1
    beq :+
      clc
      rts
    :

    dice 1,6,0
    jsrf rollDice
    lda DICE_RESULT+0
    sta memberBias

    ldx #0
    loop:
      txa
      pha
      adc memberBias
      cmp #6
      bcc :+
        sbc #6
      :
      tax
      stx CURRENT_PARTYMEMBER_INDEX

      ldy #IS_ACTIVE|IS_ALIVE
      jsrf gameState_getMemberStatus
      bcs :+
        jmp continue
      :

      jsrf gameState_getMemberDamageReductionModifier
      bcs continue

        lda CURRENT_PARTYMEMBER_INDEX
        sta ARGS+0
        lda #<100
        sta ARGS+1
        lda #>100
        sta ARGS+2
        jsrf gameState_giveMemberDamage

        lda #1
        sta textColor
        jsrf printMemberName
        ldx #<msg
        ldy #>msg
        jsr text_writeNullString
        pla
        jmp break
        msg: .byte " wurde aufgel",126,"st!",$a,0

      continue:
      pla
      tax
      inx
      cpx #6
      beq break
    jmp loop

break:
    sec
    rts
  .endproc
.endproc



.proc XanatharDeathSpellHandler
  jmp DefaultSpellHandler::cast
  jmp hit
  jmp DefaultSpellHandler::expired

  .proc hit
    lda #1
    sta textColor
    ldx #<msg
    ldy #>msg
    jsr text_writeNullString

    dice 1,4,1
    jsrf rollDice
    lda DICE_RESULT+0
    sta COUNT

    ldx #0
    stx CURRENT_PARTYMEMBER_INDEX
    loop:
      ldx CURRENT_PARTYMEMBER_INDEX
      ldy #IS_ACTIVE|IS_ALIVE
      jsrf gameState_getMemberStatus
      bcc continue
      
      ldy #PartyMember::level
      lda (CURRENT_PARTYMEMBER),y
      cmp #8
      bcs continue

        ldx CURRENT_PARTYMEMBER_INDEX
        lda #<100
        sta ARGS+0
        lda #>100
        sta ARGS+1
        jsrf gameState_giveMemberDamage

      continue:
      inc CURRENT_PARTYMEMBER_INDEX
      dec COUNT
    bne loop

    sec
    rts
    msg: .byte "Das Team wurde von einen Todesbann getroffen!",$a,0
	
  .endproc
.endproc

; x = monster index
; y = spell index
.export spells_throwSpellFromMonster
.proc spells_throwSpellFromMonster
  txa
  pha
  lda #<-1
  sta ARGS+0
  sty ARGS+1
  lda monster_position_lo,x
  sta ARGS+2
  lda monster_position_hi,x
  sta ARGS+3
  lda monster_subpos,x
  sta ARGS+4
  lda monster_direction,x
  sta ARGS+5
  jsr spells_throwSpell
  pla
  tax
  rts
.endproc


.export spells_throwSpell
.proc spells_throwSpell
  caster    = ARGS+0
  spellIndex  = ARGS+1
  position  = ARGS+2
  subpos    = ARGS+4
  direction = ARGS+5

  ; Find free thrown slot
  lda #<thrown
  sta CURRENT_THROWN+0
  lda #>thrown
  sta CURRENT_THROWN+1
  ldy #Thrown::active
  :
    lda (CURRENT_THROWN),y
    beq :+
      clc
      lax CURRENT_THROWN+0
      adc #.sizeof(Thrown)
      sta CURRENT_THROWN+0
      lda CURRENT_THROWN+1
      adc #0
      sta CURRENT_THROWN+1

      cmp #>(thrown+.sizeof(Thrown)*10)
    bne :-
    cpx #<(thrown+.sizeof(Thrown)*10)
  bne :-
    ; return false;
    clc
    jmp return
  :

  ; Activate thrown as spell (2)
  ldy #Thrown::active
  lda #2
  sta (CURRENT_THROWN),y
  ldy #Thrown::movedOnlyWithinSubpos
  lda #1
  sta (CURRENT_THROWN),y

  ; Fetch pointer to spell data into TMP
  lda spellIndex
  asl
  asl
  clc
  adc #<spells
  sta TMP+0
  lda #>spells
  adc #0
  sta TMP+1

  ; Assign all thrown-fields from spell data and input
  ldy #Spell::flags
  lda (TMP),y
  ldy #Thrown::flags
  sta (CURRENT_THROWN),y
  ldy #Thrown::direction
  lda direction
  sta (CURRENT_THROWN),y
  ldy #Spell::range
  lda (TMP),y
  ldy #Thrown::range
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
  ldy #Thrown::itemOrSpellIndex
  lda spellIndex
  sta (CURRENT_THROWN),y
  iny
  lda #0
  sta (CURRENT_THROWN),y
  ldy #Thrown::caster
  lda caster
  sta (CURRENT_THROWN),y

  ldy #Spell::spellGfxIndex
  lda (TMP),y
  ldy #Thrown::spellGfxIndexOrItemType
  sta (CURRENT_THROWN),y

  ; Fetch spell type pointer into TMP
  ldy #Spell::type
  lda (TMP),y
  tay
  lda spellTypes_lo,y
  sta TMP+0
  lda spellTypes_hi,y
  sta TMP+1

  ; Copy spell handler into thrown structure
  ldy #SpellType::hitFunc+0
  lda (TMP),y
  ldy #Thrown::spellHandler+0
  sta (CURRENT_THROWN),y
  ldy #SpellType::hitFunc+1
  lda (TMP),y
  ldy #Thrown::spellHandler+1
  sta (CURRENT_THROWN),y

  inc SHOULDRENDER

  sec

return:
  php
  lda #0
  rol
  sta ARGS+0
  plp
  rts

spells:
  ; 0 - Magic missle
  .byte  4, <-1, $41,  9

  ; 1 - Melfs acid arrow
  .byte 10, <-1, $04,  5

  ; 2 - Fireball
  .byte 13, <-1, $7A,  6

  ; 3 - Flame arrow
  .byte 14, <-1, $0C,  5

  ; 4 - Hold person
  .byte 16, <-1, $4A,  10

  ; 5 - Ligntning bolt
  .byte 18,  6, $38,  7

  ; 6 - Ice storm
  .byte 21,  6, $7A,  8

  ; 7 - Hold monster
  .byte 25, <-1, $4A,  10

  ; 8 - Flame strike
  .byte 47, <-1, $4B,  6

  ; 9 - Kuotoa lightning bolt
  .byte 50, <-1, $09,  7

  ; 10 - Xanathar disintegration spell
  .byte 51, <-1, $09,  10

  ; 11 - Xanathar death spell
  .byte 52, <-1, $09,  10
.endproc

; y = inventorySlot
.export spells_castSpell
.proc spells_castSpell
    _spellIndex = ARGS+0
    .pushseg
    .segment "BSS"
      .export castSpellIndex
      castSpellIndex:
      spellIndex: .res 1
    .popseg
    lda _spellIndex
    sta spellIndex

    sty YREG

    lda #0
    sta timersEnabled

    ldx SPELLBOOK_MEMBER_INDEX
    lda partyMembers_lo,x
    sta CURRENT_PARTYMEMBER+0
    lda partyMembers_hi,x
    sta CURRENT_PARTYMEMBER+1

    ldx spellIndex
    lda spellTypes_lo,x
    sta CURRENT_SPELLTYPE+0
    lda spellTypes_hi,x
    sta CURRENT_SPELLTYPE+1

    ldy #SpellType::usage
    lda (CURRENT_SPELLTYPE),y
    and #SPELLTYPEUSAGE_DEACTIVATEINVISIBILITY
    beq :+
      ldy #PartyMember::activeSpells+0
      lda (CURRENT_PARTYMEMBER),y
      and #(SPELL_INVISIBLE^$ff)
      sta (CURRENT_PARTYMEMBER),y
      ldx SPELLBOOK_MEMBER_INDEX
      inc partyMemberStatsChanged,x
    :

    ; int subpos = partySpellCaster<=3 ? partySpellCaster : partySpellCaster-2;
    lda SPELLBOOK_MEMBER_INDEX
    cmp #4
    bcc :+
      sbc #2
    :
    asl
    asl
    ora partyDirection
    tax
    lda throwSubpos,x
    sta SPELLCASTER_SUBPOS

    ldy #SpellType::usage
    lda (CURRENT_SPELLTYPE),y
    and #SPELLTYPEUSAGE_FREEHANDNEEDED
    bne :+
      jmp freeHandNotNeeded
    :
      ldy #PartyMember::inventory + INVENTORY_LEFT_HAND*2
      lda (CURRENT_PARTYMEMBER),y
      beq :+
      ldy #PartyMember::inventory + INVENTORY_RIGHT_HAND*2
      lda (CURRENT_PARTYMEMBER),y
      beq :+
        lda #1
        sta textColor
        ldx #<freeHandNeeded
        ldy #>freeHandNeeded
        jsr text_writeNullString
        jmp returnFalse
        freeHandNeeded: .byte "Daf",127,"r musst du eine Hand frei haben.",$a,0
      :

      ldy #PartyMember::inventory + INVENTORY_LEFT_HAND*2
      jsr checkActiveHandSpells
      bcs :+
      ldy #PartyMember::inventory + INVENTORY_RIGHT_HAND*2
      jsr checkActiveHandSpells
      bcs :+
      jmp :++
      :
        lda #1
        sta textColor
        ldx #<cantHaveTwo
        ldy #>cantHaveTwo
        jsr text_writeNullString
        jmp returnFalse
        cantHaveTwo: .byte "Du kannst nicht zwei dieser Spr",127,"che aktiv haben.",$a,0
      :
    freeHandNotNeeded:

    lda SPELLCASTER_USING_SCROLL
    beq :+
      ldy YREG
      jsr consumeMagicalItem
      jmp :++
    :
      ; Negate it to tag the spell as used
      ldy #0
      lda (SELECTED_SPELL),y
      eor #$ff
      clc
      adc #1
      sta (SELECTED_SPELL),y
    :

    lda #1
    sta textColor
    jsrf printMemberName
    ldx #<casts
    ldy #>casts
    jsr text_writeNullString
    ldx spellIndex
    jsrf printSpellName
    ldx #<retstr
    ldy #>retstr
    jsr text_writeNullString

    ldy #SpellType::usage
    lda (CURRENT_SPELLTYPE),y
    and #SPELLTYPEUSAGE_ASKWHOM
    beq :+
      jsrf askForSpellReceiver
      jmp returnFalse
    :

    lda SPELLBOOK_MEMBER_INDEX
    sta AFFECTED_MEMBER_INDEX
    lda CURRENT_PARTYMEMBER+0
    sta AFFECTED_MEMBER+0
    lda CURRENT_PARTYMEMBER+1
    sta AFFECTED_MEMBER+1

    ldy #SpellType::soundEffect
    lax (CURRENT_SPELLTYPE),y
    jsrf audio_playSoundEffect

    ldy #SpellType::usage
    lax (CURRENT_SPELLTYPE),y
    and #6
    beq :+
      ldx AFFECTED_MEMBER_INDEX
      jsrf gameState_drawMagicalGlimmerAnimation
      jmp :++
    :
    txa
    and #8
    beq :+
      ldx #<-1
      jsrf gameState_drawMagicalGlimmerAnimation
    :

    ldx spellIndex
    ldy #SpellType::castFunc
    jsr spells_callSpellHandler

    lda SPELLCASTER_USING_SCROLL
    beq :+
      jsr spellCastSetMemberTimeout
    :

returnTrue: sec
    bcs return
returnFalse:  clc
return:   lda #1
    sta timersEnabled
    rts
casts:    .byte " werfen ",0
retstr:   .byte ".",$a,0
.endproc

; x = spellType
; y = handler
.export spells_callSpellHandler
.proc spells_callSpellHandler
    lda spellTypes_lo,x
    sta CURRENT_SPELLTYPE+0
    lda spellTypes_hi,x
    sta CURRENT_SPELLTYPE+1

    lda (CURRENT_SPELLTYPE),y
    sta INDJMP+0
    iny
    lda (CURRENT_SPELLTYPE),y
    sta INDJMP+1
    ora INDJMP+0
    beq :+
      ldx #<.bank(spellBank)
      jsr indirectJsr
    :
    rts
.endproc

; y=inventory offset
; CURRENT_PARTYMEMBER
; return bool in c
.proc checkActiveHandSpells
  iny
  lda (CURRENT_PARTYMEMBER),y
  beq :+
    clc
    rts
  :
  dey
  lda (CURRENT_PARTYMEMBER),y
  cmp #11
  bcc return ;<11
  
  cmp #18
  bcc :+
    clc
    rts
  :
  sec
return: rts
.endproc

.export gameState_spellCastSetMemberTimeout
gameState_spellCastSetMemberTimeout = spellCastSetMemberTimeout
.proc spellCastSetMemberTimeout
    ldx SPELLBOOK_MEMBER_INDEX
    lda partyMembers_lo,x
    sta CURRENT_PARTYMEMBER+0
    lda partyMembers_hi,x
    sta CURRENT_PARTYMEMBER+1

    ldy #PartyMember::disabledHands
    lda (CURRENT_PARTYMEMBER),y
    ora SPELLCASTER_USING_SCROLL ;1/2
    sta (CURRENT_PARTYMEMBER),y

    lda #18
    sta ARGS+0
    lda #0
    sta ARGS+1
    sta ARGS+2
    sta ARGS+3
    clc
    lda SPELLCASTER_USING_SCROLL
    adc #1 ;+2 in the original, but I didn't -- so let's just do +1
    sta ARGS+4
    lda #1
    sta ARGS+5
    jsrf gameState_setMemberTimeout

    lda BACKUP_SPELLBOOK_TYPE
    sta SPELLBOOK_TYPE
    lda BACKUP_SPELLBOOK_MEMBER_INDEX
    sta SPELLBOOK_MEMBER_INDEX
    lda #0
    sta SPELLCASTER_USING_SCROLL

    rts
.endproc

; y=inventorySlot
; CURRENT_PARTYMEMBER
.proc consumeMagicalItem
  sty YREG
  tya
  asl
  clc
  adc #PartyMember::inventory
  tay
  lda (CURRENT_PARTYMEMBER),y
  sta CUR_ITEM+0
  iny
  lda (CURRENT_PARTYMEMBER),y
  sta CUR_ITEM+1
  ldy #Item::type
  jsr lda_CUR_ITEM_y
  cmp #$22
  beq :+
  cmp #$23
  beq :+
  jmp :++
  :
consume:
    lda SPELLBOOK_MEMBER_INDEX
    sta ARGS+0
    lda YREG
    sta ARGS+1
    jsrf gameState_consumeItem
    rts
  :
  cmp #$30
  beq :+
    rts
  :

  ldy #Item::flags
  jsr lda_CUR_ITEM_y
  and #$3f
  cmp #1
  beq consume

  ldy #Item::flags
  jsr lda_CUR_ITEM_y
  sec
  sbc #1
  jsr sta_CUR_ITEM_y 
  rts
.endproc

.proc spellTypes
  ;00
  DefSpellType 0, $00, 0, DefaultSpellHandler
  DefSpellType 1, $5C, 0, ArmorSpellHandler
  DefSpellType 0, $57, 1, BurningHandsSpellHandler
  DefSpellType 8, $5F, 0, DetectMagicSpellHandler

  ;04
  DefSpellType 0, $55, 1, MagicMissileSpellHandler
  DefSpellType 0, $00, 0, DefaultSpellHandler
  DefSpellType 4, $5C, 0, ShieldSpellHandler
  DefSpellType 2, $58, 0, ShockingGraspSpellHandler

  ;08
  DefSpellType 1, $5E, 0, InvisibilitySpellHandler
  DefSpellType 0, $00, 0, DefaultSpellHandler
  DefSpellType 0, $60, 1, MelfsAcidArrowSpellHandler
  DefSpellType 0, $00, 1, DefaultSpellHandler

  ;0c
  DefSpellType 1, $61, 0, DispelMagicSpellHandler
  DefSpellType 0, $62, 1, FireBallSpellHandler
  DefSpellType 0, $63, 1, FlameArrowSpellHandler
  DefSpellType 8, $64, 0, HasteSpellHandler

  ;10
  DefSpellType 0, $65, 1, HoldPersonSpellHandler
  DefSpellType 8, $5E, 0, Invisibility10SpellHandler
  DefSpellType 0, $1F, 1, LightningBoltSpellHandler
  DefSpellType 2, $66, 0, VampiricTouchSpellHandler

  ;14
  DefSpellType 0, $67, 1, FearSpellHandler
  DefSpellType 0, $59, 1, IceStormSpellHandler
  DefSpellType 1, $5D, 0, StoneSkinSpellHandler
  DefSpellType 0, $00, 1, DefaultSpellHandler

  ;18
  DefSpellType 0, $76, 1, ConeOfColdSpellHandler
  DefSpellType 0, $65, 1, HoldMonsterSpellHandler
  DefSpellType 8, $5B, 0, BlessSpellHandler
  DefSpellType 1, $68, 0, CureLightWoundsSpellHandler

  ;1c
  DefSpellType 0, $6B, 1, CauseLightWoundsSpellHandler
  DefSpellType 8, $5F, 0, DetectMagicSpellHandler
  DefSpellType 1, $6E, 0, ProtectionFromEvilSpellHandler
  DefSpellType 1, $5B, 0, AidSpellHandler

  ;20
  DefSpellType 2, $63, 0, FlameBladeSpellHandler
  DefSpellType 0, $65, 1, HoldPersonSpellHandler
  DefSpellType 1, $6F, 0, SlowPoisonSpellHandler
  DefSpellType 8, $70, 0, CreateFoodSpellHandler

  ;24
  DefSpellType 1, $61, 0, DispelMagicSpellHandler
  DefSpellType 4, $71, 0, MagicalVestmentSpellHandler
  DefSpellType 8, $5B, 0, PrayerSpellHandler
  DefSpellType 8, $72, 0, RemoveParalysisSpellHandler

  ;28
  DefSpellType 1, $69, 0, CureSeriousWoundsSpellHandler
  DefSpellType 0, $6C, 1, CauseSeriousWoundsSpellHandler
  DefSpellType 1, $73, 0, NeutralizePoisonSpellHandler
  DefSpellType 8, $6E, 0, ProtectionFromEvil10SpellHandler

  ;2c
  DefSpellType 4, $74, 0, DefaultSpellHandler
  DefSpellType 1, $6A, 0, CureCriticalWoundsSpellHandler
  DefSpellType 0, $6C, 1, CauseCriticalWoundsSpellHandler
  DefSpellType 0, $62, 1, FlameStrikeSpellHandler

  ;30
  DefSpellType 1, $75, 0, RaiseDeadSpellHandler
  DefSpellType 1, $68, 0, LayOnHandsSpellHandler
  DefSpellType 0, $75, 0, KuotoaSpellHandler
  DefSpellType 0, $75, 0, XanatharDisintegrationSpellHandler

  ;34
  DefSpellType 0, $75, 0, XanatharDeathSpellHandler
.endproc

.export spellTypes_lo
spellTypes_lo:
  .repeat .sizeof(spellTypes)/.sizeof(SpellType), I
    .byte <(spellTypes+I*.sizeof(SpellType))
  .endrep

.export spellTypes_hi
spellTypes_hi:
  .repeat .sizeof(spellTypes)/.sizeof(SpellType), I
    .byte >(spellTypes+I*.sizeof(SpellType))
  .endrep
