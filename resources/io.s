.include "global.inc"

EAPI_RAM_CODE = $0100 ; $df80 RAM code is detected and relocated to $0100
EAPI = (frameBuffer_bitmap+$ff)&$ff00
EAPIInit = EAPI+20
.define EAPIWriteFlash		EAPI_RAM_CODE+$00
.define EAPIEraseSector 	EAPI_RAM_CODE+$03
.define EAPISetBank			EAPI_RAM_CODE+$06
.define EAPIGetBank			EAPI_RAM_CODE+$09
.define EAPISetPtr			EAPI_RAM_CODE+$0c
.define EAPISetLen			EAPI_RAM_CODE+$0f
.define EAPIReadFlashInc	EAPI_RAM_CODE+$12
.define EAPIWriteFlashInc	EAPI_RAM_CODE+$15
.define EAPISetSlot			EAPI_RAM_CODE+$18

backupScreen = __MEMCODE_RO_RUN__
backupD800 = (__MEMCODE_RO_RUN__+$400)
.export fileBuffer
fileBuffer = frameBuffer_sprites

.segment "BSS"

.export nbrFilesFound
nbrFilesFound: .res 1
_d021: .res 1

.export driveErrorString
buffer:
.export filename = buffer
driveErrorString: .res 40

.segment "IO"

.proc stop
		bit $d011
		bpl *-3
		bit $d011
		bmi *-3
		sei
		lda #<$ea31
		sta $0314
		lda #>$ea31
		sta $0315
		lda #0
		sta $d011
		sta $d015
		sta $d020
		sta $d418
		sta $d01a
		dec $d019
		lda #$fc
		sta $d030
		lda #$7f
		sta $dc0d
		sta $dd0d
		bit $dc0d
		bit $dd0d
		rts
.endproc

.proc resume
		jsrf audio_init
		ldy #ScreenMode::setup
		jsr callScreenModeFunction

		bit $d011
		bpl *-3
		bit $d011
		bmi *-3
		jsrf restore_irq
		cli
		rts
.endproc

.proc enterIO
		lda $d021
		sta _d021
		lda #0
		sta $d021
		lda #$7f
		sta $dc0d
		sta $dd0d
		bit $dc0d
		bit $dd0d

		lda $dd00
		and #$fc
		sta $dd00
		lda #(((vic_screen/$400)&15)<<4) | ((vic_font/$400)&14)
		sta $d018
		lda #$08
		sta $d016

		ldx #0
		:
			.repeat 4,I
				lda vic_screen+$100*I,x
				sta backupScreen+$100*I,x
				lda $d800+$100*I,x
				sta backupD800+$100*I,x
			.endrep
			lda #$20
			.repeat 4,I
				sta vic_screen+$100*I,x
			.endrep
			lda #1
			.repeat 4,I
				sta $d800+$100*I,x
			.endrep
			dex
		bne :-
		lda #$1b
		sta $d011
		rts
.endproc

.proc enterKernel
		; Backup and clear ZP
		ldx #2
		:
			lda $00,x
			sta backup_zp,x
			lda #0
			sta $00,x
			inx
		bne :-

		jsr $fda3 ; initialize i/o

		jmp enterIO
.endproc

.proc leaveIO
		bit $d011
		bpl *-3
		bit $d011
		bmi *-3
		lda #0
		sta $d011

		ldx #0
		:
			.repeat 4,I
				lda backupScreen+$100*I,x
				sta vic_screen+$100*I,x
				lda backupD800+$100*I,x
				sta $d800+$100*I,x
			.endrep
			inx
		bne :-

		lda _d021
		sta $d021
		rts
.endproc

.proc leaveKernel
		sei
		jsr leaveIO

		; Restore ZP
		ldx #2
		:
			lda backup_zp,x
			sta $00,x
			inx
		bne :-

		rts
.endproc

.proc showMessage
		stx $fb
		sty $fc
		ldy #0
		:
			lda ($fb),y
			beq :+
			sta vic_screen,y
			iny
		jmp :-
		:
		rts
.endproc


.proc returnErrorCode
		sec
		lda driveErrorString
		sbc #'0'
		asl
		asl
		asl
		asl
		sta TMP
		sec
		lda driveErrorString+1
		sbc #'0'
		ora TMP
		tax
		rts
.endproc

.export io_readDirectory
.proc io_readDirectory
		jsr stop
		jsr enterKernel
		ldx #<msg
		ldy #>msg
		jsr showMessage
		jsr readDirectory
		jsr readErrorChannel
		jsr leaveKernel
		jsr installMemCodeRO
		jsr resume
		jmp returnErrorCode
msg:	.byte "LESE VERZEICHNIS, BITTE WARTEN...",0
.endproc

.export io_save
.proc io_save
		jsr stop
		jsrf gameState_saveLevel
		jsr enterKernel

		ldx #<msg
		ldy #>msg
		jsr showMessage
		jsr save
		jsr readErrorChannel
		jsr leaveKernel
		jsr installMemCodeRO
		jsr resume
		jmp returnErrorCode

msg:	.byte "SPEICHERE AUF DISK, BITTE WARTEN...",0
.endproc


.export io_flashSave
.proc io_flashSave
		jsr stop
		jsrf gameState_saveLevel
		jsr enterIO

		ldx #<msg
		ldy #>msg
		jsr showMessage
		jsrf prepareEAPI
		jsr flashSave
		jsr leaveIO
		jsr installMemCodeRO
		jsr resume
		rts

msg:	.byte "SPEICHERE IN FLASH, BITTE WARTEN...",0
.endproc


.export io_load
.proc io_load
		jsr stop
		jsr enterKernel

		ldx #<msg
		ldy #>msg
		jsr showMessage
		jsr load
		jsr readErrorChannel
		jsr leaveKernel
		jsr installMemCodeRO
		jsrf gameState_didLoadLevel
		jsr resume
		jmp returnErrorCode

msg:	.byte "LADE VON DISK, BITTE WARTEN...",0
.endproc


.export io_flashLoad
.proc io_flashLoad
		jsr stop
		jsr enterIO

		ldx #<msg
		ldy #>msg
		jsr showMessage
		jsrf prepareEAPI
		jsr flashLoad
		jsr leaveIO
		jsr installMemCodeRO
		jsrf gameState_didLoadLevel
		jsr resume
		rts

msg:	.byte "LADE VON FLASH, BITTE WARTEN...",0
.endproc


.proc readDirectory
		lda #<fileBuffer
		sta $fb
		lda #>fileBuffer
		sta $fc
		lda #0
		sta nbrFilesFound

		LDA #dirname_end-dirname
        LDX #<dirname
        LDY #>dirname
        JSR $FFBD      ; call SETNAM

        LDA #$02       ; filenumber 2
        LDX preferredDevice
		LDY #$00       ; secondary address 0 (required for dir reading!)
        JSR $FFBA      ; call SETLFS

        JSR $FFC0      ; call OPEN (open the directory)
        bcs exit

        LDX #$02       ; filenumber 2
        JSR $FFC6      ; call CHKIN

        ; Skip disk header
        ldx #6 ; skip loadaddr, link and size
        :
        	jsr $ffcf
        	dex
        bne :-
        jsr $ffb7
        bne exit
        :
        	jsr $ffcf
        bne :-

		fileLoop:
			LDX #$04 ; skip link and size
			:
				JSR $ffcf
				DEX
			BNE :-

			; Check eof
			JSR $FFB7
			BNE exit

			; Read line into buffer
			ldx #<-1
			:
				inx
				JSR $ffcf
				cmp #'*'
				beq skipLine
				sta buffer,x
				cmp #0
			bne :-
			jsr parseBuffer

			lda nbrFilesFound
			cmp #22*15*8/16
			beq exit
		jmp fileLoop
skipLine:
		jsr $ffcf
		cmp #0
		bne skipLine
		beq fileLoop

exit:	lda #2
		JSR $F642    ; CLOSE
        JSR $FFCC     ; call CLRCHN
        rts

parseBuffer:
			; Scan for slash
			:
				lda buffer,x
				cmp #'/'
				beq :+
				dex
			bpl :-
				rts
			:

			; Add file
			ldy #0
			:
				inx
				lda buffer,x
				sta ($fb),y
				iny
				cpy #8
			bne :-

			clc
			lda $fb
			adc #8
			sta $fb
			bcc :+
				inc $fc
			:
			inc nbrFilesFound

ret:		rts

dirname:	.byte "$:EOBSAVE/*=P"
dirname_end:
.endproc


.proc readErrorChannel
		lda #<driveErrorString
		sta $fb
		lda #>driveErrorString
		sta $fc

		LDA #$00
        STA $90       ; clear STATUS flags

        LDA $BA       ; device number
        JSR $FFB1     ; call LISTEN
        LDA #$6F      ; secondary address 15 (command channel)
        JSR $FF93     ; call SECLSN (SECOND)
        JSR $FFAE     ; call UNLSN
        LDA $90       ; get STATUS flags
        BNE deviceNotPresent

        LDA $BA       ; device number
        JSR $FFB4     ; call TALK
        LDA #$6F      ; secondary address 15 (error channel)
        JSR $FF96     ; call SECTLK (TKSA)

        ldy #0
		:
			LDA $90       ; get STATUS flags
        	BNE :+      ; either EOF or error
	        JSR $FFA5     ; call IECIN (get byte from IEC bus)
	        sta ($fb),y
	        iny
        JMP :-     ; next byte
        :

        dey
        lda #0
        sta ($fb),y

        JSR $FFAB     ; call UNTLK
        RTS

		.proc deviceNotPresentMsg
			.byte "DEVICE NOT PRESENT",0
		.endproc
deviceNotPresent:
		ldx #.sizeof(deviceNotPresentMsg)-1
		:
			lda deviceNotPresentMsg,x
			sta driveErrorString,x
			dex
		bpl :-
		rts
.endproc

.proc load
		lda #16
		ldx #<buffer
		ldy #>buffer
		jsr $ffbd ;SETNAM
		lda #$01
		ldx preferredDevice
		ldy #$00
		jsr $ffba ;SETLFS
		lda #$00
		ldx #<SAVEGAME_START
		ldy #>SAVEGAME_START
		jsr $ffd5 ;LOAD
		rts
.endproc

.proc save
		; Generate filename
		ldx #filename_end-filename-1
		:
			lda filename,x
			sta buffer,x
			dex
		bpl :-
		.repeat 4,I
			lda _tick+3-I
			jsr hexToAscii
			stx buffer+11+I*2
			sty buffer+12+I*2
		.endrep

		; Set filename and device
        lda #filename_end-filename
        ldx #<buffer
        ldy #>buffer
        jsr $ffbd ;SETNAM
        lda #$00
       	ldx preferredDevice
        jsr $ffba ;SETLFS

        ; Call save
        lda #<SAVEGAME_START
        sta $C1
        lda #>SAVEGAME_START
        sta $C2
        ldx #<SAVEGAME_END
        ldy #>SAVEGAME_END
        lda #$C1
        jsr kernelSave        
		rts

hexToAscii:
		pha
		and #15
		tax
		ldy hex,x
		pla
		lsr
		lsr
		lsr
		lsr
		tax
		lda hex,x
		tax
		rts

filename:		.byte "@0:EOBSAVE/00000000"
filename_end:
hex:			.byte "0123456789ABCDEF"
.endproc

.proc flashLoad
		memcpy_bank (SAVEGAME_START+$0000), $8000, (<(.bank(quickSave)+0)), $2000
		memcpy_bank (SAVEGAME_START+$2000), $8000, (<(.bank(quickSave)+1)), $2000
		memcpy_bank (SAVEGAME_START+$4000), $8000, (<(.bank(quickSave)+2)), (SAVEGAME_SIZE-$4000)
		rts
.endproc

.segment "MEMCODE_RO"
.proc flashSave
		lda BANK
		pha

		lda #<.bank(quickSave)
		ldy #$80 ;Only erase LO-chip
		jsr EAPIEraseSector

		lda #<.bank(quickSave)
		jsr EAPISetBank

		lda #$b0 ;LLLLL as bank mode strategy
		ldx #<quickSave
		ldy #>quickSave
		jsr EAPISetPtr

        lda #$00
        sta SRC+0
        lda #>SAVEGAME_START
        tax
        sta SRC+1
        ldy #<SAVEGAME_START
        nextByte:
        	lda #$35
        	sta 1
        	lda (SRC),y
        	pha
        	lda #$37
        	sta 1
        	pla
        	jsr EAPIWriteFlashInc
        	iny
        	bne :+
        		inc SRC+1
        		inx
        	:
        	cpx #>SAVEGAME_END
        	bne nextByte
        	cpy #<SAVEGAME_END
        bne nextByte

		pla
		sta BANK
		sta $de00
		rts
.endproc

.export isFlashSaveAvailable 
.proc isFlashSaveAvailable
		lda BANK
		pha
		lda #<.bank(quickSave)
		sta BANK
		sta $de00
		ldx $8000	
		pla
		sta BANK
		sta $de00
		rts
.endproc

.segment "MAIN"
.export prepareEAPI
.proc prepareEAPI
		; Copy eapi to RAM (-$4000 because EAPI is defined in ULTIMAX but now is banked in as ROMH)
		memcpy EAPI, (__EAPI_RUN__-$4000), $300
		jsr EAPIInit

		; Detect if RAM exists at $df00-df7f. If not it's safe to assume custom EF1(cr)
		ldy #1
		ldx #0
		:
			txa
			sta $df00,x
			inx
		bpl :-
		ldx #0
		:
			txa
			cmp $df00,x
			bne :+
			inx
		bpl :-
		ldy #0
		:

		cpy #0
		bne :++
			; RAM at $df00-df7f, hence normal Easyflash and JMPTBL will be at $df80-. Copy to $0100 so that it conforms with EF1(cr)
			ldx #$1f
			:

				lda $df80,x
				sta $0100,x
				dex
			bpl :-
		:

		rts
.endproc


