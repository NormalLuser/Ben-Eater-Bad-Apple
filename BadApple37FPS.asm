;BASED ON GFOOT SD CARD 6502 GUIDE

;Disconnect LCD and clock system to 5 mhz.
;run in both hsync and vsync (1 pixels on left side will have a noise line on top at 5mhz)
;I disconnect the little LCD while working with this.
;SD Card input is PA0 MISO
;to make the serial code FAST.
;I also have pulse mode on reads
;USE CA2 as clock to save more cycles!!!
;IE FAST..
;Others are pins on VIA Port B 
;Use what you want for pins on B but
;Leave Port A empty except for MISO.
;(I think????)


;This version will use a lookup dictionary
;so we can alway do 3 pixels with 2 bytes min.
;if the last byte can be a RLE that will still work.



VIA              = $6000
VIA_PORTB        = VIA                ; $6000
VIA_PORTA        = VIA+1              ; $6001
VIA_DDRB         = VIA+2              ; $6002
VIA_DDRA         = VIA+3              ; $6003
VIA_T1CL         = VIA+4              ; $6004
VIA_T1CH         = VIA+5              ; $6005
VIA_T1LL         = VIA+6              ; $6006
VIA_T1LH         = VIA+7              ; $6007
VIA_T2CL         = VIA+8              ; $6008
VIA_T2CH         = VIA+9              ; $6009
VIA_SHIFT        = VIA+10             ; $600A
VIA_AUX          = VIA+11             ; $600B ; Set to 0 to stop BEEP ALSO ACR
VIA_PCR          = VIA+12             ; $600C
VIA_IFR          = VIA+13             ; $600D
VIA_IER          = VIA+14             ; $600E
VIA_IORA         = VIA+15             ; $600F



; VIA_PORTA (Address: $6001)
SD_MISO = $01 ; %00000001 (bit 0)

; VIA_PORTB (Address: $6000)
SD_MOSI = $10 ; %00010000 (bit 4)
SD_CS   = $08 ; %00001000 (bit 3)


PORTB_OUTPUTPINS = SD_MOSI | SD_CS

VIA_PORTB_STATE = $02 ; Zero Page variable to hold current VIA_PORTB state


zp_sd_cmd_address = $40

Display           = $2000     ; Start of memory-mapped display. 100x64 mapped to 128x64
Screen            = $ED       ; GFX screen location
ScreenH           = $EE       ; To draw TO

Color       = $03
RLECount    = $01
ReadByteTemp = $07


	.ORG $1000

.reset:

 
  LDA #$00
  STA Screen
  LDA #$20
  STA ScreenH
 ;Not sure why I do this twice...? 
    stz VIA_PORTA
    lda #254;??? #$ff-SD_BIT_MISO 
    sta VIA_DDRA
    stz VIA_PORTA
    lda #254;??? #$ff-SD_BIT_MISO 
    sta VIA_DDRA

  
  ;Make CA2 pulse each time port A is read
  ;lets see if I can use this as the SD clock??
  ;gfoot says it can be done. I still need to figure it out

  lda #$0a 
  STA VIA_PCR

;Do screwy stuff to get SD card
  ;started up in SPI mode.
  jsr sd_init

;Send out ACIA that it did init
;    LDA #'I'
;    STA ACIA
;    LDA #'!'
;    STA ACIA

  ; Read a sector
  lda #SD_MOSI
  sta  VIA_PORTB 
  ; Command 17, arg is sector number, crc not checked
  lda #$52 ; 51           ; CMD17 - READ_SINGLE_BLOCK TRY CMD18
  ;WOW, that easy, change $51 to $52 and I can stream
  ;bytes off the SD card forrever! Yea!
  ;Just have to remember to throw away the 10 byte
  ;CRC every 512 bytes. Otherwise the bits stream forever.
  jsr sd_writebyte
  lda #$00           ; sector 24:31
  jsr sd_writebyte
  lda #$00           ; sector 16:23
  jsr sd_writebyte
  lda #$00           ; sector 8:15
  jsr sd_writebyte
  lda #$00           ; sector 0:7
  jsr sd_writebyte
  lda #RLECount           ; crc (not checked)
  jsr sd_writebyte

  jsr sd_waitresult
  cmp #$00
  beq .readsuccess

;This change makes it pretty reliable after a reboot.
  ;jmp loop 
  jmp .reset 
  
  LDX #$00 
  ldy #$00
  
.readsuccess
  ; wait for data
  jsr sd_waitresult
  cmp #$fe
  beq .readgotdata

  ;jmp loop
  jmp .reset






.readgotdata
  ; Need to read 512 bytes.  Read two at a time, 256 times.
  lda #0
  sta $00 ; counter HERE IT IS!!!
  ;cycle clock once.

  lda #SD_MOSI                ; enable card (CS low), set MOSI (resting state), SCK low
  sta  VIA_PORTB 
  

  lda VIA_PORTA ; toggle the clock once at the start
.readloop:
 ; UNROLL!!!
 ; The unroll here added 7 frames a second on top of 30.
; Read first byte into RLECount
    lda #$00          ; Clear accumulator

    ; Bit 7
    asl               ; Shift accumulator left
    lda VIA_PORTA     ; Trigger clock pulse, read MISO
    and #SD_MISO
    bne .bit7_one_1
    jmp .bit7_done_1
.bit7_one_1:
    ora #$01
.bit7_done_1:

    ; Bit 6
    asl               ; Shift accumulator left
    lda VIA_PORTA
    and #SD_MISO
    bne .bit6_one_1
    jmp .bit6_done_1
.bit6_one_1:
    ora #$01
.bit6_done_1:

    ; Bit 5
    asl               ; Shift accumulator left
    lda VIA_PORTA
    and #SD_MISO
    bne .bit5_one_1
    jmp .bit5_done_1
.bit5_one_1:
    ora #$01
.bit5_done_1:

    ; Bit 4
    asl               ; Shift accumulator left
    lda VIA_PORTA
    and #SD_MISO
    bne .bit4_one_1
    jmp .bit4_done_1
.bit4_one_1:
    ora #$01
.bit4_done_1:

    ; Bit 3
    asl               ; Shift accumulator left
    lda VIA_PORTA
    and #SD_MISO
    bne .bit3_one_1
    jmp .bit3_done_1
.bit3_one_1:
    ora #$01
.bit3_done_1:

    ; Bit 2
    asl               ; Shift accumulator left
    lda VIA_PORTA
    and #SD_MISO
    bne .bit2_one_1
    jmp .bit2_done_1
.bit2_one_1:
    ora #$01
.bit2_done_1:

    ; Bit 1
    asl               ; Shift accumulator left
    lda VIA_PORTA
    and #SD_MISO
    bne .bit1_one_1
    jmp .bit1_done_1
.bit1_one_1:
    ora #$01
.bit1_done_1:

    ; Bit 0
    asl               ; Shift accumulator left
    lda VIA_PORTA
    and #SD_MISO
    bne .bit0_one_1
    jmp .bit0_done_1
.bit0_one_1:
    ora #$01
.bit0_done_1:

    sta RLECount      ; Store the result

; Read second byte into Color
    lda #$00          ; Clear accumulator

    ; Bit 7
    asl               ; Shift accumulator left
    lda VIA_PORTA     ; Trigger clock pulse, read MISO
    and #SD_MISO
    bne .bit7_one_2
    jmp .bit7_done_2
.bit7_one_2:
    ora #$01
.bit7_done_2:

    ; Bit 6
    asl               ; Shift accumulator left
    lda VIA_PORTA
    and #SD_MISO
    bne .bit6_one_2
    jmp .bit6_done_2
.bit6_one_2:
    ora #$01
.bit6_done_2:

    ; Bit 5
    asl               ; Shift accumulator left
    lda VIA_PORTA
    and #SD_MISO
    bne .bit5_one_2
    jmp .bit5_done_2
.bit5_one_2:
    ora #$01
.bit5_done_2:

    ; Bit 4
    asl               ; Shift accumulator left
    lda VIA_PORTA
    and #SD_MISO
    bne .bit4_one_2
    jmp .bit4_done_2
.bit4_one_2:
    ora #$01
.bit4_done_2:

    ; Bit 3
    asl               ; Shift accumulator left
    lda VIA_PORTA
    and #SD_MISO
    bne .bit3_one_2
    jmp .bit3_done_2
.bit3_one_2:
    ora #$01
.bit3_done_2:

    ; Bit 2
    asl               ; Shift accumulator left
    lda VIA_PORTA
    and #SD_MISO
    bne .bit2_one_2
    jmp .bit2_done_2
.bit2_one_2:
    ora #$01
.bit2_done_2:

    ; Bit 1
    asl               ; Shift accumulator left
    lda VIA_PORTA
    and #SD_MISO
    bne .bit1_one_2
    jmp .bit1_done_2
.bit1_one_2:
    ora #$01
.bit1_done_2:

    ; Bit 0
    asl               ; Shift accumulator left
    lda VIA_PORTA
    and #SD_MISO
    bne .bit0_one_2
    jmp .bit0_done_2
.bit0_one_2:
    ora #$01
.bit0_done_2:

    sta Color         ; Store the result



;OK, now I need to see if it is a 'skip'
  LDA Color ;NEED THIS
  CMP #64
  BEQ .SkipRun ;it is 64, want to skip these.
  BCS .TriPixel ;BCS GREATER THAN 64
.TriDone:
  LDX #$00 
  ldy #$00
  LDA Color



.RLETop:
  DEC RLECount
  LDA Color
  STA (Screen),y

; Increment Screen Pointer
	INC Screen
	BNE .RLEContinue ; If Screen != 0, continue
	INC ScreenH
	LDA ScreenH
	CMP #$40
	BNE .RLEContinue
	LDA #$20
	STA ScreenH
.RLEContinue:

  LDA RLECount
  BNE .RLETop

.RLEDone:
  dec $00 ; counter 
  BEQ .BLOCK
  JMP .readloop 



.TriPixel:
    TAX ;color/index to x
    LDA Array1-65,x
    LDY #0
    sta (Screen),y
    INY
    LDA Array2-65,x
    sta (Screen),y
    INY
    LDA Array3-65,x
    sta (Screen),y
    STA Color
    ;------------
	CLC
	LDA Screen
	ADC #3 ; Advance by 3 bytes
	STA Screen
	BCC .NoCarry
	INC ScreenH
.NoCarry:
	LDA ScreenH
	CMP #$40
	BNE .TriDone
	LDA #$20
	STA ScreenH
    JMP .TriDone



.SkipRun:
	CLC  
	LDA Screen
	ADC RLECount
	STA Screen
	BCC .SkipNoCarry
	INC ScreenH
.SkipNoCarry:
	LDA ScreenH
	CMP #$40
	BNE .SkipContinue
	LDA #$20
	STA ScreenH
.SkipContinue:
  
  dec $00 ; counter
  BEQ .BLOCK
  JMP .readloop 


.BLOCK: ; 644 CYCLES
  ;must throw away 10 bytes every block.
  ;HRM... I could speed this up with a macro .
  ;should figure out how to do that sometime....
  ;I'll just code it....
;OK, JUST UNROLL, NEED TO FIGURE OUT MACROS SO IT DOES NOT LOOK LIKE THIS
;Regardless, this is the fastest way I could think of to throw away 10 bytes
;It is worth FRAMES.. As in more than 1 Frame a second to do this silly stuff!
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA

  ;Read a Byte
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA

  ;Read a Byte
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA

  ;Read a Byte
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA

  ;Read a Byte
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA

  ;Read a Byte
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA

  ;Read a Byte
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA

  ;Read a Byte
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA

  ;Read a Byte
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA

  ;Read a Byte
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA

 
  JMP .readloop ;HOW ABOUT WE LOOP FOREVER?
  ;Yep, it will happily stream garbage off the SD card forever.

;Below is the lookup array for the 3 pixel compression.
;If the encoded byte is a color 0-63 it uses that
;to draw the current memory location. The next byte 
;is a repeat or Run Length Byte. 
;It will repeat that number of times.
;If the number is 1 it draws 1 pixel.
;If the encoded byte is value 64 it skips.
;so insteat it reads the Run Length and adds that
;value to the memory pointer.
;If the value is 65 through 129 it uses the 3 lookup
;array's below to get a 3 pixel value.
;For Bad Apple! the table below is all possible 3 pixel
;values of Black/White/Light Grey/Dark Grey that are 
;used in this demo. 
Array1:
    .byte 0,0,0,21,0,0,42,0,0,63,0,21,21,0,0,21,21,42,42,0,0,21,21,63,63,0,42,42,0,0,42,42,63,63,0,63,63,21,21,21,42,21,21,63,21,42,42,21,21,42,42,63,63,21,63,63,42,42,42,63,42,63,63,63
Array2:
    .byte 0,0,21,0,0,42,0,0,63,0,21,0,21,21,42,0,42,0,21,21,63,0,63,0,21,42,0,42,42,63,0,63,0,42,63,0,63,21,21,42,21,21,63,21,42,21,42,42,63,21,63,21,42,63,21,63,42,42,63,42,63,42,63,63
Array3:
    .byte 0,21,0,0,42,0,0,63,0,0,21,21,0,42,21,42,0,21,0,63,21,63,0,21,0,42,42,0,63,42,63,0,42,0,63,63,0,21,42,21,21,63,21,21,42,42,21,63,42,63,21,42,21,63,63,21,42,63,42,42,63,63,42,63



sd_init:
  ; Let the SD card boot up, by pumping the clock with SD CS disabled

  ; We need to apply around 80 clock pulses with CS and MOSI high.
  ; Normally MOSI doesn't matter when CS is high, but the card is
  ; not yet is SPI mode, and in this non-SPI state it does care.

  lda #SD_CS | SD_MOSI
  ldx #160               ; toggle the clock 160 times, so 80 low-high transitions
.preinitloop:
  eor #SD_SCK
  sta PORTA
  dex
  bne .preinitloop
  
  jsr longdelay

  lda #SD_CS | SD_MOSI
  ldx #16               ; toggle the clock 16 times, so 8 low-high transitions
.precmd0loop:
  eor #SD_SCK
  sta PORTA
  dex
  bne .precmd0loop

.cmd0 ; GO_IDLE_STATE - resets card to idle state, and SPI mode
  lda #<cmd0_bytes
  sta zp_sd_cmd_address
  lda #>cmd0_bytes
  sta zp_sd_cmd_address+1

  jsr sd_sendcommand

  ; Expect status response $01 (not initialized)
  cmp #$01
  bne .initfailed

  jsr mediumdelay

  lda #SD_CS | SD_MOSI
  ldx #16               ; toggle the clock 16 times, so 8 low-high transitions
.precmd8loop:
  eor #SD_SCK
  sta PORTA
  dex
  bne .precmd8loop

.cmd8 ; SEND_IF_COND - tell the card how we want it to operate (3.3V, etc)
  lda #<cmd8_bytes
  sta zp_sd_cmd_address
  lda #>cmd8_bytes
  sta zp_sd_cmd_address+1

  jsr sd_sendcommand

  ; Expect status response $01 (not initialized)
  cmp #$01
  bne .initfailed

  ; Read 32-bit return value, but ignore it
  jsr sd_readbyte
  jsr sd_readbyte
  jsr sd_readbyte
  jsr sd_readbyte

  jsr mediumdelay

;   lda #SD_CS | SD_MOSI
;   ldx #16               ; toggle the clock 16 times, so 8 low-high transitions
; .precmd55loop:
;   eor #SD_SCK
;   sta PORTA
;   dex
;   bne .precmd55loop

.cmd55 ; APP_CMD - required prefix for ACMD commands
  lda #SD_CS | SD_MOSI
  ldx #16               ; toggle the clock 16 times, so 8 low-high transitions
.precmd55loop_1:
  eor #SD_SCK
  sta PORTA
  dex
  bne .precmd55loop_1

  lda #<cmd55_bytes
  sta zp_sd_cmd_address
  lda #>cmd55_bytes
  sta zp_sd_cmd_address+1

  jsr sd_sendcommand

  ; Expect status response $01 (not initialized)
  cmp #$01
  bne .initfailed

  jsr mediumdelay

  lda #SD_CS | SD_MOSI
  ldx #16               ; toggle the clock 16 times, so 8 low-high transitions
.precmd41loop:
  eor #SD_SCK
  sta PORTA
  dex
  bne .precmd41loop

.cmd41 ; APP_SEND_OP_COND - send operating conditions, initialize card
  lda #<cmd41_bytes
  sta zp_sd_cmd_address
  lda #>cmd41_bytes
  sta zp_sd_cmd_address+1

  jsr sd_sendcommand

  ; Status response $00 means initialised
  cmp #$00
  beq .initialized

  ; Otherwise expect status response $01 (not initialized)
  cmp #$01
  bne .initfailed

  ; Not initialized yet, so wait a while then try again.
  ; This retry is important, to give the card time to initialize.

  jsr mediumdelay

  jmp .cmd55


.initialized
  jsr longdelay

  lda #'Y'
  jsr print_char
  rts

  ; loop forever
.loop:
  jmp .loop


.initfailed
  lda #'X'
  jsr print_char
  jmp .loop


cmd0_bytes
  .byte $40, $00, $00, $00, $00, $95
cmd8_bytes
  .byte $48, $00, $00, $01, $aa, $87
cmd55_bytes
  .byte $77, $00, $00, $00, $00, $01
cmd41_bytes
  .byte $69, $40, $00, $00, $00, $01



sd_readbyte:
  ; Enable the card and tick the clock 8 times with MOSI high, 
  ; capturing bits from MISO and returning them

  ldx #8                      ; we'll read 8 bits
.loop:

  lda #SD_MOSI                ; enable card (CS low), set MOSI (resting state), SCK low
  sta PORTA

  lda #SD_MOSI | SD_SCK       ; toggle the clock high
  sta PORTA

  lda PORTA                   ; read next bit
  and #SD_MISO

  clc                         ; default to clearing the bottom bit
  beq .bitnotset              ; unless MISO was set
  sec                         ; in which case get ready to set the bottom bit
.bitnotset:

  tya                         ; transfer partial result from Y
  rol                         ; rotate carry bit into read result
  tay                         ; save partial result back to Y

  dex                         ; decrement counter
  bne .loop                   ; loop if we need to read more bits

  rts


sd_writebyte:
  ; Tick the clock 8 times with descending bits on MOSI
  ; SD communication is mostly half-duplex so we ignore anything it sends back here

  ldx #8                      ; send 8 bits

.loop:
  asl                         ; shift next bit into carry
  tay                         ; save remaining bits for later

  lda #0
  bcc .sendbit                ; if carry clear, don't set MOSI for this bit
  ora #SD_MOSI

.sendbit:
  sta PORTA                   ; set MOSI (or not) first with SCK low
  eor #SD_SCK
  sta PORTA                   ; raise SCK keeping MOSI the same, to send the bit

  tya                         ; restore remaining bits to send

  dex
  bne .loop                   ; loop if there are more bits to send

  rts


sd_waitresult:
  ; Wait for the SD card to return something other than $ff
  jsr sd_readbyte
  cmp #$ff
  beq sd_waitresult
  rts


sd_sendcommand:
  ; Debug print which command is being executed
  jsr lcd_cleardisplay

  lda #'c'
  jsr print_char
  ldx #0
  lda (zp_sd_cmd_address,x)
  jsr print_hex

  lda #SD_MOSI           ; pull CS low to begin command
  sta PORTA

  ldy #0
  lda (zp_sd_cmd_address),y    ; command byte
  jsr sd_writebyte
  ldy #1
  lda (zp_sd_cmd_address),y    ; data 1
  jsr sd_writebyte
  ldy #2
  lda (zp_sd_cmd_address),y    ; data 2
  jsr sd_writebyte
  ldy #3
  lda (zp_sd_cmd_address),y    ; data 3
  jsr sd_writebyte
  ldy #4
  lda (zp_sd_cmd_address),y    ; data 4
  jsr sd_writebyte
  ldy #5
  lda (zp_sd_cmd_address),y    ; crc
  jsr sd_writebyte

  jsr sd_waitresult
  pha

  ; Debug print the result code
  jsr print_hex

  ; End command
  lda #SD_CS | SD_MOSI   ; set CS high again
  sta PORTA

  pla   ; restore result code
  rts


delay
  ldx #0
  ldy #0
.loop
  dey
  bne .loop
  dex
  bne .loop
  rts

longdelay
  jsr mediumdelay
  jsr mediumdelay
  jsr mediumdelay
mediumdelay
  jsr delay
  jsr delay
  jsr delay
  jmp delay

