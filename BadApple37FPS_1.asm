; Define registers and pin assignments
VIA              = $6000
VIA_PORTB        = VIA                ; $6000
VIA_PORTA        = VIA+1              ; $6001
VIA_DDRB         = VIA+2              ; $6002
VIA_DDRA         = VIA+3              ; $6003
VIA_PCR          = VIA+12             ; $600C

; VIA_PORTA (Address: $6001)
SD_MISO = $01 ; %00000001 (bit 0)

; VIA_PORTB (Address: $6000)
SD_MOSI = $10 ; %00010000 (bit 4)
SD_CS   = $08 ; %00001000 (bit 3)
SD_SCK  = $20 ; %00100000 (bit 5)

PORTB_OUTPUTPINS = SD_MOSI | SD_CS | SD_SCK

VIA_PORTB_STATE = $02 ; Zero Page variable to hold current VIA_PORTB state

; Other variables and constants
zp_sd_cmd_address = $40
Display           = $2000
Screen            = $ED
ScreenH           = $EE
Color             = $03
RLECount          = $01
ReadByteTemp      = $07

	.ORG $1000

.reset:
    ; Initialize Screen Pointers
    LDA #$00
    STA Screen
    LDA #$20
    STA ScreenH

    ; Initialize VIA Ports
    STZ VIA_PORTA
    STZ VIA_PORTB

    ; Configure Data Direction Registers
    lda #%11111110      ; Set PA0 (SD_MISO) as input
    sta VIA_DDRA

    lda #%00111000      ; Set PB3 (SD_CS), PB4 (SD_MOSI), PB5 (SD_SCK) as outputs
    sta VIA_DDRB

    ; Initialize VIA_PORTB outputs to known state
    lda #SD_CS | SD_MOSI ; CS high, MOSI high, SCK low
    sta VIA_PORTB_STATE
    sta VIA_PORTB

    ; Configure VIA_PCR if needed (depends on your hardware)
    ; For now, we'll leave it as is.

    ; Start SD card initialization
    jsr sd_init


  ; Read a sector
  lda #SD_MOSI
  sta  VIA_PORTB 
  ; Command 17, arg is sector number, crc not checked
  lda #$52 ; 51           ; CMD17 - READ_SINGLE_BLOCK TRY CMD18

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


Array1:
    .byte 0,0,0,21,0,0,42,0,0,63,0,21,21,0,0,21,21,42,42,0,0,21,21,63,63,0,42,42,0,0,42,42,63,63,0,63,63,21,21,21,42,21,21,63,21,42,42,21,21,42,42,63,63,21,63,63,42,42,42,63,42,63,63,63
Array2:
    .byte 0,0,21,0,0,42,0,0,63,0,21,0,21,21,42,0,42,0,21,21,63,0,63,0,21,42,0,42,42,63,0,63,0,42,63,0,63,21,21,42,21,21,63,21,42,21,42,42,63,21,63,21,42,63,21,63,42,42,63,42,63,42,63,63
Array3:
    .byte 0,21,0,0,42,0,0,63,0,0,21,21,0,42,21,42,0,21,0,63,21,63,0,21,0,42,42,0,63,42,63,0,42,0,63,63,0,21,42,21,21,63,21,21,42,42,21,63,42,63,21,42,21,63,63,21,42,63,42,42,63,63,42,63

sd_init:

	; Initialize VIA_PORTB_STATE if not already done
	lda #SD_CS | SD_MOSI ; CS high, MOSI high, SCK low
	;sta VIA_PORTB_STATE
	;sta VIA_PORTB

	ldx #160               ; toggle the clock 160 times, so 80 low-high transitions
.preinitloop:
  lda VIA_PORTB_STATE
  eor #SD_SCK          ; Toggle SD_SCK bit
  sta VIA_PORTB_STATE
  sta VIA_PORTB        ; Write updated state to VIA_PORTB
  dex
  bne .preinitloop
  
  jsr longdelay

	; Initialize VIA_PORTB_STATE if not already done
	lda #SD_CS | SD_MOSI ; CS high, MOSI high, SCK low
	sta VIA_PORTB_STATE
	sta VIA_PORTB
.precmd0loop:
; Toggle clock in sd_init
	lda VIA_PORTB_STATE
	eor #SD_SCK          ; Toggle SD_SCK bit
	sta VIA_PORTB_STATE
	sta VIA_PORTB        ; Write updated state to VIA_PORTB

  dex
  bne .precmd0loop

.cmd0 ; GO_IDLE_STATE - resets card to idle state, and SPI mode
  lda #<cmd0_bytes
  sta zp_sd_cmd_address
  lda #>cmd0_bytes
  sta zp_sd_cmd_address+1

  jsr sd_sendcommand


  jsr mediumdelay

	; Initialize VIA_PORTB_STATE if not already done
	lda #SD_CS | SD_MOSI ; CS high, MOSI high, SCK low
	sta VIA_PORTB_STATE
	sta VIA_PORTB
.precmd8loop:
	; Toggle clock in sd_init
	lda VIA_PORTB_STATE
	eor #SD_SCK          ; Toggle SD_SCK bit
	sta VIA_PORTB_STATE
	sta VIA_PORTB        ; Write updated state to VIA_PORTB

  dex
  bne .precmd8loop

.cmd8 ; SEND_IF_COND - tell the card how we want it to operate (3.3V, etc)
  lda #<cmd8_bytes
  sta zp_sd_cmd_address
  lda #>cmd8_bytes
  sta zp_sd_cmd_address+1

  jsr sd_sendcommand



  ; Read 32-bit return value, but ignore it
  jsr sd_readbyte
  jsr sd_readbyte
  jsr sd_readbyte
  jsr sd_readbyte

  jsr mediumdelay



.cmd55 ; APP_CMD - required prefix for ACMD commands
	; Initialize VIA_PORTB_STATE if not already done
	lda #SD_CS | SD_MOSI ; CS high, MOSI high, SCK low
	sta VIA_PORTB_STATE
	sta VIA_PORTB
.precmd55loop_1:
	; Toggle clock in sd_init
	lda VIA_PORTB_STATE
	eor #SD_SCK          ; Toggle SD_SCK bit
	sta VIA_PORTB_STATE
	sta VIA_PORTB        ; Write updated state to VIA_PORTB

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

	; Initialize VIA_PORTB_STATE if not already done
	lda #SD_CS | SD_MOSI ; CS high, MOSI high, SCK low
	sta VIA_PORTB_STATE
	sta VIA_PORTB
.precmd41loop:
	; Toggle clock in sd_init
	lda VIA_PORTB_STATE
	eor #SD_SCK          ; Toggle SD_SCK bit
	sta VIA_PORTB_STATE
	sta VIA_PORTB        ; Write updated state to VIA_PORTB

  dex
  bne .precmd41loop

.cmd41 ; APP_SEND_OP_COND - send operating conditions, initialize card
    lda #<cmd41_bytes
    sta zp_sd_cmd_address
    lda #>cmd41_bytes
    sta zp_sd_cmd_address+1

    jsr sd_sendcommand

    ; Status response $00 means initialized
    cmp #$00
    beq .initialized

    ; If the response is $01, continue with initialization
    cmp #$01
    beq .continue_init

    ; If not, initialization failed
    jmp .initfailed

.continue_init:
    ; Not initialized yet, so wait a while then try again.
    jsr mediumdelay

    jmp .cmd55

.initialized
    jsr longdelay
	
    rts

.initfailed

    jmp .initialized


cmd0_bytes
  .byte $40, $00, $00, $00, $00, $95
cmd8_bytes
  .byte $48, $00, $00, $01, $aa, $87
cmd55_bytes
  .byte $77, $00, $00, $00, $00, $01
cmd41_bytes
  .byte $69, $40, $00, $00, $00, $01



sd_readbyte:
    ldx #8
    lda #0                      ; Clear A to store the result

.read_bit_loop:
    ; Toggle SCK high
    lda VIA_PORTB_STATE
    ora #SD_SCK
    sta VIA_PORTB_STATE
    sta VIA_PORTB

    ; Read MISO
    lda VIA_PORTA
    and #SD_MISO
    beq .no_set_bit
    sec                         ; Set carry if MISO is high
    rol A                       ; Rotate carry into A
    jmp .after_shift
.no_set_bit:
    clc                         ; Clear carry if MISO is low
    rol A                       ; Rotate carry into A
.after_shift:

    ; Toggle SCK low
    lda VIA_PORTB_STATE
    and #~SD_SCK
    sta VIA_PORTB_STATE
    sta VIA_PORTB

    dex
    bne .read_bit_loop

    rts



sd_writebyte:
    ldx #8

.write_bit_loop:
    asl                         ; Shift next bit into carry
    tay                         ; Save remaining bits

    lda VIA_PORTB_STATE
    and #~SD_MOSI               ; Clear SD_MOSI bit
    bcc .mosi_low               ; If carry clear, leave MOSI low
    ora #SD_MOSI                ; If carry set, set MOSI high
.mosi_low:
    sta VIA_PORTB_STATE

    ; Toggle SCK high
    lda VIA_PORTB_STATE
    ora #SD_SCK
    sta VIA_PORTB_STATE
    sta VIA_PORTB

    ; Toggle SCK low
    lda VIA_PORTB_STATE
    and #~SD_SCK
    sta VIA_PORTB_STATE
    sta VIA_PORTB

    tya                         ; Restore remaining bits
    dex
    bne .write_bit_loop

    rts



sd_waitresult:
  ; Wait for the SD card to return something other than $ff
  jsr sd_readbyte
  cmp #$ff
  beq sd_waitresult
  rts


sd_sendcommand:


  ldx #0
  lda (zp_sd_cmd_address,x)

  lda #SD_MOSI           ; pull CS low to begin command
  sta VIA_PORTA

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



  ; End command
  lda #SD_CS | SD_MOSI   ; set CS high again
  sta VIA_PORTA

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

