; **************************************************************** ;
; NormalLuser 
; BOOM! Ben Eater Breadboard Doom Demo Basic version
; **************************************************************** ;
Programstart      = $300                  ; $F700 ; Page align!
Display           = $2000     ; start of memory mapped display 100x64 mapped to 128x64
VIA               = $6000
VIA_PORTB         = VIA
VIA_PORTA         = VIA+1 ; $6001
VIA_DDRB          = VIA+2 ; $6002
VIA_DDRA          = VIA+3 ; $6003
VIA_AUX           = VIA+11; $600B 
VIA_PCR           = VIA+12; $600C
VIA_IORA          = VIA+15; $600F
ZP                = $1    ; Start ZP at $1 to save the $0 spot for good luck! :)
Block_Counter     = ZP    ; ZP counter for 512 Block CRC bytes
Screen            = ZP + 1; GFX screen location
ScreenH           = ZP + 2; to draw TO
zp_sd_cmd_address = ZP + 3; SD boot
  .org Programstart ; Page align this program in RAM/ROM
Start:
    STZ Screen ; Ben Eater's Worlds Worst Video card 
    LDA #$20   ; uses the upper 8Kb of system RAM
    STA ScreenH; This starts at location $2000
    jmp BootUp ; Returns to FrameLoop below jmp keeps program load and start address the same
FrameLoop:
  dec Block_Counter ; must count every 512 bytes read from SD card
  beq BLOCK        ; 256 roll-over go to BLOCK routine
FrameLoopstart:    ; jmp here after boot and for SD card BLOCK return
  lda VIA_PORTA    ; ReadByte1
  asl           
  ora VIA_PORTA 
  asl
  ora VIA_PORTA
  asl
  ora VIA_PORTA
  asl
  ora VIA_PORTA
  asl
  ora VIA_PORTA
  asl
  ora VIA_PORTA
  asl
  ora VIA_PORTA
    STA (Screen),Y
    INY
    BNE NoInc  ; Do until $40-00
    INC ScreenH
    LDX ScreenH
    CPX #$40   ; Top of screen memory is $3F-FF, 
    BNE NoInc  ; Do until $40-00
    STZ Screen ; Ben Eater's Worlds Worst Video card 
    LDA #$20   ; uses the upper 8Kb of system RAM    
    STA ScreenH; This starts at location $2000
NoInc:
  lda VIA_PORTA; ReadByte2 
  asl
  ora VIA_PORTA 
  asl
  ora VIA_PORTA
  asl
  ora VIA_PORTA
  asl
  ora VIA_PORTA
  asl
  ora VIA_PORTA
  asl
  ora VIA_PORTA
  asl
  ora VIA_PORTA
    STA (Screen),Y
    INY
    BNE NoInc2 ; Do until $40-00
    INC ScreenH
    LDX ScreenH
    CPX #$40   ; Top of screen memory is $3F-FF, 
    BNE NoInc2 ; Do until $40-00
    STZ Screen ; Ben Eater's Worlds Worst Video card 
    LDA #$20   ; uses the upper 8Kb of system RAM    
    STA ScreenH; This starts at location $2000
NoInc2:
   jmp FrameLoop
BLOCK:            ; Block_Counter rolled over -256 count
TossBits:         ; 320 CYCLE, 249 BYTE unrolled Routine. 
TossBitsLen = 249 ; Must throw away 10 bytes every block read from the SD card. No choice!  
 bit VIA_PORTA    ; Read Byte 1
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA   ; Read Byte 2
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA  ; Read Byte 3
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA  ; Read Byte 4
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA  ; Read Byte 5
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA  ; Read Byte 6
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA  ; Read Byte 7
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA  ; Read Byte 8
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA  ; Read Byte 9
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA  ; Read Byte 10
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA  ; Done tossing bytes
 jmp FrameLoopstart ; Back to decoding entry
MSGBadApple: byte "NormalLuser BOOM! Ben Eater Breadboard Doom Demo 2024"
BootUp: ;SD card and system boot routines.These only run once at startup and are not optimized for speed
    cld              ; Clear decimal arithmetic mode
    cli              ; Turn on IRQ
    ldx #$FF         ; Lets start at the top of the stack
    txs              ; Nice and clean,  with room on the bottom if we need it   
; SD card setup:
    stz Block_Counter; So a 16 bit counter it is
    stz VIA_PORTA    ; Port A empty Just in case
    lda #254         ; ??? #$ff-SD_BIT_MISO 
    sta VIA_DDRA     ; Don't remember why I do this??    
    lda #$00
    sta Screen       ; Ben Eater's Worlds Worst Video card 
    lda #$20         ; uses the upper 8Kb of system RAM
    sta ScreenH      ; This starts at location $2000
    ldy #0  
    ldx #0
    lda #255
    sta  VIA_DDRB    ; MAKE EVERYTHING OUTPUT! $6002   
    lda #$0a         ; Make CA2 pulse each time port A is read tnx gfoot!
    sta VIA_PCR
    jsr sd_init      ; Thanks George Foot! Do screwy stuff to get SD card started up in SPI mode 
    lda #SD_MOSI     ; Setup SD card to read forever
    sta  VIA_PORTB 
    lda #$52         ; CMD18 - READ_MULTI_BLOCK
    jsr sd_writebyte
    lda #$00         ; sector 24:31
    jsr sd_writebyte
    lda #$00         ; sector 16:23
    jsr sd_writebyte
    lda #$00         ; sector 8:15
    jsr sd_writebyte
    lda #$00         ; sector 0:7
    jsr sd_writebyte
    lda #77          ; crc (not checked, random data sent)
    jsr sd_writebyte
    jsr sd_waitresult
    cmp #$00
    beq readsuccess
    jmp Start        ; reset ;This change makes it pretty reliable after a reboot
readsuccess:
  jsr sd_waitresult  ; wait for data
  cmp #$fe
  beq SdBooted
  jmp Start         ; Retry until it works
SdBooted:
  lda #SD_MOSI                ;SD card booted, setup some values
  sta VIA_PORTB ; enable card (CS low), set MOSI (resting state), SCK low
  lda VIA_PORTA ; -WE ARE STREAMING BITS FROM THE SD CARD NOW IN PORT A PA0!!
  jmp FrameLoopstart ;Decoder entry point Toss some frames on that screen!

;******************************* SD INIT ************************************ 
SD_CS   = $20 ;  Thanks George Foot! https://githubcom/gfoot/sdcard6502 and for all your help on
SD_MOSI = $8  ;  6502.org forums and https://wwwredditcom/r/beneater/ !!
sd_init:
  lda #SD_CS | SD_MOSI
  ldx #160               ; toggle the clock 160 times, so 80 low-high transitions
preinitloop:
  sta  VIA_PORTB
  ldy VIA_PORTA 
  dex
  bne preinitloop
cmd0: ; GO_IDLE_staTE - resets card to idle state, and SPI mode
  lda #<cmd0_bytes
  sta zp_sd_cmd_address
  lda #>cmd0_bytes
  sta zp_sd_cmd_address+1
  jsr sd_sendcommand
  sta VIA
  cmp #$01    ; Expect status response $01 (not initialized)
  bne sd_init ; initfailed
cmd8: ; SEND_IF_COND - tell the card how we want it to operate (33V, etc)
  lda #<cmd8_bytes
  sta zp_sd_cmd_address
  lda #>cmd8_bytes
  sta zp_sd_cmd_address+1
  jsr sd_sendcommand
  cmp #$01 ; Expect status response $01 (not initialized)
  bne sd_init;initfailed
  jsr sd_readbyte ; Read 32-bit return value, but ignore it
  jsr sd_readbyte
  jsr sd_readbyte
  jsr sd_readbyte
cmd55: ; APP_CMD - required prefix for ACMD commands
  lda #<cmd55_bytes
  sta zp_sd_cmd_address
  lda #>cmd55_bytes
  sta zp_sd_cmd_address+1
  jsr sd_sendcommand
  cmp #$01   ; Expect status response $01 (not initialized) 
  bne sd_init; initfailed
cmd41: ; APP_SEND_OP_COND - send operating conditions, initialize card
  lda #<cmd41_bytes
  sta zp_sd_cmd_address
  lda #>cmd41_bytes
  sta zp_sd_cmd_address+1
  jsr sd_sendcommand
  cmp #$00 ; status response $00 means initialised
  beq initialized
  cmp #$01   ; Otherwise expect status response $01 (not initialized)
  bne sd_init; Not initialized yet, so try again
  nop ; just a nop instead of a delay works with my micro SD cards
  bra cmd55
initialized:
   rts
cmd0_bytes
  byte $40, $00, $00, $00, $00, $95
cmd8_bytes
  byte $48, $00, $00, $01, $aa, $87
cmd55_bytes
  byte $77, $00, $00, $00, $00, $01
cmd41_bytes
  byte $69, $40, $00, $00, $00, $01
sd_readbyte: ;Only used by SD INIT
    lda #SD_MOSI
    sta VIA_PORTB ; set MOSI
    lda VIA_PORTA ; toggle the clock once at the start
    lda VIA_PORTA ; actual load of first bit
    asl
    ora VIA_PORTA 
    asl
    ora VIA_PORTA
    asl
    ora VIA_PORTA
    asl
    ora VIA_PORTA
    asl
    ora VIA_PORTA
    asl
    ora VIA_PORTA
    asl
    ora VIA_IORA;VIA_PORTA ; read last bit without causing a clock pulse Cool!
    rts
sd_writebyte: ; Tick the clock 8 times with descending bits on MOSI
  ldx #8                      ; send 8 bits
swloop:
  asl                         ; shift next bit into carry
  tay                         ; save remaining bits for later
  lda #0
  bcc sendbit                ; if carry clear, don't set MOSI for this bit
  ora #SD_MOSI
sendbit:
  sta VIA_PORTB               ; set MOSI (or not) first with SCK low
  ;eor #SD_SCK                ; Clock not needed anymore
  sta VIA_PORTB               ; raise SCK keeping MOSI the same, to send the bit
  bit VIA_PORTA               ; * New clock for handshake *
  tya                         ; restore remaining bits to send
  dex
  bne swloop                  ; loop if there are more bits to send
  rts
sd_waitresult:
  jsr sd_readbyte            ; Wait for the SD card to return something other than $ff
  cmp #$ff
  beq sd_waitresult
  rts
sd_sendcommand:
  ldx #0
  lda (zp_sd_cmd_address,x)
  lda #SD_MOSI           ; pull CS low to begin command
  sta  VIA_PORTB 
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
  lda #SD_CS | SD_MOSI   ; set CS high again
  sta  VIA_PORTB   ; End command
  pla   ; restore result code
  rts
  ;.byte 'NormalLuser'


