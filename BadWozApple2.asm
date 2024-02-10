; NormalLuser/Fifty1Ford 
; WozMon edit for Bad Apple! Demo testing for 
; Ben Eater 6502 breadboard computer kit with
; Worlds Worst Video Card breadboard kit with
; ACIA Serial kit/addition.
; SD card pins: todo

 .org $A000
    .incbin "C:\Projects\6502\BadAppleONLYMusicROM.bin"

   .org $8000

ACIA        = $4000 
ACIA_CTRL   = ACIA+3
ACIA_CMD    = ACIA+2
ACIA_SR     = ACIA+1
ACIA_DAT    = ACIA

VIA              = $6000
VIA_PORTB        = VIA
VIA_PORTA        = VIA+1;$6001
VIA_DDRB         = VIA+2;$6002
VIA_DDRA         = VIA+3;$6003
VIA_T1CL         = VIA+4;$6004
VIA_T1CH         = VIA+5;$6005
VIA_T1LL         = VIA+6;$6006
VIA_T1LH         = VIA+7;$6007
VIA_T2CL         = VIA+8;$6008
VIA_T2CH         = VIA+9;$6009
VIA_SHIFT        = VIA+10;$600A
VIA_AUX          = VIA+11;$600B ;Set to 0 to stop BEEP ALSO ACR
VIA_PCR          = VIA+12;$600C
VIA_IFR          = VIA+13;$600F
VIA_IER          = VIA+14;$600E
VIA_IORA         = VIA+15;$600F


IN          = $0200          ;*Input buffer
XAML        = $24            ;*Index pointers
XAMH        = $25
STL         = $26
STH         = $27
L           = $28
H           = $29
YSAV        = $2A
MODE        = $2B
MSGL        = $2C
MSGH        = $2D
COUNTER     = $2E
CRC         = $2F
CRCCHECK    = $30

; XAML        = $F2            ;*Index pointers
; XAMH        = $F3
; STL         = $F4
; STH         = $F5
; L           = $F6
; H           = $F7
; YSAV        = $F8
; MODE        = $F9
; MSGL        = $FA
; MSGH        = $FB
; COUNTER     = $FC
; CRC         = $FD
; CRCCHECK    = $FE


;Vsync Beep for Bad Apple!
BeepBuffer   = $1E00;$1800 ;$1F00 ;$1A ;$0200  ;
Display      = $2000     ;Start of memory mapped display. 100x64 mapped to 128x64

;Break zeropage for a sec, dont' want to deal..

;A wire connecting the VGA Vsync signal to the NMI is used for
;BUFV command for a Vsync Buffer switch. It can also be used
;for a 1/60th of a second timer or with user input a random seed.

;Below are chosen because they don't mess up EhBasic.
;Well... now that I commented out all the normal IRQ stuff anyway.
;This allows a Vsync IRQ driven Beep command. For Bad Apple!
;Whoops! EhBasic all kinds of screwy now.. I'll just use WozMon for now.
;Fix for EhBasic maybe someday?

BeepEnable        = $DC  ; ZP ** don't move without changing IRQ code
BeepCount         = $DD  ; ZP ** don't move without changing IRQ code
BeepRead          = $DE  ; ZP ** don't move without changing IRQ code
BeepWrite         = $DF  ; ZP ** don't move without changing IRQ code


;Block_Counter     = $E0  ;ZP counter for 512 (2 bytes/2) Block CRC bytes
VGASync           = $E1;***** IF NMI hooked up to vsync this will be 1 after sync(s), you set to zero yourself *****
VGAClock          = $E2;***** IF NMI hooked up to vsync this will DEC *****

RLECount          = $E3  
ReadByteTemp      = $E4
;BeepFile          = $E5       ; Location of audio in memory
;BeepFileH         = $E6       ;
;LastClock         = $E7       ;ZP
; OldPixY           = $E7       ; MOVE Old Pixel Location y offset
; OldPixC           = $E8       ; MOVE Old Color
; OldPixL           = $E9       ; MOVE Old Pixel Location
; OldPixH           = $EA       ; MOVE memory address/ROW
BackColor         = $EB       ; Background/Color command color

PlotColor         = $EC       ; Color for plot function 
Screen            = $ED       ; GFX screen location
ScreenH           = $EE       ; to draw TO
VidCounter        = $EF

; -START-

RESET
;EDIT
            STZ BeepEnable
            STZ VIA_AUX
;END EDIT
            CLD             ;Clear decimal arithmetic mode.
            CLI
            LDA #$1F        ;* Init ACIA to 19200 Baud.
            STA ACIA_CTRL
            LDA #$0B        ;* No Parity.
            STA ACIA_CMD
            LDA #$0D
            JSR ECHO        ;* New line.
            LDA #$0A
            JSR ECHO
            LDA #<MSG1
            STA MSGL
            LDA #>MSG1
            STA MSGH
            JSR SHWMSG      ;* Show Welcome.
            LDA #$0D
            JSR ECHO        ;* New line.
            LDA #$0A
            JSR ECHO      
SOFTRESET   LDA #$9B        ;* Auto escape.
NOTCR       CMP #$88        ;"<-"? * Note this was chaged to $88 which is the back space key.
            BEQ BACKSPACE   ;Yes.
            CMP #$9B        ;ESC?
            BEQ ESCAPE      ;Yes.
            INY             ;Advance text index.
            BPL NEXTCHAR    ;Auto ESC if >127.
ESCAPE      LDA #$DC        ;"\"
            JSR ECHO        ;Output it.
GETLINE     LDA #$8D        ;CR.
            JSR ECHO        ;Output it.
            LDA #$0A
            JSR ECHO
            LDY #$01        ;Initiallize text index.
BACKSPACE   DEY             ;Backup text index.
            BMI GETLINE     ;Beyond start of line, reinitialize.
            LDA #$A0        ;*Space, overwrite the backspaced char.
            JSR ECHO
            LDA #$88        ;*Backspace again to get to correct pos.
            JSR ECHO
NEXTCHAR    LDA ACIA_SR     ;*See if we got an incoming char
            AND #$08        ;*Test bit 3
            BEQ NEXTCHAR    ;*Wait for character
            LDA ACIA_DAT    ;*Load char
            CMP #$60        ;*Is it Lower case
            BMI CONVERT     ;*Nope, just convert it
            AND #$5F        ;*If lower case, convert to Upper case
CONVERT     ORA #$80        ;*Convert it to "ASCII Keyboard" Input
            STA IN,Y        ;Add to text buffer.
            JSR ECHO        ;Display character.
            CMP #$8D        ;CR?
            BNE NOTCR       ;No.
            LDY #$FF        ;Reset text index.
            LDA #$00        ;For XAM mode.
            TAX             ;0->X.
SETSTOR     ASL             ;Leaves $7B if setting STOR mode.
SETMODE     STA MODE        ;$00 = XAM, $7B = STOR, $AE = BLOK XAM.
BLSKIP      INY             ;Advance text index.
NEXTITEM    LDA IN,Y        ;Get character.
            CMP #$8D        ;CR?
            BEQ GETLINE     ;Yes, done this line.
            CMP #$AE        ;"."?
            BCC BLSKIP      ;Skip delimiter.
            BEQ SETMODE     ;Set BLOCK XAM mode.
            CMP #$BA        ;":"?
            BEQ SETSTOR     ;Yes, set STOR mode.
            CMP #$D2        ;"R"?
            BEQ RUN         ;Yes, run user program.
            CMP #$CC        ;* "L"?
            BEQ LOADINT     ;* Yes, Load Intel Code.
            ; CMP #$D0        ;* "P"?
            ; BEQ PURGE       ;* Yes, Purge Memory.
            STX L           ;$00->L.
            STX H           ; and H.
            STY YSAV        ;Save Y for comparison.
NEXTHEX     LDA IN,Y        ;Get character for hex test.
            EOR #$B0        ;Map digits to $0-9.
            CMP #$0A        ;Digit?
            BCC DIG         ;Yes.
            ADC #$88        ;Map letter "A"-"F" to $FA-FF.
            CMP #$FA        ;Hex letter?
            BCC NOTHEX      ;No, character not hex.
DIG         ASL
            ASL             ;Hex digit to MSD of A.
            ASL
            ASL
            LDX #$04        ;Shift count.
HEXSHIFT    ASL             ;Hex digit left MSB to carry.
            ROL L           ;Rotate into LSD.
            ROL H           ;Rotate into MSD's.
            DEX             ;Done 4 shifts?
            BNE HEXSHIFT    ;No, loop.
            INY             ;Advance text index.
            BNE NEXTHEX     ;Always taken. Check next character for hex.
NOTHEX      CPY YSAV        ;Check if L, H empty (no hex digits).
            BNE NOESCAPE    ;* Branch out of range, had to improvise...
            JMP ESCAPE      ;Yes, generate ESC sequence.

RUN         JSR ACTRUN      ;* JSR to the Address we want to run.
            JMP SOFTRESET   ;* When returned for the program, reset EWOZ.
ACTRUN      JMP (XAML)      ;Run at current XAM index.

LOADINT     JSR LOADINTEL   ;* Load the Intel code.
            JMP SOFTRESET   ;* When returned from the program, reset EWOZ.

; PURGE       ;JSR PURGEMEMORY ;* EmptyMemory.
;             JMP PURGEMEMORY ;* EmptyMemory.
;             ;JMP SOFTRESET   ;* When returned from the program, reset EWOZ.
NOESCAPE    BIT MODE        ;Test MODE byte.
            BVC NOTSTOR     ;B6=0 for STOR, 1 for XAM and BLOCK XAM
            LDA L           ;LSD's of hex data.
            STA (STL, X)    ;Store at current "store index".
            INC STL         ;Increment store index.
            BNE NEXTITEM    ;Get next item. (no carry).
            INC STH         ;Add carry to 'store index' high order.
TONEXTITEM  JMP NEXTITEM    ;Get next command item.
NOTSTOR     BMI XAMNEXT     ;B7=0 for XAM, 1 for BLOCK XAM.
            LDX #$02        ;Byte count.
SETADR      LDA L-1,X       ;Copy hex data to
            STA STL-1,X     ;"store index".
            STA XAML-1,X    ;And to "XAM index'.
            DEX             ;Next of 2 bytes.
            BNE SETADR      ;Loop unless X = 0.
NXTPRNT     BNE PRDATA      ;NE means no address to print.
            LDA #$8D        ;CR.
            JSR ECHO        ;Output it.
            LDA #$0A
            JSR ECHO
            LDA XAMH        ;'Examine index' high-order byte.
            JSR PRBYTE      ;Output it in hex format.
            LDA XAML        ;Low-order "examine index" byte.
            JSR PRBYTE      ;Output it in hex format.
            LDA #$BA        ;":".
            JSR ECHO        ;Output it.
PRDATA      LDA #$A0        ;Blank.
            JSR ECHO        ;Output it.
            LDA (XAML,X)    ;Get data byte at 'examine index".
            JSR PRBYTE      ;Output it in hex format.
XAMNEXT     STX MODE        ;0-> MODE (XAM mode).
            LDA XAML
            CMP L           ;Compare 'examine index" to hex data.
            LDA XAMH
            SBC H
            BCS TONEXTITEM  ;Not less, so no more data to output.
            INC XAML
            BNE MOD8CHK     ;Increment 'examine index".
            INC XAMH
MOD8CHK     LDA XAML        ;Check low-order 'exainine index' byte
            AND #$0F        ;For MOD 8=0 ** changed to $0F to get 16 values per row **
            BPL NXTPRNT     ;Always taken.
PRBYTE      PHA             ;Save A for LSD.
            LSR
            LSR
            LSR             ;MSD to LSD position.
            LSR
            JSR PRHEX       ;Output hex digit.
            PLA             ;Restore A.
PRHEX       AND #$0F        ;Mask LSD for hex print.
            ORA #$B0        ;Add "0".
            CMP #$BA        ;Digit?
            BCC ECHO        ;Yes, output it.
            ADC #$06        ;Add offset for letter.
ECHO        PHA             ;*Save A
            AND #$7F        ;*Change to "standard ASCII"
            STA ACIA_DAT    ;*Send it.
.WAIT       LDA ACIA_SR     ;*Load status register for ACIA
            AND #$10        ;*Mask bit 4.
            BEQ    .WAIT    ;*ACIA not done yet, wait.
            PLA             ;*Restore A
            RTS             ;*Done, over and out...

SHWMSG      LDY #$0
.PRINT      LDA (MSGL),Y
            BEQ .DONE
            JSR ECHO
            INY 
            BNE .PRINT
.DONE       RTS 


; Load an program in Intel Hex Format.
LOADINTEL   LDA #$0D
            JSR ECHO        ;New line.
            LDA #<MSG2
            STA MSGL
            LDA #>MSG2
            STA MSGH
            JSR SHWMSG      ;Show Start Transfer.
            LDA #$0D
            JSR ECHO        ;New line.
            LDY #$00
            STY CRCCHECK    ;If CRCCHECK=0, all is good.
INTELLINE   JSR GETCHAR     ;Get char
            STA IN,Y        ;Store it
            INY             ;Next
            CMP #$1B        ;Escape ?
            BEQ INTELDONE   ;Yes, abort.
            CMP #$0D        ;Did we find a new line ?
            BNE INTELLINE   ;Nope, continue to scan line.
            LDY #$FF        ;Find (:)
FINDCOL     INY
            LDA IN,Y
            CMP #$3A        ; Is it Colon ?
            BNE FINDCOL     ; Nope, try next.
            INY             ; Skip colon
            LDX #$00        ; Zero in X
            STX CRC         ; Zero Check sum
            JSR GETHEX      ; Get Number of bytes.
            STA COUNTER     ; Number of bytes in Counter.
            CLC             ; Clear carry
            ADC CRC         ; Add CRC
            STA CRC         ; Store it
            JSR GETHEX      ; Get Hi byte
            STA STH         ; Store it
            CLC             ; Clear carry
            ADC CRC         ; Add CRC
            STA CRC         ; Store it
            JSR GETHEX      ; Get Lo byte
            STA STL         ; Store it
            CLC             ; Clear carry
            ADC CRC         ; Add CRC
            STA CRC         ; Store it
            LDA #$2E        ; Load "."
            JSR ECHO        ; Print it to indicate activity.
NODOT       JSR GETHEX      ; Get Control byte.
            CMP #$01        ; Is it a Termination record ?
            BEQ INTELDONE   ; Yes, we are done.
            CLC             ; Clear carry
            ADC CRC         ; Add CRC
            STA CRC         ; Store it
INTELSTORE  JSR GETHEX      ; Get Data Byte
            STA (STL,X)     ; Store it
            CLC             ; Clear carry
            ADC CRC         ; Add CRC
            STA CRC         ; Store it
            INC STL         ; Next Address
            BNE TESTCOUNT   ; Test to see if Hi byte needs INC
            INC STH         ; If so, INC it.
TESTCOUNT   DEC COUNTER     ; Count down.
            BNE INTELSTORE  ; Next byte
            JSR GETHEX      ; Get Checksum
            LDY #$00        ; Zero Y
            CLC             ; Clear carry
            ADC CRC         ; Add CRC
            BEQ INTELLINE   ; Checksum OK.
            LDA #$01        ; Flag CRC error.
            STA CRCCHECK    ; Store it
            JMP INTELLINE   ; Process next line.

INTELDONE   LDA CRCCHECK    ; Test if everything is OK.
            BEQ OKMESS      ; Show OK message.
            LDA #$0D
            JSR ECHO        ; New line.
            LDA #<MSG4      ; Load Error Message
            STA MSGL
            LDA #>MSG4
            STA MSGH
            JSR SHWMSG      ; Show Error.
            LDA #$0D
            JSR ECHO        ; New line.
            RTS

OKMESS      LDA #$0D
            JSR ECHO        ; New line.
            LDA #<MSG3      ; Load OK Message.
            STA MSGL
            LDA #>MSG3
            STA MSGH
            JSR SHWMSG      ; Show Done.
            LDA #$0D
            JSR ECHO        ; New line.
            RTS

GETHEX      LDA IN,Y        ; Get first char.
            EOR #$30
            CMP #$0A
            BCC DONEFIRST
            ADC #$08
DONEFIRST   ASL
            ASL
            ASL
            ASL
            STA L
            INY
            LDA IN,Y        ; Get next char.
            EOR #$30
            CMP #$0A
            BCC DONESECOND
            ADC #$08
DONESECOND  AND #$0F
            ORA L
            INY
            RTS

GETCHAR     LDA ACIA_SR     ; See if we got an incoming char
            AND #$08        ; Test bit 3
            BEQ GETCHAR     ; Wait for character
            LDA ACIA_DAT    ; Load char
            RTS



MSG1        .byte "BadApple EWOZ F700 R v1",0
MSG2        .byte "Start Intel Hex code Transfer.",0
MSG3        .byte "Intel Hex Imported OK.",0
MSG4        .byte "Intel Hex Imported with checksum error.",0

PURGEMEMORY
                        
            LDA #$0
            STA $FF;PointerH
            STZ $FE ;Pointer
            LDY #0 
            LDA #0
.MemLoopZ
            STA ($FE),Y
            INY
            CPY #$FE
            BNE .MemLoopZ

            LDA #$03
            STA $FF;PointerH
            STZ $FE ;Pointer
            LDY #0 
            LDA #0
.MemLoop
            STA ($FE),Y
            INY
            BNE .MemLoop
            INC $FF
            LDX $FF
            CPX #$40
            BNE .MemLoop
            STZ $FF ;Clear High byte of pointer in ZP
            ;BRK
            ;RTS
            JMP $8000;RESET

;  .org $9000 ;Need to remember where this is.
; LAB_CLS
;       PHP ;make call-able from other stuff?
;       PHA ;Seems to have issues, 
;       PHX ;Push/Pop
;       PHY 

      
;       ;ADDED BEEP CLEAR 
;       STZ   VIA_AUX;$600b;needs to be via address for PB7 timer
;       LDX #0
;       BRA FillScreen
 
;  .org $9010 ;Lets keep it in 1 spot even if I remove or change beep clear above 
; LAB_COLOR;Fifty1Ford
;       PHP ;make call-able from other stuff
;       PHA
;       PHX 
;       PHY 
; ;       ;JSR LAB_GTBY ;get byte in X
; ;       STX BackColor ;lets store the background color
;        LDX BackColor ;lets LOAD the background color
; ;       ;Lets claw back a few OPS from GTBY and just fall through
; ;       ;BRA FillScreen
; ;       ;RTS

 
; ;Best FillScreen I could manage?? Smaller is easy, but can you do faster?
; ;32,910 cycles, follows the scanbeam. 
; ;100 Cycles less than split routine. Around 1/3 Cycles of orgional loop. 
; ;IE unrolled like this is 3x faster!

; FillScreen:
;       TXA ;Color stored in X
;       ;We'll DEC instead of a INC and CMP to save cycles.
;       LDX #255 ;one more than needed because of DEX below IE 0    
; FillScreenLoop:
;       ;DEX ;DEX up here so we can clear the 0 row
;       ;PHP
;       INX
;   	;This is unrolled so that there is a STA for each row of the screen.
;       ;Display mapping has 28 bytes at end of each row unused.
;       ;Display location = $2000
;       STA Display, x 
;       STA Display +$80,x
; 	STA Display +$100,x
; 	STA Display +$180,x
; 	STA Display +$200,x
; 	STA Display +$280,x
; 	STA Display +$300,x
; 	STA Display +$380,x
; 	STA Display +$400,x
; 	STA Display +$480,x
; 	STA Display +$500,x
; 	STA Display +$580,x
; 	STA Display +$600,x
; 	STA Display +$680,x
; 	STA Display +$700,x
; 	STA Display +$780,x
; 	STA Display +$800,x
; 	STA Display +$880,x
; 	STA Display +$900,x
; 	STA Display +$980,x
; 	STA Display +$A00,x
; 	STA Display +$A80,x
; 	STA Display +$B00,x
; 	STA Display +$B80,x
; 	STA Display +$C00,x
; 	STA Display +$C80,x
; 	STA Display +$D00,x
; 	STA Display +$D80,x
; 	STA Display +$E00,x
; 	STA Display +$E80,x
; 	STA Display +$F00,x
; 	STA Display +$F80,x
; 	;MIDDLE ROW 31
; 	STA Display +$1000,x
; 	STA Display +$1080,x
; 	STA Display +$1100,x
; 	STA Display +$1180,x
; 	STA Display +$1200,x
; 	STA Display +$1280,x
; 	STA Display +$1300,x
; 	STA Display +$1380,x
; 	STA Display +$1400,x
; 	STA Display +$1480,x
; 	STA Display +$1500,x
; 	STA Display +$1580,x
; 	STA Display +$1600,x
; 	STA Display +$1680,x
; 	STA Display +$1700,x
; 	STA Display +$1780,x
; 	STA Display +$1800,x
; 	STA Display +$1880,x
; 	STA Display +$1900,x
; 	STA Display +$1980,x
; 	STA Display +$1A00,x
; 	STA Display +$1A80,x
; 	STA Display +$1B00,x
; 	STA Display +$1B80,x
; 	STA Display +$1C00,x
; 	STA Display +$1C80,x
; 	STA Display +$1D00,x
; 	STA Display +$1D80,x
; 	STA Display +$1E00,x
; 	STA Display +$1E80,x
; 	STA Display +$1F00,x
; 	STA Display +$1F80,x
; 	CPX #99
; 	BEQ FillScreenLoopEnd
; 	JMP FillScreenLoop
; FillScreenLoopEnd:
;       PLY ;Pop/Pull
;       PLX 
;       PLA 
;       PLP 
      
; 	RTS
;     ;JMP SOFTRESET


NMI:
    ; NMI:;
; BadApple Beep. Broken IRQ stuff anyway...  
; For my Ben Eater Breadboard Bad Apple! Demo I need a IRQ driven
; music routine. I'm still using EhBASIC as my 'Testbed' doing 
; tests and loading the test programs, etc. I try to stay out of the
; way of the rest of EhBASIC, but who knows.....   
; These stomp on it but are only read if we turn on the buffer.
; Maybe I'll build Wozmon into EhBasic at some point?
; VGAClock    = $E2  ;ZP
; BeepBuffer   = $1E00;$1800 ;$1F00 ;$1A ;$0200  ;
; BeepEnable        = $DC ;** don't move without changing IRQ code
; BeepCount         = $DD  ;ZP
; BeepRead          = $DE  ;ZP
; BeepWrite         = $DF  ;ZP
      ;PHP ;Push that CPU flag
      PHA 
      PHX ;I don't actually mess with x
      PHY
      
      ;This is reset by user program.
      ;Always just a '1' if vsync happened since reset
      LDA #1
      STA VGASync
      ;Do I need this CLC??

      CLC ;needed??
      DEC VGAClock ;VGACounter Lets dec ;$E2 

      LDA BeepEnable    ; BeepEnable/IRQ ;$DF
      BEQ NMIExit       ; IRQ Beep/buffer off

      LDA BeepCount     ; 
      BEQ NMIEmptyBuffer; Exit if there is nothing in the buffer
      
      
      CLC               ; Do I need this CLC??
      DEC BeepCount
      
      ;Kinda' a waste of cycles, but what can you do?
      ;Setup 'Beep' on PB7 to make sure it is on.
      LDA VIA_AUX;$600b
      ORA #$C0   ;1100-0000; square-wave/RECTANGLE output on PB7.  (Don't enable the
      STA VIA_AUX;$600b    ; T1 interrupt in the IER though.)
      LDA #255           ; Set the T1 timeout period. 
      STA VIA_T1CL;$6004

      ;Need to load the note to see if it is Zero 0
      LDX BeepRead; Load Buffer Location Pointer
      
      ;Do I need this??
      CLC
      INC BeepRead; INC Pointer for next time.      
      
      LDA BeepBuffer,X ;(BeepBuffer),y(buffer pointer)
      BEQ NMINoBeep ; on 0 stop Beep/PB7 timer
      
      ;OK, lets store the note
      STA VIA_T1CH;$6005;
      ;BEEP!
      ;sta ACIA ;$5000 ;testing
      BRA NMIExit

NMINoBeep
      STZ VIA_AUX ;$600b ;Beep Off
NMIExit
      ;CPY #99;99 byte buffer
      ;BEQ NMIResetBuffer
      
      PLY
      PLX ;I don't actually mess with x
      PLA
      ;PLP ;Pull that flag!
      RTI
NMIEmptyBuffer
      STZ BeepEnable
      ;STZ VIA_AUX
      PLY
      PLX ;I don't actually mess with x
      PLA
      ;PLP ;Pull that flag!
      RTI

IRQ:
            RTI



; Fifty1Ford/NormalLuser Ben Eater Breadboard 6502 Bad Apple! Demo
;
; 1 Byte encode with Run Length, Differential and Tri Pixel Encoding
; Zero page self modifying RLE.
; 53.4 Frames a second average decode without vsync/beep
; 20:1 compression ratio python encoder
; Automatic 30 FPS Vsync with catch-up
;
; Hardware: Start with stock Ben Eater 6502+Worlds Worst Video Card kits
; You will need 2 jumper wires. Extra bypass capacitors may help with stability.
;
; ***************** NOTE *****************************************
;
; A wire connecting the VGA Vsync signal to the NMI pin on the CPU
; is used for a Vsync IRQ. 
; If this is not connected the system will hang.
;
; ****************************************************************
;
; Disconnect LCD and clock your system to 5 mhz. 
; Use first counter output of VGA for 5mhz CPU clock instead of the 1 Mhz clock.
; Run in both hsync and vsync (1 pixels on left side will have a noise line on top at 5mhz)
; This gives you 1.4Mhz effective CPU speed.
; I disconnect the little LCD and any buttons from the VIA while working with this.
; Connect the Vsync from the VGA output to the NMI pin of the CPU
; SD Card input is PA0 MISO
; to make the serial code FAST.
; I also have pulse mode on reads
; USE CA2 as clock to save more cycles!!!
; IE FAST..
; Others are pins on VIA Port B 
; Leave Port A empty except for MISO.
; Did you connect the Vsync to the NMI?
; SD CARD ROUTINE BASED ON GFOOT SD CARD 6502 GUIDE AND HELP
;
; Extra bypass capacitors may help with stability.
;
; ***************** NOTE *****************************************
;
; Boot ROM must have a NMI rouine with :
;  DEC $E2
;  Music routine needed in boot rom for music to play.
; ****************************************************************

ProgramStart    = $F700 ; Page align!
RLERenderStart  = ProgramStart + 768  ; Page align!
RLEArrayStart   = RLERenderStart + 256; Page align!
RelocateLocation= $0 ;Zero Page Baby!   40 bytes


; *********************** DEBUG OPTIONS **************************
VsyncOn=1
Debug=1
BeepOn=1
; ****************************************************************

; VIA              = $6000
; VIA_PORTB        = VIA
; VIA_PORTA        = VIA+1;$6001
; VIA_DDRB         = VIA+2;$6002
; VIA_DDRA         = VIA+3;$6003
; VIA_AUX          = VIA+11;$600B ;Set to 0 to stop BEEP ALSO ACR
; VIA_PCR          = VIA+12;$600C
; VIA_IORA         = VIA+15;$600F
SD_CS   = $20 ;%00100000
SD_MOSI = $8  ;%00001000
; SD_SCK  = $10 ;%00010000;Clock moved to CA2
; Display      = $2000     ;Start of memory mapped display. 100x64 mapped to 128x64

zp_sd_cmd_address = $F0
; ZP addresses hold over from availible EhBasic ZP 
; Vsync Beep for Bad Apple!** don't move without changing IRQ code
; VGASync           = $E1 ;ZP ***** IF NMI hooked up to vsync this will be 1 after sync(s), you set to zero yourself.
;VGAClock          = $E2   ;ZP ***** IF NMI hooked up to vsync this will DEC *****
; Beep/Music related:
; BeepEnable        = $DC  ;ZP ** don't move without changing IRQ code
; BeepCount         = $DD  ;ZP ** don't move without changing IRQ code
; BeepRead          = $DE  ;ZP ** don't move without changing IRQ code
; BeepWrite         = $DF  ;ZP ** don't move without changing IRQ code
;BeepBuffer        = $1E00 ;  ** don't move without changing IRQ code

BeepFile          = $E5       ; Location of audio in memory
BeepFileH         = $E6       ;

;RLECount          = $E3  
;PlotColor         = $EC       ; Color for plot function 
;Screen            = $ED       ; GFX screen location
;ScreenH           = $EE       ; to draw TO
Block_Counter     = $E0       ; ZP counter for 512 Block CRC bytes
BlockTwo          = $EF       ; Counting to 256 twic



  .ORG ProgramStart;$300; Page align this program in RAM/ROM.
.reset:
  jmp .BootUp ;Returns to FrameLoop below. Keeps program load and start address the same.


.Array1: ; Putting this here page aligns below for fast code without using zero page.
    .byte 0,0,0,21,0,0,42,0,0,63,0,21,21,0,0,21,21,42,42,0,0,21,21,63,63,0,42,42,0,0,42,42,63,63,0,63,63,21,21,21,42,21,21,63,21,42,42,21,21,42,42,63,63,21,63,63,42,42,42,63,42,63,63,63
.Array2: ; Keeps it the same 4 cycles as zero page (Array1),x 
    .byte 0,0,21,0,0,42,0,0,63,0,21,0,21,21,42,0,42,0,21,21,63,0,63,0,21,42,0,42,42,63,0,63,0,42,63,0,63,21,21,42,21,21,63,21,42,21,42,42,63,21,63,21,42,63,21,63,42,42,63,42,63,42,63,63
.Array3: ; This is 192 bytes of all possible 3 Greyscale values.
    .byte 0,21,0,0,42,0,0,63,0,0,21,21,0,42,21,42,0,21,0,63,21,63,0,21,0,42,42,0,63,42,63,0,42,0,63,63,0,21,42,21,21,63,21,21,42,42,21,63,42,63,21,42,21,63,63,21,42,63,42,42,63,63,42,63


;****************************************************************************
;************************* MAIN DECODE LOOP ********************************* 
;****************************************************************************

.FrameLoop:
  dec Block_Counter ; must count 512 bytes
  beq .BLOCK        ; 256 roll-over go to BLOCK routine
.FrameLoopStart:     ; JMP here to start and for SD card BLOCK return

; Load Control Bit 1
  lda VIA_PORTA; Read bit 8 of byte from SD card. 
  bne .SkipRun ; If 1 Skip Pixels
; Load Control Bit 2  
  lda VIA_PORTA ; Else check bit 7
  beq .TriPixel ; If 0 TriPixel
  jmp .RLE      ; Else 1. New RLE self modifies, needs to be in RAM.

.TriPixel:      ; 3 pixel run
  lda VIA_PORTA ; Load 6 bits for up to 64 values for each array
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
    
  tax           ; Color/index to x
  lda .Array1,x ; Load 1 of 4 colors B/DG/LG/W for Bad Apple!
  sta (Screen),y; Draw it!
  iny           ; Next color
  lda .Array2,x ; Load 1 of 4 colors 
  sta (Screen),y; Draw it!
  iny           ; Next color
  lda .Array3,x ; Load 1 of 4 colors 
  sta (Screen),y; Draw it! 
  sta PlotColor ; Store last color used for Repeats
.TriDone:
  jmp .FrameLoop ; Decode another byte

.BLOCK:        ; Block_Counter rolled over -256 count
  inc BlockTwo ; We need to count to 512
  lda BlockTwo ; So we will do this twice
  cmp #2       ; 
  beq .TossBits; It's been 512 bytes, toss the bits
  jmp .FrameLoopStart; only 256, keep going

.SkipRun: ;Just add this amount to the screen pointer
; Load 7 bits for a skip value up to 127
 lda VIA_PORTA
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
  sta RLECount;Store because of adc of y below
  clc 
  tya 
  adc RLECount; Need to add RLE count to Y  
  tay         ; 
  lda ScreenH ; increment the top byte of the screen pointer
  adc #$00    ; 
  sta ScreenH ; Since we always encode skips on the edge of the screen we only need to check
  cmp #$40    ; for the screen roll-over at $4000 in the Skip routine.   
  beq .sRstTop; We never roll-over while in the draw routines becuse we are always on-screen. 
  jmp .FrameLoop;Decode next Byte
.sRstTop:     ; Frame Roll-Over
;****************************************************************************
;***************************** FRAME SYNC *********************************** 
;****************************************************************************
  lda #$20   ;New Frame starts at $2000   
  sta ScreenH;Reset screen pointer to first pixel
; Since we just rolled-over the screen buffer that means that we are done with the frame.
; Do Vsync routine below. 

; .Vsync: ;Org Vsync routine without Debug.
;   ldx VGAClock ;IRQ does a DEC on Vsync
;   cpx #250     ;Using this as 'zero' sync
;   bcs .EGVsync ; = > 250 wait or sync
; .Synced:; Less than 250 no wait. Try to catch up.
;   inc VGAClock ;Add two for 30 Frames a Second
;   inc VGAClock ;VGA is 60 Frames a Second 
;   jsr .GetBeep ;Put two notes per frame in music buffer
;   jsr .GetBeep ;60 vsync a sec on VGA with 30 FPS so 2 notes
;   jmp .FrameLoop;** Start next frame decode **
; .EGVsync: ; = > 250 It is a wait or a sync
;   beq .Synced  ; = 250 ;Good Vsync. Done waiting.
;   jmp .Vsync   ; > 250 ;Wait for Vsync

.Vsync: ; Debug version. I use assembler ' if ' statments here for 
        ; debug with no Vsync and a  'slow' pixel
 .if VsyncOn=1
    ldx VGAClock ;IRQ does a DEC on Vsync
    cpx #250     ;Using this as 'zero' sync
    bcs .EGVsync ; = > 250 wait or sync   
    .if Debug=1
     lda #$20 ; Red
     sta $2002; Top Left
     jmp .NextSync; Less than 250 no wait. Try to catch up.
    .endif
.Synced:
    .if Debug=1
     lda #$0  ; Black
     sta $2002; Top Left
    .endif
.NextSync:
    inc VGAClock ;Add two for 30 Frames a Second
    inc VGAClock ;VGA is 60 Frames a Second 
    ;This only runs once per frame, so I won't unroll yet.
    .if BeepOn=1
        jsr .GetBeep ;Put two notes per frame in music buffer
        jsr .GetBeep ;60 vsync a sec on VGA with 30 FPS so 2 notes
    .endif
    jmp .FrameLoop;** Start next frame decode **
.EGVsync: ; = > 250 It is a wait or a sync
    beq .Synced  ; = 250 ;Good Vsync. Done waiting.
    jmp .Vsync   ; > 250 ;Wait for Vsync 
  .endif; if VsyncOn=1
  .if VsyncOn=0
   jmp .FrameLoop;** Start next frame decode **
  .endif

.TossBits:;320 CYCLES This routine is hard to place in a spot I can easily branch to 
  stz BlockTwo  ;     Reset top Byte Counter, we know bottom rolled over to 0
; Must throw away 10 bytes every block read from the SD card. No choice. 
  bit VIA_PORTA 
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA

  ;Read a Byte
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA

  ;Read a Byte
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA

  ;Read a Byte
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA

  ;Read a Byte
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA

  ;Read a Byte
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA

  ;Read a Byte
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA

  ;Read a Byte
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA

  ;Read a Byte
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA

  ;Read a Byte
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
; Done tossing bytes
  jmp .FrameLoopStart ; Back to decoding entry


 .if BeepOn=1 ; Debug option to turn off sound
.GetBeep: ;Beep is only run twice per frame, no real need to optimize/unroll, but I could clean it up some...
  PHY     
  PHP
  LDY #0
  LDA (BeepFile),y
  JSR .Beep
  CLC  
  LDA BeepFile
  ADC #$01
  STA BeepFile
; inc memory high
; Use carry from prior
  LDA BeepFileH
  ADC #$00
  STA BeepFileH
  CMP #$D3
  BEQ .BeepFileRstTop
  BRA .BeepDone
.BeepFileRstTop
  LDA #$A0
  STA BeepFileH	
.BeepDone:
  PLP
  PLY
  rts

.Beep:
    ; Actual Beep moved to system IRQ. Right now I will assume that
    ; the decoder bootup will send some number of Audio packets before 
    ; starting to show frames. This will fill the buffer.
      PHA ;?? Do I really need this?
      PHX
      
      LDX BeepWrite
      STA BeepBuffer,x
      
      INC BeepWrite  

      ;Last thing
      INC BeepCount
      ;BEQ .StartBeep ;Full Buffer
      
.StartBeep
      LDA #1
      STA BeepEnable
.BeepExit
      PLX
      PLA
      RTS
 .endif ;If BeepOn

;****************************************************************************
;************************** ZP RELOCATED CODE *******************************
;****************************************************************************
RelocateLength=40       ; Try 6 extra bytes?
.RelocateCode:          ; 34 byte function in ZP.
 .rorg RelocateLocation ; ZeroPage location
.RLE:                   ; Run Length Draw Function
; Load 6 bits for a repeat value of up to 64
  lda VIA_PORTA
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

  tax ; RLE count
  LDA .RLEArray,x ; Load JMP location
  STA .RLEJump+1  ; Store low byte location
  lda PlotColor   ; Last pixel color used
.RLEJump:         ; ZP Self modify!  
  JMP .RLERender  ; Jump to count in RLE routine.
.RLEend: ;Pad after relocate code for testing.
 .byte 1,2,3,4,5,6,7,8,9,0;Just some padding
 .rend            ;End Relocation
;****************************************************************************
;************************** END RELOCATED CODE ******************************
;****************************************************************************

  .ORG RLERenderStart;RLERender must start on page!
.RLERender:  ; Unrolled Line draw/Run Length Routine.
; This is around 8.2 cycles a pixel vrs 13.3 for a loop
  sta (Screen),y; Draw it! 64
  iny ; Next pixel
 sta (Screen),y; Draw it! 63
  iny ; Next pixel
  sta (Screen),y; Draw it! 62
  iny ; Next pixel
  sta (Screen),y; Draw it! 61
  iny ; Next pixel
  sta (Screen),y; Draw it! 60
  iny ; Next pixel
  sta (Screen),y; Draw it! 59
  iny ; Next pixel 
  sta (Screen),y; Draw it! 58
  iny ; Next pixel
  sta (Screen),y; Draw it! 57
  iny ; Next pixel
  sta (Screen),y; Draw it! 56
  iny ; Next pixel
  sta (Screen),y; Draw it! 55
  iny ; Next pixel
  sta (Screen),y; Draw it! 54
  iny ; Next pixel
  sta (Screen),y; Draw it! 53
  iny ; Next pixel
  sta (Screen),y; Draw it! 52
  iny ; Next pixel
  sta (Screen),y; Draw it! 51
  iny ; Next pixel
  sta (Screen),y; Draw it! 50
  iny ; Next pixel
  sta (Screen),y; Draw it! 49
  iny ; Next pixel
 
 sta (Screen),y; Draw it! 48
  iny ; Next pixel
  sta (Screen),y; Draw it! 47
  iny ; Next pixel
  sta (Screen),y; Draw it! 46
  iny ; Next pixel
  sta (Screen),y; Draw it! 45
  iny ; Next pixel
  sta (Screen),y; Draw it! 44
  iny ; Next pixel
  sta (Screen),y; Draw it! 43
  iny ; Next pixel
  sta (Screen),y; Draw it! 42
  iny ; Next pixel
  sta (Screen),y; Draw it! 41
  iny ; Next pixel
  sta (Screen),y; Draw it! 40
  iny ; Next pixel
  sta (Screen),y; Draw it! 39
  iny ; Next pixel
  sta (Screen),y; Draw it! 38
  iny ; Next pixel
  sta (Screen),y; Draw it! 37
  iny ; Next pixel
  sta (Screen),y; Draw it! 36
  iny ; Next pixel
  sta (Screen),y; Draw it! 35
  iny ; Next pixel
  sta (Screen),y; Draw it! 34
  iny ; Next pixel
  sta (Screen),y; Draw it! 33
  iny ; Next pixel
  
 sta (Screen),y; Draw it! 32
  iny ; Next pixel
  sta (Screen),y; Draw it! 31
  iny ; Next pixel
  sta (Screen),y; Draw it! 30
  iny ; Next pixel
  sta (Screen),y; Draw it! 29
  iny ; Next pixel
  sta (Screen),y; Draw it! 28
  iny ; Next pixel
  sta (Screen),y; Draw it! 27
  iny ; Next pixel
  sta (Screen),y; Draw it! 26
  iny ; Next pixel
  sta (Screen),y; Draw it! 25
  iny ; Next pixel
  sta (Screen),y; Draw it! 24
  iny ; Next pixel
  sta (Screen),y; Draw it! 23
  iny ; Next pixel
  sta (Screen),y; Draw it! 22
  iny ; Next pixel
  sta (Screen),y; Draw it! 21
  iny ; Next pixel
  sta (Screen),y; Draw it! 20
  iny ; Next pixel
  sta (Screen),y; Draw it! 19
  iny ; Next pixel
  sta (Screen),y; Draw it! 18
  iny ; Next pixel
  sta (Screen),y; Draw it! 17
  iny ; Next pixel
 
 sta (Screen),y; Draw it! 16
  iny ; Next pixel
  sta (Screen),y; Draw it! 15
  iny ; Next pixel
  sta (Screen),y; Draw it! 14
  iny ; Next pixel
  sta (Screen),y; Draw it! 13
  iny ; Next pixel
  sta (Screen),y; Draw it! 12
  iny ; Next pixel
  sta (Screen),y; Draw it! 11
  iny ; Next pixel
  sta (Screen),y; Draw it! 10
  iny ; Next pixel
  sta (Screen),y; Draw it! 9
  iny ; Next pixel
  sta (Screen),y; Draw it! 8
  iny ; Next pixel
  sta (Screen),y; Draw it! 7
  iny ; Next pixel
  sta (Screen),y; Draw it! 6
  iny ; Next pixel
  sta (Screen),y; Draw it! 5
  iny ; Next pixel
  sta (Screen),y; Draw it! 4
  iny ; Next pixel
  sta (Screen),y; Draw it! 3
  iny ; Next pixel
  sta (Screen),y; Draw it! 2
  iny ; Next pixel
  sta (Screen),y; Draw it! 1
  iny ; Next pixel
 jmp .FrameLoop ; Decode another byte
  
  .ORG RLEArrayStart;Page align
 ; I could Bit shift and ADC then subtract from the length of hte routine
 ; instead of using this table, but that would take more cycles than this.
.RLEArray: ;Decending Array of 3 byte skips. I don't think I actually use zero? 
 ; I should use 0 for ' 1 ' but just dup first entry for now.
    .BYTE 189,189,186,183,180,177,174,171,168,165,162
        .BYTE 159,156,153,150,147,144,141,138,135,132
        .BYTE 129,126,123,120,117,114,111,108,105,102
        .BYTE  99, 96, 93, 90, 87, 84, 81, 78, 75, 72
        .BYTE  69, 66, 63, 60, 57, 54, 51, 48, 45, 42
        .BYTE  39, 36, 33, 30, 27, 24, 21, 18, 15, 12
    .BYTE       9,  6,  3,  0,  0 ;extra zero just in case.


;****************************************************************************
;********************************* BOOT *************************************
;****************************************************************************

.BootUp ;SD card and system boot routines
        ;These only run once at startup and are not optimized.
;********************************* Zero Page Copy *************************************
;Need to copy the RLE self modify to ram
 ldx #0
.RelocateLoop ;routine to move code
 LDA .RelocateCode,x
 STA RelocateLocation,x ;$0,x
 inx
 cpx #RelocateLength
 bne .RelocateLoop


; Some SD card setup:
; Need to read 512 bytes. Then throw away 10 CRC bytes
; Unrolling might help a tiny bit but it seems silly to
; double up the entire decoder just to do that?
    stz Block_Counter; So a 16 bit counter it is..
    stz BlockTwo     ; Upper byte of counter.
    stz VIA_PORTA    
    lda #254         ; ??? #$ff-SD_BIT_MISO 
    sta VIA_DDRA     ; Don't remember why I do this
    
    LDA #$00
    STA Screen ; Ben Eater's Worlds Worst Video card 
    LDA #$20   ; uses the upper 8Kb of system RAM
    STA ScreenH; This starts at location $2000
    
 if BeepOn=1
; Setup some commands for music    
    STZ BeepRead
    STZ BeepWrite
    STZ BeepCount
    STZ BeepEnable
      ;Beep Rom data $A000
    LDA #$00
    STA BeepFile
    LDA #$A0
    STA BeepFileH
;Add some empty audio to beginning?
; ;This is to cover the SD card boot time and such.
;     LDX #5
;     lda #0
; .FillBufferNoBeep
;     ;JSR .GetBeep
;     jsr .Beep
;     DEX 
;     BNE .FillBufferNoBeep
.FillBuffer:
    LDX #250 
.FillBufferLoop:;Fill up NMI Music buffer with 250 bytes
    JSR .GetBeep
    DEX 
    BNE .FillBufferLoop
    STZ BeepRead ;Reset read to start of buffer
 endif ; if BeepOn

    ldy #0  
    ldx #0
    LDA #255
    STA  VIA_DDRB;MAKE EVERYTHING OUTPUT! $6002   
    LDA  VIA_AUX;$600b This would be a retangle wave instead of a square wave, but no real matter.    
                        ; Set the two high bits in the ACR to get the
    ORA  #$C0   ;1100-0000; square-wave/RECTANGLE output on PB7.  (Don't enable the
    STA  VIA_AUX;$600b    ; T1 interrupt in the IER though.)  
    
    lda #$0a ; Make CA2 pulse each time port A is read. tnx gfoot!
    STA VIA_PCR

    ;Do screwy stuff to get SD card
    ;started up in SPI mode.
    jsr sd_init

    ; Setup SD card to read forever
    lda #SD_MOSI
    sta  VIA_PORTB 
    lda #$52 ; CMD18 - READ_MULTI_BLOCK
    ;WOW, that easy, change $51 to $52 and I can stream
    ;bytes off the SD card forrever! Yea!
    ;Just have to remember to throw away the 10 byte
    ;CRC every 512 bytes. Other than that the bits stream forever. Yea!
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
    jmp .reset ;This change makes it pretty reliable after a reboot.

.readsuccess
  ; wait for data
  jsr sd_waitresult
  cmp #$fe
  beq .SdBooted
  ;Retry until it works...
  jmp .reset
.SdBooted
  ;SD card booted, setup some values


  lda #SD_MOSI                ; enable card (CS low), set MOSI (resting state), SCK low
  sta  VIA_PORTB 
  ; Port A is in Clock pulse mode.
  ; A read will pulse the clock. 
  ; We will stream forever after that.
    

;****************************************************************************
;******************************** DECODE ************************************
;**************************************************************************** 

  lda VIA_PORTA ; toggle the clock once at the start to prime the SD card
  ;     -WE ARE STREAMING BITS FROM THE SD CARD NOW IN PORT A PA0-

  lda #252    ; 250 is our Vsync 'Zero' and we want 2 Vsync per frame (30fps)
  sta VGAClock; Start Vsync just before starting decode

;********************* Jump to video stream decode now! *********************
  jmp .FrameLoopStart ;Decoder entry point. Toss some frames on that screen!

;****************************************************************************


;****************************************************************************
;******************************* SD INIT ************************************
;**************************************************************************** 

sd_init:
;  Thanks George Foot! https://github.com/gfoot/sdcard6502 and for all your help on
;  6502.org forums and https://www.reddit.com/r/beneater/ !!
;
  ; Let the SD card boot up, by pumping the clock with SD CS disabled
  ; We need to apply around 80 clock pulses with CS and MOSI high.
  ; Normally MOSI doesn't matter when CS is high, but the card is
  ; not yet is SPI mode, and in this non-SPI state it does care.
;
; Since the SD boot only runs at the start there is no real need to optimize this if it works.
; I use Kingston Canvas Select Plus 32GB micro SD cards.
; 2 pack with SD adapter for $8 at normal online places.
; This code does a good job of booting them up. I've used 3 so far without issue.
  lda #SD_CS | SD_MOSI
  ldx #160               ; toggle the clock 160 times, so 80 low-high transitions
.preinitloop:
  ;eor #SD_SCK
  sta  VIA_PORTB
  ldy VIA_PORTA 
  dex
  bne .preinitloop
  ;LDA #'P'
  ;STA ACIA
.cmd0 ; GO_IDLE_STATE - resets card to idle state, and SPI mode
  lda #<cmd0_bytes
  sta zp_sd_cmd_address
  lda #>cmd0_bytes
  sta zp_sd_cmd_address+1

  jsr sd_sendcommand
  sta VIA
  ; Expect status response $01 (not initialized)
  cmp #$01
  bne sd_init;.initfailed

.cmd8 ; SEND_IF_COND - tell the card how we want it to operate (3.3V, etc)
  lda #<cmd8_bytes
  sta zp_sd_cmd_address
  lda #>cmd8_bytes
  sta zp_sd_cmd_address+1

  jsr sd_sendcommand

  ; Expect status response $01 (not initialized)
  cmp #$01
  bne sd_init;.initfailed

  ; Read 32-bit return value, but ignore it
  jsr sd_readbyte
  jsr sd_readbyte
  jsr sd_readbyte
  jsr sd_readbyte

.cmd55 ; APP_CMD - required prefix for ACMD commands
  lda #<cmd55_bytes
  sta zp_sd_cmd_address
  lda #>cmd55_bytes
  sta zp_sd_cmd_address+1

  jsr sd_sendcommand

  ; Expect status response $01 (not initialized)
  cmp #$01
  bne sd_init;.initfailed

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
  bne sd_init;.initfailed
  ; Not initialized yet, so wait a while then try again.
  ; This retry is important, to give the card time to initialize.
  ;jsr .delay.. NOT THAT IMPORTANT? Works with just a nop.. removed delay
  nop ;nop instead of a delay
  bra .cmd55

.initialized
  rts

cmd0_bytes
  .byte $40, $00, $00, $00, $00, $95
cmd8_bytes
  .byte $48, $00, $00, $01, $aa, $87
cmd55_bytes
  .byte $77, $00, $00, $00, $00, $01
cmd41_bytes
  .byte $69, $40, $00, $00, $00, $01



sd_readbyte:
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
    ora VIA_IORA;VIA_PORTA ; read last bit without causing a clock pulse. Cool!
    rts

sd_writebyte:
  ; Tick the clock 8 times with descending bits on MOSI
  ; SD communication is mostly half-duplex so we ignore anything it sends back here
  ldx #8                      ; send 8 bits
.swloop:
  asl                         ; shift next bit into carry
  tay                         ; save remaining bits for later
  lda #0
  bcc .sendbit                ; if carry clear, don't set MOSI for this bit
  ora #SD_MOSI
.sendbit:
  sta VIA_PORTB               ; set MOSI (or not) first with SCK low
  ;eor #SD_SCK                ; Clock not needed anymore
  sta VIA_PORTB               ; raise SCK keeping MOSI the same, to send the bit
  bit VIA_PORTA               ; * New clock for handshake *
  tya                         ; restore remaining bits to send
  dex
  bne .swloop                   ; loop if there are more bits to send
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
  ; End command
  lda #SD_CS | SD_MOSI   ; set CS high again
  sta  VIA_PORTB 
  pla   ; restore result code
  rts

; Turns out I just don't need any delays? 
; The SD boot works without delay.. :) 
; so I'll just comment out for now?
;.delay:
;   ldx #0
;   ldy #0
; .dloop
;   dey
;   bne .dloop
;   dex
;   bne .dloop
;   rts
MSGBadApple     .byte "NormalLuser Bad Apple! 6502 Ben Eater Demo 2023/2024",0







   .org $FFFA
   .word NMI
   .word RESET
   .word IRQ