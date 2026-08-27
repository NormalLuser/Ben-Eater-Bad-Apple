; ==============================================================================
; Fifty1Ford/NormalLuser - Ben Eater Breadboard "Bad Apple!!" Demo
;
; This project is a "Bad Apple!!" demo for the Ben Eater 6502 and
; "World's Worst Video Card" breadboard kits.
;
; Features:
; - 1-byte encoding with Run-Length, Differential, and Tri-Pixel Encoding
; - Self-modifying Run-Length Encoding (RLE) in the zero page for speed
; - Averages 56.77 frames per second (decode only, without V-sync/beep)
; - Achieves a 20.86:1 compression ratio with the Python encoder
; - Automatic 30 FPS V-sync with a catch-up mechanism
;
; Hardware Requirements:
; - Ben Eater 6502 and "World's Worst Video Card" kits
; - A 5MHz clock signal for the CPU
; - V-sync signal from the VGA output connected to the NMI pin of the CPU
; - SD card interface connected to VIA Port A and Port B
; - Extra bypass capacitors for improved stability
; ==============================================================================

; ==============================================================================
; Constants and Memory Map
; ==============================================================================

; ------------------------------------------------------------------------------
; Memory Layout
; ------------------------------------------------------------------------------
ProgramStart    = $8000
MusicStart      = $8A00
MusicEnd        = $C600
RLERenderStart  = ProgramStart + 512    ; Page-aligned for performance
ArrayStart      = RLERenderStart + 256  ; Page-aligned for lookup tables
BeepBuffer      = $1E00                 ; IRQ Beep Buffer

; ------------------------------------------------------------------------------
; I/O Addresses
; ------------------------------------------------------------------------------
Display         = $2000     ; Start of memory-mapped display (100x64)
ACIA            = $5000
VIA             = $6000

; ------------------------------------------------------------------------------
; Debugging and Feature Flags
; ------------------------------------------------------------------------------
VsyncOn         = 1   ; Enable/disable V-sync
BeepOn          = 0   ; Enable/disable music
Debug           = 0   ; Enable/disable debug pixel for V-sync
VSZero          = 225 ; V-sync counter "zero point"

; ------------------------------------------------------------------------------
; SD Card Constants
; ------------------------------------------------------------------------------
NumberOfBlocks  = 2   ; Number of 256-byte blocks in a 512-byte sector
SD_CS           = $20 ; Chip Select
SD_MOSI         = $8  ; Master Out, Slave In

; ==============================================================================
; Zero Page Variables
; ==============================================================================
ZP_START        = $01

; Bad Apple!! Demo Variables
RLECount        = ZP_START + 0
ReadByteTemp    = ZP_START + 1
BackColor       = ZP_START + 2
PlotColor       = ZP_START + 3
ScreenPtrL      = ZP_START + 4
ScreenPtrH      = ZP_START + 5
VidCounter      = ZP_START + 6
VGAClock        = ZP_START + 7

; SD Card and Bootloader Variables
zp_sd_cmd_address = ZP_START + 8
Block_Counter   = ZP_START + 9
BlockTwo        = ZP_START + 10

; Music and Sound Variables
BeepEnable      = ZP_START + 11
BeepCount       = ZP_START + 12
BeepRead        = ZP_START + 13
BeepWrite       = ZP_START + 14
BeepFileL       = ZP_START + 15
BeepFileH       = ZP_START + 16

; NMI and IRQ Temporary Storage
NMI_TA          = ZP_START + 17
NMI_TX          = ZP_START + 18
NMI_TY          = ZP_START + 19
RTI_TA          = ZP_START + 20
RTI_TX          = ZP_START + 21
RTI_TY          = ZP_START + 22

; WozMon Variables
XAML            = ZP_START + 23
XAMH            = ZP_START + 24
STL             = ZP_START + 25
STH             = ZP_START + 26
L               = ZP_START + 27
H               = ZP_START + 28
YSAV            = ZP_START + 29
MODE            = ZP_START + 30
MSGL            = ZP_START + 31
MSGH            = ZP_START + 32
COUNTER         = ZP_START + 33
CRC             = ZP_START + 34
CRCCHECK        = ZP_START + 35

; ==============================================================================
; Main Program
; ==============================================================================
	.org ProgramStart

BadAppleStart:
    jmp BootUp

; ------------------------------------------------------------------------------
; Main Frame Loop (Relocated to Zero Page)
; ------------------------------------------------------------------------------
FrameLoop:
    ; This is the main loop that decodes and renders each frame.
    ; It's relocated to the zero page for a slight performance boost.
    dec Block_Counter
    beq HandleBlockEnd

FrameLoopStart:
    ; Read the first control bit to determine the operation type
    lda VIA_PORTA
    bne SkipRun     ; If bit is 1, it's a skip operation

    ; Read the second control bit
    lda VIA_PORTA
    beq TriPixel    ; If bit is 0, it's a tri-pixel operation

    ; If both bits are 1, it's a Run-Length Encoding (RLE) operation
    ; This falls through to the RLE routine
RLE:
    ; Self-modifying RLE routine for maximum speed
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
    tax
    lda RLEArray,x
    sta RLEJump + 1
    lda PlotColor
RLEJump:
    jmp RLERender

; ------------------------------------------------------------------------------
; V-Sync Routine
; ------------------------------------------------------------------------------
Vsync:
.if VsyncOn
    ldx VGAClock
    cpx #VSZero
    bcs EGVsync

.if Debug
    lda #$20 ; Red
    sta $2002
.endif
    jmp NextSync

Synced:
.if Debug
    lda #$00 ; Black
    sta $2002
.endif

NextSync:
    inc VGAClock
    inc VGAClock
    jmp FrameLoop

EGVsync:
    beq Synced
    jmp Vsync
.else
    jmp FrameLoop
.endif

; ------------------------------------------------------------------------------
; Frame Rollover Routine
; ------------------------------------------------------------------------------
FrameRollover:
    ; This routine is called when the screen buffer pointer wraps around.
    lsr ScreenPtrH  ; Reset the high byte of the screen pointer
    jmp Vsync       ; Proceed to V-sync

; ------------------------------------------------------------------------------
; Block Handling
; ------------------------------------------------------------------------------
HandleBlockEnd:
    dec BlockTwo
    bne FrameLoopStart
    ; Fall through to TossBits, which is relocated to the stack area

; ==============================================================================
; Relocated Code (Zero Page and Stack)
; ==============================================================================

; ------------------------------------------------------------------------------
; Code to be Relocated to Zero Page
; ------------------------------------------------------------------------------
RelocateLocation = $100 - RelocateLength
RelocateLength = 144
RelocateCode:
.rorg RelocateLocation

TriPixel:
    ; Decodes and draws a 3-pixel run
    lda VIA_PORTA
    asl : ora VIA_PORTA
    asl : ora VIA_PORTA
    asl : ora VIA_PORTA
    asl : ora VIA_PORTA
    asl : ora VIA_PORTA
    tax
    lda Array1,x
    sta (ScreenPtrL),y
    iny
    lda Array2,x
    sta (ScreenPtrL),y
    iny
    lda Array3,x
    sta (ScreenPtrL),y
    sta PlotColor
    jmp FrameLoop

SkipRun:
    ; Skips a number of pixels
    lda VIA_PORTA
    asl : ora VIA_PORTA
    asl : ora VIA_PORTA
    asl : ora VIA_PORTA
    asl : ora VIA_PORTA
    asl : ora VIA_PORTA
    asl : ora VIA_PORTA
    asl : ora VIA_PORTA
    sta RLECount
    tya
    adc RLECount
    tay
    lda ScreenPtrH
    adc #$00
    sta ScreenPtrH
    cmp #$40
    bne FrameLoop
    jmp FrameRollover
.rend

; ------------------------------------------------------------------------------
; Code to be Relocated to the Stack
; ------------------------------------------------------------------------------
TossBitsLocation = $100
TossBitsLen = 249
TossBits:
    ; This routine discards the 10 CRC bytes at the end of each 512-byte block.
    ; It's unrolled for speed.
    lda #NumberOfBlocks
    sta BlockTwo

    ; Unrolled loop to read and discard 10 bytes (80 bits)
    .repeat 10
        bit VIA_PORTA : bit VIA_PORTA : bit VIA_PORTA : bit VIA_PORTA
        bit VIA_PORTA : bit VIA_PORTA : bit VIA_PORTA : bit VIA_PORTA
    .endrep

    jmp FrameLoopStart
; ==============================================================================
; RLE Render Routine (Unrolled Loop)
; ==============================================================================
.org RLERenderStart

RLERender:
    ; This is an unrolled loop to draw a run of pixels of the same color.
    ; It's much faster than a traditional loop.
    .repeat 64
        sta (ScreenPtrL),y
        iny
    .endrep
    jmp FrameLoop

; ==============================================================================
; Lookup Tables
; ==============================================================================
.org ArrayStart
.align 256

RLEArray:
    ; Lookup table for RLE jump offsets
    .byte 189,189,186,183,180,177,174,171,168,165,162
    .byte 159,156,153,150,147,144,141,138,135,132
    .byte 129,126,123,120,117,114,111,108,105,102
    .byte 99, 96, 93, 90, 87, 84, 81, 78, 75, 72
    .byte 69, 66, 63, 60, 57, 54, 51, 48, 45, 42
    .byte 39, 36, 33, 30, 27, 24, 21, 18, 15, 12
    .byte 9,  6,  3

Array1:
    ; Lookup table for the first pixel in a tri-pixel run
    .byte 0,0,0,21,0,0,42,0,0,63,0,21,21,0,0,21,21,42,42,0,0,21,21,63
    .byte 63,0,42,42,0,0,42,42,63,63,0,63,63,21,21,21,42,21,21,63,21
    .byte 42,42,21,21,42,42,63,63,21,63,63,42,42,42,63,42,63,63,63

Array2:
    ; Lookup table for the second pixel in a tri-pixel run
    .byte 0,0,21,0,0,42,0,0,63,0,21,0,21,21,42,0,42,0,21,21,63,0,63
    .byte 0,21,42,0,42,42,63,0,63,0,42,63,0,63,21,21,42,21,21,63,21
    .byte 42,21,42,42,63,21,63,21,42,63,21,63,42,42,63,42,63,42,63,63

Array3:
    ; Lookup table for the third pixel in a tri-pixel run
    .byte 0,21,0,0,42,0,0,63,0,0,21,21,0,42,21,42,0,21,0,63,21,63,0
    .byte 21,0,42,42,0,63,42,63,0,42,0,63,63,0,21,42,21,21,63,21,21
    .byte 42,42,21,63,42,63,21,42,21,63,63,21,42,63,42,42,63,63,42,63

; ==============================================================================
; Boot and Initialization
; ==============================================================================
BootUp:
    cld
    cli
    ldx #$FF
    txs

    ; Initialize VIA and other hardware
    stz Block_Counter
    lda #NumberOfBlocks
    sta BlockTwo
    stz VIA_PORTA
    lda #$FE
    sta VIA_DDRA

    ; Initialize screen pointer
    stz ScreenPtrL
    lda #$20
    sta ScreenPtrH

.if BeepOn
    ; Initialize music variables
    stz BeepRead
    stz BeepWrite
    stz BeepCount
    stz BeepEnable
.endif

    ; Initialize SD card
    jsr sd_init

    ; Set up SD card for continuous reading (CMD18)
    lda #$52 ; CMD18 - READ_MULTI_BLOCK
    jsr sd_writebyte
    lda #$00 ; Sector address (4 bytes)
    jsr sd_writebyte
    jsr sd_writebyte
    jsr sd_writebyte
    jsr sd_writebyte
    lda #$4D ; Dummy CRC
    jsr sd_writebyte

    ; Wait for the SD card to be ready
    jsr sd_waitresult
    cmp #$00
    bne BootUp ; Retry on failure

    jsr sd_waitresult
    cmp #$FE
    bne BootUp ; Retry on failure

    ; Relocate performance-critical code to zero page and stack
    ldx #0
RelocateLoop:
    lda RelocateCode,x
    sta RelocateLocation,x
    inx
    cpx #RelocateLength
    bne RelocateLoop

    ldx #0
RelocateStackLoop:
    lda TossBits,x
    sta TossBitsLocation,x
    inx
    cpx #TossBitsLen
    bne RelocateStackLoop

    ; Start the demo
    lda #VSZero + 2
    sta VGAClock
    lda #1
    sta BeepEnable
    jmp FrameLoopStart