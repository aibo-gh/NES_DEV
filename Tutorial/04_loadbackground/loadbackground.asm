PPU_CTROL   = $2000
PPU_MASK    = $2001
PPU_STATUS  = $2002
OAM_ADDR    = $2003
OAM_DATA    = $2004
PPU_SCROLL  = $2005
PPU_ADDR    = $2006
PPU_DATA    = $2007

.segment "HEADER"

.byte "NES", $1A
.byte $02
.byte $01
.byte $00
.byte $00
.byte $00
.byte $00
.byte $00
.byte $00
.byte $00
.byte $00

.segment "ZEROPAGE"
; Define your variables here...


.segment "CODE"

RESET:
    sei                 ; Disable all IRQ Interrupts
    cld                 ; Clear the decimal mode (unsupported by the NES)
    ldx #$FF            
    txs                 ; Initialize the stack pointer at $01FF
    inx                 ; Increment X, causing a roll-off from $FF to $00
    txa                 ; A = 0

ClearRAM:
    sta $0000,X         ; Zero RAM addresses from $0000 to $00FF
    sta $0100,X         ; Zero RAM addresses from $0100 to $01FF
    sta $0200,X         ; Zero RAM addresses from $0200 to $02FF
    sta $0300,X         ; Zero RAM addresses from $0300 to $03FF
    sta $0400,X         ; Zero RAM addresses from $0400 to $04FF
    sta $0500,X         ; Zero RAM addresses from $0500 to $05FF
    sta $0600,X         ; Zero RAM addresses from $0600 to $06FF
    sta $0700,X         ; Zero RAM addresses from $0700 to $07FF
    inx 
    bne ClearRAM


Main:
    ldx #$3F            
    stx PPU_ADDR           ; Set hi-byte of PPU_ADDR to $3F
    ldx #$00
    stx PPU_ADDR           ; Set lo-byte of PPU_ADDR to $00

    lda #$2A            
    sta PPU_DATA           ; Send $2A (lime-green color code) to PPU_DATA

    lda #%00011110
    sta PPU_MASK           ; Set PPU_MASK bits to show background

LoopForever:
    jmp Main

NMI:
    rti 
IRQ: 
    rti 

.segment "VECTORS"
.word NMI
.word RESET
.word IRQ
