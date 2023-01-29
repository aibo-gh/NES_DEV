PPU_CTRL   = $2000
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

; Subroutine to load all 32 color palette values from ROM
.proc LoadPalette
    ldy #0
LoadPalette: 
    lda PaletteData,y      ; Lookup byte in ROM
    sta PPU_DATA           ; Set value to send to PPU_DATA
    iny                    ; y++
    cpy #32                ; Value equal to 32?
    bne LoadPalette   
    rts 
.endproc

; Subroutine to load 255 tiles in the first nametable
.proc LoadBackground
    ldy #0                   ; Y = 0
LoopBackground:
    lda BackgroundData,y     ; Lookup byte in ROM
    sta PPU_DATA             ; Set value to send to PPU_DATA
    iny                      ; Y++
    cpy #255                 ; Is Y equal to 255?
    bne LoopBackground       ; Not yet, keep looping
    rts                      ; Return from subroutine
.endproc


RESET:
    sei                 ; Disable all IRQ Interrupts
    cld                 ; Clear the decimal mode (unsupported by the NES)
    ldx #$FF           
    txs                 ; Initialize the stack pointer at $01FF

    inx                 ; Increment X, causing a roll-off from $FF to $00 
    stx PPU_CTRL       ; disable NMI
    stx PPU_MASK        ; disable rendering (masking background and sprites)
    stx $4010           ; disable DMC IRQ's

    lda #$40 
    sta $4017           ; Disable APU frame IRQ
  
   

Wait1stVBlank:          ; Wait for the first VBlank from the PPU
    bit PPU_STATUS      ; Perform a bit-wise check with the PPU_STATUS port
    bpl Wait1stVBlank   ; Loop until bit-7 (sign bit) is 1 (inside VBlank)


    txa                 ; A = 0s
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

Wait2ndVBlank:          ; Wait for the first VBlank from the PPU
    bit PPU_STATUS      ; Perform a bit-wise check with the PPU_STATUS port
    bpl Wait2ndVBlank   ; Loop until bit-7 (sign bit) is 1 (inside VBlank)


Main:
    bit PPU_STATUS         ; Reset the Address latch from the PPU
    ldx #$3F            
    stx PPU_ADDR           ; Set hi-byte of PPU_ADDR to $3F
    ldx #$00
    stx PPU_ADDR           ; Set lo-byte of PPU_ADDR to $00
    jsr LoadPalette    

    bit PPU_STATUS         ; Reset the address latch from the PPU
    ldx #$20
    stx PPU_ADDR           ; Set hi-byte of PPU_ADDR to $20
    ldx #$00
    stx PPU_ADDR           ; Set lo-byte of PPU_ADDR to $00
    jsr LoadBackground

EnablePPURendering:
    lda #%10010000          ; Enable NMI and set background to use the 2nd pattern table ($1000)
    sta PPU_CTRL
    lda #%00011110
    sta PPU_MASK           ; Set PPU_MASK bits to show background

LoopForever:
    jmp LoopForever

NMI:
    rti 
IRQ: 
    rti 

PaletteData:
.byte $0F,$2A,$0C,$3A, $0F,$2A,$0C,$3A, $0F,$2A,$0C,$3A, $0F,$2A,$0C,$3A ; Background
.byte $0F,$10,$00,$26, $0F,$10,$00,$26, $0F,$10,$00,$26, $0F,$10,$00,$26 ; Sprites

BackgroundData:
.byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$36,$37,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
.byte $24,$24,$24,$24,$24,$24,$24,$24,$35,$25,$25,$38,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$60,$61,$62,$63,$24,$24,$24,$24
.byte $24,$36,$37,$24,$24,$24,$24,$24,$39,$3a,$3b,$3c,$24,$24,$24,$24,$53,$54,$24,$24,$24,$24,$24,$24,$64,$65,$66,$67,$24,$24,$24,$24
.byte $35,$25,$25,$38,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24,$24,$24,$24,$24,$68,$69,$26,$6a,$24,$24,$24,$24
.byte $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45
.byte $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47
.byte $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00


.segment "CHARS"
.incbin "mario.chr"

.segment "VECTORS"
.word NMI
.word RESET
.word IRQ
