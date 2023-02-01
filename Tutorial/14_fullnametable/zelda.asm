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
Frame:   .res 1              ; Reserve 1 byte to store the number of frames
Clock60: .res 1              ; Reserve 1 byte to store a counter that increments every second (60 frames)  
BgPtr:   .res 2              ; Reserve 2 bytes (16 bits) to store a pointer to the background address     
                             ; We store first the lo-byte, and immediately after, the hi-byte - little endian architecture

.segment "CODE"

.macro PPU_SETADDR addr 
    bit PPU_STATUS         ; Reset the Address latch from the PPU
    lda #>addr            
    sta PPU_ADDR           ; Set hi-byte of PPU_ADDR to $3F
    lda #<addr 
    sta PPU_ADDR           ; Set lo-byte of PPU_ADDR to $00
.endmacro 

.macro PPU_SETDATA val 
    lda val
    sta PPU_DATA
.endmacro 

; Subroutine to load all 32 color palette values from ROM
.proc LoadPalette
    PPU_SETADDR $3F00 
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
    PPU_SETADDR $2000
    ldx #00
    ldy #00
    lda #<BackgroundData
    sta BgPtr                   
    lda #>BackgroundData
    sta BgPtr+1                 

Load:
    lda (BgPtr),y 
    sta PPU_DATA
    iny 
    cpy #0
    bne Load
    inc BgPtr+1
    inx 
    cpx #4
    bne Load
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

    lda #0
    sta Frame
    sta Clock60


Main:
    jsr LoadPalette    
    jsr LoadBackground

EnablePPURendering:
    lda #%10010000          ; Enable NMI and set background to use the 2nd pattern table ($1000)
    sta PPU_CTRL
    lda #0
    sta PPU_SCROLL          ; Disable scroll in x
    sta PPU_SCROLL          ; Disable scroll in y 
    lda #%00011110
    sta PPU_MASK           ; Set PPU_MASK bits to show background

LoopForever:
    jmp LoopForever

NMI:
   inc Frame                ; Frame ++
   lda Frame
   cmp #60                  ; Compare Frame with #60
   bne :+                   ; If it's not 60, bypass...
   inc Clock60              ; else, increment Clock60 and zero the Frame counter 
   lda #0
   sta Frame
:  rti 
continue:
    inc Frame 
    rti 
IRQ: 
    rti 

PaletteData:
.byte $0F,$30,$00,$12, $0F,$16,$27,$36, $0F,$1A,$37,$12, $0F,$17,$37,$12
.byte $0F,$29,$27,$17, $0F,$02,$22,$30, $0F,$16,$27,$30, $0F,$0F,$1C,$16 



BackgroundData:
.byte $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24
.byte $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24
.byte $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24
.byte $24, $24, $F5, $F5, $F5, $F5, $F5, $F5, $F5, $F5, $24, $F7, $21, $00, $24, $69, $0B, $6B, $69, $0A, $6B, $24, $24, $62, $15, $12, $0F, $0E, $62, $24, $24, $24 
.byte $24, $24, $F5, $F5, $F5, $F5, $F5, $F5, $F5, $F5, $24, $24, $24, $24, $24, $6C, $24, $6C, $6C, $24, $6C, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24 
.byte $24, $24, $F5, $F5, $F5, $F5, $F5, $F5, $F5, $F5, $24, $F9, $21, $00, $24, $6C, $24, $6C, $6C, $24, $6C, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24
.byte $24, $24, $F5, $F5, $F5, $F5, $F5, $F5, $F5, $F5, $24, $61, $21, $00, $24, $6E, $6A, $6D, $6E, $6A, $6D, $24, $F2, $F2, $F2, $24, $24, $24, $24, $24, $24, $24 
.byte $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24 
.byte $D8, $DA, $D8, $DA, $D8, $DA, $D8, $DA, $D8, $DA, $D8, $DA, $D8, $DA, $26, $26, $26, $26, $D8, $DA, $D8, $DA, $D8, $DA, $D8, $DA, $D8, $DA, $D8, $DA, $D8, $DA
.byte $D9, $DB, $D9, $DB, $D9, $DB, $D9, $DB, $D9, $DB, $D9, $DB, $D9, $DB, $26, $26, $26, $26, $D9, $DB, $D9, $DB, $D9, $DB, $D9, $DB, $D9, $DB, $D9, $DB, $D9, $DB 
.byte $D8, $DA, $D8, $DA, $D8, $DA, $D8, $DA, $F3, $F3, $D8, $DA, $DC, $DE, $26, $26, $26, $26, $D8, $DA, $D8, $DA, $D8, $DA, $D8, $DA, $D8, $DA, $D8, $DA, $D8, $DA 
.byte $D9, $DB, $D9, $DB, $D9, $DB, $D9, $DB, $24, $24, $D9, $DB, $DD, $DF, $26, $26, $26, $26, $D9, $DB, $D9, $DB, $D9, $DB, $D9, $DB, $D9, $DB, $D9, $DB, $D9, $DB
.byte $D8, $DA, $D8, $DA, $D8, $DA, $DC, $DE, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $D8, $DA, $D8, $DA, $D8, $DA, $D8, $DA, $D8, $DA, $D8, $DA, $D8, $DA 
.byte $D9, $DB, $D9, $DB, $D9, $DB, $DD, $DF, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $D9, $DB, $D9, $DB, $D9, $DB, $D9, $DB, $D9, $DB, $D9, $DB, $D9, $DB 
.byte $D8, $DA, $D8, $DA, $DC, $DE, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $D8, $DA, $D8, $DA, $D8, $DA, $D8, $DA, $D8, $DA, $D8, $DA, $D8, $DA
.byte $D9, $DB, $D9, $DB, $DD, $DF, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $D9, $DB, $D9, $DB, $D9, $DB, $D9, $DB, $D9, $DB, $D9, $DB, $D9, $DB 
.byte $D8, $DA, $DC, $DE, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $D4, $D6, $D8, $DA, $D8, $DA, $D8, $DA, $D8, $DA, $D8, $DA, $D8, $DA
.byte $D9, $DB, $DD, $DF, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $D5, $D7, $D9, $DB, $D9, $DB, $D9, $DB, $D9, $DB, $D9, $DB, $D9, $DB
.byte $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26 
.byte $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26 
.byte $CE, $D0, $D0, $D2, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $CE, $D0, $CE, $D0
.byte $CF, $D1, $D1, $D3, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $CF, $D1, $CF, $D1 
.byte $D8, $DA, $D8, $DA, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $D8, $DA, $D8, $DA 
.byte $D9, $DB, $D9, $DB, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $D9, $DB, $D9, $DB
.byte $D8, $DA, $D8, $DA, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $D8, $DA, $D8, $DA 
.byte $D9, $DB, $D9, $DB, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $26, $D9, $DB, $D9, $DB
.byte $D8, $DA, $D8, $DA, $CE, $D0, $CE, $D0, $CE, $D0, $CE, $D0, $CE, $D0, $CE, $D0, $CE, $D0, $CE, $D0, $CE, $D0, $CE, $D0, $CE, $D0, $CE, $D0, $D8, $DA, $D8, $DA
.byte $D9, $DB, $D9, $DB, $CF, $D1, $CF, $D1, $CF, $D1, $CF, $D1, $CF, $D1, $CF, $D1, $CF, $D1, $CF, $D1, $CF, $D1, $CF, $D1, $CF, $D1, $CF, $D1, $D9, $DB, $D9, $DB 
.byte $D8, $DA, $D8, $DA, $D8, $DA, $D8, $DA, $D8, $DA, $D8, $DA, $D8, $DA, $D8, $DA, $D8, $DA, $D8, $DA, $D8, $DA, $D8, $DA, $D8, $DA, $D8, $DA, $D8, $DA, $D8, $DA 
.byte $D9, $DB, $D9, $DB, $D9, $DB, $D9, $DB, $D9, $DB, $D9, $DB, $D9, $DB, $D9, $DB, $D9, $DB, $D9, $DB, $D9, $DB, $D9, $DB, $D9, $DB, $D9, $DB, $D9, $DB, $D9, $DB

AttributeData:
.byte $00, $00, $40, $00, $00, $44, $55, $55
.byte $00, $00, $04, $00, $00, $44, $55, $55 
.byte $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA 
.byte $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA  
.byte $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA 
.byte $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA
.byte $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA 
.byte $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA 
.byte $24, $24, $24, $24, $24, $24, $24, $24
.byte $24, $24, $24, $24, $24, $24, $24, $24



.segment "CHARS"
.incbin "zelda_sprites.chr"

.segment "VECTORS"
.word NMI
.word RESET
.word IRQ
