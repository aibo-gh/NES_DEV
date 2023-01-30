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
    lda (BgPtr),y            ; Lookup byte in ROM
    sta PPU_DATA             ; Set value to send to PPU_DATA
    iny                      ; Y++
    cpy #255                 ; Is Y equal to 255?
    bne LoopBackground       ; Not yet, keep looping
    rts                      ; Return from subroutine
.endproc

; Subroutine to load 16 bytes of attributes for the first nametable
.proc LoadAttributes
    ldy #0
LoopAttributes:
    lda AttributeData,y       ; Lookup byte in ROM
    sta PPU_DATA              ; Set value to send to PPU_DATA
    iny 
    cpy #16                  ; Is Y equal to 16?
    bne LoopAttributes
    rts 
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

    PPU_SETADDR $3F00 
    jsr LoadPalette    

    lda #<BackgroundData    ; Fetch the lo-byte of Background Data address
    sta BgPtr

    lda #>BackgroundData    ; Fetch the hi-byte of Background Data address
    sta BgPtr+1 

    PPU_SETADDR $2000 
    jsr LoadBackground

    PPU_SETADDR $23C0 
    jsr LoadAttributes     ; Jump to subroutine LoadAttributes

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
.byte $22,$29,$1A,$0F, $22,$36,$17,$0F, $22,$30,$21,$0F, $22,$27,$17,$0F ; Background palette
.byte $22,$16,$27,$18, $22,$1A,$30,$27, $22,$16,$30,$27, $22,$0F,$36,$17 ; Sprite palette


BackgroundData:
.byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$36,$37,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
.byte $24,$24,$24,$24,$24,$24,$24,$24,$35,$25,$25,$38,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$60,$61,$62,$63,$24,$24,$24,$24
.byte $24,$36,$37,$24,$24,$24,$24,$24,$39,$3a,$3b,$3c,$24,$24,$24,$24,$53,$54,$24,$24,$24,$24,$24,$24,$64,$65,$66,$67,$24,$24,$24,$24
.byte $35,$25,$25,$38,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24,$24,$24,$24,$24,$68,$69,$26,$6a,$24,$24,$24,$24
.byte $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45
.byte $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47
.byte $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

AttributeData:
.byte %00000000, %00000000, %10101010, %00000000, %11110000, %00000000, %00000000, %00000000
.byte %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111


.segment "CHARS"
.incbin "mario.chr"

.segment "VECTORS"
.word NMI
.word RESET
.word IRQ
