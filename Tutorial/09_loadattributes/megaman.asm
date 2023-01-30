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
    cpy #192                 ; Is Y equal to 255?
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

    bit PPU_STATUS         ; Reset the address latch from the PPU
    ldx #$23
    stx PPU_ADDR           ; Set hi-byte of PPU_ADDR to $20
    ldx #$C0
    stx PPU_ADDR           ; Set lo-byte of PPU_ADDR to $C0
    jsr LoadAttributes     ; Jump to subroutine LoadAttributes

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
      ; Wall                                 bright stone     dark stone
    .byte $0F,$2B,$12,$02, $0F,$20,$21,$11, $0F,$30,$31,$2C, $0F,$20,$21,$11 ; Background
    .byte $0F,$0F,$2C,$11, $0F,$0F,$20,$38, $0F,$0F,$11,$30, $0F,$0F,$15,$28 ; Sprites

BackgroundData:
           ;D        ;C       ;D       ;C       ;D       ;C       ;D      ;C       ;D       ;C       ;D        ;C      ;D        ;C       ;D      ;C      
	.byte $64,$66, $64,$66, $64,$66, $64,$70, $72,$70, $72,$64, $66,$64, $66,$64, $66,$64, $66,$64, $66,$64, $66,$64, $66,$64 ,$66,$64, $66,$64, $66,$64
	.byte $65,$67, $65,$67, $65,$67, $65,$71, $73,$71, $73,$65, $67,$65, $67,$65, $67,$65, $67,$65, $67,$65, $67,$65, $67,$65 ,$67,$65, $67,$65, $67,$65
           ;B        ;A       ;B       ;A       ;B       ;A       ;B      ;A       ;B       ;A       ;B        ;A      ;B        ;A       ;B      ;A 
    .byte $64,$66, $64,$66, $64,$66, $64,$70, $72,$70, $72,$64, $66,$64, $66,$64, $66,$64, $66,$64, $66,$64, $66,$64, $66,$64, $66,$64, $64,$66, $64,$66
	.byte $65,$67, $65,$67, $65,$67, $65,$71, $73,$71, $73,$65, $63,$65, $63,$65, $63,$65, $63,$65, $63,$65, $63,$65, $63,$65, $63,$65, $63,$65, $63,$65
            ;D        ;C       ;D       ;C       ;D       ;C       ;D      ;C       ;D       ;C       ;D        ;C      ;D        ;C       ;D      ;C
	.byte $7c,$7e, $88,$8a, $8c,$8e, $90,$92, $88,$8a, $90,$92, $88,$8a, $90,$92, $7c,$7e, $88,$8a, $8c,$8e, $8c,$8e, $90,$92, $7c,$7e, $88,$8a, $90,$92
	.byte $7d,$7f, $89,$8b, $8d,$8f, $91,$93, $89,$8b, $91,$93, $89,$8b, $91,$93, $7d,$7f, $89,$8b, $8d,$8f, $8d,$8f, $91,$93, $7d,$7f, $89,$8b, $91,$93
            ;B        ;A       ;B       ;A       ;B       ;A       ;B      ;A       ;B       ;A       ;B        ;A      ;B        ;A       ;B      ;A
    .byte $00,$00, $00,$00, $00,$00, $00,$00, $00,$00, $00,$00, $00,$00, $00,$00, $00,$00, $00,$00, $00,$00, $00,$00, $00,$00, $00,$00, $00,$00, $00,$00
	.byte $00,$00, $00,$00, $00,$00, $00,$00, $00,$00, $00,$00, $00,$00, $00,$00, $00,$00, $00,$00, $00,$00, $00,$00, $00,$00, $00,$00, $00,$00, $00,$00

	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$03,$03,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

AttributeData:
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00001110, %00001111, %00001111, %00001010, %00001111, %00001111, %00001011, %00001111

.segment "CHARS"
.incbin "megaman.chr"

.segment "VECTORS"
.word NMI
.word RESET
.word IRQ
