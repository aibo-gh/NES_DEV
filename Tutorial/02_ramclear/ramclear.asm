;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The iNES header (contains a total of 16 bytes with the flags at $7FF0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "HEADER"
.org $7FF0
.byte $4E, $45, $53 ,$1A         ; 4 bytes with the characters 'N', 'E', 'S', '\n'
.byte $02                        ; How many 16KB of PRG-ROM we'll use (=32KB)
.byte $01                        ; How many 8KB of CHR-ROM we'll use (=8KB)
.byte %00000000                  ; Horz mirroring, no battery, mapper 0
.byte %00000000                  ; mapper 0, playchoice, NES 2.0
.byte $00                        ; No PRG-RAM
.byte $00                        ; NTSC TV format
.byte $00                        ; No PRG-RAM
.byte $00,$00,$00,$00,$00        ; Unused padding to complete 16 bytes of header

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRG-ROM code located at $8000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "CODE"
.org $8000

RESET:
    sei                           ; Disable all IRQ interrupts
    cld                           ; Clear the decimal mode (unsupported by the NES)
    ldx #$FF
    txs                           ; Initialize the stack pointer at $01FF
                                 
    lda #0                        ; A = 0 
    ldx #0                       ; X = FF
MemLoop:                          ; Loop all memory positions from $00 to $FF clearing them out!
    sta $0,x                      ; Store the value of A (zero) into $0 + x
    dex                           ; X--
    bne MemLoop                   ; If X is not zero, we loop back to the MemLoop label
NMI:
    rti 
IRQ: 
    rti 

.segment "VECTORS"
.org $FFFA
.word NMI   ; address of NMI handler (non maskable interrupt)
.word RESET ; address of the RESET handler
.word IRQ   ; address of the IRQ handler
