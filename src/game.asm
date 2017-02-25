; iNES Header
;    .inesprg 1   ; 1x 16KB bank of PRG code
;    .ineschr 1   ; 1x 8KB bank of CHR data
;    .inesmap 0   ; mapper 0 = NROM, no bank swapping
;    .inesmir 1   ; background mirroring (ignore for now)


; $0000-0800 - Internal RAM, 2KB chip in the NES
; $2000-2007 - PPU access ports
; $4000-4017 - Audio and controller access ports
; $6000-7FFF - Optional WRAM inside the game cart
; $8000-FFFF - Game cart ROM    

; iNES Header
; The 16 byte iNES header gives the emulator all the information about the game including mapper, graphics mirroring, and PRG/CHR sizes. You can include all this inside your asm file at the very beginning.
  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring
  

;;;;;;;;;;;;;;;
; NES is powererd on
;    
  .bank 0 ; NESASM arranges things into 8KB chunks, this is chunk 0
  .org $C000 ; Tells the assembler where to start in this 8kb chunk
RESET:
  SEI          ; disable IRQs
  CLD          ; disable decimal mode, meant to make decimal arithmetic "easier"
  LDX #$40
  STX $4017    ; disable APU frame IRQ
  LDX #$FF
  TXS          ; Set up stack
  INX          ; now X = 0
  STX $2000    ; disable NMI
  STX $2001    ; disable rendering
  STX $4010    ; disable DMC IRQs

vblankwait1:       ; First wait for vblank to make sure PPU is ready
  BIT $2002
  BPL vblankwait1

clrmem:
  LDA #$00
  STA $0000, x
  STA $0100, x
  STA $0300, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  LDA #$FE
  STA $0200, x    ;move all sprites off screen
  INX
  BNE clrmem
   
vblankwait2:      ; Second wait for vblank, PPU is ready after this
  BIT $2002
  BPL vblankwait2

;;;;;;;;;;;;;;;;;;;;;;
; Load game pallets
LoadPalettes:
  LDA $2002    ; read PPU status to reset the high/low latch
  LDA #$3F     ; max out 0011 1111
  STA $2006    ; write the high byte of $3F00 address
  LDA #$00
  STA $2006    ; write the low byte of $3F00 address
  LDX #$00
LoadPalettesLoop:
  LDA palette, x        ;load palette byte
  STA $2007             ;write pallete byte to PPU
  INX                   ;set index to next byte
  CPX #$20              ; check if x = $20 in hex, 32 in dec
  BNE LoadPalettesLoop  ;if x = $20, 32 bytes copied, all done


LoadSprites:
  LDX #$00              ; start at 0
LoadSpritesLoop:
  LDA sprites, x        ; load data from address (sprites +  x)
  STA $0200, x          ; store into RAM address ($0200 + x)
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $20, decimal 32
  BNE LoadSpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down

                       
;;;;;;;;;;;;;;;;;;;;
; Initialze some game state variables
InitialzeState:
  LDA #$00   ; player x offset
  STA $00    
  LDA #$00   ; player y offset
  STA $01
  LDA #$00   ; controller state
  STA $03


  LDA #%10000000   ; enable NMI, sprites from Pattern Table 0
  STA $2000

  LDA #%00010000   ; enable sprites
  STA $2001

Forever:
  JMP Forever     ;jump back to forever MAINLOOP will interrupt for game logic, infinite loop to keep the rom from exiting
 
;; Game main loop
MAINLOOP: ; non-maskable interrupt (draw screen)
  ;; Load graphics into PPU from the memory
  LDA #$00   ; load 1 bytes of 0 in A
  STA $2003  ; set the low byte (00) of the RAM address
  LDA #$02   ; load 1 byte of $02, 
  STA $4014  ; set the high byte (02) of the RAM address, start the transfer

  ;; Draw game
  JSR Draw

  ;;This is the PPU clean up section, so rendering the next frame starts properly.
  ;LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  ;STA $2000
  ;LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  ;STA $2001
  ;LDA #$00        ;;tell the ppu there is no background scrolling
  ;STA $2005
  ;STA $2005  

  ;; Update game
  JSR Update

  RTI        ; return from interrupt


;;;;;;;;;;;;;;;;;;;;;;
; Draw sprite to screen
Draw:
  
  JSR DrawPlayer
  LDA #%10000000   ; enable NMI, sprites from Pattern Table 0
  STA $2000

  LDA #%00010000   ; enable sprites
  STA $2001

  RTS


;;;;;;;;;;;;;;;;;;;;;
; Load Sprites 
DrawPlayer:
  LDX #$00              ; start at 0
DrawPlayerLoop:
  
  LDA $0203, x          ; load current x sprite position
  CLC
  ADC $00               ; add player x offset
  STA $0203, x          ; store into RAM address ($0200 + x)

  LDA $0200, x          ; load current y sprite position
  CLC
  ADC $01               ; add player y offset
  STA $0200, x          ; stor into R
  INX                   ; X = X + 4 loop to the next sprite
  INX
  INX
  INX 
  CPX #$10              ; Compare X to hex $10, decimal 16 meaning all 4 Player sprites done
  BNE DrawPlayerLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down
  RTS


Update:
    LDA #$00 ;; clear controller vector
    STA $00
    JSR LatchController
    JSR PollController
    JSR ReadA
    JSR ReadB
    
    ;JSR UpdatePositions

    RTS

LatchController:
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016       ; tell both the controllers to latch buttons
  RTS

;;;;;;;;;;;;;;;;;;
; Read controller input into byte vector
; 76543210
; ||||||||
; |||||||+- RIGHT button
; ||||||+-- LEFT Button
; |||||+--- DOWN Button
; ||||+---- UP Button
; |||+----- START Button
; ||+------ SELECT Button
; |+------- B Button
; +-------- A Button

PollController:
  LDX #$00   ; 8 buttons total
PollControllerLoop:
  LDA $4016  ; player 1 - A 
  LSR A      ; shift right
  ROL $03    ; right shift button vector in mem location $0003
  INX
  CPX #$08
  BNE PollControllerLoop
  RTS


HandleRightButton:
  LDA $03         ; load button vector into ADC
  AND #%00000001  ; check right button
  BNE RightButtonDone ; no input skip to end
  LDA $00         ; load sprite X position
  CLC             ; make sure the carry flag is clear
  ADC #$01        ; A = A + 1
  STA $00         ; save sprite X position
RightButtonDone:
  RTS


UpdatePositions:
  JSR HandleRightButton  
  LDA $00         ; load sprite X position
  RTS

ReadA: 
  LDA $03       ; player 1 - A
  AND #%10000000  ; only look at bit 0
  BEQ ReadADone   ; branch to ReadADone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)
  LDA $0203       ; load sprite X position
  CLC             ; make sure the carry flag is clear
  LDA #$01        ; A = A + 1
  STA $00       ; save sprite X position
ReadADone:        ; handling this button is done
  RTS
  

ReadB: 
  LDA $03       ; player 1 - B
  AND #%01000000  ; only look at bit 0
  BEQ ReadBDone   ; branch to ReadBDone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)
  LDA $0203       ; load sprite X position
  ;SEC             ; make sure carry flag is set
  ;SBC #$01        ; A = A - 1
  LDA #$FF        ; -1 in 2's compliment
  STA $00       ; save sprite X position
ReadBDone:        ; handling this button is done
  RTS

;;;;;;;;;;;;;;  
; Data initialization
; .db command is a macro for storing bytes in memory without having to write
; LDA (byte)
; STA (mem)
  
  .bank 1
  .org $E000
palette:
  .db $0F,$31,$32,$33,$0F,$35,$36,$37,$0F,$39,$3A,$3B,$0F,$3D,$3E,$0F
  .db $0F,$1C,$15,$14,$0F,$02,$38,$3C,$0F,$1C,$15,$14,$0F,$02,$38,$3C

sprites:
     ;vert tile attr horiz
  .db $80, $32, $00, $80   ;sprite 0
  .db $80, $33, $00, $88   ;sprite 1
  .db $88, $34, $00, $80   ;sprite 2
  .db $88, $35, $00, $88   ;sprite 3


;;;;;;;;;;;;;;  
; Define interrupt vectors at the top of memory $FFFF
; Basically registering callbacks for different functions NMI, RESET, IRQ
; .org means starting at $FFFA
; .dw means store dataword, in the NES that means 16 bits 2 bytes
; stores in little endian order, superior ordering ;) least sig byte first

  .bank 1
  .org $FFFA     ;first of the three vectors starts here
nescallback:
  ; after this $FFFA + 2 = $FFFC
  .dw MAINLOOP      ;when an NMI happens (once per frame if enabled) the 
                   ;processor will jump to the label NMI:
  ; after this $FFFC + 2 = $FFFE
  .dw RESET      ;when the processor first turns on or is reset, it will jump
                   ;to the label RESET:
  ; after this $FFFC + 1 = $FFFF 
  .dw 0          ;external interrupt IRQ is not used in this tutorial
  
  
;;;;;;;;;;;;;;  
; Load in external sprite or audio data
  
  .bank 2
  .org $0000
  .incbin "mario.chr"   ;includes 8KB graphics file from SMB1