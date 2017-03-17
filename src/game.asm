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
  


;; DECLARE SOME VARIABLES HERE
  .rsset $0000  ;;start variables at ram location 0 in zero page memory

loopCount     .rs 1 ; count the loop
playerx       .rs 1 ; players x pos
playervx      .rs 1 ; players x vel
playery       .rs 1 ; players y pos
playervy      .rs 1 ; player  y vel (negative is up)
controller1   .rs 1 ; controller 1 button vector

gravity       .rs 1 ; gravity


ground        .rs 1 ; y value of the ground
inAir         .rs 1 

enemyx        .rs 1
enemyy        .rs 1

backgroundLo  .rs 1
backgroundHi  .rs 1
counterLo     .rs 1
counterHi     .rs 1


scroll     .rs 1  ; horizontal scroll count
nametable  .rs 1  ; which nametable to use, 0 or 1
columnLow  .rs 1  ; low byte of new column address
columnHigh .rs 1  ; high byte of new column address
sourceLow  .rs 1  ; source for column data
sourceHigh .rs 1
columnNumber .rs 1  ; which column of level data to draw


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

;; Helper to wipe memory
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
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$3F              ; max out 0011 1111
  STA $2006             ; write the high byte of $3F00 address
  LDA #$00
  STA $2006             ; write the low byte of $3F00 address
  LDX #$00              ; start out at 0
LoadPalettesLoop:
  LDA palette, x        ; load data from address (palette + the value in x)
                          ; 1st time through loop it will load palette+0
                          ; 2nd time through loop it will load palette+1
                          ; 3rd time through loop it will load palette+2
                          ; etc
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $10, decimal 16 - copying 16 bytes = 4 sprites
  BNE LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero


LoadSprites:
  LDX #$00              ; start at 0
LoadSpritesLoop:
  LDA playersprite, x        ; load data from address (sprites +  x)
  STA $0200, x          ; store into RAM address ($0200 + x)
  INX                   ; X = X + 1
  CPX #$1C              ; Compare X to hex $20, decimal 32
  BNE LoadSpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down


;; Background is 960 bytes 240 * 4
LoadBackground:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA $2006             ; write the low byte of $2000 address
  LDX #$00              ; start out at 0

  ;; we need to copy more that 256
  LDA #LOW(background)
  STA backgroundLo
  LDA #HIGH(background)
  STA backgroundHi
  ;; 960 bytes = $03C0
  LDA #$C0;
  STA counterLo
  LDA #$03
  STA counterHi

  LDY #$00; keep y zero, we jut need y to be init to 0 for indirect index mode to work in the square bracket
LoadBackgroundLoop:
  LDA [backgroundLo], y ; load data from background
  STA $2007             ; write to PPU data port to copy to background data
  LDA backgroundLo      ; load the low byte of the background address into A
  CLC                   ; clear the carry bit
  ADC #$01              ; add 1 to A
  STA backgroundLo      ; store A back into the mem addr
  LDA backgroundHi      ; load the high byte of the background address into A
  ADC #$00              ; add 0, but if there is a carry (overflow) add 1
  STA backgroundHi      ; inc poitner to the next byte if necessary

  LDA counterLo         ; load the counter low byte
  SEC                   ; set cary flag
  SBC #$01              ; subtract with carry by 1
  STA counterLo         ; store the low byte of the counter
  LDA counterHi         ; load the high byte
  SBC #$00              ; sub 0, but there is a carry
  STA counterHi       ; decrement the loop counter

  LDA counterLo         ; load the low byte
  CMP #$00              ; see if it is zero, if not loop
  BNE LoadBackgroundLoop
  LDA counterHi
  CMP #$00              ; see if the high byte is zero, if not loop
  BNE LoadBackgroundLoop  ; if the loop counter isn't 0000, keep copying


  ;LDA background, x     ; load data from address (background + the value in x)
  ;STA $2007             ; write to PPU
  ;INX                   ; X = X + 1
  ;CPX #$80              ; Compare X to hex $80, decimal 128 - copying 128 bytes
  ;BNE LoadBackgroundLoop  ; Branch to LoadBackgroundLoop if compare was Not Equal to zero
                        ; if compare was equal to 128, keep going down

LoadAttribute:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$23
  STA $2006             ; write the high byte of $23C0 address
  LDA #$C0
  STA $2006             ; write the low byte of $23C0 address
  LDX #$00              ; start out at 0
LoadAttributeLoop:
  LDA attribute, x      ; load data from address (attribute + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$40              ; Compare X to hex $40, decimal 64
  BNE LoadAttributeLoop  ; Branch to LoadAttributeLoop if compare was Not Equal to zero
                        ; if compare was equal to 128, keep going down


;  PPUCTRL ($2000)
;  76543210
;  | ||||||
;  | ||||++- Base nametable address
;  | ||||    (0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00)
;  | |||+--- VRAM address increment per CPU read/write of PPUDATA
;  | |||     (0: increment by 1, going across; 1: increment by 32, going down)
;  | ||+---- Sprite pattern table address for 8x8 sprites (0: $0000; 1: $1000)
;  | |+----- Background pattern table address (0: $0000; 1: $1000)
;  | +------ Sprite size (0: 8x8; 1: 8x16)
;  |
;  +-------- Generate an NMI at the start of the
;            vertical blanking interval vblank (0: off; 1: on)              
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000

;  PPUMASK ($2001)
;  binary byte flags
;  76543210
;  ||||||||
;  |||||||+- Grayscale (0: normal color; 1: AND all palette entries
;  |||||||   with 0x30, effectively producing a monochrome display;
;  |||||||   note that colour emphasis STILL works when this is on!)
;  ||||||+-- Disable background clipping in leftmost 8 pixels of screen
;  |||||+--- Disable sprite clipping in leftmost 8 pixels of screen
;  ||||+---- Enable background rendering
;  |||+----- Enable sprite rendering
;  ||+------ Intensify reds (and darken other colors)
;  |+------- Intensify greens (and darken other colors)
;  +-------- Intensify blues (and darken other colors)
  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001


;;;;;;;;;;;;;;;;;;;;
; Initialze some game state variables
InitialzeState:
  LDA #$80   ; player x offset
  STA playerx

  LDA #$00   ; player y offset
  STA playery
  LDA #$C8   ; player y offset
  STA ground  

  LDA #$00   ; controller state
  STA controller1
  STA playervx;
  STA playervy;
  STA loopCount

  LDA #$03
  STA gravity




Forever:
  JMP Forever     ;jump back to forever MAINLOOP will interrupt for game logic, infinite loop to keep the rom from exiting
 
;; Game main loop
MAINLOOP: ; non-maskable interrupt (draw screen)
  ;; Needs to be done every frame
  ;; Load graphics into PPU from the memory
  LDA #$00   ; load 1 bytes of 0 in A
  STA $2003  ; set the low byte (00) of the RAM address
  LDA #$02   ; load 1 byte of $02, 
  STA $4014  ; set the high byte (02) of the RAM address, start the transfer

  ;; Draw game first
  JSR Draw 
  
  ;; Update game
  JSR Update 

  RTI        ; return from interrupt


;;;;;;;;;;;;;;;;;;;;;;
; Draw sprite to screen
Draw:
  
  JSR DrawPlayer

  ;;This is the PPU clean up section, so rendering the next frame starts properly.
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000
  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001
  LDA #$00        ;;tell the ppu there is no background scrolling
  STA $2005
  STA $2005

  RTS


;;;;;;;;;;;;;;;;;;;;;
; Draw Sprites 
DrawPlayer:
  LDX #$00              ; start at 0 
  LDY #$00              ; start at 0
DrawPlayerLoop:
  
  LDA $0203, x          ; load current x sprite position
  CLC  
  LDA playerspriteoffset, y    ; add player x with sprite offset
  ADC playerx 
  INY                   ; increment sprite offset coutner
  STA $0203, x          ; store into RAM address ($0203 + x)

  LDA $0200, x          ; load current y sprite position
  CLC
  LDA playerspriteoffset, y   ; add player y with sprite offset
  ADC playery
  INY
  STA $0200, x          ; store into RAM address ($0200 + x)
  INX                   ; X = X + 4 loop to the next sprite
  INX
  INX
  INX 
  CPX #$1C              ; Compare X to hex $1C, decimal 28 meaning all 7 Player sprites done
  BNE DrawPlayerLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down
  RTS

DrawEnemey:
  LDX #$00              ; start at 0 
  LDY #$00              ; start at 0
DrawEnemyLoop:
  
  LDA $0203, x          ; load current x sprite position
  CLC  
  LDA enemyspriteoffset, y    ; add player x with sprite offset
  ADC enemyx
  INY                   ; increment sprite offset coutner
  STA $0203, x          ; store into RAM address ($0203 + x)

  LDA $0200, x          ; load current y sprite position
  CLC
  LDA playerspriteoffset, y   ; add player y with sprite offset
  ADC enemyy
  INY
  STA $0200, x          ; store into RAM address ($0200 + x)
  INX                   ; X = X + 4 loop to the next sprite
  INX
  INX
  INX 
  CPX #$08              ; Compare X to hex $1C, decimal 28 meaning all 7 Player sprites done
  BNE DrawEnemyLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down
  RTS

Update:
    JSR LatchController
    JSR PollController
    JSR ReadLeft
    JSR ReadRight
    JSR ReadA
    INC loopCount
    JSR UpdatePlayerPosition
    RTS


UpdatePlayerPosition:
  CLC
  LDA loopCount
  CMP #$0A
  BNE SkipGravity
  LDA gravity
  ADC playervy
  STA playervy
  LDA #$00
  STA loopCount
SkipGravity:
  LDA playervy
  CLC
  ADC playery
  STA playery  
  CMP ground
  BCS PutPlayerOnGround
PlayerOnGroundDone: 
  LDA playervx
  CLC
  ADC playerx
  STA playerx 
  RTS
PutPlayerOnGround:
  LDA ground
  STA playery
  LDA #$00
  STA playervy
  STA inAir
  JMP PlayerOnGroundDone


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
  ROL controller1    ; rotate left button vector in mem location $0003
  INX
  CPX #$08
  BNE PollControllerLoop
  RTS

ReadRight: 
  LDA controller1  ; controller1 1 - A button
  AND #%00000001  ; only look at bit 0
  BEQ ReadRightDone   ; branch to ReadADone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)
  LDA $0203       ; load sprite X position
  CLC             ; make sure the carry flag is clear
  LDA playerx 
  ADC #$02
  STA playerx;
ReadRightDone:        ; handling this button is done
  RTS
  

ReadLeft: 
  LDA controller1       ; controller1 1 - B button
  AND #%00000010  ; only look at bit 0
  BEQ ReadLeftDone   ; branch to ReadBDone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)
  LDA $0203       ; load sprite X position
  CLC
  LDA playerx
  ADC #$FE 
  STA playerx
ReadLeftDone:        ; handling this button is done
  RTS

ReadA: 
  LDA controller1       ; controller1 1 - B button
  AND #%10000000  ; only look at bit 0
  BEQ ReadADone   ; branch to ReadBDone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)  
  LDA inAir
  CMP #$01
  BEQ ReadADone

  LDA ground
  STA playery
  LDA #$FA
  STA playervy

  LDA #$01
  STA inAir

ReadADone:        ; handling this button is done
  RTS

;;;;;;;;;;;;;;  
; Data initialization
; .db command is a macro for storing bytes in memory without having to write
; LDA (byte)
; STA (mem)
  
  .bank 1
  .org $E000
palette:
  ;; Background Palletes (0-3)
  .db $08,$1A,$38,$18, $08,$02,$38,$3C, $08,$1C,$15,$14, $08,$02,$38,$2A
  ;;  Character Palletes (0-3)
  .db $21,$2C,$11,$15, $0F,$35,$36,$37, $0F,$39,$3A,$3B, $0F,$3D,$3E,$0F

playersprite:
; 1st byte encodes the y position
; 2nd byte encodes the tile index loaded into the PPU 
; 3rd byte encodes any sprite attributes
;  76543210
;  |||   ||
;  |||   ++- Color Palette of sprite.  Choose which set of 4 from the 16 colors to use
;  |||
;  ||+------ Priority (0: in front of background; 1: behind background)
;  |+------- Flip sprite horizontally
;  +-------- Flip sprite vertically
; 4th byte encodes the x position

     ;vert tile attr horiz
  .db $80, $00, $00, $80   ;sprite 0
  .db $80, $01, $00, $88   ;sprite 1
  .db $80, $02, $00, $90   ;sprite 2
  .db $88, $10, $00, $80   ;sprite 3
  .db $88, $11, $00, $88   ;sprite 4
  .db $88, $12, $00, $90   ;sprite 5
  .db $90, $21, $00, $88   ;sprite 6

playerspriteoffset:
      ;x   y
  .db $F8, $F0; (-8, -16)
  .db $00, $F0; (0,  -16)
  .db $08, $F0; (8 , -16)
  .db $F8, $F8; (-8, -8) 
  .db $00, $F8; (0,  -8)
  .db $08, $F8; (8,  -8)
  .db $00, $00; (0,  0)  


enemyspriteframe1:
  .db $F0, $04, $00, $00
  .db $F0, $05, $00, $08

enemyspriteframe2:
  .db $F0, $14, $00, $00
  .db $F0, $15, $00, $08

enemyspriteoffset:
  .db $00, $00
  .db $00, $08

background:
  ; nametable 960 bytes long for the background
  ;.db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  ;.db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10

  ;.db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  ;.db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10

  ;.db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  ;.db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10

  ;.db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  ;.db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10

  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;row 1
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;all sky
  
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;row 1
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;all sky
  
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;row 1
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;all sky
  
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;row 1
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;all sky
  

  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;row 1
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;all sky
  
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;row 1
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;all sky
  
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;row 1
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;all sky


  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;row 1
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;all sky

  
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;row 1
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;all sky

  
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;row 1
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;all sky

  
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;row 1
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;all sky


  
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;row 1
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;all sky


  
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;row 1
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;all sky

  
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;row 1
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;all sky
  
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;row 1
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;all sky
  
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;row 1
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;all sky
  
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;row 1
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;all sky
  
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;row 1
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;all sky

  
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;row 1
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;all sky

    
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;row 1
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;all sky
  
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;row 1
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;all sky

  
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;row 1
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;all sky

  
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;row 1
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;all sky

  
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;row 1
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;all sky

  
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;row 1
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;all sky

  
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;row 1
  .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11  ;;all sky

  .db $00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01
  .db $00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01  ;;ground

  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10  ;; dirt

  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10  ;; dirt
  
  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10  ;; dirt

attribute:
   ; 64 bytes following a nametable
  ;.db $C7,$00,$00,$00, $00,$00,$00,$00
  .db $00,$00,$00,$00, $00,$00,$00,$00
  .db $00,$00,$00,$00, $00,$00,$00,$00
  .db $00,$00,$00,$00, $00,$00,$00,$00
  .db $00,$00,$00,$00, $00,$00,$00,$00
  .db $00,$00,$00,$00, $00,$00,$00,$00
  .db $00,$00,$00,$00, $00,$00,$00,$00
  .db $00,$00,$00,$00, $00,$00,$00,$00
  .db $00,$00,$00,$00, $00,$00,$00,$00




; Load in level columns
; The first 32 bytes is the first column and so on, each byte is sprite index
;columnData:
  ;.incbin "level.bin"

; Load column sprite attributes
; TODO how is this defined?
;attribData:
  ;.incbin "attr.bin"


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
  .incbin "art.chr"   ;includes 8KB graphics file