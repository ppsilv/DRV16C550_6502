;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; BIOS
; AUTHOR: Paulo da Silva (pgordao) copyright (C) 2024
;
;
;; vERSION 0.0.1

.setcpu "6502"
.debuginfo

.segment "ZEROPAGE"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; These variable is just for test of UART.
MSGL     = $33
MSGH     = $34
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; These variables is to save registers X and Y in cpu 6502 although it does
; not have PHX and PHY.
MPHX     = $35
MPHY     = $36
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Counter is to be used com DEC in 6502 that does not has a dec for Acc
COUNTER  = $37
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Flag to sign the use of WRITE_BYTE or WRITE_BYTE_LF
MEOR     = $38
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.segment "BIOS"

;Uart registers
PORT = $7800  ;;Uart address
R_RX = $00    ;;receiver buffer register (read only)
R_TX = $00    ;;transmitter holding register (write only)
RDLL = $00    ;;divisor latch LSB (if DLAB=1)
RDLH = $01    ;;divisor latch HSB (if DLAB=1)
RIER = $01    ;;interrupt enable register
RIIR = $02    ;;interrupt identification register
RFCR = $02    ;;FIFO control register
RLCR = $03    ;;line control register
RMCR = $04    ;;modem control register
RLSR = $05    ;;line status register
RMSR = $06    ;;modem status register
RSCR = $07	;;scratch register

; Constants
.if .not .def(CR)
	CR  = $0D ; Carriage Return
.endif
.if .not .def(LF)
	LF  = $0A ; Line feed
.endif

DIV_4800_LO   = 24
DIV_4800_HI   = 0
DIV_9600_LO   = 12
DIV_9600_HI   = 0
DIV_19200_LO  = 6
DIV_19200_HI  = 0
DIV_115200_LO = 1
DIV_115200_HI = 0
POLLED_MODE   = %00000000
LCR_8N1       = %00000011
DLAB          = %10000000
FIFO_ENABLE   = %00000111 ;%00000111
THR_EMPTY     = %01100000       ;;

DATA_READY    = %00000001
OVERRUN_ERR   = %00000010
PARITY_ERR    = %00000100
FRAMING_ERR   = %00001000
BREAK_INT     = %00010000
MCR_DTR  = $01  ;dtr output
MCR_RTS  = $02  ;rts output
MCR_OUT1 = $04  ;output #1
MCR_OUT2 = $08  ;output #2
MCR_LOOP = $10  ;loop back
MCR_AFCE = $20  ;auto flow control enable

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; INITUART: Initialize uart 16C550
; Registers changed: NONE
;
INITUART:
    LDA        #DLAB               ;set the divisor latch access bit (DLAB)
    STA        PORT+RLCR
    LDA        #DIV_9600_LO        ;store divisor low byte (9600 baud @ 1,8 MHz clock)
    STA        PORT+RDLL
    LDA        #DIV_9600_HI        ;store divisor hi byte
    STA        PORT+RDLH
    LDA        #FIFO_ENABLE        ;enable the UART FIFO
    STA        PORT+RFCR
    LDA        #POLLED_MODE	       ;disable all interrupts
    STA        PORT+RIER
	LDA        #LCR_8N1            ;set 8 data bits, 1 stop bit, no parity, disable DLAB
    STA        PORT+RLCR
    LDA        #MCR_OUT2 + MCR_RTS + MCR_DTR + MCR_AFCE
    STA        PORT+RMCR
    LDA        PORT+R_RX           ;Clear RX buffer
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; B_READ_BYTE: Read byte from UART waiting for it (BLOCANT)
; Registers changed: A
; Flag CARRY not changed.
;
B_READ_BYTE:
	LDA PORT+RLSR 												;// check the line status register:
	AND #(OVERRUN_ERR | PARITY_ERR | FRAMING_ERR | BREAK_INT)   ; check for errors
	BEQ @NO_ERR 												    ;// if no error bits, are set, no error
	LDA PORT+R_RX
	JMP B_READ_BYTE
@NO_ERR:
	LDA PORT+RLSR 												    ;// reload the line status register
	AND #DATA_READY
	BEQ B_READ_BYTE   											;// if data ready is not set, loop
	LDA PORT+R_RX
;ECHO CHAR
    ;JSR WRITE_BYTE
;*********
	RTS
										    ;// otherwise, there was an error. Clear the error byte
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; READ_BYTE: Read byte from UART waiting for it (NO BLOCANT)
; Registers changed: A, Y
; Flag CARRY: Set when character ready
;             Clear when no character ready
READ_BYTE:
    STY     MPHY                ; Save Y Reg
	LDA PORT+RLSR 												;// check the line status register:
	AND #(OVERRUN_ERR | PARITY_ERR | FRAMING_ERR | BREAK_INT)   ; check for errors
	BEQ @NO_ERR 												    ;// if no error bits, are set, no error
	LDA PORT+R_RX
	JMP NO_CHAR ;READ_BYTE
@NO_ERR:
	LDA PORT+RLSR 												    ;// reload the line status register
	AND #DATA_READY
	BEQ NO_CHAR   											;// if data ready is not set, loop
	LDA PORT+R_RX
    LDY     #$FF
@txdelay:
    DEY
    BNE     @txdelay
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ECHO CHAR
    ;JSR WRITE_BYTE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LDY     MPHY
	SEC		    										;// otherwise, we have data! Load it. 				    									;// clear the carry flag to indicate no error
	RTS
NO_CHAR:
    LDY     #$FF
@txdelay1:
    DEY
    BNE     @txdelay1
    LDY     MPHY
    CLC
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; WRITE_BYTE: Write byte to UART
; Registers changed: NONE
; Flag CARRY not changed.
;
WRITE_BYTE:
    STY     MPHY                ; Save Y Reg
    STX     MPHX                ; Save X Reg
    PHA                         ; Save A Reg
WAIT_FOR_THR_EMPTY:
    LDA     PORT+RLSR           ; Get the Line Status Register
    AND     #THR_EMPTY          ; Check for TX empty
    BEQ     WAIT_FOR_THR_EMPTY 	; loop while the THR is not empty
	PLA
	STA     PORT+R_TX 			; send the byte
    PHA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;DELAY BETWEEN CHAR SENT
    LDY     #$10
@YDELAY:
    LDA     #$FF
    STA     COUNTER
@txdelay:
    DEC     COUNTER
    BNE     @txdelay
    DEY
    BNE     @YDELAY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    PLA
    LDX     MPHX                ; Restore X Reg
    LDY     MPHY                ; Restore Y Reg
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; WRITE_BYTE_WITH_ECHO: Write byte to UART, IF BYTE IS 0D WRITE 0A(LF) TOO
; Registers changed: NONE
; Flag CARRY not changed.
;
WRITE_BYTE_WITH_ECHO:
    STY     MPHY                ; Save Y Reg
    STX     MPHX                ; Save X Reg
    PHA                         ; Save A Reg
@WAIT_FOR_THR_EMPTY:
    LDA     PORT+RLSR           ; Get the Line Status Register
    AND     #THR_EMPTY          ; Check for TX empty
    BEQ     @WAIT_FOR_THR_EMPTY 	; loop while the THR is not empty
	PLA
	STA     PORT+R_TX 			; send the byte
    PHA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;DELAY BETWEEN CHAR SENT
    LDY     #$10
@YDELAY:
    LDA     #$FF
    STA     COUNTER
@txdelay:
    DEC     COUNTER
    BNE     @txdelay
    DEY
    BNE     @YDELAY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;WRITE A LF IF ACC HAS A $0D IN IT
    PLA
    CMP     #$0D
    BNE     WRITE_BYTE_WITH_ECHO_FIM
    LDA     #$0A
    JSR     WRITE_BYTE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
WRITE_BYTE_WITH_ECHO_FIM:
    LDX     MPHX                ; Restore X Reg
    LDY     MPHY                ; Restore Y Reg
    RTS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The code from here is just for test of UART.
RESET:
	    SEI						; No interrupt
	    CLD						; Set decimal
	    LDX #$ff				; Set stack pointer
	    TXS

        JSR     INITUART
        LDA     #$00
        STA     MEOR
        LDA     #<MSG0
        STA     MSGL
        LDA     #>MSG0
        STA     MSGH
        JSR     SHWMSG
WORK:
        LDA     MEOR
        CMP     #$FF
        BEQ     WR_LF
        JSR     READ_BYTE
        BCC     WORK
        CMP     #'S'
        BEQ     GO_LF
        JSR     WRITE_BYTE
        JMP     WORK
WR_LF:
        JSR     B_READ_BYTE
        CMP     #'S'
        BEQ     GO_LF
        JSR     WRITE_BYTE_WITH_ECHO
        JMP     WORK


GO_LF:
        LDA     #$FF
        EOR     MEOR
        STA     MEOR
        JMP     WORK

;*************************************************
SHWMSG:         LDY #$0
SMSG:           LDA (MSGL),Y
                BEQ SMDONE
                JSR WRITE_BYTE
                LDA #$FF
                STA COUNTER
@txdelay:
                DEC COUNTER
                BNE @txdelay
                INY
                BNE SMSG
SMDONE:         RTS

MSG0:       .byte $0D,$0A,"PROGRAM INIT 2024 - V 0.0.1",$0D,$0A,0

.segment "RESETVEC"
                .word   $0F00          ; NMI vector
                .word   RESET          ; RESET vector
                .word   $0000          ; IRQ vector
