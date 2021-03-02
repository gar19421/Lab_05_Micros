
;-----------Laboratorio 05 Micros------------ 

; Archivo: lab05_main.S
; Dispositivo: PIC16F887
; Autor: Brandon Garrido 
; Compilador: pic-as (v2.30), MPLABX v5.45
;
; Programa: Contador con interrupciones utilizando multiples displays
; Hardware: LEDs en el puerto A - Displays puerto C y D
;
; Creado: 02 de marzo, 2021

PROCESSOR 16F887
#include <xc.inc>

; CONFIG1
  CONFIG  FOSC = INTRC_NOCLKOUT ; Oscillator Selection bits (RC oscillator: CLKOUT function on RA6/OSC2/CLKOUT pin, RC on RA7/OSC1/CLKIN)
  CONFIG  WDTE = OFF            ; Watchdog Timer Enable bit (WDT disabled and can be enabled by SWDTEN bit of the WDTCON register)
  CONFIG  PWRTE = ON            ; Power-up Timer Enable bit (PWRT enabled)
  CONFIG  MCLRE = OFF           ; RE3/MCLR pin function select bit (RE3/MCLR pin function is digital input, MCLR internally tied to VDD)
  CONFIG  CP = OFF              ; Code Protection bit (Program memory code protection is disabled)
  CONFIG  CPD = OFF             ; Data Code Protection bit (Data memory code protection is disabled)
  CONFIG  BOREN = OFF           ; Brown Out Reset Selection bits (BOR disabled)
  CONFIG  IESO = OFF            ; Internal External Switchover bit (Internal/External Switchover mode is disabled)
  CONFIG  FCMEN = OFF           ; Fail-Safe Clock Monitor Enabled bit (Fail-Safe Clock Monitor is disabled)
  CONFIG  LVP = ON              ; Low Voltage Programming Enable bit (RB3/PGM pin has PGM function, low voltage programming enabled)

; CONFIG2
  CONFIG  BOR4V = BOR40V        ; Brown-out Reset Selection bit (Brown-out Reset set to 4.0V)
  CONFIG  WRT = OFF             ; Flash Program Memory Self Write Enable bits (Write protection off)
    
;----------------constants------------------
UP EQU 2
DOWN EQU 7; constantes de los pines I/0
 
;----------------macros----------------------
reiniciar_tmr0 macro ; macro para reutilizar reinicio de tmr0
    movlw 252 ; valor de n para (256-n)
    movwf TMR0 ; delay inicial TMR0
    bcf T0IF
endm
  

 Global var, banderas, nibble, display_var, unidades, decenas, centenas, var_temp
;------------------variables-------------------
PSECT udata_bank0 ;common memory
    var: DS 1 ;1 byte -> para bucle
    banderas: DS 1 ;1 byte -> para contador de display timer0
    nibble: DS 2; variables para contador hexademimal
    display_var: DS 2	
    unidades: DS 1; variables de contador BCD
    decenas: DS 1
    centenas: DS 1
    unidades_disp: DS 1 ; variables a mostrar en displays BCD
    decenas_disp: DS 1
    centenas_disp: DS 1
    var_temp: DS 1 ;variable temporal para valor de portb
    
PSECT udata_shr ;common memory
    W_TEMP: DS 1 ;1 byte
    STATUS_TEMP: DS 1; 1 byte

    
    
PSECT resVect, class=CODE, abs, delta=2
    
;---------------- vector reset --------------------

ORG 00h	    ;posición 0000h para el reset 
resetVec:
    PAGESEL main
    goto main
    

PSECT intVect, class=CODE, abs, delta=2
;---------------- vector de interrupcion --------------------
ORG 04h ;posición 0x0004

push: ;Preservar los valores de W y las banderas
    movwf W_TEMP
    swapf STATUS, W
    movwf STATUS_TEMP 
    
isr: ; rutina de interrupción
    btfsc RBIF ;verificar si la bandera de interrupción PORTB esta levantada
    call int_iocb
    btfsc T0IF ; verifica si la bandera de interrupcion tmr0 esta levantada
    call int_tmr0

pop: ; para re-obtener los valores de W y de las banderas de status
    swapf STATUS_TEMP, W
    movwf STATUS
    swapf W_TEMP, F
    swapf W_TEMP, W
    retfie ; finalizar interrupción
    
;--- subrutina de interrupcion portb----
int_iocb:
    banksel PORTA
    btfss PORTB, UP ;verificar pin activado como pull-up
    incf PORTA
    btfss PORTB, DOWN ;verificar pin activado como pull-up
    decf PORTA
    
    bcf RBIF ; limpiar bandera
    
    return
    
;--- subrutina de interrupcion timer 0----
int_tmr0: 
    reiniciar_tmr0
    clrf PORTD 
    ;verificar bit por bit el display encendido
    btfsc banderas,0
    goto display_unidades
    btfsc banderas,3
    goto display_1
    btfsc banderas,1
    goto display_centenas
    btfsc banderas,2
    goto display_decenas
    
    
display_0: ; display del nibble menos significativo
    movf display_var+0,W
    movwf PORTC
    bsf PORTD,0 ;habilitar pin 0 D para encender display 0
    goto next_display 

display_1: ; display del nibble más significativo
    movf display_var+1,W
    movwf PORTC
    bsf PORTD,1 ;habilitar pin 1 D para encender display 1
    goto next_display
    
display_centenas: ; mostrar display centenas
    movf centenas_disp, W
    movwf PORTC
    bsf PORTD,2 ;habilitar pin 2 D para encender display 2
    goto next_display

display_decenas:; mostrar display decenas
    movf decenas_disp, W
    movwf PORTC
    bsf PORTD,3;habilitar pin 3 D para encender display 3
    goto next_display
    
display_unidades:;mostrar display unidades
    movf unidades_disp, W
    movwf PORTC
    bsf PORTD,4 ;habilitar pin 4 D para encender display 4
    goto next_display
    
next_display:; subrutina para ir iterando entre cada uno de los display
    movf banderas,W
    andlw 0x0f
    incf banderas, F 
    
    return
    
PSECT code, delta=2, abs
ORG 100h    ;posicion para el código
 
tabla: ; tabla de valor de pines encendido para mostrar x valor en el display
    clrf PCLATH
    bsf PCLATH, 0 ; PCLATH = 01 PCL = 02
    andlw 0x0f ; para solo llegar hasta f
    addwf PCL ;PC = PCLATH + PCL + W
    retlw 00111111B ;0
    retlw 00000110B ;1
    retlw 01011011B ;2
    retlw 01001111B ;3
    retlw 01100110B ;4
    retlw 01101101B ;5
    retlw 01111101B ;6
    retlw 00000111B ;7
    retlw 01111111B ;8
    retlw 01101111B ;9
    retlw 01110111B ;A
    retlw 01111100B ;B
    retlw 00111001B ;C
    retlw 01011110B ;D
    retlw 01111001B ;E
    retlw 01110001B ;F 

    
;----------- Configuración -----------------------

main:
    
    call config_io ; PORTA salida; RB7 y RB2 como input
    call config_reloj ;4MHz
    call config_tmr0 ; tmr0 
    call config_iocrb ; configurar interrup on change en puerto b
    call config_int_enable ; configurar banderas de interrupción
    
     
    
;----------------loop principal--------------------
    
loop:
   movf PORTA, W   ; pasar valor de porta a var temporal display hexadecimal
   movwf var
     
   call separar_nibbles 
   call preparar_displays  
   
   ;mover valor de puerto A a la variable temporal de displays BCD
   movf PORTA,W 
   movwf var_temp
   
   call binario_decimal
    
   goto loop	   ;loop forever

   
;-------------subrutinas-------------------------
separar_nibbles:
    movf var,W 
    andlw 0x0f ;deshabilitar nibble op
    movwf nibble
    swapf var,W ;swapear nible y luego sumar 1
    andlw 0x0f
    movwf nibble+1
    
    return
   
preparar_displays:
    movf nibble,W
    call tabla
    movwf display_var
    movf nibble+1,W
    call tabla
    movwf display_var+1
   
    return
    
binario_decimal:
    ;limpiar variables BCD
    clrf centenas
    clrf decenas
    clrf unidades
    ;ver centenas
    movlw 100
    subwf var_temp,F; se resta el al valor de porta 100D 
    btfsc STATUS, 0 ;Revisión de la bandera de carry
    incf centenas, F ; si porta>100 incrementa centenas
    btfsc STATUS, 0
    goto $-4 ; si porta>100 repite proceso 
    addwf var_temp,F;ya no es posible restar mas
	      ;suma nuevamente el valor de var_temp sumandole nuevamente 100D
    
    ;ver decenas
    movlw 10
    subwf var_temp,F; se resta el al valor de porta 10D
    btfsc STATUS, 0 ;Revisión de la bandera de carry
    incf decenas, F; si porta>10 incrementa decenas
    btfsc STATUS, 0 ;Revisión de la bandera de carry
    goto $-4; si porta>10 repite proceso 
    addwf var_temp,F;ya no es posible restar mas
	      ;suma nuevamente el valor de var_temp sumandole nuevamente 10D
	      
    ;ver unidades
    movf var_temp, W
    movwf unidades ; mueve a unidades el restante del procedimiento anterior
		   ; var_temp en este punto es menor o igual a nueve y >0
    
    call preparar_displays2
    
    return
    
preparar_displays2:
    clrf centenas_disp
    clrf decenas_disp
    clrf unidades_disp ; variables para prender displays

    movf centenas, W ; obtener el valor para display de centenas
    call tabla
    movwf centenas_disp
    
    movf decenas, W ; obtener el valor para display de decenas
    call tabla
    movwf decenas_disp
    
    movf unidades, W ; obtener el valor para display de unidades
    call tabla
    movwf unidades_disp
    
    return
    
config_iocrb:
    banksel TRISA
    bsf IOCB, UP
    bsf IOCB, DOWN ; setear IOC en los pines 0 y 7 del puerto B
    
    banksel PORTA
    movf PORTB, W ; al leer termina condición del mismatch
    bcf RBIF
    
    return
  
config_int_enable:; INTCON
    bsf GIE ; banderas globales
    bsf RBIE ; habilitar banderas de interrupción puertos B
    bcf RBIF
    bsf T0IE ; habilitar banderas de interrupción tmr0
    bcf T0IF
    
    return
   
config_reloj:
    banksel OSCCON
    bsf IRCF2 ; IRCF = 110 (4MHz) 
    bsf IRCF1
    bcf IRCF0
    bsf SCS ; reloj interno
    
    return
    
    
;t=4 * (T_osc) * (256-n) (Preescaler) = 1.02ms
config_tmr0:
    banksel TRISA
    bcf T0CS ; reloj interno
    bcf PSA ; prescaler
    bsf PS2 
    bsf PS1 
    bsf PS0 ; PS = 111 (1:256)
    banksel PORTA
    
    reiniciar_tmr0
      
    return
    
    
config_io:
    banksel ANSEL ;banco 11
    clrf ANSEL
    clrf ANSELH ; habilitar puertos digitales A y B
    
    banksel TRISA ;banco 01
    clrf TRISA
    clrf TRISC
    clrf TRISD
    bsf TRISB, UP ;up y down como entradas
    bsf TRISB, DOWN
    
    bcf OPTION_REG, 7 ;habilitar pull-ups
    bsf WPUB, UP
    bsf WPUB, DOWN
    
    banksel PORTA ; banco 00
    clrf PORTA
    clrf PORTC
    clrf PORTD ; limpiar salidas
    clrf var_temp
    
    
    return

    
    
end