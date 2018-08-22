;E:\Programmierung\Delphi7\Projects\CubeOS\CubeOOP\Works\Projects\IFStatement.OOP AT 20.08.2018 19:40:21
.486
locals
jumps
.model flat,STDCALL
;============== Prolog Ended
Include .\include\Prolog.asm
include .\include\win32.inc
_Main:
$_Main proc
push    offset lppaint
push    [hwnd]
call    BeginPaint
mov     [theDC], eax
;var
;Wert1: Int16;
;Wert2: Int16;
;Wert3: Int16;
;begin
;Wert1:=1;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,1
POP ESI ; Restore LS
mov word ptr[Wert1], ax
Pop ESI ; Exit Call_Assignment
;Wert2:=2;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,2
POP ESI ; Restore LS
mov word ptr[Wert2], ax
Pop ESI ; Exit Call_Assignment
;Wert3:=3;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,3
POP ESI ; Restore LS
mov word ptr[Wert3], ax
Pop ESI ; Exit Call_Assignment
;
;if wert1 = 1 then
mov ax, [Wert1]
PUSH ax
mov eax,1
pop dx
cmp dx,ax
mov ax,1
je CM3
xor ax,ax
CM3:
cmp ax,1
je I4
jmp I2
I4:
;begin
;writeln(Wert1)
Push esi
mov ax, [Wert1]
SUB ESP,20
mov ESI,ESP
push EAX               ; Store Result
CLD                    ; clear direction flag => increment EDI
MOV ECX, 20            ; Count init ECX for REP
MOV EAX, 32            ; Char init AL for STOSB
MOV EDI, ESI           ; dest init EDI for STOSB
REP STOSB              ; Repeat: Leerzeichen (#32) in den Speicher laden
pop EAX                ; Load Result
mov Byte ptr[ESI],1    ; String Byte 0
mov Byte ptr[ESI+1],16 ; String Anzahl
ADD ESI,18			    ; Zeiger ans Ende (Zeichen rechts ausrichten)
XOR ECX, ECX		    ; Basis 10
XOR EDX, EDX		    ; 0
Call $_Int32ToStr	  ; Konvertierung
mov ESI, ESP
Call $_StringWrite
ADD ESP,20
Call $_NLN
Pop esi
;end
;else
jmp I8
I2 label near
;begin
;writeln(Wert2);
Push esi
mov ax, [Wert2]
SUB ESP,20
mov ESI,ESP
push EAX               ; Store Result
CLD                    ; clear direction flag => increment EDI
MOV ECX, 20            ; Count init ECX for REP
MOV EAX, 32            ; Char init AL for STOSB
MOV EDI, ESI           ; dest init EDI for STOSB
REP STOSB              ; Repeat: Leerzeichen (#32) in den Speicher laden
pop EAX                ; Load Result
mov Byte ptr[ESI],1    ; String Byte 0
mov Byte ptr[ESI+1],16 ; String Anzahl
ADD ESI,18			    ; Zeiger ans Ende (Zeichen rechts ausrichten)
XOR ECX, ECX		    ; Basis 10
XOR EDX, EDX		    ; 0
Call $_Int32ToStr	  ; Konvertierung
mov ESI, ESP
Call $_StringWrite
ADD ESP,20
Call $_NLN
Pop esi
;end;
I8 label near
;
;Wert3:=16;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,16
POP ESI ; Restore LS
mov word ptr[Wert3], ax
Pop ESI ; Exit Call_Assignment
;writeln(Wert3);
Push esi
mov ax, [Wert3]
SUB ESP,20
mov ESI,ESP
push EAX               ; Store Result
CLD                    ; clear direction flag => increment EDI
MOV ECX, 20            ; Count init ECX for REP
MOV EAX, 32            ; Char init AL for STOSB
MOV EDI, ESI           ; dest init EDI for STOSB
REP STOSB              ; Repeat: Leerzeichen (#32) in den Speicher laden
pop EAX                ; Load Result
mov Byte ptr[ESI],1    ; String Byte 0
mov Byte ptr[ESI+1],16 ; String Anzahl
ADD ESI,18			    ; Zeiger ans Ende (Zeichen rechts ausrichten)
XOR ECX, ECX		    ; Basis 10
XOR EDX, EDX		    ; 0
Call $_Int32ToStr	  ; Konvertierung
mov ESI, ESP
Call $_StringWrite
ADD ESP,20
Call $_NLN
Pop esi
;end;
;
push    offset lppaint
push    [hwnd]
call    EndPaint
RET
$_Main endp
Include .\include\Lib.asm
.data
copyright        db '(c)M.Hoeller-Schlieper CubeOS 2007',0
newhwnd          dd 0
lppaint          PAINTSTRUCT <?>
msg              MSGSTRUCT   <?>
wc               WNDCLASS    <?>
hInst            dd 0
szTitleName      db 'CubeOS - Test Environment'
szClassName      db 'CubeOS',0
_Y				       dd 5
_X				       dd 5
_TextHeight		   dd 15
DecimalSep       db ','
ThousandSep      db '.'
Precision        dd 2
Digits           dd 2
conBuffer		     db 1,16,'                '
parstr           db 127,0,127 dup(?)
str              db 255,0,255 dup(32)
concat           db 255,0,255 dup(0)
cpystr           db 255,0,255 dup(0)
nts              db 7,0,7 dup(0)
zero             db 0
dummystr         db 1,1,0
chrchr           db 1,1,32
minus            db 1,1,"-"
Allocated        dd 0
MemBlock         dd 1024 dup (0); dynamischen Speicher reservieren
EndPtr           dd 0
;======== global Vars
Wert1 dw 0
Wert2 dw 0
Wert3 dw 0
ends
end start
