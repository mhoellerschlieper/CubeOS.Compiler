;C:\Entwicklung\Delphi\Projects\Privat\CubeOS\CubeOOP\Works\Projects\Chartest.OOP AT 29.01.2014 14:41:07
.486
locals
jumps
.model flat,STDCALL
;============== Prolog Ended
Include .\include\Prolog.asm
;//Environment: CubeOOP
;//Title:
;//Autor:
;//Date: 11.08.2009 20:52:03
;//Description:
;//*************************************************************}
;Main;
include .\include\win32.inc
_Main:
$_Main proc
push    offset lppaint
push    [hwnd]
call    BeginPaint
mov     [theDC], eax
;var i:Int16;
;cha:Char;
;chb:Char;
;b:Byte;
;bool:Boolean;
;s1:String;
;s2:String;
;D1: Double;
;D2: Double;
;Begin
;CHa := 'z';
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
XOR eax,eax
mov al,122
POP ESI ; Restore LS
mov byte ptr [cha]  , al
Pop ESI ; Exit Call_Assignment
;CHb := 'Y';
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
XOR eax,eax
mov al,89
POP ESI ; Restore LS
mov byte ptr [chb]  , al
Pop ESI ; Exit Call_Assignment
;i := 1233;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,1233
POP ESI ; Restore LS
mov word ptr[i], ax
Pop ESI ; Exit Call_Assignment
;s1 := 'Dies ist ein Test...';
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
LEA eax, $_S1
POP ESI ; Restore LS
mov dword ptr [s1], eax
Pop ESI ; Exit Call_Assignment
;s2 := 'abcvdfvdfvdf dfvdvf d fvd fvdfvdfvd';
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
LEA eax, $_S2
POP ESI ; Restore LS
mov dword ptr [s2], eax
Pop ESI ; Exit Call_Assignment
;
;D1:= 10.0;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,1092616192
POP ESI ; Restore LS
mov [D1], eax
Pop ESI ; Exit Call_Assignment
;D2:= 20.0;
Psh ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,1101004800
POP ESI ; Restore LS
mov [D2], eax
Pop ESI ; Exit Call_Assignment
;
;Writeln(D1);
Push esi
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
;Writeln(D2);
Push esi
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
;
;D2:= D1+D2;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
push eax
pop EDX
SUB  ESP,16
MOV  [ESP-4], EDX
MOV  [ESP-8], EAX
FLD  dword ptr[ESP-4]
FADD dword ptr[ESP-8]
FSTP dword ptr[ESP-8]
WAIT
MOV EDX, dword ptr[ESP-8]
ADD ESP,16
mov eax,edx
POP ESI ; Restore LS
mov [D2], eax
Pop ESI ; Exit Call_Assignment
;Writeln(D2);
Push esi
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
;
;Writeln(CHa);
Push esi
mov al,[cha]
Call $_CharWrite
Call $_NLN
Pop esi
;CHb := Cha;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov al,[cha]
POP ESI ; Restore LS
mov byte ptr [chb]  , al
Pop ESI ; Exit Call_Assignment
;Writeln(CHb);
Push esi
mov al,[chb]
Call $_CharWrite
Call $_NLN
Pop esi
;Writeln(s2);
Push esi
mov eax, dword ptr [s2]
mov esi,eax
Call $_StringWrite
Call $_NLN
Pop esi
;s2 := s1;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax, dword ptr [s1]
POP ESI ; Restore LS
mov dword ptr [s2], eax
Pop ESI ; Exit Call_Assignment
;Writeln(s2);
Push esi
mov eax, dword ptr [s2]
mov esi,eax
Call $_StringWrite
Call $_NLN
Pop esi
;Writeln(s1);
Push esi
mov eax, dword ptr [s1]
mov esi,eax
Call $_StringWrite
Call $_NLN
Pop esi
;Writeln(i);
Push esi
mov ax, [i]
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
;End; // of Maintask
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
$_S1 db 20,20,'Dies ist ein Test...'
$_S2 db 35,35,'abcvdfvdfvdf dfvdvf d fvd fvdfvdfvd'
i dw 0
cha db 0
chb db 0
b db 0
bool db 0
s1 db 0,0, 80 DUP(32)
s2 db 0,0, 80 DUP(32)
D1 dd 0
D2 dd 0
ends
end start
