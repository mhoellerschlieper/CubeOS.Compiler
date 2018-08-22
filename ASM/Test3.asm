;E:\Programmierung\Delphi7\Projects\CubeOS\CubeOOP\Works\Projects\Test3.OOP AT 20.08.2018 19:23:57
.486
locals
jumps
.model flat,STDCALL
;============== Prolog Ended
Include .\include\Prolog.asm
;//Environment: CubeOOP
;//Title:
;//Autor:
;//Date: 29.07.2007 09:47:02
;//Description:
;//*************************************************************
;class TTest1 of Base implements
;Public
;iValue1: Int32;
;iValue2: Int32;
;iValue3: Int32;
;iValue4: Int32;
;
;
;//**************************************************
;//
;//**************************************************
;
;Procedure Power(
TTest1_Power proc near
push ebp
mov ebp,esp
push ebp
;iPValue2: Int32;
;iMultiplikator: Int32);
;var iCount:Int32;
;begin
SUB ESP,4
;iCount := 1;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,1
POP ESI ; Restore LS
mov dword ptr [ebp - 4], eax
Pop ESI ; Exit Call_Assignment
;iValue1 := iPValue2;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,[ebp + 12]
POP ESI ; Restore LS
mov dword ptr [esi + 12], eax
Pop ESI ; Exit Call_Assignment
;while iCount <  iMultiplikator do
W4 label near
mov eax,[ebp - 4]
PUSH eax
mov eax,[ebp + 8]
pop dx
cmp dx,ax
mov ax,1
jl CM6
xor ax,ax
CM6:
cmp ax,1
je W7
jmp W5
W7:
;begin
;iValue1 := iValue1 * iPValue2;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,[esi + 12]
push eax
mov eax,[ebp + 12]
pop edx
mul edx
POP ESI ; Restore LS
mov dword ptr [esi + 12], eax
Pop ESI ; Exit Call_Assignment
;iCount := iCount + 1;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,[ebp - 4]
push eax
mov eax,1
pop EDX
add edx,eax
mov eax,edx
POP ESI ; Restore LS
mov dword ptr [ebp - 4], eax
Pop ESI ; Exit Call_Assignment
;end;
jmp W4
W5 label near
;end;
mov esp,ebp
pop ebp
RET 8
TTest1_Power ENDP
;end;
;
;//*************************************************************
;Main;
include .\include\win32.inc
_Main:
$_Main proc
push    offset lppaint
push    [hwnd]
call    BeginPaint
mov     [theDC], eax
;Var Test1:TTest1;
;Begin
;Test1:=Create(TTest1);
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov EAX,16
CALL $_Allocate
POP ESI ; Restore LS
mov dword ptr [Test1], eax
Pop ESI ; Exit Call_Assignment
;Test1.Power(2,16);
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
mov eax,[Test1]
mov esi,eax
mov eax,2
push eax
mov eax,16
push eax
Call TTest1_Power
POP ESI ; RS not used
Pop ESI ; Exit Call_Assignment
;Writeln(Test1.iValue1);
Push esi
mov eax,[Test1]
mov esi,eax
mov eax,[esi + 12]
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
Test1 dd 0,0,0,0
ends
end start
