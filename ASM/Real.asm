;E:\Programmierung\Delphi7\Projects\CubeOS\CubeOOP\Works\Projects\Real.OOP AT 21.08.2018 20:30:51
.486
locals
jumps
.model flat,STDCALL
;============== Prolog Ended
Include .\include\Prolog.asm
;//Environment: CubeOOP
;//Title:
;//Autor:
;//Date: 31.08.2007 15:39:51
;//Description:
;//*************************************************************}
;class TMath of Base implements
;Public
;function FloatToStr(sNumber: Single): String;
TMath_FloatToStr proc near
push ebp
mov ebp,esp
push ebp
;begin
SUB ESP,81
;Result := '12345';
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
LEA eax, $_S1
POP ESI ; Restore LS
mov dword ptr [ebp - 4], eax
Pop ESI ; Exit Call_Assignment
;end;
mov EAX, DWord ptr [ebp - 4]
mov esp,ebp
pop ebp
RET 4
TMath_FloatToStr ENDP
;end;
;
;//*************************************************************}
;
;Main;
include .\include\win32.inc
_Main:
$_Main proc
push    offset lppaint
push    [hwnd]
call    BeginPaint
mov     [theDC], eax
;Var
;r1: Single;
;r2: Single;
;
;r3: Single;
;
;M: TMath;
;s: string;
;
;Begin
;M:= create(TMath);
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov EAX,0
CALL $_Allocate
POP ESI ; Restore LS
mov dword ptr [M], eax
Pop ESI ; Exit Call_Assignment
;r1:= 7.5;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,40F00000h
POP ESI ; Restore LS
mov dword ptr [r1], eax
Pop ESI ; Exit Call_Assignment
;r2:= 7.5;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,40F00000h
POP ESI ; Restore LS
mov dword ptr [r2], eax
Pop ESI ; Exit Call_Assignment
;r1:= r1 + r2*2.0;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,[r1]
push eax
mov eax,[r2]
push eax
mov eax,40000000h
pop edx
SUB  ESP,8
MOV  [ESP], EDX
MOV  [ESP+4], EAX
FLD  dword ptr[ESP]
FMUL dword ptr[ESP+4]
FSTP dword ptr[ESP+4]
WAIT
MOV EAX, dword ptr[ESP+4]
ADD ESP,8
pop EDX
SUB  ESP,8
MOV  [ESP], EDX
MOV  [ESP+4], EAX
FLD  dword ptr[ESP]
FADD dword ptr[ESP+4]
FSTP dword ptr[ESP+4]
WAIT
MOV EDX, dword ptr[ESP+4]
ADD ESP,8
mov eax,edx
POP ESI ; Restore LS
mov dword ptr [r1], eax
Pop ESI ; Exit Call_Assignment
;s := M.FloatToStr(r1);
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,[M]
mov esi,eax
mov eax,[r1]
push eax
Call TMath_FloatToStr
POP ESI ; Restore LS
mov dword ptr [s], eax
Pop ESI ; Exit Call_Assignment
;writeln(s);
Push esi
mov eax, dword ptr [s]
mov esi,eax
Call $_StringWrite
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
$_S1 db 5,5,'12345'
r1 dd 0
r2 dd 0
r3 dd 0
M dd 0,0,0,0
s db 0,0, 80 DUP(32)
ends
end start
