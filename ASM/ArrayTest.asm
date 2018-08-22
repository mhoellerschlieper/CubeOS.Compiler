;E:\Programmierung\Delphi7\Projects\CubeOS\CubeOOP\Works\Projects\ArrayTest.OOP AT 20.08.2018 19:31:17
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
;var i:Int32;
;Feld: Array[-10..10] of Int32;
;
;Begin
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
mov dword ptr [i], eax
Pop ESI ; Exit Call_Assignment
;
;Feld[-10]:= 101;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
mov eax,10
mov EDX,-1
mul EDX
SUB EAX, -10
MOV EDX, 4
MUL EDX
mov ECX,EAX
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,101
POP ESI ; Restore LS
mov dword ptr [Feld + ECX], eax
Pop ESI ; Exit Call_Assignment
;Feld[2]:= 102;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
mov eax,2
SUB EAX, -10
MOV EDX, 4
MUL EDX
mov ECX,EAX
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,102
POP ESI ; Restore LS
mov dword ptr [Feld + ECX], eax
Pop ESI ; Exit Call_Assignment
;Feld[3]:= 103;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
mov eax,3
SUB EAX, -10
MOV EDX, 4
MUL EDX
mov ECX,EAX
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,103
POP ESI ; Restore LS
mov dword ptr [Feld + ECX], eax
Pop ESI ; Exit Call_Assignment
;
;
;Writeln(i);
Push esi
mov eax,[i]
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
;i:= Feld[-10];
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,10
mov EDX,-1
mul EDX
SUB EAX, -10
MOV EDX, 4
MUL EDX
mov ECX,EAX
mov eax,dword ptr [Feld + ECX]
POP ESI ; Restore LS
mov dword ptr [i], eax
Pop ESI ; Exit Call_Assignment
;Writeln(i);
Push esi
mov eax,[i]
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
;i:= Feld[2];
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,2
SUB EAX, -10
MOV EDX, 4
MUL EDX
mov ECX,EAX
mov eax,dword ptr [Feld + ECX]
POP ESI ; Restore LS
mov dword ptr [i], eax
Pop ESI ; Exit Call_Assignment
;Writeln(i);
Push esi
mov eax,[i]
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
;i:= Feld[3];
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,3
SUB EAX, -10
MOV EDX, 4
MUL EDX
mov ECX,EAX
mov eax,dword ptr [Feld + ECX]
POP ESI ; Restore LS
mov dword ptr [i], eax
Pop ESI ; Exit Call_Assignment
;Writeln(i);
Push esi
mov eax,[i]
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
;for i:= -10 to 10 do
XOR EAX,EAX
mov eax,10
mov EDX,-1
mul EDX
Mov [i],EAX
mov eax,10
Mov EDX,EAX
;begin
W8 label near
Push EDX
;if Feld[i]=0 then
mov eax,[i]
SUB EAX, -10
MOV EDX, 4
MUL EDX
mov ECX,EAX
mov eax,dword ptr [Feld + ECX]
PUSH eax
mov eax,0
pop edx
cmp edx,eax
mov ax,1
je CM10
xor ax,ax
CM10:
cmp ax,1
je I11
jmp I9
I11:
;Feld[i]:= i*2;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
mov eax,[i]
SUB EAX, -10
MOV EDX, 4
MUL EDX
mov ECX,EAX
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,[i]
push eax
mov eax,2
pop edx
mul edx
POP ESI ; Restore LS
mov dword ptr [Feld + ECX], eax
Pop ESI ; Exit Call_Assignment
jmp I15
I9 label near
I15 label near
;writeln(Feld[i]);
Push esi
mov eax,[i]
SUB EAX, -10
MOV EDX, 4
MUL EDX
mov ECX,EAX
mov eax,dword ptr [Feld + ECX]
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
Pop EDX
Mov EAX, [i]
INC EAX
Mov [i],EAX
cmp EDX,EAX
jge W8
;
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
i dd 0
Feld db 84 DUP(0)
ends
end start
