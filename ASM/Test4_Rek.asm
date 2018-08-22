;E:\Programmierung\Delphi7\Projects\CubeOS\CubeOOP\Works\Projects\Test4_Rek.OOP AT 20.08.2018 19:24:25
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
;
;
;//**************************************************
;// KLASSE Test2
;//**************************************************
;class TTest2 of Base implements
;Public
;iValue1: Int32;
;iValue2: Int32;
;iValue3: Int32;
;
;procedure LocVarTest1;
TTest2_LocVarTest1 proc near
push ebp
mov ebp,esp
push ebp
;var a: Int32;
;b: Int32;
;c: Int32;
;d: Int32;
;e: Int32;
;f: Int32;
;begin
SUB ESP,24
;a:= 100;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,100
POP ESI ; Restore LS
mov dword ptr [ebp - 24], eax
Pop ESI ; Exit Call_Assignment
;b:= 200;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,200
POP ESI ; Restore LS
mov dword ptr [ebp - 20], eax
Pop ESI ; Exit Call_Assignment
;c:= 300;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,300
POP ESI ; Restore LS
mov dword ptr [ebp - 16], eax
Pop ESI ; Exit Call_Assignment
;iValue1 := a + b + c ;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,[ebp - 24]
push eax
mov eax,[ebp - 20]
pop EDX
add edx,eax
mov eax,edx
push eax
mov eax,[ebp - 16]
pop EDX
add edx,eax
mov eax,edx
POP ESI ; Restore LS
mov dword ptr [esi + 8], eax
Pop ESI ; Exit Call_Assignment
;end;
mov esp,ebp
pop ebp
RET 0
TTest2_LocVarTest1 ENDP
;
;procedure LocVarTest2(
TTest2_LocVarTest2 proc near
push ebp
mov ebp,esp
push ebp
;a: Int32;
;b: Int32;
;c: Int32 );
;begin
;iValue2 := a + b + c ;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,[ebp + 16]
push eax
mov eax,[ebp + 12]
pop EDX
add edx,eax
mov eax,edx
push eax
mov eax,[ebp + 8]
pop EDX
add edx,eax
mov eax,edx
POP ESI ; Restore LS
mov dword ptr [esi + 4], eax
Pop ESI ; Exit Call_Assignment
;end;
mov esp,ebp
pop ebp
RET 12
TTest2_LocVarTest2 ENDP
;
;procedure LocVarTest3(
TTest2_LocVarTest3 proc near
push ebp
mov ebp,esp
push ebp
;a: Int32;
;b: Int32;
;c: Int32 );
;var d: Int32;
;e: Int32;
;f: Int32;
;begin
SUB ESP,12
;d:= 1000000;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,1000000
POP ESI ; Restore LS
mov dword ptr [ebp - 12], eax
Pop ESI ; Exit Call_Assignment
;e:= 1000000;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,1000000
POP ESI ; Restore LS
mov dword ptr [ebp - 8], eax
Pop ESI ; Exit Call_Assignment
;f:= 1000000;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,1000000
POP ESI ; Restore LS
mov dword ptr [ebp - 4], eax
Pop ESI ; Exit Call_Assignment
;
;iValue3 := a + b + c + d+ e+ f;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,[ebp + 16]
push eax
mov eax,[ebp + 12]
pop EDX
add edx,eax
mov eax,edx
push eax
mov eax,[ebp + 8]
pop EDX
add edx,eax
mov eax,edx
push eax
mov eax,[ebp - 12]
pop EDX
add edx,eax
mov eax,edx
push eax
mov eax,[ebp - 8]
pop EDX
add edx,eax
mov eax,edx
push eax
mov eax,[ebp - 4]
pop EDX
add edx,eax
mov eax,edx
POP ESI ; Restore LS
mov dword ptr [esi + 0], eax
Pop ESI ; Exit Call_Assignment
;end;
mov esp,ebp
pop ebp
RET 12
TTest2_LocVarTest3 ENDP
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
;Var
;Test2:TTest2;
;
;Begin
;Test2:=Create(TTest2);
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov EAX,12
CALL $_Allocate
POP ESI ; Restore LS
mov dword ptr [Test2], eax
Pop ESI ; Exit Call_Assignment
;Test2.LocVarTEst1;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
mov eax,[Test2]
mov esi,eax
Call TTest2_LocVarTest1
POP ESI ; RS not used
Pop ESI ; Exit Call_Assignment
;writeln(Test2.iValue1);
Push esi
mov eax,[Test2]
mov esi,eax
mov eax,[esi + 8]
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
;Test2.LocVarTEst2(200000,300000,400000);
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
mov eax,[Test2]
mov esi,eax
mov eax,200000
push eax
mov eax,300000
push eax
mov eax,400000
push eax
Call TTest2_LocVarTest2
POP ESI ; RS not used
Pop ESI ; Exit Call_Assignment
;writeln(Test2.iValue2);
Push esi
mov eax,[Test2]
mov esi,eax
mov eax,[esi + 4]
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
;Test2.LocVarTEst3(200000,300000,400000);
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
mov eax,[Test2]
mov esi,eax
mov eax,200000
push eax
mov eax,300000
push eax
mov eax,400000
push eax
Call TTest2_LocVarTest3
POP ESI ; RS not used
Pop ESI ; Exit Call_Assignment
;writeln(Test2.iValue3);
Push esi
mov eax,[Test2]
mov esi,eax
mov eax,[esi + 0]
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
Test2 dd 0,0,0,0
ends
end start
