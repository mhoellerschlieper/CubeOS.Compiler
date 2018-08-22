;E:\Programmierung\Delphi7\Projects\CubeOS\CubeOOP\Works\Projects\ListTest.OOP AT 20.08.2018 21:56:40
.486
locals
jumps
.model flat,STDCALL
;============== Prolog Ended
Include .\include\Prolog.asm
;//Environment: CubeOOP
;//Title:
;//Autor:
;//Date: 22.06.2009 13:18:18
;//Description:
;//*************************************************************
;
;class TItem of Base implements
;Private
;
;Public
;pNext: TItem;
;pPrev: TItem;
;iValue: Int32;
;end;
;//=========================================================================
;//
;//=========================================================================
;
;class TIntList of Base implements
;Public
;LastItem: TItem;
;ActItem: TItem;
;FirstItem: TItem;
;//=============================================================
;//
;//=============================================================
;Procedure Add(iValue: Int32);
TIntList_Add proc near
push ebp
mov ebp,esp
push ebp
;begin
;ActItem:= Create(TItem);
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
mov dword ptr [esi + 4], eax
Pop ESI ; Exit Call_Assignment
;ActItem.iValue := iValue;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
mov eax,[esi + 4]
mov esi,eax
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,[ebp + 8]
POP ESI ; Restore LS
mov dword ptr [esi + 0], eax
Pop ESI ; Exit Call_Assignment
;ActItem.pNext  := NIL;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
mov eax,[esi + 4]
mov esi,eax
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
Mov EAX, 0
POP ESI ; Restore LS
mov dword ptr [esi + 8], eax
Pop ESI ; Exit Call_Assignment
;
;if FirstItem = NIL then
mov eax,[esi + 0]
PUSH eax
Mov EAX, 0
pop edx
cmp edx,eax
mov ax,1
je CM3
xor ax,ax
CM3:
cmp ax,1
je I4
jmp I2
I4:
;begin
;FirstItem     := ActItem;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,[esi + 4]
POP ESI ; Restore LS
mov dword ptr [esi + 0], eax
Pop ESI ; Exit Call_Assignment
;ActItem.pPrev := NIL;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
mov eax,[esi + 4]
mov esi,eax
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
Mov EAX, 0
POP ESI ; Restore LS
mov dword ptr [esi + 4], eax
Pop ESI ; Exit Call_Assignment
;end
;else
jmp I8
I2 label near
;begin
;LastItem.pNext := ActItem;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
mov eax,[esi + 8]
mov esi,eax
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,[esi + 4]
POP ESI ; Restore LS
mov dword ptr [esi + 8], eax
Pop ESI ; Exit Call_Assignment
;ActItem.pPrev  := LastItem;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
mov eax,[esi + 4]
mov esi,eax
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,[esi + 8]
POP ESI ; Restore LS
mov dword ptr [esi + 4], eax
Pop ESI ; Exit Call_Assignment
;end;
I8 label near
;
;LastItem := ActItem;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,[esi + 4]
POP ESI ; Restore LS
mov dword ptr [esi + 8], eax
Pop ESI ; Exit Call_Assignment
;end;
mov esp,ebp
pop ebp
RET 4
TIntList_Add ENDP
;//=============================================================
;//
;//=============================================================
;Function GetValue(L_iIndex: Int32): TItem;
TIntList_GetValue proc near
push ebp
mov ebp,esp
push ebp
;var Lidx: int32;
;begin
SUB ESP,8
;ActItem := FirstItem;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,[esi + 0]
POP ESI ; Restore LS
mov dword ptr [esi + 4], eax
Pop ESI ; Exit Call_Assignment
;Result  := nil;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
Mov EAX, 0
POP ESI ; Restore LS
mov dword ptr [ebp - 8], eax
Pop ESI ; Exit Call_Assignment
;
;Lidx:=0;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,0
POP ESI ; Restore LS
mov dword ptr [ebp - 4], eax
Pop ESI ; Exit Call_Assignment
;While Lidx < L_iIndex AND ActItem <> nil do
W13 label near
mov eax,[ebp - 4]
PUSH eax
mov eax,[ebp + 8]
pop dx
cmp dx,ax
mov ax,1
jl CM15
xor ax,ax
CM15:
push ax
mov eax,[esi + 4]
PUSH eax
Mov EAX, 0
pop Edx
cmp Edx,Eax
mov ax,1
jne CM16
xor ax,ax
CM16:
pop dx
and ax,dx
mov ax,1
jnz CM17
xor ax,ax
CM17:
cmp ax,1
je W18
jmp W14
W18:
;begin
;if ActItem <> nil then
mov eax,[esi + 4]
PUSH eax
Mov EAX, 0
pop Edx
cmp Edx,Eax
mov ax,1
jne CM20
xor ax,ax
CM20:
cmp ax,1
je I21
jmp I19
I21:
;begin
;ActItem := ActItem.pNext;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,[esi + 4]
mov esi,eax
mov eax,[esi + 8]
POP ESI ; Restore LS
mov dword ptr [esi + 4], eax
Pop ESI ; Exit Call_Assignment
;end;
jmp I25
I19 label near
I25 label near
;Lidx := Lidx + 1;
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
jmp W13
W14 label near
;
;Result := ActItem;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,[esi + 4]
POP ESI ; Restore LS
mov dword ptr [ebp - 8], eax
Pop ESI ; Exit Call_Assignment
;end;
mov EAX, DWord ptr [ebp - 8]
mov esp,ebp
pop ebp
RET 4
TIntList_GetValue ENDP
;//=============================================================
;//
;//=============================================================
;Procedure INI;
TIntList_INI proc near
push ebp
mov ebp,esp
push ebp
;begin
;FirstItem:= NIL;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
Mov EAX, 0
POP ESI ; Restore LS
mov dword ptr [esi + 0], eax
Pop ESI ; Exit Call_Assignment
;LastItem := NIL;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
Mov EAX, 0
POP ESI ; Restore LS
mov dword ptr [esi + 8], eax
Pop ESI ; Exit Call_Assignment
;ActItem  := NIL;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
Mov EAX, 0
POP ESI ; Restore LS
mov dword ptr [esi + 4], eax
Pop ESI ; Exit Call_Assignment
;end;
mov esp,ebp
pop ebp
RET 0
TIntList_INI ENDP
;end;
;
;//=========================================================================
;//                              MAIN
;//=========================================================================
;Main;
include .\include\win32.inc
_Main:
$_Main proc
push    offset lppaint
push    [hwnd]
call    BeginPaint
mov     [theDC], eax
;Var TestList: TIntList;
;iValue: Int32;
;iValue32: Int32;
;iValue16: Int16;
;Item: TItem;
;Begin
;iValue16:= 8*TIntList;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,8
push ax
mov EAX,12
pop dx
mul edx
POP ESI ; Restore LS
mov word ptr[iValue16], ax
Pop ESI ; Exit Call_Assignment
;
;writeln(iValue16);
Push esi
mov ax, [iValue16]
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
;TestList:= CREATE(3*TIntList*TItem);
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,3
push ax
mov EAX,12
pop dx
mul edx
push ax
mov EAX,12
pop dx
mul edx
CALL $_Allocate
POP ESI ; Restore LS
mov dword ptr [TestList], eax
Pop ESI ; Exit Call_Assignment
;Writeln(getallocmem);
Push esi
Mov EAX, Allocated
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
;TestList:= CREATE(8);
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,8
CALL $_Allocate
POP ESI ; Restore LS
mov dword ptr [TestList], eax
Pop ESI ; Exit Call_Assignment
;Writeln(getallocmem);
Push esi
Mov EAX, Allocated
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
;TestList:= CREATE(TIntList);
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
mov dword ptr [TestList], eax
Pop ESI ; Exit Call_Assignment
;Writeln(getallocmem);
Push esi
Mov EAX, Allocated
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
;TestList.Ini;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
mov eax,[TestList]
mov esi,eax
Call TIntList_INI
POP ESI ; RS not used
Pop ESI ; Exit Call_Assignment
;
;for iValue := 0 to 10 do
XOR EAX,EAX
mov eax,0
Mov [iValue],EAX
mov eax,10
Mov EDX,EAX
;begin
W34 label near
Push EDX
;TestList.ADD(iValue);
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
mov eax,[TestList]
mov esi,eax
mov eax,[iValue]
push eax
Call TIntList_Add
POP ESI ; RS not used
Pop ESI ; Exit Call_Assignment
;end;
Pop EDX
Mov EAX, [iValue]
INC EAX
Mov [iValue],EAX
cmp EDX,EAX
jge W34
;
;for iValue := 0 to 10 do
XOR EAX,EAX
mov eax,0
Mov [iValue],EAX
mov eax,10
Mov EDX,EAX
;begin
W37 label near
Push EDX
;Item := TestList.GetValue(iValue);
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,[TestList]
mov esi,eax
mov eax,[iValue]
push eax
Call TIntList_GetValue
POP ESI ; Restore LS
mov dword ptr [Item], eax
Pop ESI ; Exit Call_Assignment
;
;if Item <> nil then
mov eax,[Item]
PUSH eax
Mov EAX, 0
pop Edx
cmp Edx,Eax
mov ax,1
jne CM39
xor ax,ax
CM39:
cmp ax,1
je I40
jmp I38
I40:
;begin
;iValue32:=Item.iValue*200;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,[Item]
mov esi,eax
mov eax,[esi + 0]
push eax
mov eax,200
pop edx
mul edx
POP ESI ; Restore LS
mov dword ptr [iValue32], eax
Pop ESI ; Exit Call_Assignment
;Writeln(iValue32*2);
Push esi
mov eax,[iValue32]
push eax
mov eax,2
pop edx
mul edx
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
jmp I44
I38 label near
I44 label near
;end;
Pop EDX
Mov EAX, [iValue]
INC EAX
Mov [iValue],EAX
cmp EDX,EAX
jge W37
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
TestList dd 0,0,0,0
iValue dd 0
iValue32 dd 0
iValue16 dw 0
Item dd 0,0,0,0
ends
end start
