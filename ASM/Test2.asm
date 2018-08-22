;C:\Entwicklung\Delphi\Projects\Privat\CubeOS\CubeOOP\Works\Projects\Test2.OOP AT 14.11.2012 10:28:35
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
;//**************************************************
;// KALASSE Test1
;//**************************************************
;
;class TTest1 of Base implements
;Public
;iValue1: Int32;
;iValue2: Int32;
;iValue3: Int32;
;iValue4: Int32;
;iValue5: Int32;
;iValue6: Int32;
;
;
;//**************************************************
;//        Rekursions Algorithmus
;//**************************************************
;
;Function PowerRek(
TTest1_PowerRek proc near
push ebp
mov ebp,esp
push ebp
;iPValue2: Int32;
;iMultiplikator: Int32;
;iCount:int32):Int32;
;begin
SUB ESP,4
;if iCount <  iMultiplikator then
mov eax,[ebp + 8]
PUSH eax
mov eax,[ebp + 12]
pop dx
cmp dx,ax
mov ax,1
jl CM3
xor ax,ax
CM3:
cmp ax,1
je I4
jmp I2
I4:
;begin
;Result := PowerRek(
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax, [esi + 0]
;iPValue2 * 2,
mov eax,[ebp + 16]
push eax
mov eax,2
pop edx
mul edx
push eax
;iMultiplikator,
mov eax,[ebp + 12]
push eax
;iCount + 1);
mov eax,[ebp + 8]
push eax
mov eax,1
pop EDX
add edx,eax
mov eax,edx
push eax
call TTest1_PowerRek
POP ESI ; Restore LS
mov dword ptr [ebp - 4], eax
Pop ESI ; Exit Call_Assignment
;end
;else
jmp I8
I2 label near
;Result := iPValue2;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,[ebp + 16]
POP ESI ; Restore LS
mov dword ptr [ebp - 4], eax
Pop ESI ; Exit Call_Assignment
I8 label near
;end;
mov EAX, DWord ptr [ebp - 4]
mov esp,ebp
pop ebp
RET 12
TTest1_PowerRek ENDP
;//**************************************************
;//
;//**************************************************
;Procedure SetVal4(iPValue2: Int32);
TTest1_SetVal4 proc near
push ebp
mov ebp,esp
push ebp
;begin
;iValue4 := iPValue2;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,[ebp + 8]
POP ESI ; Restore LS
mov dword ptr [esi + 8], eax
Pop ESI ; Exit Call_Assignment
;end;
mov esp,ebp
pop ebp
RET 4
TTest1_SetVal4 ENDP
;//**************************************************
;//         Quadrat - Methoden
;//**************************************************
;Function Quadrat(iQ: Int32): Int32;
TTest1_Quadrat proc near
push ebp
mov ebp,esp
push ebp
;begin
SUB ESP,4
;Result := PowerRek(iQ,2,1) + PowerRek(iQ,2,1)
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax, [esi + 0]
mov eax,[ebp + 8]
push eax
mov eax,2
push eax
mov eax,1
push eax
call TTest1_PowerRek
push eax
mov eax, [esi + 0]
mov eax,[ebp + 8]
push eax
mov eax,2
push eax
mov eax,1
push eax
call TTest1_PowerRek
;end;
pop EDX
add edx,eax
mov eax,edx
POP ESI ; Restore LS
mov dword ptr [ebp - 4], eax
Pop ESI ; Exit Call_Assignment
mov EAX, DWord ptr [ebp - 4]
mov esp,ebp
pop ebp
RET 4
TTest1_Quadrat ENDP
;end;
;//**************************************************
;// KALASSE Test2
;//**************************************************
;class TTest2 of Base implements
;Public
;iValue1: Int32;
;iValue4: Int32;
;iValue5: Int32;
;Obj1:TTest1;
;
;
;Procedure ForTest;
TTest2_ForTest proc near
push ebp
mov ebp,esp
push ebp
;var index: int32;
;begin
SUB ESP,4
;iValue5:=1;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,1
POP ESI ; Restore LS
mov dword ptr [esi + 4], eax
Pop ESI ; Exit Call_Assignment
;for Index := iValue5 to 10 do
XOR EAX,EAX
mov eax,[esi + 4]
Mov [ebp - 4],EAX
mov eax,10
Mov EDX,EAX
;begin
W13 label near
Push EDX
;iValue5 := iValue5 + Index;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,[esi + 4]
push eax
mov eax,[ebp - 4]
pop EDX
add edx,eax
mov eax,edx
POP ESI ; Restore LS
mov dword ptr [esi + 4], eax
Pop ESI ; Exit Call_Assignment
;writeln(Index);
Push esi
mov eax,[ebp - 4]
SUB ESP,20
mov ESI,ESP
push EAX               ; Store Result
CLD                    ; clear direction flag => increment EDI
MOV ECX, 20            ; Count init ECX for REP
MOV EAX, 32            ; Char init AL for STOSB
MOV EDI, ESI           ; dest init EDI for STOSB
REP STOSB              ; Repeat
pop EAX                ; Load Result
mov Byte ptr[ESI],1    ; Speicher leeren
mov Byte ptr[ESI+1],16 ; Speicher leeren
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
Mov EAX, [ebp - 4]
INC EAX
Mov [ebp - 4],EAX
cmp EDX,EAX
jge W13
;end;
mov esp,ebp
pop ebp
RET 0
TTest2_ForTest ENDP
;
;Procedure INI;
TTest2_INI proc near
push ebp
mov ebp,esp
push ebp
;begin
;Obj1 := Create(TTest1);
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov EAX,24
CALL $_Allocate
POP ESI ; Restore LS
mov dword ptr [esi + 0], eax
Pop ESI ; Exit Call_Assignment
;Obj1.iValue3 := 7777;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
mov eax,[esi + 0]
mov esi,eax
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,7777
POP ESI ; Restore LS
mov dword ptr [esi + 12], eax
Pop ESI ; Exit Call_Assignment
;iValue4 := 10;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,10
POP ESI ; Restore LS
mov dword ptr [esi + 8], eax
Pop ESI ; Exit Call_Assignment
;end;
mov esp,ebp
pop ebp
RET 0
TTest2_INI ENDP
;
;Procedure SetVal4(iPValue2: Int32);
TTest2_SetVal4 proc near
push ebp
mov ebp,esp
push ebp
;begin
;
;end;
mov esp,ebp
pop ebp
RET 4
TTest2_SetVal4 ENDP
;
;procedure LocVarTest;
TTest2_LocVarTest proc near
push ebp
mov ebp,esp
push ebp
;var a:int32;
;b:int32;
;c:int32;
;begin
SUB ESP,12
;a := 10000;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,10000
POP ESI ; Restore LS
mov dword ptr [ebp - 12], eax
Pop ESI ; Exit Call_Assignment
;b := 20000;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,20000
POP ESI ; Restore LS
mov dword ptr [ebp - 8], eax
Pop ESI ; Exit Call_Assignment
;c := 30000;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,30000
POP ESI ; Restore LS
mov dword ptr [ebp - 4], eax
Pop ESI ; Exit Call_Assignment
;a:= b + c;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,[ebp - 8]
push eax
mov eax,[ebp - 4]
pop EDX
add edx,eax
mov eax,edx
POP ESI ; Restore LS
mov dword ptr [ebp - 12], eax
Pop ESI ; Exit Call_Assignment
;iValue5 := a;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,[ebp - 12]
POP ESI ; Restore LS
mov dword ptr [esi + 4], eax
Pop ESI ; Exit Call_Assignment
;end;
mov esp,ebp
pop ebp
RET 0
TTest2_LocVarTest ENDP
;
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
;Test2:TTest2;
;Wert:Int32;
;Begin
;//Writeln('TEst'); // <== offen: konstante Strings
;Test1:=Create(TTest1);
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov EAX,24
CALL $_Allocate
POP ESI ; Restore LS
mov dword ptr [Test1], eax
Pop ESI ; Exit Call_Assignment
;
;Writeln(Test1.Quadrat(10));
Push esi
mov eax,[Test1]
mov esi,eax
mov eax,10
push eax
Call TTest1_Quadrat
SUB ESP,20
mov ESI,ESP
push EAX               ; Store Result
CLD                    ; clear direction flag => increment EDI
MOV ECX, 20            ; Count init ECX for REP
MOV EAX, 32            ; Char init AL for STOSB
MOV EDI, ESI           ; dest init EDI for STOSB
REP STOSB              ; Repeat
pop EAX                ; Load Result
mov Byte ptr[ESI],1    ; Speicher leeren
mov Byte ptr[ESI+1],16 ; Speicher leeren
ADD ESI,18			    ; Zeiger ans Ende (Zeichen rechts ausrichten)
XOR ECX, ECX		    ; Basis 10
XOR EDX, EDX		    ; 0
Call $_Int32ToStr	  ; Konvertierung
mov ESI, ESP
Call $_StringWrite
ADD ESP,20
Call $_NLN
Pop esi
;Writeln(GetAllocMem);
Push esi
Mov EAX, Allocated
SUB ESP,20
mov ESI,ESP
push EAX               ; Store Result
CLD                    ; clear direction flag => increment EDI
MOV ECX, 20            ; Count init ECX for REP
MOV EAX, 32            ; Char init AL for STOSB
MOV EDI, ESI           ; dest init EDI for STOSB
REP STOSB              ; Repeat
pop EAX                ; Load Result
mov Byte ptr[ESI],1    ; Speicher leeren
mov Byte ptr[ESI+1],16 ; Speicher leeren
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
;Test1.iValue3 := 24;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
mov eax,[Test1]
mov esi,eax
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,24
POP ESI ; Restore LS
mov dword ptr [esi + 12], eax
Pop ESI ; Exit Call_Assignment
;Test2:=Create(TTest2);
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
mov dword ptr [Test2], eax
Pop ESI ; Exit Call_Assignment
;Wert := 99;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,99
POP ESI ; Restore LS
mov dword ptr [Wert], eax
Pop ESI ; Exit Call_Assignment
;Test2.SetVal4(Wert);  // <== offen: Varparameter
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
mov eax,[Test2]
mov esi,eax
mov eax,[Wert]
push eax
Call TTest2_SetVal4
POP ESI ; RS not used
Pop ESI ; Exit Call_Assignment
;Writeln(Wert);
Push esi
mov eax,[Wert]
SUB ESP,20
mov ESI,ESP
push EAX               ; Store Result
CLD                    ; clear direction flag => increment EDI
MOV ECX, 20            ; Count init ECX for REP
MOV EAX, 32            ; Char init AL for STOSB
MOV EDI, ESI           ; dest init EDI for STOSB
REP STOSB              ; Repeat
pop EAX                ; Load Result
mov Byte ptr[ESI],1    ; Speicher leeren
mov Byte ptr[ESI+1],16 ; Speicher leeren
ADD ESI,18			    ; Zeiger ans Ende (Zeichen rechts ausrichten)
XOR ECX, ECX		    ; Basis 10
XOR EDX, EDX		    ; 0
Call $_Int32ToStr	  ; Konvertierung
mov ESI, ESP
Call $_StringWrite
ADD ESP,20
Call $_NLN
Pop esi
;Test2.INI;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
mov eax,[Test2]
mov esi,eax
Call TTest2_INI
POP ESI ; RS not used
Pop ESI ; Exit Call_Assignment
;
;Writeln(GetAllocMem);
Push esi
Mov EAX, Allocated
SUB ESP,20
mov ESI,ESP
push EAX               ; Store Result
CLD                    ; clear direction flag => increment EDI
MOV ECX, 20            ; Count init ECX for REP
MOV EAX, 32            ; Char init AL for STOSB
MOV EDI, ESI           ; dest init EDI for STOSB
REP STOSB              ; Repeat
pop EAX                ; Load Result
mov Byte ptr[ESI],1    ; Speicher leeren
mov Byte ptr[ESI+1],16 ; Speicher leeren
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
;Test2.Obj1.iValue2 := 2222;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
mov eax,[Test2]
mov esi,eax
mov eax,[esi + 0]
mov esi,eax
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,2222
POP ESI ; Restore LS
mov dword ptr [esi + 16], eax
Pop ESI ; Exit Call_Assignment
;Writeln(Test2.Obj1.iValue2);
Push esi
mov eax,[Test2]
mov esi,eax
mov eax,[esi + 0]
mov esi,eax
mov eax,[esi + 16]
SUB ESP,20
mov ESI,ESP
push EAX               ; Store Result
CLD                    ; clear direction flag => increment EDI
MOV ECX, 20            ; Count init ECX for REP
MOV EAX, 32            ; Char init AL for STOSB
MOV EDI, ESI           ; dest init EDI for STOSB
REP STOSB              ; Repeat
pop EAX                ; Load Result
mov Byte ptr[ESI],1    ; Speicher leeren
mov Byte ptr[ESI+1],16 ; Speicher leeren
ADD ESI,18			    ; Zeiger ans Ende (Zeichen rechts ausrichten)
XOR ECX, ECX		    ; Basis 10
XOR EDX, EDX		    ; 0
Call $_Int32ToStr	  ; Konvertierung
mov ESI, ESP
Call $_StringWrite
ADD ESP,20
Call $_NLN
Pop esi
;Writeln(Test2.Obj1.iValue3);
Push esi
mov eax,[Test2]
mov esi,eax
mov eax,[esi + 0]
mov esi,eax
mov eax,[esi + 12]
SUB ESP,20
mov ESI,ESP
push EAX               ; Store Result
CLD                    ; clear direction flag => increment EDI
MOV ECX, 20            ; Count init ECX for REP
MOV EAX, 32            ; Char init AL for STOSB
MOV EDI, ESI           ; dest init EDI for STOSB
REP STOSB              ; Repeat
pop EAX                ; Load Result
mov Byte ptr[ESI],1    ; Speicher leeren
mov Byte ptr[ESI+1],16 ; Speicher leeren
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
;Writeln(Test1.PowerRek(2,16,1));
Push esi
mov eax,[Test1]
mov esi,eax
mov eax,2
push eax
mov eax,16
push eax
mov eax,1
push eax
Call TTest1_PowerRek
SUB ESP,20
mov ESI,ESP
push EAX               ; Store Result
CLD                    ; clear direction flag => increment EDI
MOV ECX, 20            ; Count init ECX for REP
MOV EAX, 32            ; Char init AL for STOSB
MOV EDI, ESI           ; dest init EDI for STOSB
REP STOSB              ; Repeat
pop EAX                ; Load Result
mov Byte ptr[ESI],1    ; Speicher leeren
mov Byte ptr[ESI+1],16 ; Speicher leeren
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
;Writeln(Test1.PowerRek(2,8,1));
Push esi
mov eax,[Test1]
mov esi,eax
mov eax,2
push eax
mov eax,8
push eax
mov eax,1
push eax
Call TTest1_PowerRek
SUB ESP,20
mov ESI,ESP
push EAX               ; Store Result
CLD                    ; clear direction flag => increment EDI
MOV ECX, 20            ; Count init ECX for REP
MOV EAX, 32            ; Char init AL for STOSB
MOV EDI, ESI           ; dest init EDI for STOSB
REP STOSB              ; Repeat
pop EAX                ; Load Result
mov Byte ptr[ESI],1    ; Speicher leeren
mov Byte ptr[ESI+1],16 ; Speicher leeren
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
;Writeln(Test1.PowerRek(2,4,1));
Push esi
mov eax,[Test1]
mov esi,eax
mov eax,2
push eax
mov eax,4
push eax
mov eax,1
push eax
Call TTest1_PowerRek
SUB ESP,20
mov ESI,ESP
push EAX               ; Store Result
CLD                    ; clear direction flag => increment EDI
MOV ECX, 20            ; Count init ECX for REP
MOV EAX, 32            ; Char init AL for STOSB
MOV EDI, ESI           ; dest init EDI for STOSB
REP STOSB              ; Repeat
pop EAX                ; Load Result
mov Byte ptr[ESI],1    ; Speicher leeren
mov Byte ptr[ESI+1],16 ; Speicher leeren
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
;Writeln(Test1.Quadrat(10));
Push esi
mov eax,[Test1]
mov esi,eax
mov eax,10
push eax
Call TTest1_Quadrat
SUB ESP,20
mov ESI,ESP
push EAX               ; Store Result
CLD                    ; clear direction flag => increment EDI
MOV ECX, 20            ; Count init ECX for REP
MOV EAX, 32            ; Char init AL for STOSB
MOV EDI, ESI           ; dest init EDI for STOSB
REP STOSB              ; Repeat
pop EAX                ; Load Result
mov Byte ptr[ESI],1    ; Speicher leeren
mov Byte ptr[ESI+1],16 ; Speicher leeren
ADD ESI,18			    ; Zeiger ans Ende (Zeichen rechts ausrichten)
XOR ECX, ECX		    ; Basis 10
XOR EDX, EDX		    ; 0
Call $_Int32ToStr	  ; Konvertierung
mov ESI, ESP
Call $_StringWrite
ADD ESP,20
Call $_NLN
Pop esi
;Test2.iValue4 := 12345;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
mov eax,[Test2]
mov esi,eax
POP EBX ; LOAD Basepointer ESI
Push ESI ; Store LS
mov ESI ,EBX; Store LS
XOR EAX,EAX
XOR EDX,EDX
mov eax,12345
POP ESI ; Restore LS
mov dword ptr [esi + 8], eax
Pop ESI ; Exit Call_Assignment
;Writeln(Test1.iValue2);
Push esi
mov eax,[Test1]
mov esi,eax
mov eax,[esi + 16]
SUB ESP,20
mov ESI,ESP
push EAX               ; Store Result
CLD                    ; clear direction flag => increment EDI
MOV ECX, 20            ; Count init ECX for REP
MOV EAX, 32            ; Char init AL for STOSB
MOV EDI, ESI           ; dest init EDI for STOSB
REP STOSB              ; Repeat
pop EAX                ; Load Result
mov Byte ptr[ESI],1    ; Speicher leeren
mov Byte ptr[ESI+1],16 ; Speicher leeren
ADD ESI,18			    ; Zeiger ans Ende (Zeichen rechts ausrichten)
XOR ECX, ECX		    ; Basis 10
XOR EDX, EDX		    ; 0
Call $_Int32ToStr	  ; Konvertierung
mov ESI, ESP
Call $_StringWrite
ADD ESP,20
Call $_NLN
Pop esi
;Writeln(GetAllocMem);
Push esi
Mov EAX, Allocated
SUB ESP,20
mov ESI,ESP
push EAX               ; Store Result
CLD                    ; clear direction flag => increment EDI
MOV ECX, 20            ; Count init ECX for REP
MOV EAX, 32            ; Char init AL for STOSB
MOV EDI, ESI           ; dest init EDI for STOSB
REP STOSB              ; Repeat
pop EAX                ; Load Result
mov Byte ptr[ESI],1    ; Speicher leeren
mov Byte ptr[ESI+1],16 ; Speicher leeren
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
;Test2.ForTest;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
mov eax,[Test2]
mov esi,eax
Call TTest2_ForTest
POP ESI ; RS not used
Pop ESI ; Exit Call_Assignment
;writeln(Test2.iValue5);
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
REP STOSB              ; Repeat
pop EAX                ; Load Result
mov Byte ptr[ESI],1    ; Speicher leeren
mov Byte ptr[ESI+1],16 ; Speicher leeren
ADD ESI,18			    ; Zeiger ans Ende (Zeichen rechts ausrichten)
XOR ECX, ECX		    ; Basis 10
XOR EDX, EDX		    ; 0
Call $_Int32ToStr	  ; Konvertierung
mov ESI, ESP
Call $_StringWrite
ADD ESP,20
Call $_NLN
Pop esi
;Test2.LocVarTEst;
Push ESI ; Entry Call_Assignment 1
Push ESI ; Entry Call_Assignment 2
mov eax,[Test2]
mov esi,eax
Call TTest2_LocVarTest
POP ESI ; RS not used
Pop ESI ; Exit Call_Assignment
;writeln(Test2.iValue5);
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
REP STOSB              ; Repeat
pop EAX                ; Load Result
mov Byte ptr[ESI],1    ; Speicher leeren
mov Byte ptr[ESI+1],16 ; Speicher leeren
ADD ESI,18			    ; Zeiger ans Ende (Zeichen rechts ausrichten)
XOR ECX, ECX		    ; Basis 10
XOR EDX, EDX		    ; 0
Call $_Int32ToStr	  ; Konvertierung
mov ESI, ESP
Call $_StringWrite
ADD ESP,20
Call $_NLN
Pop esi
;writeln(Test2.iValue5);
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
REP STOSB              ; Repeat
pop EAX                ; Load Result
mov Byte ptr[ESI],1    ; Speicher leeren
mov Byte ptr[ESI+1],16 ; Speicher leeren
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
Test2 dd 0,0,0,0
Wert dd 0
ends
end start
