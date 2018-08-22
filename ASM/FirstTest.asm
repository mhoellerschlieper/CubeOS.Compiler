;F:\Entwicklung\Delphi\Delphi7\Projects\CubeOS\CubeOOP\Works\Projects\FirstTest.OOP AT 27.08.2008 15:44:28
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
;//*************************************************************}
;
;Class TList of Base implements
;Private
;pNext: Pointer; //
;pPrev: Pointer;
;pData: Pointer;
;iCount: Integer;
;
;Procedure SetCount(iValue: Integer);
TList_SetCount proc near
push ebp
mov ebp,esp
push ebp
;begin
;end;
;
;Public
mov esp,ebp
pop ebp
RET 4
TList_SetCount ENDP
;
;Count: Integer;
;
;Procedure TList;
TList_TList proc near
push ebp
mov ebp,esp
push ebp
;begin
;end;
;Procedure Add(pValue: Pointer);
mov esp,ebp
pop ebp
RET 0
TList_TList ENDP
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
MemBlock         dd 1024
EndPtr           dd 0
;======== global Vars
ends
end start
