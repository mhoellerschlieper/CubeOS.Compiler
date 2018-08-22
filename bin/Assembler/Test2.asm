;C:\Programme\Borland\Delphi7\Projects\CubeOS\CubeOOP\Works\Projects\Test2.OOP AT 12.08.2007 22:10:39
code segment byte public
Assume cs:code, ds:data
init:
mov ax,data
mov ds,ax
call detectscr
call initscr
mov ah,62h
int 21h
mov es,bx
mov di,80h
mov si,offset parstr
call partostr
JMP _Main
;============== Prolog Ended
;//Environment: CubeOOP
;//Title:
;//Autor:
;//Date: 29.07.2007 09:47:02
;//Description:
;//*************************************************************
;Main;
_Main:
;Var i: Integer;
;a: Integer;
;Begin
;a:= 3;
  Push ESI
  mov eax,3
  mov [a], EAX
  Pop ESI
;i := 7;
  Push ESI
  mov eax,7
  mov [i], EAX
  Pop ESI
;Writeln(a);
push dx
push bx
cmp ax,0
jge LW2
push ax
mov si,offset minus
call print
pop ax
mov dx,-1
mul dx
LW2: mov si,offset str
mov byte ptr [si+1],0
mov bl,10
mov dx,0
call numtostr
call print
pop bx
pop dx
;Writeln(i);
push dx
push bx
cmp ax,0
jge LW3
push ax
mov si,offset minus
call print
pop ax
mov dx,-1
mul dx
LW3: mov si,offset str
mov byte ptr [si+1],0
mov bl,10
mov dx,0
call numtostr
call print
pop bx
pop dx
;end;
;
;============== Epilog Start
ENDE:
mov ah,4ch
mov al,0
int 21h
;======== System Lib
include syscode.inc
code ends
;======== DATA SEGMENT
data segment word public
minus db 1,1,"-" 
parstr db 127,0,127 dup(?)
str db 255,0,255 dup(32)
concat db 255,0,255 dup(0)
cpystr db 255,0,255 dup(0)
nts db 7,0,7 dup(0)
nln db 2,2,0dh,0ah
dummystr db 1,1,0
chrchr db 1,1,32
minus db 1,1,"-" 
;======== global Vars
i db 2
a db 2
data ENDs
;======== STACK SEGMENT
Stack segment stack
  db 3000h dup(0)
stack ends
end init
