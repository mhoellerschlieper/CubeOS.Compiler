public scrmode,scrseg,winypos,winxpos
public height, widthv,actx,acty,winattr
public curflag,wselect,windows,keyflag
public bufadr,insert,strbuf,bufpos
public anzkeys,posxy,sysstr,outflag
public delflag,basis,negflag,scrupflag
public detectscr,initscr,clrscr,clreol
public delline,insline,setcolor
public getcolor,defwindow,askwindow
public selwindow,putframe,delframe
public scrollup,scrolldn,scrolllf
public scrollrg,gotoxy,getxy,print
public convstring,readkey,flush,input
public deletestr,insertstr,copystr
public concatstr,posstr,upstr,ignchar
public cutchar,partostr,dadd,dsub
public dmul,ddiv,dneg,numtostr
public strtonum


Data segment word public



scrmode  db 00h
scrseg	 dw ?
winypos  db 0
winxpos  db 0
height	 db 25 
widthv	 db 80 
acty	 db 1
actx	 db 1
winattr  db 7
curflag  db 0
wselect  db 0
windows  db 40h dup (?)
keyflag  db ?
bufadr	 dw ?
insert	 db ?
strbuf	 db 260 dup (?)
bufpos	 db ?
anzkeys  dw ?
posxy	 dw ?
sysstr	 db 1,1,?
outflag  db ?
delflag  db ?
basis	 db 10
negflag  db ?
scrupflag db ?

data ends

code segment byte public
     assume cs:code, ds:data

detectscr proc near
	  push cx
	  push es
	  xor cx,cx
	  mov es,cx
	  mov cx,es:[0463h]
	  cmp cx,03d4h
	  pop es
	  pop cx
	  jz color
mono :	 mov byte ptr scrmode,7
	 mov scrseg,0B000h
	 xor al,al
	 ret
color:	 mov byte ptr scrmode,3
	 mov scrseg,0B800h
	 mov al,1
	 ret
detectscr endp

initscr proc near
	 mov al,byte ptr scrmode
	 or al,al
	 jnz modeok
	 ret
modeok: xor ah,ah 
	 int 10h
	 mov si,offset windows
	 mov di,offset windows+8
	 mov word ptr[si],0
	 mov word ptr[si+2],5019h
	 mov word ptr[si+4],0
	 mov word ptr[si+6],7
	 pushf
	 push es
	 cld
	 push ds
	 pop es
	 mov cx,1ch
	 rep movsw
	 pop es
	 popf
	 mov wselect,0
	 jmp selectwin
initscr endp

clrscr proc near
	 pushf
	 push es
	 mov dx,word ptr winypos
	 call berechnexy
	 mov ah,winattr
	 cld
	 mov cx, word ptr height
	 xchg CH,cl
clrloop: push cx
	 push di
	 xor CH,CH
	 rep stosw
	 pop di
	 pop cx
	 add di,0a0h
	 dec CH
	 jnz clrloop
	 pop es
	 popf
	 mov word ptr acty,0
	 jmp setcursor
clrscr endp

clreol proc near
	 pushf
	 push es
	 mov dx,word ptr acty
	 add dx,word ptr winypos
	 call berechnexy
	 mov cl,widthv
	 sub cl,actx
	 xor CH,CH
	 mov al,20h
	 mov ah,winattr
	 cld
	 rep stosw
	 pop es
	 popf
	 ret
clreol endp

delline proc near
	 pushf
	 push es
	 cld
	 mov dx, word ptr winypos
	 add dl,acty
	 call berechnexy
	 mov al,acty
	 inc al
	 cmp al,height
	 jz lastline
	 mov si,di
	 add si,0a0h
	 mov cx,word ptr height
	 xchg CH,cl
	 dec CH
	 sub CH,acty
	 push ds
	 push es
	 pop ds
delloop: push cx
	 push si
	 push di
	 xor CH,CH
	 rep movsw
	 pop di
	 add di,0a0h
	 pop si
	 add si,0a0h
	 pop cx
	 dec CH
	 jnz delloop
	 pop ds
lastline label near
	 mov cl,widthv
	 xor CH,CH
	 mov al,20h
	 mov ah,winattr
	 cld
	 rep stosw
	 pop es
	 popf
	 ret
delline endp

insline proc near
	 pushf
	 push es
	 cld
	 mov dx, word ptr winxpos
	 add dl,acty
	 call berechnexy
	 mov al,acty
	 inc al
	 cmp al,height
	 jz lastline
	 push di
	 mov dx,word ptr winypos
	 add al,height
	 dec dl
	 call berechnexy
	 mov si,di
	 sub si,0a0h
	 mov cx,word ptr height
	 xchg CH,cl
	 dec CH
	 sub CH,acty
	 push ds
	 push es
	 pop ds
insloop: push cx
	 push si
	 push di
	 xor CH,CH
	 rep movsw
	 pop di
	 sub di,0a0h
	 pop si
	 sub si,0a0h
	 pop cx
	 dec CH
	 jnz insloop
	 pop ds
	 pop di
	 jmp lastline
insline endp

setcolor proc near
	 mov winattr,al
	 ret
setcolor endp

getcolor proc near
	 mov al,winattr
	 ret
getcolor endp

defwindow proc near
	 or ah,ah
	 jnz defw1
exitdefw:stc
	 ret
defw1:	 or CH,CH
	 jz exitdefw
	 or cl,cl
	 jz exitdefw
	 cmp dh,4fh
	 ja exitdefw
	 cmp dl,18h
	 ja exitdefw
	 mov di,dx
	 add dh,CH
	 add dl,cl
	 dec dh
	 dec dl
	 cmp dh,4fh
	 ja exitdefw
	 cmp dl,18h
	 ja exitdefw
	 mov dx,di
	 and ah,7
	 push ax
	 mov bl,ah
	 xor bh,bh
	 shl bx,1
	 shl bx,1
	 shl bx,1
	 add bx,offset windows
	 pop ax
	 mov [bx],dx
	 mov [bx+2],cx
	 mov word ptr [bx+4],0
	 mov [bx+6],al
	 mov byte ptr [bx+7],0
	 cmp ah,wselect
	 jz selectnew
	 clc
	 ret
selectnew:call selectwin
	 clc 
	 ret
defwindow endp

askwindow proc near
	 and ah,7
	 mov bl,ah
	 xor bh,bh
	 mov cl,3
	 shl bx,cl
	 add bx,offset windows
	 mov dx,word ptr [bx]
	 mov cx,word ptr [bx+2]
	 mov al,byte ptr [bx+6]
	 ret
askwindow endp

selwindow proc near
	 and ah,7
	 mov wselect,ah
	 jmp selectwin
selwindow endp

putframe proc near
	 cmp byte ptr [si+5],1
	 jz frameok
	 ret
frameok: call saveback
	 mov byte ptr [si+5],0
	 mov dx,word ptr [si]
	 xchg dh,dl
	 pushf
	 push es
	 cld
	 call berechnexy
	 mov cx, word ptr [si+2]
	 xchg CH,cl
	 mov ah,byte ptr [si+4] 
	 push si
	 mov si,word ptr[si+6]
frloop:  push cx
	 push di
	 xor CH,CH
frloop1: lodsb
	 or al,al
	 jz spezial1
	 cmp al,1
	 jz spezial2
	 cmp al,2
	 jz spezial3
	 stosw
contfr:  loop frloop1
	 pop di
	 add di,0a0h
	 pop cx
	 dec CH
	 jnz frloop
	 pop si
	 pop es
	 popf
	 ret
spezial1:lodsb
	 mov ah,al
	 jmp frloop1
spezial2:add di,2
	 jmp short contfr
spezial3:and byte ptr es:[di+1],7
	 jmp short spezial2
putframe endp	      

delframe proc near
	 cmp byte ptr [si+5],0
	 jz delfrok
	 ret
delfrok: mov byte ptr [si+5],1
	 mov dx,word ptr [si]
	 xchg dh,dl
	 pushf
	 push es
	 cld
	 call berechnexy
	 mov cx,word ptr [si+2]
	 xchg CH,cl
	 mov si,word ptr [si+8]
delfrloop:push cx
	 push di
	 xor CH,CH
	 rep movsw
	 pop di
	 add di,0a0h
	 pop cx
	 dec CH
	 jnz delfrloop
	 pop es
	 popf
	 ret
delframe endp

scrollup proc near
	 pushf
	 push es
	 mov dx,word ptr winypos
	 call berechnexy
	 cmp height,1
	 jnz scrupok
	 jmp lastline
scrupok: inc scrupflag
	 mov cx,word ptr height
	 xchg Ch,cl
	 dec CH
	 mov si,di
	 add si,0a0h
	 push ds
	 push es
	 pop ds
scruploop:push cx
	 push si
	 push di
	 xor CH,CH
	 rep movsw
	 pop di
	 add di,0a0h
	 pop si
	 add si,0a0h
	 pop cx
	 dec CH
	 jnz scruploop
	 pop ds
	 jmp lastline
scrollup endp

scrolldn proc near
	 pushf
	 push es
	 mov dx,word ptr winypos
	 add dl,height
	 dec dl
	 call berechnexy
	 cmp height,1
	 jnz scrdnok
	 jmp lastline
scrdnok: mov cx, word ptr height
	 xchg CH,cl
	 dec CH
	 mov si,di
	 sub si,0a0h
	 push ds
	 push es
	 pop ds
scrdnloop:push cx
	 push si
	 push di
	 xor CH,CH
	 rep movsw
	 pop di
	 sub di,0a0h
	 pop si
	 sub si,0a0h
	 pop cx
	 dec CH
	 jnz scrdnloop
	 pop ds
	 jmp lastline
scrolldn endp

scrolllf proc near
	 pushf
	 push es
	 mov dx,word ptr winypos
	 call berechnexy
	 cmp widthv,1
	 jnz scrlfok
clrcol label near
	 mov cl,height
	 xor CH,CH
	 mov al,20h
	 mov ah,winattr
clcolloop: stosw
	 add di,7eh
	 loop clcolloop
	 pop es
	 popf
	 ret
scrlfok: mov si,di
	 add si,2	  
	 mov cx,word ptr height
	 xchg CH,cl
	 dec cl
	 mov al,20h
	 mov ah,winattr
	 push ds
	 push es
	 pop ds
scrlfloop:push cx
	 push si
	 push di
	 xor CH,CH
	 rep movsw
	 stosw
	 pop di
	 add di,0a0h
	 pop si
	 add si,0a0h
	 pop cx
	 dec CH
	 jnz scrlfloop
	 pop ds
	 pop es
	 popf
	 ret
scrolllf endp

scrollrg proc near
	 pushf
	 push es
	 mov dx,word ptr winypos
	 call berechnexy
	 cmp widthv,1
	 jnz scrrgok
	 jmp short clrcol
scrrgok: mov al,widthv
	 dec al
	 xor ah,ah
	 shl ax,1
	 add di,ax
	 mov si,di
	 sub si,2
	 mov cx,word ptr height
	 xchg CH,cl
	 dec cl
	 mov al,20h
	 mov ah,winattr
	 push ds
	 push es
	 pop ds
	 std
scrrgloop:push cx
	 push si
	 push di
	 xor CH,CH
	 rep movsw
	 stosw
	 pop di
	 add di,0a0h
	 pop si
	 add si,0a0h
	 pop cx
	 dec CH
	 jnz scrrgloop
	 pop ds
	 pop es
	 popf
	 ret
scrollrg endp

gotoxy proc near
	 mov word ptr acty,dx
	 jmp setcursor
gotoxy endp

getxy proc near
	 mov dx,word ptr acty
	 ret
getxy endp

showcurs proc near
	 mov curflag,0
	 jmp setcursor
showcurs endp

hidecurs proc near
	 mov curflag,1
	 jmp setcursor
hidecurs endp

print proc near
	 pushf
	 push si
	 push es
	 mov dx,word ptr acty
	 push dx
	 add dx,word ptr winypos
	 call berechnexy
	 pop dx
	 inc si
	 lodsb
	 or al,al
	 jz exitprint
	 mov cl,al
	 xor CH,CH
	 mov ah,winattr
prtloop: lodsb
	 cmp al,0dh
	 jz cr
	 cmp al,0ah
	 jz lf
	 stosw
	 inc dh
	 cmp dh,widthv
	 jb prtok
	 xor dh,dh
	 inc dl
	 cmp dl,height
	 jb newkoord
prtscroll:dec dl
	 push ax
	 push cx
	 push dx
	 push si
	 call scrollup
	 pop si
	 pop dx
	 pop cx
	 pop ax
newkoord: push dx
	 add dx,word ptr winypos
	 call berechnexy
	 pop dx
prtok:	 loop prtloop
	 mov word ptr acty,dx
	 call setcursor
exitprint:pop es
	 pop si
	 popf
	 ret
cr:	 xor dh,dh
	 jmp newkoord
lf:	 inc dl
	 cmp dl,height
	 jb newkoord
	 jmp prtscroll
print endp

convstring proc near
	 pushf
	 push si
	 push di
	 cld
	 xor ah,ah
	 push si
cnvloop: lodsb
	 or al,al
	 jz lengthok
	 inc ah
	 jmp short cnvloop
lengthok:pop si
	 mov [di+1],ah
	 or ah,ah
	 jz nostring
	 cmp ah,[di]
	 jbe stringok
	 mov ah,[di]
stringok:mov cl,ah
	 xor CH,CH
	 mov [di+1],ah
	 add di,2
	 rep movsb
nostring:pop di
	 pop si
	 popf
	 ret
convstring endp

readkey proc near
	 mov keyflag,al
	 mov outflag,ah
rkloop:  xor ah,ah
	 int 16h
	 cmp keyflag,0
	 jz charok
	 cmp al,"a"
	 jb charok
	 cmp al,"z"
	 ja charok
	 and al,0dfh
charok:  or cx,cx
	 jnz tstchar
	 ret
tstchar: pushf
	 push cx
	 push es
	 mov di,si
	 push ds
	 pop es
	 cld
	 repnz scasb
	 jnz chrfound
	 pop es
	 pop cx
	 popf
	 jmp short rkloop
chrfound:push ax
	 cmp outflag,0
	 jz nooutput
	 push bx
	 push si
	 mov sysstr+2,al
	 mov si,offset sysstr
	 call print
	 pop si
	 pop bx
nooutput:pop ax
	 pop es
	 pop cx
	 popf
	 ret
readkey endp

flush proc near
	 cli
	 push es
	 xor ax,ax
	 mov es,ax
	 mov ax,es:[041ah]
	 mov es:[041ch],ax
	 pop es
	 sti
	 ret
flush endp

input proc near
	 pushf
	 push cx
	 push si
	 push di
	 push bp
	 push es
	 push si
	 push ds
	 pop es
	 mov scrupflag,0
	 mov delflag,ah
	 mov keyflag,al
	 mov anzkeys,cx
	 mov dx,word ptr acty
	 mov posxy,dx
	 mov bufadr,di
	 mov ah,2
	 int 16h
	 and al,80h
	 mov insert,al
	 mov di,offset strbuf
	 push di
	 mov cx,257
	 cld
	 rep movsb
	 pop si
	 call print
	 cmp scrupflag,0
	 jz noscroll
	 mov dx,posxy
	 sub dl,scrupflag
	 mov posxy,dx
	 mov scrupflag,0
noscroll:mov bp,si
	 mov al,ds:[bp+1]
	 mov bufpos,al
	 xor ah,ah
	 add ax,2
	 add bp,ax
getkey:  xor ah,ah
	 int 16h
	 cmp keyflag,0
	 jz noupchar
	 cmp al,"a"
	 jb noupchar
	 cmp al,"z"
	 ja noupchar
	 and al,0dfh
noupchar:cmp anzkeys,0
	 jz keyok
	 mov di,bufadr
	 mov cx, anzkeys
	 repnz scasb
	 jnz keyok
	 jmp short getkey
keyok:	 cmp al,1bh
	 jnz chkkey1
	 mov al,-1
	 pop si
exitinput: pop es
	 pop bp
	 pop di
	 pop si
	 pop cx
	 popf
	 ret
chkkey1: cmp al,0dh
	 jnz chkkey2
	 mov cl,[si+1]
	 xor CH,ch
	 add cx,2
	 pop di
	 push di
	 rep movsb
	 mov dx,posxy
	 mov word ptr acty,dx
	 pop si
	 call print
	 xor al,al
	 jmp short exitinput
chkkey2: cmp ah,82
	 jnz chkkey3
	 xor byte ptr insert,80h
	 jmp getkey
chkkey3: cmp ah,75
	 jnz chkkey4
	 jmp curleft
chkkey4: cmp ah,77
	 jnz chkkey5
	 jmp curright
chkkey5:cmp ah,72
	 jnz chkkey6
	 jmp curup
chkkey6: cmp ah,80
	 jnz chkkey7
	 jmp curdown
chkkey7: cmp ah,71
	 jnz chkkey8
	 jmp curhome
chkkey8: cmp ah,79
	 jnz chkkey9
	 jmp curend
chkkey9: cmp ah,83
	 jnz chkkey10
	 jmp curdel
chkkey10:cmp ax,0e08h
	 jnz chkkey11
	 jmp curdellf
chkkey11:cmp ax,7700h
	 jnz chkkey12
	 jmp delall
chkkey12:cmp al,20h
	 jae chrok
	 jmp getkey
chrok:	 cmp delflag,0
	 jz chrok1
	 mov delflag,0
	 push ax
	 call freebuffer
	 pop ax
chrok1: mov bl,bufpos
	 cmp bl,[si]
	 jnz freeok
	 push si
	 push di
	 push bp
	 push es
	 mov ax,0e07h
	 int 10h
	 pop es
	 pop bp
	 pop di
	 pop si
	 jmp getkey
freeok:  call chkinsert
	 mov ds:[bp],al
	 mov al,[si+1]
	 cmp al,[si]
	 jz dontinc
	 inc byte ptr [si+1]
dontinc: mov dx,posxy
	 cmp scrupflag,0
	 jz printok
	 dec dl
	 mov posxy,dx
	 mov scrupflag,0
printok: xchg dx,word ptr acty
	 push dx
	 call print
	 pop word ptr acty
	 mov dx,posxy
	 sub dl,scrupflag
	 mov posxy,dx
	 mov scrupflag,0
	 jmp curright
poscursor:mov word ptr acty,dx
	 push si
	 push di
	 push bp
	 call setcursor
	 pop bp
	 pop di
	 pop si
	 jmp getkey
curleft: mov delflag,0
	 cmp bufpos,0
	 jnz curleftok
	 jmp getkey
curleftok:dec bp
	 dec bufpos
	 mov dx,word ptr acty
	 sub dh,1
	 jns poscursor
	 mov dh,widthv
	 dec dh
	 dec dl
	 jmp poscursor
curright:mov delflag,0
	 mov al,bufpos
	 cmp al,[si+1]
	 jnz currgok
	 jmp getkey
currgok: inc bp
	 inc bufpos
	 mov dx,word ptr acty
	 inc dh
	 cmp dh,widthv
	 jb poscursor
	 xor dh,dh
	 inc dl
	 jmp poscursor
curup:	 mov delflag,0
	 call getpos
	 or al,al
	 jz nocurmove
	 dec dl
	 mov al,widthv
	 sub bufpos,al
	 xor ah,ah
	 sub bp,ax
	 jmp poscursor
nocurmove:jmp getkey
curdown: mov delflag,0
	 mov al,bufpos
	 add al,widthv
	 jc nocurmove
	 cmp al,[si+1]
	 ja nocurmove
	 call getpos
	 inc dl
	 mov al,widthv
	 add bufpos,al
	 xor ah,ah
	 add bp,ax
	 jmp poscursor
curhome: mov delflag,0
	 mov bp,si
	 add bp,2
	 mov bufpos,0
	 call getpos
	 jmp poscursor
curend:  mov delflag,0
	 mov al,[si+1]
	 mov bufpos,al
	 xor ah,ah
	 mov bp,ax
	 add bp,si
	 add bp,2
	 call getpos
	 jmp poscursor
curdel:  mov delflag,0
	 cmp byte ptr [si+1],0
	 jnz delok
	 jmp getkey
delok:	 mov di,bp
	 mov cl,[si+1]
	 pushf
	 push si
	 mov si,di
	 inc si
	 cld
	 sub cl,bufpos
	 jz nodelete
	 push es
	 push ds
	 pop es
	 xor CH,CH
	 dec cl
	 jz nodelete1
	 rep movsb
nodelete1:mov al,20h
	 stosb
	 pop es
	 pop si
	 push si
	 mov dx,posxy
	 xchg dx,word ptr acty
	 push dx
	 call print
	 pop dx
	 mov word ptr acty,dx
	 pop si
	 dec byte ptr [si+1]
	 popf
	 jmp poscursor
nodelete:pop si
	 popf	      
	 jmp getkey
curdellf:mov delflag,0
	 cmp bufpos,0
	 jnz curdlfok
	 jmp getkey
curdlfok:dec bp
	 dec bufpos
	 mov dx,word ptr acty
	 sub dh,1
	 jns poscurs
	 mov dh,widthv
	 dec dh
	 dec dl
poscurs: mov word ptr acty,dx
	 push si
	 call setcursor
	 pop si
	 jmp curdel
delall:  mov delflag,0
	 call freebuffer
	 jmp getkey
input endp

deletestr proc near
	 mov cl,[si+1]
	 or cl,cl
	 jz extdelstr
	 cmp ah,cl
	 ja extdelstr
	 or ah,ah
	 jz extdelstr
	 or al,al
	 jnz dellenok
	 mov al,ah
	 sub al,ah
	 inc al
dellenok:mov CH,ah
	 add CH,al
	 dec CH
	 cmp CH,cl
	 ja extdelstr
	 jnz okdelstr
	 sub [si+1],al
extdelstr:ret
okdelstr:pushf
	 push ax
	 push si
	 push es
	 push ds
	 pop es
	 mov bl,[si+1]
	 sub [si+1],al
	 mov di,si
	 mov dl,ah
	 xor dh,dh
	 add di,dx
	 inc di
	 mov si,di
	 mov dl,al
	 add si,dx
	 xor bh,bh
	 add al,ah
	 sub bl,al
	 inc bl
	 mov cx,bx
	 cld
	 rep movsb
	 pop es
	 pop si
	 pop ax
	 popf
	 ret
deletestr endp

insertstr proc near
	 mov al,[si+1]
	 or al,al
	 jz extinsstr
	 or ah,ah
	 jz extinsstr
	 cmp ah,[di+1]
	 ja extinsstr
	 add al,[di+1]
	 cmp al,[di]
	 ja extinsstr
	 pushf
	 push ax
	 push si
	 push di
	 push es
	 push ds
	 pop es
	 mov cl,[di+1]
	 sub cl,ah
	 inc cl
	 xor CH,CH
	 mov bl,[si+1]
	 push si
	 push di
	 mov si,di
	 mov dl,[di+1]
	 xor dh,dh
	 add si,dx
	 inc si
	 xor bh,bh
	 mov di,si
	 add di,bx
	 std
	 rep movsb
	 pop di
	 pop si
	 add si,2
	 mov cl,[si-1]
	 xor CH,CH
	 mov dl,ah
	 add di,dx
	 inc di
	 cld
	 rep movsb
	 pop es
	 pop di
	 pop si
	 mov al,[si+1]
	 add [di+1],al
	 pop ax
	 popf
extinsstr:ret
insertstr endp

copystr proc near
	 mov byte ptr[di+1],0
	 mov cl,[si+1]
	 or cl,cl
	 jz extcopystr
	 or ah,ah
	 jz extcopystr
	 cmp ah,cl
	 ja extcopystr
	 or al,al
	 jnz clenok
	 mov al,cl
	 sub al,ah
	 inc al
clenok:  cmp al,[di]
	 jbe countok
	 mov al,[di]
countok: mov CH,ah
	 add CH,al
	 dec CH
	 cmp CH,cl
	 ja extcopystr
	 pushf
	 push ax
	 push si
	 push di
	 push es
	 push ds
	 pop es
	 mov [di+1],al
	 add di,2
	 mov cl,al
	 xor CH,CH
	 mov al,ah
	 xor ah,ah
	 add si,ax
	 inc si
	 cld
	 rep movsb
	 pop es
	 pop di
	 pop si
	 pop ax
	 popf
extcopystr:ret
copystr endp

concatstr proc near
	mov cl,[si+1]
	or cl,cl
	jz extconstr
	mov al,cl
	add al,[di+1]
	cmp al,[di]
	ja extconstr
	pushf
	push si
	push di
	push es
	push ds
	pop es
	mov al,[di+1]
	add [di+1],cl
	xor CH,CH
	add si,2
	xor ah,ah
	add di,ax
	add di,2
	cld
	rep movsb
	pop es
	pop di
	pop si
	popf
extconstr:ret
concatstr endp

posstr proc near
	 mov cl,[di+1]
	 mov CH,[si+1]
	 or cl,cl
	 jz extposstr
	 cmp cl,CH
	 ja extposstr
	 pushf
	 cld
	 push si
	 push di
	 push es
	 push ds
	 pop es
	 add si,2
	 add di,2
	 sub CH,cl
	 inc CH
	 mov al,1
posloop: push cx
	  push si
	  push di
	  xor CH,CH
	  repz cmpsb
	  jz posfound
	  pop di
	  pop si
	  pop cx
	  inc al
	  inc si
	  dec CH
	  jnz posloop
	  pop es
	  pop di
	  pop si
	  popf
extposstr:xor al,al
	 ret
posfound:pop di
	 pop si
	 pop cx
	 pop es
	 pop di
	 pop si
	 popf
	 ret
posstr endp

upstr proc near
	 mov cl,[si+1]
	 or cl,cl
	 jnz okupstr
	 ret
okupstr: xor CH,CH
	 pushf
	 push si
	 cld
	 add si,2
uploop:  lodsb
	 cmp al,"a"
	 jb chkok
	 cmp al,"z"
	 ja chkok
	 and al,0dfh
	 mov [si-1],al
chkok:	 loop uploop
	 pop si
	 popf
	 ret
upstr endp

ignchar proc near
	 mov cl,[si+1]
	 or cl,cl
	 jz extignchr
	 push si
	 xor CH,CH
	 add si,2
	 xor ah,ah
ignloop: cmp [si],al
	 jnz ignok	   
	 inc ah
	 inc si
	 loop ignloop
ignok:	 pop si
	 or ah,ah
	 jz extignchr
	 mov al,ah
	 mov ah,1
	 jmp deletestr
extignchr:ret
ignchar endp	     

cutchar proc near
	 mov cl,[si+1]
	 or cl,cl
	 jz extcutchr
	 push si
	 xor CH,CH
	 add si,cx
	 inc si
	 xor ah,ah
	 mov dl,cl
cutloop: cmp [si],al
	 jnz cutok
	 inc ah
	 dec si
	 dec dl
	 loop cutloop
cutok:	 inc dl
	  pop si
	  or ah,ah
	  jz extcutchr
	  mov al,ah
	  mov ah,dl
	  jmp deletestr
extcutchr: ret
cutchar endp

partostr proc near
	 mov byte ptr [si+1],0
	 mov cl,es:[di]
	 or cl,cl
	 jz extparstr
	 cmp cl,[si]
	 ja extparstr
	 pushf
	 push si
	 push di
	 push ds
	 push es
	 push ds
	 push es
	 pop ds
	 pop es
	 inc di
	 mov es:[si+1],cl
	 add si,2
	 xchg si,di
	 cld
	 xor CH,CH
	 rep movsb
	 pop es
	 pop ds
	 pop di
	 pop si
	 popf
extparstr:ret
partostr endp

dadd proc near	       
	 add ax,bx
	 adc dx,cx
	 ret
dadd endp	  

dsub proc near	       
	 sub ax,bx
	 sbb dx,cx
	 ret
dsub endp	  

dmul proc near
	 push si
	 push di
	 push dx
	 push ax
	 mul bx
	 mov di,dx
	 mov si,ax
	 pop ax
	 mul cx
	 add di,ax
	 pop ax
	 mul bx
	 add di,ax
	 mov dx,di
	 mov ax,si
	 pop di
	 pop si
	 ret
dmul endp

ddiv proc near
	 push bx
	 or bx,cx
	 pop bx
	 jnz startdiv
	 stc
	 ret
startdiv:push bp
	 push si
	 push di
	 mov si,bx
	 mov di,cx
	 mov cx,20h
	 xor bx,bx
	 xor bp,bp
	 shl ax,1
	 rcl dx,1
divloop:rcl bx,1
	 rcl bp,1
	 push bx
	 push bp
	 sub bx,si
	 sbb bp,di
	 jnc divok
	 pop bp
	 pop bx
	 clc
	 jmp contdivi
divok:	 add sp,4
	 stc
contdivi:rcl ax,1
	 rcl dx,1
	 loop divloop
	 mov cx,bp
	 pop di
	 pop si
	 pop bp
	 clc
	 ret
ddiv endp

dneg proc near
	  not ax
	  not dx
	  add ax,1
	  adc dx,0
	  ret
dneg endp

numtostr proc near
	 cmp bl,2
	 jb basiserr
	 cmp bl,10h
	 jbe basisok
basiserr:stc
	 ret
basisok: push ax
	 push bx
	 push bp
	 push dx
	 push si
	 xor bp,bp
	 mov basis,bl
	 xor cx,cx
	 xor bh,bh
ntsloop: mov bl,basis
	 call ddiv
	 add bl,"0"
	 cmp bl,"9"
	 jbe ziffok
	 add bl,7
ziffok:  push bx
	 inc bp
	 or ax,ax
	 jnz ntsloop
	 or dx,dx
	 jnz ntsloop
	 mov cx,bp
	 cmp cl,[si]
	 ja nozahl
	 mov [si+1],cl
	 add si,2
gtloop:  pop ax
	 mov [si],al
	 inc si
	 loop gtloop
	 pop si
	 pop dx
	 pop bp
	 pop bx
	 pop ax
	 clc
	 ret
nozahl:  pop ax
	 loop nozahl
	 pop si
	 mov byte ptr[si+1],0
	 pop dx
	 pop bp
	 pop bx
	 pop ax
	 stc
	 ret
numtostr endp	      

strtonum proc near
	 mov negflag,0
	 xor ax,ax
	 xor dx,dx
	 mov cl,[si+1]
	 or cl,cl
	 jnz zstrok
	 ret
zstrok:  push bx
	 push cx
	 mov di,si
	 xor CH,CH
	 add di,cx
	 inc di
	 mov bl,[di]
	 and bl,0dfh
	 cmp bl,"H"
	 jnz nextz
	 mov bh,10h
	 dec cl
	 cmp cl,8
	 jbe zok
	 jmp short zerror
nextz:	 cmp bl,"B"
	 jnz nextz1
	 mov bh,2
	 cmp cl,21h
	 ja zerror
	 dec cl
	 jmp short zok
zerror:  pop cx
	 pop bx
	 stc
	 ret
nextz1:  mov bh,0ah
	cmp cl,0bh
	ja zerror
	cmp byte ptr [si+2],"-"
	jnz zok1
	dec cl
	mov bl,0ah
	mov di,si
	add di,3
	mov negflag,-1
	jmp short stnloop
zok1:	cmp cl,0bh
	jz zerror 
zok:	mov bl,bh	 
	mov di,si
	add di,2
stnloop:push bx
	push cx
	xor cx,cx
	xor bh,bh
	call dmul
	pop cx
	pop bx
	mov bh,[di]
	cmp bh,"a"
	jb zfok
	and bh,0dfh
zfok:	sub bh,"0"
	cmp bh,9
	jbe nok
	sub bh,7
nok:	push bx
	mov bl,bh
	xor bh,bh
	add ax,bx
	adc dx,0
	pop bx
	inc di
	loop stnloop
	cmp negflag,0
	jz zpositiv
	call dneg
zpositiv:pop cx
	pop bx
	clc
	ret
strtonum endp

;Hilfsroutinen

berechnexy proc near
	push ax
	push cx
	push dx
	xor dh,dh
	mov ax,dx
	mov cl,7
	shl ax,cl
	mov di,ax
	add di,dx
	add di,dx   
	add di,dx   
	add di,dx   
	add di,dx   
	add di,dx   
	add di,dx   
	add di,dx   
	add di,dx   
	add di,dx   
	add di,dx   
	add di,dx   
	add di,dx   
	add di,dx   
	add di,dx   
	add di,dx   
	add di,dx   
	add di,dx   
	add di,dx   
	add di,dx   
	add di,dx   
	add di,dx   
	add di,dx   
	add di,dx   
	add di,dx   
	add di,dx   
	add di,dx   
	add di,dx   
	add di,dx   
	add di,dx   
	add di,dx   
	add di,dx   
	pop dx
	mov al,dh
	xor ah,ah
	shl ax,1
	add di,ax
	pop cx
	pop ax
	mov es,scrseg
	ret
berechnexy endp

selectwin proc near
	pushf
	push es
	push ds
	pop es
	cld
	mov al,wselect
	xor ah,ah
	mov cl,3
	shl ax,cl
	add ax,offset windows
	mov si,ax
	mov di,offset winypos
	mov cx,4
	rep movsw
	pop es
	popf
selectwin endp

setcursor proc near
	cmp curflag,0
	jz cursorvis
	mov ah,2
	xor bh,bh
	mov dx,1900h
	int 10h
	ret
cursorvis:mov ax,word ptr acty
	mov dx,ax
	inc ah
	cmp ah,widthv
	jbe xok
	xor dh,dh
	inc dl
	inc al
xok:	inc al
	mov cl,height
	cmp al,cl
	jbe yok
	mov dl,cl
	dec dl
	sub al,cl
	dec al
	jz yok
	mov cl,al
	xor CH,CH
	push dx
scrcrloop:push cx
	call scrollup
	pop cx
	loop scrcrloop
	pop dx
yok:	mov word ptr acty,dx
	add dx,word ptr winypos
	xchg dh,dl
	xor bh,bh
	mov ah,2
	int 10h
	ret
setcursor endp

saveback proc near
	pushf
	push si
	push ds
	push es
	cld 
	mov dx,word ptr [si]
	xchg dh,dl
	call berechnexy
	mov cx,word ptr [si+2]
	xchg CH,cl
	mov si,word ptr[si+8]
	xchg si,di
	cld
	push ds
	push es
	pop ds
	pop es
sbloop: push cx
	push si
	xor CH,CH
	rep movsb
	pop si
	add si,0a0h
	pop cx
	dec CH
	jnz sbloop
	pop es
	pop ds
	pop si
	popf
	ret
saveback endp

chkinsert proc near
	pushf
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	cmp insert,0
	jz extchkins
	mov al,bufpos
	cmp al,[si+1]
	jz extchkins
	mov al,[si]
	xor ah,ah
	add si,ax
	inc si
	mov di,si
	inc di
	mov cx,ax
	sub cl,bufpos
	std
	rep movsb
extchkins:pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	popf
	ret
chkinsert endp

getpos proc near
	 push cx
	 mov al,bufpos
	 xor ah,ah
	 mov cl,widthv
	 div cl
	 mov dx,posxy
	 add dx,ax
	 pop cx
	 ret
getpos endp

freebuffer proc near
	mov cl,[si+1]
	or cl,cl
	jnz delallok
	ret
delallok:xor CH,CH
	mov di,si
	add di,2
	mov al,20h
	push es
	push ds
	pop es
	cld
	rep stosb
	pop es
	mov dx,posxy
	xchg dx,word ptr acty
	push dx
	call print
	pop word ptr acty
	mov bp,si
	add bp,2
	mov bufpos,0
	mov byte ptr[si+1],0
	call getpos
	mov word ptr acty,dx
	push si
	push di
	push bp
	call setcursor
	pop bp
	pop di
	pop si
	ret
freebuffer endp

code ends

end
