procedure getvect(intradr:integer;var seg:integer;var off:integer )
begin
 seg:=begin asm
        mov ah,35h
        mov al,08h
        int 21h
        mov ax,es
      end end
 
 off:=begin asm
        mov ax,bx
      end end
end

procedure setvect(intadr:integer; var neuseg:integer;var neuoff:integer )
begin 
 asm
  mov ah,25h
  mov al,08h
  mov ds,offset [bp-6]
  mov dx,offset [bp-4]
  int 21h
 end
end

procedure cseg
begin 
 asm
  mov ax,cs
 end
end

procedure eseg
begin 
 asm
  mov ax,es
 end
end
procedure dseg
begin 
 asm
  mov ax,ds
 end
end

procedure poffset(var i:integer)
begin 
 asm
  mov ax,offset [bp-4] 
 end
end
