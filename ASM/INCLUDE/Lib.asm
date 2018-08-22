; CUBEOS - Language Lib
; LIB.asm M.Höller-Schlieper 2008
;
;======================================================
; USED REGISTERS:
;	ESI - Base Object Pointer - DO NOT USE IN LIB!
;	EDI - string start adress
;
;	EAX - IN and Out RESULT 
;	ECX
;
;	SP(0) - Double
;
;
;======================================================
;======================================================
;Proc $_StringWrite
;Load ESI with Adress of the String db 1,Size,"String"
;
; LEA  ESI,szTitleName
; Call $_StringWrite
;
;Author: MH
;Date:  25.08.2007
;======================================================
$_CharWrite proc
		
		SUB     ESP,3				; Char soll auf den Stack
		mov     ESI,ESP
		mov 	Byte ptr[ESI],1  
        mov 	Byte ptr[ESI+1],1
		mov		Byte ptr[ESI+2], AL  ; Char laden	
		
        XOR		EAX,EAX				; Register leeren
        mov		al,1	            ; Länge des Strings laden
        push    EAX           		; Länge des Strings übergeben
		ADD 	ESI,2
        push    ESI    				; Zeiger auf String übergeben
        
        LEA		ESI,_Y
        XOR		EAX,EAX 
        mov		ax,word ptr [ESI]	; Y-Position laden
        push    EAX               	; y - Position
        
        LEA		ESI,_X
        XOR		EAX,EAX 
        mov		ax,word ptr [ESI]	; X-Position laden
        push    EAX               	; X - Position
        
        push    [theDC]           	; Fensterhandle
        call    TextOut				; Text ausgeben
		ADD		ESP,3				; Stack korrigieren
		
       
		Ret
$_CharWrite endp

;======================================================
;Proc $_StringWrite
;Load ESI with Adress of the String db 1,Size,"String"
;
; LEA  ESI,szTitleName
; Call $_StringWrite
;
;Author: MH
;Date:  25.08.2007
;======================================================
$_StringWrite proc
		
				
        XOR		EAX,EAX				; Register leeren
        mov		al,Byte ptr [ESI+1]	; Länge des Strings laden
        push    EAX           		; Länge des Strings übergeben
        add		ESI, 2				; Startadress berechnen
        push    ESI    				; string übergeben
        
        LEA		ESI,_Y
        XOR		EAX,EAX 
        mov		ax,word ptr [ESI]	; Y-Position laden
        push    EAX               	; y - Position
        
        LEA		ESI,_X
        XOR		EAX,EAX 
        mov		ax,word ptr [ESI]	; X-Position laden
        push    EAX               	; X - Position
        
        push    [theDC]           	; Fensterhandle
        call    TextOut				; Text ausgeben

       
		Ret
$_StringWrite endp

;======================================================
;Proc $_NLN
; 		New Line
;
;Author: MH
;Date:  25.08.2007
;======================================================
$_NLN proc
	Mov EAX, _TextHeight
	ADD _Y, EAX
	
	Ret
$_NLN endp

;======================================================
;Proc $_Int32ToStr
; 		
;
;IN:
;    EAX:  The integer value to be converted to text
;    ESI:  Ptr to the right-hand side of the output buffer:  LEA ESI, StrBuf[16]
;    ECX:  Base for conversion: 0 for signed decimal, 10 or 16 for unsigned
;    EDX:  Precision: zero padded minimum field width
;  OUT:
;    ESI:  Ptr to start of converted text (not start of buffer)
;    ECX:  Length of converted text
;Author: MH
;Date:  25.08.2007
;======================================================
$_Int32ToStr Proc 
		
        OR      CL,CL
        JNZ     @CvtLoop
@C1:    OR      EAX,EAX
        JNS     @C2
        NEG     EAX
        CALL    @C2
        MOV     AL,'-'
        INC     ECX
        DEC     ESI
        MOV     [ESI],AL
        RET
@C2:    MOV     ECX,10

@CvtLoop:
        PUSH    EDX
        PUSH    ESI
@D1:    XOR     EDX,EDX
        DIV     ECX
        DEC     ESI
        ADD     DL,'0'
        CMP     DL,'0'+10
        JB      @D2
        ADD     DL,('A'-'0')-10
@D2:    MOV     [ESI],DL
        OR      EAX,EAX
        JNE     @D1
        POP     ECX
        POP     EDX
        SUB     ECX,ESI
        SUB     EDX,ECX
        JBE     @D5
        ADD     ECX,EDX
        MOV     AL,'0'
        SUB     ESI,EDX
        JMP     @z
@zloop: MOV     [ESI+EDX],AL
@z:     DEC     EDX
        JNZ     @zloop
        MOV     [ESI],AL
        
@D5:
		RET
$_Int32ToStr endp

;======================================================
;Proc $_Allocate
; 		
;
;	IN:
;    EAX:  Size of Bytes to allocate 
;			Startptr dd 0
;			EndPtr   dd 0		 
;			Allocated dd 0
;					
;   OUT:    
;    ECX:  Length of allocated Address
;	 EAX:  new allocated Adress or 0 = overflow
;Author: MH
;Date:  05.11.2007
;======================================================
$_Allocate Proc
		
		LEA EBX, MemBlock 	;Startadresse laden
		ADD EBX, [Allocated]	;reservierten Speicher überspringen
				
		Add EAX,[Allocated] 	;Speicher schützen
		MOV [Allocated],EAX		;Allocated setzen
		
		;Overflow - Test
		;Mov ECX, EndPtr
		;CMP EBX, ECX			;Über den reservierten Bereich hinaus? 
		;JG @_Allocate_End
		;MOV EBX, 0 			; FEHLER
		
		; TODO: EXCEPTION AUSLÖSEN
@_Allocate_End:
		MOV EAX,EBX				;Anfangs - Speicheradresse nach EAX
		RET
$_Allocate endp

;======================================================
;Proc $_SingleToStr
; 		
;
;IN:
;    EAX:  The integer value to be converted to text
;    ESI:  Ptr to the right-hand side of the output buffer:  LEA ESI, StrBuf[16]
;    ECX:  Base for conversion: 0 for signed decimal, 10 or 16 for unsigned
;    EDX:  Precision: zero padded minimum field width
;  OUT:
;    ESI:  Ptr to start of converted text (not start of buffer)
;    ECX:  Length of converted text
;Author: MH
;Date:  25.08.2007
;======================================================
$_SingleToStr Proc 
		
        OR      CL,CL
        JNZ     @_SingleToStr_CvtLoop
@_SingleToStr_C1:    
		OR      EAX,EAX
        JNS     @_SingleToStr_C2
        NEG     EAX
        CALL    @_SingleToStr_C2
        MOV     AL,'-'
        INC     ECX
        DEC     ESI
        MOV     [ESI],AL
        RET
@_SingleToStr_C2:    
		MOV     ECX,10

@_SingleToStr_CvtLoop:
        PUSH    EDX
        PUSH    ESI
@_SingleToStr_D1:    
		XOR     EDX,EDX
        DIV     ECX
        DEC     ESI
        ADD     DL,'0'
        CMP     DL,'0'+10
        JB      @_SingleToStr_D2
        ADD     DL,('A'-'0')-10
@_SingleToStr_D2:    
		MOV     [ESI],DL
        OR      EAX,EAX
        JNE     @_SingleToStr_D1
        POP     ECX
        POP     EDX
        SUB     ECX,ESI
        SUB     EDX,ECX
        JBE     @_SingleToStr_D5
        ADD     ECX,EDX
        MOV     AL,'0'
        SUB     ESI,EDX
        JMP     @_SingleToStr_z
@_SingleToStr_zloop: 
		MOV     [ESI+EDX],AL
@_SingleToStr_z:     
		DEC     EDX
        JNZ     @_SingleToStr_zloop
        MOV     [ESI],AL
        
@_SingleToStr_D5:
		RET
$_SingleToStr endp


;======================================================
;Proc MathToString
; 		32 bit (EAX) number to string at Offset EDI
;
;  IN:  EAX: 32 Bit number 
;					
;  OUT: EDI string at Offset EDI
;    
;Author: MH
;Date:  05.11.2007
;======================================================
;Function MathToString:     32 bit (EAX) number to string at Offset EDI
 
MathToString PROC  NEAR     ;
.DATA
      @mathoutbcd     DT       0
      @mathoutint     DD       0
      @mathoutnull    DB       0
.CODE
    cmp eax,0
    jne short L1
    mov dx,48                       ; Number zero, create '0'+\0
    mov [edi],dx
    inc edi
    ret
L1:                                 ; Number not zero
    mov @mathoutint,eax
    fild @mathoutint
    fbstp @mathoutbcd
    mov ecx,9
    mov @mathoutnull,0
    mov al,byte ptr @mathoutbcd+9
    and al,128
    cmp al,0
    je short L2
    mov dx,'-'                      ; Number is negative
    mov [edi],dx
    inc edi
L2:                                 ; Number has no sign
    xor dx,dx
    mov esi,offset @mathoutbcd+8
MathToString_L3:
    mov al,[esi]
    shr ax,4
    and al,0fh
    cmp al,0
    je short MathToString_L4
    mov @mathoutnull,1
MathToString_L4:
    cmp @mathoutnull,0
    je short L5
    mov dl,al
    add dl,48
    mov [edi],dx
    inc edi
L5:
    mov al,[esi]
    and al,0fh
    cmp al,0
    je short L6
    mov @mathoutnull,1
L6:
    cmp @mathoutnull,0
    je short L7
    mov dl,al
    add dl,48
    mov [edi],dx
    inc edi
L7:
    dec esi
    dec cx
    cmp cx,0
    jne MathToString_L3
    ret
ENDP MathToString
;======================================================
;Proc DoubleToStr
; 		
;	converts double in ST(0) to string at Offset EDI
;IN:
;    	EAX: ST(0)
;					
;  		OUT: EDI
;Author: MH
;Date:  05.11.2007
;======================================================
;Function DoubleToStr;      converts double in ST(0) to string at Offset EDI
 
$_DoubleToStr  Proc ; string in EDI
.data

  const1 dq 1000000000000000000
  const2 dd 10

.code
  push ebp
  mov ebp,esp
  sub esp,24
  push edi
  fstcw word ptr [ebp-18]
  push dword ptr [ebp-18]
  bts dword ptr [ebp-18],10
  bts dword ptr [ebp-18],11
  fldcw word ptr [ebp-18]
  fclex
  ftst
  fstsw AX
  sahf
  je Null
  fxam
  fstsw AX
  bt ax,14
  jnc valid
invalid:
  bt ax,8
  jc Empty
  bt ax,9
  jnc NAN
  jmp INV
valid:
  fstp tbyte ptr [ebp-10]
  mov dword ptr [ebp-14],0
  bt ax,8
  jc Null
  fldlg2
  fld tbyte ptr [ebp-10]
  fyl2x
  fistp dword ptr [ebp-14]
  jmp NotNull
Null:
  jc invalid
  mov dword ptr [edi],'0.'
  jmp X
NotNull:
  mov dword ptr [ebp-24],0      ;   Exponent
  mov eax,dword ptr [ebp-14]
  cmp eax,4933                  ; Error
  jg INV
  cmp eax,-4933
  jl INV
  cmp eax,8
  jg extp
  cmp eax,-8
  jl expn
L0:
  fld tbyte ptr [ebp-10]
  fist dword ptr [ebp-14]
  mov eax, [ebp-14]
  call MathToString
  mov eax,'.000'
  mov [edi],eax
  fild dword ptr [ebp-14]
  fsubp st(1),st
  fld const1
  fmulp st(1),st
  fbstp tbyte ptr [ebp-10]
  mov ecx,8
  inc edi
l1:
  mov al,ss:[ebp-10+ecx]
  shr al,4
  and al,0fh
  add al,48
  mov [edi],al
  inc edi
  mov al,ss:[ebp-10+ecx]
  and al,0fh
  add al,48
  mov [edi],al
  inc edi
  dec ecx
  jns l1
l2:
  mov byte ptr [edi],0
  dec edi
  mov al,[edi]
  cmp al,48
  je L2
  cmp dword ptr [ebp-24],0
  je L4
; need to add exponent ...
  mov word ptr [edi],' E'
  inc edi
  inc edi
  cmp dword ptr [ebp-24],0
  jl L3
  mov byte ptr [edi],'+'
  inc edi
L3:
  mov eax,dword ptr [ebp-24]
  call MathToString
L4:
  jmp X

extp:  ; positive exponent is necessary!
  mov dword ptr [ebp-24],eax
  inc dword ptr [ebp-24]
  fld  const2
  dec eax
Lp:

  fld  const2
  fmulp st(1), st
  dec eax
  cmp eax,0
  jne Lp
;  fild dword ptr [ebp-14]
;  fldl2t
;  fmulp st(1),st
;  f2xm1
;  fld1
;  faddp st(1),st


  fld tbyte ptr [ebp-10]
  fdivrp st(1), st
  fstp tbyte ptr [ebp-10]
  jmp L0
expn:  ; negative exponent is necessary1
  mov dword ptr [ebp-24],eax
  dec dword ptr [ebp-24]
  neg eax
  fld  const2
Ln:
  fld  const2
  fmulp st(1), st
  dec eax
  cmp eax,0
  jne Ln
;  fild dword ptr [ebp-14]
;  fldl2t
;  fmulp st(1),st
;  f2xm1
;  fld1
;  faddp st(1),st

  fld tbyte ptr [ebp-10]
  fmulp st(1), st
  fstp tbyte ptr [ebp-10]
  jmp L0
NAN:
  mov eax,'NAN'
  mov [edi],eax
  xor eax,eax
  mov [edi+4],eax
  jmp X
INV:
  mov eax,'INV'
  mov [edi],eax
  xor eax,eax
  mov [edi+4],eax
  jmp X
Empty:
  mov eax,'EMPT'
  mov [edi],eax
  mov eax,'Y'
  mov [edi+4],eax
  jmp X

X:
  pop dword ptr [ebp-18]
  fldcw word ptr [ebp-18]
  pop edi
  mov esp,ebp
  pop ebp
  ret

$_DoubleToStr endp


