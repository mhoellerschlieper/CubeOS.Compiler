;Quelle: http://www.masm32.com/board/index.php?topic=9906.0
;
; SEEEEEEEEEEEFFFFFFFFFFFFFFFFFFFF FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
; 10000000000000000000000000000000 00000000000000000000000000000000
; 01111111111100000000000000000000 00000000000000000000000000000000
; 00000000000011111111111111111111 11111111111111111111111111111111
;            100000000000000000000 00000000000000000000000000000000 ; implied

OPTION PROLOGUE:NONE
OPTION EPILOGUE:NONE

R8_BIAS equ 1023
R8_MANT equ 52

R8ToStr proc r8:REAL8, lpBuffer:PTR

	push ebp
	push esi
	push edi
	push ebx
	locals = 32+3*4
	tmpbuff equ <[esp]>
	iTrail equ <dword ptr [esp+32]>
	iExp equ <dword ptr [esp+32+4]>
	nSign equ <dword ptr [esp+32+4+4]>
	add esp,-locals

	mov ecx,10000000000000000000000000000000b
	mov ebx,[esp+2*4][4*4][locals]
	mov esi,[esp+1*4][4*4][locals]	
	and ecx,ebx; sign bit
	and ebx,not 10000000000000000000000000000000b
	mov nSign,ecx
	mov edi,ebx
	shr ebx,20; 01111111111100000000000000000000
	and edi,00000000000011111111111111111111b
	cmp ebx,11111111111b
	je @@_NaN_Infinity
	mov eax,esi
	or eax,edi
	or eax,ebx
	jz @@Zero
	sub ebx,R8_BIAS; exponent
	or  edi,00000000000100000000000000000000b; high 20 bits + 1 implied
	xor ebp,ebp
	mov iExp,ebx
	;; 52bits in edi::esi
	.if sdword ptr ebx > 63
		shld edi,esi,63-R8_MANT
		shl esi,63-R8_MANT
		.repeat
			call __div10
			; mod 10
			lea ecx,[eax*4+eax]
			add ecx,ecx
			sub esi,ecx

			bsr ecx,edx
			neg ecx
			add ecx,31
			shld edx,eax,cl
			shl eax,cl

			mov ebx,iExp
			sub ebx,ecx
			
			or esi,eax
			mov edi,edx
			
			mov iExp,ebx
			add ebp,1
		.until sdword ptr ebx <= 63
		mov ecx,63
		sub ecx,ebx
		shrd esi,edi,cl
		shr edi,cl
	.else
		.while sdword ptr ebx < R8_MANT
			.while ! (edi & 0F0000000h)
				mov eax,esi
				mov edx,edi
				shld edi,esi,2
				shl esi,2
				add esi,eax
				adc edi,edx
				add ebx,1
				dec ebp
			.endw
			bsr ecx,edi
			sub ecx,31-4
			sbb edx,edx
			not edx
			and ecx,edx
			shrd esi,edi,cl
			shr edi,cl
			add ebx,ecx
		.endw
		lea ecx,[ebx-R8_MANT]
		shld edi,esi,cl
		shl esi,cl
	.endif
;	mov edx,nSign
;	pushad
;	shr edx,1
;	sbb edx,edx
;	and edx,'-'-'+'
;	add edx,'+'
;	invoke printf,T("%c%I64u.0e%i",13,10),edx,edi::esi,ebp
;	popad
	; job done, now just the hard part - formating
	
	;; adjust number to 16 digits 2386F26FC0FFFFh
	.while edi >= 2386F2h; LOW = 6FC0FFFF
		.break .if edi == 2386F2h && esi < 6FC0FFFFh
		call __div10
		mov esi,eax
		mov edi,edx
		add ebp,1;; increase exponent
	.endw

	;; round it if needed (if 16 digit) 38D7EA4C67FFFh
	.if edi>=38D7Eh;A4C67FFF
		.if ! (edi == 38D7Eh && esi < 0A4C67FFFh)
			add esi,5
			adc edi,0
			call __div10
			add ebp,1; increase exponent
			mov esi,eax
			mov edi,edx
		.endif		
	.endif
	
	mov iExp,ebp
	;; trailing zero count
	xor ebp,ebp
	jmp @F
	.repeat
		mov esi,eax
		mov edi,edx
		add ebp,1
	@@:	call __div10
		lea ecx,[eax*4+eax]
		neg ecx
		add ecx,ecx
		add ecx,esi
	.until !zero?
	mov iTrail,ebp
	xor ebp,ebp
	jmp @F
	.repeat
		call __div10
	@@:	lea ecx,[eax*4+eax]
		neg ecx
		lea ecx,[ecx*2+esi+'0']
		mov tmpbuff[ebp],cl
		add ebp,1
		mov esi,eax
		mov edi,edx
		or eax,edx
	.until zero?

	mov ecx,nSign
	mov esi,[esp+3*4][4*4][locals]	
	add ecx,ecx
	mov edx,'-'
	mov edi,iExp; exp
	sbb ecx,ecx
	and edx,ecx
	mov [esi],dl
	sub esi,ecx

	add edi,iTrail
	xchg esi,ebp
	.if zero?;; exponent is 0
		.repeat
			mov al,tmpbuff[esi-1]
			mov [ebp],al
			add ebp,1
			sub esi,1
		.until zero?
		
	.elseif (sdword ptr edi >=-15 && sdword ptr edi < 0)
		;; check for format without exp
		add edi,esi
		.if sdword ptr edi <= 0
			mov [ebp],word ptr '.0'
			add ebp,2
			.while sdword ptr edi < 0
				mov [ebp],byte ptr '0'
				add ebp,1
				add edi,1
			.endw
			.repeat
				mov al,tmpbuff[esi-1]
				mov [ebp],al
				add ebp,1
				sub esi,1
			.until zero?
		.else
			.repeat
				mov al,tmpbuff[esi-1]
				mov [ebp],al
				add ebp,1
				sub edi,1
				.if zero?
					mov [ebp],byte ptr '.'
					add ebp,1
				.endif
				sub esi,1
			.until zero?
		.endif
	.else
		;
		mov al,tmpbuff[esi-1]
		mov [ebp],al
		add ebp,1
		sub esi,1
		jz @F

		mov [ebp],byte ptr '.'
		add ebp,1
		.repeat
			mov al,tmpbuff[esi-1]
			mov [ebp],al
			add ebp,1
			add edi,1
			sub esi,1
		.until zero?
@@:
		mov [ebp],byte ptr 'e'
		add ebp,1

		mov eax,edi
		
		cdq
		and edx,'-'-'+'
		add edx,'+'
		mov [ebp],dl
		add ebp,1
		
		; abs
		cdq
		xor eax,edx
		sub eax,edx
		mov edi,0CCCCCCCDh; magic
		mov ecx,eax
		mul edi
		shr edx,3
		lea ebx,[edx*4+edx]
		neg ebx
		lea ebx,[ebx*2+ecx+'0']
		mov eax,edx
		.if edx
			mov ecx,eax
			mul edi
			shr edx,3
			lea esi,[edx*4+edx]
			neg esi
			lea esi,[esi*2+ecx+'0']
			mov eax,edx
			.if edx
				mov ecx,eax
				mul edi
				shr edx,3
				lea eax,[edx*4+edx]
				neg eax
				lea eax,[eax*2+ecx+'0']
				mov [ebp],al
				add ebp,1
			.endif
			mov eax,esi
			mov [ebp],al
			add ebp,1
		.endif
		mov [ebp],bl
		add ebp,1

	.endif

@@Done:	
	mov byte ptr [ebp],0
	mov eax,ebp
	sub eax,[esp+3*4][4*4][locals]	
	
	add esp,locals
	pop ebx
	pop edi
	pop esi
	pop ebp
	ret 3*4
	
@@_NaN_Infinity:
	mov ecx,nSign
	mov ebp,[esp+3*4][4*4][locals]
	add ecx,ecx
	mov edx,'-'
	sbb ecx,ecx
	and edx,ecx
	mov [ebp],dl
	sub ebp,ecx
	mov dword ptr [ebp],'#.1'	
	mov eax,edi
	or eax,esi
	.if !eax
		mov eax,'FNI'
		mov [ebp+3],eax	
		add ebp,6
	.elseif edi & 10000000000000000000b
		mov eax,'NANQ'
		mov [ebp+3],eax	
		add ebp,7
	.elseif ! (edi & 10000000000000000000b)
		mov eax,'NANS'
		mov [ebp+3],eax	
		add ebp,7
	.else
		mov eax,'DNI'
		mov [ebp+3],eax	
		add ebp,6
	.endif
	jmp @@Done

@@_Subnormal:
	mov ebp,[esp+3*4][4*4][locals]
	mov dword ptr [ebp],'!RRE'	
	add ebp,4
	jmp @@Done

@@Zero:
	mov ebp,[esp+3*4][4*4][locals]
	mov byte ptr [ebp],'0'	
	add ebp,1
	jmp @@Done

	;; div <edi::esi> by 10
	;; ret <edx::eax> 
	align 8
__div10:
	; div 10
	mov eax,0CCCCCCCDh; = b0
	mul esi; get a0*b0 = d1:d0
	mov ecx,edx;d1
	mov eax,0CCCCCCCDh; = b0
	xor ebx,ebx
	mul edi; get a1*b0 = e1:e0
	add ecx,eax;e0
	adc ebx,edx;e1
	mov eax,0CCCCCCCCh; =b1
	mul esi; get a0*b1 = f1:f0
	add ecx,eax;f0
	adc ebx,edx;f1
	mov ecx,0
	mov eax,0CCCCCCCCh; =b1
	adc ecx,ecx
	mul edi; get a1*b1 = g1:g0
	add eax,ebx;g0
	adc edx,ecx;g1
	shrd eax,edx,3
	shr edx,3;;------ quotient in edx::eax
	retn

R8ToStr endp 
