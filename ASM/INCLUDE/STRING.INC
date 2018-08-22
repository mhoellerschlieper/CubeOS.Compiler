.COMMENT
 ****************************************************************************
 STRING.INC     Zero Terminated String Operations
                (c) 1998 by Dieter Pawelczak - Pass32 Version 2.1
 ============================================================================
 INTERFACE

 Function strcat: appends string EDI with string ESI, returns new length (EAX)
 Function strupcase: converts string in EDI to upcase string, returns length
 Function strcopy : copys string ESI to EDI, returns length in EAX
 Function strlength: returns the string EDI length in EAX
 Function strpos: Checks, if string ESI contains substring EDI
 Function strcmp: compares string ESI with string EDI result in eax
 Function substrcmp: compares a substring(EDI) with the begin of another string(ESI)


 ****************************************************************************
.CODE

.COMMENT ** Function strcat: appends string EDI with string ESI, returns new length **

PROC strcat          ; destination EDI, source ESI
  xor eax,eax
L0:
  mov bl, [edi+eax]
  cmp bl,0
  je short L1
  inc eax
  jmp L0
L1:
  mov bl, [esi]
  mov [edi+eax],bl
  cmp bl,0
  je short L2
  inc esi
  inc eax
  jmp L1
L2:
  ret
ENDP strcat

.COMMENT ** Function strupcase : converts string in EDI to upcase string, returns length **

PROC strupcase ; Source + Destination EDI
  xor eax,eax
L0:
  mov bl, [edi+eax]
  cmp bl,97
  jb L1
  and bl,223
L1:
  mov [edi+eax],bl
  cmp bl,0
  je short L2
  inc eax
  jmp L0
L2:
  ret
ENDP strupcase

.COMMENT ** Function strcopy : copys one string ESI to EDI, returns length **

PROC strcopy ; ESI Source, EDI Destination
  xor eax,eax
L0:
  mov bl, [esi+eax]
  mov [edi+eax],bl
  cmp bl,0
  je short L1
  inc eax
  jmp L0
L1:
  ret
ENDP strcopy


.COMMENT ** Function strlength: returns the EDI string length **

PROC strlength; returns the string length of zero terminated string in EDI
  xor eax,eax
L0:
  cmp byte ptr [edi+eax],0
  je SHORT L1
  inc eax
  jmp L0
L1:
  ret
ENDP strlength

.COMMENT ** Function strpos: Checks, if String ESI contains substring EDI **

PROC strpos ; Checks, if Zero Terminated String in ESI contains
            ; String in EDI, returns position, carry on error
  xor eax,eax
L0:
  mov bl,[esi+eax]
  cmp bl,0
  je short L5
  cmp [edi],bl
  je short L1
  inc eax
  jmp L0
L1:
   push edi
L2:
   inc eax
   inc edi
   mov bl,[edi]
   cmp bl,0
   je short L4
   cmp [esi+eax],bl
   jne short L3
   jmp L2
L3:
   pop edi
   inc eax
   jmp L0
L4:
   pop edi
   clc
   ret
L5:
   xor eax,eax
   stc
  ret
ENDP strpos

.COMMENT ** Function strcmp: compares string ESI with string EDI result in eax **

PROC strcmp  ; strings equal: EAX=0, string ESI>EDI EAX=1, string ESI<EDI EAX=-1
  xor eax,eax
L0:
  mov bl, [esi+eax]
  cmp [edi+eax], bl
  jb SHORT L3
  ja SHORT L4
  inc eax
  cmp bl,0
  jnz L0
  xor eax,eax
  ret
L3:
  xor eax,eax
  inc eax
  ret
L4:
  xor eax,eax
  dec eax
  ret
ENDP strcmp

.COMMENT ** Function substrcmp: compares a sub string with the begin of another string **

PROC substrcmp  ; sub strings equal: EAX=0, string ESI>EDI EAX=1, string ESI<EDI EAX=-1
                ; substring in EDI, string in ESI
                ; pascal equivalent copy(ESI,1,length(EDI)) = EDI
  xor eax,eax
L0:
  mov bl, [edi+eax]
  cmp bl,0
  je short L2
  cmp [esi+eax], bl
  jb SHORT L3
  ja SHORT L4
  inc eax
  jmp L0
L2:
  xor eax,eax
  ret
L3:
  xor eax,eax
  inc eax
  ret
L4:
  xor eax,eax
  dec eax
  ret
ENDP substrcmp
