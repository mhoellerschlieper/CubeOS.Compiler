L equ <LARGE>

;
; Define the external functions we will be linking to
;
extrn            BeginPaint:PROC
extrn            CreateWindowExA:PROC
extrn            DefWindowProcA:PROC
extrn            DispatchMessageA:PROC
extrn            EndPaint:PROC
extrn            ExitProcess:PROC
extrn            FindWindowA:PROC
extrn            GetMessageA:PROC
extrn            GetModuleHandleA:PROC
extrn            GetStockObject:PROC
extrn            InvalidateRect:PROC
extrn            LoadCursorA:PROC
extrn            LoadIconA:PROC
extrn            MessageBeep:PROC
extrn            MessageBoxA:PROC
extrn            PostQuitMessage:PROC
extrn            RegisterClassA:PROC
extrn            ShowWindow:PROC
extrn            SetWindowPos:PROC
extrn            TextOutA:PROC
extrn            TranslateMessage:PROC
extrn            UpdateWindow:PROC

;
; for Unicode support, Win32 remaps some functions to either the Ansi or
; Wide char versions.  We will assume Ansi for this example.
;
CreateWindowEx   equ <CreateWindowExA>
DefWindowProc    equ <DefWindowProcA>
DispatchMessage  equ <DispatchMessageA>
FindWindow       equ <FindWindowA>
GetMessage       equ <GetMessageA>
GetModuleHandle  equ <GetModuleHandleA>
LoadCursor       equ <LoadCursorA>
LoadIcon         equ <LoadIconA>
MessageBox       equ <MessageBoxA>
RegisterClass    equ <RegisterClassA>
TextOut          equ <TextOutA>

.code
;-----------------------------------------------------------------------------
;
; This is where control is received from the loader.
;
start:

        push    L 0
        call    GetModuleHandle         ; get hmod (in eax)
        mov     [hInst], eax            ; hInstance is same as HMODULE
                                        ; in the Win32 world

        push    L 0
        push    offset szClassName
        call    FindWindow
        or      eax,eax
        jz      reg_class

        mov     [zero], ' '             ; space to modify title string

reg_class:
;
; initialize the WndClass structure
;
        mov     [wc.clsStyle], CS_HREDRAW + CS_VREDRAW + CS_GLOBALCLASS
        mov     [wc.clsLpfnWndProc], offset WndProc
        mov     [wc.clsCbClsExtra], 0
        mov     [wc.clsCbWndExtra], 0

        mov     eax, [hInst]
        mov     [wc.clsHInstance], eax

        push    L IDI_APPLICATION
        push    L 0
        call    LoadIcon
        mov     [wc.clsHIcon], eax

        push    L IDC_ARROW
        push    L 0
        call    LoadCursor
        mov     [wc.clsHCursor], eax

        mov     [wc.clsHbrBackground], COLOR_WINDOW + 1
        mov     dword ptr [wc.clsLpszMenuName], 0
        mov     dword ptr [wc.clsLpszClassName], offset szClassName

        push    offset wc
        call    RegisterClass

        push    L 0                      ; lpParam
        push    [hInst]                  ; hInstance
        push    L 0                      ; menu
        push    L 0                      ; parent hwnd
        push    L CW_USEDEFAULT          ; height
        push    L CW_USEDEFAULT          ; width
        push    L CW_USEDEFAULT          ; y
        push    L CW_USEDEFAULT          ; x
        push    L WS_OVERLAPPEDWINDOW    ; Style
        push    offset szTitleName       ; Title string
        push    offset szClassName       ; Class name
        push    L 0                      ; extra style

        call    CreateWindowEx

        mov     [newhwnd], eax

        push    L SW_SHOWNORMAL
        push    [newhwnd]
        call    ShowWindow

        push    [newhwnd]
        call    UpdateWindow
        
msg_loop:
        push    L 0
        push    L 0
        push    L 0
        push    offset msg
        call    GetMessage

        cmp     ax, 0
        je      end_loop

        push    offset msg
        call    TranslateMessage

        push    offset msg
        call    DispatchMessage
        
        jmp     msg_loop

end_loop:
        push    [msg.msWPARAM]
        call    ExitProcess

        ; we never get to here

;-----------------------------------------------------------------------------
WndProc          proc uses ebx edi esi, hwnd:DWORD, wmsg:DWORD, wparam:DWORD, lparam:DWORD
;
; WARNING: Win32 requires that EBX, EDI, and ESI be preserved!  We comply
; with this by listing those regs after the 'uses' statement in the 'proc'
; line.  This allows the Assembler to save them for us.
;
        LOCAL   theDC:DWORD

        cmp     [wmsg], WM_DESTROY
        je      wmdestroy
        cmp     [wmsg], WM_RBUTTONDOWN
        je      wmrbuttondown
        cmp     [wmsg], WM_SIZE
        je      wmsize
        cmp     [wmsg], WM_CREATE
        je      wmcreate
        cmp     [wmsg], WM_LBUTTONDOWN
        je      wmlbuttondown
        cmp     [wmsg], WM_PAINT
        je      wmpaint
        cmp     [wmsg], WM_GETMINMAXINFO
        je      wmgetminmaxinfo


        jmp     defwndproc

wmpaint:
		Call 	$_Main
        mov     eax, 0
        jmp     finish

wmcreate:
	    
		push    L 0
        push    L 0
        push    [hwnd]
        call    InvalidateRect    ; repaint window
        
        
        mov     eax, 0
        jmp     finish

defwndproc:
        push    [lparam]
        push    [wparam]
        push    [wmsg]
        push    [hwnd]
        call    DefWindowProc
        
        jmp     finish

wmdestroy:
        push    L 0
        call    PostQuitMessage
        mov     eax, 0
        jmp     finish

wmlbuttondown:
        mov     eax, 0
        jmp     finish

wmrbuttondown:
        push    L 0
        call    MessageBeep
        jmp     finish

wmsize:
        mov     eax, 0
        jmp     finish

wmgetminmaxinfo:
        mov     ebx, [lparam]  ; ptr to minmaxinfo struct
        mov     [(MINMAXINFO ptr ebx).mintrackposition_x] , 350
        mov     [(MINMAXINFO ptr ebx).mintrackposition_y] , 60
        mov     eax, 0
        jmp     finish

finish:
        ret
WndProc          endp
