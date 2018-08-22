cd %2
.\tasm.exe %1 %1 %1
.\tlink.exe %1,,,system
rem del %1.asm
del %1.lst
del %1.map
del %1.obj
move %1.exe ..\exe\%1.exe
Pause
..\exe\%1.exe
rem pause

