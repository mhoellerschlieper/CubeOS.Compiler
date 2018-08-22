cd %2

tasm32 /ml %1.asm
tlink32 /Tpe /aa /c %1, %1,, .\lib\import32, LinkOpts.DEF

rem tasm32 /ml %1.asm
rem tlink32 /Tpe /aa /c %1, %1,, .\lib\import32, LinkOpts.DEF


rem del %1.asm
del %1.lst
del %1.map
del %1.obj
move %1.exe ..\exe\%1.exe
Pause
..\exe\%1.exe
rem pause

