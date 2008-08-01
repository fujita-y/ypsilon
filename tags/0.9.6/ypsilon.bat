@echo off
if exist "C:\Program files\Ypsilon\Ypsilon.exe" goto run
echo error: Ypsilon.exe not installed properly.
goto end
:run
"c:\program files\Ypsilon\Ypsilon.exe" %1 %2 %3 %4 %5 %6 %7 %8 %9
:end