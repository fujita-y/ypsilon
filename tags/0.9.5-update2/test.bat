@echo off
setlocal
if exist .\build-win32\Release\Ypsilon.exe goto inplace
echo ----------------------------------------
echo r4rstest.scm:
"c:\program files\Ypsilon\Ypsilon.exe" --acc=%TEMP% --sitelib=./test;./sitelib;./stdlib ./test/r4rstest.scm
if errorlevel 1 goto end
echo ----------------------------------------
echo tspl.scm:
"c:\program files\Ypsilon\Ypsilon.exe" --acc=%TEMP% --sitelib=./test;./sitelib;./stdlib ./test/tspl.scm
if errorlevel 1 goto end
echo ----------------------------------------
echo arith.scm:
"c:\program files\Ypsilon\Ypsilon.exe" --acc=%TEMP% --sitelib=./test;./sitelib;./stdlib ./test/arith.scm
if errorlevel 1 goto end
echo ----------------------------------------
echo r5rs_pitfall.scm:
"c:\program files\Ypsilon\Ypsilon.exe" --acc=%TEMP% --sitelib=./test;./sitelib;./stdlib ./test/r5rs_pitfall.scm
if errorlevel 1 goto end
echo ----------------------------------------
echo syntax-rule-stress-test.scm:
"c:\program files\Ypsilon\Ypsilon.exe" --acc=%TEMP% --sitelib=./test;./sitelib;./stdlib ./test/syntax-rule-stress-test.scm
if errorlevel 1 goto end
echo ----------------------------------------
echo r6rs.scm:
"c:\program files\Ypsilon\Ypsilon.exe" --acc=%TEMP% --sitelib=./test;./sitelib;./stdlib ./test/r6rs.scm
if errorlevel 1 goto end
echo ----------------------------------------
echo r6rs-lib.scm:
"c:\program files\Ypsilon\Ypsilon.exe" --acc=%TEMP% --sitelib=./test;./sitelib;./stdlib ./test/r6rs-lib.scm
if errorlevel 1 goto end
echo ----------------------------------------
echo Passed all tests
goto end
:inplace
echo ----------------------------------------
echo r4rstest.scm:
.\build-win32\Release\Ypsilon.exe --acc=%TEMP% --sitelib=./test;./sitelib;./stdlib ./test/r4rstest.scm
if errorlevel 1 goto end
echo ----------------------------------------
echo tspl.scm:
.\build-win32\Release\Ypsilon.exe --acc=%TEMP% --sitelib=./test;./sitelib;./stdlib ./test/tspl.scm
if errorlevel 1 goto end
echo ----------------------------------------
echo arith.scm:
.\build-win32\Release\Ypsilon.exe --acc=%TEMP% --sitelib=./test;./sitelib;./stdlib ./test/arith.scm
if errorlevel 1 goto end
echo ----------------------------------------
echo r5rs_pitfall.scm:
.\build-win32\Release\Ypsilon.exe --acc=%TEMP% --sitelib=./test;./sitelib;./stdlib ./test/r5rs_pitfall.scm
if errorlevel 1 goto end
echo ----------------------------------------
echo syntax-rule-stress-test.scm:
.\build-win32\Release\Ypsilon.exe --acc=%TEMP% --sitelib=./test;./sitelib;./stdlib ./test/syntax-rule-stress-test.scm
if errorlevel 1 goto end
echo ----------------------------------------
echo r6rs.scm:
.\build-win32\Release\Ypsilon.exe --acc=%TEMP% --sitelib=./test;./sitelib;./stdlib ./test/r6rs.scm
if errorlevel 1 goto end
echo ----------------------------------------
echo r6rs-lib.scm:
.\build-win32\Release\Ypsilon.exe --acc=%TEMP% --sitelib=./test;./sitelib;./stdlib ./test/r6rs-lib.scm
if errorlevel 1 goto end
echo ----------------------------------------
echo Passed all tests
:end