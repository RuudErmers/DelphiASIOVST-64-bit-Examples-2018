@"..\..\..\Bin\Win32\madExceptPatch.exe" "..\..\..\Bin\Win32\VST\ChebyshevExciter.dll " .\ChebyshevExciter.mes
@if exist "..\..\..\Bin\Win32\VST\ChebyshevExciter.dll" @move "..\..\..\Bin\Win32\VST\ChebyshevExciter.dll" "..\..\..\Bin\Win32\VST\Chebyshev Exciter.dll"
@if exist "..\..\..\Bin\Win64\VST\ChebyshevExciter.dll" @move "..\..\..\Bin\Win64\VST\ChebyshevExciter.dll" "..\..\..\Bin\Win64\VST\Chebyshev Exciter.dll"
@if not exist "..\..\..\Bin\Win32\VST\Chebyshev Exciter.dll" goto Error
@if not exist "..\..\..\Bin\Win64\VST\Chebyshev Exciter.dll" goto Error
@"..\..\..\Bin\Win32\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win32\VST\Chebyshev Exciter.dll"
@"..\..\..\Bin\Win64\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win64\VST\Chebyshev Exciter.dll"
@"..\..\..\Bin\Win32\VST-Plugin Screenshot Tool.exe" "..\..\..\Bin\Win32\VST\Chebyshev Exciter.dll" "..\..\..\Screenshots\Chebyshev Exciter.png"
@7z a "..\..\..\Archive\ChebyshevExciter.7z" "..\..\..\Bin\*\VST\Chebyshev Exciter.dll" "..\..\..\Manuals\Chebyshev Exciter.pdf" "..\..\..\Bin\License.txt"
@makensis /V2 "..\..\..\Install Scripts\Install Script Chebyshev Exciter.nsi"
@IF ERRORLEVEL==1 GOTO Error
@if exist "..\..\..\Release Scripts\Chebyshev Exciter.ftp" ftps -s:"..\..\..\Release Scripts\Chebyshev Exciter.ftp"
@if exist "..\..\..\Release Scripts\Chebyshev Exciter.scp" WinSCP -script="..\..\..\Release Scripts\Chebyshev Exciter.scp"
@GOTO :EOF
:Error
@echo Script Error
@Pause
