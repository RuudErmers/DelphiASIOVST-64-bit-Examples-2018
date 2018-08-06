@"..\..\..\Bin\Win32\madExceptPatch.exe" "..\..\..\Bin\Win32\VST\ChebyshevWaveshaper.dll " .\ChebyshevWaveshaper.mes
@if exist "..\..\..\Bin\Win32\VST\ChebyshevWaveshaper.dll" @move "..\..\..\Bin\Win32\VST\ChebyshevWaveshaper.dll" "..\..\..\Bin\Win32\VST\Chebyshev Waveshaper.dll"
@if exist "..\..\..\Bin\Win64\VST\ChebyshevWaveshaper.dll" @move "..\..\..\Bin\Win64\VST\ChebyshevWaveshaper.dll" "..\..\..\Bin\Win64\VST\Chebyshev Waveshaper.dll"
@if not exist "..\..\..\Bin\Win32\VST\Chebyshev Waveshaper.dll" goto Error
@if not exist "..\..\..\Bin\Win64\VST\Chebyshev Waveshaper.dll" goto Error
@"..\..\..\Bin\Win32\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win32\VST\Chebyshev Waveshaper.dll"
@"..\..\..\Bin\Win64\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win64\VST\Chebyshev Waveshaper.dll"
@"..\..\..\Bin\Win32\VST-Plugin Screenshot Tool.exe" "..\..\..\Bin\Win32\VST\Chebyshev Waveshaper.dll" "..\..\..\Screenshots\Chebyshev Waveshaper.png"
@7z a "..\..\..\Archive\ChebyshevWaveshaper.7z" "..\..\..\Bin\*\VST\Chebyshev Waveshaper.dll" "..\..\..\Bin\License.txt"
@makensis /V2 "..\..\..\Install Scripts\Install Script Chebyshev Waveshaper.nsi"
@IF ERRORLEVEL==1 GOTO Error
@if exist "..\..\..\Release Scripts\Chebyshev Waveshaper.ftp" ftps -s:"..\..\..\Release Scripts\Chebyshev Waveshaper.ftp"
@if exist "..\..\..\Release Scripts\Chebyshev Waveshaper.scp" WinSCP -script="..\..\..\Release Scripts\Chebyshev Waveshaper.scp"
@GOTO :EOF
:Error
@echo Script Error
@Pause
