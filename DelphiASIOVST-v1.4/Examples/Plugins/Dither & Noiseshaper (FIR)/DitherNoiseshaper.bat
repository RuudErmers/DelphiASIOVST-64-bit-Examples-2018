@"..\..\..\Bin\Win32\madExceptPatch.exe" "..\..\..\Bin\Win32\VST\DitherNoiseshaper.dll " .\DitherNoiseshaper.mes
@if exist "..\..\..\Bin\Win32\VST\DitherNoiseshaper.dll" @move "..\..\..\Bin\Win32\VST\DitherNoiseshaper.dll" "..\..\..\Bin\Win32\VST\Dither & Noiseshaper (FIR).dll"
@if exist "..\..\..\Bin\Win64\VST\DitherNoiseshaper.dll" @move "..\..\..\Bin\Win64\VST\DitherNoiseshaper.dll" "..\..\..\Bin\Win64\VST\Dither & Noiseshaper (FIR).dll"
@if not exist "..\..\..\Bin\Win32\VST\Dither & Noiseshaper (FIR).dll" goto Error
@if not exist "..\..\..\Bin\Win64\VST\Dither & Noiseshaper (FIR).dll" goto Error
@"..\..\..\Bin\Win32\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win32\VST\Dither & Noiseshaper (FIR).dll"
@"..\..\..\Bin\Win64\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win64\VST\Dither & Noiseshaper (FIR).dll"
@"..\..\..\Bin\Win32\VST-Plugin Screenshot Tool.exe" "..\..\..\Bin\Win32\VST\Dither & Noiseshaper (FIR).dll" "..\..\..\Screenshots\Dither & Noiseshaper (FIR).png"
@7z a "..\..\..\Archive\Dither&Noiseshaper(FIR).7z" "..\..\..\Bin\*\VST\Dither & Noiseshaper (FIR).dll" "..\..\..\Bin\License.txt"
@makensis /V2 "..\..\..\Install Scripts\Install Script Dither & Noiseshaper (FIR).nsi"
@IF ERRORLEVEL==1 GOTO Error
@if exist "..\..\..\Release Scripts\Dither & Noiseshaper (FIR).ftp" ftps -s:"..\..\..\Release Scripts\Dither & Noiseshaper (FIR).ftp"
@if exist "..\..\..\Release Scripts\Dither & Noiseshaper (FIR).scp" WinSCP -script="..\..\..\Release Scripts\Dither & Noiseshaper (FIR).scp"
@GOTO :EOF
:Error
@echo Script Error
@Pause
