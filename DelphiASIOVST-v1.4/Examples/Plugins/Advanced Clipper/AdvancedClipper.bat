@"..\..\..\Bin\Win32\madExceptPatch.exe" "..\..\..\Bin\Win32\VST\AdvancedClipper.dll " .\AdvancedClipper.mes
@if exist "..\..\..\Bin\Win32\VST\AdvancedClipper.dll" @move "..\..\..\Bin\Win32\VST\AdvancedClipper.dll" "..\..\..\Bin\Win32\VST\Advanced Clipper.dll"
@if exist "..\..\..\Bin\Win64\VST\AdvancedClipper.dll" @move "..\..\..\Bin\Win64\VST\AdvancedClipper.dll" "..\..\..\Bin\Win64\VST\Advanced Clipper.dll"
@if not exist "..\..\..\Bin\Win32\VST\Advanced Clipper.dll" goto Error
@if not exist "..\..\..\Bin\Win64\VST\Advanced Clipper.dll" goto Error
@"..\..\..\Bin\Win32\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win32\VST\Advanced Clipper.dll"
@"..\..\..\Bin\Win64\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win64\VST\Advanced Clipper.dll"
@"..\..\..\Bin\Win32\VST-Plugin Screenshot Tool.exe" "..\..\..\Bin\Win32\VST\Advanced Clipper.dll" "..\..\..\Screenshots\Advanced Clipper.png"
@7z a "..\..\..\Archive\AdvancedClipper.7z" "..\..\..\Bin\*\VST\Advanced Clipper.dll" "..\..\..\Manuals\Advanced Clipper.pdf" "..\..\..\Bin\License.txt"
@makensis /V2 "..\..\..\Install Scripts\Install Script Advanced Clipper.nsi"
@IF ERRORLEVEL==1 GOTO Error
@if exist "..\..\..\Release Scripts\Advanced Clipper.ftp" ftps -s:"..\..\..\Release Scripts\Advanced Clipper.ftp"
@if exist "..\..\..\Release Scripts\Advanced Clipper.scp" WinSCP -script="..\..\..\Release Scripts\Advanced Clipper.scp"
@GOTO :EOF
:Error
@echo Script Error
@Pause
