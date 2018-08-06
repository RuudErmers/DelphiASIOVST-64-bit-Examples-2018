@"..\..\..\Bin\Win32\madExceptPatch.exe" "..\..\..\Bin\Win32\VST\BassExtender.dll " .\BassExtender.mes
@if exist "..\..\..\Bin\Win32\VST\BassExtender.dll" @move "..\..\..\Bin\Win32\VST\BassExtender.dll" "..\..\..\Bin\Win32\VST\Bass Extender.dll"
@if exist "..\..\..\Bin\Win64\VST\BassExtender.dll" @move "..\..\..\Bin\Win64\VST\BassExtender.dll" "..\..\..\Bin\Win64\VST\Bass Extender.dll"
@if not exist "..\..\..\Bin\Win32\VST\Bass Extender.dll" goto Error
@if not exist "..\..\..\Bin\Win64\VST\Bass Extender.dll" goto Error
@"..\..\..\Bin\Win32\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win32\VST\Bass Extender.dll"
@"..\..\..\Bin\Win64\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win64\VST\Bass Extender.dll"
@"..\..\..\Bin\Win32\VST-Plugin Screenshot Tool.exe" "..\..\..\Bin\Win32\VST\Bass Extender.dll" "..\..\..\Screenshots\Bass Extender.png"
@7z a "..\..\..\Archive\BassExtender.7z" "..\..\..\Bin\*\VST\Bass Extender.dll" "..\..\..\Bin\License.txt"
@makensis /V2 "..\..\..\Install Scripts\Install Script Bass Extender.nsi"
@IF ERRORLEVEL==1 GOTO Error
@if exist "..\..\..\Release Scripts\Bass Extender.ftp" ftps -s:"..\..\..\Release Scripts\Bass Extender.ftp"
@if exist "..\..\..\Release Scripts\Bass Extender.scp" WinSCP -script="..\..\..\Release Scripts\Bass Extender.scp"
@GOTO :EOF
:Error
@echo Script Error
@Pause
