@"..\..\..\Bin\Win32\madExceptPatch.exe" "..\..\..\Bin\Win32\VST\BassBaron.dll " .\BassBaron.mes
@if exist "..\..\..\Bin\Win32\VST\BassBaron.dll" @move "..\..\..\Bin\Win32\VST\BassBaron.dll" "..\..\..\Bin\Win32\VST\Bass Baron.dll"
@if exist "..\..\..\Bin\Win64\VST\BassBaron.dll" @move "..\..\..\Bin\Win64\VST\BassBaron.dll" "..\..\..\Bin\Win64\VST\Bass Baron.dll"
@if not exist "..\..\..\Bin\Win32\VST\Bass Baron.dll" goto Error
@if not exist "..\..\..\Bin\Win64\VST\Bass Baron.dll" goto Error
@"..\..\..\Bin\Win32\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win32\VST\Bass Baron.dll"
@"..\..\..\Bin\Win64\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win64\VST\Bass Baron.dll"
@"..\..\..\Bin\Win32\VST-Plugin Screenshot Tool.exe" "..\..\..\Bin\Win32\VST\Bass Baron.dll" "..\..\..\Screenshots\Bass Baron.png"
@7z a "..\..\..\Archive\BassBaron.7z" "..\..\..\Bin\*\VST\Bass Baron.dll" "..\..\..\Bin\License.txt"
@makensis /V2 "..\..\..\Install Scripts\Install Script Bass Baron.nsi"
@IF ERRORLEVEL==1 GOTO Error
@if exist "..\..\..\Release Scripts\Bass Baron.ftp" ftps -s:"..\..\..\Release Scripts\Bass Baron.ftp"
@if exist "..\..\..\Release Scripts\Bass Baron.scp" WinSCP -script="..\..\..\Release Scripts\Bass Baron.scp"
@GOTO :EOF
:Error
@echo Script Error
@Pause
