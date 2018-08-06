@"..\..\..\Bin\Win32\madExceptPatch.exe" "..\..\..\Bin\Win32\VST\Chorus.dll " .\Chorus.mes
@if not exist "..\..\..\Bin\Win32\VST\Chorus.dll" goto Error
@if not exist "..\..\..\Bin\Win64\VST\Chorus.dll" goto Error
@"..\..\..\Bin\Win32\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win32\VST\Chorus.dll"
@"..\..\..\Bin\Win64\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win64\VST\Chorus.dll"
@"..\..\..\Bin\Win32\VST-Plugin Screenshot Tool.exe" "..\..\..\Bin\Win32\VST\Chorus.dll" "..\..\..\Screenshots\Chorus.png"
@7z a "..\..\..\Archive\Chorus.7z" "..\..\..\Bin\*\VST\Chorus.dll" "..\..\..\Bin\License.txt"
@makensis /V2 "..\..\..\Install Scripts\Install Script Chorus.nsi"
@IF ERRORLEVEL==1 GOTO Error
@if exist "..\..\..\Release Scripts\Chorus.ftp" ftps -s:"..\..\..\Release Scripts\Chorus.ftp"
@if exist "..\..\..\Release Scripts\Chorus.scp" WinSCP -script="..\..\..\Release Scripts\Chorus.scp"
@GOTO :EOF
:Error
@echo Script Error
@Pause
