@"..\..\..\Bin\Win32\madExceptPatch.exe" "..\..\..\Bin\Win32\VST\AudioAmelioration.dll " .\AudioAmelioration.mes
@if exist "..\..\..\Bin\Win32\VST\AudioAmelioration.dll" @move "..\..\..\Bin\Win32\VST\AudioAmelioration.dll" "..\..\..\Bin\Win32\VST\Audio Amelioration.dll"
@if exist "..\..\..\Bin\Win64\VST\AudioAmelioration.dll" @move "..\..\..\Bin\Win64\VST\AudioAmelioration.dll" "..\..\..\Bin\Win64\VST\Audio Amelioration.dll"
@if not exist "..\..\..\Bin\Win32\VST\Audio Amelioration.dll" goto Error
@if not exist "..\..\..\Bin\Win64\VST\Audio Amelioration.dll" goto Error
@"..\..\..\Bin\Win32\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win32\VST\Audio Amelioration.dll"
@"..\..\..\Bin\Win64\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win64\VST\Audio Amelioration.dll"
@"..\..\..\Bin\Win32\VST-Plugin Screenshot Tool.exe" "..\..\..\Bin\Win32\VST\Audio Amelioration.dll" "..\..\..\Screenshots\Audio Amelioration.png"
@7z a "..\..\..\Archive\AudioAmelioration.7z" "..\..\..\Bin\*\VST\Audio Amelioration.dll" "..\..\..\Bin\License.txt"
@makensis /V2 "..\..\..\Install Scripts\Install Script Audio Amelioration.nsi"
@IF ERRORLEVEL==1 GOTO Error
@if exist "..\..\..\Release Scripts\Audio Amelioration.ftp" ftps -s:"..\..\..\Release Scripts\Audio Amelioration.ftp"
@if exist "..\..\..\Release Scripts\Audio Amelioration.scp" WinSCP -script="..\..\..\Release Scripts\Audio Amelioration.scp"
@GOTO :EOF
:Error
@echo Script Error
@Pause
