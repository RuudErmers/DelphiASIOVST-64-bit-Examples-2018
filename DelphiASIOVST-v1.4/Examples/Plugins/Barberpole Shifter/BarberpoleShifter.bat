@"..\..\..\Bin\Win32\madExceptPatch.exe" "..\..\..\Bin\Win32\VST\BarberpoleShifter.dll " .\BarberpoleShifter.mes
@if exist "..\..\..\Bin\Win32\VST\BarberpoleShifter.dll" @move "..\..\..\Bin\Win32\VST\BarberpoleShifter.dll" "..\..\..\Bin\Win32\VST\Barberpole Shifter.dll"
@if exist "..\..\..\Bin\Win64\VST\BarberpoleShifter.dll" @move "..\..\..\Bin\Win64\VST\BarberpoleShifter.dll" "..\..\..\Bin\Win64\VST\Barberpole Shifter.dll"
@if not exist "..\..\..\Bin\Win32\VST\Barberpole Shifter.dll" goto Error
@if not exist "..\..\..\Bin\Win64\VST\Barberpole Shifter.dll" goto Error
@"..\..\..\Bin\Win32\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win32\VST\Barberpole Shifter.dll"
@"..\..\..\Bin\Win64\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win64\VST\Barberpole Shifter.dll"
@"..\..\..\Bin\Win32\VST-Plugin Screenshot Tool.exe" "..\..\..\Bin\Win32\VST\Barberpole Shifter.dll" "..\..\..\Screenshots\Barberpole Shifter.png"
@7z a "..\..\..\Archive\BarberpoleShifter.7z" "..\..\..\Bin\*\VST\Barberpole Shifter.dll" "..\..\..\Bin\License.txt"
@makensis /V2 "..\..\..\Install Scripts\Install Script Barberpole Shifter.nsi"
@IF ERRORLEVEL==1 GOTO Error
@if exist "..\..\..\Release Scripts\Barberpole Shifter.ftp" ftps -s:"..\..\..\Release Scripts\Barberpole Shifter.ftp"
@if exist "..\..\..\Release Scripts\Barberpole Shifter.scp" WinSCP -script="..\..\..\Release Scripts\Barberpole Shifter.scp"
@GOTO :EOF
:Error
@echo Script Error
@Pause
