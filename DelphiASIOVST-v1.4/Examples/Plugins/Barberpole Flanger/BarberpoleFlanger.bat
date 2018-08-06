@"..\..\..\Bin\Win32\madExceptPatch.exe" "..\..\..\Bin\Win32\VST\BarberpoleFlanger.dll " .\BarberpoleFlanger.mes
@if exist "..\..\..\Bin\Win32\VST\BarberpoleFlanger.dll" @move "..\..\..\Bin\Win32\VST\BarberpoleFlanger.dll" "..\..\..\Bin\Win32\VST\Barberpole Flanger.dll"
@if exist "..\..\..\Bin\Win64\VST\BarberpoleFlanger.dll" @move "..\..\..\Bin\Win64\VST\BarberpoleFlanger.dll" "..\..\..\Bin\Win64\VST\Barberpole Flanger.dll"
@if not exist "..\..\..\Bin\Win32\VST\Barberpole Flanger.dll" goto Error
@if not exist "..\..\..\Bin\Win64\VST\Barberpole Flanger.dll" goto Error
@"..\..\..\Bin\Win32\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win32\VST\Barberpole Flanger.dll"
@"..\..\..\Bin\Win64\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win64\VST\Barberpole Flanger.dll"
@"..\..\..\Bin\Win32\VST-Plugin Screenshot Tool.exe" "..\..\..\Bin\Win32\VST\Barberpole Flanger.dll" "..\..\..\Screenshots\Barberpole Flanger.png"
@7z a "..\..\..\Archive\BarberpoleFlanger.7z" "..\..\..\Bin\*\VST\Barberpole Flanger.dll" "..\..\..\Manuals\Barberpole Flanger.pdf" "..\..\..\Bin\License.txt"
@makensis /V2 "..\..\..\Install Scripts\Install Script Barberpole Flanger.nsi"
@IF ERRORLEVEL==1 GOTO Error
@if exist "..\..\..\Release Scripts\Barberpole Flanger.ftp" ftps -s:"..\..\..\Release Scripts\Barberpole Flanger.ftp"
@if exist "..\..\..\Release Scripts\Barberpole Flanger.scp" WinSCP -script="..\..\..\Release Scripts\Barberpole Flanger.scp"
@GOTO :EOF
:Error
@echo Script Error
@Pause
