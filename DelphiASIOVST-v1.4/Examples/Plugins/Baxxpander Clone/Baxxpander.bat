@"..\..\..\Bin\Win32\madExceptPatch.exe" "..\..\..\Bin\Win32\VST\Baxxpander.dll " .\Baxxpander.mes
@if exist "..\..\..\Bin\Win32\VST\Baxxpander.dll" @move "..\..\..\Bin\Win32\VST\Baxxpander.dll" "..\..\..\Bin\Win32\VST\Baxxpander Clone.dll"
@if exist "..\..\..\Bin\Win64\VST\Baxxpander.dll" @move "..\..\..\Bin\Win64\VST\Baxxpander.dll" "..\..\..\Bin\Win64\VST\Baxxpander Clone.dll"
@if not exist "..\..\..\Bin\Win32\VST\Baxxpander Clone.dll" goto Error
@if not exist "..\..\..\Bin\Win64\VST\Baxxpander Clone.dll" goto Error
@"..\..\..\Bin\Win32\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win32\VST\Baxxpander Clone.dll"
@"..\..\..\Bin\Win64\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win64\VST\Baxxpander Clone.dll"
@"..\..\..\Bin\Win32\VST-Plugin Screenshot Tool.exe" "..\..\..\Bin\Win32\VST\Baxxpander Clone.dll" "..\..\..\Screenshots\Baxxpander Clone.png"
@7z a "..\..\..\Archive\BaxxpanderClone.7z" "..\..\..\Bin\*\VST\Baxxpander Clone.dll" "..\..\..\Bin\License.txt"
@makensis /V2 "..\..\..\Install Scripts\Install Script Baxxpander Clone.nsi"
@IF ERRORLEVEL==1 GOTO Error
@if exist "..\..\..\Release Scripts\Baxxpander Clone.ftp" ftps -s:"..\..\..\Release Scripts\Baxxpander Clone.ftp"
@if exist "..\..\..\Release Scripts\Baxxpander Clone.scp" WinSCP -script="..\..\..\Release Scripts\Baxxpander Clone.scp"
@GOTO :EOF
:Error
@echo Script Error
@Pause
