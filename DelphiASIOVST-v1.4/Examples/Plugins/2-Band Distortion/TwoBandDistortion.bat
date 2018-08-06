@"..\..\..\Bin\Win32\madExceptPatch.exe" "..\..\..\Bin\Win32\VST\TwoBandDistortion.dll " .\TwoBandDistortion.mes
@if exist "..\..\..\Bin\Win32\VST\TwoBandDistortion.dll" @move "..\..\..\Bin\Win32\VST\TwoBandDistortion.dll" "..\..\..\Bin\Win32\VST\2-Band Distortion.dll"
@if exist "..\..\..\Bin\Win64\VST\TwoBandDistortion.dll" @move "..\..\..\Bin\Win64\VST\TwoBandDistortion.dll" "..\..\..\Bin\Win64\VST\2-Band Distortion.dll"
@if not exist "..\..\..\Bin\Win32\VST\2-Band Distortion.dll" goto Error
@if not exist "..\..\..\Bin\Win64\VST\2-Band Distortion.dll" goto Error
@"..\..\..\Bin\Win32\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win32\VST\2-Band Distortion.dll"
@"..\..\..\Bin\Win64\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win64\VST\2-Band Distortion.dll"
@"..\..\..\Bin\Win32\VST-Plugin Screenshot Tool.exe" "..\..\..\Bin\Win32\VST\2-Band Distortion.dll" "..\..\..\Screenshots\2-Band Distortion.png"
@7z a "..\..\..\Archive\2BandDistortion.7z" "..\..\..\Bin\*\VST\2-Band Distortion.dll" "..\..\..\Manuals\2-Band Distortion.pdf" "..\..\..\Bin\License.txt"
@makensis /V2 "..\..\..\Install Scripts\Install Script 2-Band Distortion.nsi"
@IF ERRORLEVEL==1 GOTO Error
@if exist "..\..\..\Release Scripts\2-Band Distortion.ftp" ftps -s:"..\..\..\Release Scripts\2-Band Distortion.ftp"
@if exist "..\..\..\Release Scripts\2-Band Distortion.scp" WinSCP -script="..\..\..\Release Scripts\2-Band Distortion.scp"
@GOTO :EOF
:Error
@echo Script Error
@Pause
