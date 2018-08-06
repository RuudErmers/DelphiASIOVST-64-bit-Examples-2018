@"..\..\..\..\Bin\Win32\madExceptPatch.exe" "..\..\..\..\Bin\Win32\VST\NoiseReduction.dll " .\NoiseReduction.mes
@if exist "..\..\..\..\Bin\Win32\VST\NoiseReduction.dll" @move "..\..\..\..\Bin\Win32\VST\NoiseReduction.dll" "..\..\..\..\Bin\Win32\VST\Noise Reduction.dll"
@if exist "..\..\..\..\Bin\Win64\VST\NoiseReduction.dll" @move "..\..\..\..\Bin\Win64\VST\NoiseReduction.dll" "..\..\..\..\Bin\Win64\VST\Noise Reduction.dll"
@if not exist "..\..\..\..\Bin\Win32\VST\Noise Reduction.dll" goto Error
@if not exist "..\..\..\..\Bin\Win64\VST\Noise Reduction.dll" goto Error
@"..\..\..\..\Bin\Win32\VST-Plugin Unit Test (command line).exe" "..\..\..\..\Bin\Win32\VST\Noise Reduction.dll"
@"..\..\..\..\Bin\Win64\VST-Plugin Unit Test (command line).exe" "..\..\..\..\Bin\Win64\VST\Noise Reduction.dll"
@"..\..\..\..\Bin\Win32\VST-Plugin Screenshot Tool.exe" "..\..\..\..\Bin\Win32\VST\Noise Reduction.dll" "..\..\..\..\Screenshots\Noise Reduction.png"
@7z a "..\..\..\..\Archive\NoiseReduction.7z" "..\..\..\..\Bin\*\VST\Noise Reduction.dll" "..\..\..\..\Manuals\Noise Reduction.pdf" "..\..\..\..\Bin\License.txt"
@makensis /V2 "..\..\..\..\Install Scripts\Install Script Noise Reduction.nsi"
@IF ERRORLEVEL==1 GOTO Error
@if exist "..\..\..\..\Release Scripts\Noise Reduction.ftp" ftps -s:"..\..\..\..\Release Scripts\Noise Reduction.ftp"
@if exist "..\..\..\..\Release Scripts\Noise Reduction.scp" WinSCP -script="..\..\..\..\Release Scripts\Noise Reduction.scp"
@GOTO :EOF
:Error
@echo Script Error
@Pause
