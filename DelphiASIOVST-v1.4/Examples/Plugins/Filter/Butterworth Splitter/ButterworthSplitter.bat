@"..\..\..\..\Bin\Win32\madExceptPatch.exe" "..\..\..\..\Bin\Win32\VST\ButterworthSplitter.dll " .\ButterworthSplitter.mes
@if exist "..\..\..\..\Bin\Win32\VST\ButterworthSplitter.dll" @move "..\..\..\..\Bin\Win32\VST\ButterworthSplitter.dll" "..\..\..\..\Bin\Win32\VST\Butterworth Splitter.dll"
@if exist "..\..\..\..\Bin\Win64\VST\ButterworthSplitter.dll" @move "..\..\..\..\Bin\Win64\VST\ButterworthSplitter.dll" "..\..\..\..\Bin\Win64\VST\Butterworth Splitter.dll"
@if not exist "..\..\..\..\Bin\Win32\VST\Butterworth Splitter.dll" goto Error
@if not exist "..\..\..\..\Bin\Win64\VST\Butterworth Splitter.dll" goto Error
@"..\..\..\..\Bin\Win32\VST-Plugin Unit Test (command line).exe" "..\..\..\..\Bin\Win32\VST\Butterworth Splitter.dll"
@"..\..\..\..\Bin\Win64\VST-Plugin Unit Test (command line).exe" "..\..\..\..\Bin\Win64\VST\Butterworth Splitter.dll"
@"..\..\..\..\Bin\Win32\VST-Plugin Screenshot Tool.exe" "..\..\..\..\Bin\Win32\VST\Butterworth Splitter.dll" "..\..\..\..\Screenshots\Butterworth Splitter.png"
@7z a "..\..\..\..\Archive\ButterworthSplitter.7z" "..\..\..\..\Bin\*\VST\Butterworth Splitter.dll" "..\..\..\..\Bin\License.txt"
@makensis /V2 "..\..\..\..\Install Scripts\Install Script Butterworth Splitter.nsi"
@IF ERRORLEVEL==1 GOTO Error
@if exist "..\..\..\..\Release Scripts\Butterworth Splitter.ftp" ftps -s:"..\..\..\..\Release Scripts\Butterworth Splitter.ftp"
@if exist "..\..\..\..\Release Scripts\Butterworth Splitter.scp" WinSCP -script="..\..\..\..\Release Scripts\Butterworth Splitter.scp"
@GOTO :EOF
:Error
@echo Script Error
@Pause
