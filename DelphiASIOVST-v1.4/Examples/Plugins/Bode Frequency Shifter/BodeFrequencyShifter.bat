@"..\..\..\Bin\Win32\madExceptPatch.exe" "..\..\..\Bin\Win32\VST\BodeFrequencyShifter.dll " .\BodeFrequencyShifter.mes
@if exist "..\..\..\Bin\Win32\VST\BodeFrequencyShifter.dll" @move "..\..\..\Bin\Win32\VST\BodeFrequencyShifter.dll" "..\..\..\Bin\Win32\VST\Bode Frequency Shifter.dll"
@if exist "..\..\..\Bin\Win64\VST\BodeFrequencyShifter.dll" @move "..\..\..\Bin\Win64\VST\BodeFrequencyShifter.dll" "..\..\..\Bin\Win64\VST\Bode Frequency Shifter.dll"
@if not exist "..\..\..\Bin\Win32\VST\Bode Frequency Shifter.dll" goto Error
@if not exist "..\..\..\Bin\Win64\VST\Bode Frequency Shifter.dll" goto Error
@"..\..\..\Bin\Win32\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win32\VST\Bode Frequency Shifter.dll"
@"..\..\..\Bin\Win64\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win64\VST\Bode Frequency Shifter.dll"
@"..\..\..\Bin\Win32\VST-Plugin Screenshot Tool.exe" "..\..\..\Bin\Win32\VST\Bode Frequency Shifter.dll" "..\..\..\Screenshots\Bode Frequency Shifter.png"
@7z a "..\..\..\Archive\BodeFrequencyShifter.7z" "..\..\..\Bin\*\VST\Bode Frequency Shifter.dll" "..\..\..\Bin\License.txt"
@makensis /V2 "..\..\..\Install Scripts\Install Script Bode Frequency Shifter.nsi"
@IF ERRORLEVEL==1 GOTO Error
@if exist "..\..\..\Release Scripts\Bode Frequency Shifter.ftp" ftps -s:"..\..\..\Release Scripts\Bode Frequency Shifter.ftp"
@if exist "..\..\..\Release Scripts\Bode Frequency Shifter.scp" WinSCP -script="..\..\..\Release Scripts\Bode Frequency Shifter.scp"
@GOTO :EOF
:Error
@echo Script Error
@Pause
