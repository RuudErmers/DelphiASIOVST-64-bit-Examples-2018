;NSIS Modern User Interface version 1.70
;Effect Bundle Installer
;Written by Christian Budde

SetCompressor lzma

;--------------------------------
;Include Modern UI
;  !include "Sections.nsh"
  !include "MUI.nsh"
  !include "x64.nsh"


;--------------------------------
;General

  ;Name and file
  Name "Effect Bundle Installer"
  OutFile "DAV_Effect_Bundle_Install.exe"

  ;Default installation folder
  InstallDir "$VSTPlugins\VSTPlugIns"
  
  ;Get installation folder from registry if available
  InstallDirRegKey HKLM "SOFTWARE\VST" "VSTPluginsPath"

  BrandingText "Delphi ASIO && VST Project"

  ; Turn on the xp style of drawing
  XPStyle ON


;--------------------------------
;Variables

  Var BugReportState


;--------------------------------
;Interface Settings

  !define PRODUCT_NAME "EffectBundle"
  !define PRODUCT_VERSION "1.0.0"
  !define PRODUCT_PUBLISHER "Christian-W. Budde"
  !define PRODUCT_WEB_SITE "http://delphiasiovst.sourceforge.net/"
  !define PRODUCT_DIR_REGKEY "Software\Delphi ASIO & VST Packages\${PRODUCT_NAME}"
  !define PRODUCT_DIR_ROOT_KEY "HKLM"
  !define PRODUCT_UNINST_KEY "Software\Delphi ASIO & VST Packages\Uninstall\${PRODUCT_NAME}"
  !define PRODUCT_UNINST_ROOT_KEY "HKLM"
  !define MUI_ABORTWARNING


;--------------------------------
;Language Selection Dialog Settings

  ;Remember the installer language
  !define MUI_LANGDLL_REGISTRY_ROOT "HKLM" 
  !define MUI_LANGDLL_REGISTRY_KEY "SOFTWARE\Delphi ASIO & VST Packages\${PRODUCT_NAME}"
  !define MUI_LANGDLL_REGISTRY_VALUENAME "Installer Language"


;--------------------------------
;Reserve Files
  
  ;These files should be inserted before other files in the data block
  ;Keep these lines before any File command
  ;Only for solid compression (by default, solid compression is enabled for BZIP2 and LZMA)
  
    ReserveFile "madExcept Patch.dll"
    ReserveFile "ioBugReport.ini"
  !insertmacro MUI_RESERVEFILE_INSTALLOPTIONS
;  !insertmacro MUI_RESERVEFILE_LANGDLL


;--------------------------------
;Installer Functions

Function .onInit

;  !insertmacro MUI_LANGDLL_DISPLAY  
  !insertmacro MUI_INSTALLOPTIONS_EXTRACT "ioBugReport.ini"

FunctionEnd


;--------------------------------
;Pages

  !insertmacro MUI_PAGE_WELCOME
  !insertmacro MUI_PAGE_LICENSE "..\Bin\License.txt"
  !insertmacro MUI_PAGE_COMPONENTS
  !insertmacro MUI_PAGE_DIRECTORY
  Page custom BugReportPatch
  !insertmacro MUI_PAGE_INSTFILES
  !insertmacro MUI_PAGE_FINISH
  !insertmacro MUI_UNPAGE_WELCOME
  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES

;--------------------------------
;Languages
 
  !insertmacro MUI_LANGUAGE "English"
;  !insertmacro MUI_LANGUAGE "German"


;--------------------------------
;Installer Sections

Section "Dynamic Processors" SecDynamics
  SetOutPath "$INSTDIR"
  
  ;ADD YOUR OWN FILES HERE...
  File "..\Bin\EnhancedGate.dll"
  File "..\Bin\LA1701.dll"
  File "..\Bin\MultibandCompressor.dll"
  File "..\Bin\SimpleCompressor.dll"
  File "..\Bin\SimpleCompressor2.dll"
  File "..\Bin\SimpleFeedbackCompressor.dll"
  File "..\Bin\SimpleGate.dll"
  File "..\Bin\SimpleLimiter.dll"
  File "..\Bin\SKFBLimiter.dll"
  File "..\Bin\SoftKneeFeedbackCompressor.dll"
  File "..\Bin\SoftKneeLimiter.dll"
  File "..\Bin\SoftKneeMultibandLimiter.dll"

  ;Store installation folder
  WriteRegStr HKLM "SOFTWARE\Delphi ASIO & VST Packages\${PRODUCT_NAME}" "" $INSTDIR
  
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall_EffectBundle.exe"
SectionEnd

Section "Filters" SecFilters
  SetOutPath "$INSTDIR"
  
  ;ADD YOUR OWN FILES HERE...
  File "..\Bin\BugpassLite.dll"
  File "..\Bin\ButterworthHP.dll"
  File "..\Bin\ButterworthLP.dll"
  File "..\Bin\ChebyshevHP.dll"
  File "..\Bin\ChebyshevLP.dll"
  File "..\Bin\GraphicEQ.dll"
  File "..\Bin\LinearPhaseHP.dll"
  File "..\Bin\LinearPhaseLP.dll"
  File "..\Bin\UniQuE.dll"
  File "..\Bin\Valueable.dll"
  File "..\Bin\ValueableStereo.dll"

  ;Store installation folder
  WriteRegStr HKLM "SOFTWARE\Delphi ASIO & VST Packages\${PRODUCT_NAME}" "" $INSTDIR
  
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall_EffectBundle.exe"
SectionEnd

Section "VSTi" SecSynths
  SetOutPath "$INSTDIR"
  
  ;ADD YOUR OWN FILES HERE...
  File "..\Bin\SimpleSampler.dll"
  File "..\Bin\SineSynth.dll"
  File "..\Bin\Vocoder.dll"
  File "..\Bin\XSynth.dll"

  ;Store installation folder
  WriteRegStr HKLM "SOFTWARE\Delphi ASIO & VST Packages\${PRODUCT_NAME}" "" $INSTDIR
  
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall_EffectBundle.exe"
SectionEnd

Section "Misc. VST-Plugins" SecMisc
  SetOutPath "$INSTDIR"
  
  !system 'copy "..\Bin\ASIOVST.dll" "..\Bin\VST interfaced ASIO-Host.dll"'  

  ;ADD YOUR OWN FILES HERE...
  File "..\Bin\AdvancedClipper.dll"
  File "..\Bin\AmpSim.dll"
  File "..\Bin\VST interfaced ASIO-Host.dll"
  File "..\Bin\BassExtender.dll"
  File "..\Bin\ChebyshevWaveshaper.dll"
  File "..\Bin\Convolution.dll"
  File "..\Bin\CrossoverDistortion.dll"
  File "..\Bin\Decimator.dll"
  File "..\Bin\Delayla.dll"
  File "..\Bin\DitherNoiseshaper.dll"
  File "..\Bin\Exciter.dll"
  File "..\Bin\fReeverb.dll"
  File "..\Bin\MIDIPlugIn.dll"
  File "..\Bin\OpAmp.dll"
  File "..\Bin\OversampledTanh.dll"
  File "..\Bin\Phaser.dll"
  File "..\Bin\SimpleChorus.dll"
  File "..\Bin\SpinBugLite.dll"
  File "..\Bin\Tetris.dll"
  File "..\Bin\VSTPascalScript.dll"
  File "..\Bin\VUMeter.dll"

  ;Store installation folder
  WriteRegStr HKLM "SOFTWARE\Delphi ASIO & VST Packages\${PRODUCT_NAME}" "" $INSTDIR
  
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall_EffectBundle.exe"
SectionEnd


;--------------------------------
;Installer Functions

Function BugReportPatch
  ${If} ${SectionIsSelected} ${SecVSTPlugin}
  Goto IsVST
  ${EndIf}
  Goto NoVST

  IsVST:
  !insertmacro MUI_HEADER_TEXT "$(TEXT_IO_TITLE)" "$(TEXT_IO_SUBTITLE)"
  !insertmacro MUI_INSTALLOPTIONS_DISPLAY "ioBugReport.ini"

  NoVST:
FunctionEnd


;--------------------------------
;Descriptions

  ;Language strings
  LangString TEXT_IO_TITLE ${LANG_ENGLISH} "InstallOptions page"
  LangString TEXT_IO_SUBTITLE ${LANG_ENGLISH} "EffectBundle VST Plugin"

  LangString DESC_SecDynamics ${LANG_ENGLISH} "Dynamic Processors"
  LangString DESC_SecFilters ${LANG_ENGLISH} "Filters"
  LangString DESC_SecMisc ${LANG_ENGLISH} "Misc. VST Plugins"
  LangString DESC_SecSynths ${LANG_ENGLISH} "VSTi Instruments"
 
  ;Assign language strings to sections
  !insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
    !insertmacro MUI_DESCRIPTION_TEXT ${SecDynamics} $(DESC_SecDynamics)
    !insertmacro MUI_DESCRIPTION_TEXT ${SecFilters} $(DESC_SecFilters)
    !insertmacro MUI_DESCRIPTION_TEXT ${SecSynths} $(DESC_SecSynths)
    !insertmacro MUI_DESCRIPTION_TEXT ${SecMisc} $(DESC_SecMisc)
  !insertmacro MUI_FUNCTION_DESCRIPTION_END


;--------------------------------
;Uninstaller Section

Section "Uninstall"

  ;ADD YOUR OWN FILES HERE...
  Delete "$INSTDIR\AmpSim.dll"
  Delete "$INSTDIR\VST interfaced ASIO-Host.dll"
  Delete "$INSTDIR\BassExtender.dll"
  Delete "$INSTDIR\BugpassLite.dll"
  Delete "$INSTDIR\ButterworthHP.dll"
  Delete "$INSTDIR\ButterworthLP.dll"
  Delete "$INSTDIR\ChebyshevHP.dll"
  Delete "$INSTDIR\ChebyshevLP.dll"
  Delete "$INSTDIR\ChebyshevWaveshaper.dll"
  Delete "$INSTDIR\Convolution.dll"
  Delete "$INSTDIR\CrossoverDistortion.dll"
  Delete "$INSTDIR\Decimator.dll"
  Delete "$INSTDIR\Delayla.dll"
  Delete "$INSTDIR\DitherNoiseshaper.dll"
  Delete "$INSTDIR\EnhancedGate.dll"
  Delete "$INSTDIR\Exciter.dll"
  Delete "$INSTDIR\fReeverb.dll"
  Delete "$INSTDIR\GraphicEQ.dll"
  Delete "$INSTDIR\LA1701.dll"
  Delete "$INSTDIR\LinearPhaseHP.dll"
  Delete "$INSTDIR\LinearPhaseLP.dll"
  Delete "$INSTDIR\MIDIPlugIn.dll"
  Delete "$INSTDIR\MultibandCompressor.dll"
  Delete "$INSTDIR\OpAmp.dll"
  Delete "$INSTDIR\OversampledTanh.dll"
  Delete "$INSTDIR\Phaser.dll"
  Delete "$INSTDIR\SimpleChorus.dll"
  Delete "$INSTDIR\SimpleCompressor.dll"
  Delete "$INSTDIR\SimpleCompressor2.dll"
  Delete "$INSTDIR\SimpleFeedbackCompressor.dll"
  Delete "$INSTDIR\SimpleGate.dll"
  Delete "$INSTDIR\SimpleLimiter.dll"
  Delete "$INSTDIR\SimpleSampler.dll"
  Delete "$INSTDIR\SineSynth.dll"
  Delete "$INSTDIR\SKFBLimiter.dll"
  Delete "$INSTDIR\SoftKneeFeedbackCompressor.dll"
  Delete "$INSTDIR\SoftKneeLimiter.dll"
  Delete "$INSTDIR\SoftKneeMultibandLimiter.dll"
  Delete "$INSTDIR\SpinBugLite.dll"
  Delete "$INSTDIR\Tetris.dll"
  Delete "$INSTDIR\UniQuE.dll"
  Delete "$INSTDIR\Valueable.dll"
  Delete "$INSTDIR\ValueableStereo.dll"
  Delete "$INSTDIR\Vocoder.dll"
  Delete "$INSTDIR\VSTPascalScript.dll"
  Delete "$INSTDIR\VUMeter.dll"
  Delete "$INSTDIR\XSynth.dll"
  DeleteRegKey HKLM "SOFTWARE\Delphi ASIO & VST Packages\${PRODUCT_NAME}"

SectionEnd