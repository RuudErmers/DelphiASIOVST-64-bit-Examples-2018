;NSIS Modern User Interface version 1.70
;VST-Plugin Unit Test Installer
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
  Name "VST-Plugin Unit Test Installer"
  OutFile "VST_Plugin_Unit_Test_Install.exe"

  ;Default installation folder
  InstallDir "$PROGRAMFILES\VSTPlugIns"
  
  ;Get installation folder from registry if available
  InstallDirRegKey HKLM "SOFTWARE\VST" "VSTPluginsPath"

  BrandingText "Delphi ASIO && VST Project"

  ; Turn on the xp style of drawing
  XPStyle ON


;--------------------------------
;Interface Settings

  !define PRODUCT_NAME "VST-Plugin Unit Test"
  !define PRODUCT_VERSION "1.0.0"
  !define PRODUCT_PUBLISHER "Christian Budde"
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
;Pages

  !insertmacro MUI_PAGE_WELCOME
  !insertmacro MUI_PAGE_LICENSE "..\Bin\License.txt"
  !insertmacro MUI_PAGE_COMPONENTS
  !insertmacro MUI_PAGE_DIRECTORY
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

Section "VST-Plugin Unit Test" SecProgramFiles
  SetOutPath "$INSTDIR"
  
  !system 'copy "..\Bin\Win32\VSTPluginUnitTest.exe" "..\Bin\Win32\VST-Plugin Unit Test.exe"'  
  !system 'copy "..\Bin\Win64\VSTPluginUnitTest.exe" "..\Bin\Win64\VST-Plugin Unit Test.exe"'  

  ${If} ${RunningX64}
  File "..\Bin\Win64\VST-Plugin Unit Test.exe"
  ${Else}
  File "..\Bin\Win32\VST-Plugin Unit Test.exe"
  ${Endif}

  ;Store installation folder
  WriteRegStr HKLM "SOFTWARE\Delphi ASIO & VST Packages\${PRODUCT_NAME}" "" $INSTDIR
  
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall_VST-Plugin_Unit_Test.exe"
SectionEnd

Section "Manual" SecManual
  SetOutPath "$INSTDIR"
  
  File "..\Manuals\VST-Plugin Unit Test.pdf"

  ;Store installation folder
  WriteRegStr HKLM "SOFTWARE\Delphi ASIO & VST Packages\${PRODUCT_NAME}" "" $INSTDIR
  
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall_VST-Plugin_Unit_Test.exe"
SectionEnd


;--------------------------------
;Descriptions

  ;Language strings
  LangString TEXT_IO_TITLE ${LANG_ENGLISH} "InstallOptions page"
  LangString TEXT_IO_SUBTITLE ${LANG_ENGLISH} "VST-Plugin Unit Test"

  LangString DESC_SecProgramFiles ${LANG_ENGLISH} "VST-Plugin Unit Test"
  LangString DESC_SecManual ${LANG_ENGLISH} "VST-Plugin Unit Test (Manual)"

  ;Assign language strings to sections
  !insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
    !insertmacro MUI_DESCRIPTION_TEXT ${SecProgramFiles} $(DESC_SecProgramFiles)
    !insertmacro MUI_DESCRIPTION_TEXT ${SecManual} $(DESC_SecManual)
  !insertmacro MUI_FUNCTION_DESCRIPTION_END


;--------------------------------
;Uninstaller Section

Section "Uninstall"

  ;ADD YOUR OWN FILES HERE...
  Delete "$INSTDIR\VST-Plugin Unit Test.exe"
  Delete "$INSTDIR\VST-Plugin Unit Test.pdf"
  DeleteRegKey HKLM "SOFTWARE\Delphi ASIO & VST Packages\${PRODUCT_NAME}"

SectionEnd