;NSIS Modern User Interface version 1.70
;ASIO-Host Driver Installer
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
  Name "ASIO-Host Driver"
  OutFile "ASIO-Host Driver_Install.exe"

  ;Default installation folder
  InstallDir "$SYSDIR"
  
  ;Get installation folder from registry if available
  InstallDirRegKey HKLM "SOFTWARE\VST" "VSTPluginsPath"

  BrandingText "Delphi ASIO && VST Project"

  ; Turn on the xp style of drawing
  XPStyle ON


;--------------------------------
;Interface Settings

  !define PRODUCT_NAME "ASIO-Host Driver"
  !define PRODUCT_VERSION "1.0.0"
  !define PRODUCT_PUBLISHER "Delphi ASIO & VST Package"
  !define PRODUCT_WEB_SITE "http://delphiasiovst.sourceforge.net"
  !define PRODUCT_DIR_REGKEY "Software\Delphi ASIO & VST Packages\${PRODUCT_NAME}"
  !define PRODUCT_UNINST_KEY "Software\Delphi ASIO & VST Packages\Uninstall\${PRODUCT_NAME}"
  !define PRODUCT_UNINST_ROOT_KEY "HKLM"
  

;--------------------------------
;Language Selection Dialog Settings

  ;Remember the installer language
  !define MUI_LANGDLL_REGISTRY_ROOT "HKLM" 
  !define MUI_LANGDLL_REGISTRY_KEY "Software\Delphi ASIO & VST Packages\${PRODUCT_NAME}"
  !define MUI_LANGDLL_REGISTRY_VALUENAME "Installer Language"
  !define MUI_FINISHPAGE_NOAUTOCLOSE


;--------------------------------
;Reserve Files
  
  ;These files should be inserted before other files in the data block
  ;Keep these lines before any File command
  ;Only for solid compression (by default, solid compression is enabled for BZIP2 and LZMA)
  
;  !insertmacro MUI_RESERVEFILE_LANGDLL


;--------------------------------
;Pages

  !insertmacro MUI_PAGE_WELCOME
  !insertmacro MUI_PAGE_LICENSE "..\Bin\License.txt"
  !insertmacro MUI_PAGE_INSTFILES
  !insertmacro MUI_PAGE_FINISH
  !insertmacro MUI_UNPAGE_WELCOME
  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES


;--------------------------------
;Languages
 
  !insertmacro MUI_LANGUAGE "English"
;  !insertmacro MUI_LANGUAGE "German"
;  !insertmacro MUI_RESERVEFILE_LANGDLL


;--------------------------------
;Installer Sections

Section "ASIO-Host Driver" SecAsioHostDriver
  SetOutPath "$SYSDIR"
  
  ;ADD YOUR OWN FILES HERE...
  File "C:\Windows\System32\AsioHostDriver.dll"
  
  ExecWait '$SYSDIR\regsvr32.exe /s "$SYSDIR\AsioHostDriver.dll"'

  ;Create uninstaller
  WriteUninstaller "$SYSDIR\Uninstall_ASIO-Host_Driver.exe"

SectionEnd


;--------------------------------
;Descriptions

  ;Language strings
  LangString TEXT_IO_TITLE ${LANG_ENGLISH} "InstallOptions page"
  LangString TEXT_IO_SUBTITLE ${LANG_ENGLISH} "ASIO-Host Driver"

  LangString DESC_SecAsioHostDriver ${LANG_ENGLISH} "ASIO-Host Driver"

  ;Assign language strings to sections
  !insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
    !insertmacro MUI_DESCRIPTION_TEXT ${SecAsioHostDriver} $(DESC_SecAsioHostDriver)
  !insertmacro MUI_FUNCTION_DESCRIPTION_END


;--------------------------------
;Uninstaller Section

Section "Uninstall"

  ;ADD YOUR OWN FILES HERE...
  ExecWait '$SYSDIR\regsvr32.exe /s /u "$SYSDIR\AsioHostDriver.dll"'
  Delete "$SYSDIR\AsioHostDriver.dll"
  Delete "$SYSDIR\Uninstall_ASIO-Host_Driver.exe"
  DeleteRegKey HKLM "Software\Delphi ASIO & VST Packages\${PRODUCT_NAME}"
SectionEnd