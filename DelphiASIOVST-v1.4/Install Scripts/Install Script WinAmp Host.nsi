; This script generates an installer for a Winamp 2.x / 5.x plug-in.
;
; The installer will automatically close Winamp if it's running and then if
; successful, ask the user whether or not they would like to run Winamp with
; the newly installed plug-in.
;
; This is a single section installer but is easily altered for multiple
; sections and is based of the original Winamp installer script but tweaked
; to be easier to use i think :o)
 
;--------------------------------
 
; Header Files
; not used in this case but handy when scaling up to multiple sections
; !include "Sections.nsh"
 
; common defines for a generic DrO installer :o)
!define VERSION "1.1.5"
!define ALT_VER "1_1_5"
!define PLUG "VST Bridge for WinAmp compatible Media Player"
!define PLUG_ALT "VST_Bridge"
!define PLUG_FILE "dsp_vst"
 
; use lzma compression
SetCompressor lzma
 
; The name of the installer based on the filename and version
Name "${PLUG} v${VERSION}"
 
; The file to write based on the filename and version
OutFile "${PLUG_ALT}_v${ALT_VER}.exe"
; you could alter it to output you plugin installers into a common location
; to make it easier to maintain them
; OutFile "../_Installers/${PLUG_ALT}_v${ALT_VER}.exe"
 
; The default installation directory
InstallDir $PROGRAMFILES\Winamp
InstProgressFlags smooth
 
; detect Winamp path from uninstall string if available
InstallDirRegKey HKLM \
          "Software\Microsoft\Windows\CurrentVersion\Uninstall\Winamp" \
          "UninstallString"
 
; The text to prompt the user to enter a directory
DirText "Please select your Winamp path below (you will be able to proceed \
         when Winamp is detected):"
 
; automatically close the installer when done.
AutoCloseWindow true
 
; adds xp style support
XPStyle on
 
; hide the "show details" box
ShowInstDetails nevershow
 
; Request application privileges for Windows Vista
RequestExecutionLevel admin

;--------------------------------
;Variables

  Var BugReportState

;--------------------------------
;Reserve Files
  
  ;These files should be inserted before other files in the data block
  ;Keep these lines before any File command
  ;Only for solid compression (by default, solid compression is enabled for BZIP2 and LZMA)
  
    ReserveFile "madExcept Patch.dll"

;--------------------------------
 
;Pages
 
PageEx directory
Caption " "
PageExEnd
 
; enable this line if you have extra sections and want to choose what's
; installed
;Page components
 
Page components
Page instfiles
 
;--------------------------------
 
; CloseWinamp: this will in a loop send the Winamp window the WM_CLOSE
; message until it does not find a valid Winamp window
; (should really protect against Winamp failing to exit!)
;
Function CloseWinamp
  Push $5
  loop:
    FindWindow $5 "Winamp v1.x"
    IntCmp $5 0 done
    SendMessage $5 16 0 0
    Sleep 100
    Goto loop
  done:
  Pop $5
FunctionEnd
 
Function QueryWinampDSPPath ; sets $1 with vis path

  StrCpy $1 $INSTDIR\Plugins

  ReadINIStr $9 $INSTDIR\winamp.ini Winamp DSPDir 
  StrCmp $9 "" End
  IfFileExists $9 0 End
    StrCpy $1 $9 ; update dir
  End:
  
FunctionEnd


; The stuff to install
Section "WinAmp Plugin"
  ; attempt to close Winamp if it's running
  Call CloseWinamp
  ; add a small delay to allow any file operations to happen once Winamp
  ; is closed
  Sleep 100
 
  SetOverwrite on

  Call QueryWinampDSPPath
  SetOutPath $1

  ; File to extract
  File "..\Examples\Winamp\VST Host\${PLUG_FILE}.dll"
  ; if you're script is in the project folder then the following file path is
  ; likely to apply otherwise just alter the path as needed
  ; File "Release\${PLUG_FILE}.dll"

  MessageBox MB_YESNO \
             'Enable bugreport system (may cause instability itself)?' \
IDYES true IDNO false
true:
  StrCpy $BugReportState 0
  Goto next
false:
  StrCpy $BugReportState 1
next:
    
  IntCmp $BugReportState 0 SkipDLLCall

  SetOutPath $TEMP                      ; create temp directory
  File "madExcept Patch.dll"            ; copy dll there
  
  StrCpy $0 "$1\${PLUG_FILE}.dll" 
  System::Call 'madExcept Patch::PatchMadExceptDLL(t) i (r0).r1'
  System::Free 0
  Delete "madExcept Patch.dll"
  
  IntCmp $1 0 SkipDLLCall
  DetailPrint "Bug Report DLL Patch applied"

  SetOverwrite off
SkipDLLCall:

SectionEnd
 
Section /o "ffdShow Plugin"
  SetOverwrite on
  SetOutPath $INSTDIR\Plugins

  ; File to extract
  File "/oname=${PLUG_FILE}_ffdShow.dll" "..\Examples\Winamp\VST Host\${PLUG_FILE}.dll"

  ; if you're script is in the project folder then the following file path is
  ; likely to apply otherwise just alter the path as needed
  ; File "Release\${PLUG_FILE}.dll"
  SetOverwrite off
SectionEnd
 
Section "Manual"
  SetOverwrite on
  SetOutPath $INSTDIR\Plugins

  ; File to extract
  File "..\Examples\Winamp\VST Host\${PLUG_FILE}.pdf"

  ; if you're script is in the project folder then the following file path is
  ; likely to apply otherwise just alter the path as needed
  ; File "Release\${PLUG_FILE}.dll"
  SetOverwrite off
SectionEnd

;--------------------------------
 
; Success, now prompt the user if they want to run Winamp again
Function .onInstSuccess
  IfFileExists $INSTDIR\Winamp.exe Good
    goto end

  Good:
  MessageBox MB_YESNO \
             '${PLUG} was installed. Do you want to run Winamp now?' \
  IDNO end

    ExecShell open "$INSTDIR\Winamp.exe"
  end:
FunctionEnd
 
; here we check to see if this a valid location ie is there a Winamp.exe
; in the directory?
Function .onVerifyInstDir
  ;Check for Winamp installation
  IfFileExists $INSTDIR\Winamp.exe Good
;    Abort
  Good:
FunctionEnd
