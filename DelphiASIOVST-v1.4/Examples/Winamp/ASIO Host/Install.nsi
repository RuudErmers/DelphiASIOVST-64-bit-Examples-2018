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
 
!define VERSION "1.1"
!define ALT_VER "1_1"
!define PLUG "WinAmp ASIO Output"
!define PLUG_ALT "WinAmp_ASIO_Output"
!define PLUG_FILE "out_asio"
 
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
 
;--------------------------------
 
;Pages
 
PageEx directory
Caption " "
PageExEnd
 
; enable this line if you have extra sections and want to choose what's
; installed
;Page components
 
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
 
 
; The stuff to install
Section ""
  ; attempt to close Winamp if it's running
  Call CloseWinamp
  ; add a small delay to allow any file operations to happen once Winamp
  ; is closed
  Sleep 100
 
  SetOverwrite on
  SetOutPath "$INSTDIR\Plugins"
  ; File to extract
  File "${PLUG_FILE}.dll"
  ; if you're script is in the project folder then the following file path is
  ; likely to apply otherwise just alter the path as needed
  ; File "Release\${PLUG_FILE}.dll"
  SetOverwrite off
SectionEnd
 
;--------------------------------
 
; Success, now prompt the user if they want to run Winamp again
Function .onInstSuccess
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
    Abort
  Good:
FunctionEnd
