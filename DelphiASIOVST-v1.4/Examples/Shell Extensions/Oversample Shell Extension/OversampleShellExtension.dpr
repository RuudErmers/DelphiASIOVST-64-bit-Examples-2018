library OversampleShellExtension;

{$R 'OversampleTemplate.res' 'OversampleTemplate.rc'}

{$I DAV_Compiler.inc}

uses
  FastMM4,  // either download the library or comment if there is an error here
{$IFDEF UseFastMove}
  FastMove, // either download the library or comment if there is an error here
{$ENDIF}
{$IFNDEF Compiler14_UP}
  RTLVCLOptimize, // "
{$ENDIF}
  ComServ,
  OversamplePlugin in 'OversamplePlugin.pas';

{$R *.res}

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

begin
end.
