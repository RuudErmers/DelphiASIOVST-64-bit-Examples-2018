program VSTPluginScanner;

{$I DAV_Compiler.inc}

uses
  Interfaces,
  Forms,
  VPSmain in 'VPSmain.pas' {FmVSTPluginScanner};

{$R *.res}

{$IFDEF WINDOWS}{-$R VSTPluginScanner.rc}{$ENDIF}

begin
  Application.Initialize;
  Application.CreateForm(TFmVSTPluginScanner, FmVSTPluginScanner);
  Application.Run;
end.
