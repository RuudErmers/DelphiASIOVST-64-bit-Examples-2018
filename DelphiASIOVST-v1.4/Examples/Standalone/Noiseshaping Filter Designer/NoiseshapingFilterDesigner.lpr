program NoiseshapingFilterDesigner;

{$I DAV_Compiler.inc}

uses
{$IFNDEF FPC}
  FastMM4, FastMove,
{$ELSE}
  Interfaces, Forms,
{$ENDIF}
  NfdMain in 'NfdMain.pas' {FmNoiseshapingFilterDesigner};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmNoiseshapingFilterDesigner, FmNoiseshapingFilterDesigner);
  Application.Run;
end.
