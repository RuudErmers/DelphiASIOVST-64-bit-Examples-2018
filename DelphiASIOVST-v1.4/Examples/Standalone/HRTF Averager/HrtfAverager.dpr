program HrtfAverager;

{$I DAV_Compiler.inc}

uses
  FastMM4, // either download the library or comment if there is an error here
{$IFDEF UseFastMove}
  FastMove, // either download the library or comment if there is an error here
{$ENDIF}
  Forms,
  HAmain in 'HAmain.pas' {FmHrtfAverager};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmHrtfAverager, FmHrtfAverager);
  Application.Run;
end.
