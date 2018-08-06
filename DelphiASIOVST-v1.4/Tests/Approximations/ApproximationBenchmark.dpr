program ApproximationBenchmark;

{$I DAV_Compiler.inc}

uses
  FastMM4,
  {$IFDEF UseFastMove}
  FastMove,
  {$ENDIF}
  Forms,
  ApproxMain in 'ApproxMain.pas' {FmApproximationBenchmark};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmApproximationBenchmark, FmApproximationBenchmark);
  Application.Run;
end.
