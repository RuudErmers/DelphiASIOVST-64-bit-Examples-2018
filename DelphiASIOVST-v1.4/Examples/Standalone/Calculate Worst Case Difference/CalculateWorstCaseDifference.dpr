program CalculateWorstCaseDifference;

{$I DAV_Compiler.inc}

uses
  FastMM4, // either download the library or comment if there is an error here
  {$IFDEF UseFastMove}
  FastMove,
  {$ENDIF}
  Vcl.Forms,
  MainUnit in 'MainUnit.pas' {FmCalculateWorstCaseDifference};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFmCalculateWorstCaseDifference, FmCalculateWorstCaseDifference);
  Application.Run;
end.

