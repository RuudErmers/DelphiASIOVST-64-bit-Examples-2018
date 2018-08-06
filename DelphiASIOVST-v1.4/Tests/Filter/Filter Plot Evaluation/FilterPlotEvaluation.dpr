program FilterPlotEvaluation;

uses
  Forms,
  FPEmain in 'FPEmain.pas' {Form1},
  DAV_DspFilterCoefficientEvaluation in '..\..\..\Source\DSP\DAV_DspFilterCoefficientEvaluation.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
