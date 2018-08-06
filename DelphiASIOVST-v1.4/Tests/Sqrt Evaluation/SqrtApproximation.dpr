program SqrtApproximation;

uses
  Forms,
  SqrtApproxMain in 'SqrtApproxMain.pas' {FmSqrtApproximation};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmSqrtApproximation, FmSqrtApproximation);
  Application.Run;
end.
