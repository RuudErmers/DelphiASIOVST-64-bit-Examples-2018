program FunctionPlotter;

uses
  Forms,
  FPmain in 'FPmain.pas' {FmFunctionPlot};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmFunctionPlot, FmFunctionPlot);
  Application.Run;
end.
