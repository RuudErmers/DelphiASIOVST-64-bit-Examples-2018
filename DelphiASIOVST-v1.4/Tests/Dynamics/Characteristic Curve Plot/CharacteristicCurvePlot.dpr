program CharacteristicCurvePlot;

{$R 'CCP.res' 'CCP.rc'}

uses
  Forms,
  CCPmain in 'CCPmain.pas' {FmDynamicCurvePlot};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmDynamicCurvePlot, FmDynamicCurvePlot);
  Application.Run;
end.
