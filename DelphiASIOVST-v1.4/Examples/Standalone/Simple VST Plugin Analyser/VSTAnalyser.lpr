program VSTAnalyser;

{$MODE Delphi}

uses
  Interfaces,
  Forms,
  HostVSTLaz,
  VAMain in 'VAMain.pas' {FmVSTAnalyser},
  VAPlotIR in 'VAPlotIR.pas' {FmPlotIR};

begin
  Application.Title:='Simple VST Analyser';
  Application.Initialize;
  Application.CreateForm(TFmVSTAnalyser, FmVSTAnalyser);
  Application.CreateForm(TFmPlotIR, FmPlotIR);
  Application.Run;
end.

