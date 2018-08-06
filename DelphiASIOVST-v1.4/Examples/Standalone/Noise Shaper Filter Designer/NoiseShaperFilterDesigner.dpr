program NoiseShaperFilterDesigner;

uses
  FastMM4,
  FastMove,
  madExcept,
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  Forms,
  NSFDmain in 'NSFDmain.pas' {FmNoiseshapingFilterDesigner};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmNoiseshapingFilterDesigner, FmNoiseshapingFilterDesigner);
  Application.Run;
end.
