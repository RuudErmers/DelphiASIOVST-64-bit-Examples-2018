program SplitPluginCreator;

{$R 'SplitTemplate.res' 'SplitTemplate.rc'}

uses
  FastMM4,  // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  RTLVCLOptimize,
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Forms,
  SPCmain in 'SPCmain.pas' {FmSplitPluginCreator};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmSplitPluginCreator, FmSplitPluginCreator);
  Application.Run;
end.
