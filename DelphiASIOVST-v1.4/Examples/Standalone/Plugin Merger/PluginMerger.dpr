program PluginMerger;

// if the file below is missing please execute the batch file in this
// directory first to compile the resource file

{$R 'CustomWrapper.res' 'CustomWrapper.rc'}

uses
  FastMM4, // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  RTLVCLOptimize,
  Forms,
  PMmain in 'PMmain.pas' {FmPluginMerger},
  DAV_ChunkPluginGUI in '..\..\..\Source\DAV_ChunkPluginGUI.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmPluginMerger, FmPluginMerger);
  Application.Run;
end.
