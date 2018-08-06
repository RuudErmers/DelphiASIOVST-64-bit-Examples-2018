program HRTFEditor;

// if the file below is missing please execute the batch file in this
// directory first to compile the resource file

{$R 'HRTF3D.res' 'HRTF3D.rc'}

uses
  FastMM4, // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  Forms,
  HEmain in 'HEmain.pas' {FmHRTFEditor},
  HEeti in 'HEeti.pas' {FmEtiImport};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmHRTFEditor, FmHRTFEditor);
  Application.Run;
end.
