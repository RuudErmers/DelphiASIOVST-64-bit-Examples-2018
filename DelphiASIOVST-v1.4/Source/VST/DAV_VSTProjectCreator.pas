{******************************************************************************}
{                                                                              }
{ Code to generate the plugin's project file, i.e. <ProjectName.dpr>           }
{                                                                              }
{ Part of the VST Plugin Framework by Christian Budde and Tobybear.            }
{                                                                              }
{******************************************************************************}

unit DAV_VSTProjectCreator;

interface

{$I ..\DAV_Compiler.inc}

uses
  ToolsAPI,
  DAV_VSTPluginConfig;

type
  TVSTProjectCreator = class(TInterfacedObject,
    IOTACreator,
    {$IFDEF DELPHI8_UP} IOTAProjectCreator80, {$ENDIF}
    IOTAProjectCreator)
  private
    FConfig: TConfig;
  public
    constructor Create(Config: TConfig);
    // IOTACreator
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    // IOTAProjectCreator
    function GetFileName: string;
    function GetOptionFileName: string;
    function GetShowSource: Boolean;
    procedure NewDefaultModule;
    function NewOptionSource(const ProjectName: string): IOTAFile;
    procedure NewProjectResource(const Project: IOTAProject);
    function NewProjectSource(const ProjectName: string): IOTAFile;
    {$IFDEF DELPHI8_UP}
    // IOTAProjectCreator80
    function GetProjectPersonality: string;
    // IOTAProjectCreator50
    procedure NewDefaultProjectModule(const Project: IOTAProject);
    {$ENDIF}
  end;

implementation

uses
  SysUtils,
  DAV_OpenToolsUtils;

const
  CRLF = #13#10;

constructor TVSTProjectCreator.Create(Config: TConfig);
begin
  FConfig := Config;
end;

// =============================================================================
// IOTACreator
// =============================================================================

{ Return a string representing the default creator type in which to augment }
function TVSTProjectCreator.GetCreatorType: string;
begin
  // Create a library project
  Result := sLibrary;
end;

{ Return False if this is a new module }
function TVSTProjectCreator.GetExisting: Boolean;
begin
  // Create a new project
  Result := False;
end;

{ Return the File system IDString that this module uses for reading/writing }
function TVSTProjectCreator.GetFileSystem: string;
begin
  Result := ''; // Default
end;

{ Return the Owning module, if one exists (for a project module, this would
  be a project; for a project this is a project group) }
function TVSTProjectCreator.GetOwner: IOTAModule;
begin
  // Owned by the current project group
  Result := GetCurrentProjectGroup;
end;

{ Return true, if this item is to be marked as un-named. This will force the
  "Save As" dialog to appear the first time the user saves. }
function TVSTProjectCreator.GetUnnamed: Boolean;
begin
  // Project needs to be named/saved
  Result := False; // False still queries for a project name!
end;

// =============================================================================
// IOTAProjectCreator
// =============================================================================

{ Return the project filename. NOTE: This *must* be a fully qualified file name. }
function TVSTProjectCreator.GetFileName: string;
begin
  Result := IncludeTrailingPathDelimiter(FConfig.ProjectPath) +
    FConfig.ProjectName + '.dpr';
end;

{ Deprecated!! Return the option file name (C++ .bpr, .bpk, etc...) }
function TVSTProjectCreator.GetOptionFileName: string;
begin
  Result := ''; // Default
end;

{ Return True to show the source }
function TVSTProjectCreator.GetShowSource: Boolean;
begin
  // Show the source in the editor
  Result := True;
end;

{ Deprecated!! Called to create a new default module for this project.
  Please implement and use the method on IOTAProjectCreator50. }
procedure TVSTProjectCreator.NewDefaultModule;
//var
//  Module: IOTAModule;
//  ModuleCreator: TVSTModuleCreator;
begin
  //  ModuleCreator := TVSTModuleCreator.Create('');
  //  Module := (BorlandIDEServices as IOTAModuleServices).CreateModule(ModuleCreator);
end;

{ Deprecated!! Create and return the project option source. (C++) }
function TVSTProjectCreator.NewOptionSource(const ProjectName: string):
  IOTAFile;
begin
  Result := nil;
end;

{ Called to indicate when to create/modify the project resource file }
procedure TVSTProjectCreator.NewProjectResource(const Project: IOTAProject);
begin
  // No resources needed
end;

{ Create and return the Project source file }
function TVSTProjectCreator.NewProjectSource(const ProjectName: string):
  IOTAFile;
var
  S: string;
begin
  // returning nil would create the default source for a new library
  // but we create our own source file
  S :=
    '{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}' + CRLF +
    'library ' + ProjectName + ';' + CRLF +
    CRLF +
    'uses ' + CRLF +
    '  DAV_WinAmp,' + CRLF +
    '  DAV_VSTEffect,' + CRLF +
    '  DAV_VSTBasicModule;' + CRLF +
    CRLF +
    'function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;' + CRLF +
    'begin' + CRLF +
    '  Result := VstModuleMain(AudioMasterCallback, T' + FConfig.PluginFormName + ');' + CRLF +
    'end;' + CRLF +
    CRLF +
    'function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;' + CRLF +
    'begin' + CRLF +
    '  Result := WinampDSPModuleHeader(T' + FConfig.PluginFormName + ');' + CRLF +
    'end;' + CRLF +
    CRLF +
    'exports' + CRLF +
    '  VstPluginMain name ''main'',' + CRLF +
    '  VstPluginMain name ''VSTPluginMain'',' + CRLF +
    '  WinampDSPGetHeader name ''winampDSPGetHeader2'';' + CRLF +
    CRLF +
    'begin' + CRLF +
    'end.';

  Result := StringToIOTAFile(S);
end;

{$IFDEF DELPHI8_UP}
// =============================================================================
// IOTAProjectCreator80
// =============================================================================

function TVSTProjectCreator.GetProjectPersonality: string;
begin
  Result := sDelphiPersonality;
end;

// =============================================================================
// IOTAProjectCreator50
// =============================================================================

procedure TVSTProjectCreator.NewDefaultProjectModule(const Project:
  IOTAProject);
begin
  NewDefaultModule;
end;
{$ENDIF}

end.
