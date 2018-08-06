unit DAV_AsioDriverProjectCreator;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Code to generate the plugin's project file, i.e. <ProjectName.dpr>         //
//                                                                            //
//  Part of the ASIO Driver Framework by Christian Budde and Tobybear.         //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  ToolsAPI,
  DAV_AsioDriverConfig;

type
  TAsioDriverProjectCreator = class(TInterfacedObject,
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

constructor TAsioDriverProjectCreator.Create(Config: TConfig);
begin
  FConfig := Config;
end;

// =============================================================================
// IOTACreator
// =============================================================================

{ Return a string representing the default creator type in which to augment }
function TAsioDriverProjectCreator.GetCreatorType: string;
begin
  // Create a library project
  Result := sLibrary;
end;

{ Return False if this is a new module }
function TAsioDriverProjectCreator.GetExisting: Boolean;
begin
  // Create a new project
  Result := False;
end;

{ Return the File system IDString that this module uses for reading/writing }
function TAsioDriverProjectCreator.GetFileSystem: string;
begin
  Result := ''; // Default
end;

{ Return the Owning module, if one exists (for a project module, this would
  be a project; for a project this is a project group) }
function TAsioDriverProjectCreator.GetOwner: IOTAModule;
begin
  // Owned by the current project group
  Result := GetCurrentProjectGroup;
end;

{ Return true, if this item is to be marked as un-named. This will force the
  "Save As" dialog to appear the first time the user saves. }
function TAsioDriverProjectCreator.GetUnnamed: Boolean;
begin
  // Project needs to be named/saved
  Result := False; // False still queries for a project name!
end;

// =============================================================================
// IOTAProjectCreator
// =============================================================================

{ Return the project filename. NOTE: This *must* be a fully qualified file name. }
function TAsioDriverProjectCreator.GetFileName: string;
begin
  Result := IncludeTrailingPathDelimiter(FConfig.ProjectPath) +
    FConfig.ProjectName + '.dpr';
end;

{ Deprecated!! Return the option file name (C++ .bpr, .bpk, etc...) }
function TAsioDriverProjectCreator.GetOptionFileName: string;
begin
  Result := ''; // Default
end;

{ Return True to show the source }
function TAsioDriverProjectCreator.GetShowSource: Boolean;
begin
  // Show the source in the editor
  Result := True;
end;

{ Deprecated!! Called to create a new default module for this project.
  Please implement and use the method on IOTAProjectCreator50. }
procedure TAsioDriverProjectCreator.NewDefaultModule;
//var
//  Module: IOTAModule;
//  ModuleCreator: TAsioDriverModuleCreator;
begin
  //  ModuleCreator := TAsioDriverModuleCreator.Create('');
  //  Module := (BorlandIDEServices as IOTAModuleServices).CreateModule(ModuleCreator);
end;

{ Deprecated!! Create and return the project option source. (C++) }
function TAsioDriverProjectCreator.NewOptionSource(const ProjectName: string):
  IOTAFile;
begin
  Result := nil;
end;

{ Called to indicate when to create/modify the project resource file }
procedure TAsioDriverProjectCreator.NewProjectResource(const Project: IOTAProject);
begin
  // No resources needed
end;

{ Create and return the Project source file }
function TAsioDriverProjectCreator.NewProjectSource(const ProjectName: string):
  IOTAFile;
var
  S: string;
begin
  // returning nil would create the default source for a new library
  // but we create our own source file
  S := '{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}' + CRLF +
    'library ' + ProjectName + ';' + CRLF + CRLF +
    'uses' + CRLF +
    '  ComServ;' + CRLF + CRLF +
    'exports' + CRLF +
    '  DllGetClassObject,' + CRLF +
    '  DllCanUnloadNow,' + CRLF +
    '  DllRegisterServer,' + CRLF +
    '  DllUnregisterServer;' + CRLF + CRLF +
    '{$R *.RES}' + CRLF + CRLF +
    'begin' + CRLF +
    'end.';

  Result := StringToIOTAFile(S);
end;

{$IFDEF DELPHI8_UP}
// =============================================================================
// IOTAProjectCreator80
// =============================================================================

function TAsioDriverProjectCreator.GetProjectPersonality: string;
begin
  Result := sDelphiPersonality;
end;

// =============================================================================
// IOTAProjectCreator50
// =============================================================================

procedure TAsioDriverProjectCreator.NewDefaultProjectModule(const Project:
  IOTAProject);
begin
  NewDefaultModule;
end;
{$ENDIF}

end.

