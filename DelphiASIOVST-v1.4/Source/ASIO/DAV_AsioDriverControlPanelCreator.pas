unit DAV_AsioDriverControlPanelCreator;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Code to generate the GUI ControlPanel form.                               //
//                                                                            //
//  Part of the ASIO Driver Framework by Christian Budde and Tobybear.        //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  ToolsAPI,
  DAV_AsioDriverConfig;

type
  TAsioDriverControlPanelCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
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
    // IOTAModuleCreator
    function GetAncestorName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetFormName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string):
      IOTAFile;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string):
      IOTAFile;
    procedure FormCreated(const FormControlPanel: IOTAFormEditor);
  end;

implementation

uses
  Forms, SysUtils,
  DAV_OpenToolsUtils;

const
  CRLF          = #13#10;
  ANCESTOR_NAME = 'Form';

constructor TAsioDriverControlPanelCreator.Create(Config: TConfig);
begin
  FConfig := Config;
end;

procedure TAsioDriverControlPanelCreator.FormCreated(const FormControlPanel: IOTAFormEditor);
begin
  with TForm(INTAComponent(FormControlPanel.GetRootComponent).GetComponent) do
  begin
    BorderStyle := bsNone;
    Scaled := False;
  end;
end;

function TAsioDriverControlPanelCreator.GetAncestorName: string;
begin
  Result := ANCESTOR_NAME;
end;

function TAsioDriverControlPanelCreator.GetCreatorType: string;
begin
  Result := sForm;
end;

function TAsioDriverControlPanelCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TAsioDriverControlPanelCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TAsioDriverControlPanelCreator.GetFormName: string;
begin                             
  Result := FConfig.ControlPanelFormName;
end;

function TAsioDriverControlPanelCreator.GetImplFileName: string;
begin
  Result := {$IFDEF DELPHI6_UP}IncludeTrailingPathDelimiter{$ENDIF}(FConfig.ProjectPath) +
    FConfig.ControlPanelUnitName + '.pas';
end;

function TAsioDriverControlPanelCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TAsioDriverControlPanelCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TAsioDriverControlPanelCreator.GetOwner: IOTAModule;
begin
  Result := GetModuleOwner;
end;

function TAsioDriverControlPanelCreator.GetShowForm: Boolean;
begin
  Result := True;
end;

function TAsioDriverControlPanelCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TAsioDriverControlPanelCreator.GetUnnamed: Boolean;
begin
  Result := False; // False still queries for a project name!
end;

function TAsioDriverControlPanelCreator.NewFormFile(const FormIdent, AncestorIdent: string):
  IOTAFile;
begin
  // we initialise the form in the FormCreated procedure instead of writing out
  // a specific form definition here
  Result := nil
end;

function TAsioDriverControlPanelCreator.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
var
  s: string;
begin
  s :=
    'unit ' + ModuleIdent + ';' + CRLF +
    CRLF +
    'interface' + CRLF +
    CRLF +
    'uses ' + CRLF +
    '  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_AsioDriverModule;' + CRLF +
    CRLF +
    'type' + CRLF +
    '  T' + FormIdent + ' = class(T' + AncestorIdent + ')' + CRLF +
    //'  public' + CRLF +
    //'    ' + FConfig.PluginFormName + ': TAsioDriverModule;' + CRLF +
    '  end;' + CRLF + CRLF +
    'implementation' + CRLF +
    CRLF +
    '{$R *.DFM}' + CRLF +
    CRLF +
    'end.';

  Result := StringToIOTAFile(s);
end;

function TAsioDriverControlPanelCreator.NewIntfSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

end.
