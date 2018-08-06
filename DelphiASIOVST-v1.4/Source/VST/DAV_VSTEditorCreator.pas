{******************************************************************************}
{                                                                              }
{ Code to generate the GUI editor form.                                        }
{                                                                              }
{ Part of the VST Plugin Framework by Christian Budde and Tobybear.            }
{                                                                              }
{******************************************************************************}

unit DAV_VSTEditorCreator;

interface

{$I ..\DAV_Compiler.inc}

uses
  ToolsAPI,
  DAV_VSTPluginConfig;

type
  TVSTEditorCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
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
    procedure FormCreated(const FormEditor: IOTAFormEditor);
  end;

implementation

uses
  Forms, SysUtils,
  DAV_OpenToolsUtils;

const
  CRLF          = #13#10;
  ANCESTOR_NAME = 'Form';

constructor TVSTEditorCreator.Create(Config: TConfig);
begin
  FConfig := Config;
end;

procedure TVSTEditorCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
  with TForm(INTAComponent(FormEditor.GetRootComponent).GetComponent) do
  begin
    BorderStyle := bsNone;
    Scaled := False;
  end;
end;

function TVSTEditorCreator.GetAncestorName: string;
begin
  Result := ANCESTOR_NAME;
end;

function TVSTEditorCreator.GetCreatorType: string;
begin
  Result := sForm;
end;

function TVSTEditorCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TVSTEditorCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TVSTEditorCreator.GetFormName: string;
begin
  Result := FConfig.EditorFormName;
end;

function TVSTEditorCreator.GetImplFileName: string;
begin
  Result := {$IFDEF DELPHI6_UP}IncludeTrailingPathDelimiter{$ENDIF}(FConfig.ProjectPath) +
    FConfig.EditorUnitName + '.pas';
end;

function TVSTEditorCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TVSTEditorCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TVSTEditorCreator.GetOwner: IOTAModule;
begin
  Result := GetModuleOwner;
end;

function TVSTEditorCreator.GetShowForm: Boolean;
begin
  Result := True;
end;

function TVSTEditorCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TVSTEditorCreator.GetUnnamed: Boolean;
begin
  Result := False; // False still queries for a project name!
end;

function TVSTEditorCreator.NewFormFile(const FormIdent, AncestorIdent: string):
  IOTAFile;
begin
  // we initialise the form in the FormCreated procedure instead of writing out
  // a specific form definition here
  Result := nil
end;

function TVSTEditorCreator.NewImplSource(const ModuleIdent, FormIdent,
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
    '  Windows, Messages, SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule;' + CRLF +
    CRLF +
    'type' + CRLF +
    '  T' + FormIdent + ' = class(T' + AncestorIdent + ')' + CRLF +
    //'  public' + CRLF +
    //'    ' + FConfig.PluginFormName + ': TVSTModule;' + CRLF +
    '  end;' + CRLF + CRLF +
    'implementation' + CRLF +
    CRLF +
    '{$R *.DFM}' + CRLF +
    CRLF +
    'end.';

  Result := StringToIOTAFile(s);
end;

function TVSTEditorCreator.NewIntfSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

end.
