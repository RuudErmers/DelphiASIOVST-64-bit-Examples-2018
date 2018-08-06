{******************************************************************************}
{                                                                              }
{ Code to generate the GUI editor form.                                        }
{                                                                              }
{ Part of the VST Plugin Framework by Christian Budde and Tobybear.            }
{                                                                              }
{******************************************************************************}

unit DAV_VSTClonedEditorCreator;

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, {$ELSE}Windows, {$ENDIF} Forms, SysUtils, Graphics, 
  StdCtrls, ExtCtrls, ToolsAPI, DAV_VSTPluginCloneConfig;

type
  TVSTClonedEditorCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
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
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    procedure FormCreated(const FormEditor: IOTAFormEditor);
  end;

implementation

uses
  DAV_OpenToolsUtils, DAV_VSTHost, Controls, Classes;

const
  CRLF          = #13#10;
  CAncestorName = 'Form';

constructor TVSTClonedEditorCreator.Create(Config: TConfig);
begin
  FConfig := Config;
end;

procedure TVSTClonedEditorCreator.FormCreated(const FormEditor: IOTAFormEditor);
var
  Frm : TForm;
  Rct : TRect;
  Bmp : TBitmap;
begin
 Frm := TForm(INTAComponent(FormEditor.GetRootComponent).GetComponent);
 with Frm, FConfig do
  begin
   BorderStyle := bsNone;
   Scaled := False;

   VSTPlugin.ShowEdit(Frm);
   try
    VSTPlugin.EditIdle;
    VSTPlugin.Idle;
    Rct := VSTPlugin.GetRect;
    Frm.ClientWidth := Rct.Right - Rct.Left;
    Frm.ClientHeight := Rct.Bottom - Rct.Top;

    with TImage.Create(Frm) do
     begin
      Name := 'BackgroundImage';
      Parent := Frm;
      Width := Frm.ClientWidth;
      Height := Frm.ClientHeight;

      // create temp bitmap and render GUI
      Bmp := TBitmap.Create;
      try
       VSTPlugin.RenderEditorToBitmap(Bmp);
       Picture.Assign(Bmp);
      finally
       FreeAndNil(Bmp);
      end;

     end;
   finally
    VSTPlugin.CloseEdit;
   end;
  end;
end;

function TVSTClonedEditorCreator.GetAncestorName: string;
begin
  Result := CAncestorName;
end;

function TVSTClonedEditorCreator.GetCreatorType: string;
begin
  Result := sForm;
end;

function TVSTClonedEditorCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TVSTClonedEditorCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TVSTClonedEditorCreator.GetFormName: string;
begin
  Result := FConfig.EditorFormName;
end;

function TVSTClonedEditorCreator.GetImplFileName: string;
begin
  Result := {$IFDEF DELPHI6_UP}IncludeTrailingPathDelimiter{$ENDIF}(FConfig.ProjectPath) +
    FConfig.EditorUnitName + '.pas';
end;

function TVSTClonedEditorCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TVSTClonedEditorCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TVSTClonedEditorCreator.GetOwner: IOTAModule;
begin
  Result := GetModuleOwner;
end;

function TVSTClonedEditorCreator.GetShowForm: Boolean;
begin
  Result := True;
end;

function TVSTClonedEditorCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TVSTClonedEditorCreator.GetUnnamed: Boolean;
begin
  Result := False; // False still queries for a project name!
end;

function TVSTClonedEditorCreator.NewFormFile(const FormIdent, AncestorIdent: string):
  IOTAFile;
begin
  // we initialise the form in the FormCreated procedure instead of writing out
  // a specific form definition here
  Result := nil
end;

function TVSTClonedEditorCreator.NewImplSource(const ModuleIdent, FormIdent,
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
    '  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule;' + CRLF +
    CRLF +
    'type' + CRLF +
    '  T' + FormIdent + ' = class(T' + AncestorIdent + ')' + CRLF +
    '  end;' + CRLF + CRLF +
    'implementation' + CRLF +
    CRLF +
    '{$R *.DFM}' + CRLF +
    CRLF +
    'end.';

  Result := StringToIOTAFile(s);
end;

function TVSTClonedEditorCreator.NewIntfSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

end.

