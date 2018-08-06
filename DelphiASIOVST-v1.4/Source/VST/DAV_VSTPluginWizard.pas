{******************************************************************************}
{                                                                              }
{ The main class for the VST Plugin Wizard. The Execute method drives the      }
{ process.                                                                     }
{                                                                              }
{ Part of the VST Plugin Framework by Christian Budde and Tobybear.            }
{                                                                              }
{******************************************************************************}

unit DAV_VSTPluginWizard;

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, {$ELSE}Windows, {$ENDIF} ToolsAPI, DAV_VSTPluginConfig;

type
  TVSTPluginWizard = class(TNotifierObject, IOTAWizard, IOTARepositoryWizard,
    IOTAProjectWizard)
  private
    procedure CreateEditorForm(Config: TConfig);
    procedure CreateProject(Config: TConfig);
    procedure CreateVSTModule(Config: TConfig);
  public
    // IOTAWizard
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    // IOTARepositoryWizard
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;
    {$IFDEF DELPHI6_UP}
    function GetGlyph: cardinal;
    {$ELSE}
    function GetGlyph: HICON;
    {$ENDIF}
  end;

implementation

{$R ..\..\Resources\DAV_VSTPluginWizard.res}

uses
  Dialogs, DAV_VSTProjectCreator, DAV_VSTModuleCreator, DAV_VSTEditorCreator,
  DAV_VSTPluginWizardFrm;

const
  WIZARD_ID      = '{8B2766A1-2AB3-4885-B156-0DDA15C73F85}';
  WIZARD_PAGE    = 'VST';
  WIZARD_NAME    = 'VST Plugin';
  WIZARD_AUTHOR  = 'Christian Budde and Tobybear';
  WIZARD_COMMENT = 'VST Plugin Wizard';
  WIZARD_ICON    = 'VSTPLUGINWIZARD';
  // error messages
  PROJECT_CREATION_ERROR_MESSAGE =
    'The wizard encountered an error while generating the main project file.';
  VSTMODULE_CREATION_ERROR_MESSAGE =
    'The wizard encountered an error while generating the VSTModule unit.';
  EDITOR_FORM_CREATION_ERROR_MESSAGE =
    'The wizard encountered an error while generating the editor form unit.';

procedure TVSTPluginWizard.CreateEditorForm(Config: TConfig);
begin
  try
    (BorlandIDEServices as IOTAModuleServices).CreateModule(
      TVSTEditorCreator.Create(Config));
  except
    MessageDlg(EDITOR_FORM_CREATION_ERROR_MESSAGE, mtError, [mbOK], 0);
  end;
end;

procedure TVSTPluginWizard.CreateProject(Config: TConfig);
begin
  try
    (BorlandIDEServices as IOTAModuleServices).CreateModule(
      TVSTProjectCreator.Create(Config));
  except
    MessageDlg(PROJECT_CREATION_ERROR_MESSAGE, mtError, [mbOK], 0);
  end;
end;

procedure TVSTPluginWizard.CreateVSTModule(Config: TConfig);
begin
  try
    (BorlandIDEServices as IOTAModuleServices).CreateModule(
      TVSTModuleCreator.Create(Config));
  except
    MessageDlg(VSTMODULE_CREATION_ERROR_MESSAGE, mtError, [mbOK], 0);
  end;
end;

procedure TVSTPluginWizard.Execute;
var
  Config: TConfig;
begin
  Config := TConfig.Create;
  try
    if ShowWizardGuiDialog(Config) then
    begin
      CreateProject(Config);
      CreateVSTModule(Config);
      if Config.UseEditor then
      begin
        Sleep(20);
        CreateEditorForm(Config);
      end;
    end;
  finally
    Config.Free;
  end;
end;

function TVSTPluginWizard.GetAuthor: string;
begin
  Result := WIZARD_AUTHOR;
end;

function TVSTPluginWizard.GetComment: string;
begin
  Result := WIZARD_COMMENT;
end;

{$IFDEF DELPHI6_UP}
function TVSTPluginWizard.GetGlyph: cardinal;
{$ELSE}
function TVSTPluginWizard.GetGlyph: HICON;
{$ENDIF}
begin
  Result := LoadIcon(hInstance, WIZARD_ICON);
end;

function TVSTPluginWizard.GetIDString: string;
begin
  Result := WIZARD_ID;
end;

function TVSTPluginWizard.GetName: string;
begin
  Result := WIZARD_NAME;
end;

function TVSTPluginWizard.GetPage: string;
begin
  Result := WIZARD_PAGE;
end;

function TVSTPluginWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

end.
