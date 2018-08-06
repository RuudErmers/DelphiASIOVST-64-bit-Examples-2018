{******************************************************************************}
{                                                                              }
{ The main class for the VST Plugin Wizard. The Execute method drives the      }
{ process.                                                                     }
{                                                                              }
{ Part of the VST Plugin Framework by Christian Budde and Tobybear.            }
{                                                                              }
{******************************************************************************}

unit DAV_VSTPluginCloneWizard;

interface

{$I ..\DAV_Compiler.inc}

uses
  Windows, ToolsAPI, DAV_VSTPluginCloneConfig;

type
  TVSTPluginCloneWizard = class(TNotifierObject, IOTAWizard, IOTARepositoryWizard,
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
    function GetGlyph: Cardinal;
    {$ELSE}
    function GetGlyph: HICON;
    {$ENDIF}
  end;

implementation

{$R ..\..\Resources\DAV_VSTPluginWizard.res}

uses
  Dialogs, DAV_VSTClonedProjectCreator, DAV_VSTClonedModuleCreator,
  DAV_VSTClonedEditorCreator, DAV_VSTPluginCloneWizardFrm;

const
  CWizardID      = '{D730BEB9-503B-409A-9D96-DF6009440D86}';
  CWizardPage    = 'VST';
  CWizardName    = 'VST Plugin Clone';
  CWizardAuthor  = 'Christian Budde';
  CWizardComment = 'VST Plugin Clone Wizard';
  CWizardIcon    = 'VSTPLUGINWIZARD';
  // error messages
  CProjectCreationErrorMessage =
    'The wizard encountered an error while generating the main project file.';
  CVstModuleCreationErrorMessage =
    'The wizard encountered an error while generating the VSTModule unit.';
  CEditorFormCreationErrorMessage =
    'The wizard encountered an error while generating the editor form unit.';

procedure TVSTPluginCloneWizard.CreateProject(Config: TConfig);
begin
  try
    (BorlandIDEServices as IOTAModuleServices).CreateModule(
      TVSTClonedProjectCreator.Create(Config));
  except
    MessageDlg(CProjectCreationErrorMessage, mtError, [mbOK], 0);
  end;
end;

procedure TVSTPluginCloneWizard.CreateVSTModule(Config: TConfig);
begin
  try
    (BorlandIDEServices as IOTAModuleServices).CreateModule(
      TVSTClonedModuleCreator.Create(Config));
  except
    MessageDlg(CVstModuleCreationErrorMessage, mtError, [mbOK], 0);
  end;
end;

procedure TVSTPluginCloneWizard.CreateEditorForm(Config: TConfig);
begin
  try
    (BorlandIDEServices as IOTAModuleServices).CreateModule(
      TVSTClonedEditorCreator.Create(Config));
  except
    MessageDlg(CEditorFormCreationErrorMessage, mtError, [mbOK], 0);
  end;
end;

procedure TVSTPluginCloneWizard.Execute;
var
  Config: TConfig;
begin
 Config := TConfig.Create;
 try
  if ShowWizardGuiDialog(Config) then
   begin
    CreateProject(Config);
    CreateVSTModule(Config);
    if Config.ContainsGui then
     begin
      Sleep(20);
      CreateEditorForm(Config);
     end;
   end;
  finally
   Config.Free;
  end;
end;

function TVSTPluginCloneWizard.GetAuthor: string;
begin
  Result := CWizardAuthor;
end;

function TVSTPluginCloneWizard.GetComment: string;
begin
  Result := CWizardComment;
end;

{$IFDEF DELPHI6_UP}
function TVSTPluginCloneWizard.GetGlyph: Cardinal;
{$ELSE}
function TVSTPluginCloneWizard.GetGlyph: HICON;
{$ENDIF}
begin
 Result := LoadIcon(hInstance, CWizardIcon);
end;

function TVSTPluginCloneWizard.GetIDString: string;
begin
  Result := CWizardID;
end;

function TVSTPluginCloneWizard.GetName: string;
begin
  Result := CWizardName;
end;

function TVSTPluginCloneWizard.GetPage: string;
begin
  Result := CWizardPage;
end;

function TVSTPluginCloneWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

end.

