{******************************************************************************}
{                                                                              }
{ The Wizard form used to collect the configuration information from the user  }
{ before commencing the code generation process.                               }
{                                                                              }
{ Part of the VST Plugin Framework by Christian Budde and Tobybear.            }
{                                                                              }
{******************************************************************************}

unit DAV_VSTPluginCloneWizardFrm;

interface

{$I ..\DAV_Compiler.inc}

uses
  Windows, Messages, Classes, Controls, Forms, StdCtrls, ComCtrls, ExtCtrls,
  Graphics, DAV_VSTPluginCloneConfig, Dialogs;

type
  TVSTPluginCloneWizardForm = class(TForm)
    Bevel: TBevel;
    BlSeparator: TBevel;
    btnBrowse: TButton;
    btnCancel: TButton;
    btnFinish: TButton;
    btnNext: TButton;
    btnPrev: TButton;
    BtSelectPlugin: TButton;
    CbCloneGUI: TCheckBox;
    chkSaveWhenFinished: TCheckBox;
    EdClonedPlugin: TEdit;
    edtProjectName: TEdit;
    edtProjectPath: TEdit;
    ImageVST: TImage;
    LbClickFinish: TLabel;
    LBDesinationSelect: TLabel;
    LbDestinationTitle: TLabel;
    LbDone: TLabel;
    LbDoneInstruction: TLabel;
    LbDpr: TLabel;
    LbEffectName: TLabel;
    LbHeading: TLabel;
    LbPluginType: TLabel;
    LBProjectName: TLabel;
    LbSelectVSTTypeInstruction: TLabel;
    LbSubHeading: TLabel;
    LbWelcomeInstructions1: TLabel;
    LbWelcomeInstructions2: TLabel;
    LbWelcomeInstructions3: TLabel;
    LbWelcomeInstructions4: TLabel;
    LbWelcomeTitle: TLabel;
    PageControl: TPageControl;
    PnControl: TPanel;
    PnHeader: TPanel;
    TSDestination: TTabSheet;
    TSFinish: TTabSheet;
    TSPluginType: TTabSheet;
    TSWelcome: TTabSheet;
    OpenDialog: TOpenDialog;
    procedure btnNextClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
  private
    procedure InvalidFieldError(Control: TWinControl; const MessageText: string);
    procedure SetActiveSheetControlFocus;
    procedure SetNavigationButtons;
    procedure TrimAllEditBoxes;
    function ValidateStep(StepIndex: Integer): boolean;
  public
    procedure InitConfigFromForm(Config: TConfig);
    procedure InitFormFromConfig(Config: TConfig);
  end;

function ShowWizardGuiDialog(Config: TConfig): Boolean;

implementation

uses
  {$WARN UNIT_PLATFORM OFF}
  FileCtrl,
  {$WARN UNIT_PLATFORM ON}
  SysUtils;

{$R *.DFM}

const
  STEP_WELCOME        = 0;
  STEP_DESTINATION    = 1;
  STEP_SELECT_PLUGIN  = 2;
  STEP_FINISH         = 3;

resourcestring
  RCBrowsePathDialogCaption = 'Select Project Directory';
  RCMissingProjectPath = 'You must enter a project path.';
  RCMissingProjectName = 'You must enter a project name.';
  RCMissingClonePlugin = 'You must specify a VST plugin to be cloned';
  RCProjectPathDoesNotExists =
    'The project path does not exist. Would you like to create it?';

function ShowWizardGuiDialog(Config: TConfig): Boolean;
begin
 with TVSTPluginCloneWizardForm.Create(nil) do
  try
   InitFormFromConfig(Config);
   Result := (ShowModal = mrOK);
   if Result then InitConfigFromForm(Config);
  finally
   Free;
  end;
end;

procedure TVSTPluginCloneWizardForm.btnNextClick(Sender: TObject);
begin
 if ValidateStep(PageControl.ActivePageIndex) then
  begin
   PageControl.ActivePageIndex := PageControl.ActivePageIndex + 1;
   SetNavigationButtons;
   SetActiveSheetControlFocus;
  end;
end;

procedure TVSTPluginCloneWizardForm.btnPrevClick(Sender: TObject);
begin
 PageControl.ActivePageIndex := PageControl.ActivePageIndex - 1;
 SetNavigationButtons;
 SetActiveSheetControlFocus;
end;

procedure TVSTPluginCloneWizardForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
 for i := 0 to PageControl.PageCount - 1
  do PageControl.Pages[i].TabVisible := False;
 Height := Height - 16;
 PageControl.ActivePageIndex := STEP_WELCOME;
 SetNavigationButtons;
end;

procedure TVSTPluginCloneWizardForm.InitConfigFromForm(Config: TConfig);
begin
  Config.ProjectPath    := edtProjectPath.Text;
  Config.ProjectName    := edtProjectName.Text;
  Config.ClonedPlugin   := EdClonedPlugin.Text;
  Config.CloneGui       := CbCloneGUI.Checked;
  Config.SaveWhenDone   := chkSaveWhenFinished.Checked;
end;

procedure TVSTPluginCloneWizardForm.InitFormFromConfig(Config: TConfig);
begin
 edtProjectPath.Text := Config.ProjectPath;
 edtProjectName.Text := Config.ProjectName;
 EdClonedPlugin.Text := Config.ClonedPlugin;
 CbCloneGUI.Checked  := Config.CloneGui;
 chkSaveWhenFinished.Checked := Config.SaveWhenDone;
end;

procedure TVSTPluginCloneWizardForm.SetNavigationButtons;
begin
 btnPrev.Enabled   := (PageControl.ActivePageIndex > STEP_WELCOME);
 btnNext.Visible   := (PageControl.ActivePageIndex < STEP_FINISH);
 btnFinish.Visible := (PageControl.ActivePageIndex = STEP_FINISH);
end;

procedure TVSTPluginCloneWizardForm.BtnBrowseClick(Sender: TObject);
begin
 if OpenDialog.Execute
  then EdClonedPlugin.Text := OpenDialog.FileName;
end;

procedure TVSTPluginCloneWizardForm.InvalidFieldError(Control: TWinControl; const
    MessageText: string);
begin
 Control.SetFocus;
 MessageDlg(MessageText, mtError, [mbOK], 0);
end;

procedure TVSTPluginCloneWizardForm.SetActiveSheetControlFocus;
var
  i: Integer;
  Control: TControl;
begin
 for i := 0 to PageControl.ActivePage.ControlCount - 1 do
  begin
   Control := PageControl.ActivePage.Controls[i];
   if (Control is TCustomEdit) or (Control is TRadioButton) or (Control is TCheckBox) then
    begin
     TWinControl(Control).SetFocus;
     Break;
    end;
  end;
end;

procedure TVSTPluginCloneWizardForm.TrimAllEditBoxes;
var
  i: Integer;
  Component: TComponent;
begin
 for i := 0 to ComponentCount - 1 do
  begin
   Component := Components[i];
   if Component is TCustomEdit
    then TCustomEdit(Component).Text := Trim(TCustomEdit(Component).Text);
  end;
end;

function TVSTPluginCloneWizardForm.ValidateStep(StepIndex: Integer): boolean;
begin
 Result := False;
 TrimAllEditBoxes;
 case StepIndex of
    STEP_DESTINATION:
      begin
        if edtProjectPath.Text = '' then
          InvalidFieldError(edtProjectPath, RCMissingProjectPath)
        else if edtProjectName.Text = '' then
          InvalidFieldError(edtProjectName, RCMissingProjectName)
        else if not DirectoryExists(edtProjectPath.Text) then
        begin
          if MessageDlg(RCProjectPathDoesNotExists, mtConfirmation, [mbYes,
            mbNo], 0) = mrYes then
          begin
            ForceDirectories(edtProjectPath.Text);
            Result := True;
          end;
        end
        else
          Result := True;
      end;
    STEP_SELECT_PLUGIN:
      begin
        if EdClonedPlugin.Text = '' then
          InvalidFieldError(EdClonedPlugin, RCMissingClonePlugin)
        else if not FileExists(EdClonedPlugin.Text) then
          InvalidFieldError(EdClonedPlugin, 'File doesn''t exists')
        else
          Result := True;
      end;
  else
    Result := True;
  end;
end;

end.

