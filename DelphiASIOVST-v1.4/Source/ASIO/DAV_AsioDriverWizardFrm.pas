unit DAV_AsioDriverWizardFrm;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  The Wizard form used to collect the configuration information from the    //
//  user before commencing the code generation process.                       //
//                                                                            //
//  Part of the ASIO Driver Framework by Christian Budde and Tobybear.         //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Windows, Messages, Classes, Controls, Forms, StdCtrls, ComCtrls, ExtCtrls,
  DAV_AsioDriverConfig, Graphics;

type
  TAsioDriverWizardForm = class(TForm)
    Bevel2: TBevel;
    BlSeparator: TBevel;
    btnCancel: TButton;
    btnFinish: TButton;
    btnNext: TButton;
    btnPrev: TButton;
    ImageASIO: TImage;
    LbEffectName: TLabel;
    LbHeading: TLabel;
    LbSubHeading: TLabel;
    PnControl: TPanel;
    PnHeader: TPanel;
    PageControl: TPageControl;
    TSWelcome: TTabSheet;
    LbWelcomeTitle: TLabel;
    LbWelcomeInstructions4: TLabel;
    LbWelcomeInstructions3: TLabel;
    LbWelcomeInstructions2: TLabel;
    LbWelcomeInstructions1: TLabel;
    TSDestination: TTabSheet;
    LbDestinationTitle: TLabel;
    LBDesinationSelect: TLabel;
    LBProjectName: TLabel;
    LbDpr: TLabel;
    edtProjectPath: TEdit;
    edtProjectName: TEdit;
    btnBrowse: TButton;
    chkSaveWhenFinished: TCheckBox;
    TSModule: TTabSheet;
    LbModuleTitle: TLabel;
    LbModuleInstructions: TLabel;
    LbModuleName: TLabel;
    LbModuleUnit: TLabel;
    LbPas: TLabel;
    edtDriverFormName: TEdit;
    edtDriverUnitName: TEdit;
    TSControlPanel: TTabSheet;
    LbGUIFormTitle: TLabel;
    LbGUIFormInstructions: TLabel;
    pnlEditorDetails: TPanel;
    LbPasDfm: TLabel;
    LbGUIFormUnit: TLabel;
    lblEditorFormName: TLabel;
    edtControlPanelUnitName: TEdit;
    edtControlPanelFormName: TEdit;
    chkUseEditor: TCheckBox;
    TSNames: TTabSheet;
    LbNameTitle: TLabel;
    LbNameInstructions: TLabel;
    LbAsioDriverName: TLabel;
    LbGUID: TLabel;
    edtDriverName: TEdit;
    edtGUID: TEdit;
    TSFinish: TTabSheet;
    LbDone: TLabel;
    LbDoneInstruction: TLabel;
    LbClickFinish: TLabel;
    Label1: TLabel;
    procedure btnNextClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure edtVersionMajorKeyPress(Sender: TObject; var Key: Char);
    procedure chkUseEditorClick(Sender: TObject);
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
  Dialogs, SysUtils;

// removed, because it's missing  
{ $R *.DFM}

const
  STEP_WELCOME        = 0;
  STEP_DESTINATION    = 1;
  STEP_PLUGIN_TYPE    = 2;
  STEP_MODULE         = 3;
  STEP_EDITOR         = 4;
  STEP_NAMES          = 5;
  STEP_FINISH         = 6;

resourcestring
  CBrowsePathDialogCaption = 'Select Project Directory';
  CMissingProjectPath = 'You must enter a project path.';
  CMissingProjectName = 'You must enter a project name.';
  CMissingDriverFormName = 'You must enter a name for the ASIOModule descendant class.';
  CDriverFormNameSameAsBase = 'Your descendant class name must be different from the base class name.';
  CMissingDriverUnitName = 'You must enter a unit name for the driver module.';
  CMissingControlPanelFormName = 'You must enter a name for the control panel form.';
  CMissingControlPanelUnitName = 'You must enter a unit name for the control panel form.';
  CMissingDriverName = 'You must enter a name for the driver.';
  CMissingGUID = 'You must enter a unique ID for the driver.';
  CInvalidGUIDLength = 'The unique ID must be exactly four characters long';
  CProjectPathDoesNotExist = 'The project path does not exist. Would you like to create it?';

function ShowWizardGuiDialog(Config: TConfig): Boolean;
begin
  with TAsioDriverWizardForm.Create(nil) do
  try
    InitFormFromConfig(Config);
    Result := (ShowModal = mrOK);
    if Result then
      InitConfigFromForm(Config);
  finally
    Free;
  end;
end;

procedure TAsioDriverWizardForm.btnNextClick(Sender: TObject);
begin
  if ValidateStep(PageControl.ActivePageIndex) then
  begin
    PageControl.ActivePageIndex := PageControl.ActivePageIndex + 1;
    SetNavigationButtons;
    SetActiveSheetControlFocus;
  end;
end;

procedure TAsioDriverWizardForm.btnPrevClick(Sender: TObject);
begin
  PageControl.ActivePageIndex := PageControl.ActivePageIndex - 1;
  SetNavigationButtons;
  SetActiveSheetControlFocus;
end;

procedure TAsioDriverWizardForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to PageControl.PageCount - 1 do
  begin
    PageControl.Pages[i].TabVisible := False;
  end;
  Height := Height - 16;
  PageControl.ActivePageIndex := STEP_WELCOME;
  SetNavigationButtons;
end;

procedure TAsioDriverWizardForm.InitConfigFromForm(Config: TConfig);
begin
  Config.ProjectPath          := edtProjectPath.Text;
  Config.ProjectName          := edtProjectName.Text;
  Config.AsioDriverUnitName   := edtDriverUnitName.Text;
  Config.AsioDriverFormName   := edtDriverFormName.Text;
  Config.ControlPanelUnitName := edtControlPanelUnitName.Text;
  Config.ControlPanelFormName := edtControlPanelFormName.Text;
  Config.UseControlPanel      := chkUseEditor.Checked;
  Config.GUIDstring           := edtGUID.Text;
  Config.DriverName           := edtDriverName.Text;
  Config.GUIDstring           := edtGUID.Text;
  Config.SaveWhenDone         := chkSaveWhenFinished.Checked;
end;

procedure TAsioDriverWizardForm.InitFormFromConfig(Config: TConfig);
begin
  edtProjectPath.Text          := Config.ProjectPath;
  edtProjectName.Text          := Config.ProjectName;
  edtDriverUnitName.Text       := Config.AsioDriverUnitName;
  edtDriverFormName.Text       := Config.AsioDriverFormName;
  edtControlPanelUnitName.Text := Config.ControlPanelUnitName;
  edtControlPanelFormName.Text := Config.ControlPanelFormName;
  chkUseEditor.Checked         := Config.UseControlPanel;
  edtGUID.Text                 := Config.GUIDstring;
  edtDriverName.Text           := Config.DriverName;
  edtGUID.Text                 := Config.GUIDstring;
  chkSaveWhenFinished.Checked  := Config.SaveWhenDone;
end;

procedure TAsioDriverWizardForm.SetNavigationButtons;
begin
  btnPrev.Enabled   := (PageControl.ActivePageIndex > STEP_WELCOME);
  btnNext.Visible   := (PageControl.ActivePageIndex < STEP_FINISH);
  btnFinish.Visible := (PageControl.ActivePageIndex = STEP_FINISH);
end;

procedure TAsioDriverWizardForm.btnBrowseClick(Sender: TObject);
var
  sDirectory: string;
begin
 sDirectory := edtProjectPath.Text;
 if SelectDirectory(CBrowsePathDialogCaption, '', sDirectory)
  then edtProjectPath.Text := sDirectory;
end;

procedure TAsioDriverWizardForm.edtVersionMajorKeyPress(Sender: TObject;
  var Key: Char);
begin
 {$IFDEF DELPHI12_UP}
 if not CharInSet(Key, [#8, '0'..'9'])
 {$ELSE}
 if not (Key in [#8, '0'..'9'])
 {$ENDIF}
  then Key := #0;
end;

procedure TAsioDriverWizardForm.chkUseEditorClick(Sender: TObject);
begin
  pnlEditorDetails.Visible := chkUseEditor.Checked;
end;

procedure TAsioDriverWizardForm.InvalidFieldError(Control: TWinControl; const
    MessageText: string);
begin
  Control.SetFocus;
  MessageDlg(MessageText, mtError, [mbOK], 0);
end;

procedure TAsioDriverWizardForm.SetActiveSheetControlFocus;
var
  i: Integer;
  Control: TControl;
begin
  for i := 0 to PageControl.ActivePage.ControlCount - 1 do
  begin
    Control := PageControl.ActivePage.Controls[i];
    if (Control is TCustomEdit) or (Control is TRadioButton) or
      (Control is TCheckBox) then
    begin
      TWinControl(Control).SetFocus;
      Break;
    end;
  end;
end;

procedure TAsioDriverWizardForm.TrimAllEditBoxes;
var
  i: Integer;
  Component: TComponent;
begin
  for i := 0 to ComponentCount - 1 do
  begin
    Component := Components[i];
    if Component is TCustomEdit then
    begin
      TCustomEdit(Component).Text := Trim(TCustomEdit(Component).Text);
    end;
  end;
end;

function TAsioDriverWizardForm.ValidateStep(StepIndex: Integer): boolean;
begin
  Result := False;
  TrimAllEditBoxes;
  case StepIndex of
    STEP_DESTINATION:
      begin
        if edtProjectPath.Text = '' then
          InvalidFieldError(edtProjectPath, CMissingProjectPath)
        else if edtProjectName.Text = '' then
          InvalidFieldError(edtProjectName, CMissingProjectName)
        else if not DirectoryExists(edtProjectPath.Text) then
        begin
          if MessageDlg(CProjectPathDoesNotExist, mtConfirmation, [mbYes,
            mbNo], 0) = mrYes then
          begin
            ForceDirectories(edtProjectPath.Text);
            Result := True;
          end;
        end
        else
          Result := True;
      end;
    STEP_MODULE:
      begin
        if edtDriverFormName.Text = '' then
          InvalidFieldError(edtDriverFormName, CMissingDriverFormName)
        else if SameText(edtDriverFormName.Text, 'VSTModule') then
          InvalidFieldError(edtDriverFormName, CDriverFormNameSameAsBase)
        else if edtDriverUnitName.Text = '' then
          InvalidFieldError(edtDriverUnitName, CMissingDriverUnitName)
        else
          Result := True;
      end;
    STEP_EDITOR:
      begin
        if chkUseEditor.Checked and (edtControlPanelFormName.Text = '') then
          InvalidFieldError(edtControlPanelFormName, CMissingControlPanelFormName)
        else if chkUseEditor.Checked and (edtControlPanelUnitName.Text = '') then
          InvalidFieldError(edtControlPanelUnitName, CMissingControlPanelUnitName)
        else
          Result := True;
      end;
    STEP_NAMES:
      begin
        if edtDriverName.Text = '' then
          InvalidFieldError(edtDriverName, CMissingDriverName)
        else if edtGUID.Text = '' then
          InvalidFieldError(edtGUID, CMissingGUID)
        else if Length(edtGUID.Text) <> 4 then
          InvalidFieldError(edtGUID, CInvalidGUIDLength)
        else
          Result := True;
      end;
  else
    Result := True;
  end;
end;

end.

