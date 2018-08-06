unit UPluginForm;

{$I DAV_Compiler.inc}

interface

{$IFNDEF FPC}
//  {$DEFINE LoadPluginFromStream}
{$ENDIF}

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} Types, Messages,
  Forms, SysUtils, Classes, Graphics, Controls, StdCtrls, ExtCtrls, ComCtrls,
  Menus, SyncObjs,  DAV_Types, DAV_VstEffect,DAV_VSTHost ;

type
  ShortStr = string[255];
  PShortStr = ^ShortStr;

type
  { TFmMiniHost }


  TPluginForm = class(TForm)
    MainMenu: TMainMenu;
    MIPreset: TMenuItem;
    MILoadPreset: TMenuItem;
    MILoadBank: TMenuItem;
    MISavePreset: TMenuItem;
    MISaveBank: TMenuItem;
    MIRenamePreset: TMenuItem;
    MIShowPreset: TMenuItem;
    MIUseMouseWheel: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    PnStatus: TPanel;
    PresetBox: TComboBox;
    PanelVST: TPanel;
    procedure MILoadPresetClick(Sender: TObject);
    procedure MISavePresetClick(Sender: TObject);
    procedure MILoadBankClick(Sender: TObject);
    procedure MISaveBankClick(Sender: TObject);
    procedure MIRenamePresetClick(Sender: TObject);
    procedure MIShowPresetClick(Sender: TObject);
    procedure PresetBoxClick(Sender: TObject);
    procedure PresetBoxChange(Sender: TObject);
    procedure PresetBoxKeyPress(Sender: TObject; var Key: Char);
    procedure PresetBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDeactivate(Sender: TObject);
  private
    FTitle,FDirPreset : AnsiString;
    FCurProg        : Integer;
    FCurProgName    : AnsiString;
    fPlugin:TVstPlugin;
    procedure SetPreset(Sender: TObject);
  protected
    procedure BuildPresetList;
    procedure LoadPresets(Files: TStrings);
  public
    procedure ShowVSTPlugin(Plugin:TVstPlugin);


  end;

var
  PluginForm : TPluginForm;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

uses
  Math, Inifiles, Dialogs, ShellAPI, DAV_Common;

function EnumNamesFunc(hModule: THandle; lpType, lpName: PChar; lParam: DWORD): Boolean; stdcall;
begin
 Result := True;
 TStringList(lParam).Add(lpName);
end;

procedure TPluginForm.BuildPresetList;
var
  m    : TMenuItem;
  n, i : Integer;
  s    : AnsiString;
begin
 PresetBox.clear;
 n := fPlugin.numPrograms;

 for i := 0 to n - 1 do
  begin
   fPlugin.GetProgramNameIndexed(-1, i, s);
   m := TMenuItem.Create(Self);
   m.Caption := string(s);
   m.OnClick := SetPreset;
   m.Tag := i;
  {$IFNDEF FPC}
   if (i > 0) and (i mod 256 <> 0) and (i mod 32 = 0)
    then m.break := mbBarBreak;
  {$ENDIF}
   s := AnsiString(IntToStr(i));
   if i < 10 then s := '00' + s else
   if i < 100 then s := '0' + s;
   PresetBox.AddItem(string(s) + ': ' + M.Caption, nil);
  end;

 if n >= 0 then PresetBox.ItemIndex := FCurProg;
end;


procedure TPluginForm.ShowVSTPlugin(Plugin:TVstPlugin);
var
  rct : ERect;
begin
 fPlugin:=plugin;
 with fPlugin do
  try
   ShowEdit(PanelVST);
   EditActivate;

   FTitle := GetVendorString + ' ' +  GetEffectName;
   BuildPresetList;

   if (effFlagsHasEditor in EffectOptions) then
    begin
     rct := EditGetRect;
     PanelVST.Width  := Rct.Right - Rct.Left;
     PanelVST.Height := Rct.Bottom - Rct.Top;
     PanelVST.Top    := PnStatus.Height;
     PanelVST.Left   := 0;

     // set client width
     if PanelVST.Width < 560 then
      begin
       ClientWidth := 560;
       PanelVST.Left := (560 - PanelVST.Width) div 2;
      end
     else
      begin
       ClientWidth := PanelVST.Width;
       PanelVST.left := 0;
      end;
     ClientHeight := PanelVST.Height + PnStatus.Height;

    end
   else
    begin
     PanelVST.Width  := 560;
     PanelVST.Height := 480;
    end;
  except
   raise;
  end;
 Visible:=true;
 MILoadPreset.Enabled := True;
 MILoadBank.Enabled   := True;
 MISavePreset.Enabled := True;
 MISaveBank.Enabled   := True;
 MIRenamePreset.Enabled := fPlugin.numPrograms >= 1;

 Caption := string(FTitle);
end;


procedure TPluginForm.MILoadPresetClick(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  try
   Filename := '*.fxp';
   InitialDir := FDirPreset;
   DefaultExt := '.fxp';
   Options := [ofAllowMultiSelect, ofFileMustExist, ofForceShowHidden];
   {$IFNDEF FPC}
   Ctl3D := False;
   {$ENDIF}
   Filter := 'preset files (*.fxp)|*.fxp';
   Title := 'Select a preset';
   if Execute then
    begin
     FDirPreset := extractfiledir(filename);
     LoadPresets(Files);
    end;
  finally
   Free;
  end;
end;

procedure TPluginForm.FormActivate(Sender: TObject);
begin
  if fPlugin<>NIL then
    fPlugin.EditActivate;
end;

procedure TPluginForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fPlugin.CloseEdit;
end;

procedure TPluginForm.FormDeactivate(Sender: TObject);
begin
  fPlugin.EditDeactivate
end;

procedure TPluginForm.LoadPresets(Files: TStrings);
var
  i, j, k: Integer;
  s: string;
begin
 j := FCurProg;
 for i := 0 to Files.Count - 1 do
  begin
   if i > 0 then fPlugin.CurrentProgram := j + i;
   try
    fPlugin.LoadPreset(Files[i]);
   except
    MessageDlg('ERROR: Preset file not for this plugin (or file is corrupted)!', mtError, [mbOK], 0);
    Exit;
   end;
   k := fPlugin.CurrentProgram;
   s := IntToStr(k);
   if k < 10 then s := '00' + s else
   if k < 100 then s := '0' + s;
  end;
end;

procedure TPluginForm.MISavePresetClick(Sender: TObject);
var
  s2: string;
begin
 Sleep(2);
 with TSaveDialog.Create(Self) do
  try
   DefaultExt := '.fxp';
   filename := '*.fxp';
   Filter := 'preset files (*.fxp)|*.fxp';
   Title := 'Select a preset';
   InitialDir := FDirPreset;
   Options := [ofForceShowHidden];
{$IFNDEF FPC}
   Ctl3D := False;
{$ENDIF}

   s2 := PresetBox.Items[PresetBox.ItemIndex];
   s2 := Copy(s2, 6, Length(s2) - 5);
{$IFNDEF FPC}
   Filename := MakeGoodFileName(s2) + '.fxp';
{$ENDIF}

   if Execute then
    begin
     fPlugin.SavePreset(FileName);
     FDirPreset := extractfiledir(filename);
    end;
  finally
   Free;
  end;
end;

procedure TPluginForm.MILoadBankClick(Sender: TObject);
begin
 Sleep(2);
 with TOpenDialog.Create(Self) do
  try
   DefaultExt := '.fxb';
   filename := '*.fxb';
   Filter := 'bank files (*.fxb)|*.fxb';
   Title := 'Select a bank';
   InitialDir := FDirPreset;

   Options := [ofFileMustExist, ofForceShowHidden];
   {$IFNDEF FPC}
   Ctl3D := False;
   {$ENDIF}

   if Execute then
    begin
     FDirPreset := ExtractFileDir(filename);
     try
      fPlugin.LoadBank(Filename);
     except
      MessageDlg('ERROR: Bank file not for this plugin (or file is corrupted)!', mtError, [mbOK], 0);
     end;
     BuildPresetList;
    end;
  finally
   Free;
   FCurProg := 0;
   fPlugin.CurrentProgram := 0;
   PresetBox.ItemIndex := 0;
  end;
end;

procedure TPluginForm.MISaveBankClick(Sender: TObject);
begin
 with TSaveDialog.Create(Self) do
  try
   FileName := '*.fxb';
   DefaultExt := '.fxb';
   Filter := 'bank files (*.fxb)|*.fxb';
   Title := 'Select a bank';
   InitialDir := FDirPreset;
   Options := [ofForceShowHidden];
   {$IFNDEF FPC}
   Ctl3D := False;
   {$ENDIF}
   if Execute then
    begin
     FDirPreset := ExtractFileDir(filename);
     fPlugin.SaveBank(FileName);
    end;
  finally
   Free;
  end;
end;

procedure TPluginForm.SetPreset(Sender: TObject);
begin
 fPlugin.CurrentProgram := (Sender as TMenuItem).Tag;
end;

procedure TPluginForm.MIRenamePresetClick(Sender: TObject);
var
  s2, s: string;
begin
 s := InputBox('Rename Preset', 'New name:', string(fPlugin.GetProgramName));
 fPlugin.SetProgramName(AnsiString(s));
 fPlugin.Idle;
 fPlugin.EditIdle;

 s2 := IntToStr(FCurProg);
 if FCurProg < 10 then s2 := '00' + s2 else
 if FCurProg < 100 then s2 := '0' + s2;

 PresetBox.Items[FCurProg] := s2 + ': ' + s;
end;

procedure TPluginForm.MIShowPresetClick(Sender: TObject);
var
  s: AnsiString;
begin
 MIShowPreset.Checked := not MIShowPreset.Checked;
 s := AnsiString(IntToStr(FCurProg));
 if FCurProg < 10 then s := '00' + s else
 if FCurProg < 100 then s := '0' + s;
 if MIShowPreset.Checked
  then Caption := string(FTitle + ' - ' + s + ': ' + FCurProgName)
  else Caption := string(FTitle);
end;


procedure TPluginForm.PresetBoxClick(Sender: TObject);
begin
 fPlugin.CurrentProgram := PresetBox.ItemIndex;
 FCurProg := PresetBox.ItemIndex;
end;

procedure TPluginForm.PresetBoxKeyPress(Sender: TObject; var Key: Char);
begin
 Key := #0;
end;

procedure TPluginForm.PresetBoxDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
 if Index < 0 then Exit;
 PresetBox.Canvas.FillRect(Rect);
 PresetBox.Canvas.TextOut(rect.Left + 2, rect.top, PresetBox.items[index]);
end;

procedure TPluginForm.PresetBoxChange(Sender: TObject);
begin
 PnStatus.SetFocus;
end;


initialization

end.
