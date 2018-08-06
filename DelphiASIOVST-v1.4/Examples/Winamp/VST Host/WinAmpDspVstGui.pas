unit WinAmpDspVstGui;

interface

{$I DAV_Compiler.inc}

uses
  Windows, Classes, Forms, SysUtils, Registry, Menus, Dialogs, ExtCtrls,
  StdCtrls, Controls, Graphics, DAV_Common, DAV_VstHost;

type
  TFmWinAmpVST = class(TForm)
    MILoadDLL: TMenuItem;
    MIRecent: TMenuItem;
    MIReset: TMenuItem;
    MIResetDll: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    PUVSTPlugin: TPopupMenu;
    PnGUI: TPanel;
    PanelControl: TPanel;
    LbProgram: TLabel;
    LbPlugin: TLabel;
    EdVSTName: TEdit;
    CBPreset: TComboBox;
    CBEnhance: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CBEnhanceClick(Sender: TObject);
    procedure CBPresetChange(Sender: TObject);
    procedure CBPresetDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure EdVSTNameClick(Sender: TObject);
    procedure LbPluginClick(Sender: TObject);
    procedure MILoadDLLClick(Sender: TObject);
    procedure MIResetClick(Sender: TObject);
    procedure MIResetDllClick(Sender: TObject);
    procedure PUVSTPluginPopup(Sender: TObject);
  private
    FColDetected   : Boolean;
    FColorBack     : TColor;
    FColorEdit     : TColor;
    FColorBorder   : TColor;
    procedure SetScheme;
    procedure LoadRecent(Sender: TObject);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    procedure UpdatePluginInformation;
  end;

implementation

{$R *.DFM}

uses
  Math, DAV_GuiCommon, DAV_VSTEffect, WinAmpDspVst;

{ TFmWinAmpVST }

procedure TFmWinAmpVST.FormCreate(Sender: TObject);
begin
 with TWinAmpObject(Owner), TRegistry.Create do
  try
   if OpenKeyReadOnly(RegistryKey) then
    begin
     if ValueExists('Left') then Left := ReadInteger('Left');
     if ValueExists('Top') then Top := ReadInteger('Top');
    end;
  finally
   CloseKey;
   Free;
  end;
end;

procedure TFmWinAmpVST.FormShow(Sender: TObject);
begin
 UpdatePluginInformation;
end;

procedure TFmWinAmpVST.FormActivate(Sender: TObject);
begin
 SetScheme;
end;

procedure TFmWinAmpVST.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 with TWinAmpObject(Owner), TRegistry.Create do
  try
   if OpenKey(RegistryKey, True) then
    try
     WriteInteger('Left', Left);
     WriteInteger('Top', Top);
     if not ValueExists('Dispose Preset')
      then WriteBool('Dispose Preset', True);
    finally
     CloseKey;
    end;
   if assigned(VstHost) and (VstHost[0].EditVisible)
    then VstHost[0].CloseEdit;
  finally
   Free;
  end;
end;

procedure TFmWinAmpVST.CreateParams(var Params: TCreateParams);
begin
// Params.WndParent := TWinAmpObject(Owner).WinAmpDspModule.HwndParent;
 inherited CreateParams(Params);
end;

procedure TFmWinAmpVST.SetScheme;
var
  BMP     : TBitmap;
  SCL     : PRGB24Array;
  R, G, B : Integer;
  x       : Integer;
begin
 // make sure the VST host exists
 if (not assigned(TWinAmpObject(Owner).VSTHost)) or
    (not TWinAmpObject(Owner).VSTHost[0].EditVisible) then exit;

 // process messages to ensure drawing has taken place
 if assigned(Application)
  then Application.ProcessMessages;

 sleep(10);
    
 if assigned(Application)
  then Application.ProcessMessages;

 if TWinAmpObject(Owner).VSTHost[0].Active then
  begin
   BMP := TBitmap.Create;
   BMP.PixelFormat := pf24bit;
   with BMP do
    try
     TWinAmpObject(Owner).VSTHost[0].RenderEditorToBitmap(BMP);
     SCL := BMP.ScanLine[0];
     R := 0; G := 0; B := 0;

     for x := 0 to BMP.Width - 1 do
      begin
       R := R + SCL[x].R;
       G := G + SCL[x].G;
       B := B + SCL[x].B;
      end;

     FColorEdit      := RGB(R div (2 * BMP.Width), G div (2 * BMP.Width), B div (2 * BMP.Width));
     FColorBack      := RGB(R div BMP.Width, G div BMP.Width, B div BMP.Width);
     FColorBorder    := RGB(6 * R div (5 * BMP.Width), 6 * G div (5 * BMP.Width), 6 * B div (5 * BMP.Width));
     EdVSTName.Color := FColorEdit;
     CBPreset.Color  := FColorEdit;
     Self.Color      := FColorBack;

     // set font color
     if (R div BMP.Width) + (G div BMP.Width) + (B div BMP.Width) < 384
      then LbPlugin.Font.Color := clWhite
      else LbPlugin.Font.Color := clBlack;
     LbProgram.Font.Color  := LbPlugin.Font.Color;
     CBEnhance.Font.Color  := LbPlugin.Font.Color;
     EdVSTName.Font.Color  := LbPlugin.Font.Color;
     CBPreset.Font.Color   := LbPlugin.Font.Color;

     FColDetected        := True;
    finally
     FreeAndNil(BMP);
    end;
  end;
end;

procedure TFmWinAmpVST.UpdatePluginInformation;
var
  rct  : ERect;
  i    : Integer;
  s    : string;
  temp : AnsiString;
begin
 // make sure the VST host exists
 if not assigned(TWinAmpObject(Owner).VSTHost)
  then exit;

 with TWinAmpObject(Owner).VSTHost[0] do
  begin
   if GetVendorString = '' then
    if GetEffectName = ''
     then Caption := ExtractFileName(DLLFileName)
     else Caption := string(GetEffectName)
    else Caption := string(GetVendorString + ' - '  + GetEffectName);

   CBPreset.Clear;
   if not Active then exit;
   for i := 0 to numPrograms - 1 do
    begin
     GetProgramNameIndexed(-1, i, temp);
     s := IntToStr(i);
     if i < 10 then s := '00' + s else
     if i < 100 then s := '0' + s;
     s := s + ' - ' + string(temp);
     CBPreset.Items.Add(s)
    end;
   CBPreset.ItemIndex := 0;

   try
    s := string(GetProgramName);
    s := IntToStr(CBPreset.ItemIndex) + ' - ' + s;
    if CBPreset.ItemIndex < 10 then s := '00' + s else
    if CBPreset.ItemIndex < 100 then s := '0' + s;
    if (CBPreset.Text <> s) then
     begin
      CBPreset.Text := s;
      for i := 0 to numPrograms - 1 do
       begin
        CurrentProgram := i;
        s := string(GetProgramName);
        s := IntToStr(i) + ' - ' + s;
        if i < 10 then s := '00' + s else
        if i < 100 then s := '0' + s;
        CBPreset.Items[i] := s;
       end;
      CurrentProgram := 0;
      CBPreset.ItemIndex := 0;
     end;
   except
   end;

   try
    if effFlagsHasEditor in EffectOptions then
     begin
      EditIdle;
      rct := EditGetRect;
      if (rct.right <> 0) and (rct.Bottom <> 0) then
       begin
        ClientWidth := Max(rct.right - rct.left, 60);
        ClientHeight := rct.bottom - rct.Top + PanelControl.Height;
       end;
      CBEnhance.Visible := ClientWidth >= 386;
      if CBEnhance.Visible
       then CBPreset.Width := ClientWidth - CBPreset.Left - 8 - CBEnhance.Width
       else CBPreset.Width := ClientWidth - CBPreset.Left - 8;

      FColDetected := False;
      LbProgram.Visible := True;
     end
    else
     begin
      i := numParams * 36;
      if i < 112 then i := 112 else
      if i > 400 then i := 400;
      ClientHeight := i;
      ClientWidth := 386;
      CBEnhance.Visible := True;
     end;
    Idle;
    if not FColDetected
     then SetScheme;
   except
   end;

   if GetEffectName = '' then
    begin
     EdVSTName.Text := ExtractFileName(DLLFileName);
     EdVSTName.Text := Copy(EdVSTName.Text, 0, Pos('.dll', EdVSTName.Text) - 1);
    end else EdVSTName.Text := string(GetEffectName);
  end;
end;

procedure TFmWinAmpVST.MILoadDLLClick(Sender: TObject);
var
  i : Integer;
begin
 with TWinAmpObject(Owner), TOpenDialog.Create(Self) do
  try
   InitialDir := VSTHost.PlugInDir;
   with TRegistry.Create do
    try
     if OpenKey(RegistryKey, False) then
      if ValueExists('Last Plugin')
       then InitialDir := ExtractFilePath(ReadString('Last Plugin'));
    finally
     CloseKey; Free;
    end;
   DefaultExt := '.dll';
   Filter := 'VST Plugin (*.dll)|*.dll';
   Options := [ofHideReadOnly, ofFileMustExist, ofEnableSizing];
   Title := 'Load VST Plugin DLL';
   if Execute then
    begin
     LoadVSTDLL(FileName);
     with TRegistry.Create do
      try
       if OpenKey(RegistryKey, true) then
        begin
         WriteString('Last Plugin', FileName);
         i := 1;
         while i < 10 do
          begin
           if not ValueExists('Recent ' + IntToStr(i)) then break;
           if ReadString('Recent ' + IntToStr(i)) = Filename then break;
           inc(i);
          end;
         if i <= 10 then
          begin
           while i > 1 do
            begin
             WriteString('Recent ' + IntToStr(i), ReadString('Recent ' + IntToStr(i - 1)));
             dec(i);
            end;
           WriteString('Recent 1', FileName);
          end;
        end;
      finally
       CloseKey; Free;
      end;
    end;
  finally
   Free;
  end;
end;

procedure TFmWinAmpVST.EdVSTNameClick(Sender: TObject);
begin
 if TWinAmpObject(Owner).VstHost[0].DLLFileName <> ''
  then PUVSTPlugin.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

procedure TFmWinAmpVST.CBPresetChange(Sender: TObject);
begin
 with TWinAmpObject(Owner).VSTHost[0] do
  begin
   CurrentProgram := CBPreset.ItemIndex;
   EditIdle; Idle;
  end;
end;

procedure TFmWinAmpVST.PUVSTPluginPopup(Sender: TObject);
var
  i  : Integer;
  MI : TMenuItem;
begin
 while MIRecent.Count > 2 do MIRecent.Delete(0);
 with TWinAmpObject(Owner), TRegistry.Create do
  try
   if OpenKey(RegistryKey, True) then
    begin
     i := 1;
     while ValueExists('Recent ' + IntToStr(i)) and (i < 10) do
      try
       MI := TMenuItem.Create(MIRecent);
       MI.Caption := ExtractFileName(ReadString('Recent ' + IntToStr(i)));
       MI.Tag := i;
       MI.OnClick := LoadRecent;
       MIRecent.Insert(i - 1, MI);
      finally
       inc(i);
      end;
    end;
  finally
   CloseKey;
   Free;
  end;
end;

procedure TFmWinAmpVST.LoadRecent(Sender: TObject);
var
  str : string;
  i   : Integer;
begin
 with TWinAmpObject(Owner), TRegistry.Create do
  try
   if OpenKey(RegistryKey, True) then
     begin
      str := ReadString('Recent ' + IntToStr(TMenuItem(Sender).Tag));
      if FileExists(str) then
       try
        LoadVSTDLL(str);
       finally
        i := TMenuItem(Sender).Tag;
        while i > 1 do
         begin
          WriteString('Recent ' + IntToStr(i), ReadString('Recent ' + IntToStr(i - 1)));
          dec(i);
         end;
        WriteString('Recent 1', str);
       end;
      WriteString('Last Plugin', str);
     end;
  finally
   CloseKey;
   Free;
  end;
end;

procedure TFmWinAmpVST.CBPresetDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
 if Index < 0 then exit;

 with Control As TComboBox do
  try
   if odSelected in State
    then Canvas.Brush.color := FColorEdit + $00101010
    else Canvas.Brush.color := FColorEdit;
   Canvas.FillRect(Rect);
   Canvas.TextOut(Rect.Left + 2, Rect.top, items[index]);
  except
  end;
end;

procedure TFmWinAmpVST.MIResetClick(Sender: TObject);
var
  i : Integer;
begin
 with TWinAmpObject(Owner), TRegistry.Create do
  try
   if OpenKey(RegistryKey, False) then
    for i := 0 to 9 do if ValueExists('Recent ' + IntToStr(i))
     then DeleteValue('Recent ' + IntToStr(i));
  finally
   CloseKey; Free;
  end;
end;

procedure TFmWinAmpVST.MIResetDllClick(Sender: TObject);
begin
 with TWinAmpObject(Owner), VstHost[0] do
  try
   CloseEdit;
   Active := False;
   Unload;
   DLLFileName := '';
  except
  end;

 EdVSTName.Text := '';
 with TWinAmpObject(Owner), TRegistry.Create do
  try
   if OpenKey(RegistryKey, False)
    then DeleteValue('Last Plugin');
  finally
   CloseKey;
   Free;
  end;
 LbProgram.Visible := False;
 ClientHeight := 29;
 ClientWidth := 160;
end;

procedure TFmWinAmpVST.LbPluginClick(Sender: TObject);
begin
 with TWinAmpObject(Owner) do
  begin
   Bypass := not Bypass;
   if Bypass
    then
     begin
      PanelControl.Font.Color := $00808080;
      LbPlugin.Font.Color  := PanelControl.Font.Color;
      LbProgram.Font.Color := PanelControl.Font.Color;
      EdVSTName.Font.Color := PanelControl.Font.Color;
      CBPreset.Font.Color  := PanelControl.Font.Color;
      CBEnhance.Font.Color := PanelControl.Font.Color;
     end
    else
     begin
      if VstHost[0].DLLFileName <> ''
       then SetScheme
       else
        begin
         // set font color
         if (PanelControl.Color and $FF) +
            (PanelControl.Color shr 8 and $FF) +
            (PanelControl.Color shr 16 and $FF) < 384
          then LbPlugin.Font.Color := clWhite
          else LbPlugin.Font.Color := clBlack;
         LbProgram.Font.Color  := LbPlugin.Font.Color;
         CBEnhance.Font.Color  := LbPlugin.Font.Color;
         EdVSTName.Font.Color  := LbPlugin.Font.Color;
         CBPreset.Font.Color   := LbPlugin.Font.Color;
        end;
     end;
  end;
end;

procedure TFmWinAmpVST.CBEnhanceClick(Sender: TObject);
begin
 with TWinAmpObject(Owner) do
  begin
   Enhanced := CBEnhance.Checked;
  end;
end;

end.
