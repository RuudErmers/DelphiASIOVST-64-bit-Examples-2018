unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Menus,
  DAV_GuiCheckBox, DAV_GuiFont, DAV_GuiShadow, DAV_GuiRadioButton, DAV_GuiGroup;

type
  TFmMegaDemo = class(TForm)
    CbGroupNative: TGuiControlsCheckBox;
    CbGroupShadow: TGuiControlsCheckBox;
    CbItem1: TGuiControlsCheckBox;
    CbItem2: TGuiControlsCheckBox;
    CbItem3: TGuiControlsCheckBox;
    CbItem4: TGuiControlsCheckBox;
    CbItem5: TGuiControlsCheckBox;
    GbSide: TGuiGroupSide;
    GbTop: TGuiGroupTop;
    GbTypical: TGuiGroup;
    MainMenu: TMainMenu;
    MiDemo: TMenuItem;
    MiExit: TMenuItem;
    MiFontOversampling: TMenuItem;
    MiFontOversampling2x: TMenuItem;
    MiFontOversampling3x: TMenuItem;
    MiFontOversampling4x: TMenuItem;
    MiFontOversamplingNone: TMenuItem;
    MiFontShadow: TMenuItem;
    MiFontShadowEnabled: TMenuItem;
    MiLayout: TMenuItem;
    MiNative: TMenuItem;
    N1: TMenuItem;
    PC: TPageControl;
    RbItem1: TGuiControlsRadioButton;
    RbItem2: TGuiControlsRadioButton;
    RbItem3: TGuiControlsRadioButton;
    RbItem4: TGuiControlsRadioButton;
    RbItem5: TGuiControlsRadioButton;
    TsCheckBox: TTabSheet;
    TsGroupBox: TTabSheet;
    TsRadioButton: TTabSheet;
    procedure MiExitClick(Sender: TObject);
    procedure MiNativeClick(Sender: TObject);
    procedure MiFontShadowEnabledClick(Sender: TObject);
    procedure MiFontOversamplingClick(Sender: TObject);
  end;

var
  FmMegaDemo: TFmMegaDemo;

implementation

{$R *.dfm}

procedure TFmMegaDemo.MiExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmMegaDemo.MiFontOversamplingClick(Sender: TObject);
var
  FontOS     : TFontOversampling;
  TabIndex   : Integer;
  Index      : Integer;
  GroupIndex : Integer;
begin
 if Sender is TMenuItem
  then TMenuItem(Sender).Checked := True;

 if MiFontOversamplingNone.Checked then FontOS := foNone else
 if MiFontOversampling2x.Checked then FontOS := fo2x else
 if MiFontOversampling3x.Checked then FontOS := fo3x else
 if MiFontOversampling4x.Checked then FontOS := fo4x
  else FontOS := fo6x;

 for TabIndex := 0 to PC.ControlCount - 1 do
  if PC.Controls[TabIndex] is TTabSheet then
   with TTabSheet(PC.Controls[TabIndex]) do
    for Index := 0 to ControlCount - 1 do
     if Controls[Index] is TGuiControlsRadioButton then
      with TGuiControlsRadioButton(Controls[Index])
       do FontOversampling := FontOS else
     if Controls[Index] is TGuiControlsCheckBox then
      with TGuiControlsCheckBox(Controls[Index])
       do FontOversampling := FontOS else
     if Controls[Index] is TCustomGuiGroup then
      with TCustomGuiGroup(Controls[Index]) do
       begin
        FontOversampling := FontOS;
        for GroupIndex := 0 to ControlCount - 1 do
         if Controls[GroupIndex] is TGuiControlsRadioButton then
          with TGuiControlsRadioButton(Controls[GroupIndex])
           do FontOversampling := FontOS else
         if Controls[GroupIndex] is TGuiControlsCheckBox then
          with TGuiControlsCheckBox(Controls[GroupIndex])
           do FontOversampling := FontOS;
       end;
end;

procedure TFmMegaDemo.MiFontShadowEnabledClick(Sender: TObject);
var
  TabIndex   : Integer;
  Index      : Integer;
  GroupIndex : Integer;
begin
 MiFontShadowEnabled.Checked := not MiFontShadowEnabled.Checked;

 for TabIndex := 0 to PC.ControlCount - 1 do
  if PC.Controls[TabIndex] is TTabSheet then
   with TTabSheet(PC.Controls[TabIndex]) do
    for Index := 0 to ControlCount - 1 do
     if Controls[Index] is TGuiControlsRadioButton then
      with TGuiControlsRadioButton(Controls[Index])
       do Shadow.Visible := MiFontShadowEnabled.Checked else
     if Controls[Index] is TGuiControlsCheckBox then
      with TGuiControlsCheckBox(Controls[Index])
       do Shadow.Visible := MiFontShadowEnabled.Checked else
     if Controls[Index] is TCustomGuiGroup then
      with TCustomGuiGroup(Controls[Index]) do
       begin
        Shadow.Visible := MiFontShadowEnabled.Checked;
        for GroupIndex := 0 to ControlCount - 1 do
         if Controls[GroupIndex] is TGuiControlsRadioButton then
          with TGuiControlsRadioButton(Controls[GroupIndex])
           do Shadow.Visible := MiFontShadowEnabled.Checked else
         if Controls[GroupIndex] is TGuiControlsCheckBox then
          with TGuiControlsCheckBox(Controls[GroupIndex])
           do Shadow.Visible := MiFontShadowEnabled.Checked;
       end;
end;

procedure TFmMegaDemo.MiNativeClick(Sender: TObject);
var
  TabIndex   : Integer;
  Index      : Integer;
  GroupIndex : Integer;
begin
 MiNative.Checked := not MiNative.Checked;

 for TabIndex := 0 to PC.ControlCount - 1 do
  if PC.Controls[TabIndex] is TTabSheet then
   with TTabSheet(PC.Controls[TabIndex]) do
    for Index := 0 to ControlCount - 1 do
     if Controls[Index] is TGuiControlsRadioButton then
      with TGuiControlsRadioButton(Controls[Index])
       do Native := MiNative.Checked else
     if Controls[Index] is TGuiControlsCheckBox then
      with TGuiControlsCheckBox(Controls[Index])
       do Native := MiNative.Checked else
     if Controls[Index] is TCustomGuiGroup then
      with TCustomGuiGroup(Controls[Index]) do
       begin
        Native := MiNative.Checked;
        for GroupIndex := 0 to ControlCount - 1 do
         if Controls[GroupIndex] is TGuiControlsRadioButton then
          with TGuiControlsRadioButton(Controls[GroupIndex])
           do Native := MiNative.Checked else
         if Controls[GroupIndex] is TGuiControlsCheckBox then
          with TGuiControlsCheckBox(Controls[GroupIndex])
           do Native := MiNative.Checked;
       end;
end;

end.

