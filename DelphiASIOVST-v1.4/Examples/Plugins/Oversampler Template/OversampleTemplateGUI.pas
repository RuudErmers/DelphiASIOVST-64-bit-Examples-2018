unit OversampleTemplateGUI;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Version: MPL 1.1 or LGPL 2.1 with linking exception                       //
//                                                                            //
//  The contents of this file are subject to the Mozilla Public License       //
//  Version 1.1 (the "License"); you may not use this file except in          //
//  compliance with the License. You may obtain a copy of the License at      //
//  http://www.mozilla.org/MPL/                                               //
//                                                                            //
//  Software distributed under the License is distributed on an "AS IS"       //
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the   //
//  License for the specific language governing rights and limitations under  //
//  the License.                                                              //
//                                                                            //
//  Alternatively, the contents of this file may be used under the terms of   //
//  the Free Pascal modified version of the GNU Lesser General Public         //
//  License Version 2.1 (the "FPC modified LGPL License"), in which case the  //
//  provisions of this license are applicable instead of those above.         //
//  Please see the file LICENSE.txt for additional information concerning     //
//  this license.                                                             //
//                                                                            //
//  The code is part of the Delphi ASIO & VST Project                         //
//                                                                            //
//  The initial developer of this code is Christian-W. Budde                  //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2009-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, Menus, DAV_Types,
  DAV_GuiBaseControl, DAV_GuiLabel, DAV_GuiLED, DAV_GuiDial, DAV_GuiPanel,
  DAV_VSTWindowSizer, DAV_GuiGraphicControl;

type
  TFmOversampler = class(TForm)
    DialOversampling: TGuiDial;
    LblOversampling: TGuiLabel;
    LblOversamplingFactor: TGuiLabel;
    LedOversampling: TGuiLED;
    MiAllowResizing: TMenuItem;
    MiManualIdle: TMenuItem;
    PnControl: TGuiPanel;
    PnGui: TPanel;
    PUSettings: TPopupMenu;
    ShBorder: TShape;
    procedure FormShow(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure DialOversamplingChange(Sender: TObject);
    procedure LedOversamplingClick(Sender: TObject);
    procedure MiAllowResizingClick(Sender: TObject);
    procedure MiManualIdleClick(Sender: TObject);
  private
    FBackground: TBitmap;
    FWinSizer: TVstWindowSizer;
  public
    procedure UpdateOSFactor;
    procedure UpdateOverSampling;
    procedure ShowPlugin;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, OversampleTemplateDM, DAV_GuiCommon, DAV_VSTModuleWithPrograms;

{ TFmOversampler }

procedure TFmOversampler.FormDestroy(Sender: TObject);
begin
  if Assigned(FBackground) then
    FreeAndNil(FBackground);
  if Assigned(FWinSizer) then
    FreeAndNil(FWinSizer);
end;

procedure TFmOversampler.FormShow(Sender: TObject);
begin
  UpdateOSFactor;
  UpdateOverSampling;
  ShowPlugin;
end;

procedure TFmOversampler.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  with TOversampleTemplateDataModule(Owner) do
    try
      if VstHost[0].EditVisible then
        VstHost[0].CloseEdit;
      if VstHost[1].EditVisible then
        VstHost[1].CloseEdit;
      if Assigned(FWinSizer) then
        FreeAndNil(FWinSizer);
      MiAllowResizing.Checked := False;
    except
    end;
end;

procedure TFmOversampler.FormPaint(Sender: TObject);
begin
  if Assigned(FBackground) then
    Canvas.Draw(0, PnControl.Height, FBackground);
end;

procedure TFmOversampler.ShowPlugin;
var
  R: TRect;
  Oversize: Integer;
begin
  with TOversampleTemplateDataModule(Owner) do
  begin
    if VstHost[0].Active and not VstHost[0].EditVisible then
      VstHost[0].ShowEdit(PnGui);

    PnGui.Visible := Assigned(VstHost[0]) and VstHost[0].Active;
    ShBorder.Visible := PnGui.Visible;

    // set plugin GUI size
    if PnGui.Visible then
    begin
      PnGui.Visible := True;
      ShBorder.Visible := True;

      R := VstHost[0].GetRect;
      Oversize := PnControl.Width - (R.Right - R.Left);
      if Oversize <= 0 then
      begin
        // current editor is too small, enlarge!
        PnGui.Align := alClient;
        ShBorder.Visible := False;
        MiAllowResizing.Enabled := False;
      end
      else
      begin
        PnGui.Align := alNone;
        PnGui.Left := Oversize div 2;
        PnGui.Width := (R.Right - R.Left);

        // calculate new height and y position
        PnGui.Height := (R.Bottom - R.Top);
        PnGui.Top := PnControl.Height +
          (ClientHeight - PnControl.Height - PnGui.Height) div 2;

        // show border
        ShBorder.Visible := True;
        ShBorder.SetBounds(PnGui.Left - ShBorder.Pen.Width,
          PnGui.Top - ShBorder.Pen.Width, PnGui.Width + 2 * ShBorder.Pen.Width,
          PnGui.Height + 2 * ShBorder.Pen.Width);
        MiAllowResizing.Enabled := True;
      end;
    end;
  end;
end;

procedure Lighten(var Pixel: TRGB24; Amount: Byte);
begin
  Pixel.B := Round($2B - Amount);
  Pixel.G := Round($31 - Amount);
  Pixel.R := Round($33 - Amount);
end;

procedure TFmOversampler.FormResize(Sender: TObject);
var
  x, y: Integer;
  s: array [0 .. 1] of Single;
  h, hr: Single;
  Line: PRGB24Array;
  B: Byte;
begin
  // Create Background Image (if not already done
  if not Assigned(FBackground) then
    FBackground := TBitmap.Create;
  with FBackground do
  begin
    PixelFormat := pf24bit;
    Width := ClientWidth;
    Height := ClientHeight - PnControl.Height;
    if Height < 2 then
      exit;
    s[0] := 0;
    s[1] := 0;
    hr := 1 / Height;

    // start with separator
    Line := Scanline[0];
    for x := 0 to Width - 1 do
    begin
      s[1] := 0.97 * s[0] + 0.03 * random;
      s[0] := s[1];
      B := Round(-$2A * s[1]);
      Line[x].B := $2B + B;
      Line[x].G := $31 + B;
      Line[x].R := $33 + B;
    end;

    for y := 1 to Height - 1 do
    begin
      Line := Scanline[y];
      h := 0.3 * (1 - sqr(2 * (y - Height div 2) * hr));
      for x := 0 to Width - 1 do
      begin
        s[1] := 0.97 * s[0] + 0.03 * random;
        s[0] := s[1];
        B := Round($2A * (s[1] + h));
        Line[x].B := $2B + B;
        Line[x].G := $31 + B;
        Line[x].R := $33 + B;
      end;
    end;
  end;
end;

procedure TFmOversampler.DialOversamplingChange(Sender: TObject);
begin
  with TOversampleTemplateDataModule(Owner) do
  begin
    ParameterByName['OS Factor'] := DialOversampling.Position;
  end;
end;

procedure TFmOversampler.LedOversamplingClick(Sender: TObject);
begin
  with TOversampleTemplateDataModule(Owner) do
  begin
    ParameterByName['Oversampling'] :=
      EnsureRange(1 - ParameterByName['Oversampling'], 0, 1);
  end;
end;

procedure TFmOversampler.MiAllowResizingClick(Sender: TObject);
begin
  MiAllowResizing.Checked := not MiAllowResizing.Checked;
  if MiAllowResizing.Checked then
  begin
    if not Assigned(FWinSizer) then
      FWinSizer := TVstWindowSizer.Create;
    FWinSizer.Effect := TOversampleTemplateDataModule(Owner);
    FWinSizer.SetEditorHwnd(Self.Handle);
  end
  else
  begin
    if Assigned(FWinSizer) then
      FreeAndNil(FWinSizer);
  end;
end;

procedure TFmOversampler.MiManualIdleClick(Sender: TObject);
begin
  MiManualIdle.Checked := not MiManualIdle.Checked;
  TOversampleTemplateDataModule(Owner).ManualIdle := MiManualIdle.Checked;
end;

procedure TFmOversampler.UpdateOverSampling;
begin
  with TOversampleTemplateDataModule(Owner) do
  begin
    LedOversampling.Brightness_Percent := 20 + 60 *
      (EnsureRange(ParameterByName['Oversampling'], 0, 1));

    DialOversampling.Visible := Round(ParameterByName['Oversampling']) = 1;
    LblOversamplingFactor.Visible := DialOversampling.Visible;
    LblOversampling.Width := 80 + 5 * Round(ParameterByName['Oversampling']);
  end;
end;

procedure TFmOversampler.UpdateOSFactor;
var
  OSFactor: Single;
begin
  with TOversampleTemplateDataModule(Owner) do
  begin
    OSFactor := ParameterByName['OS Factor'];
    if DialOversampling.Position <> OSFactor then
      DialOversampling.Position := OSFactor;
    LblOversamplingFactor.Caption := IntToStr(Round(OSFactor)) + 'x';
  end;
end;

end.
