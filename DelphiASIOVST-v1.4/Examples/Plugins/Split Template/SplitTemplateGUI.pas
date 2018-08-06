unit SplitTemplateGUI;

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
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, DAV_Types,
  DAV_GuiButton, DAV_GuiLabel, DAV_GuiLED, DAV_GuiDial, DAV_GuiPanel,
  DAV_GuiSelectBox, DAV_GuiCommon, DAV_GuiCustomControl, DAV_GuiGraphicControl,
  DAV_GuiBaseControl;

type
  TFmSplitter = class(TForm)
    BtHigh: TGuiButton;
    BtLow: TGuiButton;
    DialOversampling: TGuiDial;
    DialSplitFrequency: TGuiDial;
    DialSplitOrder: TGuiDial;
    GuiLEDOversampling: TGuiLED;
    PnControl: TGuiPanel;
    LbOversampling: TGuiLabel;
    LbOversamplingFactor: TGuiLabel;
    LbSplitFrequency: TGuiLabel;
    LbSplitOrder: TGuiLabel;
    PnGui: TPanel;
    ShBorder: TShape;
    SBMode: TGuiSelectBox;
    procedure FormShow(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure BtHighClick(Sender: TObject);
    procedure BtLowClick(Sender: TObject);
    procedure DialOversamplingChange(Sender: TObject);
    procedure DialSplitFrequencyChange(Sender: TObject);
    procedure DialSplitOrderChange(Sender: TObject);
    procedure GuiLEDOversamplingClick(Sender: TObject);
    procedure SBModeChange(Sender: TObject);
  private
    FBackground : TBitmap;
  public
    procedure UpdateMode;
    procedure UpdateFrequency;
    procedure UpdateOrder;
    procedure UpdateOSFactor;
    procedure UpdateOverSampling;
    procedure ShowPlugin(Index: Integer);
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  DAV_Common, DAV_VSTModuleWithPrograms, SplitTemplateDM;

procedure TFmSplitter.BtLowClick(Sender: TObject);
begin
 BtLow.ButtonColor  := $0018CF1D;
 BtHigh.ButtonColor := $00626C71;
 ShowPlugin(0);
end;

procedure TFmSplitter.BtHighClick(Sender: TObject);
begin
 BtLow.ButtonColor  := $00626C71;
 BtHigh.ButtonColor := $0018CF1D;
 ShowPlugin(1);
end;

procedure TFmSplitter.SBModeChange(Sender: TObject);
begin
 with TSplitTemplateDataModule(Owner) do
  begin
   ParameterByName['Mode'] := SBMode.ItemIndex;
  end;
end;

procedure TFmSplitter.ShowPlugin(Index: Integer);
var
  R        : TRect;
  Oversize : Integer;
begin
 with TSplitTemplateDataModule(Owner) do
  begin
   if VstHost[Index].Active and not VstHost[Index].EditVisible
    then VstHost[Index].ShowEdit(PnGui);
   if VstHost[1 - Index].EditVisible then VstHost[1 - Index].CloseEdit;

   PluginVisible    := Index;
   PnGui.Visible    := Assigned(VstHost[Index]) and VstHost[Index].Active;
   ShBorder.Visible := PnGui.Visible;

   // set plugin GUI size
   if PnGui.Visible then
    begin
     PnGui.Visible    := True;
     ShBorder.Visible := True;

     R        := VstHost[Index].GetRect;
     Oversize := PnControl.Width - (R.Right - R.Left);
     if Oversize < 0 then
      begin
       // current editor is too small, enlarge!
       PnGui.Align := alClient;
       ShBorder.Visible := False;
      end
     else
      begin
       PnGui.Align  := alNone;
       PnGui.Left   := Oversize div 2;
       PnGui.Width  := (R.Right - R.Left);

       // calculate new height and y position
       PnGui.Height := (R.Bottom - R.Top);
       PnGui.Top    := PnControl.Height + (ClientHeight - PnControl.Height - PnGui.Height) div 2;

       // show border
       ShBorder.Visible := True;
       ShBorder.SetBounds(PnGui.Left - ShBorder.Pen.Width,
                          PnGui.Top - ShBorder.Pen.Width,
                          PnGui.Width + 2 * ShBorder.Pen.Width,
                          PnGui.Height + 2 * ShBorder.Pen.Width);
      end;
    end;
  end;
end;

procedure TFmSplitter.DialOversamplingChange(Sender: TObject);
begin
 with TSplitTemplateDataModule(Owner) do
  begin
   ParameterByName['OS Factor'] := DialOversampling.Position;
  end;
end;

procedure TFmSplitter.DialSplitFrequencyChange(Sender: TObject);
begin
 with TSplitTemplateDataModule(Owner) do
  begin
   ParameterByName['Frequency'] := DialSplitFrequency.Position;
  end;
end;

procedure TFmSplitter.DialSplitOrderChange(Sender: TObject);
begin
 with TSplitTemplateDataModule(Owner) do
  begin
   ParameterByName['Order'] := DialSplitOrder.Position;
  end;
end;

procedure TFmSplitter.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 with TSplitTemplateDataModule(Owner) do
  try
   if VstHost[0].EditVisible then VstHost[0].CloseEdit;
   if VstHost[1].EditVisible then VstHost[1].CloseEdit;
  except 
  end;
end;

procedure TFmSplitter.FormDestroy(Sender: TObject);
begin
 if Assigned(FBackground)
  then FreeAndNil(FBackground);
end;

procedure TFmSplitter.FormPaint(Sender: TObject);
begin
 if Assigned(FBackground)
  then Canvas.Draw(0, PnControl.Height, FBackground);
end;

procedure Lighten(var Pixel: TRGB24; Amount: Byte);
begin
 Pixel.B := Round($2B - Amount);
 Pixel.G := Round($31 - Amount);
 Pixel.R := Round($33 - Amount);
end;

procedure TFmSplitter.FormResize(Sender: TObject);
var
  x, y   : Integer;
  s      : array [0..1] of Single;
  h, hr  : Single;
  Line   : PRGB24Array;
  b      : Byte;
begin
 // Create Background Image (if not already done
 if not Assigned(FBackground)
  then FBackground := TBitmap.Create;
 with FBackground do
  begin
   PixelFormat := pf24bit;
   Width := ClientWidth;
   Height := ClientHeight - PnControl.Height;
   if Height < 2 then exit;
   s[0] := 0;
   s[1] := 0;
   hr   := 1 / Height;

   // start with separator
   Line := Scanline[0];
   for x := 0 to Width - 1 do
    begin
     s[1] := 0.97 * s[0] + 0.03 * random;
     s[0] := s[1];
     b    := Round(-$2A * s[1]);
     Line[x].B := $2B + b;
     Line[x].G := $31 + b;
     Line[x].R := $33 + b;
    end;

   for y := 1 to Height - 1 do
    begin
     Line := Scanline[y];
     h    := 0.3 * (1 - sqr(2 * (y - Height div 2) * hr));
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.97 * s[0] + 0.03 * random;
       s[0] := s[1];
       b    := Round($2A * (s[1] + h));
       Line[x].B := $2B + b;
       Line[x].G := $31 + b;
       Line[x].R := $33 + b;
      end;
    end;
  end;
end;

procedure TFmSplitter.FormShow(Sender: TObject);
begin
 UpdateMode;
 UpdateFrequency;
 UpdateOrder;
 UpdateOSFactor;
 ShowPlugin(0);
end;

procedure TFmSplitter.GuiLEDOversamplingClick(Sender: TObject);
begin
 with TSplitTemplateDataModule(Owner) do
  begin
   ParameterByName['Oversampling'] := Limit(1 - ParameterByName['Oversampling'], 0, 1);
  end;
end;

procedure TFmSplitter.UpdateMode;
begin
 with TSplitTemplateDataModule(Owner) do
  begin
   SBMode.ItemIndex := Round(ParameterByName['Mode']);
//   GuiLEDSplit.Brightness_Percent := 20 + 60 * (1 - f_Limit(0.5 * ParameterByName['Mode'], 0, 1));
   DialSplitFrequency.Visible := Round(ParameterByName['Mode']) < 3;
   LbSplitFrequency.Visible   := DialSplitFrequency.Visible;
   DialSplitOrder.Visible     := DialSplitFrequency.Visible;
   LbSplitOrder.Visible       := DialSplitFrequency.Visible;
   case SplitType of
      stSimple,
         stLiRi : begin
                   BtLow.Caption := 'Low';
                   BtHigh.Caption := 'High';
                  end;
          stDyn : begin
                   BtLow.Caption  := 'Quiet';
                   BtHigh.Caption := 'Loud';
                  end;
    stLeftRight : begin
                   BtLow.Caption  := 'Left';
                   BtHigh.Caption := 'Right';
                  end;
           stMS : begin
                   BtLow.Caption  := 'Mid';
                   BtHigh.Caption := 'Side';
                  end;
       stSerial : begin
                   BtLow.Caption  := 'Stage 1';
                   BtHigh.Caption := 'Stage 2';
                  end;
    stTransient : begin
                   BtLow.Caption  := 'Trans.';
                   BtHigh.Caption := 'Resid.';
                  end;
          stLFO : begin
                   BtLow.Caption  := 'A';
                   BtHigh.Caption := 'B';
                  end;
         stSpin : begin
                   BtLow.Caption  := 'Front';
                   BtHigh.Caption := 'Back';
                  end;
       stBypass : begin
                   BtLow.Caption  := 'A';
                   BtHigh.Caption := 'B';
                  end;
   end;
  end;
end;

procedure TFmSplitter.UpdateFrequency;
var
  Freq : Single;
begin
 with TSplitTemplateDataModule(Owner) do
  begin
   Freq := ParameterByName['Frequency'];
   if DialSplitFrequency.Position <> Freq
    then DialSplitFrequency.Position := Freq;
   if Freq < 1000
    then LbSplitFrequency.Caption := FloatToStrF(Freq, ffGeneral, 3, 3) + 'Hz'
    else LbSplitFrequency.Caption := FloatToStrF(1E-3 * Freq, ffGeneral, 3, 3) + 'kHz';
  end;
end;

procedure TFmSplitter.UpdateOrder;
var
  Order : Single;
begin
 with TSplitTemplateDataModule(Owner) do
  begin
   Order := ParameterByName['Order'];
   if DialSplitOrder.Position <> Order
    then DialSplitOrder.Position := Order;
   if SplitType = stLiRi
    then LbSplitOrder.Caption := ConvertOrderToString(2 * Round(Order))
    else LbSplitOrder.Caption := ConvertOrderToString(Round(Order));
  end;
end;

procedure TFmSplitter.UpdateOverSampling;
begin
 with TSplitTemplateDataModule(Owner) do
  begin
   GuiLEDOversampling.Brightness_Percent := 20 + 60 * (Limit(ParameterByName['Oversampling'], 0, 1));

   DialOversampling.Visible     := Round(ParameterByName['Oversampling']) = 1;
   LbOversamplingFactor.Visible := DialOversampling.Visible;
   LbOversampling.Width         := 97 + 5 * Round(ParameterByName['Oversampling']);
  end;
end;

procedure TFmSplitter.UpdateOSFactor;
var
  OSFactor : Single;
begin
 with TSplitTemplateDataModule(Owner) do
  begin
   OSFactor := ParameterByName['OS Factor'];
   if DialOversampling.Position <> OSFactor
    then DialOversampling.Position := OSFactor;
   LbOversamplingFactor.Caption := IntToStr(Round(OSFactor)) + 'x';
  end;
end;

end.
