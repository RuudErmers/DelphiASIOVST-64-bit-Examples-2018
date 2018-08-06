unit MBCGUI;

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
  Forms, Graphics, Controls, StdCtrls, ExtCtrls, DAV_Types, DAV_VSTModule, 
  DAV_GuiBaseControl, DAV_GuiLevelMeter, DAV_GuiStitchedControls, 
  DAV_GuiStitchedPngList, DAV_GuiStitchedDial, DAV_GuiSlider,
  DAV_GuiImageControl, DAV_GuiCustomControl;

type
  TFmMBC = class(TForm)
    CBLimiter: TCheckBox;
    DialMasterGain: TGuiStitchedDial;
    DlHighAttack: TGuiStitchedDial;
    DlHighGain: TGuiStitchedDial;
    DlHighRatio: TGuiStitchedDial;
    DlHighRelease: TGuiStitchedDial;
    DlHighThreshold: TGuiStitchedDial;
    DlLowAttack: TGuiStitchedDial;
    DlLowGain: TGuiStitchedDial;
    DlLowRatio: TGuiStitchedDial;
    DlLowRelease: TGuiStitchedDial;
    DlLowThreshold: TGuiStitchedDial;
    DlMidAttack: TGuiStitchedDial;
    DlMidGain: TGuiStitchedDial;
    DlMidRatio: TGuiStitchedDial;
    DlMidRelease: TGuiStitchedDial;
    DlMidThreshold: TGuiStitchedDial;
    GSPL: TGuiStitchedPNGList;
    GuiTimer: TTimer;
    LbAbout1: TLabel;
    LbAbout2: TLabel;
    LbCrossover: TLabel;
    LbHigh0: TLabel;
    LbHighAttack: TLabel;
    LbHighAttackValue: TLabel;
    LbHighBand: TLabel;
    LbHighBandVU: TLabel;
    LbHighdB: TLabel;
    LbHighFreqHz: TLabel;
    LbHighGain: TLabel;
    LbHighGaindB: TLabel;
    LbHighHalf: TLabel;
    LbHighInf: TLabel;
    LbHighInput: TLabel;
    LbHighRatio: TLabel;
    LbHighRatioValue: TLabel;
    LbHighRed: TLabel;
    LbHighRed0: TLabel;
    LbHighRed1: TLabel;
    LbHighRelease: TLabel;
    LbHighReleaseValue: TLabel;
    LbHighThreshold: TLabel;
    LbHighThresholddB: TLabel;
    LbIn0: TLabel;
    LbIndB: TLabel;
    LbInHalf: TLabel;
    LbInInf: TLabel;
    LbInput: TLabel;
    LbInputL: TLabel;
    LbInputR: TLabel;
    LbLow0: TLabel;
    LbLow1: TLabel;
    LbLowAttack: TLabel;
    LbLowAttackValue: TLabel;
    LbLowBand: TLabel;
    LbLowBandVU: TLabel;
    LbLowdB: TLabel;
    LbLowFreqHz: TLabel;
    LbLowGain: TLabel;
    LbLowGaindB: TLabel;
    LbLowHalf: TLabel;
    LbLowInf: TLabel;
    LbLowInput: TLabel;
    LbLowRatio: TLabel;
    LbLowRatioValue: TLabel;
    LbLowRed: TLabel;
    LbLowRed0: TLabel;
    LbLowRelease: TLabel;
    LbLowReleaseValue: TLabel;
    LbLowThreshold: TLabel;
    LbLowThresholddB: TLabel;
    LbMasterGain: TLabel;
    LbMasterGaindB: TLabel;
    LbMid0: TLabel;
    LbMidAttack: TLabel;
    LbMidAttackValue: TLabel;
    LbMidBand: TLabel;
    LbMidBandVU: TLabel;
    LbMiddB: TLabel;
    LbMidGain: TLabel;
    LbMidGaindB: TLabel;
    LbMidHalf: TLabel;
    LbMidInf: TLabel;
    LbMidInput: TLabel;
    LbMidRatio: TLabel;
    LbMidRatioValue: TLabel;
    LbMidRed: TLabel;
    LbMidRed0: TLabel;
    LbMidRed1: TLabel;
    LbMidRelease: TLabel;
    LbMidReleaseValue: TLabel;
    LbMidThreshold: TLabel;
    LbMidThresholddB: TLabel;
    LbOut0: TLabel;
    LbOutdB: TLabel;
    LbOutHalf: TLabel;
    LbOutInf: TLabel;
    LbOutput: TLabel;
    LbOutputLeft: TLabel;
    LbOutputRight: TLabel;
    LMHighLeft: TGuiColorLevelMeter;
    LMHighRed: TGuiColorLevelMeter;
    LMHighRight: TGuiColorLevelMeter;
    LMInLeft: TGuiColorLevelMeter;
    LMInRight: TGuiColorLevelMeter;
    LMLowLeft: TGuiColorLevelMeter;
    LMLowRed: TGuiColorLevelMeter;
    LMLowRight: TGuiColorLevelMeter;
    LMMidLeft: TGuiColorLevelMeter;
    LMMidRed: TGuiColorLevelMeter;
    LMMidRight: TGuiColorLevelMeter;
    LMOutLeft: TGuiColorLevelMeter;
    LMOutRight: TGuiColorLevelMeter;
    RBBWIIR: TRadioButton;
    RbLPFIR: TRadioButton;
    SbLowFreq: TGuiSlider;
    SbHighFreq: TGuiSlider;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure SbLowFreqChange(Sender: TObject);
    procedure SbHighFreqChange(Sender: TObject);
    procedure DlLowGainChange(Sender: TObject);
    procedure DlLowThresholdChange(Sender: TObject);
    procedure DlLowRatioChange(Sender: TObject);
    procedure DlLowAttackChange(Sender: TObject);
    procedure DlLowReleaseChange(Sender: TObject);
    procedure DlMidThresholdChange(Sender: TObject);
    procedure DlMidRatioChange(Sender: TObject);
    procedure DlMidAttackChange(Sender: TObject);
    procedure DlMidReleaseChange(Sender: TObject);
    procedure DlMidGainChange(Sender: TObject);
    procedure DlHighThresholdChange(Sender: TObject);
    procedure DlHighRatioChange(Sender: TObject);
    procedure DlHighAttackChange(Sender: TObject);
    procedure DlHighReleaseChange(Sender: TObject);
    procedure DlHighGainChange(Sender: TObject);
    procedure MeterInPaint(Sender: TObject);
    procedure MeterOutPaint(Sender: TObject);
    procedure GuiTimerTimer(Sender: TObject);
    procedure DialMasterGainChange(Sender: TObject);
  private
    FBackground : TBitmap;
  public
    procedure UpdateHighRelease;
    procedure UpdateMidRelease;
    procedure UpdateLowRelease;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, DAV_Common, MBCDM;

procedure TFmMBC.FormCreate(Sender: TObject);
begin
 FBackground := TBitmap.Create;
end;

procedure TFmMBC.FormDestroy(Sender: TObject);
begin
 FBackground.Free;
end;

procedure TFmMBC.GuiTimerTimer(Sender: TObject);
begin
 with TMBCDataModule(Owner) do
  begin
   LMInLeft.PeakLevel    := InputPeakLeft;
   LMInRight.PeakLevel   := InputPeakRight;
   LMOutLeft.PeakLevel   := OutputPeakLeft;
   LMOutRight.PeakLevel  := OutputPeakRight;
   LMLowLeft.PeakLevel   := LowInputPeakLeft;
   LMLowRight.PeakLevel  := LowInputPeakRight;
   LMLowRed.PeakLevel    := LowGainReduction;
   LMMidLeft.PeakLevel   := MidInputPeakLeft;
   LMMidRight.PeakLevel  := MidInputPeakRight;
   LMMidRed.PeakLevel    := MidGainReduction;
   LMHighLeft.PeakLevel  := HighInputPeakLeft;
   LMHighRight.PeakLevel := HighInputPeakRight;
   LMHighRed.PeakLevel   := HighGainReduction;
  end;
end;

procedure TFmMBC.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, FBackground);
end;

procedure TFmMBC.FormResize(Sender: TObject);
var
  x, y : Integer;
begin
 with FBackground do
  begin
   Width := ClientWidth;
   Height := ClientHeight;
   for x := 0 to Width - 1 do
    for y := 0 to Height - 1 do
     if (x mod 2 = 0) and (y mod 2 = 0)
      then Canvas.Pixels[X, Y] := $9BA0A2
      else Canvas.Pixels[X, Y] := $BAC0C3;
   Canvas.MoveTo(10, 30);
   Canvas.LineTo(Width - 10, 30);
   Canvas.Brush.Color := clBtnFace;
   Canvas.Pen.Color   := $8C9091;
   Canvas.Rectangle(20, 40, 251, 181);
   Canvas.Rectangle(260, 40, 326, 181);
   Canvas.Rectangle(335, 40, 401, 181);
   Canvas.Rectangle(410, 40, 521, 181);
   Canvas.Rectangle(530, 40, 641, 181);
   Canvas.Rectangle(650, 40, 761, 181);

   Canvas.Rectangle(20, 190, 261, 300);
   Canvas.Rectangle(270, 190, 511, 300);
   Canvas.Rectangle(520, 190, 761, 300);
  end;
end;

procedure TFmMBC.FormShow(Sender: TObject);
begin
 UpdateLowRelease;
 UpdateMidRelease;
 UpdateHighRelease;
end;

procedure TFmMBC.MeterInPaint(Sender: TObject);
begin
 with TPaintBox(Sender).Canvas do
  begin
   Brush.Color := clBlack;
   FrameRect(ClipRect);
  end;
end;

procedure TFmMBC.MeterOutPaint(Sender: TObject);
begin
 with TPaintBox(Sender).Canvas do
  begin
   Brush.Color := clBlack;
   FrameRect(ClipRect);
  end;
end;

procedure TFmMBC.DlLowGainChange(Sender: TObject);
begin
 (Owner as TMBCDataModule).Parameter[0] := DlLowGain.Value;
end;

procedure TFmMBC.DlLowThresholdChange(Sender: TObject);
begin
 (Owner as TMBCDataModule).Parameter[3] := DlLowThreshold.Value;
end;

procedure TFmMBC.DlLowRatioChange(Sender: TObject);
begin
 (Owner as TMBCDataModule).Parameter[4] := Power(10, DlLowRatio.Value);
end;

procedure TFmMBC.DlLowAttackChange(Sender: TObject);
begin
 (Owner as TMBCDataModule).Parameter[5] := Power(10, DlLowAttack.Value);
end;

procedure TFmMBC.DlLowReleaseChange(Sender: TObject);
begin
 (Owner as TMBCDataModule).Parameter[6] := Power(10, DlLowRelease.Value);
end;

procedure TFmMBC.SbLowFreqChange(Sender: TObject);
begin
 (Owner as TMBCDataModule).Parameter[1] := FreqLinearToLog(SbLowFreq.Value * 0.0001);
end;

procedure TFmMBC.DlMidGainChange(Sender: TObject);
begin
 (Owner as TMBCDataModule).Parameter[7] := DlMidGain.Value;
end;

procedure TFmMBC.DlMidThresholdChange(Sender: TObject);
begin
 (Owner as TMBCDataModule).Parameter[8] := DlMidThreshold.Value;
end;

procedure TFmMBC.DlMidRatioChange(Sender: TObject);
begin
 (Owner as TMBCDataModule).Parameter[9] := Power(10, DlMidRatio.Value);
end;

procedure TFmMBC.DlMidAttackChange(Sender: TObject);
begin
 (Owner as TMBCDataModule).Parameter[10] := Power(10, DlMidAttack.Value);
end;

procedure TFmMBC.DlMidReleaseChange(Sender: TObject);
begin
 (Owner as TMBCDataModule).Parameter[11] := Power(10, DlMidRelease.Value);
end;

procedure TFmMBC.DlHighGainChange(Sender: TObject);
begin
 (Owner as TMBCDataModule).Parameter[14] := DlHighGain.Value
end;

procedure TFmMBC.DlHighThresholdChange(Sender: TObject);
begin
 (Owner as TMBCDataModule).Parameter[15] := DlHighThreshold.Value;
end;

procedure TFmMBC.DlHighRatioChange(Sender: TObject);
begin
 (Owner as TMBCDataModule).Parameter[16] := Power(10, DlHighRatio.Value);
end;

procedure TFmMBC.DialMasterGainChange(Sender: TObject);
begin
 LbMasterGaindB.Caption := FloatToStrF(DialMasterGain.Value, ffGeneral, 5, 2) + 'dB';
end;

procedure TFmMBC.DlHighAttackChange(Sender: TObject);
begin
 (Owner as TMBCDataModule).Parameter[17] := Power(10, DlHighAttack.Value);
end;

procedure TFmMBC.DlHighReleaseChange(Sender: TObject);
begin
 (Owner as TMBCDataModule).Parameter[18] := Power(10, DlHighRelease.Value);
end;

procedure TFmMBC.SbHighFreqChange(Sender: TObject);
begin
 with TMBCDataModule(Owner) do
  begin
   Parameter[12] := FreqLinearToLog(SbHighFreq.Value * 0.0001);
  end;
end;

procedure TFmMBC.UpdateLowRelease;
var
  Release: Single;
begin
 with TMBCDataModule(Owner) do
  begin
   Release := Log10(Parameter[6]);
   if DlLowRelease.Value <> Release
    then DlLowRelease.Value := Release;
   LbLowReleaseValue.Caption := FloatToStrF(Parameter[6], ffGeneral, 3, 2) + ' ms';
  end;
end;

procedure TFmMBC.UpdateMidRelease;
var
  Release: Single;
begin
 with TMBCDataModule(Owner) do
  begin
   Release := Log10(Parameter[11]);
   if DlMidRelease.Value <> Release
    then DlMidRelease.Value := Release;
   LbMidReleaseValue.Caption := FloatToStrF(Parameter[11], ffGeneral, 3, 2) + ' ms';
  end;
end;

procedure TFmMBC.UpdateHighRelease;
var
  Release: Single;
begin
 with TMBCDataModule(Owner) do
  begin
   Release := Log10(Parameter[18]);
   if DlHighRelease.Value <> Release
    then DlHighRelease.Value := Release;
   LbHighReleaseValue.Caption := FloatToStrF(Parameter[18], ffGeneral, 3, 2) + ' ms';
  end;
end;

end.
