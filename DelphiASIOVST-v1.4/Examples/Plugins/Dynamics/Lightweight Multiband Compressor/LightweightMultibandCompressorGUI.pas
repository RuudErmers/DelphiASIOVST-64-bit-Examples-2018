unit LightweightMultibandCompressorGUI;

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
  Forms, ExtCtrls, Controls, DAV_Types, DAV_VSTModule, DAV_GuiLabel, 
  DAV_GuiBaseControl, DAV_GuiGraphXY, DAV_GuiLED, DAV_GuiPanel, 
  DAV_GuiStitchedControls, DAV_GuiStitchedPngList, DAV_GuiStitchedDial,
  DAV_GuiImageControl, DAV_GuiCustomControl, DAV_GuiGraphicControl;

type
  TGraph = (gLow, gLowMid, gHighMid, gHigh);
  TGraphs = set of TGraph;
  TFmLightweightMultibandCompressor = class(TForm)
    DialHighAttack: TGuiStitchedDial;
    DialHighFreq: TGuiStitchedDial;
    DialHighKnee: TGuiStitchedDial;
    DialHighMakeUpGain: TGuiStitchedDial;
    DialHighMidAttack: TGuiStitchedDial;
    DialHighMidKnee: TGuiStitchedDial;
    DialHighMidMakeUpGain: TGuiStitchedDial;
    DialHighMidRatio: TGuiStitchedDial;
    DialHighMidRelease: TGuiStitchedDial;
    DialHighMidThreshold: TGuiStitchedDial;
    DialHighRatio: TGuiStitchedDial;
    DialHighRelease: TGuiStitchedDial;
    DialHighThreshold: TGuiStitchedDial;
    DialLowAttack: TGuiStitchedDial;
    DialLowFreq: TGuiStitchedDial;
    DialLowKnee: TGuiStitchedDial;
    DialLowMakeUpGain: TGuiStitchedDial;
    DialLowMidAttack: TGuiStitchedDial;
    DialLowMidKnee: TGuiStitchedDial;
    DialLowMidMakeUpGain: TGuiStitchedDial;
    DialLowMidRatio: TGuiStitchedDial;
    DialLowMidRelease: TGuiStitchedDial;
    DialLowMidThreshold: TGuiStitchedDial;
    DialLowRatio: TGuiStitchedDial;
    DialLowRelease: TGuiStitchedDial;
    DialLowThreshold: TGuiStitchedDial;
    DialMidFreq: TGuiStitchedDial;
    GSPL: TGuiStitchedPNGList;
    GXYHigh: TGuiGraphXY;
    GXYHighMid: TGuiGraphXY;
    GXYLow: TGuiGraphXY;
    GXYLowMid: TGuiGraphXY;
    LbHigh: TGuiLabel;
    LbHighAttack: TGuiLabel;
    LbHighAttackValue: TGuiLabel;
    LbHighAutogain: TGuiLabel;
    LbHighBypass: TGuiLabel;
    LbHighFreqValue: TGuiLabel;
    LbHighKnee: TGuiLabel;
    LbHighKneeValue: TGuiLabel;
    LbHighMakeUpGain: TGuiLabel;
    LbHighMakeUpGainValue: TGuiLabel;
    LbHighMid: TGuiLabel;
    LbHighMidAttack: TGuiLabel;
    LbHighMidAttackValue: TGuiLabel;
    LbHighMidAutogain: TGuiLabel;
    LbHighMidBypass: TGuiLabel;
    LbHighMidKnee: TGuiLabel;
    LbHighMidKneeValue: TGuiLabel;
    LbHighMidMakeUpGain: TGuiLabel;
    LbHighMidMakeUpGainValue: TGuiLabel;
    LbHighMidMute: TGuiLabel;
    LbHighMidRatio: TGuiLabel;
    LbHighMidRatioValue: TGuiLabel;
    LbHighMidRelease: TGuiLabel;
    LbHighMidReleaseValue: TGuiLabel;
    LbHighMidSolo: TGuiLabel;
    LbHighMidThreshold: TGuiLabel;
    LbHighMidThresholdValue: TGuiLabel;
    LbHighMute: TGuiLabel;
    LbHighRatio: TGuiLabel;
    LbHighRatioValue: TGuiLabel;
    LbHighRelease: TGuiLabel;
    LbHighReleaseValue: TGuiLabel;
    LbHighSolo: TGuiLabel;
    LbHighThreshold: TGuiLabel;
    LbHighThresholdValue: TGuiLabel;
    LbLow: TGuiLabel;
    LbLowAttack: TGuiLabel;
    LbLowAttackValue: TGuiLabel;
    LbLowAutogain: TGuiLabel;
    LbLowBypass: TGuiLabel;
    LbLowFreqValue: TGuiLabel;
    LbLowKnee: TGuiLabel;
    LbLowKneeValue: TGuiLabel;
    LbLowMakeUpGain: TGuiLabel;
    LbLowMakeUpGainValue: TGuiLabel;
    LbLowMid: TGuiLabel;
    LbLowMidAttack: TGuiLabel;
    LbLowMidAttackValue: TGuiLabel;
    LbLowMidAutogain: TGuiLabel;
    LbLowMidBypass: TGuiLabel;
    LbLowMidKnee: TGuiLabel;
    LbLowMidKneeValue: TGuiLabel;
    LbLowMidMakeUpGain: TGuiLabel;
    LbLowMidMakeUpGainValue: TGuiLabel;
    LbLowMidMute: TGuiLabel;
    LbLowMidRatio: TGuiLabel;
    LbLowMidRatioValue: TGuiLabel;
    LbLowMidRelease: TGuiLabel;
    LbLowMidReleaseValue: TGuiLabel;
    LbLowMidSolo: TGuiLabel;
    LbLowMidThreshold: TGuiLabel;
    LbLowMidThresholdValue: TGuiLabel;
    LbLowMute: TGuiLabel;
    LbLowRatio: TGuiLabel;
    LbLowRatioValue: TGuiLabel;
    LbLowRelease: TGuiLabel;
    LbLowReleaseValue: TGuiLabel;
    LbLowS: TGuiLabel;
    LbLowThreshold: TGuiLabel;
    LbLowThresholdValue: TGuiLabel;
    LbMidFreqValue: TGuiLabel;
    LbSoftClip: TGuiLabel;
    LEDHighAutoGain: TGuiLED;
    LEDHighMidAutoGain: TGuiLED;
    LEDLowAutoGain: TGuiLED;
    LEDLowMidAutoGain: TGuiLED;
    LEDSoftClip: TGuiLED;
    PnHighBand: TGuiPanel;
    PnHighMidBand: TGuiPanel;
    PnLowBand: TGuiPanel;
    PnLowMidBand: TGuiPanel;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DialAttackChange(Sender: TObject);
    procedure DialHighFreqChange(Sender: TObject);
    procedure DialKneeChange(Sender: TObject);
    procedure DialLowFreqChange(Sender: TObject);
    procedure DialMakeUpGainChange(Sender: TObject);
    procedure DialMidFreqChange(Sender: TObject);
    procedure DialRatioChange(Sender: TObject);
    procedure DialReleaseChange(Sender: TObject);
    procedure DialThresholdChange(Sender: TObject);
    procedure LEDHighAutoGainClick(Sender: TObject);
    procedure LEDHighMidAutoGainClick(Sender: TObject);
    procedure LEDLowAutoGainClick(Sender: TObject);
    procedure LEDLowMidAutoGainClick(Sender: TObject);
    procedure LEDSoftClipClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    function EvaluateHighCharacteristic(Sender: TObject; X: Double): Double;
    function EvaluateLowMidCharacteristic(Sender: TObject; X: Double): Double;
    function EvaluateHighMidCharacteristic(Sender: TObject; X: Double): Double;
    function EvaluateLowCharacteristic(Sender: TObject; X: Double): Double;
  private
    FGraphNeedUpdate : TGraphs;
    procedure SetGraphNeedUpdate(const Value: TGraphs);
  public
    procedure UpdateLimit;
    procedure UpdateLowAttack;
    procedure UpdateLowAutoMakeUpGain;
    procedure UpdateLowFrequency;
    procedure UpdateLowKnee;
    procedure UpdateLowMakeUp;
    procedure UpdateLowRatio;
    procedure UpdateLowRelease;
    procedure UpdateLowThreshold;
    procedure UpdateLowMidAttack;
    procedure UpdateLowMidAutoMakeUpGain;
    procedure UpdateLowMidKnee;
    procedure UpdateLowMidMakeUp;
    procedure UpdateLowMidRatio;
    procedure UpdateLowMidRelease;
    procedure UpdateLowMidThreshold;
    procedure UpdateMidFrequency;
    procedure UpdateHighMidAttack;
    procedure UpdateHighMidAutoMakeUpGain;
    procedure UpdateHighMidKnee;
    procedure UpdateHighMidMakeUp;
    procedure UpdateHighMidRatio;
    procedure UpdateHighMidRelease;
    procedure UpdateHighMidThreshold;
    procedure UpdateHighAttack;
    procedure UpdateHighAutoMakeUpGain;
    procedure UpdateHighFrequency;
    procedure UpdateHighKnee;
    procedure UpdateHighMakeUp;
    procedure UpdateHighRatio;
    procedure UpdateHighRelease;
    procedure UpdateHighThreshold;
    property GraphNeedUpdate: TGraphs read FGraphNeedUpdate write SetGraphNeedUpdate;
  end;

implementation

uses
  DAV_Common, DAV_VSTModuleWithPrograms, LightweightMultibandCompressorDM;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmLightweightMultibandCompressor.FormCreate(Sender: TObject);
begin
 with TGuiGraphXYFunctionSeries(GXYLow[0].Series) do
  begin
   OnEvaluate := EvaluateLowCharacteristic;
  end;
 with TGuiGraphXYFunctionSeries(GXYLowMid[0].Series) do
  begin
   OnEvaluate := EvaluateLowMidCharacteristic;
  end;
 with TGuiGraphXYFunctionSeries(GXYHighMid[0].Series) do
  begin
   OnEvaluate := EvaluateHighMidCharacteristic;
  end;
 with TGuiGraphXYFunctionSeries(GXYHigh[0].Series) do
  begin
   OnEvaluate := EvaluateHighCharacteristic;
  end;
end;

procedure TFmLightweightMultibandCompressor.FormShow(Sender: TObject);
begin
 UpdateLowAttack;
 UpdateLowAutoMakeUpGain;
 UpdateLowKnee;
 UpdateLowMakeUp;
 UpdateLowRatio;
 UpdateLowRelease;
 UpdateLowThreshold;
 UpdateLowFrequency;
 UpdateLowMidAttack;
 UpdateLowMidAutoMakeUpGain;
 UpdateLowMidKnee;
 UpdateLowMidMakeUp;
 UpdateLowMidRatio;
 UpdateLowMidRelease;
 UpdateLowMidThreshold;
 UpdateMidFrequency;
 UpdateHighMidAttack;
 UpdateHighMidAutoMakeUpGain;
 UpdateHighMidKnee;
 UpdateHighMidMakeUp;
 UpdateHighMidRatio;
 UpdateHighMidRelease;
 UpdateHighMidThreshold;
 UpdateHighAttack;
 UpdateHighAutoMakeUpGain;
 UpdateHighKnee;
 UpdateHighMakeUp;
 UpdateHighRatio;
 UpdateHighRelease;
 UpdateHighThreshold;
 UpdateHighFrequency;
 UpdateLimit;
end;

function TFmLightweightMultibandCompressor.EvaluateLowCharacteristic(Sender: TObject;
  X: Double): Double;
begin
 result := TLightweightMultibandCompressorDataModule(Owner).EvaluateLowCharacteristic(X);
end;

function TFmLightweightMultibandCompressor.EvaluateLowMidCharacteristic(Sender: TObject;
  X: Double): Double;
begin
 result := TLightweightMultibandCompressorDataModule(Owner).EvaluateLowMidCharacteristic(X);
end;

function TFmLightweightMultibandCompressor.EvaluateHighMidCharacteristic(Sender: TObject;
  X: Double): Double;
begin
 result := TLightweightMultibandCompressorDataModule(Owner).EvaluateHighMidCharacteristic(X);
end;

function TFmLightweightMultibandCompressor.EvaluateHighCharacteristic(Sender: TObject;
  X: Double): Double;
begin
 result := TLightweightMultibandCompressorDataModule(Owner).EvaluateHighCharacteristic(X);
end;

// Low Frequency

procedure TFmLightweightMultibandCompressor.DialLowFreqChange(Sender: TObject);
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Parameter[0] := DialLowFreq.Value;
  end;
end;

// Mid Frequency

procedure TFmLightweightMultibandCompressor.DialMidFreqChange(Sender: TObject);
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Parameter[1] := DialMidFreq.Value;
  end;
end;

// High Frequency

procedure TFmLightweightMultibandCompressor.DialHighFreqChange(Sender: TObject);
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Parameter[2] := DialHighFreq.Value;
  end;
end;

// Soft Clip

procedure TFmLightweightMultibandCompressor.LEDSoftClipClick(Sender: TObject);
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Parameter[3] := Integer(LEDSoftClip.Brightness_Percent < 50);
  end;
end;

procedure TFmLightweightMultibandCompressor.SetGraphNeedUpdate(const Value: TGraphs);
begin
 if FGraphNeedUpdate <> Value then
  begin
   FGraphNeedUpdate := Value;
   Timer.Enabled := FGraphNeedUpdate <> [];
  end;
end;

procedure TFmLightweightMultibandCompressor.TimerTimer(Sender: TObject);
begin
 if gLow in GraphNeedUpdate then GXYLow.UpdateGraph;
 if gLowMid in GraphNeedUpdate then GXYLowMid.UpdateGraph;
 if gHighMid in GraphNeedUpdate then GXYHighMid.UpdateGraph;
 if gHigh in GraphNeedUpdate then GXYHigh.UpdateGraph;
 GraphNeedUpdate := [];
end;

procedure TFmLightweightMultibandCompressor.DialAttackChange(Sender: TObject);
begin
 with TLightweightMultibandCompressorDataModule(Owner), TGuiStitchedDial(Sender) do
  begin
   Parameter[4 + Tag * 7] := Value;
  end;
end;

procedure TFmLightweightMultibandCompressor.DialReleaseChange(Sender: TObject);
begin
 with TLightweightMultibandCompressorDataModule(Owner), TGuiStitchedDial(Sender) do
  begin
   Parameter[5 + Tag * 7] := Value;
  end;
end;

procedure TFmLightweightMultibandCompressor.DialThresholdChange(Sender: TObject);
begin
 with TLightweightMultibandCompressorDataModule(Owner), TGuiStitchedDial(Sender) do
  begin
   Parameter[6 + Tag * 7] := Value;
  end;
end;

procedure TFmLightweightMultibandCompressor.DialRatioChange(Sender: TObject);
begin
 with TLightweightMultibandCompressorDataModule(Owner), TGuiStitchedDial(Sender) do
  begin
   Parameter[7 + Tag * 7] := Value;
  end;
end;

procedure TFmLightweightMultibandCompressor.DialKneeChange(Sender: TObject);
begin
 with TLightweightMultibandCompressorDataModule(Owner), TGuiStitchedDial(Sender) do
  begin
   Parameter[8 + Tag * 7] := Value;
  end;
end;

procedure TFmLightweightMultibandCompressor.DialMakeUpGainChange(Sender: TObject);
begin
 with TLightweightMultibandCompressorDataModule(Owner), TGuiStitchedDial(Sender) do
  begin
   Parameter[9 + Tag * 7] := Value;
  end;
end;

procedure TFmLightweightMultibandCompressor.LEDLowAutoGainClick(Sender: TObject);
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   AutoGain[0] := LEDLowAutoGain.Brightness_Percent < 50;
   if not DialLowMakeUpGain.Enabled then UpdateLowMakeUp;
  end;
end;

procedure TFmLightweightMultibandCompressor.LEDLowMidAutoGainClick(Sender: TObject);
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   AutoGain[1] := LEDLowMidAutoGain.Brightness_Percent < 50;
   if not DialLowMidMakeUpGain.Enabled then UpdateLowMidMakeUp;
  end;
end;

procedure TFmLightweightMultibandCompressor.LEDHighMidAutoGainClick(Sender: TObject);
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   AutoGain[2] := LEDHighMidAutoGain.Brightness_Percent < 50;
   if not DialHighMidMakeUpGain.Enabled then UpdateHighMidMakeUp;
  end;
end;

procedure TFmLightweightMultibandCompressor.LEDHighAutoGainClick(Sender: TObject);
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   AutoGain[3] := LEDHighAutoGain.Brightness_Percent < 50;
   if not DialHighMakeUpGain.Enabled then UpdateHighMakeUp;
  end;
end;

////////////
// Update //
////////////

procedure TFmLightweightMultibandCompressor.UpdateLowFrequency;
var
  Freq : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Freq := Parameter[0];
   if Freq <> DialLowFreq.Value
    then DialLowFreq.Value := Freq;
   DialMidFreq.Min := DialLowFreq.Value;
   LbLowFreqValue.Caption := string(ParameterDisplay[0] + ' ' + ParameterLabel[0]);
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateMidFrequency;
var
  Freq : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Freq := Parameter[1];
   if Freq <> DialMidFreq.Value
    then DialMidFreq.Value := Freq;
   DialHighFreq.Min := DialMidFreq.Value;
   DialLowFreq.Max := DialMidFreq.Value;
   LbMidFreqValue.Caption := string(ParameterDisplay[1] + ' ' + ParameterLabel[1]);
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateHighFrequency;
var
  Freq : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Freq := Parameter[2];
   if Freq <> DialHighFreq.Value
    then DialHighFreq.Value := Freq;
   DialMidFreq.Max := DialHighFreq.Value;
   LbHighFreqValue.Caption := string(ParameterDisplay[2] + ' ' + ParameterLabel[2]);
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateLowAttack;
var
  Attack : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Attack := Parameter[4];
   if Attack <> DialLowAttack.Value
    then DialLowAttack.Value := Attack;
   LbLowAttackValue.Caption := string(ParameterDisplay[4] + ' ' + ParameterLabel[4]);
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateLowRelease;
var
  Release : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Release := Parameter[5];
   if Release <> DialLowRelease.Value
    then DialLowRelease.Value := Release;
   LbLowReleaseValue.Caption := string(ParameterDisplay[5] + ' ' + ParameterLabel[5]);
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateLowThreshold;
var
  Threshold : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Threshold := Parameter[6];
   if Threshold <> DialLowThreshold.Value
    then DialLowThreshold.Value := Threshold;
   LbLowThresholdValue.Caption := string(ParameterDisplay[6] + ' ' + ParameterLabel[6]);
   GraphNeedUpdate := GraphNeedUpdate + [gLow];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateLowRatio;
var
  Ratio : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Ratio := Parameter[7];
   if Ratio <> DialLowRatio.Value
    then DialLowRatio.Value := Ratio;
   LbLowRatioValue.Caption := string('1 : ' + ParameterDisplay[7]);
   GraphNeedUpdate := GraphNeedUpdate + [gLow];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateLowKnee;
var
  Knee : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Knee := Parameter[8];
   if Knee <> DialLowKnee.Value
    then DialLowKnee.Value := Knee;
   LbLowKneeValue.Caption := string(ParameterDisplay[8] + ' ' + ParameterLabel[8]);
   GraphNeedUpdate := GraphNeedUpdate + [gLow];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateLowMakeUp;
var
  MakeUp : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   MakeUp := LightweightMultibandCompressor[0].MakeUpGain_dB;
   if MakeUp <> DialLowMakeUpGain.Value
    then DialLowMakeUpGain.Value := MakeUp;
   LbLowMakeUpGainValue.Caption := string(ParameterDisplay[9] + ' ' + ParameterLabel[9]);
   GraphNeedUpdate := GraphNeedUpdate + [gLow];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateLowAutoMakeUpGain;
var
  Brightness : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Brightness := Limit(100 * (0.1 + 0.9 * Integer(AutoGain[0])), 10, 100);
   if Brightness <> LEDLowAutoGain.Brightness_Percent
    then LEDLowAutoGain.Brightness_Percent := Brightness;
   DialLowMakeUpGain.Enabled := Brightness < 50;
   GraphNeedUpdate := GraphNeedUpdate + [gLow];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateLowMidAttack;
var
  Attack : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Attack := Parameter[11];
   if Attack <> DialLowMidAttack.Value
    then DialLowMidAttack.Value := Attack;
   LbLowMidAttackValue.Caption := string(ParameterDisplay[11] + ' ' + ParameterLabel[11]);
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateLowMidRelease;
var
  Release : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Release := Parameter[12];
   if Release <> DialLowMidRelease.Value
    then DialLowMidRelease.Value := Release;
   LbLowMidReleaseValue.Caption := string(ParameterDisplay[12] + ' ' + ParameterLabel[12]);
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateLowMidThreshold;
var
  Threshold : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Threshold := Parameter[13];
   if Threshold <> DialLowMidThreshold.Value
    then DialLowMidThreshold.Value := Threshold;
   LbLowMidThresholdValue.Caption := ParameterDisplay[13] + ' ' + ParameterLabel[13];
   GraphNeedUpdate := GraphNeedUpdate + [gLowMid];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateLowMidRatio;
var
  Ratio : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Ratio := Parameter[14];
   if Ratio <> DialLowMidRatio.Value
    then DialLowMidRatio.Value := Ratio;
   LbLowMidRatioValue.Caption := '1 : ' + ParameterDisplay[14];
   GraphNeedUpdate := GraphNeedUpdate + [gLowMid];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateLowMidKnee;
var
  Knee : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Knee := Parameter[15];
   if Knee <> DialLowMidKnee.Value
    then DialLowMidKnee.Value := Knee;
   LbLowMidKneeValue.Caption := ParameterDisplay[15] + ' ' + ParameterLabel[15];
   GraphNeedUpdate := GraphNeedUpdate + [gLowMid];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateLowMidMakeUp;
var
  MakeUp : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   MakeUp := LightweightMultibandCompressor[1].MakeUpGain_dB;
   if MakeUp <> DialLowMidMakeUpGain.Value
    then DialLowMidMakeUpGain.Value := MakeUp;
   LbLowMidMakeUpGainValue.Caption := ParameterDisplay[16] + ' ' + ParameterLabel[16];
   GraphNeedUpdate := GraphNeedUpdate + [gLowMid];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateLowMidAutoMakeUpGain;
var
  Brightness : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Brightness := Limit(100 * (0.1 + 0.9 * Integer(AutoGain[1])), 10, 100);
   if Brightness <> LEDLowMidAutoGain.Brightness_Percent
    then LEDLowMidAutoGain.Brightness_Percent := Brightness;
   DialLowMidMakeUpGain.Enabled := Brightness < 50;
   GraphNeedUpdate := GraphNeedUpdate + [gLowMid];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateHighMidAttack;
var
  Attack : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Attack := Parameter[18];
   if Attack <> DialHighMidAttack.Value
    then DialHighMidAttack.Value := Attack;
   LbHighMidAttackValue.Caption := ParameterDisplay[18] + ' ' + ParameterLabel[18];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateHighMidRelease;
var
  Release : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Release := Parameter[19];
   if Release <> DialHighMidRelease.Value
    then DialHighMidRelease.Value := Release;
   LbHighMidReleaseValue.Caption := ParameterDisplay[19] + ' ' + ParameterLabel[19];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateHighMidThreshold;
var
  Threshold : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Threshold := Parameter[20];
   if Threshold <> DialHighMidThreshold.Value
    then DialHighMidThreshold.Value := Threshold;
   LbHighMidThresholdValue.Caption := ParameterDisplay[20] + ' ' + ParameterLabel[20];
   GraphNeedUpdate := GraphNeedUpdate + [gHighMid];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateHighMidRatio;
var
  Ratio : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Ratio := Parameter[21];
   if Ratio <> DialHighMidRatio.Value
    then DialHighMidRatio.Value := Ratio;
   LbHighMidRatioValue.Caption := '1 : ' + ParameterDisplay[21];
   GraphNeedUpdate := GraphNeedUpdate + [gHighMid];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateHighMidKnee;
var
  Knee : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Knee := Parameter[22];
   if Knee <> DialHighMidKnee.Value
    then DialHighMidKnee.Value := Knee;
   LbHighMidKneeValue.Caption := ParameterDisplay[22] + ' ' + ParameterLabel[22];
   GraphNeedUpdate := GraphNeedUpdate + [gHighMid];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateHighMidMakeUp;
var
  MakeUp : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   MakeUp := LightweightMultibandCompressor[2].MakeUpGain_dB;
   if MakeUp <> DialHighMidMakeUpGain.Value
    then DialHighMidMakeUpGain.Value := MakeUp;
   LbHighMidMakeUpGainValue.Caption := ParameterDisplay[23] + ' ' + ParameterLabel[23];
   GraphNeedUpdate := GraphNeedUpdate + [gHighMid];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateHighMidAutoMakeUpGain;
var
  Brightness : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Brightness := Limit(100 * (0.1 + 0.9 * Integer(AutoGain[2])), 10, 100);
   if Brightness <> LEDHighMidAutoGain.Brightness_Percent
    then LEDHighMidAutoGain.Brightness_Percent := Brightness;
   DialHighMidMakeUpGain.Enabled := Brightness < 50;
   GraphNeedUpdate := GraphNeedUpdate + [gHighMid];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateHighAttack;
var
  Attack : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Attack := Parameter[25];
   if Attack <> DialHighAttack.Value
    then DialHighAttack.Value := Attack;
   LbHighAttackValue.Caption := ParameterDisplay[25] + ' ' + ParameterLabel[25];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateHighRelease;
var
  Release : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Release := Parameter[26];
   if Release <> DialHighRelease.Value
    then DialHighRelease.Value := Release;
   LbHighReleaseValue.Caption := ParameterDisplay[26] + ' ' + ParameterLabel[26];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateHighThreshold;
var
  Threshold : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Threshold := Parameter[27];
   if Threshold <> DialHighThreshold.Value
    then DialHighThreshold.Value := Threshold;
   LbHighThresholdValue.Caption := ParameterDisplay[27] + ' ' + ParameterLabel[27];
   GraphNeedUpdate := GraphNeedUpdate + [gHigh];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateHighRatio;
var
  Ratio : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Ratio := Parameter[28];
   if Ratio <> DialHighRatio.Value
    then DialHighRatio.Value := Ratio;
   LbHighRatioValue.Caption := '1 : ' + ParameterDisplay[28];
   GraphNeedUpdate := GraphNeedUpdate + [gHigh];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateHighKnee;
var
  Knee : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Knee := Parameter[29];
   if Knee <> DialHighKnee.Value
    then DialHighKnee.Value := Knee;
   LbHighKneeValue.Caption := ParameterDisplay[29] + ' ' + ParameterLabel[29];
   GraphNeedUpdate := GraphNeedUpdate + [gHigh];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateHighMakeUp;
var
  MakeUp : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   MakeUp := LightweightMultibandCompressor[3].MakeUpGain_dB;
   if MakeUp <> DialHighMakeUpGain.Value
    then DialHighMakeUpGain.Value := MakeUp;
   LbHighMakeUpGainValue.Caption := ParameterDisplay[30] + ' ' + ParameterLabel[30];
   GraphNeedUpdate := GraphNeedUpdate + [gHigh];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateHighAutoMakeUpGain;
var
  Brightness : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Brightness := Limit(100 * (0.1 + 0.9 * Integer(AutoGain[3])), 10, 100);
   if Brightness <> LEDHighAutoGain.Brightness_Percent
    then LEDHighAutoGain.Brightness_Percent := Brightness;
   DialHighMakeUpGain.Enabled := Brightness < 50;
   GraphNeedUpdate := GraphNeedUpdate + [gHigh];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateLimit;
var
  Brightness : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Brightness := 100 * (0.1 + 0.9 * Parameter[3]);
   if Brightness <> LEDSoftClip.Brightness_Percent
    then LEDSoftClip.Brightness_Percent := Brightness;
  end;
end;

end.
