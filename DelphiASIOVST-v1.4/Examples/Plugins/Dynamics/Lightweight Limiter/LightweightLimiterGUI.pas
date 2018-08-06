unit LightweightLimiterGUI;

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
  Forms, Controls, ExtCtrls, DAV_Types, DAV_VSTModule, DAV_GuiLabel, 
  DAV_GuiBaseControl, DAV_GuiGraphXY, DAV_GuiLED, DAV_GuiLevelMeter, 
  DAV_GuiStitchedControls, DAV_GuiStitchedPngList, DAV_GuiStitchedDial;

type
  TFmLightweightLimiter = class(TForm)
    DialAttack: TGuiStitchedDial;
    DialKnee: TGuiStitchedDial;
    DialMakeUpGain: TGuiStitchedDial;
    DialRelease: TGuiStitchedDial;
    DialThreshold: TGuiStitchedDial;
    GSPL: TGuiStitchedPNGList;
    GuiGraphXY: TGuiGraphXY;
    Lb0dB: TGuiLabel;
    Lb10dB: TGuiLabel;
    Lb20dB: TGuiLabel;
    LbAttack: TGuiLabel;
    LbAttackValue: TGuiLabel;
    LbAutoGain: TGuiLabel;
    LbGR: TGuiLabel;
    LbKnee: TGuiLabel;
    LbKneeValue: TGuiLabel;
    LbMakeUpGain: TGuiLabel;
    LbMakeUpGainValue: TGuiLabel;
    LbRelease: TGuiLabel;
    LbReleaseValue: TGuiLabel;
    LbSoftClip: TGuiLabel;
    LbStereo: TGuiLabel;
    LbThreshold: TGuiLabel;
    LbThresholdValue: TGuiLabel;
    LEDAutoGain: TGuiLED;
    LEDLimit: TGuiLED;
    LEDStereo: TGuiLED;
    LMGainReduction: TGuiColorLevelMeter;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure DialAttackChange(Sender: TObject);
    procedure DialReleaseChange(Sender: TObject);
    procedure DialThresholdChange(Sender: TObject);
    procedure DialKneeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DialMakeUpGainChange(Sender: TObject);
    procedure LEDStereoClick(Sender: TObject);
    procedure LEDLimitClick(Sender: TObject);
    procedure LEDAutoGainClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  public
    procedure UpdateAttack;
    procedure UpdateRelease;
    procedure UpdateThreshold;
    procedure UpdateKnee;
    procedure UpdateMakeUp;
    procedure UpdateStereo;
    procedure UpdateLimit;
    procedure UpdateAutoMakeUpGain;
    function EvaluateCharacteristic(Sender: TObject; X: Double): Double;
  end;

implementation

uses
  DAV_VSTModuleWithPrograms, LightweightLimiterDM;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmLightweightLimiter.FormCreate(Sender: TObject);
begin
 with TGuiGraphXYFunctionSeries(GuiGraphXY[0].Series) do
  begin
   OnEvaluate := EvaluateCharacteristic;
  end;
end;

procedure TFmLightweightLimiter.FormShow(Sender: TObject);
begin
 UpdateAttack;
 UpdateRelease;
 UpdateThreshold;
 UpdateKnee;
 UpdateMakeUp;
 UpdateStereo;
 UpdateLimit;
 UpdateAutoMakeUpGain;
end;

procedure TFmLightweightLimiter.LEDAutoGainClick(Sender: TObject);
begin
 with TLightweightLimiterDataModule(Owner) do
  begin
   Parameter[8] := Integer(LEDAutoGain.Brightness_Percent < 50);
   if not DialMakeUpGain.Enabled then UpdateMakeUp;
  end;
end;

procedure TFmLightweightLimiter.LEDLimitClick(Sender: TObject);
begin
 with TLightweightLimiterDataModule(Owner) do
  begin
   Parameter[7] := Integer(LEDLimit.Brightness_Percent < 50);
  end;
end;

procedure TFmLightweightLimiter.LEDStereoClick(Sender: TObject);
begin
 with TLightweightLimiterDataModule(Owner) do
  begin
   Parameter[6] := Integer(LEDStereo.Brightness_Percent < 50);
  end;
end;

procedure TFmLightweightLimiter.TimerTimer(Sender: TObject);
begin
 with TLightweightLimiterDataModule(Owner), LMGainReduction do
  begin
   if LightweightLimiter[0].GainReductiondB > PeakLevel
    then PeakLevel := LightweightLimiter[0].GainReductiondB
    else PeakLevel := 0.5 * PeakLevel - 0.5 * LightweightLimiter[0].GainReductiondB;
  end;
end;

procedure TFmLightweightLimiter.DialAttackChange(Sender: TObject);
begin
 with TLightweightLimiterDataModule(Owner) do
  begin
   Parameter[0] := DialAttack.Value;
  end;
end;

procedure TFmLightweightLimiter.DialReleaseChange(Sender: TObject);
begin
 with TLightweightLimiterDataModule(Owner) do
  begin
   Parameter[1] := DialRelease.Value;
  end;
end;

procedure TFmLightweightLimiter.DialThresholdChange(Sender: TObject);
begin
 with TLightweightLimiterDataModule(Owner) do
  begin
   Parameter[2] := DialThreshold.Value;
  end;
end;

function TFmLightweightLimiter.EvaluateCharacteristic(Sender: TObject;
  X: Double): Double;
begin
 result := TLightweightLimiterDataModule(Owner).EvaluateCharacteristic(X);
end;

procedure TFmLightweightLimiter.DialKneeChange(Sender: TObject);
begin
 with TLightweightLimiterDataModule(Owner) do
  begin
   Parameter[4] := DialKnee.Value;
  end;
end;

procedure TFmLightweightLimiter.DialMakeUpGainChange(Sender: TObject);
begin
 with TLightweightLimiterDataModule(Owner) do
  begin
   Parameter[5] := DialMakeUpGain.Value;
  end;
end;

procedure TFmLightweightLimiter.UpdateAttack;
var
  Attack : Single;
begin
 with TLightweightLimiterDataModule(Owner) do
  begin
   Attack := Parameter[0];
   if Attack <> DialAttack.Value
    then DialAttack.Value := Attack;
   LbAttackValue.Caption := ParameterDisplay[0] + ' ' + ParameterLabel[0];
  end;
end;

procedure TFmLightweightLimiter.UpdateRelease;
var
  Release : Single;
begin
 with TLightweightLimiterDataModule(Owner) do
  begin
   Release := Parameter[1];
   if Release <> DialRelease.Value
    then DialRelease.Value := Release;
   LbReleaseValue.Caption := ParameterDisplay[1] + ' ' + ParameterLabel[1];
  end;
end;

procedure TFmLightweightLimiter.UpdateKnee;
var
  Knee : Single;
begin
 with TLightweightLimiterDataModule(Owner) do
  begin
   Knee := Parameter[4];
   if Knee <> DialKnee.Value
    then DialKnee.Value := Knee;
   LbKneeValue.Caption := ParameterDisplay[4] + ' ' + ParameterLabel[4];
   GuiGraphXY.UpdateGraph;
  end;
end;

procedure TFmLightweightLimiter.UpdateMakeUp;
var
  MakeUp : Single;
begin
 with TLightweightLimiterDataModule(Owner) do
  begin
   MakeUp := LightweightLimiter[0].MakeUpGain_dB;
   if MakeUp <> DialMakeUpGain.Value
    then DialMakeUpGain.Value := MakeUp;
   LbMakeUpGainValue.Caption := ParameterDisplay[5] + ' ' + ParameterLabel[5];
   GuiGraphXY.UpdateGraph;
  end;
end;

procedure TFmLightweightLimiter.UpdateThreshold;
var
  Threshold : Single;
begin
 with TLightweightLimiterDataModule(Owner) do
  begin
   Threshold := Parameter[2];
   if Threshold <> DialThreshold.Value
    then DialThreshold.Value := Threshold;
   LbThresholdValue.Caption := ParameterDisplay[2] + ' ' + ParameterLabel[2];
   GuiGraphXY.UpdateGraph;
  end;
end;

procedure TFmLightweightLimiter.UpdateStereo;
var
  Brightness : Single;
begin
 with TLightweightLimiterDataModule(Owner) do
  begin
   Brightness := 100 * (0.1 + 0.9 * Parameter[6]);
   if Brightness <> LEDStereo.Brightness_Percent
    then LEDStereo.Brightness_Percent := Brightness;
  end;
end;

procedure TFmLightweightLimiter.UpdateAutoMakeUpGain;
var
  Brightness : Single;
begin
 with TLightweightLimiterDataModule(Owner) do
  begin
   Brightness := 100 * (0.1 + 0.9 * Parameter[8]);
   if Brightness <> LEDAutoGain.Brightness_Percent
    then LEDAutoGain.Brightness_Percent := Brightness;
   DialMakeUpGain.Enabled := Brightness < 50;
   GuiGraphXY.UpdateGraph;
  end;
end;

procedure TFmLightweightLimiter.UpdateLimit;
var
  Brightness : Single;
begin
 with TLightweightLimiterDataModule(Owner) do
  begin
   Brightness := 100 * (0.1 + 0.9 * Parameter[7]);
   if Brightness <> LEDLimit.Brightness_Percent
    then LEDLimit.Brightness_Percent := Brightness;
  end;
end;

end.
