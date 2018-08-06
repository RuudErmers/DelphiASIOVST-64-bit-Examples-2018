unit SidechainCompressorGUI;

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
  Forms, Controls, ExtCtrls, Dialogs, DAV_Types, DAV_VSTModule, DAV_VstHost, 
  DAV_GuiLabel, DAV_GuiGraphXY, DAV_GuiLED, DAV_GuiSlider, DAV_GuiEQGraph, 
  DAV_GuiPanel, DAV_GuiBaseControl, DAV_GuiGraphicControl, DAV_GuiCustomControl;

type
  TFmSidechainCompressor = class(TForm)
    GuiEQGraph: TGuiEQGraph;
    GuiGraphXY: TGuiGraphXY;
    PnPlugin: TGuiPanel;
    LbAttack: TGuiLabel;
    LbAttackValue: TGuiLabel;
    LbAutoMakeUpGain: TGuiLabel;
    LbClear: TGuiLabel;
    LbHighcutFrequency: TGuiLabel;
    LbHighcutFrequencyValue: TGuiLabel;
    LbHighcutOrder: TGuiLabel;
    LbHighcutOrderValue: TGuiLabel;
    LbHold: TGuiLabel;
    LbHoldValue: TGuiLabel;
    LbKnee: TGuiLabel;
    LbKneeValue: TGuiLabel;
    LbLowcutFrequency: TGuiLabel;
    LbLowcutFrequencyValue: TGuiLabel;
    LbLowcutOrder: TGuiLabel;
    LbLowcutOrderValue: TGuiLabel;
    LbMakeUpGainValue: TGuiLabel;
    LbMakupGain: TGuiLabel;
    LbMix: TGuiLabel;
    LbMixValue: TGuiLabel;
    LbRatio: TGuiLabel;
    LbRatioValue: TGuiLabel;
    LbRelease: TGuiLabel;
    LbReleaseValue: TGuiLabel;
    LbSidechainVstPlugin: TGuiLabel;
    LbSoftClip: TGuiLabel;
    LbStereo: TGuiLabel;
    LbThreshold: TGuiLabel;
    LbThresholdValue: TGuiLabel;
    LbTitle: TGuiLabel;
    LbVstPluginValue: TGuiLabel;
    LEDAutoGain: TGuiLED;
    LEDSideChain: TGuiLED;
    LEDSoftClip: TGuiLED;
    LEDStereo: TGuiLED;
    PnTitle: TPanel;
    SliderAttack: TGuiSlider;
    SliderHighcutFrequency: TGuiSlider;
    SliderHighcutOrder: TGuiSlider;
    SliderHold: TGuiSlider;
    SliderKnee: TGuiSlider;
    SliderLowcutFrequency: TGuiSlider;
    SliderLowcutOrder: TGuiSlider;
    SliderMakeUpGain: TGuiSlider;
    SliderMix: TGuiSlider;
    SliderRatio: TGuiSlider;
    SliderRelease: TGuiSlider;
    SliderThreshold: TGuiSlider;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    function GetFilterGain(Sender: TObject; const Frequency: Single): Single;
    procedure LbVstPluginValueClick(Sender: TObject);
    procedure LbVstPluginValueDblClick(Sender: TObject);
    procedure LEDAutoGainClick(Sender: TObject);
    procedure LEDSoftClipClick(Sender: TObject);
    procedure LEDStereoClick(Sender: TObject);
    procedure SliderAttackChange(Sender: TObject);
    procedure SliderHighcutFrequencyChange(Sender: TObject);
    procedure SliderHighcutOrderChange(Sender: TObject);
    procedure SliderHoldChange(Sender: TObject);
    procedure SliderKneeChange(Sender: TObject);
    procedure SliderLowcutFrequencyChange(Sender: TObject);
    procedure SliderLowcutOrderChange(Sender: TObject);
    procedure SliderMakeUpGainChange(Sender: TObject);
    procedure SliderRatioChange(Sender: TObject);
    procedure SliderReleaseChange(Sender: TObject);
    procedure SliderThresholdChange(Sender: TObject);
    procedure SliderMixChange(Sender: TObject);
    procedure LbClearDblClick(Sender: TObject);
    procedure LEDSideChainClick(Sender: TObject);
  public
    procedure UpdateLowcutFrequency;
    procedure UpdateLowcutOrder;
    procedure UpdateHighcutFrequency;
    procedure UpdateHighcutOrder;
    procedure UpdateAttack;
    procedure UpdateHold;
    procedure UpdateRelease;
    procedure UpdateThreshold;
    procedure UpdateRatio;
    procedure UpdateKnee;
    procedure UpdateMakeUp;
    procedure UpdateStereo;
    procedure UpdateLimit;
    procedure UpdateAutoMakeUpGain;
    procedure UpdateMix;
    procedure UpdateEnableVSTSideChain;
    procedure UpdateVstPlugin;
    function EvaluateCharacteristic(Sender: TObject; X: Double): Double;
  end;

implementation

uses
  SidechainCompressorDM, DAV_VSTModuleWithPrograms;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmSidechainCompressor.FormCreate(Sender: TObject);
begin
 with TGuiGraphXYFunctionSeries(GuiGraphXY[0].Series) do
  begin
   OnEvaluate := EvaluateCharacteristic;
  end;
end;

procedure TFmSidechainCompressor.FormShow(Sender: TObject);
begin
 UpdateLowcutFrequency;
 UpdateLowcutOrder;
 UpdateHighcutFrequency;
 UpdateHighcutOrder;
 UpdateAttack;
 UpdateHold;
 UpdateRelease;
 UpdateThreshold;
 UpdateRatio;
 UpdateKnee;
 UpdateMakeUp;
 UpdateStereo;
 UpdateLimit;
 UpdateAutoMakeUpGain;
 UpdateMix;
end;

function TFmSidechainCompressor.GetFilterGain(
  Sender: TObject; const Frequency: Single): Single;
begin
 with TSidechainCompressorDataModule(Owner)
  do Result := EvaluateFrequencyResponse(Frequency);
end;

procedure TFmSidechainCompressor.LbClearDblClick(Sender: TObject);
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   VstPlugIn.Active := False;
   LbVstPluginValue.Caption := '(double click to load)';
  end;
end;

procedure TFmSidechainCompressor.LbVstPluginValueClick(Sender: TObject);
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   if VstPlugIn.Active then
    begin
     if VstPlugIn.EditVisible
      then VstPlugIn.GUIControl.BringToFront
      else VstPlugIn.ShowEdit;
    end;
  end;
end;

procedure TFmSidechainCompressor.LbVstPluginValueDblClick(Sender: TObject);
begin
 with TSidechainCompressorDataModule(Owner), TOpenDialog.Create(Self) do
  try
   DefaultExt := '.dll';
   Filter := 'VST plugin (*.dll)|*.dll';
   Options := [ofHideReadOnly, ofFileMustExist, ofEnableSizing];
   Title := 'Select a VST Plugin';
   if Execute then
    begin
     LoadVSTPlugin(FileName);
     LbVstPluginValueClick(Self);
    end;
  finally
   Free;
  end;
end;

procedure TFmSidechainCompressor.SliderLowcutFrequencyChange(Sender: TObject);
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Parameter[0] := SliderLowcutFrequency.Value;
  end;
end;

procedure TFmSidechainCompressor.SliderLowcutOrderChange(Sender: TObject);
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Parameter[1] := SliderLowcutOrder.Value;
  end;
end;

procedure TFmSidechainCompressor.SliderHighcutFrequencyChange(Sender: TObject);
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Parameter[2] := SliderHighcutFrequency.Value;
  end;
end;

procedure TFmSidechainCompressor.SliderHighcutOrderChange(Sender: TObject);
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Parameter[3] := SliderHighcutOrder.Value;
  end;
end;

procedure TFmSidechainCompressor.SliderAttackChange(Sender: TObject);
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Parameter[4] := SliderAttack.Value;
  end;
end;

procedure TFmSidechainCompressor.SliderHoldChange(Sender: TObject);
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Parameter[5] := SliderHold.Value;
  end;
end;

procedure TFmSidechainCompressor.SliderReleaseChange(Sender: TObject);
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Parameter[6] := SliderRelease.Value;
  end;
end;

procedure TFmSidechainCompressor.SliderThresholdChange(Sender: TObject);
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Parameter[7] := SliderThreshold.Value;
  end;
end;

function TFmSidechainCompressor.EvaluateCharacteristic(Sender: TObject;
  X: Double): Double;
begin
 result := TSidechainCompressorDataModule(Owner).EvaluateCharacteristic(X);
end;

procedure TFmSidechainCompressor.SliderRatioChange(Sender: TObject);
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Parameter[8] := SliderRatio.Value;
  end;
end;

procedure TFmSidechainCompressor.SliderKneeChange(Sender: TObject);
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Parameter[9] := SliderKnee.Value;
  end;
end;

procedure TFmSidechainCompressor.SliderMakeUpGainChange(Sender: TObject);
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Parameter[10] := SliderMakeUpGain.Value;
  end;
end;

procedure TFmSidechainCompressor.LEDStereoClick(Sender: TObject);
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Parameter[11] := Integer(LEDStereo.Brightness_Percent < 50);
  end;
end;

procedure TFmSidechainCompressor.LEDSoftClipClick(Sender: TObject);
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Parameter[12] := Integer(LEDSoftClip.Brightness_Percent < 50);
  end;
end;

procedure TFmSidechainCompressor.LEDAutoGainClick(Sender: TObject);
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Parameter[13] := Integer(LEDAutoGain.Brightness_Percent < 50);
   if not SliderMakeUpGain.Enabled then UpdateMakeUp;
  end;
end;

procedure TFmSidechainCompressor.SliderMixChange(Sender: TObject);
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Parameter[14] := SliderMix.Value;
  end;
end;

procedure TFmSidechainCompressor.LEDSideChainClick(Sender: TObject);
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Parameter[15] := Integer(LEDSideChain.Brightness_Percent < 50);
  end;
end;

procedure TFmSidechainCompressor.UpdateLowcutFrequency;
var
  Frequency : Single;
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Frequency := Parameter[0];
   if Frequency <> SliderLowcutFrequency.Value
    then SliderLowcutFrequency.Value := Frequency;
   LbLowcutFrequencyValue.Caption := ParameterDisplay[0] + ' ' + ParameterLabel[0];
   GuiEQGraph.UpdateGraph;
  end;
end;

procedure TFmSidechainCompressor.UpdateLowcutOrder;
var
  Order : Integer;
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Order := Round(Parameter[1]);
   if Order <> Round(SliderLowcutOrder.Value)
    then SliderLowcutOrder.Value := Order;
   LbLowcutOrderValue.Caption := ParameterDisplay[1] + ' ' + ParameterLabel[1];
   GuiEQGraph.UpdateGraph;
  end;
end;

procedure TFmSidechainCompressor.UpdateHighcutFrequency;
var
  Frequency : Single;
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Frequency := Parameter[2];
   if Frequency <> SliderHighcutFrequency.Value
    then SliderHighcutFrequency.Value := Frequency;
   LbHighcutFrequencyValue.Caption := ParameterDisplay[2] + ' ' + ParameterLabel[2];
   GuiEQGraph.UpdateGraph;
  end;
end;

procedure TFmSidechainCompressor.UpdateHighcutOrder;
var
  Order : Integer;
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Order := Round(Parameter[3]);
   if Order <> Round(SliderHighcutOrder.Value)
    then SliderHighcutOrder.Value := Order;
   LbHighcutOrderValue.Caption := ParameterDisplay[3] + ' ' + ParameterLabel[3];
   GuiEQGraph.UpdateGraph;
  end;
end;

procedure TFmSidechainCompressor.UpdateAttack;
var
  Attack : Single;
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Attack := Parameter[4];
   if Attack <> SliderAttack.Value
    then SliderAttack.Value := Attack;
   LbAttackValue.Caption := ParameterDisplay[4] + ' ' + ParameterLabel[4];
  end;
end;

procedure TFmSidechainCompressor.UpdateHold;
var
  Hold : Single;
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Hold := Parameter[5];
   if Hold <> SliderHold.Value
    then SliderHold.Value := Hold;
   LbHoldValue.Caption := ParameterDisplay[5] + ' ' + ParameterLabel[5];
  end;
end;

procedure TFmSidechainCompressor.UpdateRelease;
var
  Release : Single;
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Release := Parameter[6];
   if Release <> SliderRelease.Value
    then SliderRelease.Value := Release;
   LbReleaseValue.Caption := ParameterDisplay[6] + ' ' + ParameterLabel[6];
  end;
end;

procedure TFmSidechainCompressor.UpdateThreshold;
var
  Threshold : Single;
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Threshold := Parameter[7];
   if Threshold <> SliderThreshold.Value
    then SliderThreshold.Value := Threshold;
   LbThresholdValue.Caption := ParameterDisplay[7] + ' ' + ParameterLabel[7];
   GuiGraphXY.UpdateGraph;
  end;
end;

procedure TFmSidechainCompressor.UpdateRatio;
var
  Ratio : Single;
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Ratio := Parameter[8];
   if Ratio <> SliderRatio.Value
    then SliderRatio.Value := Ratio;
   LbRatioValue.Caption := ParameterDisplay[8] + ' : 1';
   GuiGraphXY.UpdateGraph;
  end;
end;

procedure TFmSidechainCompressor.UpdateKnee;
var
  Knee : Single;
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Knee := Parameter[9];
   if Knee <> SliderKnee.Value
    then SliderKnee.Value := Knee;
   LbKneeValue.Caption := ParameterDisplay[9] + ' ' + ParameterLabel[9];
   GuiGraphXY.UpdateGraph;
  end;
end;

procedure TFmSidechainCompressor.UpdateMakeUp;
var
  MakeUp : Single;
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   MakeUp := SidechainCompressor[0].MakeUpGain_dB;
   if MakeUp <> SliderMakeUpGain.Value
    then SliderMakeUpGain.Value := MakeUp;
   LbMakeUpGainValue.Caption := ParameterDisplay[10] + ' ' + ParameterLabel[10];
   GuiGraphXY.UpdateGraph;
  end;
end;

procedure TFmSidechainCompressor.UpdateStereo;
var
  Brightness : Single;
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Brightness := 100 * (0.3 + 0.7 * Parameter[11]);
   if Brightness <> LEDStereo.Brightness_Percent
    then LEDStereo.Brightness_Percent := Brightness;
  end;
end;

procedure TFmSidechainCompressor.UpdateLimit;
var
  Brightness : Single;
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Brightness := 100 * (0.3 + 0.7 * Parameter[12]);
   if Brightness <> LEDSoftClip.Brightness_Percent
    then LEDSoftClip.Brightness_Percent := Brightness;
  end;
end;

procedure TFmSidechainCompressor.UpdateAutoMakeUpGain;
var
  Brightness : Single;
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Brightness := 100 * (0.3 + 0.7 * Parameter[13]);
   if Brightness <> LEDAutoGain.Brightness_Percent
    then LEDAutoGain.Brightness_Percent := Brightness;
   SliderMakeUpGain.Enabled := Brightness < 50;
   GuiGraphXY.UpdateGraph;
  end;
end;

procedure TFmSidechainCompressor.UpdateEnableVSTSideChain;
var
  Brightness : Single;
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Brightness := 100 * (0.3 + 0.7 * Parameter[15]);
   if Brightness <> LEDSideChain.Brightness_Percent
    then LEDSideChain.Brightness_Percent := Brightness;
  end;
end;

procedure TFmSidechainCompressor.UpdateMix;
var
  Mix : Single;
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Mix := Parameter[14];
   if Mix <> SliderMix.Value
    then SliderMix.Value := Mix;
   LbMixValue.Caption := ParameterDisplay[14] + ' ' + ParameterLabel[14];
  end;
end;

procedure TFmSidechainCompressor.UpdateVstPlugin;
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   if VstPlugIn.Active
    then LbVstPluginValue.Caption := VstPlugIn.GetFriendlyNameString(45)
    else
     begin
      if VstPlugIn.numInputs <> 2 then LbVstPluginValue.Caption := 'input channel mismatch' else
      if VstPlugIn.numOutputs <> 2 then LbVstPluginValue.Caption := 'output channel mismatch'
       else LbVstPluginValue.Caption := '(double click to load)';
     end;
  end;
end;

end.
