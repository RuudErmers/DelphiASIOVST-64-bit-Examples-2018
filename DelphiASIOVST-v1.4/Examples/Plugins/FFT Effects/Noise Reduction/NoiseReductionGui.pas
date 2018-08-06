unit NoiseReductionGui;

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

uses 
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, Controls, DAV_GuiLabel, DAV_GuiSelectBox, DAV_GuiLED, DAV_GuiPng,
  DAV_GuiStitchedControls, DAV_GuiStitchedPngList, DAV_GuiStitchedDial,
  DAV_GuiImageControl, DAV_GuiCustomControl, DAV_GuiGraphicControl;

type
  TFmNoiseReduction = class(TForm)
    DialAttack: TGuiStitchedDial;
    DialKnee: TGuiStitchedDial;
    DialRatio: TGuiStitchedDial;
    DialRelease: TGuiStitchedDial;
    DialThresholdOffset: TGuiStitchedDial;
    GSPL: TGuiStitchedPNGList;
    LbAttack: TGuiLabel;
    LbAttackValue: TGuiLabel;
    LbCaptureNoiseProfile: TGuiLabel;
    LbFftSize: TGuiLabel;
    LbKnee: TGuiLabel;
    LbKneeValue: TGuiLabel;
    LbRatio: TGuiLabel;
    LbRatioValue: TGuiLabel;
    LbRelease: TGuiLabel;
    LbReleaseValue: TGuiLabel;
    LbThresholdOffset: TGuiLabel;
    LbThresholdOffsetValue: TGuiLabel;
    LbWindowFunction: TGuiLabel;
    LedNoiseProfile: TGuiLED;
    SbFftSize: TGuiSelectBox;
    SbWindowFunction: TGuiSelectBox;
    procedure FormShow(Sender: TObject);
    procedure DialAttackChange(Sender: TObject);
    procedure DialKneeChange(Sender: TObject);
    procedure DialRatioChange(Sender: TObject);
    procedure DialReleaseChange(Sender: TObject);
    procedure DialThresholdOffsetChange(Sender: TObject);
    procedure LedNoiseProfileClick(Sender: TObject);
    procedure SbFftSizeChange(Sender: TObject);
    procedure SbWindowFunctionChange(Sender: TObject);
  public
    procedure UpdateMatchThreshold;
    procedure UpdateFftOrder;
    procedure UpdateWindowFunction;
    procedure UpdateAttack;
    procedure UpdateRelease;
    procedure UpdateRatio;
    procedure UpdateKnee;
    procedure UpdateThresholdOffset;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  DAV_Common, NoiseReductionDM;

procedure TFmNoiseReduction.FormShow(Sender: TObject);
begin
 UpdateThresholdOffset;
 UpdateMatchThreshold;
 UpdateFftOrder;
 UpdateWindowFunction;
 UpdateAttack;
 UpdateRelease;
 UpdateRatio;
 UpdateKnee;
end;

procedure TFmNoiseReduction.LedNoiseProfileClick(Sender: TObject);
begin
 with TNoiseReductionModule(Owner) do
  begin
   Parameter[7] := 1 - Parameter[7];
(*
   if Parameter[7] > 0.5
    then LbNoiseProfileState.Caption := '(capturing...)'
    else LbNoiseProfileState.Caption := '(' +
      FloatToStrF(TimeCaptured, ffGeneral, 3, 1) + 's captured)';
*)
  end;
end;

procedure TFmNoiseReduction.DialThresholdOffsetChange(Sender: TObject);
begin
 with TNoiseReductionModule(Owner) do
  begin
   if Parameter[0] <> DialThresholdOffset.Value
    then Parameter[0] := DialThresholdOffset.Value;
  end;
end;

procedure TFmNoiseReduction.DialRatioChange(Sender: TObject);
begin
 with TNoiseReductionModule(Owner) do
  begin
   if Parameter[3] <> DialRatio.Value
    then Parameter[3] := DialRatio.Value;
  end;
end;

procedure TFmNoiseReduction.DialKneeChange(Sender: TObject);
begin
 with TNoiseReductionModule(Owner) do
  begin
   if Parameter[4] <> DialKnee.Value
    then Parameter[4] := DialKnee.Value;
  end;
end;

procedure TFmNoiseReduction.DialAttackChange(Sender: TObject);
begin
 with TNoiseReductionModule(Owner) do
  begin
   if Parameter[5] <> DialAttack.Value
    then Parameter[5] := DialAttack.Value;
  end;
end;

procedure TFmNoiseReduction.DialReleaseChange(Sender: TObject);
begin
 with TNoiseReductionModule(Owner) do
  begin
   if Parameter[6] <> DialRelease.Value
    then Parameter[6] := DialRelease.Value;
  end;
end;

procedure TFmNoiseReduction.SbFftSizeChange(Sender: TObject);
begin
 with TNoiseReductionModule(Owner) do
  begin
   if Parameter[1] <> 6 + SbFftSize.ItemIndex
    then Parameter[1] := 6 + SbFftSize.ItemIndex;
  end;
end;

procedure TFmNoiseReduction.SbWindowFunctionChange(Sender: TObject);
begin
 with TNoiseReductionModule(Owner) do
  begin
   if Round(Parameter[2]) <> SbWindowFunction.ItemIndex
    then Parameter[2] := SbWindowFunction.ItemIndex;
  end;
end;

procedure TFmNoiseReduction.UpdateThresholdOffset;
begin
 with TNoiseReductionModule(Owner) do
  begin
   if DialThresholdOffset.Value <> Parameter[0]
    then DialThresholdOffset.Value := Parameter[0];

   LbThresholdOffsetValue.Caption := string(ParameterDisplay[0] + ' ' + ParameterLabel[0]);
  end;
end;

procedure TFmNoiseReduction.UpdateFftOrder;
begin
 with TNoiseReductionModule(Owner) do
  begin
   if SbFftSize.ItemIndex <> Round(Parameter[1] - 6)
    then SbFftSize.ItemIndex := Round(Parameter[1] - 6);
  end;
end;

procedure TFmNoiseReduction.UpdateWindowFunction;
begin
 with TNoiseReductionModule(Owner) do
  begin
   if SbWindowFunction.ItemIndex <> Round(Parameter[2])
    then SbWindowFunction.ItemIndex := Round(Parameter[2]);
  end;
end;

procedure TFmNoiseReduction.UpdateRatio;
begin
 with TNoiseReductionModule(Owner) do
  begin
   if DialRatio.Value <> Parameter[3]
    then DialRatio.Value := Parameter[3];
   LbRatioValue.Caption := string(ParameterDisplay[3] + ' ' + ParameterLabel[3]);
  end;
end;

procedure TFmNoiseReduction.UpdateKnee;
begin
 with TNoiseReductionModule(Owner) do
  begin
   if DialKnee.Value <> Parameter[4]
    then DialKnee.Value := Parameter[4];
   LbKneeValue.Caption := string(ParameterDisplay[4] + ' ' + ParameterLabel[4]);
  end;
end;

procedure TFmNoiseReduction.UpdateAttack;
begin
 with TNoiseReductionModule(Owner) do
  begin
   if DialAttack.Value <> Parameter[5]
    then DialAttack.Value := Parameter[5];
   LbAttackValue.Caption := string(ParameterDisplay[5] + ' ' + ParameterLabel[5]);
  end;
end;

procedure TFmNoiseReduction.UpdateRelease;
begin
 with TNoiseReductionModule(Owner) do
  begin
   if DialRelease.Value <> Parameter[6]
    then DialRelease.Value := Parameter[6];
   LbReleaseValue.Caption := string(ParameterDisplay[6] + ' ' + ParameterLabel[6]);
  end;
end;

procedure TFmNoiseReduction.UpdateMatchThreshold;
begin
 with TNoiseReductionModule(Owner) do
  begin
   LedNoiseProfile.Brightness_Percent := Limit(10 + 80 * Parameter[7], 10, 90);
  end;
end;

end.
