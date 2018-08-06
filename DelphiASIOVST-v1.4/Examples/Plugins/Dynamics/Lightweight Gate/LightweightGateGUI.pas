unit LightweightGateGUI;

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
  Forms, DAV_Types, DAV_VSTModule, DAV_GuiLabel, Controls, DAV_GuiBaseControl, 
  DAV_GuiGraphXY, DAV_GuiLED, DAV_GuiStitchedControls, DAV_GuiStitchedDial, 
  DAV_GuiStitchedPngList;

type
  TFmLightweightGate = class(TForm)
    DialAttack: TGuiStitchedDial;
    DialKnee: TGuiStitchedDial;
    DialRatio: TGuiStitchedDial;
    DialRelease: TGuiStitchedDial;
    DialThreshold: TGuiStitchedDial;
    GSPL: TGuiStitchedPNGList;
    GuiGraphXY: TGuiGraphXY;
    LbAttack: TGuiLabel;
    LbAttackValue: TGuiLabel;
    LbKnee: TGuiLabel;
    LbKneeValue: TGuiLabel;
    LbRatio: TGuiLabel;
    LbRatioValue: TGuiLabel;
    LbRelease: TGuiLabel;
    LbReleaseValue: TGuiLabel;
    LbThreshold: TGuiLabel;
    LbThresholdValue: TGuiLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DialAttackChange(Sender: TObject);
    procedure DialReleaseChange(Sender: TObject);
    procedure DialThresholdChange(Sender: TObject);
    procedure DialRatioChange(Sender: TObject);
    procedure DialKneeChange(Sender: TObject);
  public
    procedure UpdateAttack;
    procedure UpdateRelease;
    procedure UpdateThreshold;
    procedure UpdateRatio;
    procedure UpdateKnee;
    function EvaluateCharacteristic(Sender: TObject; X: Double): Double;
  end;

implementation

uses
  LightweightGateDM, DAV_VSTModuleWithPrograms;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmLightweightGate.FormCreate(Sender: TObject);
begin
 with TGuiGraphXYFunctionSeries(GuiGraphXY[0].Series) do
  begin
   OnEvaluate := EvaluateCharacteristic;
  end;
end;

procedure TFmLightweightGate.FormShow(Sender: TObject);
begin
 UpdateAttack;
 UpdateRelease;
 UpdateThreshold;
 UpdateRatio;
 UpdateKnee;
end;

procedure TFmLightweightGate.DialAttackChange(Sender: TObject);
begin
 with TLightweightGateDataModule(Owner) do
  begin
   Parameter[0] := DialAttack.Value;
  end;
end;

procedure TFmLightweightGate.DialReleaseChange(Sender: TObject);
begin
 with TLightweightGateDataModule(Owner) do
  begin
   Parameter[1] := DialRelease.Value;
  end;
end;

procedure TFmLightweightGate.DialThresholdChange(Sender: TObject);
begin
 with TLightweightGateDataModule(Owner) do
  begin
   Parameter[2] := DialThreshold.Value;
  end;
end;

function TFmLightweightGate.EvaluateCharacteristic(Sender: TObject;
  X: Double): Double;
begin
 result := TLightweightGateDataModule(Owner).EvaluateCharacteristic(X);
end;

procedure TFmLightweightGate.DialRatioChange(Sender: TObject);
begin
 with TLightweightGateDataModule(Owner) do
  begin
   Parameter[3] := 1 / DialRatio.Value;
  end;
end;

procedure TFmLightweightGate.DialKneeChange(Sender: TObject);
begin
 with TLightweightGateDataModule(Owner) do
  begin
   Parameter[4] := DialKnee.Value;
  end;
end;

procedure TFmLightweightGate.UpdateAttack;
var
  Attack : Single;
begin
 with TLightweightGateDataModule(Owner) do
  begin
   Attack := Parameter[0];
   if Attack <> DialAttack.Value
    then DialAttack.Value := Attack;
   LbAttackValue.Caption := ParameterDisplay[0] + ' ' + ParameterLabel[0];
  end;
end;

procedure TFmLightweightGate.UpdateRelease;
var
  Release : Single;
begin
 with TLightweightGateDataModule(Owner) do
  begin
   Release := Parameter[1];
   if Release <> DialRelease.Value
    then DialRelease.Value := Release;
   LbReleaseValue.Caption := ParameterDisplay[1] + ' ' + ParameterLabel[1];
  end;
end;

procedure TFmLightweightGate.UpdateKnee;
var
  Knee : Single;
begin
 with TLightweightGateDataModule(Owner) do
  begin
   Knee := Parameter[4];
   if Knee <> DialKnee.Value
    then DialKnee.Value := Knee;
   LbKneeValue.Caption := ParameterDisplay[4] + ' ' + ParameterLabel[4];
   GuiGraphXY.UpdateGraph;
  end;
end;

procedure TFmLightweightGate.UpdateRatio;
var
  Ratio : Single;
begin
 with TLightweightGateDataModule(Owner) do
  begin
   Ratio := 1 / Parameter[3];
   if Ratio <> DialRatio.Value
    then DialRatio.Value := Ratio;
   LbRatioValue.Caption := '1 : ' + FloatToStrF(Ratio, ffGeneral, 3, 3);
   GuiGraphXY.UpdateGraph;
  end;
end;

procedure TFmLightweightGate.UpdateThreshold;
var
  Threshold : Single;
begin
 with TLightweightGateDataModule(Owner) do
  begin
   Threshold := Parameter[2];
   if Threshold <> DialThreshold.Value
    then DialThreshold.Value := Threshold;
   LbThresholdValue.Caption := ParameterDisplay[2] + ' ' + ParameterLabel[2];
   GuiGraphXY.UpdateGraph;
  end;
end;

end.
