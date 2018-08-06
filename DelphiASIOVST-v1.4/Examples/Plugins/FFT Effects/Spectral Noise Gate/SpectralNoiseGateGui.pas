unit SpectralNoiseGateGui;

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
  Forms, Controls, DAV_Types, DAV_VSTModule, DAV_GuiPng, DAV_GuiLabel, 
  DAV_GuiBaseControl, DAV_GuiSelectBox, DAV_GuiStitchedControls, 
  DAV_GuiStitchedPngList, DAV_GuiStitchedDial;

type
  TFmSpectralNoiseGate = class(TForm)
    DialAttack: TGuiStitchedDial;
    DialFftOrder: TGuiStitchedDial;
    DialKnee: TGuiStitchedDial;
    DialRatio: TGuiStitchedDial;
    DialRelease: TGuiStitchedDial;
    DialThreshold: TGuiStitchedDial;
    GSPL: TGuiStitchedPNGList;
    LbAttack: TGuiLabel;
    LbAttackValue: TGuiLabel;
    LbFftOrder: TGuiLabel;
    LbFftOrderValue: TGuiLabel;
    LbKnee: TGuiLabel;
    LbKneeValue: TGuiLabel;
    LbRatio: TGuiLabel;
    LbRatioValue: TGuiLabel;
    LbRelease: TGuiLabel;
    LbReleaseValue: TGuiLabel;
    LbThreshold: TGuiLabel;
    LbThresholdValue: TGuiLabel;
    LbWindowFunction: TGuiLabel;
    SbWindowFunction: TGuiSelectBox;
    procedure FormShow(Sender: TObject);
    procedure SbWindowFunctionChange(Sender: TObject);
    procedure DialThresholdChange(Sender: TObject);
    procedure DialFftOrderChange(Sender: TObject);
    procedure DialRatioChange(Sender: TObject);
    procedure DialKneeChange(Sender: TObject);
    procedure DialAttackChange(Sender: TObject);
    procedure DialReleaseChange(Sender: TObject);
  public
    procedure UpdateThreshold;  
    procedure UpdateFftOrder;
    procedure UpdateWindowFunction;
    procedure UpdateAttack;
    procedure UpdateRelease;
    procedure UpdateRatio;
    procedure UpdateKnee;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, DAV_VSTModuleWithPrograms, SpectralNoiseGateDM;

procedure TFmSpectralNoiseGate.FormShow(Sender: TObject);
begin
 UpdateThreshold;
 UpdateFftOrder;
 UpdateWindowFunction;
 UpdateAttack;
 UpdateRelease;
 UpdateRatio;
 UpdateKnee;
end;

procedure TFmSpectralNoiseGate.DialThresholdChange(Sender: TObject);
begin
 with TSpectralNoiseGateModule(Owner) do
  begin
   if Parameter[0] <> DialThreshold.Value
    then Parameter[0] := DialThreshold.Value;
  end;
end;

procedure TFmSpectralNoiseGate.DialFftOrderChange(Sender: TObject);
begin
 with TSpectralNoiseGateModule(Owner) do
  begin
   if Parameter[1] <> DialFftOrder.Value
    then Parameter[1] := DialFftOrder.Value;
  end;
end;

procedure TFmSpectralNoiseGate.DialRatioChange(Sender: TObject);
begin
 with TSpectralNoiseGateModule(Owner) do
  begin
   if Parameter[3] <> DialRatio.Value
    then Parameter[3] := DialRatio.Value;
  end;
end;

procedure TFmSpectralNoiseGate.DialKneeChange(Sender: TObject);
begin
 with TSpectralNoiseGateModule(Owner) do
  begin
   if Parameter[4] <> DialKnee.Value
    then Parameter[4] := DialKnee.Value;
  end;
end;

procedure TFmSpectralNoiseGate.DialAttackChange(Sender: TObject);
begin
 with TSpectralNoiseGateModule(Owner) do
  begin
   if Parameter[5] <> DialAttack.Value
    then Parameter[5] := DialAttack.Value;
  end;
end;

procedure TFmSpectralNoiseGate.DialReleaseChange(Sender: TObject);
begin
 with TSpectralNoiseGateModule(Owner) do
  begin
   if Parameter[6] <> DialRelease.Value
    then Parameter[6] := DialRelease.Value;
  end;
end;

procedure TFmSpectralNoiseGate.SbWindowFunctionChange(Sender: TObject);
begin
 with TSpectralNoiseGateModule(Owner) do
  begin
   if Round(Parameter[2]) <> SbWindowFunction.ItemIndex
    then Parameter[2] := SbWindowFunction.ItemIndex;
  end;
end;

procedure TFmSpectralNoiseGate.UpdateThreshold;
begin
 with TSpectralNoiseGateModule(Owner) do
  begin
   if DialThreshold.Value <> Parameter[0]
    then DialThreshold.Value := Parameter[0];
   LbThresholdValue.Caption := ParameterDisplay[0] + ' ' + ParameterLabel[0];
  end;
end;

procedure TFmSpectralNoiseGate.UpdateFftOrder;
begin
 with TSpectralNoiseGateModule(Owner) do
  begin
   if DialFftOrder.Value <> Parameter[1]
    then DialFftOrder.Value := Parameter[1];
   LbFftOrderValue.Caption := ParameterDisplay[1] + ' ' + ParameterLabel[1];
  end;
end;

procedure TFmSpectralNoiseGate.UpdateWindowFunction;
begin
 with TSpectralNoiseGateModule(Owner) do
  begin
   if SbWindowFunction.ItemIndex <> Round(Parameter[2])
    then SbWindowFunction.ItemIndex := Round(Parameter[2]);
  end;
end;

procedure TFmSpectralNoiseGate.UpdateRatio;
begin
 with TSpectralNoiseGateModule(Owner) do
  begin
   if DialRatio.Value <> Parameter[3]
    then DialRatio.Value := Parameter[3];
   LbRatioValue.Caption := ParameterDisplay[3] + ' ' + ParameterLabel[3];
  end;
end;

procedure TFmSpectralNoiseGate.UpdateKnee;
begin
 with TSpectralNoiseGateModule(Owner) do
  begin
   if DialKnee.Value <> Parameter[4]
    then DialKnee.Value := Parameter[4];
   LbKneeValue.Caption := ParameterDisplay[4] + ' ' + ParameterLabel[4];
  end;
end;

procedure TFmSpectralNoiseGate.UpdateAttack;
begin
 with TSpectralNoiseGateModule(Owner) do
  begin
   if DialAttack.Value <> Parameter[5]
    then DialAttack.Value := Parameter[5];
   LbAttackValue.Caption := ParameterDisplay[5] + ' ' + ParameterLabel[5];
  end;
end;

procedure TFmSpectralNoiseGate.UpdateRelease;
begin
 with TSpectralNoiseGateModule(Owner) do
  begin
   if DialRelease.Value <> Parameter[6]
    then DialRelease.Value := Parameter[6];
   LbReleaseValue.Caption := ParameterDisplay[6] + ' ' + ParameterLabel[6];
  end;
end;

end.
