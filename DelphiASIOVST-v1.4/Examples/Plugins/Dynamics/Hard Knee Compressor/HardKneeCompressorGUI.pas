unit HardKneeCompressorGUI;

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
  Forms, Controls, StdCtrls, DAV_Types, DAV_VSTModule, DAV_GuiPng, 
  DAV_GuiPixelMap, DAV_GuiLabel, DAV_GuiStitchedControls, 
  DAV_GuiStitchedPngList, DAV_GuiStitchedDial, DAV_GuiCustomControl, 
  DAV_GuiGraphicControl;

type
  TFmHardKneeCompressor = class(TForm)
    DialAttack: TGuiStitchedDial;
    DialRatio: TGuiStitchedDial;
    DialRelease: TGuiStitchedDial;
    DialThreshold: TGuiStitchedDial;
    GSPL: TGuiStitchedPNGList;
    LbAttack: TGuiLabel;
    LbAttackValue: TLabel;
    LbRatio: TGuiLabel;
    LbRatioValue: TLabel;
    LbRelease: TGuiLabel;
    LbReleaseValue: TLabel;
    LbThreshold: TGuiLabel;
    LbThresholdValue: TLabel;
    procedure FormShow(Sender: TObject);
    procedure DialThresholdChange(Sender: TObject);
    procedure DialRatioChange(Sender: TObject);
    procedure DialAttackChange(Sender: TObject);
    procedure DialReleaseChange(Sender: TObject);
  public
    procedure UpdateRatio;
    procedure UpdateThreshold;
    procedure UpdateAttack;
    procedure UpdateRelease;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, HardKneeCompressorDM;

procedure TFmHardKneeCompressor.FormShow(Sender: TObject);
begin
 UpdateAttack;
 UpdateRatio;
 UpdateThreshold;
 UpdateRelease;
end;

procedure TFmHardKneeCompressor.UpdateThreshold;
begin
 with THardKneeCompressorDataModule(Owner) do
  begin
   if Parameter[0] <> DialThreshold.Value
    then DialThreshold.Value := Parameter[0];
   LbThresholdValue.Caption := FloatToStrF(DialThreshold.Value, ffFixed, 3, 1) + ' dB';
  end;
end;

procedure TFmHardKneeCompressor.UpdateAttack;
var
  AttackTemp : Single;
begin
 with THardKneeCompressorDataModule(Owner) do
  begin
   AttackTemp := 100 * Log10(Parameter[2]);
   if DialAttack.Value <> AttackTemp
    then DialAttack.Value := AttackTemp;
   LbAttackValue.Caption := FloatToStrF(Parameter[2], ffGeneral, 4, 2) + ' ms';
  end;
end;

procedure TFmHardKneeCompressor.UpdateRatio;
var
  RatioTemp : Single;
begin
 with THardKneeCompressorDataModule(Owner) do
  begin
   RatioTemp := 100 * Log10(Parameter[1]);
   if DialRatio.Value <> RatioTemp
    then DialRatio.Value := RatioTemp;
   LbRatioValue.Caption := '1 : ' + FloatToStrF(Parameter[1], ffFixed, 3, 1);
  end;
end;

procedure TFmHardKneeCompressor.UpdateRelease;
var
  ReleaseTemp : Single;
begin
 with THardKneeCompressorDataModule(Owner) do
  begin
   ReleaseTemp := 1000 * Log10(Parameter[3]);
   if DialRelease.Value <> ReleaseTemp
    then DialRelease.Value := ReleaseTemp;
   LbReleaseValue.Caption := FloatToStrF(Parameter[3], ffGeneral, 4, 5) + ' ms';
  end;
end;

procedure TFmHardKneeCompressor.DialThresholdChange(Sender: TObject);
begin
 with THardKneeCompressorDataModule(Owner) do
  begin
   if Parameter[0] <> DialThreshold.Value
    then Parameter[0] := DialThreshold.Value;
  end;
end;

procedure TFmHardKneeCompressor.DialRatioChange(Sender: TObject);
var
  TempRatio : Single;
begin
 with THardKneeCompressorDataModule(Owner) do
  begin
   TempRatio := Power(10, 0.01 * DialRatio.Value);
   if Parameter[1] <> TempRatio
    then Parameter[1] := TempRatio;
  end;
end;

procedure TFmHardKneeCompressor.DialAttackChange(Sender: TObject);
var
  TempAttack : Single;
begin
 with THardKneeCompressorDataModule(Owner) do
  begin
   TempAttack := Power(10, 0.01 * DialAttack.Value);
   if Parameter[2] <> TempAttack
    then Parameter[2] := TempAttack;
  end;
end;

procedure TFmHardKneeCompressor.DialReleaseChange(Sender: TObject);
var
  TempRelease : Single;
begin
 with THardKneeCompressorDataModule(Owner) do
  begin
   TempRelease := Power(10, 0.001 * DialRelease.Value);
   if Parameter[3] <> TempRelease
    then Parameter[3] := TempRelease;
  end;
end;

end.
