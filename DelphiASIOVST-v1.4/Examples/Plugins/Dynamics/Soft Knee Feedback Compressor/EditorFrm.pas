unit EditorFrm;

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
  Forms, Controls, StdCtrls, DAV_Types, DAV_VSTModule, DAV_GuiBaseControl, 
  DAV_GuiPixelMap, DAV_GuiLabel, DAV_GuiStitchedControls, 
  DAV_GuiStitchedPngList, DAV_GuiStitchedDial;

type
  TEditorForm = class(TForm)
    DialAttack: TGuiStitchedDial;
    DialMakeUp: TGuiStitchedDial;
    DialRatio: TGuiStitchedDial;
    DialRelease: TGuiStitchedDial;
    DialThreshold: TGuiStitchedDial;
    GSPL: TGuiStitchedPNGList;
    LbAttack: TGuiLabel;
    LbAttackValue: TLabel;
    LbMakeUp: TGuiLabel;
    LbMakeUpValue: TLabel;
    LbRatio: TGuiLabel;
    LbRatioValue: TLabel;
    LbRelease: TGuiLabel;
    LbReleaseValue: TLabel;
    LbThreshold: TGuiLabel;
    LbThresholdValue: TLabel;
    procedure DialThresholdChange(Sender: TObject);
    procedure DialRatioChange(Sender: TObject);
    procedure DialAttackChange(Sender: TObject);
    procedure DialReleaseChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DialMakeUpChange(Sender: TObject);
  public
    procedure UpdateAttack;
    procedure UpdateMakeUpGain;
    procedure UpdateRatio;
    procedure UpdateRelease;
    procedure UpdateThreshold;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, SoftKneeFeedbackCompressorDM;

procedure TEditorForm.FormShow(Sender: TObject);
begin
 UpdateAttack;
 UpdateMakeUpGain;
 UpdateRatio;
 UpdateRelease;
 UpdateThreshold;
end;

procedure TEditorForm.UpdateThreshold;
begin
 with TSoftKneeFeedbackCompressorDataModule(Owner) do
  begin
   if Parameter[0] <> DialThreshold.Value
    then DialThreshold.Value := Parameter[0];
   LbThresholdValue.Caption := FloatToStrF(Parameter[0], ffFixed, 3, 1) + ' dB';
  end;
end;

procedure TEditorForm.UpdateAttack;
var
  AttackTemp : Single;
begin
 with TSoftKneeFeedbackCompressorDataModule(Owner) do
  begin
   AttackTemp := 100 * Log10(Parameter[2]);
   if DialAttack.Value <> AttackTemp
    then DialAttack.Value := AttackTemp;
   LbAttackValue.Caption := FloatToStrF(Parameter[2], ffGeneral, 4, 2) + ' ms';
  end;
end;

procedure TEditorForm.UpdateMakeUpGain;
var
  MakeUpTemp : Single;
begin
 with TSoftKneeFeedbackCompressorDataModule(Owner) do
  begin
   MakeUpTemp := Parameter[4];
   if DialMakeUp.Value <> MakeUpTemp
    then DialMakeUp.Value := MakeUpTemp;
   LbMakeUpValue.Caption := FloatToStrF(Parameter[4], ffFixed, 3, 1) + ' dB';
  end;
end;

procedure TEditorForm.UpdateRatio;
var
  RatioTemp : Single;
begin
 with TSoftKneeFeedbackCompressorDataModule(Owner) do
  begin
   RatioTemp := 100 * Log10(Parameter[1]);
   if DialRatio.Value <> RatioTemp
    then DialRatio.Value := RatioTemp;
   LbRatioValue.Caption := '1 : ' + FloatToStrF(Parameter[1], ffFixed, 3, 1);
  end;
end;

procedure TEditorForm.UpdateRelease;
var
  ReleaseTemp : Single;
begin
 with TSoftKneeFeedbackCompressorDataModule(Owner) do
  begin
   ReleaseTemp := 1000 * Log10(Parameter[3]);
   if DialRelease.Value <> ReleaseTemp
    then DialRelease.Value := ReleaseTemp;
   LbReleaseValue.Caption := FloatToStrF(Parameter[3], ffGeneral, 4, 5) + ' ms';
  end;
end;

procedure TEditorForm.DialThresholdChange(Sender: TObject);
begin
 with TSoftKneeFeedbackCompressorDataModule(Owner) do
  begin
   Parameter[0] := DialThreshold.Value;
  end;
end;

procedure TEditorForm.DialRatioChange(Sender: TObject);
begin
 with TSoftKneeFeedbackCompressorDataModule(Owner) do
  begin
   Parameter[1] := Power(10, 0.01 * DialRatio.Value);
  end;
end;

procedure TEditorForm.DialAttackChange(Sender: TObject);
begin
 with TSoftKneeFeedbackCompressorDataModule(Owner) do
  begin
   Parameter[2] := Power(10, 0.01 * DialAttack.Value);
  end;
end;

procedure TEditorForm.DialReleaseChange(Sender: TObject);
begin
 with TSoftKneeFeedbackCompressorDataModule(Owner) do
  begin
   Parameter[3] := Power(10, 0.001 * DialRelease.Value);
  end;
end;

procedure TEditorForm.DialMakeUpChange(Sender: TObject);
begin
 with TSoftKneeFeedbackCompressorDataModule(Owner) do
  begin
   Parameter[4] := DialMakeUp.Value;
  end;
end;

end.
