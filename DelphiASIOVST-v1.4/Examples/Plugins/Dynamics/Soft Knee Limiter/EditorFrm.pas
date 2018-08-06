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
  DAV_GuiLabel, DAV_GuiStitchedControls, DAV_GuiStitchedPngList, 
  DAV_GuiStitchedDial;

type
  TEditorForm = class(TForm)
    DialAttack: TGuiStitchedDial;
    DialMakeUp: TGuiStitchedDial;
    DialRelease: TGuiStitchedDial;
    DialSoftKnee: TGuiStitchedDial;
    DialThreshold: TGuiStitchedDial;
    GSPL: TGuiStitchedPNGList;
    LbAttack: TGuiLabel;
    LbAttackValue: TGuiLabel;
    LbMakeUp: TGuiLabel;
    LbMakeUpValue: TGuiLabel;
    LbRelease: TGuiLabel;
    LbReleaseValue: TGuiLabel;
    LbSoftKnee: TGuiLabel;
    LbSoftKneeValue: TGuiLabel;
    LbThreshold: TGuiLabel;
    LbThresholdValue: TGuiLabel;
    procedure FormShow(Sender: TObject);
    procedure DialThresholdChange(Sender: TObject);
    procedure DialMakeUpChange(Sender: TObject);
    procedure DialAttackChange(Sender: TObject);
    procedure DialReleaseChange(Sender: TObject);
    procedure DialSoftKneeChange(Sender: TObject);
  public
    procedure UpdateAttack;
    procedure UpdateMakeUp;
    procedure UpdateRelease;
    procedure UpdateSoftKnee;
    procedure UpdateThreshold;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, Graphics, SKLDM;

procedure TEditorForm.FormShow(Sender: TObject);
begin
 UpdateAttack;
 UpdateMakeUp;
 UpdateRelease;
 UpdateSoftKnee;
 UpdateThreshold;
end;

procedure TEditorForm.UpdateSoftKnee;
begin
 with TSoftKneeLimiterDataModule(Owner) do
  begin
   if DialSoftKnee.Value <> Parameter[1]
    then DialSoftKnee.Value := Parameter[1];
   LbSoftKneeValue.Caption := FloatToStrF(Parameter[1], ffFixed, 3, 1) + ' dB';
  end;
end;

procedure TEditorForm.UpdateAttack;
begin
 with TSoftKneeLimiterDataModule(Owner) do
  begin
   if DialAttack.Value <> Log10(Parameter[2])
    then DialAttack.Value := Log10(Parameter[2]);
   if Parameter[2] < 1
    then LbAttackValue.Caption := FloatToStrF(Parameter[2] * 1E3, ffGeneral, 4, 2) + ' μs'
    else LbAttackValue.Caption := FloatToStrF(Parameter[2], ffGeneral, 4, 2) + ' ms';
  end;
end;

procedure TEditorForm.UpdateRelease;
begin
 with TSoftKneeLimiterDataModule(Owner) do
  begin
   if DialRelease.Value <> Log10(Parameter[3])
    then DialRelease.Value := Log10(Parameter[3]);
   if Parameter[3] >= 1000
    then LbReleaseValue.Caption := FloatToStrF(Parameter[3] * 1E-3, ffGeneral, 3, 2) + ' s'
    else LbReleaseValue.Caption := FloatToStrF(Parameter[3], ffGeneral, 3, 2) + ' ms';
  end;
end;

procedure TEditorForm.UpdateMakeUp;
begin
 with TSoftKneeLimiterDataModule(Owner) do
  begin
   if DialMakeUp.Value <> Parameter[4]
    then DialMakeUp.Value := Parameter[4];
   LbMakeUpValue.Caption := FloatToStrF(Parameter[4], ffFixed, 3, 1) + ' dB';
  end;
end;

procedure TEditorForm.UpdateThreshold;
begin
 with TSoftKneeLimiterDataModule(Owner) do
  begin
   if DialThreshold.Value <> Parameter[0]
    then DialThreshold.Value := Parameter[0];
   LbThresholdValue.Caption := FloatToStrF(Parameter[0], ffFixed, 3, 1) + ' dB';
  end;
end;

procedure TEditorForm.DialThresholdChange(Sender: TObject);
begin
 with TSoftKneeLimiterDataModule(Owner) do
  begin
   Parameter[0] := DialThreshold.Value;
  end;
end;

procedure TEditorForm.DialSoftKneeChange(Sender: TObject);
begin
 with TSoftKneeLimiterDataModule(Owner) do
  begin
   Parameter[1] := DialSoftKnee.Value;
  end;
end;

procedure TEditorForm.DialAttackChange(Sender: TObject);
begin
 with TSoftKneeLimiterDataModule(Owner) do
  begin
   Parameter[2] := Power(10, DialAttack.Value);
  end;
end;

procedure TEditorForm.DialReleaseChange(Sender: TObject);
begin
 with TSoftKneeLimiterDataModule(Owner) do
  begin
   Parameter[3] := Power(10, DialRelease.Value);
  end;
end;

procedure TEditorForm.DialMakeUpChange(Sender: TObject);
begin
 with TSoftKneeLimiterDataModule(Owner) do
  begin
   Parameter[4] := DialMakeUp.Value;
  end;
end;

end.
