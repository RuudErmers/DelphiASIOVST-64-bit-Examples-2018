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
  Forms, ExtCtrls, Controls, StdCtrls, DAV_Types, DAV_VSTModule, DAV_GuiLabel, 
  DAV_GuiPanel, DAV_GuiStitchedControls, DAV_GuiStitchedDial, 
  DAV_GuiStitchedPngList, DAV_GuiImageControl, DAV_GuiCustomControl,
  DAV_GuiGraphicControl;

type
  TEditorForm = class(TForm)
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
    Panel: TGuiPanel;
    procedure DialThresholdChange(Sender: TObject);
    procedure DialRatioChange(Sender: TObject);
    procedure DialAttackChange(Sender: TObject);
    procedure DialReleaseChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    procedure UpdateThreshold;
    procedure UpdateAttack;
    procedure UpdateRatio;
    procedure UpdateRelease;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, SimpleFeedbackCompressorDM;

procedure TEditorForm.FormShow(Sender: TObject);
begin
 UpdateThreshold;
 UpdateAttack;
 UpdateRatio;
 UpdateRelease;
end;

procedure TEditorForm.UpdateThreshold;
begin
 LbThresholdValue.Caption := FloatToStrF(DialThreshold.Value, ffFixed, 3, 1) + ' dB';
end;

procedure TEditorForm.UpdateRatio;
begin
 with TSimpleFeedbackCompressorDataModule(Owner)
  do LbRatioValue.Caption := '1 : ' + FloatToStrF(Parameter[1], ffFixed, 3, 1);
end;

procedure TEditorForm.UpdateAttack;
var
  TempAttack : Single;
begin
 with TSimpleFeedbackCompressorDataModule(Owner) do
  begin
   TempAttack := 100 * Log10(Parameter[2]);
   if Parameter[2] <> TempAttack
    then DialAttack.Value := TempAttack;
   if Parameter[2] < 1
    then LbAttackValue.Caption := FloatToStrF(1000 * Parameter[2], ffGeneral, 4, 2) + ' µs'
    else LbAttackValue.Caption := FloatToStrF(Parameter[2], ffGeneral, 4, 2) + ' ms';
  end;
end;

procedure TEditorForm.UpdateRelease;
begin
 with TSimpleFeedbackCompressorDataModule(Owner) do
 if Parameter[3] < 1000
   then LbReleaseValue.Caption := FloatToStrF(Parameter[3], ffGeneral, 4, 5) + ' ms'
   else LbReleaseValue.Caption := FloatToStrF(0.001 * Parameter[3], ffGeneral, 4, 5) + ' s'
end;

procedure TEditorForm.DialThresholdChange(Sender: TObject);
begin
 with TSimpleFeedbackCompressorDataModule(Owner) do
  begin
   Parameter[0] := DialThreshold.Value;
   UpdateThreshold;
  end;
end;

procedure TEditorForm.DialRatioChange(Sender: TObject);
begin
 with TSimpleFeedbackCompressorDataModule(Owner) do
  begin
   Parameter[1] := Power(10, 0.01 * DialRatio.Value);
   UpdateRatio;
  end;
end;

procedure TEditorForm.DialAttackChange(Sender: TObject);
begin
 with TSimpleFeedbackCompressorDataModule(Owner) do
  begin
   Parameter[2] := Power(10, 0.01 * DialAttack.Value);
   UpdateAttack;
  end;
end;

procedure TEditorForm.DialReleaseChange(Sender: TObject);
begin
 with TSimpleFeedbackCompressorDataModule(Owner) do
  begin
   Parameter[3] := Power(10, 0.001 * DialRelease.Value);
   UpdateRelease;
  end;
end;

end.
