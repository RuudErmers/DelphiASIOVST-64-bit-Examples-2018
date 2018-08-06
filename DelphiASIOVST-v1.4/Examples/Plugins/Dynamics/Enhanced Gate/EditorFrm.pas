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
  Forms, Controls, StdCtrls, Graphics, ExtCtrls, Gauges, DAV_Types,
  DAV_VSTModule, DAV_GuiPixelMap, DAV_GuiPng, DAV_GuiLED, DAV_GuiGroup,
  DAV_GuiGraphicControl, DAV_GuiCustomControl, DAV_GuiImageControl,
  DAV_GuiLabel, DAV_GuiStitchedDial, DAV_GuiStitchedControls,
  DAV_GuiStitchedPngList, DAV_GuiCheckBox;

type
  TEditorForm = class(TForm)
    CBOnOff: TGuiLED;
    CBSideChain: TComboBox;
    DialAttack: TGuiStitchedDial;
    DialDecay: TGuiStitchedDial;
    DialHiCut: TGuiStitchedDial;
    DialHold: TGuiStitchedDial;
    DialKnee: TGuiStitchedDial;
    DialLoCut: TGuiStitchedDial;
    DialRange: TGuiStitchedDial;
    DialRatio: TGuiStitchedDial;
    DialThreshold: TGuiStitchedDial;
    EdAttack: TEdit;
    EdDecay: TEdit;
    EdHiCut: TEdit;
    EdHold: TEdit;
    EdKnee: TEdit;
    EdLoCut: TEdit;
    EdRange: TEdit;
    EdRatio: TEdit;
    EdThreshold: TEdit;
    GaugeL: TGauge;
    GaugeR: TGauge;
    GBDynamics: TGuiGroup;
    GBMain: TGuiGroup;
    GBSideChain: TGuiGroup;
    GSPL: TGuiStitchedPNGList;
    LbAttack: TLabel;
    LbDecay: TLabel;
    LBHighCut: TLabel;
    LbHold: TLabel;
    LbKnee: TLabel;
    LBLowCut: TLabel;
    LbRange: TLabel;
    LbRatio: TLabel;
    LbSource: TLabel;
    LbThreshold: TLabel;
    VUTimer: TTimer;
    LbEnhancedAudioGate: TGuiLabel;
    CBDuck: TGuiControlsCheckBox;
    CBStereoLink: TGuiControlsCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure CBOnOffClick(Sender: TObject);
    procedure CBDuckClick(Sender: TObject);
    procedure CBStereoLinkClick(Sender: TObject);
    procedure DialThresholdChange(Sender: TObject);
    procedure DialAttackChange(Sender: TObject);
    procedure DialHoldChange(Sender: TObject);
    procedure DialDecayChange(Sender: TObject);
    procedure DialLoCutChange(Sender: TObject);
    procedure DialHiCutChange(Sender: TObject);
    procedure CBSideChainChange(Sender: TObject);
    procedure DialRatioChange(Sender: TObject);
    procedure DialKneeChange(Sender: TObject);
    procedure DialRangeChange(Sender: TObject);
    procedure VUTimerTimer(Sender: TObject);
  private
    FBackground : TGuiCustomPixelMap;
  public
    procedure UpdateThreshold;
    procedure UpdateAttack;
    procedure UpdateHold;
    procedure UpdateDecay;
    procedure UpdateHiCut;
    procedure UpdateLoCut;
    procedure UpdateKnee;
    procedure UpdateRange;
    procedure UpdateRatio;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, DAV_GUICommon, EnhancedGateDM;

procedure TEditorForm.FormCreate(Sender: TObject);
begin
 FBackground := TGuiPixelMapMemory.Create;
end;

procedure TEditorForm.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackground);
end;

procedure TEditorForm.FormPaint(Sender: TObject);
begin
 if Assigned(FBackground)
  then FBackground.PaintTo(Canvas);
end;

procedure TEditorForm.FormResize(Sender: TObject);
var
  x, y  : Integer;
  s     : array [0..1] of Single;
  b     : ShortInt;
  ScnLn : PPixel32Array;
begin
 with FBackground do
  begin
   SetSize(ClientWidth, ClientHeight);
   s[0] := 0;
   s[1] := 0;
   for y := 0 to Height - 1 do
    begin
     ScnLn := Scanline[y];
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.9 * s[0] + 0.1 * (2 * Random - 1);
       b := Round($1F * s[1]);
       s[0] := s[1];
       ScnLn[x].B := $A4 + b;
       ScnLn[x].G := $D1 + b;
       ScnLn[x].R := $EA + b;
       ScnLn[x].A := $FF;
      end;
    end;
  end;
end;

procedure TEditorForm.FormShow(Sender: TObject);
begin
 with TEnhancedGateDataModule(Owner) do
  if Boolean(Round(Parameter[0]))
   then CBOnOff.Brightness_Percent := 100
   else CBOnOff.Brightness_Percent := 20;
 UpdateThreshold;
 UpdateAttack;
 UpdateHold;
 UpdateDecay;
 UpdateHiCut;
 UpdateLoCut;
 UpdateKnee;
 UpdateRange;
 UpdateRatio;
end;

procedure TEditorForm.CBOnOffClick(Sender: TObject);
begin
  with TEnhancedGateDataModule(Owner)
   do Parameter[0] := Integer(CBOnOff.Brightness_Percent > 90);
end;

procedure TEditorForm.DialThresholdChange(Sender: TObject);
begin
  with TEnhancedGateDataModule(Owner)
   do Parameter[1] := DialThreshold.Value;
  UpdateThreshold;
end;

procedure TEditorForm.DialAttackChange(Sender: TObject);
begin
  with TEnhancedGateDataModule(Owner)
   do Parameter[2] := Power(10, DialAttack.Value);
  UpdateAttack;
end;

procedure TEditorForm.DialHoldChange(Sender: TObject);
begin
  with TEnhancedGateDataModule(Owner)
   do Parameter[3] := Power(10, DialHold.Value);
  UpdateHold;
end;

procedure TEditorForm.DialDecayChange(Sender: TObject);
begin
  with TEnhancedGateDataModule(Owner)
   do Parameter[4] := Power(10, DialDecay.Value);
  UpdateDecay;
end;

procedure TEditorForm.CBDuckClick(Sender: TObject);
begin
  with TEnhancedGateDataModule(Owner)
   do Parameter[5] := Integer(CBDuck.Checked);
end;

procedure TEditorForm.CBStereoLinkClick(Sender: TObject);
begin
  with TEnhancedGateDataModule(Owner)
   do Parameter[6] := Integer(CBStereoLink.Checked);
end;

procedure TEditorForm.CBSideChainChange(Sender: TObject);
begin
  with TEnhancedGateDataModule(Owner)
   do Parameter[7] := CBSideChain.ItemIndex;
end;

procedure TEditorForm.DialLoCutChange(Sender: TObject);
begin
  with TEnhancedGateDataModule(Owner)
   do Parameter[8] := Power(10, DialLoCut.Value);
  UpdateLoCut;
end;

procedure TEditorForm.DialHiCutChange(Sender: TObject);
begin
  with TEnhancedGateDataModule(Owner)
   do Parameter[9] := 0.001 * Power(10, DialHiCut.Value);
  UpdateHiCut;
end;

procedure TEditorForm.DialRatioChange(Sender: TObject);
begin
  with TEnhancedGateDataModule(Owner) do Parameter[10] := DialRatio.Value;
  UpdateRatio;
end;

procedure TEditorForm.DialKneeChange(Sender: TObject);
begin
  with TEnhancedGateDataModule(Owner) do Parameter[11] := DialKnee.Value;
  UpdateKnee;
end;

procedure TEditorForm.DialRangeChange(Sender: TObject);
begin
  with TEnhancedGateDataModule(Owner) do Parameter[12] := DialRange.Value;
  UpdateRange;
end;

procedure TEditorForm.UpdateThreshold;
begin
  with TEnhancedGateDataModule(Owner) do
   begin
    if DialThreshold.Value <> Parameter[1]
     then DialThreshold.Value := Parameter[1];
    EdThreshold.Text := FloatToStrF(DialThreshold.Value, ffFixed, 5, 1) + ' dB';
   end;
end;

procedure TEditorForm.VUTimerTimer(Sender: TObject);
begin
 with TEnhancedGateDataModule(Owner) do
  begin
   GaugeL.Progress := Round(100 * LevelLeft);
   GaugeR.Progress := Round(100 * LevelRight);
  end;
end;

procedure TEditorForm.UpdateAttack;
var
  i: Integer;
begin
  with TEnhancedGateDataModule(Owner) do
   begin
    if DialAttack.Value <> Log10(Parameter[2])
     then DialAttack.Value := Log10(Parameter[2]);
    i := Round(1.499999 - DialAttack.Value);
    if i < 0 then i := 0 else if i > 2 then i := 2;
    EdAttack.Text := FloatToStrF(Parameter[2], ffFixed, 5, i) + ' ms';
   end;
end;

procedure TEditorForm.UpdateHold;
var
  i: Integer;
begin
  with TEnhancedGateDataModule(Owner) do
   begin
    if DialHold.Value <> Log10(Parameter[3])
     then DialHold.Value := Log10(Parameter[3]);
    i := Round(1.499999 - DialHold.Value);
    if i < 0 then i := 0 else if i > 2 then i := 2;
    EdHold.Text := FloatToStrF(Parameter[3], ffFixed, 5, i) + ' s';
   end;
end;

procedure TEditorForm.UpdateDecay;
var
  i: Integer;
begin
  with TEnhancedGateDataModule(Owner) do
   begin
    if DialDecay.Value <> Log10(Parameter[4])
     then DialDecay.Value := Log10(Parameter[4]);
    i := Round(1.499999 - DialDecay.Value);
    if i < 0 then i := 0 else if i > 2 then i := 2;
    EdDecay.Text := FloatToStrF(Parameter[4], ffFixed, 5, i) + ' ms';
   end;
end;

procedure TEditorForm.UpdateLoCut;
begin
  with TEnhancedGateDataModule(Owner) do
   begin
    if DialLoCut.Value <> Log10(Parameter[8])
     then DialLoCut.Value := Log10(Parameter[8]);
    if Parameter[8] < 1000
     then EdLoCut.Text := FloatToStrF(Parameter[8], ffFixed, 5, Round(2.49999 - Log10(Parameter[8]))) + ' Hz'
     else EdLoCut.Text := FloatToStrF(0.001 * Parameter[8], ffFixed, 5, 1) + ' kHz';
    if Parameter[8] > Parameter[9] * 1100
     then GBSideChain.Font.Color := clRed
     else GBSideChain.Font.Color := clWhite;
   end;
end;

procedure TEditorForm.UpdateHiCut;
begin
  with TEnhancedGateDataModule(Owner) do
   begin
    if DialHiCut.Value <> Log10(1000 * Parameter[9])
     then DialHiCut.Value := Log10(1000 * Parameter[9]);
    if Parameter[9] < 1000
     then EdHiCut.Text := FloatToStrF(1000 * Parameter[9], ffFixed, 5, 0) + ' Hz'
     else EdHiCut.Text := FloatToStrF(Parameter[9], ffFixed, 5, Round(4.49999 - Log10(Parameter[9]))) + ' kHz';
    if Parameter[8] > Parameter[9] * 1100
     then GBSideChain.Font.Color := clRed
     else GBSideChain.Font.Color := clWhite;
   end;
end;

procedure TEditorForm.UpdateRatio;
begin
  with TEnhancedGateDataModule(Owner) do
   begin
    if DialRatio.Value <> Parameter[10]
     then DialRatio.Value := Parameter[10];
    EdRatio.Text := FloatToStrF(Parameter[10], ffGeneral, 5, 5);
   end;
end;

procedure TEditorForm.UpdateKnee;
begin
  with TEnhancedGateDataModule(Owner) do
   begin
    if DialKnee.Value <> Parameter[11]
     then DialKnee.Value := Parameter[11];
    EdKnee.Text := FloatToStrF(Parameter[11], ffFixed, 5, 2) + ' dB';
   end;
end;

procedure TEditorForm.UpdateRange;
begin
  with TEnhancedGateDataModule(Owner) do
   begin
    if DialRange.Value <> Parameter[12]
     then DialRange.Value := Parameter[12];
    EdRange.Text := FloatToStrF(Parameter[12], ffFixed, 5, 1) + ' dB';
   end;
end;

end.
