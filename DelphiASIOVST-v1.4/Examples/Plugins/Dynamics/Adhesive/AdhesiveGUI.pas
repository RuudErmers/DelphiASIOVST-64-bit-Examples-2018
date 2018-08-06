unit AdhesiveGUI;

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
  Forms, Controls, Graphics, ExtCtrls, DAV_Types, DAV_VSTModule, DAV_GuiLabel, 
  DAV_GuiPng, DAV_GuiPixelMap, DAV_GuiStitchedControls, DAV_GuiStitchedPngList, 
  DAV_GuiStitchedDial, DAV_GuiStitchedDisplay, DAV_GuiStitchedSwitch, 
  DAV_GuiCustomControl, DAV_GuiGraphicControl, DAV_GuiImageControl;

type
  TFmAdhesive = class(TForm)
    DialAttack: TGuiStitchedDial;
    DialFilter: TGuiStitchedDial;
    DialKnee: TGuiStitchedDial;
    DialMakeUpGain: TGuiStitchedDial;
    DialMix: TGuiStitchedDial;
    DialRatio: TGuiStitchedDial;
    DialRelease: TGuiStitchedDial;
    DialThreshold: TGuiStitchedDial;
    GSPL: TGuiStitchedPNGList;
    LbAttack: TGuiLabel;
    LbAttackMax: TGuiLabel;
    LbAttackMin: TGuiLabel;
    LbCharacteristic: TGuiLabel;
    LbExt: TGuiLabel;
    LbIn: TGuiLabel;
    LbIO: TGuiLabel;
    LbKnee: TGuiLabel;
    LbKneeMax: TGuiLabel;
    LbKneeMid: TGuiLabel;
    LbKneeMin: TGuiLabel;
    LbMakeUpGain: TGuiLabel;
    LbMakeUpMax: TGuiLabel;
    LbMakeUpMid: TGuiLabel;
    LbMakeupMin: TGuiLabel;
    LbMix: TGuiLabel;
    LbMixMax: TGuiLabel;
    LbMixMin: TGuiLabel;
    LbPeakClip: TGuiLabel;
    LbRatio: TGuiLabel;
    LbRatioMax: TGuiLabel;
    LbRatioMed: TGuiLabel;
    LbRatioMin: TGuiLabel;
    LbRelease: TGuiLabel;
    LbReleaseMax: TGuiLabel;
    LbReleaseMin: TGuiLabel;
    LbSCHP: TGuiLabel;
    LbSCmin: TGuiLabel;
    LbSideChain: TGuiLabel;
    LbSidechainMax: TGuiLabel;
    LbSidechainMid: TGuiLabel;
    LbThreshold: TGuiLabel;
    LbThresholdMax: TGuiLabel;
    LbThresholdMid: TGuiLabel;
    LbThresholdMin: TGuiLabel;
    LbTimeConstants: TGuiLabel;
    LbTitle: TGuiLabel;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    Shape5: TShape;
    Shape6: TShape;
    Shape7: TShape;
    Shape8: TShape;
    SwLimit: TGuiStitchedSwitch;
    SwOnOff: TGuiStitchedSwitch;
    SwSideChain: TGuiStitchedSwitch;
    Timer: TTimer;
    VUMeter: TGuiStitchedDisplay;
    procedure FormCreate(Sender: TObject);
    procedure DialAttackChange(Sender: TObject);
    procedure DialReleaseChange(Sender: TObject);
    procedure DialThresholdChange(Sender: TObject);
    procedure DialRatioChange(Sender: TObject);
    procedure DialKneeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DialMakeUpGainChange(Sender: TObject);
    procedure LEDStereoClick(Sender: TObject);
    procedure LEDLimitClick(Sender: TObject);
    procedure LEDAutoGainClick(Sender: TObject);
    procedure DialMixChange(Sender: TObject);
    procedure DialFilterChange(Sender: TObject);
    procedure SwOnOffChange(Sender: TObject);
    procedure SwSideChainChange(Sender: TObject);
    procedure SwLimitChange(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    FBackground : TGuiCustomPixelMap;
  public
    procedure UpdateAttack;
    procedure UpdateRelease;
    procedure UpdateThreshold;
    procedure UpdateRatio;
    procedure UpdateKnee;
    procedure UpdateMakeUp;
    procedure UpdateMix;
    procedure UpdateSideChainFilter;
    procedure UpdateOnOff;
    procedure UpdatePeakClip;
    procedure UpdateExtSideChain;
  end;

implementation

uses
  AdhesiveDM, DAV_Common, DAV_GuiCommon, DAV_VSTModuleWithPrograms;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmAdhesive.FormCreate(Sender: TObject);
begin
 FBackground := TGuiPixelMapMemory.Create;
end;

procedure TFmAdhesive.FormPaint(Sender: TObject);
begin
 if Assigned(FBackground)
  then FBackground.PaintTo(Canvas);
end;

procedure TFmAdhesive.FormResize(Sender: TObject);
var
  x, y  : Integer;
  b     : Byte;
  ScnLn : PPixel32Array;
begin
 with FBackground do
  begin
   SetSize(ClientWidth, ClientHeight);
   for y := 0 to Height - 1 do
    begin
     ScnLn := ScanLine[y];
     for x := 0 to Width - 1 do
      begin
       b := Random(16);
       ScnLn[x].B := b;
       ScnLn[x].G := b;
       ScnLn[x].R := b;
      end;
    end;
  end;
end;

procedure TFmAdhesive.FormShow(Sender: TObject);
begin
 UpdateAttack;
 UpdateRelease;
 UpdateThreshold;
 UpdateRatio;
 UpdateKnee;
 UpdateMakeUp;
 UpdateOnOff;
 UpdatePeakClip;
 UpdateExtSideChain;
end;

procedure TFmAdhesive.LEDAutoGainClick(Sender: TObject);
begin
 with TAdhesiveDataModule(Owner) do
  begin
//   Parameter[8] := Integer(LEDAutoGain.Brightness_Percent < 50);
  end;
end;

procedure TFmAdhesive.LEDLimitClick(Sender: TObject);
begin
 with TAdhesiveDataModule(Owner) do
  begin
//   Parameter[7] := Integer(LEDLimit.Brightness_Percent < 50);
  end;
end;

procedure TFmAdhesive.LEDStereoClick(Sender: TObject);
begin
 with TAdhesiveDataModule(Owner) do
  begin
//   Parameter[6] := Integer(LEDStereo.Brightness_Percent < 50);
  end;
end;

procedure TFmAdhesive.DialThresholdChange(Sender: TObject);
begin
 with TAdhesiveDataModule(Owner) do
  begin
   Parameter[0] := -DialThreshold.Value;
  end;
end;

procedure TFmAdhesive.DialMakeUpGainChange(Sender: TObject);
begin
 with TAdhesiveDataModule(Owner) do
  begin
   Parameter[1] := DialMakeUpGain.Value;
  end;
end;

procedure TFmAdhesive.DialRatioChange(Sender: TObject);
begin
 with TAdhesiveDataModule(Owner) do
  begin
   Parameter[2] := DialRatio.Value;
  end;
end;

procedure TFmAdhesive.DialKneeChange(Sender: TObject);
begin
 with TAdhesiveDataModule(Owner) do
  begin
   Parameter[3] := DialKnee.Value;
  end;
end;

procedure TFmAdhesive.DialAttackChange(Sender: TObject);
begin
 with TAdhesiveDataModule(Owner) do
  begin
   Parameter[4] := DialAttack.Value;
  end;
end;

procedure TFmAdhesive.DialReleaseChange(Sender: TObject);
begin
 with TAdhesiveDataModule(Owner) do
  begin
   Parameter[5] := 1E3 * DialRelease.Value;
  end;
end;

procedure TFmAdhesive.DialMixChange(Sender: TObject);
begin
 with TAdhesiveDataModule(Owner) do
  begin
   Parameter[6] := DialMix.Value;
  end;
end;

procedure TFmAdhesive.SwOnOffChange(Sender: TObject);
begin
 with TAdhesiveDataModule(Owner) do
  begin
   Parameter[7] := 1 - SwOnOff.GlyphIndex;
  end;
end;

procedure TFmAdhesive.SwLimitChange(Sender: TObject);
begin
 with TAdhesiveDataModule(Owner) do
  begin
   Parameter[8] := 1 - SwLimit.GlyphIndex;
  end;
end;

procedure TFmAdhesive.DialFilterChange(Sender: TObject);
begin
 with TAdhesiveDataModule(Owner) do
  begin
   Parameter[9] := DialFilter.Value;
  end;
end;

procedure TFmAdhesive.SwSideChainChange(Sender: TObject);
begin
 with TAdhesiveDataModule(Owner) do
  begin
   Parameter[10] := 1 - SwSideChain.GlyphIndex;
  end;
end;

procedure TFmAdhesive.TimerTimer(Sender: TObject);
begin
 with TAdhesiveDataModule(Owner).FastCompressor, VUMeter
  do GlyphIndex := Round(GlyphCount * Limit(-GainReductiondB, 0, 40) / 40);
end;

procedure TFmAdhesive.UpdateThreshold;
var
  Threshold : Single;
begin
 with TAdhesiveDataModule(Owner) do
  begin
   Threshold := -Parameter[0];
   if Threshold <> DialThreshold.Value
    then DialThreshold.Value := Threshold;
  end;
end;

procedure TFmAdhesive.UpdateMakeUp;
var
  MakeUp : Single;
begin
 with TAdhesiveDataModule(Owner) do
  begin
   MakeUp := Parameter[1];
   if MakeUp <> DialMakeUpGain.Value
    then DialMakeUpGain.Value := MakeUp;
  end;
end;

procedure TFmAdhesive.UpdateRatio;
var
  Ratio : Single;
begin
 with TAdhesiveDataModule(Owner) do
  begin
   Ratio := Parameter[2];
   if Ratio <> DialRatio.Value
    then DialRatio.Value := Ratio;
  end;
end;

procedure TFmAdhesive.UpdateKnee;
var
  Knee : Single;
begin
 with TAdhesiveDataModule(Owner) do
  begin
   Knee := Parameter[3];
   if Knee <> DialKnee.Value
    then DialKnee.Value := Knee;
  end;
end;

procedure TFmAdhesive.UpdateAttack;
var
  Attack : Single;
begin
 with TAdhesiveDataModule(Owner) do
  begin
   Attack := Parameter[4];
   if Attack <> DialAttack.Value
    then DialAttack.Value := Attack;
  end;
end;

procedure TFmAdhesive.UpdateRelease;
var
  Release : Single;
begin
 with TAdhesiveDataModule(Owner) do
  begin
   Release := 1E-3 * Parameter[5];
   if Release <> DialRelease.Value
    then DialRelease.Value := Release;
  end;
end;

procedure TFmAdhesive.UpdateSideChainFilter;
var
  SidechainFilter : Single;
begin
 with TAdhesiveDataModule(Owner) do
  begin
   SidechainFilter := Parameter[9];
   if SidechainFilter <> DialFilter.Value
    then DialFilter.Value := SidechainFilter;
  end;
end;

procedure TFmAdhesive.UpdateMix;
var
  Mix : Single;
begin
 with TAdhesiveDataModule(Owner) do
  begin
   Mix := Parameter[6];
   if Mix <> DialMix.Value
    then DialMix.Value := Mix;
  end;
end;

procedure TFmAdhesive.UpdateOnOff;
begin
 with TAdhesiveDataModule(Owner) do
  begin
   SwOnOff.GlyphIndex := 1 - Round(Parameter[7]);
  end;
end;

procedure TFmAdhesive.UpdateExtSideChain;
begin
 with TAdhesiveDataModule(Owner) do
  begin
   SwSideChain.GlyphIndex := 1 - Round(Parameter[10]);
  end;
end;

procedure TFmAdhesive.UpdatePeakClip;
begin
 with TAdhesiveDataModule(Owner) do
  begin
   SwLimit.GlyphIndex := 1 - Round(Parameter[8]);
  end;
end;

end.
