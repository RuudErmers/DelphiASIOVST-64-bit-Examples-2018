unit BassExtenderGUI;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses 
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, Graphics, Controls, ExtCtrls, DAV_Types, DAV_VSTModule, DAV_GuiPng,  
  DAV_GuiStitchedControls, DAV_GuiStitchedPngList, DAV_GuiStitchedDial, 
  DAV_GuiPixelMap, DAV_GuiPanel, DAV_GuiLabel, DAV_GuiImageControl,
  DAV_GuiCustomControl, DAV_GuiGraphicControl;

type
  TFmBassExtender = class(TForm)
    DialAttack: TGuiStitchedDial;
    DialBalance: TGuiStitchedDial;
    DialCompression: TGuiStitchedDial;
    DialDivide: TGuiStitchedDial;
    DialFrequency: TGuiStitchedDial;
    DialOrder: TGuiStitchedDial;
    DialRatio: TGuiStitchedDial;
    DialRelease: TGuiStitchedDial;
    DialShape: TGuiStitchedDial;
    DialThreshold: TGuiStitchedDial;
    GSPL: TGuiStitchedPNGList;
    LbAttack: TGuiLabel;
    LbAttackValue: TGuiLabel;
    LbBalance: TGuiLabel;
    LbBalanceValue: TGuiLabel;
    LbCompression: TGuiLabel;
    LbCompressionValue: TGuiLabel;
    LbDivide: TGuiLabel;
    LbDivideValue: TGuiLabel;
    LbFrequency: TGuiLabel;
    LbFrequencyValue: TGuiLabel;
    LbOrder: TGuiLabel;
    LbOrderValue: TGuiLabel;
    LbRatio: TGuiLabel;
    LbRatioValue: TGuiLabel;
    LbRelease: TGuiLabel;
    LbReleaseValue: TGuiLabel;
    LbShape: TGuiLabel;
    LbShapeValue: TGuiLabel;
    LbThreshold: TGuiLabel;
    LbThresholdValue: TGuiLabel;
    PnMain: TGuiPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure DialFrequencyChange(Sender: TObject);
    procedure DialOrderChange(Sender: TObject);
    procedure DialDivideChange(Sender: TObject);
    procedure DialShapeChange(Sender: TObject);
    procedure DialBalanceChange(Sender: TObject);
    procedure DialThresholdChange(Sender: TObject);
    procedure DialRatioChange(Sender: TObject);
    procedure DialAttackChange(Sender: TObject);
    procedure DialReleaseChange(Sender: TObject);
    procedure DialCompressionChange(Sender: TObject);
  private
    FBackground : TGuiCustomPixelMap;
  public
    procedure UpdateAttack;
    procedure UpdateBalance;
    procedure UpdateCompressionMix;
    procedure UpdateDivider;
    procedure UpdateRatio;
    procedure UpdateRelease;
    procedure UpdateShape;
    procedure UpdateSplitFrequency;
    procedure UpdateSplitOrder;
    procedure UpdateThreshold;
  end;

implementation

uses
  Math, DAV_GuiCommon, BassExtenderDM;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmBassExtender.FormCreate(Sender: TObject);
begin
 FBackground := TGuiPixelMapMemory.Create;
end;

procedure TFmBassExtender.DialAttackChange(Sender: TObject);
begin
 with TBassExtenderModule(Owner) do
  begin
   if ParameterByName['Attack'] <> DialAttack.Value
    then ParameterByName['Attack'] := DialAttack.Value;
  end;
end;

procedure TFmBassExtender.DialBalanceChange(Sender: TObject);
begin
 with TBassExtenderModule(Owner) do
  begin
   if ParameterByName['Balance'] <> DialBalance.Value
    then ParameterByName['Balance'] := DialBalance.Value;
  end;
end;

procedure TFmBassExtender.DialCompressionChange(Sender: TObject);
begin
 with TBassExtenderModule(Owner) do
  begin
   if ParameterByName['Compression Mix'] <> DialCompression.Value
    then ParameterByName['Compression Mix'] := DialCompression.Value;
  end;
end;

procedure TFmBassExtender.DialDivideChange(Sender: TObject);
begin
 with TBassExtenderModule(Owner) do
  begin
   if ParameterByName['Divider'] <> DialDivide.Value
    then ParameterByName['Divider'] := DialDivide.Value;
  end;
end;

procedure TFmBassExtender.DialFrequencyChange(Sender: TObject);
begin
 with TBassExtenderModule(Owner) do
  begin
   if ParameterByName['Split Frequency'] <> DialFrequency.Value
    then ParameterByName['Split Frequency'] := DialFrequency.Value;
  end;
end;

procedure TFmBassExtender.DialOrderChange(Sender: TObject);
begin
 with TBassExtenderModule(Owner) do
  begin
   if ParameterByName['Split Order'] <> DialOrder.Value
    then ParameterByName['Split Order'] := DialOrder.Value;
  end;
end;

procedure TFmBassExtender.DialRatioChange(Sender: TObject);
begin
 with TBassExtenderModule(Owner) do
  begin
   if ParameterByName['Ratio'] <> DialRatio.Value
    then ParameterByName['Ratio'] := DialRatio.Value;
  end;
end;

procedure TFmBassExtender.DialReleaseChange(Sender: TObject);
begin
 with TBassExtenderModule(Owner) do
  begin
   if ParameterByName['Release'] <> DialRelease.Value
    then ParameterByName['Release'] := DialRelease.Value;
  end;
end;

procedure TFmBassExtender.DialShapeChange(Sender: TObject);
begin
 with TBassExtenderModule(Owner) do
  begin
   if ParameterByName['Shape'] <> DialShape.Value
    then ParameterByName['Shape'] := DialShape.Value;
  end;
end;

procedure TFmBassExtender.DialThresholdChange(Sender: TObject);
begin
 with TBassExtenderModule(Owner) do
  begin
   if ParameterByName['Threshold'] <> DialThreshold.Value
    then ParameterByName['Threshold'] := DialThreshold.Value;
  end;
end;

procedure TFmBassExtender.FormPaint(Sender: TObject);
begin
 if Assigned(FBackground)
  then FBackground.PaintTo(Canvas);
end;

procedure TFmBassExtender.FormResize(Sender: TObject);
var
  x, y   : Integer;
  Filter : array [0..1] of Single;
  h, hr  : Single;
  ScnLn  : PPixel32Array;
begin
 with FBackground do
  begin
   SetSize(ClientWidth, ClientHeight);
   Filter[0] := 0;
   Filter[1] := 0;
   hr := 1 / Height;
   for y := 0 to Height - 1 do
    begin
     ScnLn := Scanline[y];
     h := 0.3 * (1 - Sqr(2 * (y - Height div 2) * hr));
     for x := 0 to Width - 1 do
      begin
       Filter[1] := 0.97 * Filter[0] + 0.03 * Random;
       Filter[0] := Filter[1];

       ScnLn[x].B := Round($30 - $24 * (Filter[1] - h));
       ScnLn[x].G := Round($44 - $38 * (Filter[1] - h));
       ScnLn[x].R := Round($4D - $40 * (Filter[1] - h));
       ScnLn[x].A := $FF;
      end;
    end;
  end;
end;

procedure TFmBassExtender.FormShow(Sender: TObject);
begin
 UpdateAttack;
 UpdateBalance;
 UpdateCompressionMix;
 UpdateDivider;
 UpdateRatio;
 UpdateRelease;
 UpdateShape;
 UpdateSplitFrequency;
 UpdateSplitOrder;
 UpdateThreshold;
end;

procedure TFmBassExtender.UpdateAttack;
var
  Attack : Single;
begin
 with TBassExtenderModule(Owner) do
  begin
   Attack := ParameterByName['Attack'];
   if DialAttack.Value <> Attack
    then DialAttack.Value := Attack;
   if Attack < 1000
    then LbAttackValue.Caption := FloatToStrF(Attack, ffGeneral, 3, 3) + ' µs'
    else LbAttackValue.Caption := FloatToStrF(1E-3 * Attack, ffGeneral, 3, 3) + ' ms';
  end;
end;

procedure TFmBassExtender.UpdateBalance;
var
  Balance : Single;
begin
 with TBassExtenderModule(Owner) do
  begin
   Balance := ParameterByName['Balance'];
   if DialBalance.Value <> Balance
    then DialBalance.Value := Balance;
//   Balance := Round(1E5 * Balance) * 1E-5;
   LbBalanceValue.Caption := FloatToStrF(RoundTo(Balance, -2), ffGeneral, 3, 4) + '%';
  end;
end;

procedure TFmBassExtender.UpdateCompressionMix;
var
  Compression : Single;
begin
 with TBassExtenderModule(Owner) do
  begin
   Compression := ParameterByName['Compression Mix'];
   if DialCompression.Value <> Compression
    then DialCompression.Value := Compression;
   LbCompressionValue.Caption := FloatToStrF(RoundTo(Compression, -2), ffGeneral, 3, 3) + '%';
  end;
end;

procedure TFmBassExtender.UpdateDivider;
var
  Divider : Single;
begin
 with TBassExtenderModule(Owner) do
  begin
   Divider := ParameterByName['Divider'];
   if DialDivide.Value <> Divider
    then DialDivide.Value := Divider;
   LbDivideValue.Caption := FloatToStrF(Divider, ffGeneral, 3, 3) + '%';
  end;
end;

procedure TFmBassExtender.UpdateRatio;
var
  Ratio : Single;
begin
 with TBassExtenderModule(Owner) do
  begin
   Ratio := ParameterByName['Ratio'];
   if DialRatio.Value <> Ratio
    then DialRatio.Value := Ratio;
   if Ratio = 1000
    then LbRatioValue.Caption := '1 : oo'
    else LbRatioValue.Caption := '1 : ' + FloatToStrF(Ratio, ffGeneral, 3, 4);
  end;
end;

procedure TFmBassExtender.UpdateRelease;
var
  Release : Single;
begin
 with TBassExtenderModule(Owner) do
  begin
   Release := ParameterByName['Release'];
   if DialRelease.Value <> Release
    then DialRelease.Value := Release;
   if Release < 1000
    then LbReleaseValue.Caption := FloatToStrF(Release, ffGeneral, 3, 2) + ' ms'
    else LbReleaseValue.Caption := FloatToStrF(Release * 1E-3, ffGeneral, 3, 2) + ' s';
  end;
end;

procedure TFmBassExtender.UpdateShape;
var
  Shape : Single;
begin
 with TBassExtenderModule(Owner) do
  begin
   Shape := ParameterByName['Shape'];
   if DialShape.Value <> Shape
    then DialShape.Value := Shape;
   LbShapeValue.Caption := FloatToStrF(RoundTo(Shape, -2), ffGeneral, 3, 2) + '%';
  end;
end;

procedure TFmBassExtender.UpdateSplitFrequency;
var
  Frequency : Single;
begin
 with TBassExtenderModule(Owner) do
  begin
   Frequency := ParameterByName['Split Frequency'];
   if DialFrequency.Value <> Frequency
    then DialFrequency.Value := Frequency;
   if Frequency < 1000
    then LbFrequencyValue.Caption := FloatToStrF(Frequency, ffGeneral, 3, 2) + ' Hz'
    else LbFrequencyValue.Caption := FloatToStrF(Frequency * 1E-3, ffGeneral, 3, 2) + ' kHz';
  end;
end;

procedure TFmBassExtender.UpdateSplitOrder;
var
  Order : Single;
begin
 with TBassExtenderModule(Owner) do
  begin
   Order := ParameterByName['Split Order'];
   if DialOrder.Value <> Order
    then DialOrder.Value := Order;
   LbOrderValue.Caption := IntToStr(2 * Round(Order));
  end;
end;

procedure TFmBassExtender.UpdateThreshold;
var
  Threshold : Single;
begin
 with TBassExtenderModule(Owner) do
  begin
   Threshold := ParameterByName['Threshold'];
   if DialThreshold.Value <> Threshold
    then DialThreshold.Value := Threshold;
   Threshold := Round(1E4 * Threshold) * 1E-4;
   LbThresholdValue.Caption := FloatToStrF(RoundTo(Threshold, -2), ffGeneral, 3, 4) + 'dB';
  end;
end;

end.
