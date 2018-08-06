unit TwoBandDistortionGUI;

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
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} Classes,
  SysUtils, Forms, Controls, Graphics, ExtCtrls, StdCtrls, DAV_GuiLabel,
  DAV_GuiPanel, DAV_GuiPixelMap, DAV_GuiStitchedPngList, DAV_GuiStitchedDial,
  DAV_GuiImageControl, DAV_GuiStitchedControls, DAV_GuiCustomControl,
  DAV_GuiGraphicControl;

type
  TFmTwoBandDistortion = class(TForm)
    DialFreq: TGuiStitchedDial;
    DialHighDist: TGuiStitchedDial;
    DialLowDist: TGuiStitchedDial;
    DialOrder: TGuiStitchedDial;
    DSIL: TGuiStitchedPNGList;
    LbFreq: TGuiLabel;
    LbFreqValue: TGuiLabel;
    LbHighDist: TGuiLabel;
    LbHighDistValue: TGuiLabel;
    LbLowDist: TGuiLabel;
    LbLowDistValue: TGuiLabel;
    LbOrder: TGuiLabel;
    LbOrderValue: TGuiLabel;
    PnControl: TGuiPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure DialFreqChange(Sender: TObject);
    procedure DialLowDistChange(Sender: TObject);
    procedure DialHighDistChange(Sender: TObject);
    procedure DialOrderChange(Sender: TObject);
  private
    FBackground : TGuiCustomPixelMap;
    FEdValue    : TEdit;
  public
    procedure UpdateFrequency;
    procedure UpdateOrder;
    procedure UpdateLowDistortion;
    procedure UpdateHighDistortion;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, DAV_GUICommon, DAV_GuiPng, TwoBandDistortionDM;

procedure TFmTwoBandDistortion.FormCreate(Sender: TObject);
begin
 FBackground := TGuiPixelMapMemory.Create;
end;

procedure TFmTwoBandDistortion.FormDestroy(Sender: TObject);
begin
 if Assigned(FEdValue)
  then FreeAndNil(FEdValue);
end;

procedure TFmTwoBandDistortion.FormPaint(Sender: TObject);
begin
 if Assigned(FBackground)
  then FBackground.PaintTo(Canvas);
end;

procedure TFmTwoBandDistortion.FormResize(Sender: TObject);
var
  x, y   : Integer;
  s      : array [0..1] of Single;
  b      : ShortInt;
  ScnLn  : PPixel32Array;
begin
 if Assigned(FBackground) then
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
        s[1] := 0.97 * s[0] + 0.03 * (2 * Random - 1);
        b := Round($3F + $1A * s[1]);
        s[0] := s[1];
        ScnLn[x].B := b;
        ScnLn[x].G := b;
        ScnLn[x].R := b;
       end;
     end;
   end;
end;

procedure TFmTwoBandDistortion.DialFreqChange(Sender: TObject);
begin
 with Owner as TTwoBandDistortionDataModule do
  begin
   ParameterByName['Frequency'] := DialFreq.Value;
  end;
end;

procedure TFmTwoBandDistortion.DialHighDistChange(Sender: TObject);
begin
 with Owner as TTwoBandDistortionDataModule do
  begin
   ParameterByName['High Distortion'] := DialHighDist.Value;
  end; 
end;

procedure TFmTwoBandDistortion.DialLowDistChange(Sender: TObject);
begin
 with Owner as TTwoBandDistortionDataModule do
  begin
   ParameterByName['Low Distortion'] := DialLowDist.Value;
  end;
end;

procedure TFmTwoBandDistortion.DialOrderChange(Sender: TObject);
var
  CurrentOrder : Single;
  DesiredOrder : Integer;
begin
 with Owner as TTwoBandDistortionDataModule do
  begin
   DesiredOrder := Round(DialOrder.Value);
   CurrentOrder := ParameterByName['Order'];
   if Round(CurrentOrder) = DesiredOrder then
    if DialOrder.Value < CurrentOrder
     then ParameterByName['Order'] := DesiredOrder - 1 else
    if DialOrder.Value > CurrentOrder
     then ParameterByName['Order'] := DesiredOrder + 1 else
  end;
end;

procedure TFmTwoBandDistortion.FormShow(Sender: TObject);
begin
 UpdateFrequency;
 UpdateOrder;
 UpdateLowDistortion;
 UpdateHighDistortion;
end;

procedure TFmTwoBandDistortion.UpdateFrequency;
var
  Freq : Single;
const
  CThousand : Single = 1000;
begin
 with Owner as TTwoBandDistortionDataModule do
  begin
   Freq := ParameterByName['Frequency'];
   if Freq < CThousand
    then LbFreqValue.Caption := FloatToStrF(Freq, ffGeneral, 3, 4) + ' Hz'
    else LbFreqValue.Caption := FloatToStrF(1E-3 * Freq, ffGeneral, 3, 3) + ' kHz';
   if DialFreq.Value <> Freq
    then DialFreq.Value := Freq;
  end;
end;

procedure TFmTwoBandDistortion.UpdateHighDistortion;
var
  HighDist : Single;
begin
 with Owner as TTwoBandDistortionDataModule do
  begin
   HighDist := ParameterByName['High Distortion'];
   LbHighDistValue.Caption := FloatToStrF(RoundTo(HighDist, -2), ffGeneral, 3, 1) + '%';
   if DialHighDist.Value <> HighDist
    then DialHighDist.Value := HighDist;
  end;
end;

procedure TFmTwoBandDistortion.UpdateLowDistortion;
var
  LowDist : Single;
begin
 with Owner as TTwoBandDistortionDataModule do
  begin
   LowDist := ParameterByName['Low Distortion'];
   LbLowDistValue.Caption := FloatToStrF(RoundTo(LowDist, -2), ffGeneral, 3, 1) + '%';
   if DialLowDist.Value <> LowDist
    then DialLowDist.Value := LowDist;
  end;
end;

procedure TFmTwoBandDistortion.UpdateOrder;
var
  Order : Integer;
begin
 with Owner as TTwoBandDistortionDataModule do
  begin
   Order := Round(ParameterByName['Order']);
   LbOrderValue.Caption := IntToStr(Order);
   if DialOrder.Value <> Order
    then DialOrder.Value := Order;
  end;
end;

end.
