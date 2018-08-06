unit AdvancedClipperGUI;

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
  Forms, Graphics, Controls, StdCtrls, ExtCtrls, 
  DAV_Types, DAV_VSTModule, DAV_GuiPixelMap, DAV_GuiCommon, DAV_GuiInterface,
  DAV_GuiCustomControl, DAV_GuiGraphicControl, DAV_GuiImageControl,
  DAV_GuiGroup, DAV_GuiPanel, DAV_GuiStitchedControls, DAV_GuiStitchedPngList,
  DAV_GuiStitchedDial, DAV_GuiLabel, DAV_GuiLED;

type
  TFmAdvancedClipper = class(TForm, IPixel32Access)
    ClipLEDInput: TGuiLED;
    ClipLEDStage1: TGuiLED;
    ClipLEDStage2: TGuiLED;
    DialFilterOrder1: TGuiStitchedDial;
    DialFilterOrder2: TGuiStitchedDial;
    DialInputGain: TGuiStitchedDial;
    DialOSFactor1: TGuiStitchedDial;
    DialOSFactor2: TGuiStitchedDial;
    DialOutputGain: TGuiStitchedDial;
    DSIL: TGuiStitchedPNGList;
    GpStage1: TGuiGroup;
    GpStage2: TGuiGroup;
    LbClipInput: TGuiLabel;
    LbClipStage1: TGuiLabel;
    LbClipStage2: TGuiLabel;
    LbDisplay: TGuiLabel;
    LbFilterOrder: TGuiLabel;
    LbFilterOrder2: TGuiLabel;
    LbHardClip: TGuiLabel;
    LbInputGain: TGuiLabel;
    LbOSFactor: TGuiLabel;
    LbOSFactor2: TGuiLabel;
    LbOutputGain: TGuiLabel;
    LEDHardClip: TGuiLED;
    PnClipping: TGuiPanel;
    PnDisplay: TGuiPanel;
    PnHardClipping: TGuiPanel;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure DialFilterOrder1Change(Sender: TObject);
    procedure DialFilterOrder2Change(Sender: TObject);
    procedure DialInputGainChange(Sender: TObject);
    procedure DialOSFactor1Change(Sender: TObject);
    procedure DialOSFactor2Change(Sender: TObject);
    procedure DialOutputGainChange(Sender: TObject);
    procedure LbHardClipClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ClipLEDClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FBackgroundBitmap : TGuiCustomPixelMap;
    function GetDataPointer: PPixel32Array;
    function GetPixel(X, Y: Integer): TPixel32;
    function GetPixelPointer(X, Y: Integer): PPixel32;
    function GetScanLine(Y: Integer): PPixel32Array;
    procedure SetPixel(X, Y: Integer; const Value: TPixel32);
  public
    procedure UpdateInputGain;
    procedure UpdateOSFactor1;
    procedure UpdateOSFactor2;
    procedure UpdateOrder1;
    procedure UpdateOrder2;
    procedure UpdateOutputGain;
    procedure UpdateHardClip;
  end;

implementation

{$IFDEF FPC}
{$R *.LFM}
{$ELSE}
{$R *.DFM}
{$ENDIF}

uses
  Math, DAV_Common, AdvancedClipperDM;

procedure TFmAdvancedClipper.FormCreate(Sender: TObject);
begin
 FBackgroundBitmap := TGuiPixelMapMemory.Create;
end;

procedure TFmAdvancedClipper.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackgroundBitmap);
end;

procedure TFmAdvancedClipper.FormPaint(Sender: TObject);
begin
 if Assigned(FBackgroundBitmap)
  then FBackgroundBitmap.PaintTo(Canvas);
end;

procedure TFmAdvancedClipper.FormResize(Sender: TObject);
var
  x, y  : Integer;
  s     : array [0..1] of Single;
  h, hr : Single;
  ScnLn : PPixel32Array;
begin
 with FBackgroundBitmap do
  begin
   SetSize(ClientWidth, ClientHeight);
   s[0] := 0;
   s[1] := 0;
   hr   := 1 / Height;
   for y := 0 to Height - 1 do
    begin
     ScnLn := Scanline[y];
     h    := 0.1 * (1 - Sqr(2 * (y - Height div 2) * hr));
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.97 * s[0] + 0.03 * random;
       s[0] := s[1];

       ScnLn[x].B := Round($70 - $34 * (s[1] - h));
       ScnLn[x].G := Round($84 - $48 * (s[1] - h));
       ScnLn[x].R := Round($8D - $50 * (s[1] - h));
      end;
    end;
  end;
end;

procedure TFmAdvancedClipper.FormShow(Sender: TObject);
begin
 UpdateInputGain;
 UpdateOSFactor1;
 UpdateOSFactor2;
 UpdateOrder1;
 UpdateOrder2;
 UpdateOutputGain;
 UpdateHardClip;
 LbDisplay.Caption := 'Advanced Clipper';
end;

function TFmAdvancedClipper.GetDataPointer: PPixel32Array;
begin
 Result := FBackgroundBitmap.DataPointer;
end;

function TFmAdvancedClipper.GetPixel(X, Y: Integer): TPixel32;
begin
 Result := FBackgroundBitmap.Pixel[X, Y];
end;

function TFmAdvancedClipper.GetPixelPointer(X, Y: Integer): PPixel32;
begin
 Result := FBackgroundBitmap.PixelPointer[X, Y];
end;

function TFmAdvancedClipper.GetScanLine(Y: Integer): PPixel32Array;
begin
 Result := FBackgroundBitmap.ScanLine[Y];
end;

procedure TFmAdvancedClipper.SetPixel(X, Y: Integer; const Value: TPixel32);
begin
 FBackgroundBitmap.Pixel[X, Y] := Value;
end;

procedure TFmAdvancedClipper.DialFilterOrder1Change(Sender: TObject);
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   ParameterByName['Stage 1: Filter Order'] := DialFilterOrder1.Value;
  end;
end;

procedure TFmAdvancedClipper.DialFilterOrder2Change(Sender: TObject);
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   ParameterByName['Stage 2: Filter Order'] := DialFilterOrder2.Value;
  end;
end;

procedure TFmAdvancedClipper.DialInputGainChange(Sender: TObject);
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   ParameterByName['Input Gain'] := DialInputGain.Value;
  end;
end;

procedure TFmAdvancedClipper.DialOutputGainChange(Sender: TObject);
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   ParameterByName['Output Gain'] := DialOutputGain.Value;
  end;
end;

procedure TFmAdvancedClipper.DialOSFactor1Change(Sender: TObject);
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   ParameterByName['Stage 1: Oversampling Factor'] := DialOSFactor1.Value;
  end;
end;

procedure TFmAdvancedClipper.DialOSFactor2Change(Sender: TObject);
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   ParameterByName['Stage 2: Oversampling Factor'] := DialOSFactor2.Value;
  end;
end;

procedure TFmAdvancedClipper.ClipLEDClick(Sender: TObject);
begin
 (Sender As TGuiLED).Brightness_Percent := 0;
end;

procedure TFmAdvancedClipper.LbHardClipClick(Sender: TObject);
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   ParameterByName['Hard Clip'] := 1 - ParameterByName['Hard Clip'];
  end;
end;

procedure TFmAdvancedClipper.TimerTimer(Sender: TObject);
begin
 with TAdvancedClipperDataModule(Owner) do
  begin
   ClipLEDInput.Brightness_Percent := Power(Limit(PeakInput - 1, 0, 1), 0.01) * 100;
   ClipLEDStage1.Brightness_Percent := Power(Limit(PeakStage1 - 1, 0, 1), 0.01) * 100;
   ClipLEDStage2.Brightness_Percent := Power(Limit(PeakStage2 - 1, 0, 1), 0.01) * 100;
  end;
end;

procedure TFmAdvancedClipper.UpdateHardClip;
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   LEDHardClip.Brightness_Percent := 10 + 80 * ParameterByName['Hard Clip'];
  end;
end;

procedure TFmAdvancedClipper.UpdateInputGain;
var
  Value : Single;
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   Value := ParameterByName['Input Gain'];
   if DialInputGain.Value <> Value
    then DialInputGain.Value := Value;
   LbDisplay.Caption := 'Input Gain: ' + FloatToStrF(Value, ffGeneral, 2, 2) + 'dB';
  end;
end;

procedure TFmAdvancedClipper.UpdateOrder1;
var
  Value : Single;
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   Value := ParameterByName['Stage 1: Filter Order'];
   if DialFilterOrder1.Value <> Value
    then DialFilterOrder1.Value := Value;
   LbDisplay.Caption := 'Filter Order: ' + IntToStr(Round(Value));
  end;
end;

procedure TFmAdvancedClipper.UpdateOrder2;
var
  Value : Single;
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   Value := ParameterByName['Stage 2: Filter Order'];
   if DialFilterOrder2.Value <> Value
    then DialFilterOrder2.Value := Value;
   LbDisplay.Caption := 'Filter Order: ' + IntToStr(Round(Value));
  end;
end;

procedure TFmAdvancedClipper.UpdateOSFactor1;
var
  Value : Single;
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   Value := ParameterByName['Stage 1: Oversampling Factor'];
   if DialOSFactor1.Value <> Value
    then DialOSFactor1.Value := Value;
   LbDisplay.Caption := 'Oversampling: ' + IntToStr(Round(Value)) + 'x';
  end;
end;

procedure TFmAdvancedClipper.UpdateOSFactor2;
var
  Value : Single;
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   Value := ParameterByName['Stage 2: Oversampling Factor'];
   if DialOSFactor2.Value <> Value
    then DialOSFactor2.Value := Value;
   LbDisplay.Caption := 'Oversampling: ' + IntToStr(Round(Value)) + 'x';
  end;
end;

procedure TFmAdvancedClipper.UpdateOutputGain;
var
  Value : Single;
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   Value := ParameterByName['Output Gain'];
   if DialOutputGain.Value <> Value
    then DialOutputGain.Value := Value;
   LbDisplay.Caption := 'Output Gain: ' + FloatToStrF(Value, ffGeneral, 2, 2) + 'dB';
  end;
end;

end.
