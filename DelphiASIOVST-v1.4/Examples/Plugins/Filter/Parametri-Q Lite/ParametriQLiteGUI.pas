unit ParametriQLiteGUI;

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
  Forms, Controls, Graphics, Menus, ExtCtrls, DAV_Types, DAV_VSTModule, 
  DAV_GuiBaseControl, DAV_GuiLabel, DAV_GuiVUMeter, DAV_GuiEQGraph, 
  DAV_GuiPixelMap, DAV_GuiStitchedSwitch, DAV_GuiStitchedDisplay, 
  DAV_GuiStitchedControls, DAV_GuiStitchedPngList, DAV_GuiStitchedDial, 
  DAV_GuiCustomControl, DAV_GuiGraphicControl, DAV_GuiImageControl,
  DAV_GuiFont;

type
  TFmParametriQLite = class(TForm)
    Box1: TShape;
    Box2: TShape;
    Box3: TShape;
    Box4: TShape;
    Box5: TShape;
    Box6: TShape;
    Box7: TShape;
    Box8: TShape;
    DialBW1: TGuiStitchedDial;
    DialBW2: TGuiStitchedDial;
    DialBW3: TGuiStitchedDial;
    DialBW4: TGuiStitchedDial;
    DialBW5: TGuiStitchedDial;
    DialBW6: TGuiStitchedDial;
    DialBW7: TGuiStitchedDial;
    DialBW8: TGuiStitchedDial;
    DialFreq1: TGuiStitchedDial;
    DialFreq2: TGuiStitchedDial;
    DialFreq3: TGuiStitchedDial;
    DialFreq4: TGuiStitchedDial;
    DialFreq5: TGuiStitchedDial;
    DialFreq6: TGuiStitchedDial;
    DialFreq7: TGuiStitchedDial;
    DialFreq8: TGuiStitchedDial;
    DialGain1: TGuiStitchedDial;
    DialGain2: TGuiStitchedDial;
    DialGain3: TGuiStitchedDial;
    DialGain4: TGuiStitchedDial;
    DialGain5: TGuiStitchedDial;
    DialGain6: TGuiStitchedDial;
    DialGain7: TGuiStitchedDial;
    DialGain8: TGuiStitchedDial;
    DialInput: TGuiStitchedDial;
    DialOutput: TGuiStitchedDial;
    GSPL: TGuiStitchedPNGList;
    GuiEQGraph: TGuiEQGraph;
    GuiMax1: TGuiLabel;
    GuiMax2: TGuiLabel;
    GuiMax3: TGuiLabel;
    GuiMax4: TGuiLabel;
    GuiMax5: TGuiLabel;
    GuiMax6: TGuiLabel;
    GuiMax7: TGuiLabel;
    GuiMax8: TGuiLabel;
    GuiMin1: TGuiLabel;
    GuiMin2: TGuiLabel;
    GuiMin3: TGuiLabel;
    GuiMin4: TGuiLabel;
    GuiMin5: TGuiLabel;
    GuiMin6: TGuiLabel;
    GuiMin7: TGuiLabel;
    GuiMin8: TGuiLabel;
    LbBW1: TGuiLabel;
    LbBW2: TGuiLabel;
    LbBW3: TGuiLabel;
    LbBW4: TGuiLabel;
    LbBW5: TGuiLabel;
    LbBW6: TGuiLabel;
    LbBW7: TGuiLabel;
    LbBW8: TGuiLabel;
    LbdB: TGuiLabel;
    LbFreq1: TGuiLabel;
    LbFreq2: TGuiLabel;
    LbFreq3: TGuiLabel;
    LbFreq4: TGuiLabel;
    LbFreq5: TGuiLabel;
    LbFreq6: TGuiLabel;
    LbFreq7: TGuiLabel;
    LbFreq8: TGuiLabel;
    LbFreqValue1: TGuiLabel;
    LbFreqValue2: TGuiLabel;
    LbFreqValue3: TGuiLabel;
    LbFreqValue4: TGuiLabel;
    LbFreqValue5: TGuiLabel;
    LbFreqValue6: TGuiLabel;
    LbFreqValue7: TGuiLabel;
    LbFreqValue8: TGuiLabel;
    LbGain: TGuiLabel;
    LbGain1: TGuiLabel;
    LbGain2: TGuiLabel;
    LbGain3: TGuiLabel;
    LbGain4: TGuiLabel;
    LbGain5: TGuiLabel;
    LbGain6: TGuiLabel;
    LbGain7: TGuiLabel;
    LbGain8: TGuiLabel;
    LbIn: TGuiLabel;
    LbInput: TGuiLabel;
    LbOut: TGuiLabel;
    LbOutput: TGuiLabel;
    LbTitle: TGuiLabel;
    LbType1: TGuiLabel;
    LbType2: TGuiLabel;
    LbType3: TGuiLabel;
    LbType4: TGuiLabel;
    LbType5: TGuiLabel;
    LbType6: TGuiLabel;
    LbType7: TGuiLabel;
    LbType8: TGuiLabel;
    LbTypeValue1: TGuiLabel;
    LbTypeValue2: TGuiLabel;
    LbTypeValue3: TGuiLabel;
    LbTypeValue4: TGuiLabel;
    LbTypeValue5: TGuiLabel;
    LbTypeValue6: TGuiLabel;
    LbTypeValue7: TGuiLabel;
    LbTypeValue8: TGuiLabel;
    MIAllpass: TMenuItem;
    MIBandpass: TMenuItem;
    MIBypass: TMenuItem;
    MIHighpass: TMenuItem;
    MIHighshelf: TMenuItem;
    MILowpass: TMenuItem;
    MILowshelf: TMenuItem;
    MILowShelfA: TMenuItem;
    MINotch: TMenuItem;
    MIPeak: TMenuItem;
    N1: TMenuItem;
    PopupFilter: TPopupMenu;
    SeparatorA1: TShape;
    SeparatorA2: TShape;
    SeparatorA3: TShape;
    SeparatorA4: TShape;
    SeparatorA5: TShape;
    SeparatorA6: TShape;
    SeparatorA7: TShape;
    SeparatorA8: TShape;
    SeparatorB1: TShape;
    SeparatorB2: TShape;
    SeparatorB3: TShape;
    SeparatorB4: TShape;
    SeparatorB5: TShape;
    SeparatorB6: TShape;
    SeparatorB7: TShape;
    SeparatorB8: TShape;
    SeparatorC1: TShape;
    SeparatorC2: TShape;
    SeparatorC3: TShape;
    SeparatorC4: TShape;
    SeparatorC5: TShape;
    SeparatorC6: TShape;
    SeparatorC7: TShape;
    SeparatorC8: TShape;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    Shape5: TShape;
    Shape6: TShape;
    Shape7: TShape;
    Shape8: TShape;
    ShapeInfo: TShape;
    ShapeInputBottom: TShape;
    ShapeInputLeft: TShape;
    ShapeOutputBottom: TShape;
    ShapeOutputRight: TShape;
    Switch: TGuiStitchedSwitch;
    Timer: TTimer;
    VUMeter: TGuiStitchedDisplay;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DialBWChange(Sender: TObject);
    procedure DialFreqChange(Sender: TObject);
    procedure DialGainChange(Sender: TObject);
    procedure DialInputChange(Sender: TObject);
    procedure DialOutputChange(Sender: TObject);
    procedure LbTypeClick(Sender: TObject);
    procedure MIAllpassClick(Sender: TObject);
    procedure MIBandpassClick(Sender: TObject);
    procedure MIBypassClick(Sender: TObject);
    procedure MIHighpassClick(Sender: TObject);
    procedure MIHighshelfClick(Sender: TObject);
    procedure MILowpassClick(Sender: TObject);
    procedure MILowShelfAClick(Sender: TObject);
    procedure MILowshelfClick(Sender: TObject);
    procedure MINotchClick(Sender: TObject);
    procedure MIPeakClick(Sender: TObject);
    procedure PopupFilterPopup(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    function GetFilterGain(Sender: TObject; const Frequency: Single): Single;
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CurrentPlotMouseEnter(Sender: TObject);
    procedure CurrentPlotMouseLeave(Sender: TObject);
    function GetCurrentFilterGain(Sender: TObject;
      const Frequency: Single): Single;
  private
    FBackground : TGuiCustomPixelMap;
    FCurrent    : Integer;
  public
    procedure UpdateGain(const Index: Integer);
    procedure UpdateBandwidth(const Index: Integer);
    procedure UpdateFrequency(const Index: Integer);
    procedure UpdateFilterType(const Index: Integer);
  end;

implementation

uses
  DAV_Common, DAV_GuiCommon, DAV_Approximations, DAV_VSTModuleWithPrograms,
  ParametriQLiteDM;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmParametriQLite.FormCreate(Sender: TObject);
begin
 FBackground := TGuiPixelMapMemory.Create;
 ShapeInfo.ControlStyle := ShapeInfo.ControlStyle + [csOpaque];
end;

procedure TFmParametriQLite.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackground);
end;

procedure TFmParametriQLite.FormPaint(Sender: TObject);
begin
 if Assigned(FBackground)
  then FBackground.PaintTo(Canvas);
end;

procedure TFmParametriQLite.FormResize(Sender: TObject);
var
  X, Y   : Integer;
  Filter : array [0..1] of Single;
  Value  : ShortInt;
  Rct    : TRect;
  ScnLn  : PPixel32Array;
const
  CDisplayRectFrame : TPixel32 = (ARGB: $FF414342);
  CDisplayRectFill : TPixel32 = (ARGB: $FF151716);
begin
 with FBackground do
  begin
   SetSize(ClientWidth, ClientHeight);
   Filter[0] := 0;
   Filter[1] := 0;
   for Y := 0 to Height - 1 do
    begin
     ScnLn := Scanline[Y];
     for X := 0 to Width - 1 do
      begin
       Filter[1] := 0.9 * Filter[0] + 0.1 * Random;
       Value := Round($C * Filter[1]) - $6;
       Filter[0] := Filter[1];
       ScnLn[X].B := $2F + Value;
       ScnLn[X].G := $30 + Value;
       ScnLn[X].R := $2E + Value;
      end;
    end;
   Rct := Rect(8, 8, 221, 190);
   FillRect(Rct, CDisplayRectFill);
   FrameRect(Rct, CDisplayRectFrame);
  end;
end;

procedure TFmParametriQLite.FormShow(Sender: TObject);
var
  Band : Integer;
begin
 for Band := 0 to 7 do
  begin
   UpdateGain(Band);
   UpdateBandwidth(Band);
   UpdateFrequency(Band);
   UpdateFilterType(Band);
  end;
 GuiEQGraph.YAxis.Granularity := 10; 
end;

function TFmParametriQLite.GetFilterGain(
  Sender: TObject; const Frequency: Single): Single;
var
  Band : Integer;
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   Result := Filter[0].MagnitudeSquared(0.5 * Frequency);
   for Band := 1 to 7
    do Result := Result * Filter[Band].MagnitudeSquared(0.5 * Frequency);
   Result := 10 * FastLog10MinError5(Result);
  end;
end;

function TFmParametriQLite.GetCurrentFilterGain(Sender: TObject;
  const Frequency: Single): Single;
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   Result := 10 * FastLog10MinError5(
     Filter[FCurrent].MagnitudeSquared(0.5 * Frequency));
  end;
end;

procedure TFmParametriQLite.LbTypeClick(Sender: TObject);
begin
 Assert(Sender is TGuiLabel);
 PopupFilter.Tag := (TGuiLabel(Sender).Tag - 1);
 PopupFilter.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

procedure TFmParametriQLite.MIBypassClick(Sender: TObject);
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   Parameter[PopupFilter.Tag * 4 + 4] := 0;
  end;
end;

procedure TFmParametriQLite.MIPeakClick(Sender: TObject);
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   Parameter[PopupFilter.Tag * 4 + 4] := 1;
  end;
end;

procedure TFmParametriQLite.MILowshelfClick(Sender: TObject);
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   Parameter[PopupFilter.Tag * 4 + 4] := 2;
  end;
end;

procedure TFmParametriQLite.MIHighshelfClick(Sender: TObject);
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   Parameter[PopupFilter.Tag * 4 + 4] := 3;
  end;
end;

procedure TFmParametriQLite.MILowpassClick(Sender: TObject);
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   Parameter[PopupFilter.Tag * 4 + 4] := 4;
  end;
end;

procedure TFmParametriQLite.MILowShelfAClick(Sender: TObject);
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   Parameter[PopupFilter.Tag * 4 + 4] := 9;
  end;
end;

procedure TFmParametriQLite.MIHighpassClick(Sender: TObject);
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   Parameter[PopupFilter.Tag * 4 + 4] := 5;
  end;
end;

procedure TFmParametriQLite.MIBandpassClick(Sender: TObject);
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   Parameter[PopupFilter.Tag * 4 + 4] := 6;
  end;
end;

procedure TFmParametriQLite.MINotchClick(Sender: TObject);
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   Parameter[PopupFilter.Tag * 4 + 4] := 7;
  end;
end;

procedure TFmParametriQLite.MIAllpassClick(Sender: TObject);
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   Parameter[PopupFilter.Tag * 4 + 4] := 8;
  end;
end;

function FastFreqLogToLinear(Value: Single): Single;
const
  fltl1 : Single = 0.05;
  fltl2 : Single = 0.1;
begin
 Result := FastLog2MinError3(value * fltl1) * fltl2;
end;

function FastFreqLinearToLog(Value: Single): Single;
const
  fltl1 : Single = 9.9657840729;
begin
 Result := (CTwenty32 * FastPower2MinError3 (value * fltl1));
end;

procedure TFmParametriQLite.PopupFilterPopup(Sender: TObject);
var
  FilterType: Integer;
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   FilterType          := Round(Parameter[PopupFilter.Tag * 4 + 4]);
   MIBypass.Checked    := FilterType = 0;
   MIPeak.Checked      := FilterType = 1;
   MILowshelf.Checked  := FilterType = 2;
   MIHighshelf.Checked := FilterType = 3;
   MILowpass.Checked   := FilterType = 4;
   MIHighpass.Checked  := FilterType = 5;
   MIBandpass.Checked  := FilterType = 6;
   MINotch.Checked     := FilterType = 7;
   MIAllpass.Checked   := FilterType = 8;
  end;
end;

procedure TFmParametriQLite.TimerTimer(Sender: TObject);
var
  PeakLevel : Single;
const
  COne25th : Single = 1 / 25;
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   if Switch.GlyphIndex = 0
    then PeakLevel := InputPeakLevel
    else PeakLevel := OutputPeakLevel;

   VUMeter.GlyphIndex := Round((25 + Limit(PeakLevel, -25, 0)) * VUMeter.GlyphCount * COne25th);
  end;
end;

procedure TFmParametriQLite.UpdateFrequency(const Index: Integer);
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   case Index of
    0 : begin
         if DialFreq1.Value <> Parameter[Index * 4 + 1]
          then DialFreq1.Value := Parameter[Index * 4 + 1];
//         LbFreq1.Caption := ParameterDisplay[Index * 4 + 1];
        end;
    1 : begin
         if DialFreq2.Value <> Parameter[Index * 4 + 1]
          then DialFreq2.Value := Parameter[Index * 4 + 1];
//         LbFreq2.Caption := ParameterDisplay[Index * 4 + 1];
        end;
    2 : begin
         if DialFreq3.Value <> Parameter[Index * 4 + 1]
          then DialFreq3.Value := Parameter[Index * 4 + 1];
//         LbFreq3.Caption := ParameterDisplay[Index * 4 + 1];
        end;
    3 : begin
         if DialFreq4.Value <> Parameter[Index * 4 + 1]
          then DialFreq4.Value := Parameter[Index * 4 + 1];
//         LbFreq4.Caption := ParameterDisplay[Index * 4 + 1];
        end;
    4 : begin
         if DialFreq5.Value <> Parameter[Index * 4 + 1]
          then DialFreq5.Value := Parameter[Index * 4 + 1];
//         LbFreq5.Caption := ParameterDisplay[Index * 4 + 1];
        end;
    5 : begin
         if DialFreq6.Value <> Parameter[Index * 4 + 1]
          then DialFreq6.Value := Parameter[Index * 4 + 1];
//         LbFreq6.Caption := ParameterDisplay[Index * 4 + 1];
        end;
    6 : begin
         if DialFreq7.Value <> Parameter[Index * 4 + 1]
          then DialFreq7.Value := Parameter[Index * 4 + 1];
//         LbFreq7.Caption := ParameterDisplay[Index * 4 + 1];
        end;
    7 : begin
         if DialFreq8.Value <> Parameter[Index * 4 + 1]
          then DialFreq8.Value := Parameter[Index * 4 + 1];
//         LbFreq8.Caption := ParameterDisplay[Index * 4 + 1];
        end;
   end;
  end;
 GuiEQGraph.UpdateGraph;
end;

procedure TFmParametriQLite.UpdateBandwidth(const Index: Integer);
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   case Index of
    0 : begin
         if DialBW1.Value <> Parameter[Index * 4 + 2]
          then DialBW1.Value := Parameter[Index * 4 + 2];
//         LbBW1.Caption := ParameterDisplay[Index * 4 + 2];
        end;
    1 : begin
         if DialBW2.Value <> Parameter[Index * 4 + 2]
          then DialBW2.Value := Parameter[Index * 4 + 2];
//         LbBW2.Caption := ParameterDisplay[Index * 4 + 2];
        end;
    2 : begin
         if DialBW3.Value <> Parameter[Index * 4 + 2]
          then DialBW3.Value := Parameter[Index * 4 + 2];
//         LbBW3.Caption := ParameterDisplay[Index * 4 + 2];
        end;
    3 : begin
         if DialBW4.Value <> Parameter[Index * 4 + 2]
          then DialBW4.Value := Parameter[Index * 4 + 2];
//         LbBW4.Caption := ParameterDisplay[Index * 4 + 2];
        end;
    4 : begin
         if DialBW5.Value <> Parameter[Index * 4 + 2]
          then DialBW5.Value := Parameter[Index * 4 + 2];
//         LbBW5.Caption := ParameterDisplay[Index * 4 + 2];
        end;
    5 : begin
         if DialBW6.Value <> Parameter[Index * 4 + 2]
          then DialBW6.Value := Parameter[Index * 4 + 2];
//         LbBW6.Caption := ParameterDisplay[Index * 4 + 2];
        end;
    6 : begin
         if DialBW7.Value <> Parameter[Index * 4 + 2]
          then DialBW7.Value := Parameter[Index * 4 + 2];
//         LbBW7.Caption := ParameterDisplay[Index * 4 + 2];
        end;
    7 : begin
         if DialBW8.Value <> Parameter[Index * 4 + 2]
          then DialBW8.Value := Parameter[Index * 4 + 2];
//         LbBW8.Caption := ParameterDisplay[Index * 4 + 2];
        end;
   end;
  end;
 GuiEQGraph.UpdateGraph;
end;

procedure TFmParametriQLite.UpdateGain(const Index: Integer);
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   case Index of
    0 : begin
         if DialGain1.Value <> Parameter[Index * 4 + 3]
          then DialGain1.Value := Parameter[Index * 4 + 3];
//         LbGain1.Caption := ParameterDisplay[Index * 4 + 3];
        end;
    1 : begin
         if DialGain2.Value <> Parameter[Index * 4 + 3]
          then DialGain2.Value := Parameter[Index * 4 + 3];
//         LbGain2.Caption := ParameterDisplay[Index * 4 + 3];
        end;
    2 : begin
         if DialGain3.Value <> Parameter[Index * 4 + 3]
          then DialGain3.Value := Parameter[Index * 4 + 3];
//         LbGain3.Caption := ParameterDisplay[Index * 4 + 3];
        end;
    3 : begin
         if DialGain4.Value <> Parameter[Index * 4 + 3]
          then DialGain4.Value := Parameter[Index * 4 + 3];
//         LbGain4.Caption := ParameterDisplay[Index * 4 + 3];
        end;
    4 : begin
         if DialGain5.Value <> Parameter[Index * 4 + 3]
          then DialGain5.Value := Parameter[Index * 4 + 3];
//         LbGain5.Caption := ParameterDisplay[Index * 4 + 3];
        end;
    5 : begin
         if DialGain6.Value <> Parameter[Index * 4 + 3]
          then DialGain6.Value := Parameter[Index * 4 + 3];
//         LbGain6.Caption := ParameterDisplay[Index * 4 + 3];
        end;
    6 : begin
         if DialGain7.Value <> Parameter[Index * 4 + 3]
          then DialGain7.Value := Parameter[Index * 4 + 3];
//         LbGain7.Caption := ParameterDisplay[Index * 4 + 3];
        end;
    7 : begin
         if DialGain8.Value <> Parameter[Index * 4 + 3]
          then DialGain8.Value := Parameter[Index * 4 + 3];
//         LbGain8.Caption := ParameterDisplay[Index * 4 + 3];
        end;
   end;
  end;
 GuiEQGraph.UpdateGraph;
end;

procedure TFmParametriQLite.UpdateFilterType(const Index: Integer);
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   case Index of
    0 : LbTypeValue1.Caption := string(ParameterDisplay[Index * 4 + 4]);
    1 : LbTypeValue2.Caption := string(ParameterDisplay[Index * 4 + 4]);
    2 : LbTypeValue3.Caption := string(ParameterDisplay[Index * 4 + 4]);
    3 : LbTypeValue4.Caption := string(ParameterDisplay[Index * 4 + 4]);
    4 : LbTypeValue5.Caption := string(ParameterDisplay[Index * 4 + 4]);
    5 : LbTypeValue6.Caption := string(ParameterDisplay[Index * 4 + 4]);
    6 : LbTypeValue7.Caption := string(ParameterDisplay[Index * 4 + 4]);
    7 : LbTypeValue8.Caption := string(ParameterDisplay[Index * 4 + 4]);
   end;
  end;
 GuiEQGraph.UpdateGraph;
end;

procedure TFmParametriQLite.CurrentPlotMouseEnter(Sender: TObject);
begin
 GuiEQGraph.FilterSeries[1].Visible := True;
 FCurrent := TComponent(Sender).Tag - 1;
end;

procedure TFmParametriQLite.CurrentPlotMouseLeave(Sender: TObject);
begin
 GuiEQGraph.FilterSeries[1].Visible := False;
end;

procedure TFmParametriQLite.DialFreqChange(Sender: TObject);
var
  Band : Integer;
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   Band := (TGuiStitchedDial(Sender).Tag - 1);
   Parameter[Band * 4 + 1] := TGuiStitchedDial(Sender).Value;
  end;
end;

procedure TFmParametriQLite.DialBWChange(Sender: TObject);
var
  Band : Integer;
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   Band := (TGuiStitchedDial(Sender).Tag - 1);
   Parameter[Band * 4 + 2] := TGuiStitchedDial(Sender).Value;
  end;
end;

procedure TFmParametriQLite.DialGainChange(Sender: TObject);
var
  Band : Integer;
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   Band := (TGuiStitchedDial(Sender).Tag - 1);
   Parameter[Band * 4 + 3] := TGuiStitchedDial(Sender).Value;
  end;
end;

procedure TFmParametriQLite.DialInputChange(Sender: TObject);
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   Parameter[0] := DialInput.Value;
  end;
end;

procedure TFmParametriQLite.DialOutputChange(Sender: TObject);
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   Parameter[numParams - 1] := DialOutput.Value;
  end;
end;

end.
