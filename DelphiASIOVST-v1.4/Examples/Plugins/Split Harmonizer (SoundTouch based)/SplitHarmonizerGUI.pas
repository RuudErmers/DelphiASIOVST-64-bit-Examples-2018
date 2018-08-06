unit SplitHarmonizerGUI;

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
  Forms, Controls, Graphics, ExtCtrls, DAV_Types, DAV_VSTModule, DAV_GuiCommon, 
  DAV_GuiLabel, DAV_GuiPanel, DAV_GuiPixelMap, DAV_GuiStitchedControls, 
  DAV_GuiStitchedDial, DAV_GuiStitchedSwitch, DAV_GuiStitchedPngList,
  DAV_GuiCustomControl, DAV_GuiBaseControl, DAV_GuiDial, DAV_GuiGraphicControl,
  DAV_GuiImageControl;

type
  TFmSplitHarmonizer = class(TForm)
    LbDelayA: TGuiLabel;
    LbDelayAValue: TGuiLabel;
    LbDelayB: TGuiLabel;
    LbDelayBValue: TGuiLabel;
    LbDetuneA: TGuiLabel;
    LbDetuneAValue: TGuiLabel;
    LbDetuneB: TGuiLabel;
    LbDetuneBValue: TGuiLabel;
    LbEncoding: TGuiLabel;
    LbLowpassA: TGuiLabel;
    LbLowpassAValue: TGuiLabel;
    LbLowpassB: TGuiLabel;
    LbLowpassBValue: TGuiLabel;
    LbMixA: TGuiLabel;
    LbMixAValue: TGuiLabel;
    LbMixB: TGuiLabel;
    LbMixBValue: TGuiLabel;
    PnStageA: TGuiPanel;
    PnStageB: TGuiPanel;
    SwEncoding: TGuiSwitch;
    GSPL: TGuiStitchedPNGList;
    DialDetuneA: TGuiStitchedDial;
    DialDelayA: TGuiStitchedDial;
    DialMixA: TGuiStitchedDial;
    DialLowpassA: TGuiStitchedDial;
    DialDetuneB: TGuiStitchedDial;
    DialDelayB: TGuiStitchedDial;
    DialMixB: TGuiStitchedDial;
    DialLowpassB: TGuiStitchedDial;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DialDelayAChange(Sender: TObject);
    procedure DialDelayBChange(Sender: TObject);
    procedure DialDetuneAChange(Sender: TObject);
    procedure DialDetuneBChange(Sender: TObject);
    procedure DialMixAChange(Sender: TObject);
    procedure DialMixBChange(Sender: TObject);
    procedure SwEncodingChange(Sender: TObject);
    procedure DialLowpassAChange(Sender: TObject);
    procedure DialLowpassBChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FBackground : TGuiCustomPixelMap;
  public
    procedure UpdateEncoding;
    procedure UpdateSemitones(const Channel: Integer);
    procedure UpdateDelay(const Channel: Integer);
    procedure UpdateMix(const Channel: Integer);
    procedure UpdateLowpassFilter(const Channel: Integer);
  end;

implementation

uses
  Math, SplitHarmonizerDM, DAV_VSTModuleWithPrograms;

resourcestring
  RCStrChannelInvalid = 'Channel invalid (%d)';

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmSplitHarmonizer.FormCreate(Sender: TObject);
begin
 FBackground := TGuiPixelMapMemory.Create;
end;

procedure TFmSplitHarmonizer.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackground);
end;

procedure TFmSplitHarmonizer.FormPaint(Sender: TObject);
begin
 if Assigned(FBackground)
  then FBackground.PaintTo(Canvas);
end;

procedure TFmSplitHarmonizer.FormResize(Sender: TObject);
var
  X, Y   : Integer;
  Filter : array [0..1] of Single;
  ScnLn  : PPixel32Array;
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
       Filter[1] := 0.97 * Filter[0] + 0.03 * (2 * Random - 1);
       Filter[0] := Filter[1];
       ScnLn[X].B := Round($0F + $0E * Filter[1]);;
       ScnLn[X].G := Round($12 + $0E * Filter[1]);;
       ScnLn[X].R := Round($13 + $0E * Filter[1]);;
      end;
    end;
  end;
end;

procedure TFmSplitHarmonizer.FormShow(Sender: TObject);
begin
 UpdateEncoding;
 UpdateSemitones(0);
 UpdateSemitones(1);
 UpdateDelay(0);
 UpdateDelay(1);
 UpdateMix(0);
 UpdateMix(1);
 UpdateLowpassFilter(0);
 UpdateLowpassFilter(1);
end;

procedure TFmSplitHarmonizer.SwEncodingChange(Sender: TObject);
begin
 with TSplitHarmonizerModule(Owner) do
  begin
   if Round(Parameter[0]) <> SwEncoding.GlyphNr
    then Parameter[0] := SwEncoding.GlyphNr;
  end;
end;

procedure TFmSplitHarmonizer.DialDetuneAChange(Sender: TObject);
begin
 with TSplitHarmonizerModule(Owner) do
  begin
   if Parameter[1] <> DialDetuneA.Value then
    begin
     Parameter[1] := DialDetuneA.Value;
     if ssAlt in KeyboardStateToShiftState
      then Parameter[5] := -Parameter[1];
    end;
  end;
end;

procedure TFmSplitHarmonizer.DialDelayAChange(Sender: TObject);
begin
 with TSplitHarmonizerModule(Owner) do
  begin
   if Parameter[2] <> DialDelayA.Value then
    begin
     Parameter[2] := DialDelayA.Value;
     if ssAlt in KeyboardStateToShiftState
      then Parameter[6] := Parameter[2];
    end;
  end;
end;

procedure TFmSplitHarmonizer.DialLowpassAChange(Sender: TObject);
begin
 with TSplitHarmonizerModule(Owner) do
  begin
   if Parameter[3] <> DialLowpassA.Value then
    begin
     Parameter[3] := DialLowpassA.Value;
     if ssAlt in KeyboardStateToShiftState
      then Parameter[7] := Parameter[3];
    end;
  end;
end;

procedure TFmSplitHarmonizer.DialMixAChange(Sender: TObject);
begin
 with TSplitHarmonizerModule(Owner) do
  begin
   if Parameter[4] <> DialMixA.Value then
    begin
     Parameter[4] := DialMixA.Value;
     if ssAlt in KeyboardStateToShiftState
      then Parameter[8] := Parameter[4];
    end;
  end;
end;

procedure TFmSplitHarmonizer.DialDetuneBChange(Sender: TObject);
begin
 with TSplitHarmonizerModule(Owner) do
  begin
   if Parameter[5] <> DialDetuneB.Value then
    begin
     Parameter[5] := DialDetuneB.Value;
     if ssAlt in KeyboardStateToShiftState
      then Parameter[1] := -Parameter[5];
    end;
  end;
end;

procedure TFmSplitHarmonizer.DialDelayBChange(Sender: TObject);
begin
 with TSplitHarmonizerModule(Owner) do
  begin
   if Parameter[6] <> DialDelayB.Value then
    begin
     Parameter[6] := DialDelayB.Value;
     if ssAlt in KeyboardStateToShiftState
      then Parameter[2] := Parameter[6];
    end;
  end;
end;

procedure TFmSplitHarmonizer.DialLowpassBChange(Sender: TObject);
begin
 with TSplitHarmonizerModule(Owner) do
  begin
   if Parameter[7] <> DialLowpassB.Value then
    begin
     Parameter[7] := DialLowpassB.Value;
     if ssAlt in KeyboardStateToShiftState
      then Parameter[3] := Parameter[7];
    end;
  end;
end;

procedure TFmSplitHarmonizer.DialMixBChange(Sender: TObject);
begin
 with TSplitHarmonizerModule(Owner) do
  begin
   if Parameter[8] <> DialMixB.Value then
    begin
     Parameter[8] := DialMixB.Value;
     if ssAlt in KeyboardStateToShiftState
      then Parameter[4] := Parameter[8];
    end;
  end;
end;

procedure TFmSplitHarmonizer.UpdateDelay(const Channel: Integer);
var
  Dial : TGuiStitchedDial;
  Labl : TGuiLabel;
begin
 with TSplitHarmonizerModule(Owner) do
  begin
   case Channel of
    0 : begin Dial := DialDelayA; Labl := LbDelayAValue; end;
    1 : begin Dial := DialDelayB; Labl := LbDelayBValue; end;
    else raise Exception.CreateFmt(RCStrChannelInvalid, [Channel]);
   end;
   if Dial.Value <> Parameter[2 + 4 * Channel]
    then Dial.Value := Parameter[2 + 4 * Channel];
   Labl.Caption := FloatToStrF(RoundTo(Parameter[2 + 4 * Channel], -1), ffGeneral, 4, 4) + ' ms';
  end;
end;

procedure TFmSplitHarmonizer.UpdateEncoding;
begin
 with TSplitHarmonizerModule(Owner)
  do SwEncoding.GlyphNr := Round(Parameter[0]);
end;

procedure TFmSplitHarmonizer.UpdateSemitones(const Channel: Integer);
var
  Dial : TGuiStitchedDial;
  Labl : TGuiLabel;
begin
 with TSplitHarmonizerModule(Owner) do
  begin
   case Channel of
    0 : begin Dial := DialDetuneA; Labl := LbDetuneAValue; end;
    1 : begin Dial := DialDetuneB; Labl := LbDetuneBValue; end;
    else raise Exception.CreateFmt(RCStrChannelInvalid, [Channel]);
   end;
   if Dial.Value <> Parameter[1 + 4 * Channel]
    then Dial.Value := Parameter[1 + 4 * Channel];
   Labl.Caption := IntToStr(Round(Parameter[1 + 4 * Channel])) + ' Cent';
  end;
end;

procedure TFmSplitHarmonizer.UpdateLowpassFilter(const Channel: Integer);
var
  Dial : TGuiStitchedDial;
  Labl : TGuiLabel;
begin
 with TSplitHarmonizerModule(Owner) do
  begin
   case Channel of
    0 : begin Dial := DialLowpassA; Labl := LbLowpassAValue; end;
    1 : begin Dial := DialLowpassB; Labl := LbLowpassBValue; end;
    else raise Exception.CreateFmt(RCStrChannelInvalid, [Channel]);
   end;
   if Dial.Value <> Parameter[3 + 4 * Channel]
    then Dial.Value := Parameter[3 + 4 * Channel];
   if Parameter[3 + 4 * Channel] < 1000
    then Labl.Caption := FloatToStrF(Parameter[3 + 4 * Channel], ffGeneral, 4, 4) + ' Hz'
    else Labl.Caption := FloatToStrF(Parameter[3 + 4 * Channel] * 1E-3, ffGeneral, 3, 3) + ' kHz';
  end;
end;

procedure TFmSplitHarmonizer.UpdateMix(const Channel: Integer);
var
  Dial : TGuiStitchedDial;
  Labl : TGuiLabel;
begin
 with TSplitHarmonizerModule(Owner) do
  begin
   case Channel of
    0 : begin Dial := DialMixA; Labl := LbMixAValue; end;
    1 : begin Dial := DialMixB; Labl := LbMixBValue; end;
    else raise Exception.CreateFmt(RCStrChannelInvalid, [Channel]);
   end;
   if Dial.Value <> Parameter[4 + 4 * Channel]
    then Dial.Value := Parameter[4 + 4 * Channel];
   Labl.Caption := FloatToStrF(RoundTo(50 + 0.5 * Parameter[4 + 4 * Channel], -1), ffGeneral, 4, 4) + ' %';
  end;
end;

end.
