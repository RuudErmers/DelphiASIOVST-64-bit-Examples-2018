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
  Forms, Controls, Graphics, StdCtrls, ExtCtrls, Menus, DAV_Types, 
  DAV_VSTModule, DAV_GuiButton, DAV_GuiVUMeter, DAV_GuiLED, DAV_GuiLabel, 
  DAV_GuiPanel, DAV_GuiPixelMap, DAV_GuiPng, DAV_GuiStitchedControls, 
  DAV_GuiStitchedPngList, DAV_GuiStitchedDisplay, DAV_GuiStitchedDial,
  DAV_GuiImageControl, DAV_GuiCustomControl, DAV_GuiGraphicControl;

type
  TLevelState = (lsIn, lsGR, lsOut);
  TFmLA4029 = class(TForm)
    BtGR: TGuiButton;
    BtIn: TGuiButton;
    BtOut: TGuiButton;
    DialAttack: TGuiStitchedDial;
    DialInput: TGuiStitchedDial;
    DialKnee: TGuiStitchedDial;
    DialMix: TGuiStitchedDial;
    DialOutput: TGuiStitchedDial;
    DialRatio: TGuiStitchedDial;
    DialRelease: TGuiStitchedDial;
    GSPL: TGuiStitchedPNGList;
    LbAttack: TGuiLabel;
    LbFast: TGuiLabel;
    LbInput: TGuiLabel;
    LbInputValue: TLabel;
    LbKnee: TGuiLabel;
    LbKneeValue: TLabel;
    LbLevelingAmplifier: TLabel;
    LbMix: TGuiLabel;
    LbMixValue: TLabel;
    LbOnOff: TGuiLabel;
    LbOutput: TGuiLabel;
    LbOutputValue: TLabel;
    LbRatioValue: TLabel;
    LbRatioX: TGuiLabel;
    LbRelease: TGuiLabel;
    LbSlow: TGuiLabel;
    LbTitle: TGuiLabel;
    LbVUMeterDisplay: TLabel;
    LEDOnOff: TGuiLED;
    MIFast: TMenuItem;
    MIMedium: TMenuItem;
    MISlow: TMenuItem;
    PnA: TGuiPanel;
    PnB: TGuiPanel;
    PnInputValue: TGuiPanel;
    PnKnee: TGuiPanel;
    PnMix: TGuiPanel;
    PnOutputValue: TGuiPanel;
    PnRatio: TGuiPanel;
    PopupVUMeterSpeed: TPopupMenu;
    SpDivide1: TShape;
    SpDivide2: TShape;
    Timer: TTimer;
    VUMeter: TGuiStitchedDisplay;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtGRClick(Sender: TObject);
    procedure BtInClick(Sender: TObject);
    procedure BtOutClick(Sender: TObject);
    procedure DialAttackChange(Sender: TObject);
    procedure DialInputChange(Sender: TObject);
    procedure DialKneeChange(Sender: TObject);
    procedure DialMixChange(Sender: TObject);
    procedure DialOutputChange(Sender: TObject);
    procedure DialRatioChange(Sender: TObject);
    procedure DialReleaseChange(Sender: TObject);
    procedure LEDOnOffClick(Sender: TObject);
    procedure MIFastClick(Sender: TObject);
    procedure MIMediumClick(Sender: TObject);
    procedure MISlowClick(Sender: TObject);
    procedure PopupVUMeterSpeedPopup(Sender: TObject);
    procedure VUMeterTimerTimer(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FBackground : TGuiCustomPixelMap;
    procedure SetLevelState(const Value: TLevelState);
    function GetLevelState: TLevelState;
    function VUMeterValueToPos(Value: Double): Integer;
  public
    procedure UpdateOnOff;
    procedure UpdateInput;
    procedure UpdateOutput;
    procedure UpdateAttack;
    procedure UpdateRelease;
    procedure UpdateRatio;
    procedure UpdateKnee;
    procedure UpdateMix;
    procedure UpdateLevelState;
  published
    property LevelState: TLevelState read GetLevelState write SetLevelState;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, DAV_GuiCommon, LA4029DM;

procedure TFmLA4029.FormCreate(Sender: TObject);
begin
 FBackground := TGuiPixelMapMemory.Create;
end;

procedure TFmLA4029.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackground);
end;

procedure TFmLA4029.FormPaint(Sender: TObject);
begin
 if Assigned(FBackground)
  then FBackground.PaintTo(Canvas);
end;

procedure TFmLA4029.FormResize(Sender: TObject);
var
  ClrBt  : Byte;
  x, y   : Integer;
  Filter : array [0..1] of Single;
  Value  : ShortInt;
  ScnLn  : PPixel32Array;
begin
 ClrBt := $F + Random($40);
 with FBackground do
  begin
   SetSize(ClientWidth, ClientHeight);
   Filter[0] := 0;
   Filter[1] := 0;
   for y := 0 to Height - 1 do
    begin
     ScnLn := Scanline[y];
     for x := 0 to Width - 1 do
      begin
       Filter[1] := 0.9 * Filter[0] + 0.1 * (2 * Random - 1);
       Value := Round($F * Filter[1]);
       Filter[0] := Filter[1];
       ScnLn[x].B := ClrBt + Value;
       ScnLn[x].G := ClrBt + Value;
       ScnLn[x].R := ClrBt + Value;
       ScnLn[x].A := 0;
      end;
    end;
  end;
end;

procedure TFmLA4029.FormShow(Sender: TObject);
begin
 UpdateOnOff;
 UpdateInput;
 UpdateOutput;
 UpdateAttack;
 UpdateRelease;
 UpdateRatio;
 UpdateKnee;
 UpdateMix;
 UpdateLevelState;
end;

function TFmLA4029.GetLevelState: TLevelState;
begin
 with TLA4029DataModule(Owner)
  do result := TLevelState(Round(Parameter[8]));
end;

procedure TFmLA4029.LEDOnOffClick(Sender: TObject);
begin
 with TLA4029DataModule(Owner) do
  begin
   Parameter[0] := 1 - Parameter[0];
   UpdateOnOff;
  end;
end;

procedure TFmLA4029.MIFastClick(Sender: TObject);
begin
 with TLA4029DataModule(Owner)
  do Parameter[9] := 5;
end;

procedure TFmLA4029.MIMediumClick(Sender: TObject);
begin
 with TLA4029DataModule(Owner)
  do Parameter[9] := 50;
end;

procedure TFmLA4029.MISlowClick(Sender: TObject);
begin
 with TLA4029DataModule(Owner)
  do Parameter[9] := 500;
end;

procedure TFmLA4029.PopupVUMeterSpeedPopup(Sender: TObject);
begin
 with TLA4029DataModule(Owner) do
  begin
   MIFast.Checked   := Parameter[9] < 20;
   MISlow.Checked   := Parameter[9] > 200;
   MIMedium.Checked := not (MIFast.Checked or MISlow.Checked);
  end;
end;

procedure TFmLA4029.SetLevelState(const Value: TLevelState);
begin
 with TLA4029DataModule(Owner) do
  if LevelState <> Value then
   begin
    Parameter[8] := Integer(Value);
    UpdateLevelState;
   end;
end;

procedure TFmLA4029.UpdateOnOff;
begin
 with TLA4029DataModule(Owner) do
  begin
   if Parameter[0] < 0.5
    then LEDOnOff.Brightness_Percent := 100
    else LEDOnOff.Brightness_Percent := 10;
  end;
end;

procedure TFmLA4029.UpdateInput;
begin
 with TLA4029DataModule(Owner) do
  begin
   if DialInput.Value <> Parameter[1]
    then DialInput.Value := Parameter[1];
   LbInputValue.Caption := FloatToStrF(DialInput.Value, ffFixed, 3, 1) + ' dB';
  end;
end;

procedure TFmLA4029.UpdateKnee;
begin
 with TLA4029DataModule(Owner) do
  begin
   if DialKnee.Value <> Parameter[6]
    then DialKnee.Value := Parameter[6];
   LbKneeValue.Caption := FloatToStrF(DialKnee.Value, ffFixed, 3, 1);
  end;
end;

procedure TFmLA4029.UpdateMix;
begin
 with TLA4029DataModule(Owner) do
  begin
   if DialMix.Value <> Parameter[7]
    then DialMix.Value := Parameter[7];
   LbMixValue.Caption := FloatToStrF(DialMix.Value, ffFixed, 3, 1) + '%';
  end;
end;

procedure TFmLA4029.UpdateLevelState;
begin
 case LevelState of
   lsIn : begin
           BtIn.ButtonColor  := $00202020;
           BtIn.Font.Color   := $00E2E2E2;
           BtIn.BorderColor  := clSilver;
           BtGR.ButtonColor  := $00000000;
           BtGR.Font.Color   := clGray;
           BtGR.BorderColor  := $00333333;
           BtOut.ButtonColor := $00000000;
           BtOut.Font.Color  := clGray;
           BtOut.BorderColor := $00333333;
           LbVUMeterDisplay.Caption := 'Input';
          end;
   lsGR : begin
           BtIn.ButtonColor  := $00000000;
           BtIn.Font.Color   := clGray;
           BtIn.BorderColor  := $00333333;
           BtGR.ButtonColor  := $00202020;
           BtGR.Font.Color   := $00E2E2E2;
           BtGR.BorderColor  := clSilver;
           BtOut.ButtonColor := $00000000;
           BtOut.Font.Color  := clGray;
           BtOut.BorderColor := $00333333;
           LbVUMeterDisplay.Caption := 'Gain Reduction';
          end;
  lsOut : begin
           BtIn.ButtonColor  := $00000000;
           BtIn.Font.Color   := clGray;
           BtIn.BorderColor  := $00333333;
           BtGR.ButtonColor  := $00000000;
           BtGR.Font.Color   := clGray;
           BtGR.BorderColor  := $00333333;
           BtOut.ButtonColor := $00202020;
           BtOut.Font.Color  := $00E2E2E2;
           BtOut.BorderColor := clSilver;
           LbVUMeterDisplay.Caption := 'Output';
          end;
 end;
end;

procedure TFmLA4029.UpdateOutput;
begin
 with TLA4029DataModule(Owner) do
  begin
   if DialOutput.Value <> Parameter[2]
    then DialOutput.Value := Parameter[2];
   LbOutputValue.Caption := FloatToStrF(DialOutput.Value, ffFixed, 3, 1) + ' dB';
  end;
end;

procedure TFmLA4029.UpdateAttack;
var
  s : Single;
begin
 with TLA4029DataModule(Owner) do
  begin
   s := Log10(Parameter[3]);
   if DialAttack.Value <> s
    then DialAttack.Value := s;
//   LbAttackValue.Caption := FloatToStrF(Parameter[3], ffGeneral, 4, 2) + ' ms';
  end;
end;

procedure TFmLA4029.UpdateRelease;
var
  s : Single;
begin
 with TLA4029DataModule(Owner) do
  begin
   s := Log10(Parameter[4]);
   if DialRelease.Value <> s
    then DialRelease.Value := s;
//   LbReleaseValue.Caption := FloatToStrF(Parameter[4], ffGeneral, 4, 5) + ' ms';
  end;
end;

function TFmLA4029.VUMeterValueToPos(Value: Double): Integer;
begin
 // ToDo: Create a true mapping
 if Value < -40 then result := 0 else
 if Value >   3 then result := VUMeter.GlyphCount - 1 else
 if Value < -10 then result := Round(((40 + Value) / 30) * 22) else
 if Value <  -7 then result := Round(22 + (10 + Value) * 1.66) else
 if Value <  -5 then result := Round(28 + (7 + Value) * 2.5) else
 if Value <  -3 then result := Round(33 + (5 + Value) * 2.5) else
 if Value <  -1 then result := Round(38 + (3 + Value) * 2.5) else
 if Value <   0 then result := Round(43 + (1 + Value) * 5)
  else result := Round(48 + Value / 3 * (VUMeter.GlyphCount - 49));
end;

procedure TFmLA4029.VUMeterTimerTimer(Sender: TObject);
begin
 with TLA4029DataModule(Owner) do
  case LevelState of
    lsIn : VUMeter.GlyphIndex := VUMeterValueToPos(InLevel_dB);
    lsGR : VUMeter.GlyphIndex := VUMeterValueToPos(GRReduction_dB);
   lsOut : VUMeter.GlyphIndex := VUMeterValueToPos(OutLevel_dB);
  end;
end;

procedure TFmLA4029.UpdateRatio;
var
  s : Single;
begin
 with TLA4029DataModule(Owner) do
  begin
   s := Log10(Parameter[5]);
   if DialRatio.Value <> s
    then DialRatio.Value := s;
   LbRatioValue.Caption := '1 : ' + FloatToStrF(Parameter[5], ffFixed, 3, 1);
  end;
end;

procedure TFmLA4029.DialInputChange(Sender: TObject);
begin
 with TLA4029DataModule(Owner) do
  begin
   if Parameter[1] <> DialInput.Value
    then Parameter[1] := DialInput.Value;
   UpdateInput;
  end;
end;

procedure TFmLA4029.DialOutputChange(Sender: TObject);
begin
 with TLA4029DataModule(Owner) do
  begin
   if Parameter[2] <> DialOutput.Value
    then Parameter[2] := DialOutput.Value;
   UpdateOutput;
  end;
end;

procedure TFmLA4029.BtGRClick(Sender: TObject);
begin
 LevelState := lsGR;
end;

procedure TFmLA4029.BtInClick(Sender: TObject);
begin
 LevelState := lsIn;
end;

procedure TFmLA4029.BtOutClick(Sender: TObject);
begin
 LevelState := lsOut;
end;

procedure TFmLA4029.DialAttackChange(Sender: TObject);
var
  s : Single;
begin
 with TLA4029DataModule(Owner) do
  begin
   s := Power(10, DialAttack.Value);
   Parameter[3] := s;
  end;
end;

procedure TFmLA4029.DialReleaseChange(Sender: TObject);
var
  s : Single;
begin
 with TLA4029DataModule(Owner) do
  begin
   s := Power(10, DialRelease.Value);
   Parameter[4] := s;
  end;
end;

procedure TFmLA4029.DialRatioChange(Sender: TObject);
var
  s : Single;
begin
 with TLA4029DataModule(Owner) do
  begin
   s := Power(10, DialRatio.Value);
   if abs (Parameter[5] - s) > 1E-3
    then Parameter[5] := s;
   UpdateRatio;
  end;
end;

procedure TFmLA4029.DialKneeChange(Sender: TObject);
begin
 with TLA4029DataModule(Owner) do
  begin
   Parameter[6] := DialKnee.Value;
   UpdateKnee;
  end;
end;

procedure TFmLA4029.DialMixChange(Sender: TObject);
begin
 with TLA4029DataModule(Owner) do
  begin
   Parameter[7] := DialMix.Value;
   UpdateMix;
  end;
end;

end.
