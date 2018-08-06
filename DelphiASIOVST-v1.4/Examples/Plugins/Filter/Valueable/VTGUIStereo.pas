unit VTGUIStereo;

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
  Forms, Controls, ExtCtrls, StdCtrls, DAV_Types, DAV_VSTModule, DAV_GuiPanel, 
  DAV_GuiBaseControl, DAV_GuiLabel, DAV_GuiStitchedControls, 
  DAV_GuiStitchedDial, DAV_GuiStitchedPngList, DAV_GuiStitchedSwitch, 
  DAV_GuiCustomControl, DAV_GuiGraphicControl, DAV_GuiImageControl;

type
  TFmVT = class(TForm)
    DialHiGainLeft: TGuiStitchedDial;
    DialHiGainRight: TGuiStitchedDial;
    DialLowGainLeft: TGuiStitchedDial;
    DialLowGainRight: TGuiStitchedDial;
    DialOutputGain: TGuiStitchedDial;
    DialSelector: TGuiStitchedDial;
    GSPL: TGuiStitchedPNGList;
    GuiPanel: TGuiPanel;
    LbBassFlatLeft: TGuiLabel;
    LbBassFlatRight: TGuiLabel;
    LbBassLeft: TGuiLabel;
    LbBassLeftMax: TGuiLabel;
    LbBassRight: TGuiLabel;
    LbBassRightMax: TGuiLabel;
    LbChannel: TGuiLabel;
    LbDrive: TGuiLabel;
    LbGain: TGuiLabel;
    LbGainValue: TGuiLabel;
    LbHiGainLeftLower: TGuiLabel;
    LbHiGainLeftUpper: TGuiLabel;
    LbHiGainRightLower: TGuiLabel;
    LbLowGainLeftLower: TGuiLabel;
    LbLowGainRightLower: TGuiLabel;
    LbMono: TGuiLabel;
    LbRoasty1: TGuiLabel;
    LbRoasty2: TGuiLabel;
    LbSteamin1: TGuiLabel;
    LbSteamin2: TGuiLabel;
    LbStereo: TGuiLabel;
    LbSubTitle: TGuiLabel;
    LbTitle: TGuiLabel;
    LbTrebleFlatLeft: TGuiLabel;
    LbTrebleFlatRight: TGuiLabel;
    LbTrebleLeft: TGuiLabel;
    LbTrebleRight: TGuiLabel;
    LbTrebleRightMax: TGuiLabel;
    SwitchChannel: TGuiStitchedSwitch;
    SwitchLowBypassLeft: TGuiStitchedSwitch;
    SwitchHiBypassRight: TGuiStitchedSwitch;
    SwitchHiBypassLeft: TGuiStitchedSwitch;
    SwitchLowBypassRight: TGuiStitchedSwitch;
    procedure FormShow(Sender: TObject);
    procedure SwitchChannelChange(Sender: TObject);
    procedure SwitchHiBypassLeftChange(Sender: TObject);
    procedure SwitchHiBypassRightChange(Sender: TObject);
    procedure DialHiGainLeftChange(Sender: TObject);
    procedure DialHiGainRightChange(Sender: TObject);
    procedure SwitchLowBypassRightChange(Sender: TObject);
    procedure DialLowGainLeftChange(Sender: TObject);
    procedure DialLowGainRightChange(Sender: TObject);
    procedure DialOutputGainChange(Sender: TObject);
    procedure DialSelectorChange(Sender: TObject);
    procedure LbRoasty1Click(Sender: TObject);
    procedure LbRoasty2Click(Sender: TObject);
    procedure LbSteamin1Click(Sender: TObject);
    procedure LbSteamin2Click(Sender: TObject);
    procedure SwitchLowBypassLeftChange(Sender: TObject);
  public
    procedure UpdateBassGainLeft;
    procedure UpdateTrebleGainLeft;
    procedure UpdateBassBypassLeft;
    procedure UpdateTrebleBypassLeft;
    procedure UpdateBassGainRight;
    procedure UpdateTrebleGainRight;
    procedure UpdateBassBypassRight;
    procedure UpdateTrebleBypassRight;
    procedure UpdateGain;
    procedure UpdateChannel;
    procedure UpdateSelector;
  end;

implementation

uses
  Math, VTModuleStereo;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmVT.DialLowGainLeftChange(Sender: TObject);
begin
 with TVTVSTModule(Owner) do
  begin
   if Parameter[2] <> DialLowGainLeft.Value
    then Parameter[2] := DialLowGainLeft.Value;
   if ssAlt in KeyboardStateToShiftState
    then Parameter[6] := Parameter[2];
  end;
end;

procedure TFmVT.DialLowGainRightChange(Sender: TObject);
begin
 with TVTVSTModule(Owner) do
  begin
   if Parameter[6] <> DialLowGainRight.Value
    then Parameter[6] := DialLowGainRight.Value;
   if ssAlt in KeyboardStateToShiftState
    then Parameter[2] := Parameter[6];
  end;
end;

procedure TFmVT.DialHiGainLeftChange(Sender: TObject);
begin
 with TVTVSTModule(Owner) do
  begin
   if Parameter[0] <> DialHiGainLeft.Value
    then Parameter[0] := DialHiGainLeft.Value;
   if ssAlt in KeyboardStateToShiftState
    then Parameter[4] := Parameter[0];
  end;
end;

procedure TFmVT.DialHiGainRightChange(Sender: TObject);
begin
 with TVTVSTModule(Owner) do
  begin
   if Parameter[4] <> DialHiGainRight.Value
    then Parameter[4] := DialHiGainRight.Value;
   if ssAlt in KeyboardStateToShiftState
    then Parameter[0] := Parameter[4];
  end;
end;

procedure TFmVT.SwitchLowBypassLeftChange(Sender: TObject);
begin
 with TVTVSTModule(Owner) do
  begin
   if Parameter[3] <> 1 - SwitchLowBypassLeft.GlyphIndex
    then Parameter[3] := 1 - SwitchLowBypassLeft.GlyphIndex;
  end;
end;

procedure TFmVT.SwitchLowBypassRightChange(Sender: TObject);
begin
 with TVTVSTModule(Owner) do
  begin
   if Parameter[7] <> 1 - SwitchLowBypassRight.GlyphIndex
    then Parameter[7] := 1 - SwitchLowBypassRight.GlyphIndex;
  end;
end;

procedure TFmVT.SwitchHiBypassLeftChange(Sender: TObject);
begin
 with TVTVSTModule(Owner) do
  begin
   if Parameter[1] <> 1 - SwitchHiBypassLeft.GlyphIndex
    then Parameter[1] := 1 - SwitchHiBypassLeft.GlyphIndex;
  end;
end;

procedure TFmVT.SwitchHiBypassRightChange(Sender: TObject);
begin
 with TVTVSTModule(Owner) do
  begin
   if Parameter[5] <> 1 - SwitchHiBypassRight.GlyphIndex
    then Parameter[5] := 1 - SwitchHiBypassRight.GlyphIndex;
  end;
end;

procedure TFmVT.DialSelectorChange(Sender: TObject);
begin
 with TVTVSTModule(Owner) do
  begin
   if Parameter[8] <> DialSelector.Value
    then Parameter[8] := DialSelector.Value;
  end;
end;

procedure TFmVT.SwitchChannelChange(Sender: TObject);
begin
 with TVTVSTModule(Owner) do
  begin
   if Parameter[9] <> 3 - SwitchChannel.GlyphIndex
    then Parameter[9] := 3 - SwitchChannel.GlyphIndex;
  end;
end;

procedure TFmVT.DialOutputGainChange(Sender: TObject);
begin
 with TVTVSTModule(Owner) do
  begin
   if Parameter[10] <> DialOutputGain.Value
    then Parameter[10] := DialOutputGain.Value;
  end;
end;

procedure TFmVT.FormShow(Sender: TObject);
begin
 UpdateBassGainLeft;
 UpdateTrebleGainLeft;
 UpdateBassBypassLeft;
 UpdateTrebleBypassLeft;
 UpdateBassGainRight;
 UpdateTrebleGainRight;
 UpdateBassBypassRight;
 UpdateTrebleBypassRight;
 UpdateGain;
 UpdateChannel;
 UpdateSelector;
end;

procedure TFmVT.LbRoasty1Click(Sender: TObject);
begin
 TVTVSTModule(Owner).Parameter[8] := 2;
end;

procedure TFmVT.LbRoasty2Click(Sender: TObject);
begin
 TVTVSTModule(Owner).Parameter[8] := 1;
end;

procedure TFmVT.LbSteamin1Click(Sender: TObject);
begin
 TVTVSTModule(Owner).Parameter[8] := 3;
end;

procedure TFmVT.LbSteamin2Click(Sender: TObject);
begin
 TVTVSTModule(Owner).Parameter[8] := 4;
end;

procedure TFmVT.UpdateBassBypassLeft;
var
  TempPosition : Integer;
begin
 with TVTVSTModule(Owner) do
  begin
   TempPosition := Round(1 - Parameter[3]);
   if SwitchLowBypassLeft.GlyphIndex <> TempPosition
    then SwitchLowBypassLeft.GlyphIndex := TempPosition;
  end;
end;

procedure TFmVT.UpdateBassBypassRight;
var
  TempPosition : Integer;
begin
 with TVTVSTModule(Owner) do
  begin
   TempPosition := Round(1 - Parameter[7]);
   if SwitchLowBypassRight.GlyphIndex <> TempPosition
    then SwitchLowBypassRight.GlyphIndex := TempPosition;
  end;
end;

procedure TFmVT.UpdateBassGainLeft;
var
  TempPosition : Single;
begin
 with TVTVSTModule(Owner) do
  begin
   TempPosition := Parameter[2];
   if DialLowGainLeft.Value <> TempPosition
    then DialLowGainLeft.Value := TempPosition;
  end;
end;

procedure TFmVT.UpdateBassGainRight;
var
  TempPosition : Single;
begin
 with TVTVSTModule(Owner) do
  begin
   TempPosition := Parameter[6];
   if DialLowGainRight.Value <> TempPosition
    then DialLowGainRight.Value := TempPosition;
  end;
end;

procedure TFmVT.UpdateChannel;
var
  TempPosition: Integer;
begin
 with TVTVSTModule(Owner) do
  begin
   TempPosition := Round(3 - Parameter[9]);
   if SwitchChannel.GlyphIndex <> TempPosition
    then SwitchChannel.GlyphIndex := TempPosition;
  end;
end;

procedure TFmVT.UpdateGain;
var
  TempGain : Single;
begin
 with TVTVSTModule(Owner) do
  begin
   TempGain := Parameter[10];
   if DialOutputGain.Value <> TempGain
    then DialOutputGain.Value := TempGain;
   LbGainValue.Caption := FloatToStrF(RoundTo(TempGain, -1), ffGeneral, 0, 1) + ' dB';
  end;
end;

procedure TFmVT.UpdateSelector;
var
  TempPosition : Single;
begin
 with TVTVSTModule(Owner) do
  begin
   TempPosition := Parameter[8];
   if DialSelector.Value <> TempPosition
    then DialSelector.Value := TempPosition
  end;
end;

procedure TFmVT.UpdateTrebleBypassLeft;
var
  TempPosition : Integer;
begin
 with TVTVSTModule(Owner) do
  begin
   TempPosition := Round(1 - Parameter[1]);
   if SwitchHiBypassLeft.GlyphIndex <> TempPosition
    then SwitchHiBypassLeft.GlyphIndex := TempPosition
  end;
end;

procedure TFmVT.UpdateTrebleBypassRight;
var
  TempPosition : Integer;
begin
 with TVTVSTModule(Owner) do
  begin
   TempPosition := Round(1 - Parameter[5]);
   if SwitchHiBypassRight.GlyphIndex <> TempPosition
    then SwitchHiBypassRight.GlyphIndex := TempPosition
  end;
end;

procedure TFmVT.UpdateTrebleGainLeft;
var
  TempPosition : Single;
begin
 with TVTVSTModule(Owner) do
  begin
   TempPosition := Parameter[0];
   if DialHiGainLeft.Value <> TempPosition
    then DialHiGainLeft.Value := TempPosition
  end;
end;

procedure TFmVT.UpdateTrebleGainRight;
var
  TempPosition : Single;
begin
 with TVTVSTModule(Owner) do
  begin
   TempPosition := Parameter[4];
   if DialHiGainRight.Value <> TempPosition
    then DialHiGainRight.Value := TempPosition
  end;
end;

end.
