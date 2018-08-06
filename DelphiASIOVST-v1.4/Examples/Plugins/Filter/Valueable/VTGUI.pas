unit VTGUI;

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
  DAV_GuiStitchedPngList, DAV_GuiStitchedDial, DAV_GuiStitchedSwitch, 
  DAV_GuiCustomControl, DAV_GuiGraphicControl, DAV_GuiImageControl;

type
  TFmVT = class(TForm)
    SwitchChannel: TGuiStitchedSwitch;
    DialHiGain: TGuiStitchedDial;
    DialLowGain: TGuiStitchedDial;
    DialOutputGain: TGuiStitchedDial;
    DialSelector: TGuiStitchedDial;
    GSPL: TGuiStitchedPNGList;
    LbTrebleMin: TGuiLabel;
    LbBassMin: TGuiLabel;
    LbTrebleMax: TGuiLabel;
    LbBassMax: TGuiLabel;
    GuiPanel: TGuiPanel;
    LbBass: TGuiLabel;
    LbBassFLat: TGuiLabel;
    LbChannel: TGuiLabel;
    LbDrive: TGuiLabel;
    LbGain: TGuiLabel;
    LbGainValue: TGuiLabel;
    LbMono: TGuiLabel;
    LbRoasty1: TGuiLabel;
    LbRoasty2: TGuiLabel;
    LbSteamin1: TGuiLabel;
    LbSteamin2: TGuiLabel;
    LbStereo: TGuiLabel;
    LbSubTitle: TGuiLabel;
    LbTitle: TGuiLabel;
    LbTreble: TGuiLabel;
    LbTrebleFLat: TGuiLabel;
    SwitchLowBypass: TGuiStitchedSwitch;
    SwitchHiBypass: TGuiStitchedSwitch;
    procedure FormShow(Sender: TObject);
    procedure DialLowGainChange(Sender: TObject);
    procedure DialHiGainChange(Sender: TObject);
    procedure SwitchLowBypassChange(Sender: TObject);
    procedure SwitchHiBypassChange(Sender: TObject);
    procedure DialSelectorChange(Sender: TObject);
    procedure LbRoasty2Click(Sender: TObject);
    procedure LbRoasty1Click(Sender: TObject);
    procedure LbSteamin1Click(Sender: TObject);
    procedure LbSteamin2Click(Sender: TObject);
    procedure DialOutputGainChange(Sender: TObject);
    procedure SwitchChannelChange(Sender: TObject);
  public
    procedure UpdateBassGain;
    procedure UpdateTrebleGain;
    procedure UpdateBassBypass;
    procedure UpdateTrebleBypass;
    procedure UpdateGain;
    procedure UpdateChannel;
    procedure UpdateSelector;
  end;

implementation

uses
  Math, VTModule;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmVT.DialLowGainChange(Sender: TObject);
begin
 with TVTVSTModule(Owner) do
  begin
   if Parameter[0] <> DialLowGain.Value
    then Parameter[0] := DialLowGain.Value;
  end;
end;

procedure TFmVT.DialHiGainChange(Sender: TObject);
begin
 with TVTVSTModule(Owner) do
  begin
   if Parameter[1] <> DialHiGain.Value
    then Parameter[1] := DialHiGain.Value;
  end;
end;

procedure TFmVT.SwitchLowBypassChange(Sender: TObject);
begin
 with TVTVSTModule(Owner) do
  begin
   if Parameter[2] <> 1 - SwitchLowBypass.GlyphIndex
    then Parameter[2] := 1 - SwitchLowBypass.GlyphIndex;
  end;
end;

procedure TFmVT.SwitchHiBypassChange(Sender: TObject);
begin
 with TVTVSTModule(Owner) do
  begin
   if Parameter[3] <> 1 - SwitchHiBypass.GlyphIndex
    then Parameter[3] := 1 - SwitchHiBypass.GlyphIndex;
  end;
end;

procedure TFmVT.DialSelectorChange(Sender: TObject);
begin
 with TVTVSTModule(Owner) do
  begin
   if Parameter[4] <> DialSelector.Value
    then Parameter[4] := DialSelector.Value;
  end;
end;

procedure TFmVT.SwitchChannelChange(Sender: TObject);
begin
 with TVTVSTModule(Owner) do
  begin
   if Parameter[5] <> 3 - SwitchChannel.GlyphIndex
    then Parameter[5] := 3 - SwitchChannel.GlyphIndex;
  end;
end;

procedure TFmVT.DialOutputGainChange(Sender: TObject);
begin
 with TVTVSTModule(Owner) do
  begin
   if Parameter[6] <> DialOutputGain.Value
    then Parameter[6] := DialOutputGain.Value;
  end;
end;

procedure TFmVT.FormShow(Sender: TObject);
begin
 UpdateBassGain;
 UpdateTrebleGain;
 UpdateBassBypass;
 UpdateTrebleBypass;
 UpdateGain;
 UpdateChannel;
 UpdateSelector;
end;

procedure TFmVT.LbRoasty1Click(Sender: TObject);
begin
 TVTVSTModule(Owner).Parameter[4] := 2;
end;

procedure TFmVT.LbRoasty2Click(Sender: TObject);
begin
 TVTVSTModule(Owner).Parameter[4] := 1;
end;

procedure TFmVT.LbSteamin1Click(Sender: TObject);
begin
 TVTVSTModule(Owner).Parameter[4] := 3;
end;

procedure TFmVT.LbSteamin2Click(Sender: TObject);
begin
 TVTVSTModule(Owner).Parameter[4] := 4;
end;

procedure TFmVT.UpdateBassBypass;
var
  TempPosition : Integer;
begin
 with TVTVSTModule(Owner) do
  begin
   TempPosition := Round(1 - Parameter[2]);
   if SwitchLowBypass.GlyphIndex <> TempPosition
    then SwitchLowBypass.GlyphIndex := TempPosition
  end;
end;

procedure TFmVT.UpdateBassGain;
var
  TempPosition : Single;
begin
 with TVTVSTModule(Owner) do
  begin
   TempPosition := Parameter[0];
   if DialLowGain.Value <> TempPosition
    then DialLowGain.Value := TempPosition
  end;
end;

procedure TFmVT.UpdateChannel;
var
  TempPosition: Integer;
begin
 with TVTVSTModule(Owner) do
  begin
   TempPosition := Round(3 - Parameter[5]);
   if SwitchChannel.GlyphIndex <> TempPosition
    then SwitchChannel.GlyphIndex := TempPosition
  end;
end;

procedure TFmVT.UpdateGain;
var
  TempGain : Single;
begin
 with TVTVSTModule(Owner) do
  begin
   TempGain := Parameter[6];
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
   TempPosition := Parameter[4];
   if DialSelector.Value <> TempPosition
    then DialSelector.Value := TempPosition
  end;
end;

procedure TFmVT.UpdateTrebleBypass;
var
  TempPosition : Integer;
begin
 with TVTVSTModule(Owner) do
  begin
   TempPosition := Round(1 - Parameter[3]);
   if SwitchHiBypass.GlyphIndex <> TempPosition
    then SwitchHiBypass.GlyphIndex := TempPosition
  end;
end;

procedure TFmVT.UpdateTrebleGain;
var
  TempPosition : Single;
begin
 with TVTVSTModule(Owner) do
  begin
   TempPosition := Parameter[1];
   if DialHiGain.Value <> TempPosition
    then DialHiGain.Value := TempPosition
  end;
end;

end.
