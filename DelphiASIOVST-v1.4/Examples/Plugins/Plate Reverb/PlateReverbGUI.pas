unit PlateReverbGUI;

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
  Forms, Controls, StdCtrls, ExtCtrls, DAV_Types, DAV_VSTModule, DAV_GuiPng, 
  DAV_GuiBaseControl, DAV_GuiLabel, DAV_GuiPanel, DAV_GuiStitchedControls,
  DAV_GuiStitchedPngList, DAV_GuiStitchedDial, DAV_GuiCustomControl,
  DAV_GuiSelectBox, DAV_GuiButton, DAV_GuiGraphicControl, DAV_GuiImageControl;

type

  TFmPlateReverb = class(TForm)
    BtAB: TGuiButton;
    BtAbout: TGuiButton;
    CBFreeze: TCheckBox;
    LbPreset: TGuiLabel;
    PnLabel: TGuiPanel;
    PnToolbar: TPanel;
    SBPreset: TGuiSelectBox;
    LbDry: TGuiLabel;
    LbWet: TGuiLabel;
    LbPreDelay: TGuiLabel;
    LbInputDiffusion: TGuiLabel;
    LbDecayDiffusion: TGuiLabel;
    LbDampingFrequency: TGuiLabel;
    LbDecay: TGuiLabel;
    LbMod: TGuiLabel;
    DialDry: TGuiStitchedDial;
    DialWet: TGuiStitchedDial;
    DialPreDelay: TGuiStitchedDial;
    DialDampingFrequency: TGuiStitchedDial;
    DialInputDiffusion: TGuiStitchedDial;
    DialDecayDiffusion: TGuiStitchedDial;
    DialDecay: TGuiStitchedDial;
    DialModulation: TGuiStitchedDial;
    GSPL: TGuiStitchedPNGList;
    procedure FormShow(Sender: TObject);
    procedure BtAboutClick(Sender: TObject);
    procedure DialWetChange(Sender: TObject);
    procedure DialDryChange(Sender: TObject);
    procedure DialPreDelayChange(Sender: TObject);
    procedure DialInputDiffusionChange(Sender: TObject);
    procedure DialDecayDiffusionChange(Sender: TObject);
    procedure DialDampingFrequencyChange(Sender: TObject);
    procedure DialDecayChange(Sender: TObject);
    procedure SBPresetChange(Sender: TObject);
  public
    procedure UpdateDry;
    procedure UpdateWet;
    procedure UpdatePreDelay;
    procedure UpdateDecay;
    procedure UpdateDampingFrequency;
    procedure UpdateInputDiffusion;
    procedure UpdateDecayDiffusion;
    procedure UpdateModulation;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Dialogs, PlateReverbModule;

procedure TFmPlateReverb.FormShow(Sender: TObject);
var
  i : Integer;
begin
 with TPlateReverbVST(Owner) do
  begin
   SBPreset.Items.Clear;
   for i := 0 to numPrograms - 1
    do SBPreset.Items.Add(Programs[i].DisplayName);
   SBPreset.ItemIndex := CurrentProgram; 
  end;
end;

procedure TFmPlateReverb.SBPresetChange(Sender: TObject);
begin
 with TPlateReverbVST(Owner) do
  begin
   CurrentProgram := SBPreset.ItemIndex;
   UpdateDry;
   UpdateWet;
   UpdatePreDelay;
   UpdateDecay;
   UpdateDampingFrequency;
   UpdateInputDiffusion;
   UpdateDecayDiffusion;
   UpdateModulation;
  end;
end;

procedure TFmPlateReverb.UpdateDry;
begin
 with TPlateReverbVST(Owner) do
  if DialDry.Value <> Parameter[0]  then
   begin
    DialDry.Value := Parameter[0];
   end;
end;

procedure TFmPlateReverb.UpdateWet;
begin
 with TPlateReverbVST(Owner) do
  if DialWet.Value <> Parameter[1]  then
   begin
    DialWet.Value := Parameter[1];
   end;
end;

procedure TFmPlateReverb.UpdatePreDelay;
begin
 with TPlateReverbVST(Owner) do
  if DialPreDelay.Value <> Parameter[2]  then
   begin
    DialPreDelay.Value := Parameter[2];
   end;
end;

procedure TFmPlateReverb.UpdateDecay;
begin
 with TPlateReverbVST(Owner) do
  if DialDecay.Value <> Parameter[3]  then
   begin
    DialDecay.Value := Parameter[3];
   end;
end;

procedure TFmPlateReverb.UpdateDampingFrequency;
begin
 with TPlateReverbVST(Owner) do
  if DialDampingFrequency.Value <> Parameter[4] then
   begin
    DialDampingFrequency.Value := Parameter[4];
   end;
end;

procedure TFmPlateReverb.UpdateInputDiffusion;
begin
 with TPlateReverbVST(Owner) do
  if DialInputDiffusion.Value <> Parameter[5] then
   begin
    DialInputDiffusion.Value := Parameter[5];
   end;
end;

procedure TFmPlateReverb.UpdateDecayDiffusion;
begin
 with TPlateReverbVST(Owner) do
  if DialDecayDiffusion.Value <> Parameter[6] then
   begin
    DialDecayDiffusion.Value := Parameter[6];
   end;
end;

procedure TFmPlateReverb.UpdateModulation;
begin
 with TPlateReverbVST(Owner) do
  if DialModulation.Value <> Parameter[7] then
   begin
    DialModulation.Value := Parameter[7];
   end;
end;

procedure TFmPlateReverb.DialDryChange(Sender: TObject);
var
  Value : Single;
begin
 with TPlateReverbVST(Owner) do
  begin
   Value := DialDry.Value;
   if Parameter[0] <> Value
    then Parameter[0] := Value;
  end;
end;

procedure TFmPlateReverb.DialWetChange(Sender: TObject);
var
  Value : Single;
begin
 with TPlateReverbVST(Owner) do
  begin
   Value := DialWet.Value;
   if Parameter[1] <> Value
    then Parameter[1] := Value;
  end;
end;

procedure TFmPlateReverb.DialPreDelayChange(Sender: TObject);
begin
 with TPlateReverbVST(Owner) do
  if Parameter[2] <> DialPreDelay.Value then
   begin
    Parameter[2] := DialPreDelay.Value;
   end;
end;

procedure TFmPlateReverb.DialDecayChange(Sender: TObject);
begin
 with TPlateReverbVST(Owner) do
  if Parameter[3] <> DialDecay.Value then
   begin
    Parameter[3] := DialDecay.Value;
   end;
end;

procedure TFmPlateReverb.DialDampingFrequencyChange(Sender: TObject);
begin
 with TPlateReverbVST(Owner) do
  if Parameter[4] <> DialDampingFrequency.Value then
   begin
    Parameter[4] := DialDampingFrequency.Value;
   end;
end;

procedure TFmPlateReverb.DialInputDiffusionChange(Sender: TObject);
begin
 with TPlateReverbVST(Owner) do
  if Parameter[5] <> DialInputDiffusion.Value then
   begin
    Parameter[5] := DialInputDiffusion.Value;
   end;
end;

procedure TFmPlateReverb.DialDecayDiffusionChange(Sender: TObject);
begin
 with TPlateReverbVST(Owner) do
  if Parameter[6] <> DialDecayDiffusion.Value then
   begin
    Parameter[6] := DialDecayDiffusion.Value;
   end;
end;

procedure TFmPlateReverb.BtAboutClick(Sender: TObject);
begin
 MessageDlg('PlateReverb example plugin written by Christian Budde' + #13#10 +
            'based on GUI by thcilnnahoj', mtInformation, [mbOK], 0);
end;

end.
