unit HumRemovalGUI;

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
  Forms, Controls, StdCtrls, DAV_Types, DAV_VSTModule, DAV_GuiLED, DAV_GuiPng,
  DAV_GuiBaseControl, DAV_GuiGraphicControl, DAV_GuiSelectBox, DAV_GuiLabel,
  DAV_GuiGroup, DAV_GuiEQGraph, DAV_GuiStitchedControls, DAV_GuiStitchedPngList,
  DAV_GuiStitchedDial, DAV_GuiImageControl, DAV_GuiCustomControl;

type
  TFmHumRemoval = class(TForm)
    GbHighpass: TGuiGroup;
    GbNotchFilters: TGuiGroup;
    GuiEQGraph: TGuiEQGraph;
    LbCaptureHumProfile: TGuiLabel;
    LbFundamentalFrequency: TGuiLabel;
    LbFundamentalFrequencyValue: TGuiLabel;
    LbHighpassFrequency: TGuiLabel;
    LbHighpassFrequencyValue: TGuiLabel;
    LbHighpassOrder: TGuiLabel;
    LbHighpassOrderValue: TGuiLabel;
    LbNotchBandwidth: TGuiLabel;
    LbNotchBandwidthValue: TGuiLabel;
    LedHighpassActive: TGuiLED;
    LedHumProfile: TGuiLED;
    SbHighpassType: TGuiSelectBox;
    DialHighpassFrequency: TGuiStitchedDial;
    DialHighpassOrder: TGuiStitchedDial;
    DialFundamentalFrequency: TGuiStitchedDial;
    DialNotchBandwidth: TGuiStitchedDial;
    GSPL: TGuiStitchedPNGList;
    procedure FormShow(Sender: TObject);
    function GuiEQGraphGetFilterGain(Sender: TObject; const Frequency: Single): Single;
    procedure DialFundamentalFrequencyChange(Sender: TObject);
    procedure DialHighpassFrequencyChange(Sender: TObject);
    procedure DialHighpassOrderChange(Sender: TObject);
    procedure DialNotchBandwidthChange(Sender: TObject);
    procedure LedHighpassActiveClick(Sender: TObject);
    procedure LedHumProfileClick(Sender: TObject);
    procedure SbHighpassTypeChange(Sender: TObject);
  public
    procedure UpdateHighpassActive;
    procedure UpdateHighpassType;
    procedure UpdateHighpassFrequency;
    procedure UpdateHighpassOrder;
    procedure UpdateFundamentalFrequency;
    procedure UpdateBandwidth;
    procedure UpdateCaptureHumProfile;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, DAV_Common, DAV_VSTModuleWithPrograms, HumRemovalDSP;

procedure TFmHumRemoval.FormShow(Sender: TObject);
begin
 UpdateHighpassActive;
 UpdateHighpassType;
 UpdateHighpassFrequency;
 UpdateHighpassOrder;
 UpdateBandwidth;
 UpdateFundamentalFrequency;
end;

function TFmHumRemoval.GuiEQGraphGetFilterGain(Sender: TObject;
  const Frequency: Single): Single;
begin
 with THumRemovalModule(Owner) do
  begin
   Result := Magnitude_dB(Frequency);
  end;
end;

procedure TFmHumRemoval.LedHighpassActiveClick(Sender: TObject);
begin
 with THumRemovalModule(Owner) do
  begin
   Parameter[0] := 1 - Parameter[0];
  end;
end;

procedure TFmHumRemoval.LedHumProfileClick(Sender: TObject);
begin
 with THumRemovalModule(Owner) do
  begin
   Parameter[7] := 1 - Parameter[7];
  end;
end;

procedure TFmHumRemoval.SbHighpassTypeChange(Sender: TObject);
begin
 with THumRemovalModule(Owner) do
  begin
   if Parameter[1] <> SbHighpassType.ItemIndex
    then Parameter[1] := SbHighpassType.ItemIndex;
  end;
end;

procedure TFmHumRemoval.DialHighpassFrequencyChange(Sender: TObject);
begin
 with THumRemovalModule(Owner) do
  begin
   if Parameter[2] <> DialHighpassFrequency.Value
    then Parameter[2] := DialHighpassFrequency.Value;
  end;
end;

procedure TFmHumRemoval.DialHighpassOrderChange(Sender: TObject);
begin
 with THumRemovalModule(Owner) do
  begin
   if Parameter[3] <> DialHighpassOrder.Value
    then Parameter[3] := DialHighpassOrder.Value;
  end;
end;

procedure TFmHumRemoval.DialFundamentalFrequencyChange(Sender: TObject);
begin
 with THumRemovalModule(Owner) do
  begin
   if Parameter[4] <> DialFundamentalFrequency.Value
    then Parameter[4] := DialFundamentalFrequency.Value;
  end;
end;

procedure TFmHumRemoval.DialNotchBandwidthChange(Sender: TObject);
begin
 with THumRemovalModule(Owner) do
  begin
   if Parameter[5] <> DialNotchBandwidth.Value
    then Parameter[5] := DialNotchBandwidth.Value;
  end;
end;

procedure TFmHumRemoval.UpdateHighpassType;
begin
 with THumRemovalModule(Owner) do
  begin
   if SbHighpassType.ItemIndex <> Round(Parameter[1])
    then SbHighpassType.ItemIndex := Round(Parameter[1]);
   GuiEQGraph.FilterSeries[0].DataChanged;
   GuiEQGraph.UpdateGraph;
  end;
end;

procedure TFmHumRemoval.UpdateHighpassActive;
begin
 with THumRemovalModule(Owner) do
  begin
   LedHighpassActive.Brightness_Percent := Limit(10 + 80 * Parameter[0], 10, 90);
   GuiEQGraph.FilterSeries[0].DataChanged;
   GuiEQGraph.UpdateGraph;
  end;
end;

procedure TFmHumRemoval.UpdateHighpassFrequency;
begin
 with THumRemovalModule(Owner) do
  begin
   if DialHighpassFrequency.Value <> Parameter[2]
    then DialHighpassFrequency.Value := Parameter[2];
   LbHighpassFrequencyValue.Caption := string(ParameterDisplay[2] + ' ' + ParameterLabel[2]);
   GuiEQGraph.FilterSeries[0].DataChanged;
   GuiEQGraph.UpdateGraph;
  end;
end;

procedure TFmHumRemoval.UpdateHighpassOrder;
begin
 with THumRemovalModule(Owner) do
  begin
   if DialHighpassOrder.Value <> Parameter[3]
    then DialHighpassOrder.Value := Parameter[3];
   LbHighpassOrderValue.Caption := string(ParameterDisplay[3]);
   GuiEQGraph.FilterSeries[0].DataChanged;
   GuiEQGraph.UpdateGraph;
  end;
end;

procedure TFmHumRemoval.UpdateFundamentalFrequency;
begin
 with THumRemovalModule(Owner) do
  begin
   if DialFundamentalFrequency.Value <> Parameter[4]
    then DialFundamentalFrequency.Value := Parameter[4];
   LbFundamentalFrequencyValue.Caption := string(ParameterDisplay[4] + ' ' + ParameterLabel[4]);
   GuiEQGraph.FilterSeries[0].DataChanged;
   GuiEQGraph.UpdateGraph;
  end;
end;

procedure TFmHumRemoval.UpdateBandwidth;
begin
 with THumRemovalModule(Owner) do
  begin
   if DialNotchBandwidth.Value <> Parameter[5]
    then DialNotchBandwidth.Value := Parameter[5];
   LbNotchBandwidthValue.Caption := string(ParameterDisplay[5] + ' ' + ParameterLabel[5]);
   GuiEQGraph.FilterSeries[0].DataChanged;
   GuiEQGraph.UpdateGraph;
  end;
end;

procedure TFmHumRemoval.UpdateCaptureHumProfile;
begin
 with THumRemovalModule(Owner) do
  begin
   LedHumProfile.Brightness_Percent := Limit(10 + 80 * Parameter[7], 10, 90);
  end;
end;

end.
