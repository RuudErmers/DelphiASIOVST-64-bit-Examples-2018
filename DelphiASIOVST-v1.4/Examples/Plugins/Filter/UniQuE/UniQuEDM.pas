unit UniQuEDM;

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
  Forms, DAV_Types, DAV_VSTModule, DAV_DspFilter, DAV_DspFilterBasics;

const
  CGainRange = 15;

type
  TUniQuEDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParamPowerDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParamPhaseDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParamPadDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure ParamPowerChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamPhaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamPadChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamLowChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamMidChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamPresChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamHigh(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleResume(Sender: TObject);
  private
    FFade   : array [0..1] of Single;
    FVolume : Single;
    FLow    : array [0..1] of TBasicLowShelfFilter;
    FMid    : array [0..1] of TBasicPeakFilter;
    FPres   : array [0..1] of TBasicPeakFilter;
    FHigh   : array [0..1] of TBasicHighShelfFilter;
    procedure UpdateVolume;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  DAV_Common, DAV_VSTCustomModule, UniQuEGUI;

procedure TUniQuEDataModule.VSTModuleOpen(Sender: TObject);
var
  ch : Integer;
begin
 for ch := 0 to 1 do
  begin
   FLow[ch]  := TBasicLowShelfFilter.Create;
   with FLow[ch] do
    begin
     Frequency := 777;
     Gain      := 0;
     Bandwidth := 3.2;
    end;
   FMid[ch]  := TBasicPeakFilter.Create;
   with FMid[ch] do
    begin
     Frequency := 1700;
     Gain      := 0;
     Bandwidth := 3.6;
    end;
   FPres[ch] := TBasicPeakFilter.Create;
   with FPres[ch] do
    begin
     Frequency := 7280;
     Gain      := 0;
     Bandwidth := 1.0;
    end;
   FHigh[ch] := TBasicHighShelfFilter.Create;
   with FHigh[ch] do
    begin
     Frequency := 4340;
     Gain      := 0;
     Bandwidth := 2.55;
    end;
  end;

 // Initial Parameters
 Parameter[0] := 1;
 Parameter[1] := 0;
 Parameter[2] := 0;
 Parameter[3] := 0;
 Parameter[4] := 0;
 Parameter[5] := 0;
 Parameter[6] := 0;

 EditorFormClass := TFmUniQuE;
end;

procedure TUniQuEDataModule.VSTModuleClose(Sender: TObject);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to 1 do
  begin
   FreeAndNil(FLow[ChannelIndex]);
   FreeAndNil(FMid[ChannelIndex]);
   FreeAndNil(FPres[ChannelIndex]);
   FreeAndNil(FHigh[ChannelIndex]);
  end;
end;

procedure TUniQuEDataModule.ParamPowerDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] > 0.5
  then PreDefined := 'On'
  else PreDefined := 'Off';
end;

procedure TUniQuEDataModule.ParamPresChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  Gain, Frequency  : Single;
begin
 if Value > 0
  then Gain := Value * 11.46 / CGainRange
  else Gain := Value * 12.96 / CGainRange;

 if Assigned(FPres[0]) then FPres[0].Gain := Gain;
 if Assigned(FPres[1]) then FPres[1].Gain := Gain;

 Frequency := 7278 + 108 * Value / CGainRange;

 if Assigned(FPres[0]) then FPres[0].Frequency := Frequency;
 if Assigned(FPres[1]) then FPres[1].Frequency := Frequency;

 // update GUI
 if Assigned(EditorForm) then
  with TFmUniQuE(EditorForm)
   do UpdatePres;
end;

procedure TUniQuEDataModule.ParamPhaseChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 UpdateVolume;
 if Assigned(EditorForm) then
  with TFmUniQuE(EditorForm)
   do UpdateInvert;
end;

procedure TUniQuEDataModule.ParamPowerChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FFade[0] := Value;
 FFade[1] := 1 - Value;

 // update GUI
 if Assigned(EditorForm) then
  with TFmUniQuE(EditorForm)
   do UpdateOnOff;
end;

procedure TUniQuEDataModule.ParamHigh(Sender: TObject; const Index: Integer; var Value: Single);
var
  Gain, Frequency: Single;
begin
 if Value > 0
  then Gain := Value * 15 / CGainRange
  else Gain := Value * 13.5 / CGainRange;

 if Assigned(FHigh[0]) then FHigh[0].Gain := Gain;
 if Assigned(FHigh[1]) then FHigh[1].Gain := Gain;

 Frequency := 4340 - 300 * Value / CGainRange;

 if Assigned(FHigh[0]) then FHigh[0].Frequency := Frequency;
 if Assigned(FHigh[1]) then FHigh[1].Frequency := Frequency;

 UpdateVolume;

 // update GUI
 if Assigned(EditorForm) then
  with TFmUniQuE(EditorForm)
   do UpdateHigh;
end;

procedure TUniQuEDataModule.ParamLowChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  Gain : Single;
begin
 if Value > 0
  then Gain := Value * 11.9 / CGainRange
  else Gain := Value * 12.3 / CGainRange;

 if Assigned(FLow[0]) then FLow[0].Gain := Gain;
 if Assigned(FLow[1]) then FLow[1].Gain := Gain;

 // update GUI
 if Assigned(EditorForm) then
  with TFmUniQuE(EditorForm)
   do UpdateLow;
end;

procedure TUniQuEDataModule.ParamMidChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  Gain, BW : Single;
begin
 if Value > 0
  then Gain := Value * 11.42 / CGainRange
  else Gain := Value * 13.35 / CGainRange;

 if Assigned(FMid[0]) then FMid[0].Gain := Gain;
 if Assigned(FMid[1]) then FMid[1].Gain := Gain;

 BW := 3.6 + 0.1 * Value / CGainRange;

 if Assigned(FMid[0]) then FMid[0].Bandwidth := BW;
 if Assigned(FMid[1]) then FMid[1].Bandwidth := BW;

 // update GUI
 if Assigned(EditorForm) then
  with TFmUniQuE(EditorForm)
   do UpdateMid;
end;

procedure TUniQuEDataModule.UpdateVolume;
var
  HighAtt : Single;
begin
 if Parameter[6] > 0
  then HighAtt := 4.05 * Parameter[6] / CGainRange
  else HighAtt := 1.28 * Parameter[6] / CGainRange;

 if Parameter[2] > 0.5
  then FVolume := -dB_to_Amp(-Parameter[1] - HighAtt)
  else FVolume :=  dB_to_Amp(-Parameter[1] - HighAtt);
end;

procedure TUniQuEDataModule.ParamPadChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 UpdateVolume;

 // update GUI
 if Assigned(EditorForm) then
  with TFmUniQuE(EditorForm)
   do UpdatePad;
end;

procedure TUniQuEDataModule.ParamPadDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(IntToStr(Round(Parameter[Index])));
end;

procedure TUniQuEDataModule.ParamPhaseDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] > 0.5
  then PreDefined := '-'
  else PreDefined := '+';
end;

procedure TUniQuEDataModule.VSTModuleResume(Sender: TObject);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to 1 do
  begin
   if Assigned(FLow[ChannelIndex]) then FLow[ChannelIndex].ResetStates;
   if Assigned(FMid[ChannelIndex]) then FMid[ChannelIndex].ResetStates;
   if Assigned(FPres[ChannelIndex]) then FPres[ChannelIndex].ResetStates;
   if Assigned(FHigh[ChannelIndex]) then FHigh[ChannelIndex].ResetStates;
  end;
end;

procedure TUniQuEDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  ChannelIndex : Integer;
begin
 if Abs(SampleRate) > 0 then
  for ChannelIndex := 0 to 1 do
   begin
    if Assigned(FLow[ChannelIndex]) then FLow[ChannelIndex].SampleRate   := SampleRate;
    if Assigned(FMid[ChannelIndex]) then FMid[ChannelIndex].SampleRate   := SampleRate;
    if Assigned(FPres[ChannelIndex]) then FPres[ChannelIndex].SampleRate := SampleRate;
    if Assigned(FHigh[ChannelIndex]) then FHigh[ChannelIndex].SampleRate := SampleRate;
   end;
end;

procedure TUniQuEDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  ChannelIndex, SampleIndex : Integer;
begin
 for ChannelIndex := 0 to 1 do
  for SampleIndex := 0 to SampleFrames - 1 do
   begin
    Outputs[ChannelIndex, SampleIndex] := FFade[0] * FVolume *
      FLow[ChannelIndex].ProcessSample64(
      FMid[ChannelIndex].ProcessSample64(
      FPres[ChannelIndex].ProcessSample64(
      FHigh[ChannelIndex].ProcessSample64(Inputs[ChannelIndex, SampleIndex])))) +
      FFade[1] * Inputs[ChannelIndex, SampleIndex];
   end;
end;

end.
