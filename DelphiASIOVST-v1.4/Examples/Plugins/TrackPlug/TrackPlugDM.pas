unit TrackPlugDM;

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
  Forms, DAV_Types, DAV_DspFilter, DAV_DspFilterButterworth, 
  DAV_DspFilterBasics, DAV_DspDynamics, DAV_DspLightweightDynamics, 
  Dav_DspPsychoacousticBassEnhancer, DAV_VSTModule;

type
  TTrackPlugModule = class(TVSTModule)
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure ParameterDcFilterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDcFilterChangeOrder(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterEqTypeDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterEqFilterFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterEqFilterGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFilterBandwidthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterEqTypeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterCompressorAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterCompressorReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterCompressorThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterCompressorRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterCompressorKneeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterCompressorMakeUpGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterCompressorAutoMakeUpGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterCompressorMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterThresholdDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterRatioDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterKneeDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterOnOffDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterMakeUpGainDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterTimeDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterTimeLabel(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterGateAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterGateReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterGateThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterGateRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterGateKneeChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FDCFilter   : array [0..1] of TButterworthHighPassFilter;
    FEqFilter   : array [0..1, 0..8] of TCustomBandwidthIIRFilter;
    FGate       : array [0..1] of TCustomKneeCompressor;
    FCompressor : array [0..1, 0..1] of TCustomKneeCompressor;
    function GetFilter(Index: Integer): TCustomIIRFilter;
    function GetFilterClass(Index: Integer): TBandwidthIIRFilterClass;
    procedure SetFilterClass(Index: Integer; const Value: TBandwidthIIRFilterClass);
  public
    property FilterClass[Index: Integer]: TBandwidthIIRFilterClass read GetFilterClass write SetFilterClass;
    property Filter[Index: Integer]: TCustomIIRFilter read GetFilter;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, {$IFDEF HAS_UNIT_ANSISTRINGS} AnsiStrings, {$ENDIF} TrackPlugGUI;

resourcestring
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';

procedure TTrackPlugModule.VSTModuleOpen(Sender: TObject);
var
  Channel, Band : Integer;
begin
 // create DC filters
 for Channel := 0 to Length(FDCFilter) - 1 do
  begin
   FDCFilter[Channel] := TButterworthHighPassFilter.Create(1);
   FDCFilter[Channel].SampleRate := SampleRate;
  end;

 // create EQ filters
 for Channel := 0 to Length(FEqFilter) - 1 do
  for Band := 0 to Length(FEqFilter[Channel]) - 1 do
   begin
    FEqFilter[Channel, Band] := TBasicPeakFilter.Create;
    FEqFilter[Channel, Band].SampleRate := SampleRate;
   end;

 // create Gate
 for Channel := 0 to Length(FGate) - 1 do
  begin
   FGate[Channel] := TLightweightSoftKneeCompressor.Create;
   FGate[Channel].SampleRate := SampleRate;
   FGate[Channel].Threshold_dB := -90;
  end;

 // create compressors
 for Channel := 0 to Length(FCompressor) - 1 do
  for Band := 0 to Length(FCompressor[Channel]) - 1 do
   begin
    FCompressor[Channel, Band] := TLightweightSoftKneeCompressor.Create;
    FCompressor[Channel, Band].SampleRate := SampleRate;
    FCompressor[Channel, Band].Threshold_dB := -30;
    FCompressor[Channel, Band].MakeUpGain_dB := 10;
   end;

 // DC filter
 Parameter[ 0] := 5;
 Parameter[ 1] := 1;

 // EQ filter 1
 Parameter[ 2] := 100;
 Parameter[ 3] := 0;
 Parameter[ 4] := 1;
 Parameter[ 5] := 2;

 // EQ filter 2
 Parameter[ 6] := 315;
 Parameter[ 7] := 0;
 Parameter[ 8] := 1;
 Parameter[ 9] := 3;

 // EQ filter 3
 Parameter[10] := 1000;
 Parameter[11] := 0;
 Parameter[12] := 1;
 Parameter[13] := 3;

 // EQ filter 4
 Parameter[14] := 3150;
 Parameter[15] := 0;
 Parameter[16] := 1;
 Parameter[17] := 3;

 // EQ filter 5
 Parameter[18] := 10000;
 Parameter[19] := 0;
 Parameter[20] := 1;
 Parameter[21] := 6;

 // EQ filter 6
 Parameter[22] := 10000;
 Parameter[23] := 0;
 Parameter[24] := 1;
 Parameter[25] := 6;

 // EQ filter 7
 Parameter[26] := 10000;
 Parameter[27] := 0;
 Parameter[28] := 1;
 Parameter[29] := 6;

 // EQ filter 8
 Parameter[30] := 10000;
 Parameter[31] := 0;
 Parameter[32] := 1;
 Parameter[33] := 6;

 // EQ filter 9
 Parameter[34] := 10000;
 Parameter[35] := 0;
 Parameter[36] := 1;
 Parameter[37] := 6;

 // gate
 Parameter[38] := 1.5;
 Parameter[39] := 7.5;
 Parameter[40] := -70;
 Parameter[41] := 0.2;
 Parameter[42] := 6;

 // compressor A
 Parameter[43] := 15;
 Parameter[44] := 75;
 Parameter[45] := -10;
 Parameter[46] := 5;
 Parameter[47] := 2;
 Parameter[48] := 6;
 Parameter[49] := 0;
 Parameter[50] := 100;

 // compressor B
 Parameter[51] := 15;
 Parameter[52] := 75;
 Parameter[53] := -10;
 Parameter[54] := 5;
 Parameter[55] := 2;
 Parameter[56] := 6;
 Parameter[57] := 0;
 Parameter[58] := 100;

 // set editor form class
 EditorFormClass := TFmTrackPlug;
end;

procedure TTrackPlugModule.VSTModuleClose(Sender: TObject);
var
  Channel, Band : Integer;
begin
 // free DC filters
 for Channel := 0 to Length(FDCFilter) - 1
  do FreeAndNil(FDCFilter[Channel]);

 // free EQ filters
 for Channel := 0 to Length(FEqFilter) - 1 do
  for Band := 0 to Length(FEqFilter[Channel]) - 1
   do FreeAndNil(FEqFilter[Channel, Band]);

 // free gate  
 for Channel := 0 to Length(FGate) - 1
  do FreeAndNil(FGate[Channel]);

 // free compressor
 for Channel := 0 to Length(FCompressor) - 1 do
  for Band := 0 to Length(FCompressor[Channel]) - 1
   do FreeAndNil(FCompressor[Channel, Band]);
end;

function TTrackPlugModule.GetFilter(Index: Integer): TCustomIIRFilter;
begin
 if (Index >= 0) and (Index < Length(FEqFilter[0]))
  then Result := FEqFilter[0, Index]
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TTrackPlugModule.GetFilterClass(
  Index: Integer): TBandwidthIIRFilterClass;
begin
 if (Index >= 0) and (Index < Length(FEqFilter[0]))
  then Result := TBandwidthIIRFilterClass(FEqFilter[0, Index].ClassType)
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TTrackPlugModule.ParameterDCFilterChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FDCFilter) - 1 do
  if Assigned(FDCFilter[Channel])
   then FDCFilter[Channel].Frequency := Value;
end;

procedure TTrackPlugModule.ParameterDcFilterChangeOrder(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FDCFilter) - 1 do
  if Assigned(FDCFilter[Channel])
   then FDCFilter[Channel].Order := Round(Value);
end;

procedure TTrackPlugModule.ParameterEqTypeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case Round(Parameter[Index]) of
  0 : Predefined := 'Bypass';
  1 : Predefined := 'Lowcut';
  2 : Predefined := 'Lowshelf';
  3 : Predefined := 'Peak';
  4 : Predefined := 'Bandpass';
  5 : Predefined := 'Notch';
  6 : Predefined := 'Highshelf';
  7 : Predefined := 'Highcut';
 end;
end;

procedure TTrackPlugModule.ParameterEqFilterFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel, Band : Integer;
begin
 Band := (Index - 2) div 4;

 for Channel := 0 to Length(FEqFilter) - 1 do
  if Assigned(FEqFilter[Channel, Band])
   then FEqFilter[Channel, Band].Frequency := Value;
end;

procedure TTrackPlugModule.ParameterEqFilterGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel, Band : Integer;
begin
 Band := (Index - 2) div 4;

 for Channel := 0 to Length(FEqFilter) - 1 do
  if Assigned(FEqFilter[Channel, Band])
   then FEqFilter[Channel, Band].Gain := Value;
end;

procedure TTrackPlugModule.ParameterFilterBandwidthChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel, Band : Integer;
begin
 Band := (Index - 2) div 4;

 for Channel := 0 to Length(FEqFilter) - 1 do
  if Assigned(FEqFilter[Channel, Band])
   then FEqFilter[Channel, Band].Bandwidth := Value;
end;

procedure TTrackPlugModule.SetFilterClass(Index: Integer;
  const Value: TBandwidthIIRFilterClass);
var
  OldFilter : TCustomIIRFilter;
  Channel   : Integer;
begin
 if (Index >= 0) and (Index < Length(FEqFilter[0])) then
  for Channel := 0 to Length(FEqFilter) - 1 do
   if Assigned(FEqFilter[Channel, Index]) then
    if TBandwidthIIRFilterClass(FEqFilter[Channel, Index].ClassType) <> Value then
     begin
      OldFilter := FEqFilter[Channel, Index];
      FEqFilter[Channel, Index] := Value.Create;
      FEqFilter[Channel, Index].Assign(OldFilter);
      if Assigned(OldFilter) then FreeAndNil(OldFilter);
     end else
   else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TTrackPlugModule.ParameterEqTypeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Band := (Index - 2) div 4;

 if Assigned(FEqFilter[0, Band]) then
  case Round(Value) of
    0 : FilterClass[Band] := TBasicGainFilter;
    1 : FilterClass[Band] := TBasicLowcutFilter;
    2 : FilterClass[Band] := TBasicLowShelfFilter;
    3 : FilterClass[Band] := TBasicPeakFilter;
    4 : FilterClass[Band] := TBasicBandpassFilter;
    5 : FilterClass[Band] := TBasicNotchFilter;
    6 : FilterClass[Band] := TBasicHighShelfFilter;
    7 : FilterClass[Band] := TBasicHighpassFilter;
  end;
end;

procedure TTrackPlugModule.ParameterCompressorAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Assert(Index >= 43);
 Band := (Index - 43) div 8;

 if Assigned(FCompressor[0, Band]) then
  begin
   FCompressor[0, Band].Attack := Value;
   if Assigned(FCompressor[1, Band])
    then FCompressor[1, Band].Attack := FCompressor[0, Band].Attack;
  end;
end;

procedure TTrackPlugModule.ParameterCompressorReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Assert(Index >= 43);
 Band := (Index - 43) div 8;

 if Assigned(FCompressor[0, Band]) then
  begin
   FCompressor[0, Band].Release := Value;
   if Assigned(FCompressor[1, Band])
    then FCompressor[1, Band].Release := FCompressor[0, Band].Release;
  end;
end;

procedure TTrackPlugModule.ParameterCompressorThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Assert(Index >= 43);
 Band := (Index - 43) div 8;

 if Assigned(FCompressor[0, Band]) then
  begin
   FCompressor[0, Band].Threshold_dB := Value;
   if Assigned(FCompressor[1, Band])
    then FCompressor[1, Band].Threshold_dB := Value;
  end;
end;

procedure TTrackPlugModule.ParameterCompressorRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Assert(Index >= 43);
 Band := (Index - 43) div 8;

 if Assigned(FCompressor[0, Band]) then
  begin
   FCompressor[0, Band].Ratio := Value;
   if Assigned(FCompressor[1, Band])
    then FCompressor[1, Band].Ratio := Value;
  end;
end;

procedure TTrackPlugModule.ParameterCompressorKneeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Assert(Index >= 43);
 Band := (Index - 43) div 8;

 if Assigned(FCompressor[0, Band]) then
  begin
   FCompressor[0, Band].Knee_dB := Value;
   if Assigned(FCompressor[1, Band])
    then FCompressor[1, Band].Knee_dB := Value;
  end;
end;

procedure TTrackPlugModule.ParameterCompressorAutoMakeUpGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Assert(Index >= 43);
 Band := (Index - 43) div 8;

 if Assigned(FCompressor[0, Band]) then
  begin
   FCompressor[0, Band].AutoMakeUp := Boolean(Round(Value));
   FCompressor[1, Band].AutoMakeUp := FCompressor[0, Band].AutoMakeUp;
  end;
end;

procedure TTrackPlugModule.ParameterCompressorMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 Value := 100;
end;

procedure TTrackPlugModule.ParameterGateAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FGate[0]) then
  begin
   FGate[0].Attack := Value;
   if Assigned(FGate[1])
    then FGate[1].Attack := FGate[0].Attack;
  end;
end;

procedure TTrackPlugModule.ParameterGateReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FGate[0]) then
  begin
   FGate[0].Release := Value;
   if Assigned(FGate[1])
    then FGate[1].Release := FGate[0].Release;
  end;
end;

procedure TTrackPlugModule.ParameterGateThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FGate[0]) then
  begin
   FGate[0].Threshold_dB := Value;
   if Assigned(FGate[1])
    then FGate[1].Threshold_dB := Value;
  end;
end;

procedure TTrackPlugModule.ParameterGateRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FGate[0]) then
  begin
   FGate[0].Ratio := Value;
   if Assigned(FGate[1])
    then FGate[1].Ratio := Value;
  end;
end;

procedure TTrackPlugModule.ParameterGateKneeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FGate[0]) then
  begin
   FGate[0].Knee_dB := Value;
   if Assigned(FGate[1])
    then FGate[1].Knee_dB := Value;
  end;
end;

procedure TTrackPlugModule.ParameterTimeLabel(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
var
  Val : Single;
begin
 Val := Parameter[Index];
 if Val < 1
  then PreDefined := 'µs' else
 if Val >= 1000
  then PreDefined := 's';
end;

procedure TTrackPlugModule.ParameterTimeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
var
  Val : Single;
begin
 Val := Parameter[Index];
 if Val < 1
  then PreDefined := AnsiString(FloatToStrF(RoundTo(1E3 * Val, -2), ffGeneral, 3, 3)) else
 if Val < 1000
  then PreDefined := AnsiString(FloatToStrF(RoundTo(Val, -2), ffGeneral, 3, 3))
  else PreDefined := AnsiString(FloatToStrF(RoundTo(1E-3 * Val, -2), ffGeneral, 3, 3));
end;

procedure TTrackPlugModule.ParameterMakeUpGainDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3));
end;

procedure TTrackPlugModule.ParameterCompressorMakeUpGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Assert(Index >= 43);
 Band := (Index - 43) div 8;

 if Assigned(FCompressor[0, Band]) then
  begin
   FCompressor[0, Band].MakeUpGain_dB := Value;
   FCompressor[1, Band].MakeUpGain_dB := FCompressor[0, Band].MakeUpGain_dB;
  end;
end;

procedure TTrackPlugModule.ParameterThresholdDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3));
end;

procedure TTrackPlugModule.ParameterRatioDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3));
end;

procedure TTrackPlugModule.ParameterKneeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3));
end;

procedure TTrackPlugModule.ParameterOnOffDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case Round(Parameter[Index]) of
  0 : PreDefined := 'Off';
  1 : PreDefined := 'On';
 end;
end;

procedure TTrackPlugModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel, Band : Integer;
begin
 // DC filters
 for Channel := 0 to Length(FDCFilter) - 1 do
  if Assigned(FDCFilter[Channel])
   then FDCFilter[Channel].SampleRate := SampleRate;

 // EQ filters
 for Channel := 0 to Length(FEqFilter) - 1 do
  for Band := 0 to Length(FEqFilter[Channel]) - 1 do
   if Assigned(FEqFilter[Channel, Band])
    then FEqFilter[Channel, Band].SampleRate := SampleRate;

 // compressor
 for Channel := 0 to Length(FCompressor) - 1 do
  for Band := 0 to Length(FCompressor[Channel]) - 1 do
   if Assigned(FCompressor[Channel, Band])
    then FCompressor[Channel, Band].SampleRate := SampleRate;
end;

procedure TTrackPlugModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Channel, Sample : Integer;
  CurrentSample   : Double;
  Band            : Integer;
begin
 for Channel := 0 to Length(FDCFilter) - 1 do
  for Sample := 0 to SampleFrames - 1 do
   begin
    // DC filter first
    CurrentSample := FDCFilter[Channel].ProcessSample64(Inputs[Channel, Sample]);

    // process EQ filters
    for Band := 0 to Length(FEqFilter) - 1
     do CurrentSample := FEqFilter[Channel, Band].ProcessSample64(CurrentSample);

    CurrentSample := FGate[Channel].ProcessSample64(CurrentSample);

    CurrentSample := FCompressor[Channel, 0].ProcessSample64(CurrentSample);
    CurrentSample := FCompressor[Channel, 1].ProcessSample64(CurrentSample);

    Outputs[Channel, Sample] := CurrentSample;
   end;
end;

end.
