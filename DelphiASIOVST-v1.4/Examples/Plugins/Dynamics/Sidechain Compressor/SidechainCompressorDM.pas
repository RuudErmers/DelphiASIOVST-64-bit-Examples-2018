unit SidechainCompressorDM;

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
  Forms, SyncObjs, DAV_Types, DAV_VSTModule, DAV_VSTEffect, DAV_VstHost, 
  DAV_DspFilter, DAV_DspDynamics, DAV_DspLightweightDynamics, 
  DAV_DspDelayLines, DAV_DspFilterButterworth;

type
  TSidechainCompressorDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcessMono(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessStereo(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessMonoSoftClip(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessStereoSoftClip(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleBlockSizeChange(Sender: TObject; const BlockSize: Integer);
    procedure VSTModuleStartProcess(Sender: TObject);
    procedure VSTModuleStopProcess(Sender: TObject);
    procedure VSTModuleProcessMidi(Sender: TObject; const MidiEvent: TVstMidiEvent);
    procedure ParameterAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterAutoMakeUpGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterHoldChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterKneeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterKneeDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterLimitChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterMakeUpGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterMakeUpGainDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOnOffDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterRatioDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterStereoChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterThresholdDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterTimeDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterTimeLabel(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterLowcutFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterLowcutSlopeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterHighcutFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterHighcutSlopeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterSlopeDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterFrequencyDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterFrequencyLabel(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterVstEnableChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FVstHost             : TVstHost;
    FCriticalSection     : TCriticalSection;
    FCompressor          : array [0..1] of TCustomKneeCompressor;
    FDelayLine           : array [0..1] of TDelayLineTime32;
    FLowcut              : array [0..1] of TCustomOrderFilter;
    FHighcut             : array [0..1] of TCustomOrderFilter;
    FMixFactor           : Single;
    FProcessVSTSidechain : Boolean;
    function GetSidechainCompressor(Index: Integer): TCustomKneeCompressor;
    procedure ChooseProcess;
    function GetVSTPlugin: TCustomVstPlugIn;
  public
    function EvaluateCharacteristic(const Input: Single): Single;
    function EvaluateFrequencyResponse(const Frequency: Single): Single;
    procedure LoadVstPlugin(FileName: TFileName);
    class function GetStaticDescription: string; override;

    property SidechainCompressor[Index: Integer]: TCustomKneeCompressor read GetSidechainCompressor;
    property VstPlugIn: TCustomVstPlugIn read GetVSTPlugin;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, DAV_Common, DAV_Approximations, DAV_VSTModuleWithPrograms,
  SidechainCompressorGUI;

procedure TSidechainCompressorDataModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TSidechainCompressorDataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TSidechainCompressorDataModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
const
  CPresets : array [1..12, 0..15] of Single = (
    (20, 1, 20000, 1, 15   , 0.1,  75, -10,  5, 2  ,  6, 0, 0, 0, 100, 1),
    (20, 1, 20000, 1, 50   , 0.1, 500, -10,  3, 4  ,  3, 0, 1, 0, 100, 1),
    (20, 1, 20000, 1, 20   , 0.1, 100, -12,  4, 2.5,  6, 0, 0, 0, 100, 1),
    (20, 1, 20000, 1, 20   , 0.1,  80, -15,  8, 2  ,  8, 0, 1, 0, 100, 1),
    (20, 1, 20000, 1,  5   , 0.1,  60, -20,  7, 3  , 13, 1, 0, 0, 100, 1),
    (20, 1, 20000, 1,  1   , 0.1,  50, -30,  6, 2  , 18, 0, 0, 0, 100, 1),
    (20, 1, 20000, 1,  8   , 0.1,  64, -30, 12, 5  , 17, 0, 0, 0, 100, 1),
    (20, 1, 20000, 1, 16   , 0.1,  78, -24, 15, 1.8, 19, 0, 1, 0, 100, 1),
    (20, 1, 20000, 1,  1   , 0.1,  20, -14,  5, 3  ,  8, 0, 1, 0, 100, 1),
    (20, 1, 20000, 1,  3   , 0.1,  44, -17,  7, 1  ,  9, 1, 0, 0, 100, 1),
    (20, 1, 20000, 1,  8   , 0.1,  56, -11,  9, 4  ,  5, 1, 1, 0, 100, 1),
    (70, 0, 20000, 0,  0.01, 0.8,  50, -50,  4, 0  , 26, 0, 0, 0, 100, 1));
begin
 // create compressor
 for Channel := 0 to Length(FCompressor) - 1 do
  begin
   FCompressor[Channel] := TLightweightSoftKneeCompressor.Create;
   FCompressor[Channel].SampleRate := SampleRate;
  end;

 // create delay line
 for Channel := 0 to Length(FDelayLine) - 1 do
  begin
   FDelayLine[Channel] := TDelayLineTime32.Create;
   FDelayLine[Channel].SampleRate := SampleRate;
  end;

 for Channel := 0 to Length(FLowcut) - 1 do
  begin
   FLowcut[Channel] := TButterworthLowCutFilter.Create;
   FLowcut[Channel].SampleRate := SampleRate;
   FLowcut[Channel].Frequency := 20;
   FLowcut[Channel].Order := 1;
  end;

 for Channel := 0 to Length(FHighcut) - 1 do
  begin
   FHighcut[Channel] := TButterworthHighCutFilter.Create;
   FHighcut[Channel].SampleRate := SampleRate;
   FHighcut[Channel].Frequency := 20000;
   FHighcut[Channel].Order := 1;
  end;

 // create compressor
 FVstHost := TVstHost.Create(Self);
 with FVstHost do
  begin
   VstTimeInfo.SampleRate := Self.SampleRate;
   FVstHost.BlockSize := Self.BlockSize;
  end;
 with FVstHost.VstPlugIns.Add do
  begin
  end;

 Parameter[ 0] :=    20.0;
 Parameter[ 1] :=     0.0;
 Parameter[ 2] := 20000.0;
 Parameter[ 3] :=     0.0;
 Parameter[ 4] :=    15.0;
 Parameter[ 5] :=     0.1;
 Parameter[ 6] :=    75.0;
 Parameter[ 7] :=     0.0;
 Parameter[ 8] :=     1.0;
 Parameter[ 9] :=     0.0;
 Parameter[10] :=     0.0;
 Parameter[11] :=     0.0;
 Parameter[12] :=     0.0;
 Parameter[13] :=     0.0;
 Parameter[14] :=   100.0;

 Programs[0].SetParameters(FParameter);
 for Channel := 1 to numPrograms - 1
  do Programs[Channel].SetParameters(CPresets[Channel]);

 // set GUI editor form class
 EditorFormClass := TFmSidechainCompressor;
end;

procedure TSidechainCompressorDataModule.VSTModuleBlockSizeChange(
  Sender: TObject; const BlockSize: Integer);
begin
 if Assigned(FVstHost)
  then FVstHost.BlockSize := BlockSize;
end;

procedure TSidechainCompressorDataModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 // free compressor
 for Channel := 0 to Length(FCompressor) - 1
  do FreeAndNil(FCompressor[Channel]);

 // free delay line
 for Channel := 0 to Length(FDelayLine) - 1
  do FreeAndNil(FDelayLine[Channel]);

 // free lowpass filter
 for Channel := 0 to Length(FLowcut) - 1
  do FreeAndNil(FLowcut[Channel]);

 // free highpass
 for Channel := 0 to Length(FHighcut) - 1
  do FreeAndNil(FHighcut[Channel]);

 FreeAndNil(FVstHost);
end;

function TSidechainCompressorDataModule.EvaluateCharacteristic(
  const Input: Single): Single;
begin
 Result := FCompressor[0].CharacteristicCurve_dB(Input);
end;

function TSidechainCompressorDataModule.EvaluateFrequencyResponse(
  const Frequency: Single): Single;
begin
 Result := 10 * FastLog10ContinousError4(
   FLowcut[0].MagnitudeSquared(Frequency) *
   FHighcut[1].MagnitudeSquared(Frequency));
end;

procedure TSidechainCompressorDataModule.ChooseProcess;
begin
 case Round(Parameter[12]) of
  0 : case Round(Parameter[11]) of
       0 : OnProcess := VSTModuleProcessMono;
       1 : OnProcess := VSTModuleProcessStereo;
      end;
  1 : case Round(Parameter[11]) of
       0 : OnProcess := VSTModuleProcessMonoSoftClip;
       1 : OnProcess := VSTModuleProcessStereoSoftClip;
      end;
 end;
 OnProcess32Replacing := OnProcess;
end;

function TSidechainCompressorDataModule.GetSidechainCompressor(Index: Integer): TCustomKneeCompressor;
begin
 if Index in [0..Length(FCompressor) - 1]
  then Result := FCompressor[Index]
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

class function TSidechainCompressorDataModule.GetStaticDescription: string;
begin
 Result := 'Sidechain Compressor';
end;

function TSidechainCompressorDataModule.GetVSTPlugin: TCustomVstPlugIn;
begin
 Result := FVSTHost[0];
end;

procedure TSidechainCompressorDataModule.LoadVstPlugin(FileName: TFileName);
begin
 FCriticalSection.Enter;
 try
  with FVSTHost[0] do
   begin
    Active := False;
    LoadFromFile(FileName);
    Active := (numInputs = 2) and (numOutputs = 2);
   end;
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmSidechainCompressor
  then TFmSidechainCompressor(EditorForm).UpdateVstPlugin;
end;

procedure TSidechainCompressorDataModule.ParameterAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCompressor[0]) then
  begin
   FCompressor[0].Attack := Value;
   if Assigned(FCompressor[1])
    then FCompressor[1].Attack := FCompressor[0].Attack;
  end;

 // update GUI
 if EditorForm is TFmSidechainCompressor
  then TFmSidechainCompressor(EditorForm).UpdateAttack;
end;

procedure TSidechainCompressorDataModule.ParameterHoldChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if Assigned(FDelayLine[0]) then
   begin
    FDelayLine[0].Time := 0.001 * Value;
    if Assigned(FDelayLine[1])
     then FDelayLine[1].Time := FDelayLine[0].Time;
   end;
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmSidechainCompressor
  then TFmSidechainCompressor(EditorForm).UpdateHold;
end;

procedure TSidechainCompressorDataModule.ParameterReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCompressor[0]) then
  begin
   FCompressor[0].Release := Value;
   if Assigned(FCompressor[1])
    then FCompressor[1].Release := FCompressor[0].Release;
  end;

 // update GUI
 if EditorForm is TFmSidechainCompressor
  then TFmSidechainCompressor(EditorForm).UpdateRelease;
end;

procedure TSidechainCompressorDataModule.ParameterThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCompressor[0]) then
  begin
   FCompressor[0].Threshold_dB := Value;
   if Assigned(FCompressor[1])
    then FCompressor[1].Threshold_dB := Value;
  end;

 // update GUI
 if EditorForm is TFmSidechainCompressor then
  with TFmSidechainCompressor(EditorForm) do
   begin
    UpdateThreshold;
    if Parameter[10] > 0.5
     then UpdateMakeUp;
   end;
end;

procedure TSidechainCompressorDataModule.ParameterRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCompressor[0]) then
  begin
   FCompressor[0].Ratio := Value;
   if Assigned(FCompressor[1])
    then FCompressor[1].Ratio := Value;
  end;

 // update GUI
 if EditorForm is TFmSidechainCompressor then
  with TFmSidechainCompressor(EditorForm) do
   begin
    UpdateRatio;
    if Parameter[10] > 0.5
     then UpdateMakeUp;
   end;
end;

procedure TSidechainCompressorDataModule.ParameterKneeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCompressor[0]) then
  begin
   FCompressor[0].Knee_dB := Value;
   if Assigned(FCompressor[1])
    then FCompressor[1].Knee_dB := Value;
  end;

 // update GUI
 if EditorForm is TFmSidechainCompressor then
  with TFmSidechainCompressor(EditorForm) do
   begin
    UpdateKnee;
    if Parameter[10] > 0.5
     then UpdateMakeUp;
   end;
end;

procedure TSidechainCompressorDataModule.ParameterAutoMakeUpGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCompressor[0]) then
  begin
   FCompressor[0].AutoMakeUp := Boolean(Round(Value));
   FCompressor[1].AutoMakeUp := FCompressor[0].AutoMakeUp;
  end;

 // update GUI
 if EditorForm is TFmSidechainCompressor
  then TFmSidechainCompressor(EditorForm).UpdateAutoMakeUpGain;
end;

procedure TSidechainCompressorDataModule.ParameterMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FMixFactor := Limit(0.01 * Value, 0, 1);

 // update GUI
 if EditorForm is TFmSidechainCompressor
  then TFmSidechainCompressor(EditorForm).UpdateMix;
end;

procedure TSidechainCompressorDataModule.ParameterVstEnableChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FProcessVSTSidechain := Value > 0.5;

 // update GUI
 if EditorForm is TFmSidechainCompressor
  then TFmSidechainCompressor(EditorForm).UpdateEnableVSTSideChain;
end;

procedure TSidechainCompressorDataModule.ParameterTimeLabel(
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

procedure TSidechainCompressorDataModule.ParameterFrequencyLabel(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] > 1000
  then PreDefined := 'kHz';
end;

procedure TSidechainCompressorDataModule.ParameterFrequencyDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] > 1000
  then PreDefined := AnsiString(FloatToStrF(0.001 * Parameter[Index], ffGeneral, 4, 4));
end;

procedure TSidechainCompressorDataModule.ParameterSlopeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(IntToStr(Round(Parameter[Index]) * 6));
end;

procedure TSidechainCompressorDataModule.ParameterTimeDisplay(
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

procedure TSidechainCompressorDataModule.ParameterMakeUpGainDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3));
end;

procedure TSidechainCompressorDataModule.ParameterLowcutFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FLowcut[0]) then
  begin
   FLowcut[0].Frequency := Value;
   FLowcut[1].Frequency := FLowcut[0].Frequency;
  end;

 // update GUI
 if EditorForm is TFmSidechainCompressor
  then TFmSidechainCompressor(EditorForm).UpdateLowcutFrequency;
end;

procedure TSidechainCompressorDataModule.ParameterLowcutSlopeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FLowcut[0]) then
  begin
   FLowcut[0].Order := Round(Value);
   FLowcut[1].Order := FLowcut[0].Order;
  end;

 // update GUI
 if EditorForm is TFmSidechainCompressor
  then TFmSidechainCompressor(EditorForm).UpdateLowcutOrder;
end;

procedure TSidechainCompressorDataModule.ParameterHighcutFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FHighcut[0]) then
  begin
   FHighcut[0].Frequency := Value;
   FHighcut[1].Frequency := FHighcut[0].Frequency;
  end;

 // update GUI
 if EditorForm is TFmSidechainCompressor
  then TFmSidechainCompressor(EditorForm).UpdateHighcutFrequency;
end;

procedure TSidechainCompressorDataModule.ParameterHighcutSlopeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FHighcut[0]) then
  begin
   FHighcut[0].Order := Round(Value);
   FHighcut[1].Order := FHighcut[0].Order;
  end;

 // update GUI
 if EditorForm is TFmSidechainCompressor
  then TFmSidechainCompressor(EditorForm).UpdateHighcutOrder;
end;

procedure TSidechainCompressorDataModule.ParameterMakeUpGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCompressor[0]) then
  begin
   FCompressor[0].MakeUpGain_dB := Value;
   FCompressor[1].MakeUpGain_dB := FCompressor[0].MakeUpGain_dB;
  end;

 // update GUI
 if EditorForm is TFmSidechainCompressor
  then TFmSidechainCompressor(EditorForm).UpdateMakeUp;
end;

procedure TSidechainCompressorDataModule.ParameterThresholdDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3));
end;

procedure TSidechainCompressorDataModule.ParameterRatioDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3));
end;

procedure TSidechainCompressorDataModule.ParameterKneeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3));
end;

procedure TSidechainCompressorDataModule.ParameterOnOffDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case Round(Parameter[Index]) of
  0 : PreDefined := 'Off';
  1 : PreDefined := 'On';
 end;
end;

procedure TSidechainCompressorDataModule.ParameterStereoChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 ChooseProcess;

 // update GUI
 if EditorForm is TFmSidechainCompressor
  then TFmSidechainCompressor(EditorForm).UpdateStereo;
end;

procedure TSidechainCompressorDataModule.ParameterLimitChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 ChooseProcess;

 // update GUI
 if EditorForm is TFmSidechainCompressor
  then TFmSidechainCompressor(EditorForm).UpdateLimit;
end;

procedure TSidechainCompressorDataModule.VSTModuleProcessMidi(Sender: TObject;
  const MidiEvent: TVstMidiEvent);
var
  Status : Integer;
  CCData : Single;
begin
 Status := MidiEvent.midiData[0] and $F0; // channel information is removed

 if (Status = $B0) then // midi CC ?
  begin
   CCData := MidiEvent.MidiData[2] / 127; // CC data
   case MidiEvent.MidiData[1] of // midi CC#
    70: Parameter[0] := FreqLinearToLog(CCData);
    71: Parameter[1] := Round(16 * CCData);
    72: Parameter[2] := FreqLinearToLog(CCData);
    73: Parameter[3] := Round(16 * CCData);
    77: Parameter[3] := Round(100 * CCData - 90);
   end;
  end;
end;

procedure TSidechainCompressorDataModule.VSTModuleProcessStereo(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample : Integer;
begin
 FCriticalSection.Enter;
 try
  // process hosted VST plugin
  if FVstHost[0].Active and FProcessVSTSidechain
   then FVstHost[0].Process32Replacing(@Inputs[0], @Outputs[0], SampleFrames)
   else
    begin
     Move(Inputs[0, 0], Outputs[0, 0], SampleFrames * SizeOf(Single));
     Move(Inputs[1, 0], Outputs[1, 0], SampleFrames * SizeOf(Single));
    end;

  // process compressor
  for Sample := 0 to SampleFrames - 1 do
   begin
    with FCompressor[0] do
     begin
      InputSample(FLowcut[0].ProcessSample32(FHighcut[0].ProcessSample32(
        FDelayLine[0].ProcessSample32(Outputs[0, Sample]))));
      Outputs[0, Sample] := (FMixFactor * MakeUpGain * GainReductionFactor
         + (1 - FMixFactor)) * Inputs[0, Sample];
     end;

    with FCompressor[1] do
     begin
      InputSample(FLowcut[1].ProcessSample32(FHighcut[1].ProcessSample32(
        FDelayLine[1].ProcessSample32(Outputs[1, Sample]))));
      Outputs[1, Sample] := (FMixFactor * MakeUpGain * GainReductionFactor
         + (1 - FMixFactor)) * Inputs[1, Sample];
     end;
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSidechainCompressorDataModule.VSTModuleProcessMono(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample : Integer;
  Temp   : Single;
begin
 FCriticalSection.Enter;
 try
  // process hosted VST plugin
  if FVstHost[0].Active and FProcessVSTSidechain
   then FVstHost[0].Process32Replacing(@Inputs[0], @Outputs[0], SampleFrames)
   else
    begin
     Move(Inputs[0, 0], Outputs[0, 0], SampleFrames * SizeOf(Single));
     Move(Inputs[1, 0], Outputs[1, 0], SampleFrames * SizeOf(Single));
    end;

  // process compressor
  for Sample := 0 to SampleFrames - 1 do
   with FCompressor[0] do
    begin
     InputSample(FLowcut[0].ProcessSample32(FHighcut[0].ProcessSample32(
       FDelayLine[0].ProcessSample32(CHalf32 * (Outputs[0, Sample] + Outputs[1, Sample])))));
     Temp := FMixFactor * MakeUpGain * GainReductionFactor + (1 - FMixFactor);
     Outputs[0, Sample] := Temp * Inputs[0, Sample];
     Outputs[1, Sample] := Temp * Inputs[1, Sample];
    end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSidechainCompressorDataModule.VSTModuleProcessStereoSoftClip(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample : Integer;
begin
 FCriticalSection.Enter;
 try
  // process hosted VST plugin
  if FVstHost[0].Active and FProcessVSTSidechain
   then FVstHost[0].Process32Replacing(@Inputs[0], @Outputs[0], SampleFrames)
   else
    begin
     Move(Inputs[0, 0], Outputs[0, 0], SampleFrames * SizeOf(Single));
     Move(Inputs[1, 0], Outputs[1, 0], SampleFrames * SizeOf(Single));
    end;

  // process compressor
  for Sample := 0 to SampleFrames - 1 do
   begin
    with FCompressor[0] do
     begin
      InputSample(FLowcut[0].ProcessSample32(FHighcut[0].ProcessSample32(
        FDelayLine[0].ProcessSample32(Outputs[0, Sample]))));
      Outputs[0, Sample] := FastTanhContinousError4((FMixFactor * MakeUpGain *
        GainReductionFactor + (1 - FMixFactor)) * Inputs[0, Sample]);
     end;

    with FCompressor[1] do
     begin
      InputSample(FLowcut[1].ProcessSample32(FHighcut[1].ProcessSample32(
        FDelayLine[1].ProcessSample32(Outputs[1, Sample]))));
      Outputs[1, Sample] := FastTanhContinousError4((FMixFactor * MakeUpGain *
        GainReductionFactor + (1 - FMixFactor)) * Inputs[1, Sample]);
     end;
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSidechainCompressorDataModule.VSTModuleProcessMonoSoftClip(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample : Integer;
  Temp   : Single;
begin
 FCriticalSection.Enter;
 try
  // process hosted VST plugin
  if FVstHost[0].Active and FProcessVSTSidechain  
   then FVstHost[0].Process32Replacing(@Inputs[0], @Outputs[0], SampleFrames)
   else
    begin
     Move(Inputs[0, 0], Outputs[0, 0], SampleFrames * SizeOf(Single));
     Move(Inputs[1, 0], Outputs[1, 0], SampleFrames * SizeOf(Single));
    end;

  // process compressor
  for Sample := 0 to SampleFrames - 1 do
   with FCompressor[0] do
    begin
     InputSample(FLowcut[0].ProcessSample32(FHighcut[0].ProcessSample32(
       FDelayLine[0].ProcessSample32(CHalf32 * (Outputs[0, Sample] + Outputs[1, Sample])))));
     Temp := FMixFactor * (MakeUpGain * GainReductionFactor - 1) + 1;
     Outputs[0, Sample] := FastTanhContinousError4(Temp * Inputs[0, Sample]);
     Outputs[1, Sample] := FastTanhContinousError4(Temp * Inputs[1, Sample]);
    end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSidechainCompressorDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  ChannelIndex : Integer;
begin
 if Abs(SampleRate) <> 0 then
  begin
   if Assigned(FVstHost) then FVstHost.VstTimeInfo.SampleRate := Self.SampleRate;

   // compressor
   for ChannelIndex := 0 to Length(FCompressor) - 1 do
    if Assigned(FCompressor[ChannelIndex])
     then FCompressor[ChannelIndex].SampleRate := SampleRate;

   // delay line
   for ChannelIndex := 0 to Length(FCompressor) - 1 do
    if Assigned(FDelayLine[ChannelIndex])
     then FDelayLine[ChannelIndex].SampleRate := SampleRate;

   // lowpass
   for ChannelIndex := 0 to Length(FCompressor) - 1 do
    if Assigned(FDelayLine[ChannelIndex])
     then FDelayLine[ChannelIndex].SampleRate := SampleRate;

   // highpass
   for ChannelIndex := 0 to Length(FCompressor) - 1 do
    if Assigned(FDelayLine[ChannelIndex])
     then FDelayLine[ChannelIndex].SampleRate := SampleRate;

  end;
end;

procedure TSidechainCompressorDataModule.VSTModuleStartProcess(Sender: TObject);
begin
 if FVstHost[0].Active
  then FVstHost[0].StartProcess;
end;

procedure TSidechainCompressorDataModule.VSTModuleStopProcess(Sender: TObject);
begin
 if FVstHost[0].Active
  then FVstHost[0].StopProcess;
end;

end.
