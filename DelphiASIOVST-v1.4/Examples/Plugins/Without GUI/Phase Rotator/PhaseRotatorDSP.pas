unit PhaseRotatorDSP;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2010-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, SyncObjs, DAV_Types, DAV_DspFilter, DAV_DspFilterBasics, DAV_VSTModule, 
  DAV_VSTEffect;

type
  TPhaseRotatorModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessMidi(Sender: TObject; const MidiEvent: TVstMidiEvent);
    procedure ParameterOrderDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterFrequencyDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFrequencyLabel(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterBandwidthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterBandwidthDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
  private
    FAllpass         : array of array [0..3] of TBasicAllpassFilter;
    FOrder           : Integer;
    FCriticalSection : TCriticalSection;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, DAV_Common;

procedure TPhaseRotatorModule.VSTModuleCreate(Sender: TObject);
begin
 Assert(numInputs = numOutputs);
 FCriticalSection := TCriticalSection.Create;
end;

procedure TPhaseRotatorModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TPhaseRotatorModule.VSTModuleOpen(Sender: TObject);
var
  ChannelIndex : Integer;
  BandIndex    : Integer;
begin
 SetLength(FAllpass, numInputs);

 for ChannelIndex := 0 to Length(FAllpass) - 1 do
  for BandIndex := 0 to Length(FAllpass[ChannelIndex]) - 1 do
   begin
    FAllpass[ChannelIndex, BandIndex] := TBasicAllpassFilter.Create;
    with FAllpass[ChannelIndex, BandIndex] do
     begin
      SampleRate := Self.SampleRate;
      Frequency  := 200;
      Bandwidth  := 1.4;
     end;
   end;

 Parameter[0] := 200;
 Parameter[1] := 2;
 Parameter[2] := 1.4;
end;

procedure TPhaseRotatorModule.VSTModuleClose(Sender: TObject);
var
  ChannelIndex : Integer;
  BandIndex    : Integer;
begin
 for ChannelIndex := 0 to Length(FAllpass) - 1 do
  for BandIndex := 0 to Length(FAllpass[ChannelIndex]) - 1
   do FreeAndNil(FAllpass[ChannelIndex, BandIndex]);
end;

procedure TPhaseRotatorModule.ParameterFrequencyDisplay(Sender: TObject;
  const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] >= 1000
  then PreDefined := FloatToAnsiString(1E-3 * Parameter[Index], 3);
end;

procedure TPhaseRotatorModule.ParameterFrequencyLabel(Sender: TObject;
  const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] >= 1000
  then PreDefined := 'kHz';
end;

procedure TPhaseRotatorModule.ParameterOrderDisplay(Sender: TObject;
  const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(IntToStr(2 * Round(Parameter[Index])));
end;

procedure TPhaseRotatorModule.ParameterBandwidthDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := FloatToAnsiString(Parameter[Index], 2);
end;

procedure TPhaseRotatorModule.ParameterBandwidthChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
  BandIndex    : Integer;
begin
 for ChannelIndex := 0 to Length(FAllpass) - 1 do
  for BandIndex := 0 to Length(FAllpass[ChannelIndex]) - 1 do
   if Assigned(FAllpass[ChannelIndex, BandIndex])
    then FAllpass[ChannelIndex, BandIndex].Bandwidth := Value;
end;

procedure TPhaseRotatorModule.ParameterFrequencyChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
  BandIndex    : Integer;
begin
 for ChannelIndex := 0 to Length(FAllpass) - 1 do
  for BandIndex := 0 to Length(FAllpass[ChannelIndex]) - 1 do
   if Assigned(FAllpass[ChannelIndex, BandIndex])
    then FAllpass[ChannelIndex, BandIndex].Frequency := Value;
end;

procedure TPhaseRotatorModule.ParameterOrderChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 FOrder := IntLimit(Round(Value), 0, 4);
end;

procedure TPhaseRotatorModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  ChannelIndex : Integer;
  BandIndex    : Integer;
begin
 if Abs(SampleRate) > 0 then
  for ChannelIndex := 0 to Length(FAllpass) - 1 do
   for BandIndex := 0 to Length(FAllpass[ChannelIndex]) - 1 do
    if Assigned(FAllpass[ChannelIndex, BandIndex])
     then FAllpass[ChannelIndex, BandIndex].SampleRate := Abs(SampleRate);
end;

procedure TPhaseRotatorModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  SampleIndex  : Integer;
  ChannelIndex : Integer;
  BandIndex    : Integer;
  Data         : Double;
begin
 for ChannelIndex := 0 to Length(FAllpass) - 1 do
  for SampleIndex := 0 to SampleFrames - 1 do
   begin
    Data := Inputs[ChannelIndex, SampleIndex];
    for BandIndex := 0 to FOrder - 1
     do Data := FAllpass[ChannelIndex, BandIndex].ProcessSample64(Data);
    Outputs[ChannelIndex, SampleIndex] := Data;
   end;
end;

procedure TPhaseRotatorModule.VSTModuleProcessMidi(Sender: TObject;
  const MidiEvent: TVstMidiEvent);
var
  Status : Integer;
  CCData : Single;
begin
 Status := MidiEvent.MidiData[0] and $F0; // channel information is removed

 if (Status = $B0) then // midi CC ?
  begin
   CCData := MidiEvent.MidiData[2] / 127; // CC data
   case MidiEvent.MidiData[1] of // midi CC#
    70: Parameter[0] := FreqLinearToLog(CCData);
    71: Parameter[1] := 4 * CCData;
    72: Parameter[2] := 0.1 * Power(10, 2 * CCData);
   end;
  end;
end;

end.
