unit SplitHarmonizerDM;

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
  Forms, SyncObjs, DAV_Types, DAV_VSTModule, DAV_DspDelayLines, DAV_DspFilter, 
  DAV_DspFilterButterworth, DAV_SoundTouchDLL;

const
  CInputDelay = 5384;

type
  TSplitHarmonizerModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcessLR(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessMS(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessReplacing64LR(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessReplacing64MS(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterDelayAChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDelayBChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterEncodeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterEncodeDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterMixAChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterMixRightChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterSemiTonesAChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterSemiTonesBChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterUseAntiAliasChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterUseQuickSeekChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOverlapChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterSeekWindowChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterSequenceChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOnOffDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterAutoSettingsChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterLowpassAChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterLowpassBChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FSoundTouch      : array [0..1] of TSoundTouch;
    FDelayLine       : array [0..1, 0..1] of TDelayLineSamples32;
    FLowpassFilter   : array [0..1] of TButterworthLowPassFilter;
    FMix             : array [0..1] of Single;
    FCriticalSection : TCriticalSection;
    FAutoSettings    : Boolean;
    procedure SetAutoSettings(const Value: Boolean);
    procedure AutoSettingsChanged;
  public
    property AutoSettings: Boolean read FAutoSettings write SetAutoSettings;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, Dialogs, DAV_Common, DAV_VSTCustomModule, DAV_VSTPrograms,
  SplitHarmonizerGUI;

procedure TSplitHarmonizerModule.VSTModuleCreate(Sender: TObject);
begin
 FAutoSettings := True;
 InitialDelay := CInputDelay;
 FCriticalSection := TCriticalSection.Create;
end;

procedure TSplitHarmonizerModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TSplitHarmonizerModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 // initialize sound touch pitch shifter
 for Channel := 0 to Length(FSoundTouch) - 1 do
  begin
   FSoundTouch[Channel] := TSoundTouch.Create;
  end;

 // initialize delay lines
 for Channel := 0 to Length(FDelayLine) - 1 do
  begin
   FDelayLine[Channel, 0] := TDelayLineSamples32.Create;
   FDelayLine[Channel, 0].BufferSize := CInputDelay;
   FDelayLine[Channel, 1] := TDelayLineSamples32.Create;
   FDelayLine[Channel, 1].BufferSize := CInputDelay;
  end;

 for Channel := 0 to Length(FLowpassFilter) - 1 do
  begin
   FLowpassFilter[Channel] := TButterworthLowPassFilter.Create(1);
   FLowpassFilter[Channel].SampleRate := SampleRate;
  end;

 // initialize parameters
 Parameter[ 0] := 0;
 Parameter[ 1] := -20;
 Parameter[ 2] := 10;
 Parameter[ 3] := 0;
 Parameter[ 4] := +20;
 Parameter[ 5] := 10;
 Parameter[ 6] := 0;
 Parameter[ 7] := 1;
 Parameter[ 8] := 0;
 Parameter[ 9] := 1;
 Parameter[10] := 10;
 Parameter[11] := 10;
 Parameter[12] := 5;

 Programs[0].SetParameters(FParameter);
 with Programs[1] do SetParameters([0, -9, 13.5, 52.5,   9, 14.1, 50.0, 1, 0, 1, 10, 10, 5]);
 with Programs[2] do SetParameters([1, -6, 23.1, 52.5,   6, 21.6, 50.0, 1, 0, 1, 10, 10, 5]);
 with Programs[3] do SetParameters([0, 16, 48.2, 13.5, -13, 79.1, 63.5, 1, 0, 1, 10, 10, 5]);

 // set editor form class
 EditorFormClass := TFmSplitHarmonizer;
end;

procedure TSplitHarmonizerModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FSoundTouch) - 1
  do FreeAndNil(FSoundTouch[Channel]);

 for Channel := 0 to Length(FDelayLine) - 1 do
  begin
   FreeAndNil(FDelayLine[Channel, 0]);
   FreeAndNil(FDelayLine[Channel, 1]);
  end;

 for Channel := 0 to Length(FLowpassFilter) - 1
  do FreeAndNil(FLowpassFilter[Channel]);
end;

procedure TSplitHarmonizerModule.ParameterSemiTonesAChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if Assigned(FSoundTouch[0])
   then FSoundTouch[0].Pitch := Power(2, Value / 1200);
 finally
  FCriticalSection.Leave;
 end;

 if EditorForm is TFmSplitHarmonizer
  then TFmSplitHarmonizer(EditorForm).UpdateSemitones(0);
end;

procedure TSplitHarmonizerModule.ParameterSemiTonesBChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if Assigned(FSoundTouch[1])
   then FSoundTouch[1].Pitch := Power(2, Value / 1200);
 finally
  FCriticalSection.Leave;
 end;

 if EditorForm is TFmSplitHarmonizer
  then TFmSplitHarmonizer(EditorForm).UpdateSemitones(1);
end;

procedure TSplitHarmonizerModule.ParameterUseAntiAliasChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if Assigned(FSoundTouch[0])
   then FSoundTouch[0].UseAntiAliasFilter := Value > 0.5;
  if Assigned(FSoundTouch[1])
   then FSoundTouch[1].UseAntiAliasFilter := Value > 0.5;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSplitHarmonizerModule.ParameterUseQuickSeekChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to 1 do
   if Assigned(FSoundTouch[Channel])
    then FSoundTouch[Channel].UseQuickSeek := Value > 0.5;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSplitHarmonizerModule.SetAutoSettings(const Value: Boolean);
begin
 if FAutoSettings <> Value then
  begin
   FAutoSettings := Value;
   AutoSettingsChanged;
  end;
end;

procedure TSplitHarmonizerModule.ParameterLowpassAChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FLowpassFilter[0])
  then FLowpassFilter[0].Frequency := Value;

 // update GUI
 if EditorForm is TFmSplitHarmonizer
  then TFmSplitHarmonizer(EditorForm).UpdateLowpassFilter(0);
end;

procedure TSplitHarmonizerModule.ParameterLowpassBChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FLowpassFilter[1])
  then FLowpassFilter[1].Frequency := Value;

 // update GUI
 if EditorForm is TFmSplitHarmonizer
  then TFmSplitHarmonizer(EditorForm).UpdateLowpassFilter(1);
end;

procedure TSplitHarmonizerModule.AutoSettingsChanged;
var
  Channel : Integer;
begin
 if FAutoSettings then
  begin
   for Channel := 0 to 1 do
    if Assigned(FSoundTouch[Channel]) then
     begin
      FSoundTouch[Channel].OverlapMs := -1;
      FSoundTouch[Channel].SequenceMs := -1;
      FSoundTouch[Channel].SeekWindow := -1;
     end;
  end;
end;

procedure TSplitHarmonizerModule.ParameterAutoSettingsChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  AutoSettings := Value > 0.5;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSplitHarmonizerModule.ParameterOnOffDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] > 0.5
  then PreDefined := 'On'
  else PreDefined := 'Off';
end;

procedure TSplitHarmonizerModule.ParameterSequenceChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  if not FAutoSettings then
   for Channel := 0 to 1 do
    begin
     if Assigned(FSoundTouch[Channel])
      then FSoundTouch[Channel].SequenceMs := Round(Value);
     if Assigned(FDelayLine[Channel, 0])
      then FDelayLine[Channel, 0].BufferSize := Round(0.001 * Value * SampleRate);

    end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSplitHarmonizerModule.ParameterSeekWindowChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  if not FAutoSettings then
   for Channel := 0 to 1 do
    if Assigned(FSoundTouch[Channel])
     then FSoundTouch[Channel].SeekWindow := Round(Value);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSplitHarmonizerModule.ParameterOverlapChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  if not FAutoSettings then
   for Channel := 0 to 1 do
    if Assigned(FSoundTouch[Channel])
     then FSoundTouch[Channel].OverlapMs := Round(Value);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSplitHarmonizerModule.ParameterDelayAChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if Assigned(FDelayLine[0, 1])
   then FDelayLine[0, 1].BufferSize := Max(1, Round(Value * SampleRate * 1E-3));
 finally
  FCriticalSection.Leave;
 end;
 if EditorForm is TFmSplitHarmonizer
  then TFmSplitHarmonizer(EditorForm).UpdateDelay(0);
end;

procedure TSplitHarmonizerModule.ParameterDelayBChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if Assigned(FDelayLine[1, 1])
   then FDelayLine[1, 1].BufferSize := Max(1, Round(Value * SampleRate * 1E-3));
 finally
  FCriticalSection.Leave;
 end;
 if EditorForm is TFmSplitHarmonizer
  then TFmSplitHarmonizer(EditorForm).UpdateDelay(1);
end;

procedure TSplitHarmonizerModule.ParameterMixAChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FMix[0] := 0.01 * Value;

 // update GUI
 if EditorForm is TFmSplitHarmonizer
  then TFmSplitHarmonizer(EditorForm).UpdateMix(0);
end;

procedure TSplitHarmonizerModule.ParameterMixRightChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FMix[1] := 0.01 * Value;

 // update GUI
 if EditorForm is TFmSplitHarmonizer
  then TFmSplitHarmonizer(EditorForm).UpdateMix(1);
end;

procedure TSplitHarmonizerModule.ParameterEncodeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 case Round(Value) of
  0 : begin
       OnProcess := VSTModuleProcessLR;
       OnProcess64Replacing := VSTModuleProcessReplacing64LR;
      end;
  1 : begin
       OnProcess := VSTModuleProcessMS;
       OnProcess64Replacing := VSTModuleProcessReplacing64MS;
      end;
 end;
 OnProcess32Replacing := OnProcess;
end;

procedure TSplitHarmonizerModule.ParameterEncodeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case Round(Parameter[Index]) of
  0 : PreDefined := 'L/R';
  1 : PreDefined := 'M/S';
 end;
end;

procedure TSplitHarmonizerModule.VSTModuleProcessLR(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Channel : Integer;
  Sample  : Integer;
  Mix     : array [0..1, 0..1] of Single;
begin
 FCriticalSection.Enter;
 try
  for Sample := 0 to SampleFrames - 1 do
   begin
    Outputs[0, Sample] := FLowpassFilter[0].ProcessSample32(Inputs[0, Sample]);
    Outputs[1, Sample] := FLowpassFilter[1].ProcessSample32(Inputs[1, Sample]);
   end;

  for Channel := 0 to NumInputs - 1 do
   begin
    Mix[Channel, 0] := CHalf32 * (1 - CHalf32 * abs(FMix[Channel]) - FMix[Channel] + CHalf32);
    Mix[Channel, 1] := CHalf32 * (1 - CHalf32 * abs(FMix[Channel]) + FMix[Channel] + CHalf32);

    FSoundTouch[Channel].PutSamples(@Outputs[Channel, 0], SampleFrames);
    FSoundTouch[Channel].ReceiveSamples(@Outputs[Channel, 0], SampleFrames);
   end;

  for Sample := 0 to SampleFrames - 1 do
   begin
    Outputs[0, Sample] := Mix[0, 0] * FDelayLine[0, 0].ProcessSample32(Inputs[0, Sample]) + Mix[0, 1] * FDelayLine[0, 1].ProcessSample32(Outputs[0, Sample]);
    Outputs[1, Sample] := Mix[1, 0] * FDelayLine[1, 0].ProcessSample32(Inputs[1, Sample]) + Mix[1, 1] * FDelayLine[1, 1].ProcessSample32(Outputs[1, Sample]);
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSplitHarmonizerModule.VSTModuleProcessReplacing64LR(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  Channel : Integer;
  Sample  : Integer;
  Mix     : array [0..1, 0..1] of Double;
begin
 FCriticalSection.Enter;
 try
  for Sample := 0 to SampleFrames - 1 do
   begin
    Outputs[0, Sample] := FLowpassFilter[0].ProcessSample32(FDelayLine[0, 1].ProcessSample32(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample])));
    Outputs[1, Sample] := FLowpassFilter[1].ProcessSample32(FDelayLine[1, 1].ProcessSample32(CHalf32 * (Inputs[0, Sample] - Inputs[1, Sample])));
   end;

  for Channel := 0 to NumInputs - 1 do
   begin
    Mix[Channel, 0] := CHalf32 * (1 - CHalf32 * abs(FMix[Channel]) - FMix[Channel] + CHalf32);
    Mix[Channel, 1] := CHalf32 * (1 - CHalf32 * abs(FMix[Channel]) + FMix[Channel] + CHalf32);

    FSoundTouch[Channel].PutSamples(@Outputs[Channel, 0], SampleFrames);
    FSoundTouch[Channel].ReceiveSamples(@Outputs[Channel, 0], SampleFrames);
   end;

  for Sample := 0 to SampleFrames - 1 do
   begin
    Outputs[0, Sample] := Mix[0, 0] * FDelayLine[0, 0].ProcessSample32(Inputs[0, Sample]) + Mix[0, 1] * FDelayLine[0, 1].ProcessSample32(Outputs[0, Sample]);
    Outputs[1, Sample] := Mix[1, 0] * FDelayLine[1, 0].ProcessSample32(Inputs[1, Sample]) + Mix[1, 1] * FDelayLine[1, 1].ProcessSample32(Outputs[1, Sample]);
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSplitHarmonizerModule.VSTModuleProcessMS(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Channel : Integer;
  Sample  : Integer;
  Temp    : Single;
  Mix     : array [0..1, 0..1] of Single;
begin
 FCriticalSection.Enter;
 try
  for Sample := 0 to SampleFrames - 1 do
   begin
    Outputs[0, Sample] := FLowpassFilter[0].ProcessSample32(FDelayLine[0, 1].ProcessSample32(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample])));
    Outputs[1, Sample] := FLowpassFilter[1].ProcessSample32(FDelayLine[1, 1].ProcessSample32(CHalf32 * (Inputs[0, Sample] - Inputs[1, Sample])));
   end;

  for Channel := 0 to NumInputs - 1 do
   begin
    Mix[Channel, 0] := CHalf32 * (1 - CHalf32 * abs(FMix[Channel]) - FMix[Channel] + CHalf32);
    Mix[Channel, 1] := CHalf32 * (1 - CHalf32 * abs(FMix[Channel]) + FMix[Channel] + CHalf32);
    FSoundTouch[Channel].PutSamples(@Outputs[Channel, 0], SampleFrames);
    FSoundTouch[Channel].ReceiveSamples(@Outputs[Channel, 0], SampleFrames);
   end;

  for Sample := 0 to SampleFrames - 1 do
   begin
    Temp := Mix[0, 0] * FDelayLine[0, 0].ProcessSample32(Inputs[0, Sample]) + Mix[0, 1] * (Outputs[0, Sample] + Outputs[1, Sample]);
    Outputs[1, Sample] := Mix[1, 0] * FDelayLine[1, 0].ProcessSample32(Inputs[1, Sample]) + Mix[1, 1] * (Outputs[0, Sample] - Outputs[1, Sample]);
    Outputs[0, Sample] := Temp;
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSplitHarmonizerModule.VSTModuleProcessReplacing64MS(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  Channel : Integer;
  Sample  : Integer;
  Temp    : Double;
  Mix     : array [0..1, 0..1] of Double;
begin
 FCriticalSection.Enter;
 try
  for Sample := 0 to SampleFrames - 1 do
   begin
    Outputs[0, Sample] := FLowpassFilter[0].ProcessSample32(FDelayLine[0, 1].ProcessSample32(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample])));
    Outputs[1, Sample] := FLowpassFilter[1].ProcessSample32(FDelayLine[1, 1].ProcessSample32(CHalf32 * (Inputs[0, Sample] - Inputs[1, Sample])));
   end;

  for Channel := 0 to NumInputs - 1 do
   begin
    Mix[Channel, 0] := CHalf32 * (1 - CHalf32 * abs(FMix[Channel]) - FMix[Channel] + CHalf32);
    Mix[Channel, 1] := CHalf32 * (1 - CHalf32 * abs(FMix[Channel]) + FMix[Channel] + CHalf32);
    FSoundTouch[Channel].PutSamples(@Outputs[Channel, 0], SampleFrames);
    FSoundTouch[Channel].ReceiveSamples(@Outputs[Channel, 0], SampleFrames);
   end;

  for Sample := 0 to SampleFrames - 1 do
   begin
    Temp := Mix[0, 0] * FDelayLine[0, 0].ProcessSample32(Inputs[0, Sample]) + Mix[0, 1] * (Outputs[0, Sample] + Outputs[1, Sample]);
    Outputs[1, Sample] := Mix[1, 0] * FDelayLine[1, 0].ProcessSample32(Inputs[1, Sample]) + Mix[1, 1] * (Outputs[0, Sample] - Outputs[1, Sample]);
    Outputs[0, Sample] := Temp;
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSplitHarmonizerModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel : Integer;
begin
 if SampleRate = 0 then exit;
 FCriticalSection.Enter;
 try
  for Channel := 0 to NumInputs - 1 do
   begin
    if Assigned(FSoundTouch[Channel])
     then FSoundTouch[Channel].SampleRate := abs(SampleRate);
    if Assigned(FDelayLine[Channel, 0])
     then FDelayLine[Channel, 0].BufferSize := CInputDelay;
    if Assigned(FDelayLine[Channel, 1])
     then FDelayLine[Channel, 1].BufferSize := Max(1, Round(Parameter[2 + 4 * Channel] * 1E-3 * Abs(SampleRate)));
    if Assigned(FLowpassFilter[Channel])
     then FLowpassFilter[Channel].SampleRate := SampleRate;
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

end.
