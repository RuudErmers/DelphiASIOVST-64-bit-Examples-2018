unit BassBaronVST;

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
  Forms, SyncObjs, DAV_Types, DAV_VSTModule, DAV_DspBassBaron;

type
  TBassBaronModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleResume(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcess32(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcess64(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure ParameterFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterBassMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterHfLevelChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterResponseDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterResponseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterInputLevelChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterLevelDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterOutputChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFilterTypeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFilterTypeDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterLowcutLabel(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterLowcutDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterLowcutChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterAlgorithmChange(
      Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterMixDisplay(
      Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
  private
    FBassBaron       : array [0..1] of TBassBaron;
    FCriticalSection : TCriticalSection;
  public
  end;

implementation

uses
  Math, {$IFDEF HAS_UNIT_ANSISTRINGS} AnsiStrings, {$ENDIF} DAV_Common, 
  DAV_Approximations, BassBaronGUI;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TBassBaronModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TBassBaronModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TBassBaronModule.VSTModuleOpen(Sender: TObject);
var
  ChannelIndex : Integer;
begin
 // create & setup upward compressor
 for ChannelIndex := 0 to Length(FBassBaron) - 1 do
  begin
   FBassBaron[ChannelIndex] := TBassBaron.Create;
   FBassBaron[ChannelIndex].SampleRate := SampleRate;
   FBassBaron[ChannelIndex].Decay := 0.5 * dB_to_Amp(-15);
  end;

 Parameter[0] := 0;
 Parameter[1] := 80;
 Parameter[2] := 1;
 Parameter[3] := 20;
 Parameter[4] := 0;
 Parameter[5] := 10;
 Parameter[6] := 0;
 Parameter[7] := 50;
 Parameter[8] := 0;

 with Programs[0] do
  begin
   Parameter[0] := 0;
   Parameter[1] := 80;
   Parameter[2] := 1;
   Parameter[3] := 20;
   Parameter[4] := 0;
   Parameter[5] := 10;
   Parameter[6] := 0;
   Parameter[7] := 50;
   Parameter[8] := 0;
  end;
 with Programs[1] do
  begin
   Parameter[0] := 0;
   Parameter[1] := 80;
   Parameter[2] := 0;
   Parameter[3] := 20;
   Parameter[4] := 0;
   Parameter[5] := 10;
   Parameter[6] := 0;
   Parameter[7] := 50;
   Parameter[8] := 0;
  end;
 with Programs[2] do
  begin
   Parameter[0] := 0;
   Parameter[1] := 85;
   Parameter[2] := 0;
   Parameter[3] := 20;
   Parameter[4] := 0;
   Parameter[5] := 10;
   Parameter[6] := 0;
   Parameter[7] := 50;
   Parameter[8] := 0;
  end;
 with Programs[3] do
  begin
   Parameter[0] := 0;
   Parameter[1] := 85;
   Parameter[2] := 1;
   Parameter[3] := 20;
   Parameter[4] := 0;
   Parameter[5] := 10;
   Parameter[6] := 0;
   Parameter[7] := 50;
   Parameter[8] := 0;
  end;
 with Programs[4] do
  begin
   Parameter[0] := 0;
   Parameter[1] := 75;
   Parameter[2] := 0;
   Parameter[3] := 20;
   Parameter[4] := 0;
   Parameter[5] := 10;
   Parameter[6] := 0;
   Parameter[7] := 50;
   Parameter[8] := 0;
  end;

 // set editor form
 EditorFormClass := TFmBassBaron;
end;

procedure TBassBaronModule.VSTModuleClose(Sender: TObject);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to Length(FBassBaron) - 1
  do FreeAndNil(FBassBaron[ChannelIndex]);
end;

procedure TBassBaronModule.ParameterResponseDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
(*
 case Round(Parameter[Index]) of
  0 : PreDefined := 'Off';
  1 : PreDefined := 'On';
 end;
*)
end;

procedure TBassBaronModule.ParameterInputLevelChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FBassBaron) - 1 do
   if Assigned(FBassBaron[ChannelIndex])
    then FBassBaron[ChannelIndex].InputLevel := dB_to_Amp(Value);
 finally
  FCriticalSection.Release;
 end;

 // eventually update GUI
 if EditorForm is TFmBassBaron
  then TFmBassBaron(EditorForm).UpdateInputLevel;
end;

procedure TBassBaronModule.ParameterOutputChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FBassBaron) - 1 do
   if Assigned(FBassBaron[ChannelIndex])
    then FBassBaron[ChannelIndex].OutputLevel := dB_to_Amp(Value);
 finally
  FCriticalSection.Release;
 end;

 // eventually update GUI
 if EditorForm is TFmBassBaron
  then TFmBassBaron(EditorForm).UpdateOutputLevel;
end;

procedure TBassBaronModule.ParameterLevelDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(FloatToStrF(Parameter[Index], ffFixed, 3, 1));
end;

procedure TBassBaronModule.ParameterFilterTypeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
  Order        : Integer;
  FilterType   : TSplitFilter;
begin
 Order := 1 + Round(Parameter[Index]);
 if Order mod 2 = 1
  then FilterType := sfSimple
  else FilterType := sfLinkwitzRiley;

 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FBassBaron) - 1 do
   if Assigned(FBassBaron[ChannelIndex]) then
    begin
     FBassBaron[ChannelIndex].SplitOrder := Order;
     FBassBaron[ChannelIndex].SplitFilter := FilterType;
    end;
 finally
  FCriticalSection.Release;
 end;

 // eventually update GUI
 if EditorForm is TFmBassBaron
  then TFmBassBaron(EditorForm).UpdateFilterType;
end;

procedure TBassBaronModule.ParameterFilterTypeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case Round(Parameter[Index]) of
  0 : PreDefined := '6 dB/Oct (Simple)';
  1 : PreDefined := '12 dB/Oct (Linkwitz-Riley)';
  2 : PreDefined := '18 dB/Oct (Butterworth)';
  3 : PreDefined := '24 dB/Oct (Linkwitz-Riley)';
  4 : PreDefined := '30 dB/Oct (Butterworth)';
 end;
end;

procedure TBassBaronModule.ParameterLowcutLabel(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
  if Parameter[Index] < 5 then PreDefined := '';
end;

procedure TBassBaronModule.ParameterLowcutDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
  if Parameter[Index] < 5 then PreDefined := 'Disabled';
end;

procedure TBassBaronModule.ParameterLowcutChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FBassBaron) - 1 do
   if Assigned(FBassBaron[ChannelIndex])
    then FBassBaron[ChannelIndex].LowcutFrequency := Value;
 finally
  FCriticalSection.Release;
 end;

 // eventually update GUI
 if EditorForm is TFmBassBaron
  then TFmBassBaron(EditorForm).UpdateLowCut;
end;

procedure TBassBaronModule.ParameterResponseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FBassBaron) - 1 do
   if Assigned(FBassBaron[ChannelIndex])
    then FBassBaron[ChannelIndex].Response := Value;
 finally
  FCriticalSection.Release;
 end;

 // eventually update GUI
 if EditorForm is TFmBassBaron
  then TFmBassBaron(EditorForm).UpdateResponse;
end;

procedure TBassBaronModule.ParameterHfLevelChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
  LevelFactor  : Single;
begin
 LevelFactor := dB_to_Amp(Value);

 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FBassBaron) - 1 do
   if Assigned(FBassBaron[ChannelIndex])
    then FBassBaron[ChannelIndex].HighFrequencyLevel := LevelFactor;
 finally
  FCriticalSection.Release;
 end;

 // eventually update GUI
 if EditorForm is TFmBassBaron
  then TFmBassBaron(EditorForm).UpdateHFLevel;
end;

procedure TBassBaronModule.ParameterMixDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(FloatToStrF(Parameter[Index], ffFixed, 3, 1));
end;

procedure TBassBaronModule.ParameterAlgorithmChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
  Algorithm    : TBassBaronAlgorithm;
begin
 Algorithm := TBassBaronAlgorithm(Round(Value));

 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FBassBaron) - 1 do
   if Assigned(FBassBaron[ChannelIndex])
    then FBassBaron[ChannelIndex].Algorithm := Algorithm;
 finally
  FCriticalSection.Release;
 end;

 // eventually update GUI
 if EditorForm is TFmBassBaron
  then TFmBassBaron(EditorForm).UpdateAlgorithm;
end;

procedure TBassBaronModule.ParameterBassMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FBassBaron) - 1 do
   if Assigned(FBassBaron[ChannelIndex]) then
    begin
     FBassBaron[ChannelIndex].OriginalBassLevel := 1 - 0.01 * Value;
     FBassBaron[ChannelIndex].HarmonicBassLevel := 0.01 * Value;
    end;
 finally
  FCriticalSection.Release;
 end;

 // eventually update GUI
 if EditorForm is TFmBassBaron
  then TFmBassBaron(EditorForm).UpdateBassMix;
end;

procedure TBassBaronModule.ParameterFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FBassBaron) - 1 do
   if Assigned(FBassBaron[ChannelIndex])
    then FBassBaron[ChannelIndex].Frequency := Value;
 finally
  FCriticalSection.Release;
 end;

 // eventually update GUI
 if EditorForm is TFmBassBaron
  then TFmBassBaron(EditorForm).UpdateFrequency;
end;

procedure TBassBaronModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  if Abs(SampleRate) > 0 then
   for ChannelIndex := 0 to Length(FBassBaron) - 1 do
    if Assigned(FBassBaron[ChannelIndex])
     then FBassBaron[ChannelIndex].SampleRate := Abs(SampleRate);
 finally
  FCriticalSection.Release;
 end;
end;

procedure TBassBaronModule.VSTModuleResume(Sender: TObject);
var
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FBassBaron) - 1 do
   if Assigned(FBassBaron[ChannelIndex])
    then FBassBaron[ChannelIndex].ResetStates;
 finally
  FCriticalSection.Release;
 end;
end;

procedure TBassBaronModule.VSTModuleProcess32(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  ChannelIndex   : Integer;
  SampleIndex    : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FBassBaron) - 1 do
   if Assigned(FBassBaron[ChannelIndex]) then
    for SampleIndex := 0 to Min(BlockSize, SampleFrames) - 1
     do Outputs[ChannelIndex, SampleIndex] := FBassBaron[ChannelIndex].ProcessSample32(
       Inputs[ChannelIndex, SampleIndex]);
 finally
  FCriticalSection.Release;
 end;
end;

procedure TBassBaronModule.VSTModuleProcess64(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  ChannelIndex : Integer;
  SampleIndex  : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FBassBaron) - 1 do
   if Assigned(FBassBaron[ChannelIndex]) then
    for SampleIndex := 0 to Min(BlockSize, SampleFrames) - 1
     do Outputs[ChannelIndex, SampleIndex] := FBassBaron[ChannelIndex].ProcessSample64(
       Inputs[ChannelIndex, SampleIndex]);
 finally
  FCriticalSection.Release;
 end;
end;

end.
