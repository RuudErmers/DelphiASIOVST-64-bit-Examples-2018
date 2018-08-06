unit ChunkDemoDM;

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
  Forms, DAV_Types, DAV_VSTModule, DAV_DspFilterLinkwitzRiley, DAV_DspDynamics,
  DAV_DspGranularPitchshifter, DAV_DspLightweightDynamics,
  DAV_DspFilterButterworth;

const
  CNumChannels = 2;

type
  TChunkDemoDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    function VSTModuleGetChunkParameter(Sender: TObject; const Index: Integer): Single;
    procedure ParameterAlphaChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterBetaChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterGammaChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDeltaChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FPitchShifter : array [0..CNumChannels - 1] of TDspGranularPitchShifter32;
    FCrossover    : array [0..CNumChannels - 1] of TLinkwitzRiley;
    FAliasFilter  : array [0..CNumChannels - 1] of TButterworthLowPassFilter;
    FHighpass     : array [0..CNumChannels - 1] of TButterworthHighPassFilter;
    FState        : array [0..CNumChannels - 1] of Single;
    FMix          : array [0..4] of Single;
    FCompressor   : TLightweightSoftKneeCompressor;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  ChunkDemoGUI, DAV_VSTCustomModule;

procedure TChunkDemoDataModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 FCompressor := TLightweightSoftKneeCompressor.Create;
 FCompressor.Attack := 5;
 FCompressor.Release := 50;
 for Channel := 0 to CNumChannels - 1 do
  begin
   FPitchShifter[Channel] := TDspGranularPitchShifter32.Create;
   FCrossover[Channel]    := TLinkwitzRiley.Create;
   FAliasFilter[Channel]  := TButterworthLowPassFilter.Create;
   FHighpass[Channel]     := TButterworthHighPassFilter.Create;

   // initial settings
   FAliasFilter[Channel].Order := 3;
   FAliasFilter[Channel].Frequency := 0.38 * SampleRate;
   FHighpass[Channel].Order := 1;
   FHighpass[Channel].Frequency := 20;
   FCrossover[Channel].Order := 1;
   FPitchShifter[Channel].Semitones := 5;
   FPitchShifter[Channel].Granularity := 0.15;
   FPitchShifter[Channel].Stages := 2;
  end;

 // initialize default parameters
 Parameter[0] := 50;
 Parameter[1] := 50;
 Parameter[2] := 50;
 Parameter[3] := 50;

 with Programs[0] do
  begin
   Parameter[0] := 50;
   Parameter[1] := 50;
   Parameter[2] := 50;
   Parameter[3] := 50;
  end;
 with Programs[0] do
  begin
   Parameter[0] := 40;
   Parameter[1] := 60;
   Parameter[2] := 40;
   Parameter[3] := 60;
  end;

 // set editor form class
 EditorFormClass := TFmChunkDemo;
end;

procedure TChunkDemoDataModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 FreeAndNil(FCompressor);
 for Channel := 0 to CNumChannels - 1 do
  begin
   FreeAndNil(FPitchShifter[Channel]);
   FreeAndNil(FCrossover[Channel]);
   FreeAndNil(FAliasFilter[Channel]);
  end;
end;

procedure TChunkDemoDataModule.ParameterBetaChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCompressor) then
  with FCompressor do
   begin
    Ratio        := 2 + sqr(0.01 * Value) * 98;
    Knee_dB      := 0.1 * Value;
    Threshold_dB := -20 + 0.1 * Value;
   end;

 Chunk.Position := Index * SizeOf(Single);
 Chunk.Write(Value, SizeOf(Single));

 if EditorForm is TFmChunkDemo then
  with TFmChunkDemo(EditorForm)
   do UpdateBeta;
end;

procedure TChunkDemoDataModule.ParameterGammaChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FMix[2] := 0.01 * Value;
 FMix[3] := 1 - FMix[2];
 if Assigned(FCompressor)
  then FCompressor.MakeUpGain_dB := 0.2 * Value;

 Chunk.Position := Index * SizeOf(Single);
 Chunk.Write(Value, SizeOf(Single));

 if EditorForm is TFmChunkDemo then
  with TFmChunkDemo(EditorForm)
   do UpdateDelta;
end;

procedure TChunkDemoDataModule.ParameterDeltaChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FMix[0] := 0.1 + 0.009 * Value;
 FMix[1] := 1 - FMix[0];

 Chunk.Position := Index * SizeOf(Single);
 Chunk.Write(Value, SizeOf(Single));

 if EditorForm is TFmChunkDemo then
  with TFmChunkDemo(EditorForm)
   do UpdateGamma;
end;

procedure TChunkDemoDataModule.ParameterAlphaChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to CNumChannels - 1 do
  if Assigned(FCrossover[Channel])
   then FCrossover[Channel].Frequency := sqr(sqr(0.01 * Value)) * 20000;

 Chunk.Position := Index * SizeOf(Single);
 Chunk.Write(Value, SizeOf(Single));

 if EditorForm is TFmChunkDemo then
  with TFmChunkDemo(EditorForm)
   do UpdateAlpha;
end;

function TChunkDemoDataModule.VSTModuleGetChunkParameter(Sender: TObject;
  const Index: Integer): Single;
begin
 Chunk.Position := Index * SizeOf(Single);
 Chunk.Read(Result, SizeOf(Single));
end;

procedure TChunkDemoDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 //
end;

procedure TChunkDemoDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample    : Integer;
  Low, High : array [0..1] of Single;
begin
 Assert(CNumChannels = 2);
 for Sample := 0 to SampleFrames - 1 do
  begin
   FCrossover[0].ProcessSample32(Inputs[0, Sample], Low[0], High[0]);
   FCrossover[1].ProcessSample32(Inputs[1, Sample], Low[1], High[1]);
   FState[0] := FPitchShifter[0].ProcessSample32(FAliasFilter[0].ProcessSample64(FState[0] + High[0]));
   FState[1] := FPitchShifter[1].ProcessSample32(FAliasFilter[1].ProcessSample64(FState[1] + High[1]));

   Outputs[0, Sample] := Low[0] + FMix[0] * High[0] + FMix[1] * FState[1];
   Outputs[1, Sample] := Low[1] + FMix[0] * High[1] + FMix[1] * FState[0];

   // compress delayed signal
   FCompressor.InputSample(FMix[2] * (Inputs[0, Sample] + Outputs[1, Sample]) +
                           FMix[3] * (FState[1] + FState[0]));
   FState[0] := FHighpass[0].ProcessSample64(FCompressor.GainSample(FState[0]));
   FState[1] := FHighpass[1].ProcessSample64(FCompressor.GainSample(FState[1]));
  end;
end;

procedure TChunkDemoDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel : Integer;
begin
 FCompressor.SampleRate := SampleRate;
 for Channel := 0 to CNumChannels - 1 do
  begin
   FPitchShifter[Channel].SampleRate := SampleRate;
   FCrossover[Channel].SampleRate := SampleRate;
   FAliasFilter[Channel].SampleRate := SampleRate;
   FAliasFilter[Channel].Frequency := 0.38 * SampleRate;
  end;
end;

end.
