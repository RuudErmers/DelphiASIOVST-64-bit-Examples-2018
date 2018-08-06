unit MaxxBassCloneDM;

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
  Forms, SyncObjs, DAV_Types, DAV_VSTModule, DAV_DspPsychoAcousticBassEnhancer;

type
  THarmonicBassModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure ParameterFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterHighpassSelectChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOriginalBassChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterMaxxbassChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterListenChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterResponseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDecayChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterInputChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterdBDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterListenDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterHighpassDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure VSTModuleResume(Sender: TObject);
  private
    FHarmonicBass    : array [0..1] of TCustomHarmonicBass;
    FCriticalSection : TCriticalSection;
    procedure CalculateGains;
  public
  end;

implementation

uses
  Math, DAV_Common, DAV_Approximations, MaxxBassCloneGUI;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure THarmonicBassModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure THarmonicBassModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure THarmonicBassModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 // create & setup upward compressor
 for Channel := 0 to Length(FHarmonicBass) - 1 do
  begin
   FHarmonicBass[Channel] := TCustomHarmonicBass.Create;
   FHarmonicBass[Channel].SampleRate := SampleRate;
  end;

 Parameter[0] := 80;
 Parameter[1] := 1;
 Parameter[2] := 20;
 Parameter[3] := 0;
 Parameter[4] := -15;
 Parameter[5] := 1;
 Parameter[6] := 1;
 Parameter[7] := 0;
 Parameter[8] := 0;

 with Programs[0] do
  begin
   Parameter[0] := 80;
   Parameter[1] := 1;
   Parameter[2] := 20;
   Parameter[3] := 0;
   Parameter[4] := -15;
   Parameter[5] := 1;
   Parameter[6] := 1;
   Parameter[7] := 0;
   Parameter[8] := 0;
  end;
 with Programs[1] do
  begin
   Parameter[0] := 80;
   Parameter[1] := 1.5;
   Parameter[2] := 15;
   Parameter[3] := 1;
   Parameter[4] := -15;
   Parameter[5] := 1;
   Parameter[6] := 1;
   Parameter[7] := dB_to_Amp(-10);
   Parameter[8] := 0;
  end;
 with Programs[2] do
  begin
   Parameter[0] := 85;
   Parameter[1] := 1.5;
   Parameter[2] := 15;
   Parameter[3] := 0;
   Parameter[4] := -15;
   Parameter[5] := 1;
   Parameter[6] := 1;
   Parameter[7] := dB_to_Amp(-12);
   Parameter[8] := 0;
  end;
 with Programs[3] do
  begin
   Parameter[0] := 85;
   Parameter[1] := 1.8;
   Parameter[2] := 15;
   Parameter[3] := 0;
   Parameter[4] := -10;
   Parameter[5] := 1;
   Parameter[6] := 1;
   Parameter[7] := dB_to_Amp(-10);
   Parameter[8] := 0;
  end;
 with Programs[4] do
  begin
   Parameter[0] := 75;
   Parameter[1] := 1.8;
   Parameter[2] := 10;
   Parameter[3] := 0;
   Parameter[4] := -10;
   Parameter[5] := 1;
   Parameter[6] := dB_to_Amp(-4);
   Parameter[7] := 1;
   Parameter[8] := 0;
  end;

 // set editor form class
 EditorFormClass := TFmHarmonicBassClone;
end;

procedure THarmonicBassModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FHarmonicBass) - 1
  do FreeAndNil(FHarmonicBass[Channel]);
end;

procedure THarmonicBassModule.ParameterHighpassDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case Round(Parameter[Index]) of
  0 : PreDefined := '16 Hz DC Filter';
  1 : PreDefined := '12 dB/oct';
  2 : PreDefined := '24 dB/oct';
 end;
end;

procedure THarmonicBassModule.ParameterListenDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case Round(Parameter[Index]) of
  0 : PreDefined := 'Audio';
  1 : PreDefined := 'Original Bass';
  2 : PreDefined := 'HarmonicBass';
 end;
end;

procedure THarmonicBassModule.ParameterdBDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] = 0
  then PreDefined := '-oo'
  else PreDefined := AnsiString(FloatToStrF(RoundTo(Amp_to_dB(Parameter[Index]), -2), ffGeneral, 3, 3));
end;

procedure THarmonicBassModule.ParameterHighpassSelectChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FHarmonicBass) - 1 do
   if Assigned(FHarmonicBass[Channel])
    then FHarmonicBass[Channel].HighpassSelect := THighpassSelect(Round(Value));
 finally
  FCriticalSection.Release;
 end;
end;

procedure THarmonicBassModule.ParameterInputChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FHarmonicBass) - 1 do
   if Assigned(FHarmonicBass[Channel])
    then FHarmonicBass[Channel].InputLevel := Value;
 finally
  FCriticalSection.Release;
 end;
end;

procedure THarmonicBassModule.ParameterDecayChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FHarmonicBass) - 1 do
   if Assigned(FHarmonicBass[Channel])
    then FHarmonicBass[Channel].Decay := 0.5 * dB_to_Amp(Value);
 finally
  FCriticalSection.Release;
 end;
end;

procedure THarmonicBassModule.ParameterResponseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
const
  CScale : Single = 0.36787945032;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FHarmonicBass) - 1 do
   if Assigned(FHarmonicBass[Channel])
    then FHarmonicBass[Channel].Response := Value * CScale;
 finally
  FCriticalSection.Release;
 end;
end;

procedure THarmonicBassModule.ParameterRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FHarmonicBass) - 1 do
   if Assigned(FHarmonicBass[Channel])
    then FHarmonicBass[Channel].Ratio := Value;
 finally
  FCriticalSection.Release;
 end;
end;

procedure THarmonicBassModule.ParameterOriginalBassChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 CalculateGains;
end;

procedure THarmonicBassModule.ParameterMaxxbassChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 CalculateGains;
end;

procedure THarmonicBassModule.ParameterListenChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 CalculateGains;
end;

procedure THarmonicBassModule.CalculateGains;
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FHarmonicBass) - 1 do
   if Assigned(FHarmonicBass[Channel]) then
    case Round(Parameter[8]) of
     0 : begin
          FHarmonicBass[Channel].OriginalBassLevel := Parameter[6];
          FHarmonicBass[Channel].HarmonicBassLevel := Parameter[7];
          FHarmonicBass[Channel].HighFrequencyLevel := 1;
         end;
     1 : begin
          FHarmonicBass[Channel].OriginalBassLevel := 1;
          FHarmonicBass[Channel].HarmonicBassLevel := 0;
          FHarmonicBass[Channel].HighFrequencyLevel := 0;
         end;
     2 : begin
          FHarmonicBass[Channel].OriginalBassLevel := 0;
          FHarmonicBass[Channel].HarmonicBassLevel := 1;
          FHarmonicBass[Channel].HighFrequencyLevel := 0;
         end;
    end;
 finally
  FCriticalSection.Release;
 end;
end;

procedure THarmonicBassModule.ParameterFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FHarmonicBass) - 1 do
   if Assigned(FHarmonicBass[Channel])
    then FHarmonicBass[Channel].Frequency := Value;
 finally
  FCriticalSection.Release;
 end;
end;

procedure THarmonicBassModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  if Abs(SampleRate) > 0 then
   for Channel := 0 to Length(FHarmonicBass) - 1 do
    if Assigned(FHarmonicBass[Channel])
     then FHarmonicBass[Channel].SampleRate := Abs(SampleRate);
 finally
  FCriticalSection.Release;
 end;
end;

procedure THarmonicBassModule.VSTModuleResume(Sender: TObject);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FHarmonicBass) - 1
   do FHarmonicBass[Channel].ResetStates;
 finally
  FCriticalSection.Release;
 end;
end;

procedure THarmonicBassModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Channel : Integer;
  Sample  : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FHarmonicBass) - 1 do
   for Sample := 0 to Min(FBlockSize, SampleFrames) - 1 do
    if IsNaN(Inputs[Channel, Sample])
     then Outputs[Channel, Sample] := FHarmonicBass[Channel].ProcessSample32(0)
     else Outputs[Channel, Sample] := FHarmonicBass[Channel].ProcessSample32(Inputs[Channel, Sample]);
 finally
  FCriticalSection.Release;
 end;
end;

procedure THarmonicBassModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  Channel : Integer;
  Sample  : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FHarmonicBass) - 1 do
   for Sample := 0 to Min(FBlockSize, SampleFrames) - 1
    do Outputs[Channel, Sample] := FHarmonicBass[Channel].ProcessSample32(Inputs[Channel, Sample]);
 finally
  FCriticalSection.Release;
 end;
end;

end.
