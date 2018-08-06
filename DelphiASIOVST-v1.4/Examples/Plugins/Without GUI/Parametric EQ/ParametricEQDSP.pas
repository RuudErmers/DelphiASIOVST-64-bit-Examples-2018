unit ParametricEQDSP;

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
  {$IFDEF FPC}LCLIntf, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, Forms,
  SyncObjs, DAV_Types, DAV_VSTModule, DAV_DspFilter, DAV_DspFilterBasics,
  DAV_DspPolyphaseUpsampler, DAV_DspPolyphaseDownsampler;

type
  TParametricEQDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure ParameterGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterBandwidthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterInputChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOutputChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterTypeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterTypeDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
  private
    FCriticalSection : TCriticalSection;
    FFilters         : array [0..1, 0..7] of TCustomBandwidthIIRFilter;
    FUpSampler       : array [0..1] of TPolyphaseUpsampler64;
    FDownSampler     : array [0..1] of TPolyphaseDownSampler64;
    FPeaks           : array [0..1, 0..1] of Single;
    FGains           : array [0..1] of Single;
    procedure SetFilterClass(Index: Integer; const Value: TBandwidthIIRFilterClass);
    function GetFilterClass(Index: Integer): TBandwidthIIRFilterClass;
    function GetDownSampler(Index: Integer): TPolyphaseDownSampler64;
    function GetUpSampler(Index: Integer): TPolyphaseUpSampler64;
    function GetInputPeakLevel: Single;
    function GetOutputPeakLevel: Single;
    function GetFilter(Index: Integer): TCustomIIRFilter;
  public
    property FilterClass[Index: Integer]: TBandwidthIIRFilterClass read GetFilterClass write SetFilterClass;
    property Filter[Index: Integer]: TCustomIIRFilter read GetFilter;
    property DownSampler[Index: Integer]: TPolyphaseDownSampler64 read GetDownSampler;
    property UpSampler[Index: Integer]: TPolyphaseUpSampler64 read GetUpSampler;
    property InputPeakLevel: Single read GetInputPeakLevel;
    property OutputPeakLevel: Single read GetOutputPeakLevel;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  DAV_Common, DAV_Approximations, DAV_VSTModuleWithPrograms;

resourcestring
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';

procedure TParametricEQDataModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TParametricEQDataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TParametricEQDataModule.VSTModuleOpen(Sender: TObject);
var
  Channel, Band : Integer;
begin
 FGains[0] := 0.01125;
 FGains[1] := 100;
 for Channel := 0 to Length(FFilters) - 1 do
  begin
   FUpSampler[Channel] := TPolyphaseUpsampler64.Create;
   FUpSampler[Channel].Transition := 0.499;
   FUpSampler[Channel].NumberOfCoefficients := 6;
   FDownSampler[Channel] := TPolyphaseDownSampler64.Create;
   FDownSampler[Channel].Transition := 0.499;
   FDownSampler[Channel].NumberOfCoefficients := 6;
   for Band := 0 to Length(FFilters[Channel]) - 1 do
    begin
     FFilters[Channel, Band] := TBasicGainFilter.Create;
     FFilters[Channel, Band].SampleRate := SampleRate;
     FFilters[Channel, Band].Frequency := 1000;
     FFilters[Channel, Band].Gain := 0;
    end;
  end;

 // Initial Parameters
 Parameter[ 0] := 0;
 Parameter[ 1] := 60;
 Parameter[ 2] := 2;
 Parameter[ 3] := 0;
 Parameter[ 4] := 2;
 Parameter[ 5] := 180;
 Parameter[ 6] := 2;
 Parameter[ 7] := 0;
 Parameter[ 8] := 1;
 Parameter[ 9] := 350;
 Parameter[10] := 2;
 Parameter[11] := 0;
 Parameter[12] := 1;
 Parameter[13] := 700;
 Parameter[14] := 2;
 Parameter[15] := 0;
 Parameter[16] := 1;
 Parameter[17] := 1800;
 Parameter[18] := 2;
 Parameter[19] := 0;
 Parameter[20] := 1;
 Parameter[21] := 4000;
 Parameter[22] := 2;
 Parameter[23] := 0;
 Parameter[24] := 1;
 Parameter[25] := 8000;
 Parameter[26] := 2;
 Parameter[27] := 0;
 Parameter[28] := 1;
 Parameter[29] := 12000;
 Parameter[30] := 2;
 Parameter[31] := 0;
 Parameter[32] := 3;
 Parameter[33] := 0;
 SetProgramParameters(0, FParameter);
 SetProgramParameters(1, FParameter);
 SetProgramParameters(2, FParameter);
 SetProgramParameters(3, FParameter);
end;

procedure TParametricEQDataModule.VSTModuleClose(Sender: TObject);
var
  Channel, Band : Integer;
begin
 for Channel := 0 to Length(FFilters) - 1 do
  for Band := 0 to Length(FFilters[Channel]) - 1
   do FreeAndNil(FFilters[Channel, Band]);
end;

procedure TParametricEQDataModule.ParameterTypeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case Round(Parameter[Index]) of
   0 : PreDefined := 'Bypass';
   1 : PreDefined := 'Peak';
   2 : PreDefined := 'LoShlf';
   3 : PreDefined := 'HiShlf';
   4 : PreDefined := 'LoPass';
   5 : PreDefined := 'HiPass';
   6 : PreDefined := 'Bndpss';
   7 : PreDefined := 'Notch';
   8 : PreDefined := 'Allpass';
   9 : PreDefined := 'LoSlfA';
  10 : PreDefined := 'LoSlfB';
  11 : PreDefined := 'HiSlfA';
  12 : PreDefined := 'HiSlfB';
 end;
end;

procedure TParametricEQDataModule.SetFilterClass(Index: Integer;
  const Value: TBandwidthIIRFilterClass);
var
  OldFilter : TCustomIIRFilter;
  Channel   : Integer;
begin
 if (Index >= 0) and (Index < Length(FFilters[0])) then
  for Channel := 0 to Length(FFilters) - 1 do
   if Assigned(FFilters[Channel, Index]) then
    if TBandwidthIIRFilterClass(FFilters[Channel, Index].ClassType) <> Value then
     begin
      OldFilter := FFilters[Channel, Index];
      FFilters[Channel, Index] := Value.Create;
      FFilters[Channel, Index].Assign(OldFilter);
      if Assigned(OldFilter) then FreeAndNil(OldFilter);
     end else
   else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TParametricEQDataModule.GetDownSampler(
  Index: Integer): TPolyphaseDownSampler64;
begin
 if (Index >= 0) and (Index < Length(FFilters))
  then Result := FDownSampler[0]
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TParametricEQDataModule.GetFilter(Index: Integer): TCustomIIRFilter;
begin
 if (Index >= 0) and (Index < Length(FFilters[0]))
  then Result := FFilters[0, Index]
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TParametricEQDataModule.GetFilterClass(
  Index: Integer): TBandwidthIIRFilterClass;
begin
 if (Index >= 0) and (Index < Length(FFilters[0]))
  then Result := TBandwidthIIRFilterClass(FFilters[0, Index].ClassType)
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TParametricEQDataModule.GetInputPeakLevel: Single;
const
  CdBFactor : Single = 6.020487696;
begin
 if FPeaks[0, 0] > FPeaks[1, 0]
  then Result := FPeaks[0, 0]
  else Result := FPeaks[1, 0];
 Result := CdBFactor * FastLog2ContinousError5(Result);
end;

function TParametricEQDataModule.GetOutputPeakLevel: Single;
const
  CdBFactor : Single = 6.020487696;
begin
 if FPeaks[0, 1] > FPeaks[1, 1]
  then Result := FPeaks[0, 1]
  else Result := FPeaks[1, 1];
 Result := CdBFactor * FastLog2ContinousError5(Result);
end;

function TParametricEQDataModule.GetUpSampler(
  Index: Integer): TPolyphaseUpSampler64;
begin
 if (Index >= 0) and (Index < Length(FFilters))
  then Result := FUpSampler[0]
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TParametricEQDataModule.ParameterInputChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  FGains[0] := 0.01125 * dB_to_Amp(Value);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TParametricEQDataModule.ParameterOutputChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  FGains[1] := 100 * dB_to_Amp(Value);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TParametricEQDataModule.ParameterTypeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band: Integer;
begin
 // determine band based on parameter index
 Band := (Index - 1) div 4;

 FCriticalSection.Enter;
 try
  if Assigned(FFilters[0, Band]) then
   case Round(Value) of
     0 : FilterClass[Band] := TBasicGainFilter;
     1 : FilterClass[Band] := TBasicPeakFilter;
     2 : FilterClass[Band] := TBasicLowShelfFilter;
     3 : FilterClass[Band] := TBasicHighShelfFilter;
     4 : FilterClass[Band] := TBasicLowpassFilter;
     5 : FilterClass[Band] := TBasicHighpassFilter;
     6 : FilterClass[Band] := TBasicBandpassFilter;
     7 : FilterClass[Band] := TBasicNotchFilter;
     8 : FilterClass[Band] := TBasicAllpassFilter;
     9 : FilterClass[Band] := TBasicLowShelfAFilter;
    10 : FilterClass[Band] := TBasicLowShelfBFilter;
    11 : FilterClass[Band] := TBasicHighShelfAFilter;
    12 : FilterClass[Band] := TBasicHighShelfBFilter;
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TParametricEQDataModule.ParameterGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band: Integer;
begin
 // determine band based on parameter index
 Band := (Index - 1) div 4;

 FCriticalSection.Enter;
 try
  if Assigned(FFilters[0, Band]) then FFilters[0, Band].Gain := Value;
  if Assigned(FFilters[1, Band]) then FFilters[1, Band].Gain := Value;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TParametricEQDataModule.ParameterFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band: Integer;
begin
 // determine band based on parameter index
 Band := (Index - 1) div 4;

 FCriticalSection.Enter;
 try
  if Assigned(FFilters[0, Band]) then FFilters[0, Band].Frequency := 0.5 * Value;
  if Assigned(FFilters[1, Band]) then FFilters[1, Band].Frequency := 0.5 * Value;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TParametricEQDataModule.ParameterBandwidthChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band: Integer;
begin
 // determine band based on parameter index
 Band := (Index - 1) div 4;

 FCriticalSection.Enter;
 try
  if Assigned(FFilters[0, Band]) then FFilters[0, Band].BandWidth := Value;
  if Assigned(FFilters[1, Band]) then FFilters[1, Band].BandWidth := Value;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TParametricEQDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel, Band : Integer;
  Transition    : Single;
begin
 FCriticalSection.Enter;
 try
  if Abs(SampleRate) > 0 then
   for Channel := 0 to Length(FFilters) - 1 do
    begin
     // calculate up/downsampler transition bandwidth
     if 20000 / Abs(SampleRate) < 0.5
      then Transition := 20000 / Abs(SampleRate)
      else Transition := 0.499;

     if Assigned(FUpSampler[Channel]) then FUpSampler[Channel].Transition := Transition;
     if Assigned(FDownSampler[Channel]) then FDownSampler[Channel].Transition := Transition;

     for Band := 0 to Length(FFilters[Channel]) - 1 do
      if Assigned(FFilters[Channel, Band])
       then FFilters[Channel, Band].SampleRate := Abs(SampleRate);
    end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TParametricEQDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample  : Integer;
  Channel : Integer;
  Band    : Integer;
  Temp    : TDAV2DoubleArray;
const
  CPeakRelease : Single = 0.99999;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FFilters) - 1 do
   for Sample := 0 to SampleFrames - 1 do
    begin
     // Peak Meter (Input)
     Temp[0] := CHalf32 * (abs(Inputs[Channel, Sample]) - FPeaks[Channel, 0]);
     FPeaks[Channel, 0] := CPeakRelease * (FPeaks[Channel, 0] + Temp[0] + abs(Temp[0]));

     FUpSampler[Channel].ProcessSample(FGains[0] * Inputs[Channel, Sample] + CDenorm64, Temp);
     for Band := 0 to Length(FFilters[Channel]) - 1 do
      with FFilters[Channel, Band] do
       begin
        Temp[0] := ProcessSample64(Temp[0]);
        Temp[1] := ProcessSample64(Temp[1]);
       end;
     {$IFDEF PUREPASCAL}
     Temp[0] := FastTanhOpt5Term(Temp[0]);
     Temp[1] := FastTanhOpt5Term(Temp[1]);
     {$ELSE}
     Temp[0] := FastTanhOpt5TermFPU(Temp[0]);
     Temp[1] := FastTanhOpt5TermFPU(Temp[1]);
     {$ENDIF}
     Outputs[Channel, Sample] := FGains[1] * FDownSampler[Channel].ProcessSample(Temp);

     // Peak Meter (Output)
     Temp[1] := CHalf32 * (abs(Outputs[Channel, Sample]) - FPeaks[Channel, 1]);
     FPeaks[Channel, 1] := CPeakRelease * (FPeaks[Channel, 1] + Temp[1] + abs(Temp[1]));
    end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TParametricEQDataModule.VSTModuleProcessDoubleReplacing(
  const Inputs, Outputs: TDAVArrayOfDoubleFixedArray;
  const SampleFrames: Cardinal);
var
  Sample  : Integer;
  Channel : Integer;
  Band    : Integer;
  Temp    : TDAV2DoubleArray;
const
  CPeakRelease : Single = 0.99999;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FFilters) - 1 do
   for Sample := 0 to SampleFrames - 1 do
    begin
     // Peak Meter (Input)
     Temp[0] := CHalf32 * (abs(Inputs[Channel, Sample]) - FPeaks[Channel, 0]);
     FPeaks[Channel, 0] := CPeakRelease * (FPeaks[Channel, 0] + Temp[0] + abs(Temp[0]));

     FUpSampler[Channel].ProcessSample(FGains[0] * Inputs[Channel, Sample] + CDenorm64, Temp);
     for Band := 0 to Length(FFilters[Channel]) - 1 do
      with FFilters[Channel, Band] do
       begin
        Temp[0] := ProcessSample64(Temp[0]);
        Temp[1] := ProcessSample64(Temp[1]);
       end;
     {$IFDEF PUREPASCAL}
     Temp[0] := FastTanhOpt5Term(Temp[0]);
     Temp[1] := FastTanhOpt5Term(Temp[1]);
     {$ELSE}
     Temp[0] := FastTanhOpt5TermFPU(Temp[0]);
     Temp[1] := FastTanhOpt5TermFPU(Temp[1]);
     {$ENDIF}
     Outputs[Channel, Sample] := FGains[1] * FDownSampler[Channel].ProcessSample(Temp);

     // Peak Meter (Output)
     Temp[1] := CHalf32 * (abs(Outputs[Channel, Sample]) - FPeaks[Channel, 1]);
     FPeaks[Channel, 1] := CPeakRelease * (FPeaks[Channel, 1] + Temp[1] + abs(Temp[1]));
    end;
 finally
  FCriticalSection.Leave;
 end;
end;

end.
