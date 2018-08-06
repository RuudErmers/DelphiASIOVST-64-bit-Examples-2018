unit TanhWaveshaperDSP;

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
  Forms, SyncObjs, DAV_Types, DAV_VSTModule, DAV_DspUpDownsampling;

type
  TUpDownSampler = record
    Upsampling   : TDAVUpsampling;
    Downsampling : TDAVDownsampling;
  end;

  TSimpleWaveshaper = function(Input: Double): Double of object;

  TTanhWaveshaperModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessOversampled(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterOversamplingChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterTypeDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterIntegerDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterTypeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
  private
    FCriticalSection : TCriticalSection;
    FOversampler     : array of TUpDownSampler;
    FOSFactor        : Integer;
    FGain            : Double;
    FTanhWaveShaper  : TSimpleWaveshaper;
    function TanhWaveshaper(Input: Double): Double;
    function TanhLike1Waveshaper(Input: Double): Double;
    function TanhLike2Waveshaper(Input: Double): Double;
    function TanhLike3Waveshaper(Input: Double): Double;
    function TanhLike4Waveshaper(Input: Double): Double;
    function TanhOpt3Waveshaper(Input: Double): Double;
    function TanhOpt4Waveshaper(Input: Double): Double;
    function TanhOpt5Waveshaper(Input: Double): Double;
    function TanhOpt6Waveshaper(Input: Double): Double;
    function TanhOpt7Waveshaper(Input: Double): Double;
    function FastTanhContinousError2Waveshaper(Input: Double): Double;
    function FastTanhContinousError3Waveshaper(Input: Double): Double;
    function FastTanhContinousError4Waveshaper(Input: Double): Double;
    function FastTanhContinousError5Waveshaper(Input: Double): Double;
    function FastTanhMinError2Waveshaper(Input: Double): Double;
    function FastTanhMinError3Waveshaper(Input: Double): Double;
    function FastTanhMinError4Waveshaper(Input: Double): Double;
    function FastTanhMinError5Waveshaper(Input: Double): Double;
    function FastTanhNollock3Waveshaper(Input: Double): Double;
    function FastTanhMrToast3TermWaveshaper(Input: Double): Double;
    function FastTanhMrToast4TermWaveshaper(Input: Double): Double;
    function FastTanhToguAudioLineWaveshaper(Input: Double): Double;
    function FastTanhTALtoastWaveshaper(Input: Double): Double;
    function FastTanhOhmforceWaveshaper(Input: Double): Double;
  public
    function TanhWaveshaperMath(Input: Double): Double;
  end;

implementation

uses
  Math, DAV_Common, DAV_Math, DAV_Approximations;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TTanhWaveshaperModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TTanhWaveshaperModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TTanhWaveshaperModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 Assert(numInputs = numOutputs);

 SetLength(FOversampler, numInputs);
 for Channel := 0 to Length(FOversampler) - 1 do
  begin
   FOversampler[Channel].Upsampling := TDAVUpsampling.Create;
   with FOversampler[Channel].Upsampling do
    begin
     SampleRate := Self.SampleRate;
     TransitionBandwidth := 2 * 21000 / SampleRate;
     Order := 16;
    end;
   FOversampler[Channel].Downsampling := TDAVDownsampling.Create;
   with FOversampler[Channel].Downsampling do
    begin
     SampleRate := Self.SampleRate;
     TransitionBandwidth := 2 * 21000 / SampleRate;
     Order := 16;
    end;
  end;

 FOSFactor := 1;
 FTanhWaveShaper := TanhWaveshaperMath;

 Parameter[0] := 1;
 Parameter[1] := 16;
 Parameter[2] := 6;
 Parameter[3] := 0;
end;

procedure TTanhWaveshaperModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 {$IFDEF UseCriticalSection}
 FreeAndNil(FCriticalSection);
 {$ENDIF}

 for Channel := 0 to Length(FOversampler) - 1 do
  begin
   FreeAndNil(FOversampler[Channel].Upsampling);
   FreeAndNil(FOversampler[Channel].Downsampling);
  end;
end;

function TTanhWaveshaperModule.TanhWaveshaper(Input: Double): Double;
begin
 Result := DAV_Math.Tanh(FGain * Input);
end;

function TTanhWaveshaperModule.TanhWaveshaperMath(Input: Double): Double;
begin
 Result := Math.Tanh(FGain * Input);
end;

procedure TTanhWaveshaperModule.ParameterOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FOversampler) - 1 do
  begin
   if Assigned(FOversampler[Channel].Upsampling)
    then FOversampler[Channel].Upsampling.Order := Round(Value);
   if Assigned(FOversampler[Channel].Downsampling)
    then FOversampler[Channel].Downsampling.Order := Round(Value);
  end;
end;

function TTanhWaveshaperModule.TanhLike1Waveshaper(Input: Double): Double;
begin
 Result := FastTanh2Like1Term(2 * FGain * Input);
end;

function TTanhWaveshaperModule.TanhLike2Waveshaper(Input: Double): Double;
begin
 Result := FastTanh2Like2Term(2 * FGain * Input);
end;

function TTanhWaveshaperModule.TanhLike3Waveshaper(Input: Double): Double;
begin
 Result := FastTanh2Like3Term(2 * FGain * Input);
end;

function TTanhWaveshaperModule.TanhLike4Waveshaper(Input: Double): Double;
begin
 Result := FastTanh2Like4Term(2 * FGain * Input);
end;

function TTanhWaveshaperModule.TanhOpt3Waveshaper(Input: Double): Double;
begin
 Result := FastTanhOpt3Term(FGain * Input);
end;

function TTanhWaveshaperModule.TanhOpt4Waveshaper(Input: Double): Double;
begin
 Result := FastTanhOpt4Term(FGain * Input);
end;

function TTanhWaveshaperModule.TanhOpt5Waveshaper(Input: Double): Double;
begin
 Result := FastTanhOpt5Term(FGain * Input);
end;

function TTanhWaveshaperModule.TanhOpt6Waveshaper(Input: Double): Double;
begin
 Result := FastTanhOpt6Term(FGain * Input);
end;

function TTanhWaveshaperModule.TanhOpt7Waveshaper(Input: Double): Double;
begin
 Result := FastTanhOpt7Term(FGain * Input);
end;

function TTanhWaveshaperModule.FastTanhMinError2Waveshaper(Input: Double): Double;
begin
 Result := FastTanhMinError2(FGain * Input);
end;

function TTanhWaveshaperModule.FastTanhContinousError2Waveshaper(Input: Double): Double;
begin
 Result := FastTanhContinousError2(FGain * Input);
end;

function TTanhWaveshaperModule.FastTanhMinError3Waveshaper(Input: Double): Double;
begin
 Result := FastTanhMinError3(FGain * Input);
end;

function TTanhWaveshaperModule.FastTanhContinousError3Waveshaper(Input: Double): Double;
begin
 Result := FastTanhContinousError3(FGain * Input);
end;

function TTanhWaveshaperModule.FastTanhMinError4Waveshaper(Input: Double): Double;
begin
 Result := FastTanhMinError4(FGain * Input);
end;

function TTanhWaveshaperModule.FastTanhContinousError4Waveshaper(Input: Double): Double;
begin
 Result := FastTanhContinousError4(FGain * Input);
end;

function TTanhWaveshaperModule.FastTanhMinError5Waveshaper(Input: Double): Double;
begin
 Result := FastTanhMinError5(FGain * Input);
end;

function TTanhWaveshaperModule.FastTanhContinousError5Waveshaper(Input: Double): Double;
begin
 Result := FastTanhContinousError5(FGain * Input);
end;

function FastPower2NollockError3(Value: Single): Single;
var
  IntCast : Integer absolute result;
begin
 IntCast := Round(Value);
 Value := Value - IntCast;
 IntCast := ($7F + Intcast) shl 23;
 Result := Result * (1 + Value * (0.69314718 + Value * (0.22741128 +
   Value *  0.079441542)));
end;

function FastTanhNollockError3(Value: Double): Double;
var
  Temp : Double;
begin
 Temp := FastPower2NollockError3(C2Exp32 * Value);
 Result := (Temp - 1) / (Temp + 1);
end;

function TTanhWaveshaperModule.FastTanhNollock3Waveshaper(
  Input: Double): Double;
begin
 Result := FastTanhNollockError3(FGain * Input);
end;

function TanhApprox(Input: Double): Double;
begin
 Result := abs(Input) + sqr(Input) * (1 + 1.41645 * sqr(Input));
 Result := Result / (1 + Result);
 if Input < 0
  then Result := -Result;
end;

function TTanhWaveshaperModule.FastTanhOhmforceWaveshaper(
  Input: Double): Double;
begin
 Result := TanhApprox(FGain * Input);
end;

function TTanhWaveshaperModule.FastTanhToguAudioLineWaveshaper(
  Input: Double): Double;
begin
 Input := 2 * FGain * Input;
 Result := Input / (abs(Input) + 2 / (2.12 - 1.44 * abs(Input) + sqr(Input)));
end;

function TTanhWaveshaperModule.FastTanhTALtoastWaveshaper(
  Input: Double): Double;
begin
 Input := 2 * FGain * Input;
 Result := Input / (abs(Input) + 4.3448473351 / (0.11932825377 - 0.15402606842 * abs(Input) + sqr(Input)));
end;

function TTanhWaveshaperModule.FastTanhMrToast3TermWaveshaper(
  Input: Double): Double;
begin
 Input := FGain * Input;
 Result := Input / (1.1605589854 + 0.37011709966 * sqr(Input))
   + 0.10710006710 * Input;
end;

function TTanhWaveshaperModule.FastTanhMrToast4TermWaveshaper(
  Input: Double): Double;
begin
 Input := FGain * CHalf32 * Input;
 Result := Input / (3.8566590621 + 0.11306387017 * sqr(Input)) + Input / (0.59265249338 + 0.79772974129 * sqr(Input))
end;

procedure TTanhWaveshaperModule.ParameterTypeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  case Round(Parameter[Index]) of
    0 : FTanhWaveShaper := TanhWaveshaperMath;
    1 : FTanhWaveShaper := TanhWaveshaper;
    2 : FTanhWaveShaper := TanhOpt3Waveshaper;
    3 : FTanhWaveShaper := TanhOpt4Waveshaper;
    4 : FTanhWaveShaper := TanhOpt5Waveshaper;
    5 : FTanhWaveShaper := TanhOpt6Waveshaper;
    6 : FTanhWaveShaper := TanhOpt7Waveshaper;
    7 : FTanhWaveShaper := FastTanhContinousError2Waveshaper;
    8 : FTanhWaveShaper := FastTanhContinousError3Waveshaper;
    9 : FTanhWaveShaper := FastTanhContinousError4Waveshaper;
   10 : FTanhWaveShaper := FastTanhContinousError5Waveshaper;
   11 : FTanhWaveShaper := FastTanhMinError2Waveshaper;
   12 : FTanhWaveShaper := FastTanhMinError3Waveshaper;
   13 : FTanhWaveShaper := FastTanhMinError4Waveshaper;
   14 : FTanhWaveShaper := FastTanhMinError5Waveshaper;
   15 : FTanhWaveShaper := TanhLike1Waveshaper;
   16 : FTanhWaveShaper := TanhLike2Waveshaper;
   17 : FTanhWaveShaper := TanhLike3Waveshaper;
   18 : FTanhWaveShaper := TanhLike4Waveshaper;
   19 : FTanhWaveShaper := FastTanhNollock3Waveshaper;
   20 : FTanhWaveShaper := FastTanhMrToast3TermWaveshaper;
   21 : FTanhWaveShaper := FastTanhMrToast4TermWaveshaper;
   22 : FTanhWaveShaper := FastTanhToguAudioLineWaveshaper;
   23 : FTanhWaveShaper := FastTanhTALtoastWaveshaper;
   24 : FTanhWaveShaper := FastTanhOhmforceWaveshaper;
  end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TTanhWaveshaperModule.ParameterTypeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case Round(Parameter[Index]) of
   0 : PreDefined := 'Tanh (Math)';
   1 : PreDefined := 'Tanh (DAV_Types)';
   2 : PreDefined := 'FastTanhOpt3Term';
   3 : PreDefined := 'FastTanhOpt4Term';
   4 : PreDefined := 'FastTanhOpt5Term';
   5 : PreDefined := 'FastTanhOpt6Term';
   6 : PreDefined := 'FastTanhOpt7Term';
   7 : PreDefined := 'FastTanhContinousError2Waveshaper';
   8 : PreDefined := 'FastTanhContinousError3Waveshaper';
   9 : PreDefined := 'FastTanhContinousError4Waveshaper';
  10 : PreDefined := 'FastTanhContinousError5Waveshaper';
  11 : PreDefined := 'FastTanhMinError2Waveshaper';
  12 : PreDefined := 'FastTanhMinError3Waveshaper';
  13 : PreDefined := 'FastTanhMinError4Waveshaper';
  14 : PreDefined := 'FastTanhMinError5Waveshaper';
  15 : PreDefined := 'TanhLike1Waveshaper';
  16 : PreDefined := 'TanhLike2Waveshaper';
  17 : PreDefined := 'TanhLike3Waveshaper';
  18 : PreDefined := 'TanhLike4Waveshaper';
  19 : PreDefined := 'Nollock';
  20 : PreDefined := 'mistertoast 3-Term';
  21 : PreDefined := 'mistertoast 4-Term';
  22 : PreDefined := 'ToguAudioLine';
  23 : PreDefined := 'TAL vs. Mr.Toast';
  24 : PreDefined := 'Ohmforce';
 end;
end;

procedure TTanhWaveshaperModule.ParameterIntegerDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(IntToStr(Round(Parameter[Index])));
end;

procedure TTanhWaveshaperModule.ParameterGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  FGain := dB_to_Amp(Value);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TTanhWaveshaperModule.ParameterOversamplingChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  FOSFactor := Round(Value);
  for Channel := 0 to Length(FOversampler) - 1 do
   begin
    if Assigned(FOversampler[Channel].Upsampling)
     then FOversampler[Channel].Upsampling.Factor := FOSFactor;
    if Assigned(FOversampler[Channel].Downsampling)
     then FOversampler[Channel].Downsampling.Factor := FOSFactor;
   end;

  if FOSFactor = 1
   then OnProcess := VSTModuleProcess
   else OnProcess := VSTModuleProcessOversampled;
  OnProcess32Replacing := OnProcess;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TTanhWaveshaperModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Channel : Integer;
  Sample  : Integer;
begin
 FCriticalSection.Enter;
 try
  for Sample := 0 to SampleFrames - 1 do
   for Channel := 0 to Length(FOversampler) - 1
    do Outputs[Channel, Sample] := FTanhWaveShaper(Inputs[Channel, Sample]);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TTanhWaveshaperModule.VSTModuleProcessOversampled(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Channel : Integer;
  Sample  : Integer;
  OScnt   : Integer;
  Temp    : array [0..15] of Double;
begin
 FCriticalSection.Enter;
 try
  for Sample := 0 to SampleFrames - 1 do
   for Channel := 0 to Length(FOversampler) - 1 do
    begin
     FOversampler[Channel].Upsampling.Upsample64(Inputs[Channel, Sample], @Temp);

     for OScnt := 0 to FOSFactor - 1
      do Temp[OScnt] := FTanhWaveShaper(Temp[OScnt]);

     Outputs[Channel, Sample] := FOversampler[Channel].Downsampling.Downsample64(@Temp);
    end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TTanhWaveshaperModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel : Integer;
begin
 if Abs(SampleRate) > 0 then
  for Channel := 0 to Length(FOversampler) - 1 do
   begin
    FCriticalSection.Enter;
    try
     if Assigned(FOversampler[Channel].Upsampling)
      then FOversampler[Channel].Upsampling.SampleRate := Abs(SampleRate);
     if Assigned(FOversampler[Channel].Downsampling)
      then FOversampler[Channel].Downsampling.SampleRate := Abs(SampleRate);
    finally
     FCriticalSection.Leave;
    end;
   end;
end;

end.
