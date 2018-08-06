unit DAV_DspDynamicLookaheadLimiter;

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

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Types, DAV_Classes;

type
  TAttackShape = (asLinear, asParabolic); 

  TCustomDspLookaheadLimiter = class(TDspSampleRatePersistent)
  private
    FInput_dB      : Single;
    FOutput_dB     : Single;
    FGainReduction : Single;
    FRelease       : Single;
    FLookAhead     : Integer;
    FAttackShape   : TAttackShape;
    procedure SetInputGain(const Value: Single);
    procedure SetOutputGain(const Value: Single);
    procedure SetRelease(const Value: Single);
    procedure SetLookAhead(const Value: Integer);
  protected
    FOutputFactor   : Double;
    FInputFactor    : Double;
    FReleaseFactor  : Double;
    FReleaseSamples : Double;
    FLookAheadInv   : Double;
    FHoldCounter    : Integer;
    FWindowSum      : array [0..1] of Double;
    FBufferPos      : array [0..1] of Integer;

    procedure CalculateOutputGainFactor;
    procedure CalculateReleaseFactor;
    procedure CalculateReciprocalLookAhead;
    procedure CalculateThresholdFactor;
    procedure LookAheadChanged; virtual;
    procedure OutputChanged; virtual;
    procedure ReleaseChanged; virtual;
    procedure ThresholdChanged; virtual;
    procedure SampleRateChanged; override;
  public
    constructor Create; override;

    property GainReductionFactor: Single read FGainReduction;
    property AttackShape: TAttackShape read FAttackShape write FAttackShape default asParabolic;
    property LookAhead: Integer read FLookAhead write SetLookAhead;
    property Output_dB: Single read FOutput_dB write SetOutputGain;
    property Release: Single read FRelease write SetRelease;
    property Input_dB: Single read FInput_dB write SetInputGain;
  end;

  TCustomDspFeedforwardLookaheadLimiter = class(TCustomDspLookaheadLimiter);

  TDspFeedforwardLookaheadLimiter32 = class(TCustomDspFeedforwardLookaheadLimiter, IDspProcessor32)
  protected
    FHoldValue      : Single;
    FPeak           : Single;
    FSampleBuffer32 : PDAVSingleFixedArray;
    FWindowBuffer32 : array [0..1] of PDAVSingleFixedArray;
    procedure AllocateBuffer;
    procedure LookAheadChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure InputSample(Input: Single);
    function GainSample(Input: Single): Single;
    function ProcessSample32(Input: Single): Single;
    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer);
  published
    property GainReductionFactor;
    property Output_dB;
    property Release;
    property Input_dB;
  end;

  TCustomDspFeedbackLikeLookaheadLimiter = class(TCustomDspLookaheadLimiter);

  TDspFeedbackLikeLookaheadLimiter32 = class(TCustomDspFeedbackLikeLookaheadLimiter, IDspProcessor32)
  protected
    FHoldValue      : Single;
    FPeak           : Single;
    FSampleBuffer32 : PDAVSingleFixedArray;
    FWindowBuffer32 : array [0..1] of PDAVSingleFixedArray;
    procedure AllocateBuffer;
    procedure LookAheadChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure InputSample(Input: Single);
    function GainSample(Input: Single): Single;
    function ProcessSample32(Input: Single): Single;
    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer);
  published
    property GainReductionFactor;
    property Output_dB;
    property Release;
    property Input_dB;
  end;

implementation

uses
  SysUtils, Math, DAV_Approximations, DAV_Common, DAV_Math;

{ TCustomDspLookaheadLimiter }

constructor TCustomDspLookaheadLimiter.Create;
begin
 inherited;
 FGainReduction := 1;
 FLookAhead     := 64;
 FOutput_dB     := -0.02;
 FInput_dB      := -3;
 FHoldCounter   := 0;
 FAttackShape   := asParabolic;

 CalculateThresholdFactor;
 CalculateOutputGainFactor;
 CalculateReciprocalLookAhead;
end;

procedure TCustomDspLookaheadLimiter.SetLookAhead(const Value: Integer);
begin
 if FLookAhead <> Value then
  begin
   FLookAhead := Value;
   LookAheadChanged;
  end;
end;

procedure TCustomDspLookaheadLimiter.LookAheadChanged;
begin
 CalculateReciprocalLookAhead;
 Changed;
end;

procedure TCustomDspLookaheadLimiter.CalculateReciprocalLookAhead;
begin
 FLookAheadInv :=  1 / FLookAhead;
end;

procedure TCustomDspLookaheadLimiter.SetOutputGain(const Value: Single);
begin
 if FOutput_dB <> Value then
  begin
   FOutput_dB := Value;
   OutputChanged;
  end;
end;

procedure TCustomDspLookaheadLimiter.SetRelease(const Value: Single);
begin
 if FRelease <> Value then
  begin
   FRelease := Value;
   ReleaseChanged;
  end;
end;

procedure TCustomDspLookaheadLimiter.SampleRateChanged;
begin
 CalculateReleaseFactor;
 inherited;
end;

procedure TCustomDspLookaheadLimiter.SetInputGain(const Value: Single);
begin
 if FInput_dB <> Value then
  begin
   FInput_dB := Value;
   ThresholdChanged;
  end;
end;

procedure TCustomDspLookaheadLimiter.ReleaseChanged;
begin
 CalculateReleaseFactor;
end;

procedure TCustomDspLookaheadLimiter.OutputChanged;
begin
 CalculateOutputGainFactor;
end;

procedure TCustomDspLookaheadLimiter.ThresholdChanged;
begin
 CalculateThresholdFactor;
 CalculateOutputGainFactor;
end;

procedure TCustomDspLookaheadLimiter.CalculateReleaseFactor;
begin
 FReleaseSamples := (FRelease * 0.001 * SampleRate);
 if FRelease = 0
  then FReleaseFactor := 1
  else FReleaseFactor := Power(2, -1 / FReleaseSamples);

(*
 Temp[0] := -CHalf32 * (FastLog2ContinousError5(FPeak) - FThrshlddB);
 Temp[1] := ;
 FGain   := FastPower2MinError3(Temp[0] - Temp[1]);
 Temp[1] := 2 * Temp[1] / (Temp[1] - abs(Temp[0]));
 FReleaseFactor := FastPower2MinError3(Temp[1] * FReleaseSampleCycle);
*)
end;

procedure TCustomDspLookaheadLimiter.CalculateThresholdFactor;
begin
 FInputFactor := dB_to_Amp(FInput_dB);
end;

procedure TCustomDspLookaheadLimiter.CalculateOutputGainFactor;
begin
 FOutputFactor := dB_to_Amp(FOutput_dB);
end;


{ TDspFeedforwardLookaheadLimiter32 }

constructor TDspFeedforwardLookaheadLimiter32.Create;
begin
 FWindowBuffer32[0] := nil;
 FWindowBuffer32[1] := nil;
 FSampleBuffer32    := nil;

 inherited;

 FWindowSum[0] := 0;
 FWindowSum[1] := 0;
 FPeak         := 0;
 FHoldValue    := 0;
 FPeak         := 0;

 AllocateBuffer;
end;

destructor TDspFeedforwardLookaheadLimiter32.Destroy;
begin
 Dispose(FWindowBuffer32[0]);
 Dispose(FWindowBuffer32[1]);
 Dispose(FSampleBuffer32);
 inherited;
end;

function TDspFeedforwardLookaheadLimiter32.GainSample(Input: Single): Single;
begin
 Result := Input * FOutputFactor * FGainReduction;
end;

procedure TDspFeedforwardLookaheadLimiter32.LookAheadChanged;
begin
 AllocateBuffer;
 inherited;
end;

procedure TDspFeedforwardLookaheadLimiter32.AllocateBuffer;
begin
 ReAllocMem(FWindowBuffer32[0], (FLookAhead div 2) * SizeOf(Single));
 ReAllocMem(FWindowBuffer32[1], (FLookAhead div 2) * SizeOf(Single));
 ReAllocMem(FSampleBuffer32, FLookAhead * SizeOf(Single));
 FillChar(FWindowBuffer32[0]^, (FLookAhead div 2) * SizeOf(Single), 0);
 FillChar(FWindowBuffer32[1]^, (FLookAhead div 2) * SizeOf(Single), 0);
 FillChar(FSampleBuffer32^, FLookAhead * SizeOf(Single), 0);
 FWindowSum[0] := 0;
 FWindowSum[1] := 0;
end;

procedure TDspFeedforwardLookaheadLimiter32.InputSample(Input: Single);
var
  Pos  : Integer;
  Temp : Single;
begin
 // get maximum and hold it
 if Abs(Input) >= FHoldValue then
  begin
   FHoldValue := Abs(Input);
   FHoldCounter := FLookAhead;
  end else
 if FHoldCounter > 0
  then Dec(FHoldCounter) else
   begin
    // find maximum
    FHoldCounter := 0;
    FHoldValue := Abs(FSampleBuffer32^[0]);
    for Pos := 1 to FLookAhead - 1 do
     if Abs(FSampleBuffer32^[Pos]) > FHoldValue then
      begin
       FHoldValue := Abs(FSampleBuffer32^[Pos]);
       FHoldCounter := Pos + 1;
      end;

    if FHoldCounter >= FBufferPos[0]
     then FHoldCounter := FHoldCounter - FBufferPos[0]
     else FHoldCounter := FHoldCounter - FBufferPos[0] + FLookAhead;

    if Abs(Input) > FHoldValue then
     begin
      FHoldValue := Abs(Input);
      FHoldCounter := FLookAhead;
     end;
   end;

 // store input sample
 FSampleBuffer32^[FBufferPos[0]] := Input;

 // update circular buffer position
 Inc(FBufferPos[0]);
 if FBufferPos[0] >= FLookAhead
  then FBufferPos[0] := 0;

 // calculate gain reduction
 Input := FInputFactor * FHoldValue;
 if Input > 1 then
  begin
   FPeak := FReleaseFactor * FPeak;
   Input := (Input - 1) / Input;
   if Input > FPeak
    then FPeak := Input;
  end
 else FPeak := FReleaseFactor * FPeak;

 Input := FPeak;

 case FAttackShape of
  asLinear :
   begin
    // apply rectangle window
    Temp := FWindowBuffer32[1]^[FBufferPos[1]];
    FWindowSum[1] := FWindowSum[1] + Input - Temp;

    FWindowBuffer32[1]^[FBufferPos[1]] := FWindowBuffer32[0]^[FBufferPos[1]];
    FWindowBuffer32[0]^[FBufferPos[1]] := Input;

    Input := FWindowSum[1] * FLookAheadInv;
   end;
  asParabolic :
   begin
    // apply triangle window
    Temp := FWindowBuffer32[0]^[FBufferPos[1]];
    FWindowBuffer32[0]^[FBufferPos[1]] := Input;

    FWindowSum[0] := FWindowSum[0] + Input - Temp;
    Input := FWindowSum[0] * 2 * FLookAheadInv;

     Temp := FWindowBuffer32[1]^[FBufferPos[1]];
    FWindowBuffer32[1]^[FBufferPos[1]] := Input;

    FWindowSum[1] := FWindowSum[1] + Input - Temp;
    Input := FWindowSum[1] * 2 * FLookAheadInv;
   end;
 end;

 // advance rectangle/triangle window buffer pos
 Inc(FBufferPos[1]);
 if FBufferPos[1] >= FLookAhead div 2
  then FBufferPos[1] := 0;

 // calculate gain reduction
 FGainReduction := FOutputFactor * FInputFactor * (1 - Input);
end;

procedure TDspFeedforwardLookaheadLimiter32.ProcessBlock32(
  const Data: PDAVSingleFixedArray; SampleCount: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample32(Data[Sample]);
end;

function TDspFeedforwardLookaheadLimiter32.ProcessSample32(Input: Single): Single;
var
  Pos  : Integer;
  Temp : Single;
begin
 // hold
 if Abs(Input) >= FHoldValue then
  begin
   FHoldValue := Abs(Input);
   FHoldCounter := FLookAhead;
  end else
 if FHoldCounter > 0
  then Dec(FHoldCounter) else
   begin
    // find maximum
    FHoldCounter := 0;
    FHoldValue := Abs(FSampleBuffer32^[0]);
    for Pos := 1 to FLookAhead - 1 do
     if Abs(FSampleBuffer32^[Pos]) > FHoldValue then
      begin
       FHoldValue := Abs(FSampleBuffer32^[Pos]);
       FHoldCounter := Pos + 1;
      end;

    if FHoldCounter >= FBufferPos[0]
     then FHoldCounter := FHoldCounter - FBufferPos[0]
     else FHoldCounter := FHoldCounter - FBufferPos[0] + FLookAhead;

    if Abs(Input) > FHoldValue then
     begin
      FHoldValue := Abs(Input);
      FHoldCounter := FLookAhead;
     end;
   end;

 // get current output sample and store input sample
 Result := FSampleBuffer32^[FBufferPos[0]];
 FSampleBuffer32^[FBufferPos[0]] := Input;

 // update circular buffer position
 Inc(FBufferPos[0]);
 if FBufferPos[0] >= FLookAhead
  then FBufferPos[0] := 0;

 // calculate gain reduction
 Input := FInputFactor * FHoldValue;
 if Input > 1 then
  begin
   FPeak := FReleaseFactor * FPeak;
   Input := (Input - 1) / Input;
   if Input > FPeak
    then FPeak := Input;
  end
 else FPeak := FReleaseFactor * FPeak;

 Input := FPeak;

 case FAttackShape of
  asLinear :
   begin
    Temp := FWindowBuffer32[1]^[FBufferPos[1]];
    FWindowSum[1] := FWindowSum[1] + Input - Temp;

    FWindowBuffer32[1]^[FBufferPos[1]] := FWindowBuffer32[0]^[FBufferPos[1]];
    FWindowBuffer32[0]^[FBufferPos[1]] := Input;

    Input := FWindowSum[1] * FLookAheadInv;
   end;
  asParabolic :
   begin
    // apply triangle window
    Temp := FWindowBuffer32[0]^[FBufferPos[1]];
    FWindowBuffer32[0]^[FBufferPos[1]] := Input;

    FWindowSum[0] := FWindowSum[0] + Input - Temp;
    Input := FWindowSum[0] * 2 * FLookAheadInv;

     Temp := FWindowBuffer32[1]^[FBufferPos[1]];
    FWindowBuffer32[1]^[FBufferPos[1]] := Input;

    FWindowSum[1] := FWindowSum[1] + Input - Temp;
    Input := FWindowSum[1] * 2 * FLookAheadInv;
   end;
 end;

 // advance rectangle/triangle window buffer pos
 Inc(FBufferPos[1]);
 if FBufferPos[1] >= FLookAhead div 2
  then FBufferPos[1] := 0;

 // calculate gain reduction
 FGainReduction := FOutputFactor * FInputFactor * (1 - Input);

 Result := FGainReduction * Result;
end;


{ TDspFeedbackLikeLookaheadLimiter32 }

constructor TDspFeedbackLikeLookaheadLimiter32.Create;
begin
 FWindowBuffer32[0] := nil;
 FWindowBuffer32[1] := nil;
 FSampleBuffer32    := nil;

 inherited;

 FWindowSum[0] := 0;
 FWindowSum[1] := 0;
 FPeak         := 0;
 FHoldValue    := 0;
 FPeak         := 0;

 AllocateBuffer;
end;

destructor TDspFeedbackLikeLookaheadLimiter32.Destroy;
begin
 Dispose(FWindowBuffer32[0]);
 Dispose(FWindowBuffer32[1]);
 Dispose(FSampleBuffer32);
 inherited;
end;

function TDspFeedbackLikeLookaheadLimiter32.GainSample(Input: Single): Single;
begin
 Result := Input * FOutputFactor * FGainReduction;
end;

procedure TDspFeedbackLikeLookaheadLimiter32.LookAheadChanged;
begin
 AllocateBuffer;
 inherited;
end;

procedure TDspFeedbackLikeLookaheadLimiter32.AllocateBuffer;
begin
 ReAllocMem(FWindowBuffer32[0], (FLookAhead div 2) * SizeOf(Single));
 ReAllocMem(FWindowBuffer32[1], (FLookAhead div 2) * SizeOf(Single));
 ReAllocMem(FSampleBuffer32, FLookAhead * SizeOf(Single));
 FillChar(FWindowBuffer32[0]^, (FLookAhead div 2) * SizeOf(Single), 0);
 FillChar(FWindowBuffer32[1]^, (FLookAhead div 2) * SizeOf(Single), 0);
 FillChar(FSampleBuffer32^, FLookAhead * SizeOf(Single), 0);
 FWindowSum[0] := 0;
 FWindowSum[1] := 0;
end;

procedure TDspFeedbackLikeLookaheadLimiter32.InputSample(Input: Single);
var
  Pos  : Integer;
  Temp : Single;
begin
 // get maximum and hold it
 if abs(Input) >= FHoldValue then
  begin
   FHoldValue := Abs(Input);
   FHoldCounter := FLookAhead;
  end else
 if FHoldCounter > 0
  then Dec(FHoldCounter) else
   begin
    // find maximum
    FHoldCounter := 0;
    FHoldValue := Abs(FSampleBuffer32^[0]);
    for Pos := 1 to FLookAhead - 1 do
     if Abs(FSampleBuffer32^[Pos]) > FHoldValue then
      begin
       FHoldValue := Abs(FSampleBuffer32^[Pos]);
       FHoldCounter := Pos + 1;
      end;

    if FHoldCounter >= FBufferPos[0]
     then FHoldCounter := FHoldCounter - FBufferPos[0]
     else FHoldCounter := FHoldCounter - FBufferPos[0] + FLookAhead;

    if Abs(Input) > FHoldValue then
     begin
      FHoldValue := Abs(Input);
      FHoldCounter := FLookAhead;
     end;
   end;

 // store input sample
 FSampleBuffer32^[FBufferPos[0]] := Input;

 // update circular buffer position
 Inc(FBufferPos[0]);
 if FBufferPos[0] >= FLookAhead
  then FBufferPos[0] := 0;

 // calculate gain reduction
 Input := FInputFactor * FHoldValue;
 if Input > 1 then
  begin
   FPeak := FastPower2MinError3(-1 / (sqr(Input) * FReleaseSamples)) * FPeak;
   Input := (Input - 1) / Input;
   if Input > FPeak
    then FPeak := Input;
  end
 else FPeak := FReleaseFactor * FPeak;

 Input := FPeak;

 case FAttackShape of
  asLinear :
   begin
    // apply rectangle window
    Temp := FWindowBuffer32[1]^[FBufferPos[1]];
    FWindowSum[1] := FWindowSum[1] + Input - Temp;

    FWindowBuffer32[1]^[FBufferPos[1]] := FWindowBuffer32[0]^[FBufferPos[1]];
    FWindowBuffer32[0]^[FBufferPos[1]] := Input;

    Input := FWindowSum[1] * FLookAheadInv;
   end;
  asParabolic :
   begin
    // apply triangle window
    Temp := FWindowBuffer32[0]^[FBufferPos[1]];
    FWindowBuffer32[0]^[FBufferPos[1]] := Input;

    FWindowSum[0] := FWindowSum[0] + Input - Temp;
    Input := FWindowSum[0] * 2 * FLookAheadInv;

     Temp := FWindowBuffer32[1]^[FBufferPos[1]];
    FWindowBuffer32[1]^[FBufferPos[1]] := Input;

    FWindowSum[1] := FWindowSum[1] + Input - Temp;
    Input := FWindowSum[1] * 2 * FLookAheadInv;
   end;
 end;

 // advance rectangle/triangle window buffer pos
 Inc(FBufferPos[1]);
 if FBufferPos[1] >= FLookAhead div 2
  then FBufferPos[1] := 0;

 // calculate gain reduction
 FGainReduction := FOutputFactor * FInputFactor * (1 - Input);
end;

procedure TDspFeedbackLikeLookaheadLimiter32.ProcessBlock32(
  const Data: PDAVSingleFixedArray; SampleCount: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample32(Data[Sample]);
end;

function TDspFeedbackLikeLookaheadLimiter32.ProcessSample32(Input: Single): Single;
var
  Pos  : Integer;
  Temp : Single;
begin
 // hold
 if Abs(Input) >= FHoldValue then
  begin
   FHoldValue := Abs(Input);
   FHoldCounter := FLookAhead;
  end else
 if FHoldCounter > 0
  then Dec(FHoldCounter) else
   begin
    // find maximum
    FHoldCounter := 0;
    FHoldValue := Abs(FSampleBuffer32^[0]);
    for Pos := 1 to FLookAhead - 1 do
     if Abs(FSampleBuffer32^[Pos]) > FHoldValue then
      begin
       FHoldValue := Abs(FSampleBuffer32^[Pos]);
       FHoldCounter := Pos + 1;
      end;

    if FHoldCounter >= FBufferPos[0]
     then FHoldCounter := FHoldCounter - FBufferPos[0]
     else FHoldCounter := FHoldCounter - FBufferPos[0] + FLookAhead;

    if Abs(Input) > FHoldValue then
     begin
      FHoldValue := Abs(Input);
      FHoldCounter := FLookAhead;
     end;
   end;

 // get current output sample and store input sample
 Result := FSampleBuffer32^[FBufferPos[0]];
 FSampleBuffer32^[FBufferPos[0]] := Input;

 // update circular buffer position
 Inc(FBufferPos[0]);
 if FBufferPos[0] >= FLookAhead
  then FBufferPos[0] := 0;

 // calculate gain reduction
 Input := FInputFactor * FHoldValue;
 if Input > 1 then
  begin
   FPeak := FastPower2MinError3(-1 / (sqr(Input) * FReleaseSamples)) * FPeak;
   Input := (Input - 1) / Input;
   if Input > FPeak
    then FPeak := Input;
  end
 else FPeak := FReleaseFactor * FPeak;

 Input := FPeak;

 case FAttackShape of
  asLinear :
   begin
    Temp := FWindowBuffer32[1]^[FBufferPos[1]];
    FWindowSum[1] := FWindowSum[1] + Input - Temp;

    FWindowBuffer32[1]^[FBufferPos[1]] := FWindowBuffer32[0]^[FBufferPos[1]];
    FWindowBuffer32[0]^[FBufferPos[1]] := Input;

    Input := FWindowSum[1] * FLookAheadInv;
   end;
  asParabolic :
   begin
    // apply triangle window
    Temp := FWindowBuffer32[0]^[FBufferPos[1]];
    FWindowBuffer32[0]^[FBufferPos[1]] := Input;

    FWindowSum[0] := FWindowSum[0] + Input - Temp;
    Input := FWindowSum[0] * 2 * FLookAheadInv;

     Temp := FWindowBuffer32[1]^[FBufferPos[1]];
    FWindowBuffer32[1]^[FBufferPos[1]] := Input;

    FWindowSum[1] := FWindowSum[1] + Input - Temp;
    Input := FWindowSum[1] * 2 * FLookAheadInv;
   end;
 end;

 // advance rectangle/triangle window buffer pos
 Inc(FBufferPos[1]);
 if FBufferPos[1] >= FLookAhead div 2
  then FBufferPos[1] := 0;

 // calculate gain reduction
 FGainReduction := FOutputFactor * FInputFactor * (1 - Input);

 Result := FGainReduction * Result;
end;

end.
