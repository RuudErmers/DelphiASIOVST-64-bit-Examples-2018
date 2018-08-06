unit DAV_DspBassBaron;

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
  DAV_Types, DAV_Classes, DAV_DspFilter, DAV_DspFilterButterworth,
  DAV_DspFilterLinkwitzRiley, DAV_DspDynamics, DAV_DspLightweightDynamics,
  DAV_DspPsychoacousticBassEnhancer;

type
  TSplit32 = procedure (Input: Single; out Low, High: Single) of object;
  TSplit64 = procedure (Input: Double; out Low, High: Double) of object;
  TSplitFilter = (sfSimple, sfLinkwitzRiley);
  TBassBaronAlgorithm = (bbaStraight, bbaPerfectHarmony, bbaResurrection,
    bbaSteam);

  TCustomBassBaron = class(TCustomPsychoAcousticBassEnhancer)
  private
    FDecay       : Single;
    FGains       : array [0..4] of Single;
    FResponse    : Single;
    FSplitFilter : TSplitFilter;
    FAlgorithm: TBassBaronAlgorithm;
    procedure SetLowcutFrequency(const Value: Single);
    procedure SetDecay(const Value: Single);
    procedure SetResponse(const Value: Single);
    procedure SetSplitFilter(const Value: TSplitFilter);
    procedure SetSplitOrder(const Value: Byte);
    procedure UpdateSplitFilters;
    procedure SetAlgorithm(const Value: TBassBaronAlgorithm);
  protected
    // split band filters
    FCrossover       : TButterworthSplitBandFilter;
    FLowpass         : TButterworthLowpassFilter;
    FHighpass        : TButterworthHighpassFilter;
    FLowCut          : TButterworthLowCutFilter;
    FLimiter         : TCustomLimiter;
    FSplit32         : TSplit32;
    FSplit64         : TSplit64;
    FSign            : Single;
    FOrder           : Byte;
    FLowCutFrequency : Single;

    procedure AlgorithmChanged; virtual;
    procedure DecayChanged; virtual;
    procedure LowcutFrequencyChanged; virtual;
    procedure OrderChanged; virtual;
    procedure ResponseChanged; virtual;
    procedure FrequencyChanged; override;
    procedure SampleRateChanged; override;
    procedure SplitFilterChanged; virtual;

    procedure SplitSimple32(Input: Single; out Low, High: Single);
    procedure SplitSimple64(Input: Double; out Low, High: Double);
    procedure SplitLinkwitzRiley32(Input: Single; out Low, High: Single);
    procedure SplitLinkwitzRiley64(Input: Double; out Low, High: Double);
  public
    constructor Create; override;
    destructor Destroy; override;

    function ProcessSample32(Input: Single): Single; override;
    function ProcessSample64(Input: Double): Double; override;
    procedure ResetStates; virtual;

    property InputLevel: Single read FGains[0] write FGains[0];
    property OutputLevel: Single read FGains[4] write FGains[4];
    property SplitFilter: TSplitFilter read FSplitFilter write SetSplitFilter;
    property SplitOrder: Byte read FOrder write SetSplitOrder;
    property LowcutFrequency: Single read FLowCutFrequency write SetLowcutFrequency;
    property
    Algorithm: TBassBaronAlgorithm read FAlgorithm write SetAlgorithm;

    property Response: Single read FResponse write SetResponse;

    property HighFrequencyLevel: Single read FGains[1] write FGains[1];
    property OriginalBassLevel: Single read FGains[2] write FGains[2];
    property HarmonicBassLevel: Single read FGains[3] write FGains[3];
    property Decay: Single read FDecay write SetDecay;
  end;

  TBassBaron = class(TCustomBassBaron)
  public
    property Response;
    property InputLevel;
    property HighFrequencyLevel;
    property OriginalBassLevel;
    property HarmonicBassLevel;
    property Decay;
    property LowcutFrequency;
  end;

implementation

uses
  SysUtils, DAV_Common, DAV_DspInterpolation, DAV_Approximations;

{ TCustomBassBaron }

constructor TCustomBassBaron.Create;
begin
 inherited;
 FOrder := 3;
 FResponse := 20;
 FSplitFilter := sfSimple;
 UpdateSplitFilters;

 // create & setup limiter
 FLimiter := TLightweightSoftKneeLimiter.Create;
 with TLightweightSoftKneeLimiter(FLimiter) do
  begin
   Attack := FResponse;
   Release := FResponse;
   Threshold_dB := 0;
   Knee_dB := 6;
   MakeUpGain_dB := 3;
   SampleRate := SampleRate;
  end;

 // create crossover filter
 FCrossover := TButterworthSplitBandFilter.Create(FOrder);
 FCrossover.SampleRate := SampleRate;
 FLowpass := TButterworthLowpassFilter.Create(FOrder);
 FLowpass.SampleRate := SampleRate;
 FHighpass := TButterworthHighpassFilter.Create(FOrder);
 FHighpass.SampleRate := SampleRate;

 // create & setup low cut filter
 FLowCutFrequency := 20;
 FLowCut := TButterworthHighpassFilter.Create(1);
 FLowCut.SampleRate := SampleRate;
 FLowCut.Frequency  := FLowCutFrequency;

 FGains[0] := 1;
 FGains[1] := 1;
 FGains[2] := 1;
 FGains[3] := 0;

 FrequencyChanged;
end;

destructor TCustomBassBaron.Destroy;
begin
 FreeAndNil(FLimiter);

 FreeAndNil(FCrossover);
 FreeAndNil(FLowpass);
 FreeAndNil(FHighpass);

 FreeAndNil(FLowCut);

 inherited;
end;

procedure TCustomBassBaron.SetAlgorithm(const Value: TBassBaronAlgorithm);
begin
 if FAlgorithm <> Value then
  begin
   FAlgorithm := Value;
   AlgorithmChanged;
  end;
end;

procedure TCustomBassBaron.SetDecay(const Value: Single);
begin
 if FDecay <> Value then
  begin
   FDecay := Value;
   DecayChanged;
  end;
end;

procedure TCustomBassBaron.SetLowcutFrequency(
  const Value: Single);
begin
 if FLowCutFrequency <> Value then
  begin
   FLowCutFrequency := Value;
   LowcutFrequencyChanged;
  end;
end;

procedure TCustomBassBaron.SetResponse(const Value: Single);
begin
 if FResponse <> Value then
  begin
   FResponse := Value;
   ResponseChanged;
  end;
end;

procedure TCustomBassBaron.SetSplitFilter(const Value: TSplitFilter);
begin
 if FSplitFilter <> Value then
  begin
   FSplitFilter := Value;
   SplitFilterChanged;
  end;
end;

procedure TCustomBassBaron.SetSplitOrder(const Value: Byte);
begin
 if FOrder <> Value then
  begin
   FOrder := Value;
   OrderChanged;
  end;
end;

procedure TCustomBassBaron.SplitFilterChanged;
begin
 UpdateSplitFilters;
end;

procedure TCustomBassBaron.UpdateSplitFilters;
begin
 case FSplitFilter of
          sfSimple : begin
                      FSplit32 := SplitSimple32;
                      FSplit64 := SplitSimple64;
                     end;
   sfLinkwitzRiley : begin
                      FSplit32 := SplitLinkwitzRiley32;
                      FSplit64 := SplitLinkwitzRiley64;
                     end;
 end;
end;

procedure TCustomBassBaron.SplitLinkwitzRiley32(Input: Single; out Low,
  High: Single);
begin
 FCrossover.ProcessSample32(FGains[0] * Input, Low, High);
 Low  := FLowpass.ProcessSample32(Low - CDenorm32);
 High := FHighpass.ProcessSample32(FSign * High - CDenorm32);
end;

procedure TCustomBassBaron.SplitLinkwitzRiley64(Input: Double; out Low,
  High: Double);
begin
 FCrossover.ProcessSample64(FGains[0] * Input, Low, High);
 Low  := FLowpass.ProcessSample64(Low - CDenorm32);
 High := FHighpass.ProcessSample64(FSign * High - CDenorm32);
end;

procedure TCustomBassBaron.SplitSimple32(Input: Single; out Low, High: Single);
begin
 FCrossover.ProcessSample32(FGains[0] * Input, Low, High);
end;

procedure TCustomBassBaron.SplitSimple64(Input: Double; out Low, High: Double);
begin
 FCrossover.ProcessSample64(FGains[0] * Input, Low, High);
end;


procedure TCustomBassBaron.AlgorithmChanged;
var
  OldLimiter : TCustomLimiter;
begin
 OldLimiter := FLimiter;
 case FAlgorithm of
  bbaStraight :
    begin
     FLimiter := TRCLimiter.Create;
     with TRCLimiter(FLimiter) do
      begin
       Attack := FResponse;
       Release := FResponse;
       Threshold_dB := 0;
       MakeUpGain_dB := 0;
       SampleRate := SampleRate;
      end;
    end;
  bbaPerfectHarmony :
    begin
     FLimiter := TLightweightSoftKneeLimiter.Create;
     with TLightweightSoftKneeLimiter(FLimiter) do
      begin
       Attack := FResponse;
       Release := FResponse;
       Threshold_dB := 0;
       Knee_dB := 6;
       MakeUpGain_dB := 3;
       SampleRate := SampleRate;
      end;
    end;
  bbaResurrection :
    begin
     FLimiter := TLightweightSoftKneeFeedbackLikeLimiter.Create;
     with TLightweightSoftKneeFeedbackLikeLimiter(FLimiter) do
      begin
       Attack := 20;
       Release := 20;
       Threshold_dB := 0;
       Knee_dB := 6;
       MakeUpGain_dB := 3;
       SampleRate := SampleRate;
      end;
    end;
  bbaSteam :
    begin
     FLimiter := TLightweightSoftKneeLimiter.Create;
     with TLightweightSoftKneeLimiter(FLimiter) do
      begin
       Attack := FResponse;
       Release := FResponse;
       Threshold_dB := -5;
       Knee_dB := 2;
       MakeUpGain_dB := 6;
       SampleRate := SampleRate;
      end;
    end;
 end;

 FreeAndNil(OldLimiter);
 Changed;
end;

procedure TCustomBassBaron.DecayChanged;
begin
 Changed;
end;

procedure TCustomBassBaron.ResetStates;
begin
 FCrossover.ResetStates;
 FLowCut.ResetStates;
 FLowpass.ResetStates;
 FHighpass.ResetStates;
 FLimiter.Reset;
end;

procedure TCustomBassBaron.ResponseChanged;
begin
 FLimiter.Attack := FResponse;
 FLimiter.Release := FResponse;
 Changed;
end;

procedure TCustomBassBaron.FrequencyChanged;
begin
 FCrossover.Frequency := Frequency;
 FLowpass.Frequency := Frequency;
 FHighpass.Frequency := Frequency;
 Changed;
end;

procedure TCustomBassBaron.LowcutFrequencyChanged;
begin
 FLowCut.Frequency := FLowCutFrequency;
 Changed;
end;

procedure TCustomBassBaron.OrderChanged;
begin
 FCrossover.Order := FOrder;
 FLowpass.Order := FOrder;
 FHighpass.Order := FOrder;
 FSign := 1 - 2 * (FOrder mod 2);
end;

function TCustomBassBaron.ProcessSample32(Input: Single): Single;
var
  Low, High, Harmonic : Single;
begin
 FSplit32(Input, Low, High);
 Harmonic := Limit(0.5 * FLimiter.ProcessSample64(4 *
             FLowCut.ProcessSample64(
             Low * (1 + Low * -2 * FDecay))));

 Result := FGains[4] * (FGains[2] * Low + FGains[3] * Harmonic + FGains[1] * High);
end;

function TCustomBassBaron.ProcessSample64(Input: Double): Double;
var
  Low, High, Harmonic : Double;
begin
 FSplit64(Input, Low, High);
 Harmonic := Limit(0.5 * FLimiter.ProcessSample64(4 *
             FLowCut.ProcessSample64(
             Low * (1 + Low * -2 * FDecay))));

 Result := FGains[4] * FGains[2] * Low + FGains[3] * Harmonic + FGains[1] * High;
end;

procedure TCustomBassBaron.SampleRateChanged;
begin
 FCrossover.SampleRate := SampleRate;
 FLowpass.SampleRate := SampleRate;
 FHighpass.SampleRate := SampleRate;
 FLowCut.SampleRate := SampleRate;
 FLimiter.SampleRate := SampleRate;
end;

initialization
  RegisterDspProcessors32([TBassBaron]);

end.
