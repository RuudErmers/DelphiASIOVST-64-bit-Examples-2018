unit DAV_DspVoiceInput;

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

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_Classes;

type
  TTrackingType = (ttOff, ttFree, ttQuantized);
  TCustomVoiceInput = class(TDspSampleRatePersistent)
  private
    FMaximumFrequency    : Single;
    FTracking            : TTrackingType;
    FInvSampleRate       : Single;
    FVoicedUnvoicedRatio : Single;
    FBreath              : Single;
    FPitch               : Single;

    procedure SetMaximumFrequency(const Value: Single);
    procedure SetBreath(const Value: Single);
    procedure SetPitch(const Value: Single);
    procedure SetVoicedUnvoicedRatio(const Value: Single);
    procedure SetTracking(const Value: TTrackingType);
  protected
    FLowpassState : Array [0..1] of Single;
    FLowBuffer    : Array [0..1] of Single;
    FPitchStep    : Single;
    FSawPhase     : Single;
    FNoise        : Single;
    FLowEnv       : Single;
    FHighEnv      : Single;
    FLowFreq      : Single;
    FVUv          : Single;
    FRoot         : Single;
    FMinPitch     : Single;
    FMaxPitch     : Single;
    FTempMinPitch : Single;
    FPitchMult    : Single;

    procedure BreathChanged; virtual;
    procedure CalculateFrequencyBounds; virtual;
    procedure CalculateLowFrequency; virtual;
    procedure CalculatePitchStep; virtual;
    procedure PitchChanged; virtual;
    procedure SampleRateChanged; override;
    procedure TrackingChanged; virtual;
    procedure VoicedUnvoicedRatioChanged; virtual;
  public
    constructor Create; override;

    function ProcessSample(Input: Single): Single; virtual;
    procedure ResetStates; virtual;

    property Breath: Single read FBreath write SetBreath;
    property MaximumFrequency: Single read FMaximumFrequency write SetMaximumFrequency;
    property Pitch: Single read FPitch write SetPitch;
    property Tracking: TTrackingType read FTracking write SetTracking;
    property VoicedUnvoicedRatio: Single read FVoicedUnvoicedRatio write SetVoicedUnvoicedRatio;
  end;

  TVoiceInput = class(TCustomVoiceInput)
  published
    property Breath;
    property MaximumFrequency;
    property Pitch;
    property SampleRate;
    property VoicedUnvoicedRatio;
  end;

function Midi2String(const n: Single): string;

implementation

uses
  Math, SysUtils;

function Midi2String(const n: Single): string;
var
  o, s : Integer;
begin
 o := round(n / 12 - 0.49999);
 s := round(n - (12 * o) - 0.49999);
 o := o - 2;
 case s of
    0: result := result + 'C';
    1: result := result + 'C#';
    2: result := result + 'D';
    3: result := result + 'Eb';
    4: result := result + 'E';
    5: result := result + 'F';
    6: result := result + 'F#';
    7: result := result + 'G';
    8: result := result + 'G#';
    9: result := result + 'A';
   10: result := result + 'Bb';
   11: result := result + 'B';
  else result := '  ';
 end;

 result := result + ' ';

 if (o < 0) then result := result + '-';
 result := result + AnsiChar(48 + (abs(o) mod 10));
end;

{ TCustomVoiceInput }

constructor TCustomVoiceInput.Create;
begin
 inherited;
 FTracking := ttFree;
 FPitch := 0.5;
 FBreath := 20;
 FVoicedUnvoicedRatio := 50;
 FMaximumFrequency := 69;

 SampleRateChanged;

 CalculateFrequencyBounds;
 VoicedUnvoicedRatioChanged;
 BreathChanged;
 PitchChanged;
end;

procedure TCustomVoiceInput.CalculateFrequencyBounds;
begin
 FMinPitch := Power(16, 0.5 - (FMaximumFrequency - 45) / 48) * SampleRate / 440;
 FMaxPitch := 0.03 * SampleRate;
 FTempMinPitch := FMinPitch;
end;

procedure TCustomVoiceInput.CalculateLowFrequency;
begin
 FLowFreq := 660 * FInvSampleRate;
end;

procedure TCustomVoiceInput.CalculatePitchStep;
begin
 if (FTracking = ttOff)
  then FPitchStep := 110.0 * FPitchMult * FInvSampleRate;
end;

procedure TCustomVoiceInput.ResetStates;
begin
 FLowBuffer[0] := 0;
 FLowBuffer[1] := 0;
 FLowBuffer[0] := 0;
 FLowBuffer[1] := 0;
 FPitchStep    := 0;
 FSawPhase     := 0;
 FLowEnv       := 0;
end;

procedure TCustomVoiceInput.SetBreath(const Value: Single);
begin
 if FBreath <> Value then
  begin
   FBreath := Value;
   BreathChanged;
  end;
end;

procedure TCustomVoiceInput.BreathChanged;
begin
 FNoise := 6 * 0.01 * FBreath;
end;

procedure TCustomVoiceInput.SetMaximumFrequency(const Value: Single);
begin
 if FMaximumFrequency <> Value then
  begin
   FMaximumFrequency := Value;
   CalculateFrequencyBounds;
  end;
end;

procedure TCustomVoiceInput.SetPitch(const Value: Single);
begin
 if FPitch <> Value then
  begin
   FPitch := Value;
   PitchChanged;
  end;
end;

procedure TCustomVoiceInput.PitchChanged;
begin
 FPitchMult := Power(1.0594631, Round(48 * FPitch - 24));
 CalculatePitchStep;
end;

procedure TCustomVoiceInput.SetTracking(const Value: TTrackingType);
begin
 if FTracking <> Value then
  begin
   FTracking := Value;
   TrackingChanged;
  end;
end;

procedure TCustomVoiceInput.SetVoicedUnvoicedRatio(const Value: Single);
begin
 if FVoicedUnvoicedRatio <> Value then
  begin
   FVoicedUnvoicedRatio := Value;
   VoicedUnvoicedRatioChanged;
  end;
end;

procedure TCustomVoiceInput.VoicedUnvoicedRatioChanged;
begin
 FVUv := sqr(0.01 * FVoicedUnvoicedRatio);
end;

procedure TCustomVoiceInput.TrackingChanged;
begin
 CalculatePitchStep;
end;

procedure TCustomVoiceInput.SampleRateChanged;
begin
 FInvSampleRate := 1 / SampleRate;
 FRoot := Log10(8.1757989 * FInvSampleRate);
 CalculatePitchStep;
 CalculateLowFrequency;
 CalculateFrequencyBounds;
end;

function TCustomVoiceInput.ProcessSample(Input: Single): Single;
const
  CRootM : Single = 39.863137;
begin
 // fundamental filter (peaking 2nd-order 100Hz lpf)
 FLowpassState[0] := FLowpassState[0] - FLowFreq * (FLowpassState[1] + Input);
 FLowpassState[1] := FLowpassState[1] - FLowFreq * (FLowpassState[1] - FLowpassState[0]);

 // fundamental level
 FLowEnv := FLowEnv - FLowFreq * 0.1 * (FLowEnv - abs(FLowpassState[0]));

 Result := abs((Input + 0.03) * FVUv);

 // overall level (+ constant so >f0 when quiet)
 FHighEnv := FHighEnv - FLowFreq * 0.1 * (FHighEnv - Result);

 FLowBuffer[1] := FLowBuffer[1] + 1;
 if FTracking > ttOff then                                                      // pitch tracking
  begin
   if ((FLowpassState[1] > 0) and (FLowBuffer[0] <= 0)) then                    // found +ve zero crossing
    begin
     if ((FLowBuffer[1] > FTempMinPitch) and (FLowBuffer[1] < FMaxPitch)) then  // ...in allowed range
      begin
       FTempMinPitch := 0.6 * FLowBuffer[1];                                    // new max pitch to discourage octave jumps!
       FLowBuffer[0] := FLowpassState[1] / (FLowpassState[1] - FLowBuffer[0]);  // fractional period...
       FPitchStep := FPitchMult / (FLowBuffer[1] - FLowBuffer[0]);              // new period

       if (FTracking = ttQuantized) then                                        // quantize pitch
        begin
         FPitchStep := CRootM * (log10(FPitchStep) - FRoot);
         FPitchStep := Power(1.0594631, trunc(FPitchStep + 0.5) + CRootM * FRoot);
        end;
      end;
     FLowBuffer[1] := FLowBuffer[0];                                            // restart period measurement
     FTempMinPitch := 0.9999 * FTempMinPitch + 0.0001 * FMinPitch;
    end;
   FLowBuffer[0] := FLowpassState[1];                                           // remember previous sample
  end;

//   Result := 0.16384 * (2 * random - 1); // sibilance, but not used here


 // ...or modulated breath noise
 if (FLowEnv > FHighEnv)
  then Result := Result * FSawPhase * FNoise;
 Result := 1 * (Result + FSawPhase);
 FSawPhase := FSawPhase + FPitchStep;

 // badly aliased sawtooth!
 if (FSawPhase > 0.5)
  then FSawPhase := FSawPhase - 1;

 // catch denormals
 if (abs(FHighEnv) < 1E-10)
  then FHighEnv := 0;

 if (abs(FLowpassState[1]) < 1E-10) then
  begin
   FLowBuffer[0] := 0;
   FLowBuffer[1] := 0;
   FLowEnv       := 0;
  end;
end;

end.
