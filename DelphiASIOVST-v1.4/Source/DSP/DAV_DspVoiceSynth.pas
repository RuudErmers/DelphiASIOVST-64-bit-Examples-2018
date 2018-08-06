unit DAV_DspVoiceSynth;

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
  DAV_Types, DAV_Complex, DAV_Classes, DAV_DspTuner;

type
  TCustomVoiceSynth = class(TCustomLinearZeroCrossingTuner, IDspProcessor32)
  private
    procedure SetAttack(const Value: Single);
    procedure SetRelease(const Value: Single);
    procedure SetThreshold(const Value: Single);
  protected
    FCurrentPosition         : TComplex32;
    FComplexAngle            : TComplex32;
    FSampleRateReciprocal    : Single;
    FAttack, FAttackFactor   : Single;
    FRelease, FReleaseFactor : Single;
    FLevel, FThreshold       : Single;
    FQuantizeToNotes         : Boolean;
    procedure ProcessDownsampled(DownSampled: Single); override;
    procedure SampleRateChanged; override;

    procedure CalculateAttackFactor; virtual;
    procedure CalculateReleaseFactor; virtual;
    procedure AttackChanged; virtual;
    procedure ReleaseChanged; virtual;
  public
    constructor Create; override;
    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer);
    function ProcessSample32(Input: Single): Single; reintroduce; virtual;

    property Attack: Single read FAttack write SetAttack;
    property Release: Single read FRelease write SetRelease;
    property Threshold: Single read FThreshold write SetThreshold;
    property QuantizeToNotes: Boolean read FQuantizeToNotes write FQuantizeToNotes;
  end;

  TVoiceSynth = class(TCustomVoiceSynth)
  published
    property SampleRate;
    property DownSampleFilterOrder;
    property MinimumFrequency;
    property MaximumFrequency;
    property Attack;  // in ms
    property Release; // in ms
  end;

implementation

uses
  DAV_Common, DAV_Math, DAV_Approximations, DAV_DspDynamics;

{ TCustomVoiceSynth }

constructor TCustomVoiceSynth.Create;
begin
 inherited;
 FCurrentPosition.Re := 1;
 FCurrentPosition.Im := 0;
 FComplexAngle.Re := 1;
 FComplexAngle.Im := 0;
 FAttack := 1;
 FRelease := 10;
 FThreshold := 0;
 FQuantizeToNotes := True;
 CalculateAttackFactor;
 CalculateReleaseFactor;
end;

procedure TCustomVoiceSynth.SampleRateChanged;
begin
 inherited;
 FSampleRateReciprocal := 1 / SampleRate;
end;

procedure TCustomVoiceSynth.SetAttack(const Value: Single);
begin
 if FAttack <> Value then
  begin
   FAttack := Value;
   AttackChanged;
  end;
end;

procedure TCustomVoiceSynth.SetRelease(const Value: Single);
begin
 if FRelease <> Value then
  begin
   FRelease := Value;
   ReleaseChanged;
  end;
end;

procedure TCustomVoiceSynth.SetThreshold(const Value: Single);
begin
 FThreshold := Limit(Value, -1, 1);
end;

procedure TCustomVoiceSynth.AttackChanged;
begin
 CalculateAttackFactor;
end;

procedure TCustomVoiceSynth.ReleaseChanged;
begin
 CalculateReleaseFactor;
end;

procedure TCustomVoiceSynth.CalculateAttackFactor;
begin
  if FAttack = 0 then FAttackFactor := 0
  else FAttackFactor := 1 - exp( -ln2 / (FAttack * 0.001 * SampleRate));
end;

procedure TCustomVoiceSynth.CalculateReleaseFactor;
begin
  if FRelease = 0 then FReleaseFactor := 0
  else FReleaseFactor := exp( -ln2 / (FRelease * 0.001 * SampleRate));
end;

function TCustomVoiceSynth.ProcessSample32(Input: Single): Single;
begin
 inherited ProcessSample32(Input);

 if abs(Input) > FLevel
  then FLevel := FLevel + (abs(Input) - FLevel) * FAttackFactor
  else FLevel := abs(Input) + (FLevel - abs(Input)) * FReleaseFactor;

 ComplexMultiplyInplace32(FCurrentPosition, FComplexAngle);
 result := FLevel * FCurrentPosition.Re;
end;

procedure TCustomVoiceSynth.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample32(Data[Sample]);
end;

procedure TCustomVoiceSynth.ProcessDownsampled(DownSampled: Single);
var
  Offset : Single;
begin
 if (Downsampled < FThreshold * FLevel) = FIsAbove then
  begin
   FIsAbove := not FIsAbove;

   if FOneCrossingOnly and FIsAbove then
    begin
     inc(FSamples);
     exit;
    end;

   Offset := (FLastSample / (FLastSample - Downsampled));

   FAverageSamples := SmoothFactor * FAverageSamples +
     (1 - SmoothFactor) * (FSamples - FLastOffset + Offset);
   FSamples := 1;
   FLastOffset := Offset;

   FCurrentFreq := FFrequencyFactor * SampleRate / (DownSampleFilterOrder * FAverageSamples);
   if FQuantizeToNotes
    then FCurrentFreq := 440 * FastPower2ContinousError3(round(12 *
           FastLog2ContinousError3(FCurrentFreq / 440)) * COneTwelfth32);

   GetSinCos(2 * Pi * FCurrentFreq * FSampleRateReciprocal, FComplexAngle.Im, FComplexAngle.Re);
  end
 else inc(FSamples);
end;

initialization
  RegisterDspProcessor32(TVoiceSynth);

end.
