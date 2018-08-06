unit DAV_DspExciter;

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
  Classes, DAV_Types, DAV_Classes, DAV_DspFilterButterworth,
  DAV_DspDynamics, DAV_DspLightweightDynamics, DAV_DspPolyphaseUpsampler,
  DAV_DspPolyphaseDownsampler;

type
  TCustomExciter = class(TDspSampleRatePersistent, IDspProcessor32,
    IDspProcessor64)
  private
    FFrequency   : Single;
    FGains       : array [0..3] of Single;
    FCrossover   : TButterworthSplitBandFilter;
    FHighpass    : TButterworthHighPassFilter;
    FUpsampler   : TPolyphaseUpsampler32;
    FDownsampler : TPolyphaseDownsampler32;

    procedure SetFrequency(const Value: Single);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure FrequencyChanged; virtual;
    procedure SampleRateChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer);
    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer);
    function ProcessSample32(Input: Single): Single; virtual;
    function ProcessSample64(Input: Double): Double; virtual;

    property InputLevel: Single read FGains[0] write FGains[0];
    property LowFrequencyLevel: Single read FGains[1] write FGains[1];
    property HighFrequencyLevel: Single read FGains[2] write FGains[2];
    property HarmonicsLevel: Single read FGains[3] write FGains[3];
    property Frequency: Single read FFrequency write SetFrequency;
  end;

  TExciter = class(TCustomExciter)
  published
    property InputLevel;
    property LowFrequencyLevel;
    property HighFrequencyLevel;
    property HarmonicsLevel;
    property Frequency;
    property SampleRate;
  end;

implementation

uses
  SysUtils, DAV_Math, DAV_Approximations;

{ TCustomExciter }

constructor TCustomExciter.Create;
begin
 inherited;
 FFrequency := 80;

 FGains[0] := 1;
 FGains[1] := 1;
 FGains[2] := 1;
 FGains[3] := 0;

 // create crossover filter
 FCrossover := TButterworthSplitBandFilter.Create(3);
 FCrossover.SampleRate := SampleRate;

 // create highpass filter
 FHighpass := TButterworthHighPassFilter.Create(1);
 FHighpass.SampleRate := SampleRate;

 // upsampler
 FUpsampler := TPolyphaseUpsampler32.Create;
 FUpsampler.NumberOfCoefficients := 6;

 // downsampler
 FDownsampler := TPolyphaseDownsampler32.Create;
 FDownsampler.NumberOfCoefficients := 6;
end;

destructor TCustomExciter.Destroy;
begin
 FreeAndNil(FCrossover);
 inherited;
end;

procedure TCustomExciter.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomExciter then
  with TCustomExciter(Dest) do
   begin
    inherited;
    FFrequency   := Self.FFrequency;
    FGains       := Self.FGains;

    FCrossover.Assign(Self.FCrossover);
    FHighpass.Assign(Self.FHighpass);
    FUpsampler.Assign(Self.FUpsampler);
    FDownsampler.Assign(Self.FDownsampler);
   end
 else inherited;
end;

procedure TCustomExciter.SetFrequency(const Value: Single);
begin
 if FFrequency <> Value then
  begin
   FFrequency := Value;
   FrequencyChanged;
  end;
end;

procedure TCustomExciter.FrequencyChanged;
begin
 FCrossover.Frequency := FFrequency;
 FHighpass.Frequency := 1.5 * FFrequency;
 Changed;
end;

procedure TCustomExciter.SampleRateChanged;
begin
 FCrossover.SampleRate := SampleRate;
 FHighpass.SampleRate := SampleRate;
 Changed;
end;

procedure TCustomExciter.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample    : Integer;
  Low, High : Single;
  Harmonic  : Single;
  IntData   : TDAV2SingleArray;
begin
 for Sample := 0 to SampleCount - 1 do
  begin
   FCrossover.ProcessSample32(FGains[0] * Data[Sample], Low, High);

   FUpsampler.ProcessSample(Low, IntData);
   IntData[0] := 0.125 * FastTanhMinError4(8 * IntData[0]);
   IntData[1] := 0.125 * FastTanhMinError4(8 * IntData[1]);
   Harmonic := FHighpass.ProcessSample64(FDownsampler.ProcessSample(IntData));

   Data[Sample] := FGains[1] * Low + FGains[2] * High - FGains[3] * Harmonic;
  end;
end;

procedure TCustomExciter.ProcessBlock64(const Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
var
  Sample    : Integer;
  Low, High : Double;
  Harmonic  : Double;
  IntData   : TDAV2SingleArray;
begin
 for Sample := 0 to SampleCount - 1 do
  begin
   FCrossover.ProcessSample64(FGains[0] * Data[Sample], Low, High);

   FUpsampler.ProcessSample(Low, IntData);
   IntData[0] := 0.125 * FastTanhMinError4(8 * IntData[0]);
   IntData[1] := 0.125 * FastTanhMinError4(8 * IntData[1]);
   Harmonic := FHighpass.ProcessSample64(FDownsampler.ProcessSample(IntData));

   Data[Sample] := FGains[1] * Low + FGains[2] * High - FGains[3] * Harmonic;
  end;
end;

function TCustomExciter.ProcessSample32(Input: Single): Single;
var
  Low, High, Harmonic : Single;
  Data : TDAV2SingleArray;
begin
 FCrossover.ProcessSample32(FGains[0] * Input, Low, High);

 FUpsampler.ProcessSample(Low, Data);
 Data[0] := 0.125 * FastTanhMinError4(8 * Data[0]);
 Data[1] := 0.125 * FastTanhMinError4(8 * Data[1]);
 Harmonic := FHighpass.ProcessSample64(FDownsampler.ProcessSample(Data));

 Result := FGains[1] * Low + FGains[2] * High - FGains[3] * Harmonic;
end;

function TCustomExciter.ProcessSample64(Input: Double): Double;
var
  Low, High, Harmonic : Double;
  Data : TDAV2SingleArray;
begin
 FCrossover.ProcessSample64(FGains[0] * Input, Low, High);

 FUpsampler.ProcessSample(Low, Data);
 Data[0] := 0.125 * DAV_Math.Tanh(8 * Data[0]);
 Data[1] := 0.125 * DAV_Math.Tanh(8 * Data[1]);
 Harmonic := FHighpass.ProcessSample64(FDownsampler.ProcessSample(Data));

 Result := FGains[1] * Low + FGains[2] * High - FGains[3] * Harmonic;
end;

initialization
  RegisterDspProcessor32(TExciter);
  RegisterDspProcessor64(TExciter);

end.
