unit DAV_DspVocoder;

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
  Classes, DAV_Common, DAV_Classes, DAV_DspFilter, DAV_DspFilterBasics,
  DAV_DspFilterChebyshev, DAV_DspFilterChebyshevType1;

const
  CNumFrequencies = 32;
  CThirdOctaveFrequencies: array [0..CNumFrequencies - 1] of Single =
    (16, 20, 25, 31, 40, 50, 63, 80, 100, 125, 160, 200, 250, 315, 400, 500,
    630, 800, 1000, 1250, 1600, 2000, 2500, 3150, 4000, 5000, 6300, 8000,
    10000, 12500, 16000, 20000);

  CBarkFrequencyScale: array [0..23] of Single = (100, 200, 300, 400, 510,
    630, 770, 920, 1080, 1270, 1480, 1720, 2000, 2320, 2700, 3150, 3700, 4400,
    5300, 6400, 7700, 9500, 12000, 15500);

type
  TCustomVocoder = class(TDspSampleRatePersistent)
  private
    procedure SetInputLevel(const Value: Double);
    procedure SetSynthLevel(const Value: Double);
    procedure SetVocoderLevel(const Value: Double);
    procedure SetAttack(const Value: Double);
    procedure SetRelease(const Value: Double);
  protected
    FVolFactors    : array [0..2] of Double;
    FAttack        : Double;
    FAttackFactor  : Double;
    FRelease       : Double;
    FReleaseFactor : Double;
    procedure SampleRateChanged; override;
    procedure AttackChanged; virtual;
    procedure ReleaseChanged; virtual;
    procedure CalculateAttackFactor; virtual;
    procedure CalculateReleaseFactor; virtual;
  public
    constructor Create; override;

    function ProcessSample(Input, Carrier: Single): Single; virtual; abstract;

    property InputLevel: Double read FVolFactors[0] write SetInputLevel;
    property SynthLevel: Double read FVolFactors[1] write SetSynthLevel;
    property VocoderLevel: Double read FVolFactors[2] write SetVocoderLevel;
    property Attack: Double read FAttack write SetAttack;    // in ms
    property Release: Double read FRelease write SetRelease; // in ms
    property SampleRate;
  end;

  TSimpleThirdOctaveVocoder = class(TCustomVocoder)
  private
    FSynthesisBW: Double;
    procedure SetSynthesisBW(const Value: Double);
  protected
    FAnalysisPeak      : array [0..CNumFrequencies - 1] of Single;
    FAnalysisFilters   : array [0..CNumFrequencies - 1] of TBasicBandpassFilter;
    FSynthesisFilters  : array [0..CNumFrequencies - 1] of TBasicBandpassFilter;
    procedure SampleRateChanged; override;
    procedure SynthesisBandwidthChanged; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    function ProcessSample(Input, Carrier: Single): Single; override;

    property SynthesisBandwidth: Double read FSynthesisBW write SetSynthesisBW;
  published
    property InputLevel;
    property SynthLevel;
    property VocoderLevel;
    property Attack;       // in ms
    property Release;      // in ms
    property SampleRate;
  end;

  TBarkScaleBand = record
    AnalysisLowpass   : TChebyshev1LowpassFilter;
    AnalysisHighpass  : TChebyshev1HighpassFilter;
    Peak              : Single;
    DownsampleFactor  : Integer;
    SynthesisLowpass  : TChebyshev1LowpassFilter;
    SynthesisHighpass : TChebyshev1HighpassFilter;
  end;

  TBarkScaleVocoder = class(TCustomVocoder)
  private
    FSynthesisBW: Double;
    FAnalysisOrder: Integer;
    procedure SetSynthesisBW(const Value: Double);
    procedure SetAnalysisOrder(Value: Integer);
  protected
    FBands               : array [0..23] of TBarkScaleBand;
    FDownSampler         : Integer;
    FDownSampleMax       : Integer;
    FTransitionBandwidth : Single;
    procedure SampleRateChanged; override;
    procedure AnalysisOrderChanged; virtual;
    procedure SynthesisBandwidthChanged; virtual;
    procedure UpdateFilters; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    function ProcessSample(Input, Carrier: Single): Single; override;

    property SynthesisBandwidth: Double read FSynthesisBW write SetSynthesisBW;
    property AnalysisOrder: Integer read FAnalysisOrder write SetAnalysisOrder;
  published
    property InputLevel;
    property SynthLevel;
    property VocoderLevel;
    property Attack;       // in ms
    property Release;      // in ms
    property SampleRate;
  end;

  TVocoderBand = record
    AnalysisLowpass   : TChebyshev1LowpassFilter;
    AnalysisHighpass  : TChebyshev1HighpassFilter;
    Peak              : Single;
    DownsampleFactor  : Integer;
  end;

  TVocoder = class(TCustomVocoder)
  private
    FSynthesisBW: Double;
    FAnalysisOrder: Integer;
    procedure SetSynthesisBW(const Value: Double);
    procedure SetAnalysisOrder(Value: Integer);
  protected
    FVocoderBands      : array [0..CNumFrequencies - 1] of TVocoderBand;
    FSynthesisFilters  : array [0..CNumFrequencies - 1] of TBasicBandpassFilter;
    FDownSampler       : Integer;
    FDownSampleMax     : Integer;
    procedure SampleRateChanged; override;
    procedure AnalysisOrderChanged; virtual;
    procedure SynthesisBandwidthChanged; virtual;
    procedure UpdateFilters; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    function ProcessSample(Input, Carrier: Single): Single; override;

    property SynthesisBandwidth: Double read FSynthesisBW write SetSynthesisBW;
    property AnalysisOrder: Integer read FAnalysisOrder write SetAnalysisOrder;
  published
    property InputLevel;
    property SynthLevel;
    property VocoderLevel;
    property Attack;       // in ms
    property Release;      // in ms
    property SampleRate;
  end;

implementation

uses
  Math, SysUtils, DAV_Approximations;

{ TCustomVocoder }

constructor TCustomVocoder.Create;
begin
 inherited;
 FAttack := 0.5;
 FRelease := 2;
 CalculateAttackFactor;
 CalculateReleaseFactor;
end;

procedure TCustomVocoder.AttackChanged;
begin
 CalculateAttackFactor;
 Changed;
end;

procedure TCustomVocoder.ReleaseChanged;
begin
 CalculateReleaseFactor;
 Changed;
end;

procedure TCustomVocoder.CalculateAttackFactor;
begin
 if FAttack = 0
  then FAttackFactor := 0
  else FAttackFactor := 1 - FastPower2MinError3(-1 / (FAttack * 0.001 * SampleRate));
end;

procedure TCustomVocoder.CalculateReleaseFactor;
begin
 if FRelease = 0
  then FReleaseFactor := 0
  else FReleaseFactor := FastPower2MinError3(-1 / (FRelease * 0.001 * SampleRate));
end;

procedure TCustomVocoder.SampleRateChanged;
begin
 CalculateAttackFactor;
 CalculateReleaseFactor;
 Changed;
end;

procedure TCustomVocoder.SetAttack(const Value: Double);
begin
 if FAttack <> Value then
  begin
   FAttack := Value;
   AttackChanged;
  end;
end;

procedure TCustomVocoder.SetRelease(const Value: Double);
begin
 if FRelease <> Value then
  begin
   FRelease := Value;
   ReleaseChanged;
  end;
end;

procedure TCustomVocoder.SetInputLevel(const Value: Double);
begin
 if FVolFactors[0] <> Value
  then FVolFactors[0] := Value;
end;

procedure TCustomVocoder.SetSynthLevel(const Value: Double);
begin
 if FVolFactors[1] <> Value
  then FVolFactors[1] := Value;
end;

procedure TCustomVocoder.SetVocoderLevel(const Value: Double);
begin
 if FVolFactors[2] <> Value
  then FVolFactors[2] := Value;
end;


{ TSimpleThirdOctaveVocoder }

constructor TSimpleThirdOctaveVocoder.Create;
var
  i: Integer;
const
  HalfThirdOctaveMulFak64: Double = 1.0905077326652576592070106557607;
begin
  inherited;

  for i := 0 to CNumFrequencies - 1 do
   begin
    FAnalysisFilters[i] := TBasicBandpassFilter.Create;
    with FAnalysisFilters[i] do
     begin
      SampleRate := Self.SampleRate;
      Gain := 0; Bandwidth := 0.707;
      Frequency := CThirdOctaveFrequencies[CNumFrequencies - i - 1];
     end;

    FSynthesisFilters[i] := TBasicBandpassFilter.Create;
    with FSynthesisFilters[i] do
     begin
      SampleRate := Self.SampleRate;
      Gain := 0; Bandwidth := 0.707;
      Frequency := CThirdOctaveFrequencies[CNumFrequencies - i - 1];
     end;
   end;
end;

destructor TSimpleThirdOctaveVocoder.Destroy;
var
  i: Integer;
begin
 for i := 0 to CNumFrequencies - 1 do
  begin
   FreeAndNil(FAnalysisFilters[i]);
   FreeAndNil(FSynthesisFilters[i]);
  end;
 inherited;
end;

function TSimpleThirdOctaveVocoder.ProcessSample(Input, Carrier: Single): Single;
var
  Band       : Integer;
  BandSignal : Double;
begin
 for Band := 0 to CNumFrequencies - 1 do
  begin
   BandSignal := FAnalysisFilters[Band].ProcessSample64(Input + 1E-32);

   if abs(BandSignal) > FAnalysisPeak[Band]
    then FAnalysisPeak[Band] := FAnalysisPeak[Band] + (abs(BandSignal) - FAnalysisPeak[Band]) * FAttackFactor
    else FAnalysisPeak[Band] := abs(BandSignal) + (FAnalysisPeak[Band] - abs(BandSignal)) * FReleaseFactor;
  end;

 // process vocoded signal
 Result := 0;
 for Band := 0 to CNumFrequencies - 1
  do Result := Result + FSynthesisFilters[Band].ProcessSample64(FAnalysisPeak[Band] * Carrier);

 Result := FVolFactors[2] * Result +
           FVolFactors[1] * Carrier +
           FVolFactors[0] * Input;
end;

procedure TSimpleThirdOctaveVocoder.SampleRateChanged;
var
  Band : Integer;
begin
 inherited;

 for Band := 0 to Length(FAnalysisFilters) - 1
  do FAnalysisFilters[Band].SampleRate := SampleRate;

 for Band := 0 to Length(FSynthesisFilters) - 1
  do FSynthesisFilters[Band].SampleRate := SampleRate;
end;

procedure TSimpleThirdOctaveVocoder.SetSynthesisBW(const Value: Double);
begin
 if FSynthesisBW <> Value then
  begin
   FSynthesisBW := Value;
   SynthesisBandwidthChanged;
  end;
end;

procedure TSimpleThirdOctaveVocoder.SynthesisBandwidthChanged;
var
  Band: Integer;
begin
 for Band := 0 to Length(FSynthesisFilters) - 1 do
  begin
   FSynthesisFilters[Band].Bandwidth := FSynthesisBW;
  end;
 Changed;
end;

{ TBarkScaleVocoder }

constructor TBarkScaleVocoder.Create;
var
  Band : Integer;
begin
  FDownSampler := 0;
  FTransitionBandwidth := 0.1;

  // create filters
  for Band := 0 to Length(CBarkFrequencyScale) - 1 do
   with FBands[Band] do
    begin
     AnalysisLowpass   := TChebyshev1LowpassFilter.Create(4);
     AnalysisHighpass  := TChebyshev1HighpassFilter.Create(4);
     SynthesisLowpass  := TChebyshev1LowpassFilter.Create(4);
     SynthesisHighpass := TChebyshev1HighpassFilter.Create(4);
    end;

  UpdateFilters;

  inherited Create;
end;

destructor TBarkScaleVocoder.Destroy;
var
  Band : Integer;
begin
 for Band := 0 to CNumFrequencies - 1 do
  with FBands[Band] do
   begin
    FreeAndNil(AnalysisLowpass);
    FreeAndNil(AnalysisHighpass);
    FreeAndNil(SynthesisLowpass);
    FreeAndNil(SynthesisHighpass);
   end;
 inherited;
end;

procedure TBarkScaleVocoder.UpdateFilters;
var
  Band : Integer;
  DS   : Integer;
begin
  DS := 0;

  // build band filters
  for Band := 0 to Length(CBarkFrequencyScale) - 1 do
   with FBands[Band] do
    begin
     if FDownSampler >= 0 then
      while (2 shl DS) * CBarkFrequencyScale[Band] < FTransitionBandwidth * SampleRate
       do DS := DS + 1;

     // setup filters
     with AnalysisLowpass do
      begin
       SampleRate := Self.SampleRate / (2 shl DS);
       SetFilterValues(CBarkFrequencyScale[Band] , 0, 0.1);
       CalculateCoefficients;
      end;

     with AnalysisHighpass do
      begin
       SampleRate := Self.SampleRate / (2 shl DS);
       SetFilterValues(CBarkFrequencyScale[Band], 0, 0.1);
       CalculateCoefficients;
      end;

     with SynthesisLowpass do
      begin
       SampleRate := Self.SampleRate / (2 shl DS);
       SetFilterValues(CBarkFrequencyScale[Band] , 0, 0.1);
       CalculateCoefficients;
      end;

     with SynthesisHighpass do
      begin
       SampleRate := Self.SampleRate / (2 shl DS);
       SetFilterValues(CBarkFrequencyScale[Band], 0, 0.1);
       CalculateCoefficients;
      end;

     DownsampleFactor := 1 shl DS;
    end;

  FDownSampleMax := 1 shl DS;
end;

function TBarkScaleVocoder.ProcessSample(Input, Carrier: Single): Single;
var
  Band       : Integer;
  Lowpassed  : Double;
  BandSignal : Double;
begin
 Lowpassed := Input;
 for Band := 0 to CNumFrequencies - 1 do
  with FBands[Band] do
   begin
    if (FDownSampler mod DownsampleFactor) <> 0
     then Break;

    BandSignal := AnalysisHighpass.ProcessSample64(Lowpassed + 1E-32);

    if abs(BandSignal) > Peak
     then Peak := Peak + (abs(BandSignal) - Peak) * FAttackFactor
     else Peak := abs(BandSignal) + (Peak - abs(BandSignal)) * FReleaseFactor;

    Lowpassed := AnalysisLowpass.ProcessSample64(Lowpassed + 1E-32);
   end;

 Inc(FDownSampler);
 if FDownSampler >= FDownSampleMax
  then FDownSampler := 0;

(*
 // process vocoded signal
 Result := 0;
 for i := 0 to CNumFrequencies - 1
  do Result := Result + FSynthesisFilters[i].ProcessSample64(FAnalysisPeak[i] * Carrier);
*)

 Result := FVolFactors[2] * Result +
           FVolFactors[1] * Carrier +
           FVolFactors[0] * Input;
end;

procedure TBarkScaleVocoder.SampleRateChanged;
begin
 inherited;
 UpdateFilters;
end;

procedure TBarkScaleVocoder.SetAnalysisOrder(Value: Integer);
begin
 if Value < 2
  then Value := 2
  else Value := ((Value shr 1) shl 1);
 if FAnalysisOrder <> Value then
  begin
   FAnalysisOrder := Value;
   AnalysisOrderChanged;
  end;
end;

procedure TBarkScaleVocoder.AnalysisOrderChanged;
var
  Band : Integer;
begin
 for Band := 0 to Length(FBands) - 1 do
  with FBands[Band] do
   begin
    AnalysisLowpass.Order  := FAnalysisOrder shr 1;
    AnalysisHighpass.Order := FAnalysisOrder shr 1;
   end;
 Changed;
end;

procedure TBarkScaleVocoder.SetSynthesisBW(const Value: Double);
begin
 if FSynthesisBW <> Value then
  begin
   FSynthesisBW := Value;
   SynthesisBandwidthChanged;
  end;
end;

procedure TBarkScaleVocoder.SynthesisBandwidthChanged;
//var
//  Band: Integer;
begin
(*
 for Band := 0 to Length(FSynthesisFilters) - 1 do
  begin
   // ToDo
  end;
*)
 Changed;
end;

{ TVocoder }

constructor TVocoder.Create;
var
  Band: Integer;
begin
  FDownSampler := 0;

  // create filters
  for Band := 0 to CNumFrequencies - 1 do
   begin
    with FVocoderBands[Band] do
     begin
      AnalysisLowpass  := TChebyshev1LowpassFilter.Create(6);
      AnalysisHighpass := TChebyshev1HighpassFilter.Create(6);
     end;
    FSynthesisFilters[Band] := TBasicBandpassFilter.Create;
   end;

  UpdateFilters;

  inherited Create;
end;

destructor TVocoder.Destroy;
var
  Band: Integer;
begin
 for Band := 0 to CNumFrequencies - 1 do
  begin
   with FVocoderBands[Band] do
    begin
     FreeAndNil(AnalysisLowpass);
     FreeAndNil(AnalysisHighpass);
    end;
   FreeAndNil(FSynthesisFilters[Band]);
  end;
 inherited;
end;

procedure TVocoder.UpdateFilters;
var
  Band : Integer;
  DS   : Integer;
const
  HalfThirdOctaveMulFak64: Double = 1.0905077326652576592070106557607;
begin
  DS := 0;

 for Band := 0 to CNumFrequencies - 1 do
  begin
   with FVocoderBands[Band] do
    begin
     with AnalysisLowpass do
      begin
       SampleRate := Self.SampleRate;
       SetFilterValues(min(0.5 * Samplerate, HalfThirdOctaveMulFak64 * (CThirdOctaveFrequencies[CNumFrequencies - Band - 1])), 0, 0.1);
       if FDownSampler >= 0 then
        while (2 shl DS) * Frequency < 0.1 * SampleRate
         do Inc(DS);
       CalculateCoefficients;
      end;

     with AnalysisHighpass do
      begin
       SampleRate := Self.SampleRate;
       SetFilterValues((CThirdOctaveFrequencies[CNumFrequencies - Band - 1]) / HalfThirdOctaveMulFak64, 0, 0.1);
       CalculateCoefficients;
      end;

     DownsampleFactor := DS; 
    end;

   with FSynthesisFilters[Band] do
    begin
     SampleRate := Self.SampleRate;
     Gain := 0;
     Bandwidth := 0.707;
     Frequency := CThirdOctaveFrequencies[CNumFrequencies - Band - 1];
    end;
  end;

 FDownSampleMax := DS;
end;

function TVocoder.ProcessSample(Input, Carrier: Single): Single;
var
  Band       : Integer;
  Lowpassed  : Double;
  BandSignal : Double;
begin
 Lowpassed := Input;
 for Band := 0 to CNumFrequencies - 1 do
  with FVocoderBands[Band] do
   begin
    if (FDownSampler mod DownsampleFactor) <> 0
     then Break;

    Lowpassed := AnalysisLowpass.ProcessSample64(Lowpassed + 1E-32);
    BandSignal := AnalysisHighpass.ProcessSample64(Lowpassed + 1E-32);

    if abs(BandSignal) > Peak
     then Peak := Peak + (abs(BandSignal) - Peak) * FAttackFactor
     else Peak := abs(BandSignal) + (Peak - abs(BandSignal)) * FReleaseFactor;
   end;
 Inc(FDownSampler);
 if FDownSampler >= FDownSampleMax
  then FDownSampler := 0;

 // process vocoded signal
 Result := 0;
 for Band := 0 to CNumFrequencies - 1
  do Result := Result + FSynthesisFilters[Band].ProcessSample64(FVocoderBands[Band].Peak * Carrier);

 Result := FVolFactors[2] * Result +
           FVolFactors[1] * Carrier +
           FVolFactors[0] * Input;
end;

procedure TVocoder.SampleRateChanged;
begin
 inherited;

 UpdateFilters;
end;

procedure TVocoder.SetAnalysisOrder(Value: Integer);
begin
 if Value < 2
  then Value := 2
  else Value := ((Value shr 1) shl 1);
 if FAnalysisOrder <> Value then
  begin
   FAnalysisOrder := Value;
   AnalysisOrderChanged;
  end;
end;

procedure TVocoder.AnalysisOrderChanged;
var
  Band : Integer;
begin
 for Band := 0 to Length(FVocoderBands) - 1 do
  with FVocoderBands[Band] do
   begin
    AnalysisLowpass.Order := FAnalysisOrder shr 1;
    AnalysisHighpass.Order := FAnalysisOrder shr 1;
   end;
 Changed;
end;

procedure TVocoder.SetSynthesisBW(const Value: Double);
begin
 if FSynthesisBW <> Value then
  begin
   FSynthesisBW := Value;
   SynthesisBandwidthChanged;
  end;
end;

procedure TVocoder.SynthesisBandwidthChanged;
var
  Band: Integer;
begin
 for Band := 0 to Length(FSynthesisFilters) - 1 do
  begin
   FSynthesisFilters[Band].Bandwidth := FSynthesisBW;
  end;
 Changed;
end;

end.
