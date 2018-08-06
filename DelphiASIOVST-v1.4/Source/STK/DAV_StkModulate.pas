unit DAV_StkModulate;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{  STK periodic/random modulator.

   This class combines random and periodic modulations to give a nice, natural
   human modulation function.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkSubNoise, DAV_StkOnePole, DAV_StkLfo;

type
  TStkModulate = class(TStk)
  private
    FVibratoRate: Single;
    procedure SetVibratoRate(const Value: Single); // in Hz
    procedure SetVibratoGain(const Value: Single);
    procedure SetRandomGain(const Value: Single);
    function GetVibratoRate: Single;
  protected
    FVibrato     : TStkLFO;
    FNoise       : TStkSubNoise;
    FFilter      : TStkOnePole;
    FVibratoGain : Single;
    FRandomGain  : Single;
    FLastOutput  : Single;
  public
    constructor Create(const SampleRate: Single); override;
    destructor Destroy; override;

    // Reset internal state.
    procedure Reset;

    // Compute one output sample.
    function Tick: Single; overload;

    // Return \e VectorSize outputs in \e Vector.
    procedure Tick(const Data: PDavSingleFixedArray; const SampleFrames: Integer); overload;

  published
    property VibratoRate: Single read GetVibratoRate write SetVibratoRate;
    property VibratoGain: Single read FVibratoRate write SetVibratoGain;
    property RandomGain: Single read FRandomGain write SetRandomGain;
    property LastOutput: Single read FLastOutput;
  end;

implementation

uses
  SysUtils;

constructor TStkModulate.Create;
begin
  inherited Create(SampleRate);
  FVibrato := TStkLFO.Create(SampleRate);
  FVibrato.Frequency := 6.0;
  FVibratoGain := 0.04;

  FNoise := TStkSubNoise.Create(SampleRate, 330);
  FRandomGain := 0.05;

  FFilter := TStkOnePole.Create(SampleRate, 0.999);
  FFilter.Gain := FRandomGain;
end;

destructor TStkModulate.Destroy;
begin
  FreeAndNil(FVibrato);
  FreeAndNil(FNoise);
  FreeAndNil(FFilter);
  inherited Destroy;
end;

procedure TStkModulate.Reset;
begin
  FLastOutput := 0.0;
end;

function TStkModulate.GetVibratoRate: Single;
begin
 result := FVibrato.Frequency;
end;

procedure TStkModulate.SetVibratoRate(const Value: Single);
begin
 if VibratoRate <> Value then
  begin
   FVibrato.Frequency := Value;
  end;
end;

procedure TStkModulate.SetVibratoGain(const Value: Single);
begin
 if FVibratoGain <> Value then
  begin
   FVibratoGain := Value;
  end;
end;

procedure TStkModulate.SetRandomGain(const Value: Single);
begin
 if FRandomGain <> Value then
  begin
   FRandomGain := Value;
   FFilter.Gain := FRandomGain;
  end;
end;

function TStkModulate.Tick: Single;
begin
  // Compute periodic and random modulations.
  FLastOutput := FVibratoGain * FVibrato.Tick;
  FLastOutput := FLastOutput + FFilter.Tick(FNoise.Tick);
  Result := FLastOutput;
end;

procedure TStkModulate.Tick(const Data: PDavSingleFixedArray;
  const SampleFrames: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleFrames - 1
  do Data^[Sample] := Tick; 
end;

end.
