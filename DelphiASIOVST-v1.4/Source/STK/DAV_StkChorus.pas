unit DAV_StkChorus;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Types, DAV_StkCommon, DAV_StkDelayl, DAV_StkLfo;

type
  TStkChorus = class(TStk)
  private
    function GetLastOutput: Single;
    function GetModFrequency: Single;
    procedure SetModDepth(const Value: Single);
    procedure SetModFrequency(const Value: Single);
    procedure SetEffectMix(const Value: Single);
  protected
    FDelayLine   : array[0..1] of TStkDelayL;
    FBaseLength  : Single;
    FModDepth    : Single;
    FEffectMix   : Single;
    FLastOutput  : array[0..1] of Single;
    FMods        : array[0..1] of TStkLFO;
    procedure SampleRateChanged; override;
  public
    constructor Create(const SampleRate, BaseDelay: Single); reintroduce; overload; virtual;
    constructor Create(const SampleRate: Single = 44100); overload; override;
    destructor Destroy; override;
    procedure Clear;
    function Tick(const Input: Single): Single; overload;
    procedure Tick(const Input, Output: PDAVSingleFixedArray; const SampleFrames: Integer); overload;

    property LastOutput: Single read GetLastOutput;
    property LastOutputLeft: Single read FLastOutput[0];
    property LastOutputRight: Single read FLastOutput[1];

    property ModDepth: Single read FModDepth write SetModDepth;
    property ModFrequency: Single read GetModFrequency write SetModFrequency;
    property EffectMix: Single read FEffectMix write SetEffectMix;
  end;

implementation

uses
  SysUtils, DAV_Common;

constructor TStkChorus.Create(const SampleRate, BaseDelay: Single);
begin
  inherited Create(SampleRate);
  FDelayLine[0] := TStkDelayL.Create(SampleRate, round(BaseDelay), round(BaseDelay * 1.414) + 2);
  FDelayLine[1] := TStkDelayL.Create(SampleRate, round(BaseDelay), round(BaseDelay) + 2);
  FBaseLength := BaseDelay;

  FMods[0] := TStkLFO.Create(SampleRate);
  FMods[1] := TStkLFO.Create(SampleRate);
  FMods[0].Frequency := 0.2;
  FMods[1].Frequency := 0.222222;
  FModDepth := 0.05;
  FEffectMix := 0.5;
  Clear;
end;

constructor TStkChorus.Create(const SampleRate: Single = 44100);
begin
 Create(SampleRate, SampleRate);
end;

destructor TStkChorus.Destroy;
begin
  FreeAndNil(FDelayLine[0]);
  FreeAndNil(FDelayLine[1]);
  FreeAndNil(FMods[0]);
  FreeAndNil(FMods[1]);
  inherited Destroy;
end;

procedure TStkChorus.Clear;
begin
  FDelayLine[0].Clear;
  FDelayLine[1].Clear;
  FLastOutput[0] := 0.0;
  FLastOutput[1] := 0.0;
end;

procedure TStkChorus.SampleRateChanged;
begin
 inherited;
 if assigned(FMods[0]) then FMods[0].SampleRate := SampleRate;
 if assigned(FMods[1]) then FMods[1].SampleRate := SampleRate;
end;

procedure TStkChorus.SetEffectMix(const Value: Single);
begin
 FEffectMix := Limit(Value, 0, 1);
end;

procedure TStkChorus.SetModDepth(const Value: Single);
begin
 if FModDepth <> Value then
  begin
   FModDepth := Value;
   FBaseLength := SampleRate * FModDepth;
  end;
end;

procedure TStkChorus.SetModFrequency(const Value: Single);
begin
 if FMods[0].Frequency <> Value then
  begin
   FMods[0].Frequency := Value;
   FMods[1].Frequency := Value * 1.1111;
  end;
end;

function TStkChorus.GetLastOutput: Single;
begin
  Result := (FLastOutput[0] + FLastOutput[1]) * 0.5;
end;

function TStkChorus.GetModFrequency: Single;
begin
 result := FMods[0].Frequency
end;

function TStkChorus.Tick(const Input: Single): Single;
begin
  FDelayLine[0].Delay := FBaseLength * 0.707 * (1.0 + FMods[0].Tick);
  FDelayLine[1].Delay := FBaseLength * 0.5 * (1.0 - FMods[1].Tick);
  FLastOutput[0] := Input * (1.0 - FEffectMix);
  FLastOutput[0] := FLastOutput[0] + FEffectMix * FDelayLine[0].Tick(Input);
  FLastOutput[1] := Input * (1.0 - FEffectMix);
  FLastOutput[1] := FLastOutput[1] + FEffectMix * FDelayLine[1].Tick(Input);
  Result := (FLastOutput[0] + FLastOutput[1]) * 0.5;
end;

procedure TStkChorus.Tick(const Input, Output: PDAVSingleFixedArray; const SampleFrames: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := Tick(Input^[Sample]);
end;

end.
