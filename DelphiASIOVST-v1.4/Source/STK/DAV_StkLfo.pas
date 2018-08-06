unit DAV_StkLFO;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_StkCommon;

type
  TStkLFO = class(TStk)
  private
    procedure SetPhase(Value: Single);
    procedure SetPhaseOffset(Value: Single);
    procedure SetFreq(const Value: Single);
  protected
    FWave    : Integer;
    FCnt     : Single;
    FTmp     : Single;
    FPOfs    : Single;
    FPhase   : Single;
    FFreq    : Single;
    FFreqInv : Single;
    procedure PhaseOffsetChanged; virtual;
    procedure PhaseChanged; virtual;
    procedure FrequencyChanged; virtual;
  public
    constructor Create(const SampleRate: Single); override;
    destructor Destroy; override;
    procedure Reset;
    procedure AddPhaseOffset(Value: Single);
    function Tick: Single; virtual;

    property ActiveWave: Integer read FWave write FWave;
    property Phase: Single read FPhase write SetPhase;
    property PhaseOffset: Single read FPofs write SetPhaseOffset;
    property Frequency: Single read FFreq write SetFreq;
  end;

implementation


constructor TStkLFO.Create(const SampleRate: Single);
begin
  inherited Create(SampleRate);
  FFreq := 1;
  FPOfs := 0;
  FPhase := 0;
  FWave := 0;
  FrequencyChanged;
  PhaseChanged;
end;

destructor TStkLFO.Destroy;
begin
  inherited Destroy;
end;

function TStkLFO.Tick: Single;
var
  y, j: Single;
begin
  j := SampleRate * FFreqInv;
  FPhase := FCnt * FFreq * FSampleRateInv;
  case FWave of
    0 : y := sin(2 * pi * FPhase);
    1 : if FPhase < 0.5 then
        y := 2 * FPhase
      else
        y := 2 * FPhase - 2;
    2 : if FPhase < 0.5 then
        y := 1
      else
        y := -1;
    3 : if (FPhase >= 0.25) and (FPhase < 0.75) then
        y := -4 * (FPhase + 0.25) + 3
      else if (FPhase < 0.25) then
        y := 4 * (FPhase + 0.25) - 1
      else
        y := 4 * (FPhase + 0.25) - 5;
    4 : y := FTmp;
  else y := random * 2 - 1;
  end;
  FCnt := FCnt + 1;
  while (FCnt >= j) do
   begin
    FCnt := FCnt - j;
    FTmp := random * 2 - 1;
   end;
  Result := y;
end;

procedure TStkLFO.SetPhaseOffset(Value: Single);
begin
 while Value >= 1 do Value := Value - 1;
 while Value < 0 do Value := Value + 1;
 if FPofs <> Value then
  begin
   FPofs := Value;
   PhaseOffsetChanged;
  end;
end;

procedure TStkLFO.PhaseOffsetChanged;
begin
 Phase := FPofs + FPhase;
end;

procedure TStkLFO.SetFreq(const Value: Single);
begin
 if FFreq <> Value then
  begin
   FFreq := Value;
   FrequencyChanged;
  end;
end;

procedure TStkLFO.FrequencyChanged;
begin
 FFreqInv := 1 / FFreq;
end;

procedure TStkLFO.SetPhase(Value: Single);
begin
 while Value >= 1 do Value := Value - 1;
 while Value < 0 do Value := Value + 1;
 if FPhase <> Value then
  begin
   FPhase := Value;
   PhaseChanged;
  end;
end;

procedure TStkLFO.PhaseChanged;
begin
 FCnt := FPhase * SampleRate / FFreq;
end;

procedure TStkLFO.Reset;
begin
  Phase := FPofs;
end;

procedure TStkLFO.AddPhaseOffset(Value: Single);
begin
  SetPhase(FPhase + Value);
end;

end.

