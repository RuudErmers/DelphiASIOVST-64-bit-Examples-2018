unit DAV_StkPitchShift;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK simple pitch shifter effect class.

  This class implements a simple pitch shifter using delay lines.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Types, DAV_StkCommon, DAV_StkDelayl;

type
  TStkPitchShifter = class(TStk)
  private
    procedure SetShift(const Value: Single);
    procedure SetEffectMix(const Value: Single);
  protected
    FDelayLine    : array[0..1] of TStkDelayL;
    FEffectMix    : Single;
    FRate, FShift : Single;
    FLastOutput   : Single;
    FDelay, FEnv  : array[0..1] of Single;
    procedure ShiftChanged; virtual;
  public
    constructor Create(const SampleRate: Single); override;
    destructor Destroy; override;
    procedure Clear;
    function Tick(const Input: Single): Single; overload;
    procedure Tick(const Input, Output: PDAVSingleFixedArray; const SampleFrames: Integer); overload; virtual;

    property LastOutput: Single read FLastOutput;
    property EffectMix: Single read FEffectMix write SetEffectMix;
    property Shift: Single read FShift write SetShift;
  end;

implementation

uses
  SysUtils, DAV_Common;

constructor TStkPitchShifter.Create;
begin
  inherited Create(SampleRate);
  FDelay[0] := 12;
  FDelay[1] := 512;
  FDelayLine[0] := TStkDelayL.Create(SampleRate, FDelay[0], 1024);
  FDelayLine[1] := TStkDelayL.Create(SampleRate, FDelay[1], 1024);
  FEffectMix := 0.5;
  FShift := 0;
  FRate := 1.0;
end;

destructor TStkPitchShifter.Destroy;
begin
 FreeAndNil(FDelayLine[0]);
 FreeAndNil(FDelayLine[1]);
 inherited Destroy;
end;

procedure TStkPitchShifter.Clear;
begin
  FDelayLine[0].Clear;
  FDelayLine[1].Clear;
  FLastOutput := 0.0;
end;

procedure TStkPitchShifter.SetEffectMix(const Value: Single);
begin
  FEffectMix := Limit(Value, 0, 1);
end;

procedure TStkPitchShifter.SetShift(const Value: Single);
begin
 if Shift <> Value then
  begin
   FShift := Value;
   ShiftChanged;
  end;
end;

procedure TStkPitchShifter.ShiftChanged;
begin
 if (FShift < 1.0) then FRate := 1.0 - FShift else
 if (FShift > 1.0) then FRate := 1.0 - FShift
  else
   begin
    FRate := 0.0;
    FDelay[0] := 512;
   end;
end;

function TStkPitchShifter.Tick(const Input: Single): Single;
begin
  FDelay[0] := FDelay[0] + FRate;
  while (FDelay[0] > 1012) do FDelay[0] := FDelay[0] - 1000;
  while (FDelay[0] < 12) do FDelay[0] := FDelay[0] + 1000;

  FDelay[1] := FDelay[0] + 500;
  while (FDelay[1] > 1012) do FDelay[1] := FDelay[1] - 1000;
  while (FDelay[1] < 12) do FDelay[1] := FDelay[1] + 1000;
  FDelayLine[0].Delay := round(FDelay[0]);
  FDelayLine[1].Delay := round(FDelay[1]);
  FEnv[1] := abs(FDelay[0] - 512) * 0.002;
  FEnv[0] := 1.0 - FEnv[1];
  FLastOutput := FEnv[0] * FDelayLine[0].Tick(input);
  FLastOutput := FLastOutput + FEnv[1] * FDelayLine[1].Tick(input);
  FLastOutput := FLastOutput * FEffectMix;
  FLastOutput := FLastOutput + (1.0 - FEffectMix) * input;
  Result := FLastOutput;
end;

procedure TStkPitchShifter.Tick(const Input, Output: PDAVSingleFixedArray; const SampleFrames: Integer);
var
  Sample: Integer;
begin
  for Sample := 0 to SampleFrames - 1
   do Output^[Sample] := Tick(Input^[Sample])
end;

end.
