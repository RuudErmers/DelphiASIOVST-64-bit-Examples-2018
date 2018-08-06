unit DAV_ModularOscSaw;

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_Complex, DAV_ModularBaseComponent, DAV_ModularBaseOsc;

type
  TDspOscSaw = class(TDspBaseOsc)
  protected
    procedure FrequencyChanged; override; 
    procedure Process(var Data: Single; const channel: integer); override;
    procedure Process(var Data: Double; const channel: integer); override;
  end;

implementation

uses
  Math;

procedure TDspOscSaw.FrequencyChanged;
begin
  FAngle.Re := FFrequency / FSampleRate
end;

procedure TDspOscSaw.Process(var Data: Single; const channel: integer);
begin
  FPosition[channel].Re := FPosition[channel].Re + FAngle.Re;
  if FPosition[channel].Re > 1 then
    FPosition[channel].Re := FastFractional(FPosition[channel].Re);

  Data := (1 - FPosition[channel].Re * 2) * FAmplitude + FDCOffset;
end;

procedure TDspOscSaw.Process(var Data: Double; const channel: integer);
begin
  FPosition[channel].Re := FPosition[channel].Re + FAngle.Re;
  if FPosition[channel].Re > 1 then
    FPosition[channel].Re := FastFractional(FPosition[channel].Re);

  Data := (1 - FPosition[channel].Re * 2) * FAmplitude + FDCOffset;
end;

end.
