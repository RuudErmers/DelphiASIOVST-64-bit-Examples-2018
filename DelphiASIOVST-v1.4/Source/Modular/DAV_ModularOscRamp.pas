unit DAV_ModularOscRamp;

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_Complex, DAV_ModularBaseComponent, DAV_ModularBaseOsc;

type
  TDspOscRamp = class(TDspBaseOsc)
  protected
    procedure FrequencyChanged; override; 
    procedure Process(var Data: Single; const Channel: integer); override;
    procedure Process(var Data: Double; const Channel: integer); override;
  end;

implementation


procedure TDspOscRamp.FrequencyChanged;
begin
  FAngle.Re:=FFrequency/FSampleRate
end;

procedure TDspOscRamp.Process(var Data: Single; const Channel: integer);
begin
  FPosition[Channel].Re := FPosition[Channel].Re + FAngle.Re;
  if FPosition[Channel].Re > 1 then
    FPosition[Channel].Re := FastFractional(FPosition[Channel].Re);

  Data := (FPosition[Channel].Re * 2 - 1) * fAmplitude + FDCOffset;
end;

procedure TDspOscRamp.Process(var Data: Double; const Channel: integer);
begin
  FPosition[Channel].Re := FPosition[Channel].Re + FAngle.Re;
  if FPosition[Channel].Re > 1 then
    FPosition[Channel].Re := FastFractional(FPosition[Channel].Re);

  Data:=(FPosition[Channel].Re * 2 - 1) * fAmplitude + FDCOffset;
end;

end.
