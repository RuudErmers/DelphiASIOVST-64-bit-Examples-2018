unit DAV_ModularOscSquare;

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_Complex, DAV_ModularBaseComponent, DAV_ModularBaseOsc;

type
  TDspOscSquare = class(TDspBaseOsc)
  protected
    procedure Process(var Data: Single; const channel: integer); override;
    procedure Process(var Data: Double; const channel: integer); override;
  end;

implementation

procedure TDspOscSquare.Process(var Data: Single; const channel: integer);
begin
  Data := FPosition[channel].Re * FAngle.Re-FPosition[channel].Im*FAngle.Im;
  FPosition[channel].Im := FPosition[channel].Im*FAngle.Re+FPosition[channel].Re*FAngle.Im;
  FPosition[channel].Re := Data;

  if Data > 0 then Data := 1 else Data := -1;
  Data := Data * fAmplitude + FDCOffset;
end;

procedure TDspOscSquare.Process(var Data: Double; const channel: integer);
begin
  Data := FPosition[channel].Re*FAngle.Re-FPosition[channel].Im*FAngle.Im;
  FPosition[channel].Im := FPosition[channel].Im*FAngle.Re+FPosition[channel].Re*FAngle.Im;
  FPosition[channel].Re := Data;

  if Data>0 then Data := 1 else Data := -1;
  Data := Data * fAmplitude + FDCOffset;
end;

end.
