unit DAV_ModularOscAbsSine;

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Types, DAV_Complex, DAV_ModularBaseComponent, DAV_ModularBaseOsc;

type
  TDspOscAbsSine = class(TDspBaseOsc)
  protected
    procedure Process(var Data: Single; const Channel: integer); override;
    procedure Process(var Data: Double; const Channel: integer); override;
  end;

implementation

uses
  DAV_Common;

procedure TDspOscAbsSine.Process(var Data: Single; const Channel: integer);
begin
  Data:=FPosition[Channel].Re*FAngle.Re-FPosition[Channel].Im*FAngle.Im;
  FPosition[Channel].Im:=FPosition[Channel].Im*FAngle.Re+FPosition[Channel].Re*FAngle.Im;
  FPosition[Channel].Re:=Data;
  FastAbs(Data);
  Data := (Data*2-1) * FAmplitude + FDCOffset;
end;

procedure TDspOscAbsSine.Process(var Data: Double; const Channel: integer);
begin
  Data:=FPosition[Channel].Re*FAngle.Re-FPosition[Channel].Im*FAngle.Im;
  FPosition[Channel].Im:=FPosition[Channel].Im*FAngle.Re+FPosition[Channel].Re*FAngle.Im;
  FPosition[Channel].Re:=Data;
  FastAbs(Data);
  Data := (Data*2-1) * FAmplitude + FDCOffset;
end;

end.
