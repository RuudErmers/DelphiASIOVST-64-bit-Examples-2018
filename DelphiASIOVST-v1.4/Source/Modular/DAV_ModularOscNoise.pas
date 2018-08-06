unit DAV_ModularOscNoise;

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_Complex, DAV_ModularBaseComponent, DAV_ModularBaseOsc;

type
  TDspOscNoise = class(TDspBaseOsc)
  protected
    procedure Process(var Data: Single; const channel: integer); override;
    procedure Process(var Data: Double; const channel: integer); override;
  public
    procedure Init;  override;
  end;

implementation

procedure TDspOscNoise.Init;
begin
  inherited;
  Randomize;
end;

procedure TDspOscNoise.Process(var Data: Single; const channel: integer);
begin
  Data := (2 * Random - 1) * FAmplitude + FDCOffset;
end;

procedure TDspOscNoise.Process(var Data: Double; const channel: integer);
begin
  Data := (2 * Random - 1) * FAmplitude + FDCOffset;
end;

end.
