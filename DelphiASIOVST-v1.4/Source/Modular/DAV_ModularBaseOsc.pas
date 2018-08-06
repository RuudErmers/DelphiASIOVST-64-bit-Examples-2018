unit DAV_ModularBaseOsc;

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_Complex, DAV_ModularBaseComponent;

type
  TDspBaseOsc = class(TDspBaseComponent)
  protected
    FDCOffset  : Single;
    FFrequency : Single;
    FAmplitude : Single;
    FAngle     : TComplex64;
    FPosition  : TDAVComplexDoubleDynArray;

    procedure SetAmplitude(const Value: Single); virtual;
    procedure SetDCOffset(const Value: Single);  virtual;
    procedure SetFrequency(const Value: Single); virtual;

    procedure SampleRateChanged; override;
    procedure FrequencyChanged;  virtual;
    procedure ChannelsChanged;   override; 
    procedure BeforeDestroy;     override;

    procedure Process(var Data: Single; const Channel: Integer); overload; virtual; abstract;
    procedure Process(var Data: Double; const Channel: Integer); overload; virtual; abstract;
  public
    procedure Init;  override;
    procedure Reset; override;
  published
    property Amplitude: Single read FAmplitude write SetAmplitude; //  0..1
    property DCOffset:  Single read FDCOffset  write SetDCOffset;  // -1..1
    property Frequency: Single read FFrequency write SetFrequency; //  0..Samplerate
  end;

implementation

uses
  DAV_Math;

{ TDspBaseOsc }

procedure TDspBaseOsc.Init;
begin
  FFrequency   := 440;
  FDCOffset    := 0;
  FAmplitude   := 1;

  fStdProcessS  := Process;
  fStdProcessD  := Process;
  ChannelsChanged;
  FrequencyChanged;
end;

procedure TDspBaseOsc.Reset;
begin
  ChannelsChanged;
  FrequencyChanged;
end;

procedure TDspBaseOsc.BeforeDestroy;
begin
  SetLength(FPosition, 0);
end;

procedure TDspBaseOsc.SampleRateChanged;
begin
  FrequencyChanged;
  inherited;
end;

procedure TDspBaseOsc.FrequencyChanged;
begin
  GetSinCos(2 * Pi * FFrequency / FSampleRate, FAngle.Im, FAngle.Re);
end;

procedure TDspBaseOsc.ChannelsChanged;
var
  i: Integer;
begin
  SetLength(FPosition, FChannels);

  for i := 0 to fChannels - 1 do
  begin
    FPosition[i].Re := 0;
    FPosition[i].Im := -1;
  end;
end;

procedure TDspBaseOsc.SetAmplitude(const Value: Single);
begin
  FAmplitude := Value;
end;

procedure TDspBaseOsc.SetDCOffset(const Value: Single);
begin
  FDCOffset := Value;
end;

procedure TDspBaseOsc.SetFrequency(const Value: Single);
begin
  if FFrequency <> Value then
  begin
    FFrequency := Value;
    FrequencyChanged;
  end;
end;

end.
