unit SimpleSamplerVoice;

interface

uses
  DAV_VSTModule, DAV_Complex;

{$i Consts.inc}

type
  TSimpleSamplerVoice = class(TObject)
  private
    FMidiKeyNr  : Integer;
    FVelocity   : Integer;
    FSampleRate : Single;
    FSampleReci : Single;
    FFrequency  : Single;
    FAmplitude  : Single;
    FVSTModule  : TVSTModule;
    FSamplePos  : Integer;
    FSampleFrac : Single;
    FSampleInc  : Single;
    FMem        : array [0..3] of Single;

    FAngle,
    FPosition   : TComplex64;
    procedure SetSampleRate(const Value: Single);
    procedure SetFrequency(const Value: Single);
  protected
    procedure SamplerateChanged; virtual;
    procedure FrequencyChanged; virtual;
  public
    constructor Create(theModule: TVSTModule);
    destructor Destroy; override;
    procedure NoteOn(Frequency, Amplitude: Single);
    procedure NoteOff;
    function Process: Single; virtual;

    property Frequency: Single read FFrequency write SetFrequency;
    property SampleRate: Single read FSampleRate write SetSampleRate;
    property MidiKeyNr: Integer read FMidiKeyNr write FMidiKeyNr;
    property Velocity: Integer read FVelocity write FVelocity;
  end;

implementation

uses
  SysUtils, DAV_Common, DAV_Math, DAV_Types, DAV_DspInterpolation,
  SimpleSamplerModule;

{ TSimpleSamplerVoice }

constructor TSimpleSamplerVoice.Create(theModule: TVSTModule);
begin
 FVSTModule       := theModule;
 if theModule.SampleRate = 0
  then SampleRate := 44100
  else SampleRate := theModule.SampleRate;
 FPosition.Re     :=  0;
 FPosition.Im     := -1;
 FSamplePos       :=  0;
 FSampleFrac      :=  0;
 FSampleInc       :=  0;
end;

destructor TSimpleSamplerVoice.Destroy;
begin
 inherited;
end;

procedure TSimpleSamplerVoice.SetSampleRate(const Value: Single);
begin
 if (Value <= 0)
  then raise Exception.Create('Samplerate must be larger than 0!');
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SamplerateChanged;
  end;
end;

function TSimpleSamplerVoice.Process: Single;
begin
 if TVSTSSModule(FVSTModule).SampleLength <= 0 then
  begin
   result := FPosition.Re * FAngle.Re - FPosition.Im * FAngle.Im;
   FPosition.Im := FPosition.Im * FAngle.Re + FPosition.Re * FAngle.Im;
   FPosition.Re := result; result := result * FAmplitude;
  end
 else
  begin
   Result := FAmplitude * Hermite32_asm(FSampleFrac, @FMem[0]);
   FSampleFrac := FSampleFrac + FSampleInc;
   while FSampleFrac >= 1 do
     begin
      inc(FSamplePos);
      if FSamplePos >= TVSTSSModule(FVSTModule).SampleLength
       then FSamplePos := 0;
      FSampleFrac := FSampleFrac - 1;
      Move(FMem[1], FMem[0], 12);
      FMem[3] := TVSTSSModule(FVSTModule).Sample[FSamplePos];
     end;
  end;
end;

procedure TSimpleSamplerVoice.SetFrequency(const Value: Single);
begin
 if (Value <= 0)
  then raise Exception.Create('Frequency must be larger than 0!');
 if FFrequency <> Value then
  begin
   FFrequency := Value;
   FrequencyChanged;
  end;
end;

procedure TSimpleSamplerVoice.FrequencyChanged;
begin
 FSampleInc := Frequency / 440;
 GetSinCos(2 * Pi * FFrequency * FSampleReci, FAngle.Im, FAngle.Re);
end;

procedure TSimpleSamplerVoice.SamplerateChanged;
begin
 FSampleReci := 1 / FSampleRate; 
end;

procedure TSimpleSamplerVoice.NoteOn(Frequency, Amplitude: Single);
begin
 SetFrequency(Frequency);
 FAmplitude := Amplitude;
end;

procedure TSimpleSamplerVoice.NoteOff;
begin
 FAmplitude := 0;
end;

end.
