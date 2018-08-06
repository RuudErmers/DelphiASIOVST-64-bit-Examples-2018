unit SineSynthVoice;

interface

uses
  DAV_VSTModule, DAV_Complex, DAV_DspSimpleOscillator;

{$i Consts.inc}

type
  TSineSynthVoice = class(TObject)
  private
    FOscillator : TSimpleOscillator;
    FMidiKeyNr  : Integer;
    FVelocity   : Integer;
    FVSTModule  : TVSTModule;
    FSampleRate : Single;
    FFrequency  : Single;
    procedure SetSampleRate(const Value: Single);
    procedure SetFrequency(const Frequency: Single);
  protected
    procedure FrequencyChanged; virtual;
    procedure SampleRateChanged; virtual;
  public
    constructor Create(VstModule: TVSTModule);
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
  SysUtils, DAV_Types, SineSynthModule;

{ TSineSynthVoice }

constructor TSineSynthVoice.Create(VstModule: TVSTModule);
begin
 FVSTModule := VstModule;
 FOscillator := TSimpleOscillator.Create;
 if VstModule.SampleRate = 0
  then SampleRate := 44100
  else SampleRate := VstModule.SampleRate;
end;

destructor TSineSynthVoice.Destroy;
begin
 FreeAndNil(FOscillator);
 inherited;
end;

procedure TSineSynthVoice.SetFrequency(const Frequency: Single);
begin
 if FFrequency <> Frequency then
  begin
   FFrequency := Frequency;
   FrequencyChanged;
  end;
end;

procedure TSineSynthVoice.SetSampleRate(const Value: Single);
begin
 if Value <= 0
  then raise Exception.Create('Samplerate must be positive and larger than 0!');
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SamplerateChanged;
  end;
end;

procedure TSineSynthVoice.FrequencyChanged;
begin
 FOscillator.Frequency := FFrequency;
end;

procedure TSineSynthVoice.SampleRateChanged;
begin
 FOscillator.SampleRate := SampleRate;
end;

procedure TSineSynthVoice.NoteOn(Frequency, Amplitude: Single);
begin
 FFrequency := Frequency;
 FOscillator.Frequency := FFrequency;
 FOscillator.Amplitude := Amplitude;
end;

procedure TSineSynthVoice.NoteOff;
begin
 FOscillator.Amplitude := 0
end;

function TSineSynthVoice.Process: Single;
begin
 Result := FOscillator.Sine;
 FOscillator.CalculateNextSample;
end;

end.
