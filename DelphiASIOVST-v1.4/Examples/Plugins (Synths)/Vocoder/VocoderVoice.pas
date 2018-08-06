unit VocoderVoice;

interface

uses
  DAV_VSTModule, DAV_Complex;

{$i Consts.inc}

type
  TVocoderVoice = class(TObject)
  private
    FMidiKeyNr        : Integer;
    FVelocity         : Integer;
    FSampleRate       : Single;
    FInvSampleRate    : Single;
    FFrequency        : Single;
    FAmplitude        : Single;
    FVSTModule        : TVSTModule;
    FAngle, FPosition : TComplex64;
    procedure SetSampleRate(const Value: Single);
  protected
    procedure FrequencyChanged; virtual;
    procedure SamplerateChanged; virtual;
  public
    constructor Create(theModule: TVSTModule);
    destructor Destroy; override;
    procedure SetFrequency(const Value: Single); virtual;
    procedure NoteOn(Frequency, Amplitude: Single);
    procedure NoteOff;
    function Process: Single; virtual;
  published
    property Frequency: Single read FFrequency write SetFrequency;
    property SampleRate: Single read FSampleRate write SetSampleRate;
    property MidiKeyNr: Integer read FMidiKeyNr write FMidiKeyNr;
    property Velocity: Integer read FVelocity write FVelocity;
  end;

implementation

uses
  SysUtils, DAV_Common, DAV_Types, DAV_Math, VocoderModule;

{ TVocoderVoice }

constructor TVocoderVoice.Create(theModule: TVSTModule);
begin
 FVSTModule := theModule;
 if theModule.SampleRate = 0
  then SampleRate := 44100
  else SampleRate := theModule.SampleRate;
 FPosition.Re := 0;
 FPosition.Im := -1;
end;

destructor TVocoderVoice.Destroy;
begin
 inherited;
end;

procedure TVocoderVoice.SetSampleRate(const Value: Single);
begin
 if Value <= 0
  then raise Exception.Create('Samplerate must be larger than 0!'); 
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

function TVocoderVoice.Process: Single;
begin
 Result := FPosition.Re * FAngle.Re - FPosition.Im * FAngle.Im;
 FPosition.Im := FPosition.Im * FAngle.Re + FPosition.Re * FAngle.Im;
 FPosition.Re := Result;

 if Result > 0
  then Result := FAmplitude * Result - FAmplitude
  else Result := FAmplitude * Result + FAmplitude;
 Result := Result + FAmplitude * 0.01 * (random - 0.5)

(*
 if Result > 0
  then Result := FAmplitude * Result
  else Result := -FAmplitude;
*)
end;

procedure TVocoderVoice.SamplerateChanged;
begin
 FInvSampleRate := 1 / Samplerate;
end;

procedure TVocoderVoice.FrequencyChanged;
begin
 GetSinCos(2 * Pi * FFrequency * FInvSampleRate, FAngle.Im, FAngle.Re);
end;

procedure TVocoderVoice.SetFrequency(const Value: Single);
begin
 if FFrequency <> Value then
  begin
   FFrequency := Value;
   FrequencyChanged;
  end;
end;

procedure TVocoderVoice.NoteOn(Frequency, Amplitude: Single);
begin
 FFrequency := Frequency;
 FrequencyChanged;
 FAmplitude := Amplitude;
end;

procedure TVocoderVoice.NoteOff;
begin
  FAmplitude := 0;
end;

end.
